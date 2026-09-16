#!/usr/bin/env python3
"""Focused real Meson/Jinja install tests; does not build systemd executables."""
from pathlib import Path
import subprocess,shutil,tempfile,json,hashlib,os
root=Path(__file__).resolve().parents[1]; delivery=root
results=[]
def run(cmd, **kw):
 x=subprocess.run(cmd,text=True,capture_output=True,**kw)
 if x.returncode:raise RuntimeError(f'{cmd}\n{x.stdout}\n{x.stderr}')
 return x
with tempfile.TemporaryDirectory() as temp:
 w=Path(temp)
 cases=[('before-disabled',False,False,True,True,0x01d10200,0),('after-disabled',True,False,True,True,0x01d10200,4),('before-enabled',False,True,True,True,0x01d10200,4),('after-enabled',True,True,True,True,0x01d10200,4),('after-no-tpm',True,False,True,False,0x01d10200,0),('after-no-openssl',True,False,False,True,0x01d10200,0),('after-custom-base',True,False,True,True,0x01d10300,4)]
 all_contents={}
 for name,patched,boot,openssl,tpm,base,count in cases:
  s=w/name;shutil.copytree(root/'validation/upstream',s)
  if patched:
   x=run(['patch','--batch','--fuzz=0','-p1','-i',str(delivery/'0002-install-nvpcr-without-bootloader.patch')],cwd=s)
   assert 'offset' not in x.stdout and 'fuzz' not in x.stdout
  for c in ['tpm2-setup','tpm2-clear','tpm2-swtpm','tpm2-generator']:
   (s/f'src/tpm2-setup/{c}.c').write_text('/* Source-presence stub: not compiled by this data-target test. */\n')
  (s/'config.h').write_text(f'#define TPM2_NVPCR_BASE {base}\n')
  (s/'meson.build').write_text(f'''project('nvpcr-install-regression', version: '1')
conf = configuration_data()
conf.set('ENABLE_BOOTLOADER', {int(boot)})
conf.set('HAVE_OPENSSL', {int(openssl)})
conf.set('HAVE_TPM2', {int(tpm)})
executables = []
libexec_template = {{}}
generator_template = {{}}
libopenssl_cflags = []
prefixdir = get_option('prefix')
python = import('python').find_installation()
jinja2_cmdline = [python, files('tools/meson-render-jinja2.py'), files('config.h')]
subdir('src/tpm2-setup')
foreach executable : executables
  assert('ENABLE_BOOTLOADER' in executable['conditions'])
endforeach
''')
  b=w/(name+'-build');dest=w/(name+'-stage')
  run(['meson','setup',str(b),str(s),'--prefix=/usr'])
  run(['meson','install','-C',str(b),'--destdir',str(dest)])
  files=sorted((dest/'usr/lib/nvpcr').glob('*.nvpcr'));assert len(files)==count,(name,files)
  contents={p.name:json.loads(p.read_text()) for p in files};all_contents[name]=contents
  for p in files:
   data=contents[p.name];assert data['name']==p.stem and data['algorithm']=='sha256'
   assert base<=data['nvIndex']<=base+3
   assert '{{' not in p.read_text()
  results.append({'case':name,'installed_definitions':len(files),'result':'PASS'})
 assert all_contents['after-disabled']==all_contents['before-enabled']==all_contents['after-enabled']
 for k,v in all_contents['after-disabled'].items():
  assert all_contents['after-custom-base'][k]['nvIndex']-v['nvIndex']==256
 # Exercise the actual PKGBUILD package guard.
 for name,expected in [('after-disabled',0),('before-disabled',1)]:
  x=subprocess.run(['bash','-c','source "$1"; _check_nvpcr_files "$2"','test',str(delivery/'PKGBUILD'),str(w/(name+'-stage'))],capture_output=True,text=True)
  assert x.returncode==expected
 stage=w/'after-disabled-stage';(stage/'usr/lib/nvpcr/login.nvpcr').write_text('')
 x=subprocess.run(['bash','-c','source "$1"; _check_nvpcr_files "$2"','test',str(delivery/'PKGBUILD'),str(stage)],capture_output=True)
 assert x.returncode==1
 # Every local source has a real matching hash, and all source/install files exist.
 x=run(['bash','-c','source "$1"; for i in "${!source[@]}"; do printf "%s\\t%s\\n" "${source[i]}" "${sha512sums[i]}"; done','test',str(delivery/'PKGBUILD')])
 for i,line in enumerate(x.stdout.splitlines()):
  name,sha=line.split('\t');
  if i==0:assert sha=='SKIP' and '?signed' in name;continue
  assert hashlib.sha512((delivery/name).read_bytes()).hexdigest()==sha,name
 assert (delivery/'systemd.install').is_file()
 run(['bash','-n',str(delivery/'PKGBUILD')])
print(json.dumps({'meson_install_cases':results,'render_equivalence':'PASS: bootloader-disabled patched output equals original bootloader-enabled output','custom_base':'PASS','actual_package_guard':'PASS: complete accepted, missing and empty rejected','local_source_checksums':'PASS','bash_syntax':'PASS','scope':'Real Meson data-target generation/install, upstream Jinja renderer/templates; no complete systemd compilation or host boot'},indent=2))
