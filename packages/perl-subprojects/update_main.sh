#!/bin/sh
# Maintainer: Marcus Seyfarth <marcus85@gmx.de>
cd /home/marcus/Downloads/perl-subprojects
for pkg in perl-b-cow-main \
perl-clone-main \
perl-encode-locale-main \
perl-error-main \
perl-file-listing-main \
perl-html-parser-main \
perl-html-tagset-main \
perl-io-html-main \
perl-libwww-main \
perl-locale-gettext-main \
perl-net-http-main \
perl-timedate-main \
perl-xml-parser-main \
perl-xml-writer-main \
; do
  cd $pkg
  rm -f *.zst
  makepkg -si --cleanbuild --skippgpcheck --skipchecksums --noconfirm
  cd ..
done
