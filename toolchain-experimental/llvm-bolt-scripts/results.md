# Benchmarking with script ./measure_build.bash

### LLVM 15 Stage 1

```
== Start Build
[2529/2529] Creating executable symlink bin/clang

real    7m11,300s
user    154m25,691s
sys     4m8,671s

```

### Stage 3 LLVM 15 PGO+THINLTO:

```
== Start Build
[2529/2529] Creating executable symlink bin/clang

real    4m35,989s
user    97m51,335s
sys     3m53,897s

```

### Stage3 LLVM 15 PGO+THINLTO+BOLT without branch sampling:

```
== Start Build
[2529/2529] Creating executable symlink bin/clang

real    4m12,274s
user    92m19,092s
sys     3m48,425s

```
