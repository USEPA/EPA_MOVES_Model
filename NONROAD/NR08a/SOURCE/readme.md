To compile the nonroad executable, install `gfortran` and navigate to this folder in the command prompt. Then type `make`.

`make` will find the makefile and then `gfortran` is used to compile the code.

Note that the location of the nonroad executable is configured in MOVES using 4 files:

1. MOVESConfiguration.txt
2. MOVESWorker.txt
3. maketodo.txt
4. manyworkers.txt

Depending on how this is set and which operating system is used, the nonroad executable may need to be moved and/or renamed to match the capitalization set in the configuration files.