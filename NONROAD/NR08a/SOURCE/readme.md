# Compiling NONROAD

The NONROAD model is written in FORTRAN. It is available pre-compiled both on GitHub and when using the Windows installer for MOVES, so most users will not need to compile NONROAD. However, users running in a Linux environment will need to recompile.

Previously, EPA recommended using `gfortran` to compile NONROAD. However, we have received reports that in certain instances, NONROAD compiled with `gfortran` on some Linux platforms incorrectly produces near-zero emission results for some equipment types. Therefore, when compiling for Linux, EPA recommends finding a compatible FORTRAN compiler using the resources at [fortran-lang.org](https://fortran-lang.org/compilers/). Note that we have not experienced any issues compiling with `gfortran` on Windows installed with [MSYS2](https://www.msys2.org/) or [Cygwin](https://cygwin.com/).

Regardless of which compiler is used, NONROAD should be compiled using the `makefile` found in the `NONROAD/NR08a/SOURCE` folder. This can be done by navigating to the `NONROAD/NR08a/SOURCE` folder in a terminal and running the `make` command. Note that the first line of the makefile may need to be edited to specify your chosen compiler if you do not use `gfortran`.

After compiling NONROAD, delete the `NONROAD/NR08a/NONROAD.exe` file, and move your new executable to this location. After moving, ensure the file name is the same (including capitalization) as `NONROAD.exe`. If a different filename or directory is used, you will need to update the path to executable in the following configuration files:

1. MOVESConfiguration.txt
2. MOVESWorker.txt
3. maketodo.txt
4. manyworkers.txt
