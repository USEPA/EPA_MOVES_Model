# Running MOVES from the Windows command line

## Introduction

While the easiest way to interact with MOVES is via its graphical user interface (GUI) it is sometimes useful to process MOVES runspecs in batches via the windows command line. To this end, MOVES has included a set of commands that can be accessed via the command line tool Ant. These commands can in turn be further scripted using standard scripting languages such as Windows Batch or PowerShell. This document provides a brief introduction to the MOVES ant commands and a very simple example batch script for running multiple MOVES runspecs in series. All of the examples presented here will use the windows command shell (cmd.exe)

## Setup the command environment

The first step to running MOVES from the command line is to setup the shell environment to point to all of the resources it needs to run MOVES.  To do this, open the windows command prompt and navigate to the MOVES program folder (default: C:\Users\Public\EPA\MOVES3). Enter the command `setenv`:

```cmd
C:\Users\Public\EPA\MOVES3>setenv
```

This should execute the setenv.bat script without displaying anything to the command window. The script tells the windows command shell where to find the Java runtime environment (JRE) bundled with MOVES, and where to find the Ant utility.

## View available MOVES Ant commands

After running setenv, MOVES ant commands should be available in the windows command shell for the duration of the shell session. To test this you can type `ant` at the command prompt:

```cmd
C:\Users\Public\EPA\MOVES\MOVES3>ant
```

This will display a list of the ant commands that are defined in the MOVES build.xml file. Each command listed will include a brief description of what it does. There are commands included for recompiling MOVES, testing the MOVES code, and for running MOVES in a number of ways. This document will focus only on the basic commands needed for executing MOVES runs. 

NOTE: several of the commands require that `ant setlogin` be run first. If you installed MOVES using the MOVES installer you will not need to run `ant setlogin`

## Example calls to the MOVES Ant commands

To execute a MOVES run you only need to use one or two commands, `ant run` and `ant manyworkers`. The first command runs MOVES using a designated runspec, the other can be used to start additional workers to help speed up a MOVES run. Below are brief examples of how these commands are entered.

### Running MOVES

The following command runs MOVES

```
C:\Users\Public\EPA\MOVES\MOVES3>ant run -Drunspec="PathToRunspec\runspec.mrs"
```

In this example the runspec is specified using the `-Drunspec=` flag. The flag takes a relative path from the MOVES directory to the runspec to be run.  By default `run` will also start one worker to process bundles for the MOVES run.

### Launching MOVES workers

The following command launches additional workers to process bundles in a MOVES run.

```
C:\Users\Public\EPA\MOVES\MOVES3>ant manyworkers -Dmaxworkers=3 -Dnoshutdown=1
```

Here the option `-Dmaxworkers=3` specifies the number of additional workers to start (in this case 3). The option `-Dnoshutdown=1` keeps the workers running if no bundles are received after a certain amount of time. This can be a helpful option when the bundles take a long time to be generated, or when running many back to back MOVES runs. 

## Example script for executing MOVES batch runs

The above Ant commands can be used in batch scripts written to run in the Windows command shell. Batch scripts are text files with a .bat file extension that contain Windows shell commands that are run sequentially from the Windows command shell. Two example batch scripts are shown below. The first is a simple example that demonstrates how to run multiple MOVES runspecs sequentially. The second shows how to start additional workers to help speed up the MOVES run. Both of these scripts assume that MOVES is installed in its default location on the windows C drive `C:\Users\Public\EPA\MOVES\MOVES3` and that the folder containing the script and runspecs is also on the `C:` drive. Finally both of these scripts assume that any required input databases specified in the runspecs have already been placed in the appropriate database folder.

### Basic MOVES batch script

Below is a simple MOVES batch script for running multiple runspecs sequentially. 

```batch
:: Find current folder 
set RunspecDir=%CD%

:: Set MOVES install location
set MOVESDir=C:\Users\Public\EPA\MOVES\MOVES3-Beta

:: setup moves environment
cd /d %MOVESDir%
call setenv.bat

:: Run MOVES for each runspec 
::(add a new line for each runspec)
call ant run -Drunspec="%RunspecDir%\BatchTest1.mrs"

call ant run -Drunspec="%RunspecDir%\BatchTest2.mrs"

:: Return to your test folder when everything is finished
cd /d %RunspecDir%
```

In this batch script the  runspecs `BatchTest1.mrs`, and `Batchtest2.mrs` are stored in the same folder as the script. The script finds its location, sets up the MOVES run environment, and executes the runspecs sequentially. After the last run is complete the script returns the command prompt to the directory containing itself and the two runspecs.

### MOVES batch script with extra workers

For computers with multiple CPU cores, MOVES performance can sometimes be improved by launching additional workers. In general you should not run more workers than the number of cores minus two. Note that as multiple workers begin to compete for access to the MOVES database their performance can decrease. Therefore there are diminishing and potentially negative returns for launching too many workers on a given computer. See [Tips For Faster MOVES Runs](TipsForFasterMOVESRuns.md) for more information.

The script below is exactly like the previous script, except that this script also launches three MOVES workers in a second command window. 

```batch
:: Find current folder 
set TestingDir=%CD%

:: Set MOVES install location
set MOVESDir=C:\Users\Public\EPA\MOVES\MOVES3-Beta

:: setup moves environment
cd /d %MOVESDir%
call setenv.bat

:: Launch two additional MOVES workers
start cmd /k ant manyworkers -Dmaxworkers=3 -Dnoshutdown=1

:: Run MOVES for each runspec 
::(add a new line for each runspec, or get fancy and write a loop here)
call ant run -Drunspec="%TestingDir%\BatchTest1.mrs"

call ant run -Drunspec="%TestingDir%\BatchTest2.mrs"

:: Return to your test folder when everything is finished
cd /d %TestingDir%
```

Once these moves runs finish, the workers will remain active until the user closes the command window that they are running in. Note when the `run` command is executed, MOVES checks to see if any workers are running. It if doesn't find any it will automatically start one worker to process its bundles. 

## Additional remarks

The above examples demonstrate a simple way of running MOVES from the windows command shell. The example scripts should be considered just EXAMPLES. Different MOVES install configurations may change some of the details needed to run the scripts. Likewise the Windows command shell offers a rich scripting environment that can be used to expand these examples to do things such as: 

* archive the `moveslog.txt` generated with each run

* loop through an arbitrary set of runspecs by searching the folder for files with .mrs extensions

* copy input databases stored elsewhere into the MariaDB data directory

* execute custom sql post-processing scripts with MariaDB 

Further details for the suggestions above are outside the scope of this overview. However, online you can find many good scripting tutorials and user guides for writing batch scripts in Windows.
