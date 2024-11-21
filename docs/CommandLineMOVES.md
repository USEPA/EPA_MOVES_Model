# Running MOVES from the Windows command line

## Introduction

While the easiest way to interact with MOVES is via its graphical user interface (GUI), it is sometimes useful to process MOVES RunSpecs in batches via the Windows command line. To this end, MOVES has a set of commands that can be accessed via the command line tool Ant. These commands can be further scripted using Batch files or PowerShell scripts. This document provides a brief introduction to the MOVES Ant commands and example batch scripts for running multiple MOVES RunSpecs in series. All of the examples presented here will use the Windows command shell (cmd.exe)

## Set up the command environment

The first step to running MOVES from the command line is to set up the shell environment to point to all of the resources it needs to run MOVES. Open the Windows command prompt and navigate to the MOVES directory. By default, the path is `C:\Users\Public\EPA\MOVES\MOVES5`. The command to change directories is `cd`:

```cmd
C:\> cd C:\Users\Public\EPA\MOVES\MOVES5

```

Then, enter the name `setenv` to run the setenv.bat script, which will set up your environment for you:

```cmd
C:\Users\Public\EPA\MOVES\MOVES5> setenv
```

This will execute the setenv.bat script without displaying anything to the command window. The script tells the Windows command shell where to find the Java Runtime Environment (JRE) bundled with MOVES and where to find the Ant utility.

The rest of the instructions in this document assume that you have run these statements and are entering the subsequent commands in the same command prompt (except where noted).

## View available MOVES Ant commands

After running `setenv`, MOVES Ant commands will be available in the Windows command prompt for the duration of the shell session. To test this you can type `ant` at the command prompt:

```cmd
ant
```

This will display a list of the Ant commands that are defined in the MOVES build.xml file. Each command listed will include a brief description of what it does. There are commands included for recompiling MOVES, testing the MOVES code, and for running MOVES in a number of ways. This document will focus only on the basic commands needed for executing MOVES runs and using MOVES tools. 

## Running MOVES

A MOVES run can be launched from the command line. This method assumes that you have previously built a RunSpec (and any required input databases) using the GUI. 

### Launching a MOVES run

The following command starts a MOVES run:

```cmd
ant run -Drunspec="PathToRunSpec\RunSpec.mrs"
```

The RunSpec path is specified using the `-Drunspec=` flag. If the path is not absolute (i.e., does not start with a drive letter such as C:), it is interpreted relative to the MOVES directory.  By default, `run` will also start one worker to process bundles for the MOVES run as long as no other workers are detected. However, you may start additional MOVES workers using the command described below, which may speed up your MOVES run.

### Launching MOVES workers

The following command launches additional workers to process bundles in a MOVES run. 

> Note: This should be executed from a separate command prompt (after running `setenv` to configure the new command prompt's environment), so that this can be running at the same time as the `run` command above.

```cmd
ant 3workers -Dnoshutdown=1
```

`3workers` specifies that 3 workers should be started. Other options include `2workers` and `1worker`, which start 2 workers and only 1, respectively.

The option `-Dnoshutdown=1` keeps the workers running if no bundles are received after a certain amount of time. This can be a helpful option when the bundles take a long time to be generated, or when running many back to back MOVES runs. However, note that worker processes started with this option can only be terminated by pressing Ctrl+C in the terminal window or by using the Windows Task Manager to kill the Java processes.

>   Note: If the `3workers` (or the other variants) command is executed on the same computer before the `run` command is started, `run` will not launch another worker. However, if `run` is executed first, `run` will launch its own worker, and these workers will be in addition to that one.

If more than 3 workers are desired, there is an additional command that can be used:

```cmd
ant manyworkers -Dmaxworkers=4
```

Here the option `-Dmaxworkers=4` specifies the number of additional workers to start (in this case 4). Please note that having more than 3 workers is unlikely to improve performance.

## Example script for executing MOVES batch runs

The above Ant commands can be used in batch scripts written to be run in the Windows command prompt. Batch scripts are text files with a .bat file extension that contain Windows shell commands that are run sequentially from the Windows command prompt. Two example batch scripts are shown below. The first is a simple example that demonstrates how to run multiple MOVES RunSpecs sequentially. The second shows how to start additional workers to potentially help speed up the MOVES run. Note that these scripts assume that all the RunSpecs show all green checks when loaded in the GUI (i.e., they are error-free) and that any input databases specified in the RunSpecs are available in MariaDB.

### Basic MOVES batch script

Below is a sample MOVES batch script for running multiple RunSpecs sequentially. 

```batch
:: Find current folder 
set RunSpecDir=%CD%

:: Set MOVES install location
set MOVESDir=C:\Users\Public\EPA\MOVES\MOVES5

:: Set up MOVES environment
cd /d %MOVESDir%
call setenv.bat

:: Run MOVES for each RunSpec 
:: (add a new line for each RunSpec)
call ant run -Drunspec="%RunSpecDir%\BatchTest1.mrs"
call ant run -Drunspec="%RunSpecDir%\BatchTest2.mrs"

:: Return to your RunSpec folder when everything is finished
cd /d %RunSpecDir%
```

In this batch script, the  RunSpecs `BatchTest1.mrs` and `Batchtest2.mrs` are stored in the same folder as the script. The script finds its location, sets up the MOVES run environment, and executes the RunSpecs sequentially. After the last run is complete, the script returns the command prompt to the directory containing itself and the two RunSpecs.

### MOVES batch script with extra workers

MOVES performance can sometimes be improved by launching additional workers. Note that as multiple workers begin to compete for access to the MOVES database, their performance can decrease. Therefore, there are diminishing and potentially negative returns for launching too many workers on a given computer. In general, starting more than 3 workers is unlikely to improve performance. See [Tips For Faster MOVES Runs](TipsForFasterMOVESRuns.pdf) for more information.

The example script below is like the previous script, except that this script also launches three MOVES workers in a second command window. 

```batch
:: Find current folder 
set RunSpecDir=%CD%

:: Set MOVES install location
set MOVESDir=C:\Users\Public\EPA\MOVES\MOVES5

:: Set up MOVES environment
cd /d %MOVESDir%
call setenv.bat

:: Launch 3 MOVES workers
start cmd /k ant 3workers -Dnoshutdown=1

:: Run MOVES for each RunSpec 
:: (add a new line for each RunSpec)
call ant run -Drunspec="%RunSpecDir%\BatchTest1.mrs"
call ant run -Drunspec="%RunSpecDir%\BatchTest2.mrs"

:: Return to your RunSpec folder when everything is finished
cd /d %RunSpecDir%
```

Once these MOVES runs finish, the workers will remain active until the user closes the command window that they are running in. Note that when the `run` command is executed, MOVES checks to see if any workers are running. It if doesn't find any, it will automatically start one worker to process its bundles. In the example above, the three workers are started before launching MOVES, so the `run` command will not start an additional worker.

## Batch mode input database creation
MOVES includes an Ant command to create input databases, which can save time if you need to create many input databases at once. The following steps assume that you already have all of your input data files quality checked and ready to go.

1.  Using the MOVES GUI, create your base RunSpec.

2.  Using the Create Input Database Panel, create a corresponding database, and enter the Data Manager. Fully populate the database so that you get all green checks. 

    >   Note: Do not close and re-enter the Data Manager during this step or before performing step 3. All tables must be populated in one session for step 3 to work.

3.  On the Tools tab of the Data Manager, click the Generate Importer XML File button, and give it a name. The file extension should be `.xml`.

4.  Make copies of the XML file for each database that you want to build on the command line. Open the file and make the following changes:

    *   Within the `<filters>` tag, make any changes reflective of differences between the base RunSpec and the RunSpec that you will use with this database. For example, if the new database will be for a different calendar year, change the `<year key="XXXX">` entry.
    *   Name the database in the `<databaseselection>` tag. For example, `<databaseselection servername="localhost" databasename="new_db" />`
    *   Update the `<filename>` and `<section>` tags for each input group to point to the corresponding data file and tab (if the file is .xlsx)

5.  For each input database to be created, run the following command (changing the path to the importer as appropriate):
```cmd
ant dbimporter -Dimport="c:\mydbimporter.xml"
```

## Converting input databases on the command line

MOVES includes an Ant command to convert input databases from previous versions of MOVES to the current version, which can save time if you need to do this for many databases. Please see the help file at [database\ConversionScripts\InputDatabaseConversionHelp.pdf](../database/ConversionScripts/InputDatabaseConversionHelp.pdf) for more information on the uses and limitations of this feature. Note that more work is needed after running this command before the converted database can be used with MOVES.

The following command can be used to convert a MOVES4 input database to the MOVES5 format:
```cmd
ant convert4_db_to_5 -Dinput=m4_in -Doutput=m5_in
```

`-Dinput` is used to specify the old input database name, and `-Doutput` is used to specify the name for the new, converted database. `convert4_db_to_5` tells Ant to use the conversion scripts for going from MOVES4 to MOVES5.

## Compiling MOVES

MOVES uses three compiled languages during runtime: Java, Go, and Fortran (Fortran is only used when running Nonroad). If you use the MOVES Installer, you will receive all the compiled code and can run the model directly without compiling. If you are cloning this repository, you will only receive the compiled Fortran, and will need to compile Java and Go before running the model.

The following command can be used to compile both the Java and the Go, and then launch the GUI:

```cmd
ant crungui
```

If desired, Java and Go can be compiled separately. The following commands can be used when compiling Java:
* `ant clean`: Removes all compiled Java .class files
* `ant compile`: Compiles all uncompiled Java files and recompiles any existing, out-of-date .class files
* `ant compileall`: Compiles all Java files

The following commands can be used when compiling Go:
* `ant go64`: Compiles 64-bit versions of the two Go runtime executables used by MOVES. This is the option most users will use.
* `ant go32`: Compiles 32-bit versions of the two Go runtime executables used by MOVES. This option is only needed when running MOVES on 32-bit architecture.
* `ant go32local`: Compiles 32-bit versions of the Go runtime using a version of the Go compiler installed in the `.\go32` subdirectory of MOVES. This option is useful if you want to compile the 32-bit executable using a different version of Go than what is installed system-wide. It only works if you have created a `.\go32` subdirectory and copied an entire Go installation to this location.
* `ant go`: Compiles all of the above (i.e., 64-bit, 32-bit, and 32-bit local), as well as Linux versions of the two Go runtime executables. If any step fails (e.g., `go32local`), you'll see an error message, but the compiler will continue to the next step in the sequence. 

For instructions on how to compile the Fortran code, see [Readme.md](../NONROAD/NR08a/SOURCE/readme.md) in the Nonroad source code directory.

## Other MOVES command line tools

MOVES also includes other command line tools available outside of Ant. These are found in the [Tools](../tools) directory, along with their documentation on how and why to use them.

## Additional remarks

The above examples demonstrate a simple way of running MOVES from the Windows command prompt. The example scripts should be considered just examples. Different MOVES install configurations may change some of the details needed to run the scripts. Likewise, the Windows command prompt offers a rich scripting environment that can be used to expand these examples to do things such as: 

* Archive the `moveslog.txt` generated with each run (see `MOVESMain.bat` for an example of this)

* Loop through an arbitrary set of RunSpecs by searching the folder for files with .mrs extensions

* Copy input databases stored elsewhere into the MariaDB data directory

* Execute custom SQL post-processing scripts with MariaDB 

Further details for the suggestions above are outside the scope of this overview. However, online you can find many good scripting tutorials and user guides for writing batch scripts in Windows.
