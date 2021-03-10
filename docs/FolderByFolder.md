# MOVES Folder By Folder

This document contains information about the contents of the MOVES code folder and its subfolders. The goal is to provide a general overview of how the MOVES code files are organized, and the purpose of the individual folders and subfolders. The list is not necessarily comprehensive.

## amazon

This folder contains jars and other code to help setup and to run MOVES on the cloud. 

## ant

Ant is the MOVES build tool and provides the easiest way of interfacing with MOVES on the command line. This folder contains the ant libraries. The ant commands used to compile and run MOVES are specified in build.xml in the MOVES root directory.

## calc

This folder contains code for the MOVES calculators that are written in Go. Every calculator runs the main package in this folder, `externalcalculator.go`, which references the other packages as necessary. 

As a general rule, the Go calculators replace SQL code in the database folder. There is some SQL code in the database folder which builds the MOVES bundles for Go to pick up and process. Therefore some of the SQL code in the database folder is still run, even though a Go-equivalent calculator exists in this folder.

## database

This folder contains many SQL scripts used by MOVES. Most of the scripts are calculators, some of of which are partially deprecated by the Go calculators. However, there are non-calculator scripts in this folder as well, such as pre-aggregation and the rates output post-processor. There are also some generator files, but like the SQL calculators, they have been at least partially replaced by the Go generators in the generators folder.

### database/ConversionScripts

This folder contains conversion scripts to help users convert input databases for MOVES2014, MOVES2014a, and MOVES2014b to MOVES3 compatible input databases. These scripts can be accessed from the MOVES GUI under the Tools menu.

### database/LEV_NLEVScripts

This folder contains the template SQL scripts used to create the needed inputs for modeling emissions in states that have adopted LEV or NLEV standards. These scripts can be accessed from the MOVES GUI under the Tools menu.

### database/NEIQA

This folder contains the SQL scripts for the NEI Submissions QA Tool as well as the instructions for using them. These scripts are run via ant.

### database/NonroadProcessingScripts

This folder contains the post-processing scripts for nonroad runs. These scripts can be run from the MOVES GUI under the Post Processing menu.

### database/ONITool

This folder contains the SQL script for the ONI Tool as well as instructions for using it. This tool can be accessed from the MOVES GUI under the Tools menu.

### database/OutputProcessingScripts

This folder contains the post-processing scripts for MOVES onroad runs. These scripts can be run from the MOVES GUI under the Post Processing menu.

### database/Setup

This folder contains the scripts needed to configure MariaDB and load the MOVES default database without the installer.

## generators

Similar to calc, this folder contains Go code, but this code is used by the MOVES generators. Some of these generators replace parts of the Java generator code. Similar to the Go calculators and SQL, some parts of the Java generators are still run. 

## gov/epa/otaq/moves

This contains all the Java code for MOVES itself.

### gov/epa/otaq/moves/common

This folder contains various dependencies that are shared throughout the MOVES Java code. As a general rule, it handles tasks like connecting to SQL, compiling, and doing simple data conversions and formatting.

### gov/epa/otaq/moves/master

This folder contains the substantive MOVES Java codebase. It contains the generators, most calculators, the GUI code, nonroad, and the runspec building and parsing code.

#### gov/epa/otaq/moves/master/commandline

This contains the Java code that is called by Ant to run MOVES from the command line.

#### gov/epa/otaq/moves/master/framework

This contains basic classes used through much of MOVES. Many of them get extended in other parts of the codebase, but this sets up the basic infrastructure and most of the program flow. Things like the basic emission calculator class and master loop can be found here.

#### gov/epa/otaq/moves/master/gui

This contains all of the code for the MOVES GUI. 

#### gov/epa/otaq/moves/master/implementation

This folder contains the specific implementations of many of the classes found in framework. This is where most of the "MOVES code" actually is - the classes that handle pollutant or process specific calculations live here. As a general rule, it contains both calculators and generators. Most of the SQL executed by the Java code is found in these files. Like the database folder, some of the code in the implementation folder is no longer used due to the introduction of the Go code in either the generators or calculators.

#### gov/epa/otaq/moves/master/nonroad

This folder contains the Java code used to interface with the Fortran based nonroad model. However, there is more general MOVES code, especially in the framework folder, which is run for nonroad as well.

#### gov/epa/otaq/moves/master/runspec

This folder contains the code to parse and write MOVES runspecs.

### gov/epa/otaq/moves/systemtests

This folder contains some code-level tests of MOVES

### gov/epa/otaq/moves/tools

This folder contains some general tools used in MOVES code development, but not during MOVES runs themselves. 

### gov/epa/otaq/moves/utils

This contains Java code for basic tasks that aren't MOVES-specific but that MOVES does rely on, specifically relating to Java environments. 

### gov/epa/otaq/moves/worker

This contains the basic code for the MOVES worker. It includes the java files for the worker command that is executed through the command line via the Ant commands defined in build.xml. There is also a framework folder similar to the master/framework folder but quite a bit smaller. Finally there is gui folder for the GUI component that displays the worker status during a MOVES run.

## jre

This folder contains the components of the Java Runtime Environment necessary for running MOVES.

## libs

This folder contains the dependency jar files for MOVES. 

## MOVESTemporary

This folder is used during MOVES runtime to store temporary data. 

## NONROAD/NR08a

This folder contains a modified version of the standalone model NONROAD2008a. This model is written in Fortran, and the modifications are necessary for the changes to the model since it has been incorporated into MOVES.

### NONROAD/NR08a/DATA

This folder contains original default input data for the standalone nonroad model. File extensions include `.DAT`, `.POP`, `.ALO`, `.EMF`, and `.GRW`. These files are not used when running nonroad from MOVES.

### NONROAD/NR08a/OUTPUTS

This folder is only used when running the standalone nonroad model outside of MOVES.

### NONROAD/NR08a/SOURCE

This folder contains the Fortran code used to compile the nonroad model. See the readme.txt file for help on compiling Nonroad.

## NonroadTemporaryData

This folder is used during MOVES runtime to store temporary data. 

## SharedWork

This folder is used during MOVES runtime. It contains the bundles (in zipped form) to be processed by the MOVES workers. The bundles themselves will be marked as "TODO", "INPROGRESS", or "DONE" depending on whether they're written by the master and waiting for a worker to pick them up, being actively processed by a worker, or finished. This folder gets cleared out during the MOVES run as bundles finish and gets cleared out entirely at the start of the next MOVES run as well. 

## testdata

This contains jars and files used for testing some of the code components of MOVES. 

## tests

This folder contains XML files used to test some components of the MOVES GUI.

## WorkerFolder

This folder is normally empty. There is a flag in the Java code (gov\epa\otaq\moves\worker\framework\RemoteEmissionsCalculator.java) that will tell MOVES to save the worker bundles as its running. When this flag is on, the bundles are saved in this folder, unzipped, *after they have been processed*. This folder is very useful for debugging.
