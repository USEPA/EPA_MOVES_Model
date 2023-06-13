# Documentation

This readme contains a table of contents of MOVES help files, notes, and code documentation to help users understand how MOVES is structured and how to run MOVES in a development environment. The [EPA MOVES website](https://www.epa.gov/moves) includes other documentation, such as peer-reviewed documentation on the data and algorithms used in MOVES in the [Onroad Technical Reports](https://www.epa.gov/moves/moves-onroad-technical-reports) and [Nonroad Technical Reports](https://www.epa.gov/moves/nonroad-technical-reports). On-your-own training modules are available [here](https://www.epa.gov/moves/moves-training-sessions#training).

## Getting Started:

* [Installation Troubleshooting](InstallationTroubleshooting.pdf): Answers for frequently asked questions regarding installation issues when using the .exe installer
* [Quick Start Guide To Accessing MariaDB Data](QuickStartGuideToAccessingMariaDBData.pdf): Tips for users who are transitioning from MySQL and MySQL Workbench to MariaDB and HeidiSQL
* [Command Line MOVES](CommandLineMOVES.md): A brief guide on how to run MOVES from the command line
* [Tips for Faster MOVES Runs](TipsForFasterMOVESRuns.md): Suggestions for how to structure MOVES runs to be as efficient as possible

## MOVES Cheat Sheets:

These PDF cheat sheets summarize common tables and values used to create MOVES runs and interpret their outputs. 

* [Onroad Cheat Sheet](MOVES4CheatsheetOnroad.pdf)
* [Nonroad Cheat Sheet](MOVES4CheatsheetNonroad.pdf)

## Help for MOVES Tools:

* [Converting MOVES3 input databases to MOVES4](../database/ConversionScripts/InputDatabaseConverstionHelp.pdf): Information on steps needed to convert MOVES3 input databases to the MOVES4 format
* [Building LEV and NLEV input databases](../database/LEV_NLEVScripts/InstructionsForLEV_NLEV_Tool.pdf): Instructions on how to use the LEV/NLEV Tool in the MOVES GUI
* [National Emissions Inventory Submissions QA Tool](NEIQAInstructions.md): Instructions and documentation for the NEI Submissions QA Tool
* [ONI Tool](../database/ONITool/InstructionsForONITool.pdf): Instructions on how to use the ONI Tool when running MOVES in rates mode with default off-network idling activity
* MOVES Speciation tools: Instructions for how to speciate MOVES output as a post-processing step, for both [onroad](../database/ProfileWeightScripts/OnroadSpeciationInstructions.pdf) and [nonroad](../database/ProfileWeightScripts/NonroadSpeciationInstructions.pdf).

## Developer References:

* [MOVES Database Glossary](MOVESGlossary.md): Glossary of the column names used in the MOVES default database
* [MOVES Database Tables](MOVESDatabaseTables.md): Schema descriptions for each table in the MOVES default database
* [Input Database Changes in MOVES4](inputDBchanges.md): Description of the schema changes to MOVES County Scale and Project Scale input databases 
* [Anatomy of a RunSpec](AnatomyOfARunspec.md): An overview of all of the fields contained in a MOVES RunSpec
* [MOVES Code: Folder by Folder](FolderByFolder.md): Descriptions of the contents within the folders in the MOVES source code directory
* [Debugging MOVES](DebuggingMOVES.md): Tips for troubleshooting and debugging unexpected behavior in MOVES runs
* [SQL Tip Sheet](SQLTipSheet.pdf): Cheat sheet for SQL syntax
* [Compiling NONROAD](../NONROAD/NR08a/SOURCE/readme.md): Instructions on how to compile the NONROAD Fortran code


