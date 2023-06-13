# MOVES4-ReleaseCandidate2

EPA's MOtor Vehicle Emission Simulator (MOVES) is a state-of-the-science emission modeling system that estimates emissions for mobile sources at the national, county, and project level for criteria air pollutants, greenhouse gases, and air toxics, available under EPA's Open Source Software policy. 

The MOVES4 Release Candidate is made available for testing purposes and for modelers to become familiar with functional changes between MOVES3 and MOVES4 before MOVES4.0.0 is released. Emission results of MOVES4.0.0 may differ from the results of this release candidate. Results from this version may not be used in work for state implementation plans, conformity determinations, or for any other regulatory purpose.

MOVES3.1 (available at https://github.com/USEPA/EPA_MOVES_Model and https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves) is the latest version of MOVES available for regulatory purposes. For more information, see [Policy Guidance on the Use of MOVES3 for State Implementation Plan Development, Transportation Conformity, General Conformity, and Other Purposes (EPA-420-B-20-044)](https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves#sip). 

For additional information on MOVES, visit EPA's [MOVES website](https://www.epa.gov/moves). A standard installer for MOVES is available [here](https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves#download). Or, to compile and MOVES from source, follow the instructions below.

### Requirements and Set Up

This repository contains all of the source code and data required to compile and run MOVES.

MOVES uses MariaDB, Java, and Go. To run MOVES from the source code (i.e., without running the installer), you will need the following:

* [MariaDB](https://mariadb.org/download/?t=mariadb&p=mariadb&r=10.11.2) (version 10.11 is recommended)
* [Java JDK](https://learn.microsoft.com/en-us/java/openjdk/download#openjdk-17) (version 17 is recommended)
* [Go](https://golang.org/dl) (version 1.13 or later)

To get set up:

1. Clone or download this repository.

2. Make sure MariaDB, Java, and Go are all installed and available on your system path (i.e., make sure the `\bin` folders for each of these are in your PATH environment variable). You can test this by running each of the following lines in a command prompt:
    *   `mysql.exe --version`
    *   `java.exe -version`
    *   `go.exe version`

3. MOVES has some specific MariaDB configuration requirements. Locate the MariaDB configuration file (on Windows, this is my.ini located in your data directory; see #4 of [Quick Start Guide to Accessing MariaDB Data](docs/QuickStartGuideToAccessingMariaDBData.pdf) for help on finding your data directory), and ensure the following lines are saved in the file:

    ```ini
    [mysqld]
    default-storage-engine=MyISAM
    secure-file-priv=''
    sql_mode=STRICT_TRANS_TABLES,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION
    lower_case_table_names=1
    character-set-server=utf8
    collation-server=utf8_unicode_ci
    init-connect='SET NAMES utf8'
    ```
    Restart the MariaDB service after modifying this file.

4. If MariaDB is running on a non-default port (i.e., any port other than 3306), create a file called `MySQL.txt` in the root MOVES directory, and save the port number to this text file. This file should contain no whitespace, just the port number.

5. Uncomment line 2 of `setenv.bat` and comment out line 3 to set the JDK and JRE paths to match your environment (i.e., remove `REM` from the beginning of line 2 and add it to the beginning of line 3). Then, set `JAVA_HOME` to your JDK path on line 6.

6. Unzip the default database dumpfile from the .zip file in the `\database\Setup` directory to the same directory.

7. Edit `\database\Setup\SetupDatabase.bat` to use the MariaDB root user's password, and then run it. This batch file creates the MOVES database user by running `\database\Setup\CreateMOVESUser.sql`, and then installs the default database by running the dumpfile you extracted in the previous step. 

    Note: if MariaDB is running on a different port, you will need to edit `SetupDatabase.bat` to add the command-line flag `--port=XXXX`, where `XXXX` is the port number.

8. Open a command prompt, navigate to your MOVES source code directory, and run the following commands to compile MOVES and launch the GUI:

    ```bash
    setenv
    ant crungui
    ```

    For additional information about compiling MOVES, see [CommandLineMOVES.md](docs\CommandLineMOVES.md#compiling-moves).

9. Hereafter to run MOVES, simply navigate to the MOVES directory and run:

    ```bash
    MOVESMain.bat
    ```

### Need help?

Documentation on the software components of MOVES, database structure, running MOVES from the command line, tips for improving MOVES performance, and other information are available in the [\docs](docs/Readme.md) directory of this project. To check the status of reported issues and planned improvements, see the [MOVES GitHub Issue Tracker](https://github.com/USEPA/EPA_MOVES_Model/issues).

Additional resources listed below are available at the MOVES website:

* [MOVES Technical Guidance](https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves#guidance): Guidance on appropriate input assumptions and sources of data for the use of MOVES in SIP development and regional emissions analyses for transportation conformity determinations in states other than California. It also includes guidance on developing nonroad inventories with MOVES.
* [Onroad Technical Reports](https://www.epa.gov/moves/moves-onroad-technical-reports) and [Nonroad Technical Reports](https://www.epa.gov/moves/nonroad-technical-reports): Access peer-reviewed documentation on the default inputs and algorithms used in MOVES
* [MOVES Training](https://www.epa.gov/moves/moves-training-sessions#training): Contains on-your-own training modules for using MOVES
* [MOVES FAQ](https://www.epa.gov/moves/frequent-questions-about-moves-and-related-models): Frequent asked questions about MOVES and related models

If you have questions or feedback about MOVES, [email the MOVES inbox](mailto:mobile@epa.gov).

### Previous MOVES Versions

Previous versions of MOVES going back to the MOVES2014b December 2018 Technical Update can be accessed on GitHub at [MOVES Releases](https://github.com/USEPA/EPA_MOVES_Model/releases). Older versions of MOVES are available at the [MOVES website](https://www.epa.gov/moves/moves-versions-limited-current-use).

### License

MOVES is licensed for use pursuant to the [GNU General Public License (GPL)](http://www.gnu.org/licenses/old-licenses/gpl-2.0.html).

### EPA Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
