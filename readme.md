# MOVES3.0.1

EPA's MOtor Vehicle Emission Simulator (MOVES) is a state-of-the-science emission modeling system that estimates emissions for mobile sources at the national, county, and project level for criteria air pollutants, greenhouse gases, and air toxics, available under EPA's Open Source Software policy. 

MOVES3.0 (available at https://github.com/USEPA/EPA_MOVES_Model and https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves) is the latest version of MOVES available for regulatory purposes. For more information, see [Policy Guidance on the Use of MOVES3 for State Implementation Plan Development, Transportation Conformity, General Conformity, and Other Purposes (EPA-420-B-20-044)](https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves#sip). 

For additional information on MOVES, visit EPA's [MOVES website](https://www.epa.gov/moves). A standard installer for MOVES is available [here](https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves#download). Or, to compile and MOVES from source, follow the instructions below.

### Requirements and Set Up

This repository contains all of the source code and data required to compile and run MOVES.

MOVES uses MariaDB, Java, and Go. At minimum, you will need the following:

* [MariaDB](https://downloads.mariadb.org/mariadb/10.4.12/) (version 10.4 or later)
* [Java JDK](https://adoptopenjdk.net/) (version 11)
* [Go](https://golang.org/dl) (version 1.13 or later)

To get set up:

1. Clone or download this repository.

2. Make sure MariaDB, Java, and Go are all installed and available on your system path (i.e., make sure the `\bin` folders for each of these are in your PATH environment variable). You can test this by running each of the following lines in a command prompt:
    *   `mysql.exe --version`
    *   `java.exe -version`
    *   `go.exe version`
    
3. If MariaDB is running on a non-default port (i.e., any port other than 3306), create a file called `MySQL.txt` in the root MOVES directory, and save the port number to this text file. This file should contain no whitespace, just the port number.

4. Uncomment line 2 of `setenv.bat` and comment out line 3 to set the JDK and JRE paths to match your environment (i.e., remove `REM` from the beginning of line 2 and add it to the beginning of line 3).

5. Unzip the default database dumpfile from the .zip file in the `\database\Setup` directory to the same directory.

6. Edit `\database\Setup\SetupDatabase.bat` to use the MariaDB root user's password, and then run it. This batch file creates the MOVES database user by running `\database\Setup\CreateMOVESUser.sql`, and then installs the default database by running the dumpfile you extracted in the previous step. 

    Note: if MariaDB is running on a different port, you will need to edit `SetupDatabase.bat` to add the command-line flag `--port=XXXX`, where `XXXX` is the port number.

7. Open a command prompt, navigate to your MOVES source code directory, and run the following commands to compile MOVES and launch the GUI:

```bash
setenv
ant crungui
```

6. Hereafter to run MOVES, simply navigate to the MOVES directory and run:

```bash
MOVESMaster.bat
```

### Need help?

Documentation on the software components of MOVES, database structure, running MOVES from the command line, tips for improving MOVES performance, and other information are available in the [\docs](docs/Readme.md) directory of this project. Peer-reviewed documentation on the data and algorithms used in MOVES are available in the [Onroad Technical Reports](https://www.epa.gov/moves/moves-onroad-technical-reports) and [Nonroad Technical Reports](https://www.epa.gov/moves/nonroad-technical-reports). On-your-own training modules are available [here](https://www.epa.gov/moves/moves-training-sessions#training). If you have feedback, [email the MOVES inbox](mailto:mobile@epa.gov).

### Previous MOVES Versions

MOVES3 is the second version of MOVES posted on GitHub. The previous version, MOVES2014b December 2018 Technical Update, can be accessed using the following tag: [MOVES2014b-Dec2018](https://github.com/USEPA/EPA_MOVES_Model/tree/MOVES2014b-Dec2018). Older versions of MOVES are available at the [MOVES website](https://www.epa.gov/moves/moves-versions-limited-current-use).

### License

MOVES is licensed for use pursuant to the [GNU General Public License (GPL)](http://www.gnu.org/licenses/old-licenses/gpl-2.0.html).

### EPA Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
