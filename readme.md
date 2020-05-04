# MOVES2014b

EPA's MOtor Vehicle Emission Simulator (MOVES) is a state-of-the-science emission modeling system that estimates emissions for mobile sources at the national, county, and project level for criteria air pollutants, greenhouse gases, and air toxics, available under EPA's Open Source Software policy. 


MOVES2014b is the latest version of MOVES available for regulatory purposes. For more information, see [Policy Guidance on the Use of  MOVES2014 for State Implementation  Plan Development, Transportation  Conformity, and Other Purposes](https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100K4EB.txt).

For additional information on MOVES, visit EPA's [MOVES website](https://www.epa.gov/moves). A standard installer for MOVES is also available [here](https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves#download). To install MOVES from source, follow the instructions below.

### Requirements and Set Up

This repository contains all of the source code and data required to compile and run MOVES.

MOVES uses MariaDB, Java, and Go. At minimum, you will need the following:

* [MariaDB](https://downloads.mariadb.org/mariadb/10.4.12/) (version 10.4) or MySQL (5.5-5.7)
* [Java JDK](https://adoptopenjdk.net/) (version 8)
* [Go](https://golang.org/dl/#go1.5.4) (version 1.5.4)

To get set up:

1. Clone or download this repository.
2. Edit lines 3 and 4 of setenv.bat to set the JDK and JRE paths to match your environment.
3. Unzip the default database from `\database\Setup\movesdb20181022.zip` to the same directory.
4. Edit `\database\Setup\SetupDatabase.bat` to use the MariaDB root user's password, and then run it. This batch file creates the MOVES database user by running `\database\Setup\CreateMOVESUser.sql`, and then installs the default database by running `\database\Setup\movesdb20181022.sql`. Note: This batch file assumes that the MariaDB executable (called mysql.exe) is on your Windows PATH. If it is not, you will need to modify the batch file so that it can find mysql.exe. 
5. Open a command prompt, navigate to your MOVES source code directory, and run the following commands to compile MOVES and launch the GUI:

```bash
setenv
ant compileall
ant go
ant rungui
```
6. Hereafter to run MOVES, simply navigate to the MOVES directory and run:

```bash
MOVESMaster.bat
```

### Need help?

Documentation is available for [MOVES Algorithms](https://www.epa.gov/moves/moves-algorithms), [Onroad Technical Reports](https://www.epa.gov/moves/moves-onroad-technical-reports), and [Nonroad Technical Reports](https://www.epa.gov/moves/nonroad-technical-reports). On your own training modules are available [here](https://www.epa.gov/moves/moves-training-sessions#training). If you have feedback, [email the MOVES inbox](mailto:mobile@epa.gov).

### Previous MOVES Versions 

This is the first version of MOVES posted on GitHub. Previous versions of MOVES are available at the [MOVES website](https://www.epa.gov/moves/moves-versions-limited-current-use).

### License

MOVES is licensed for use pursuant to the [GNU General Public License (GPL)](http://www.gnu.org/licenses/old-licenses/gpl-2.0.html).

### EPA Disclaimer 

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
