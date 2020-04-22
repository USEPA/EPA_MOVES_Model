# MOVES 2014b

This readme document is currently a PLACEHOLDER. A full MOVES2014b repository will be posted to this location shortly.

EPA's MOtor Vehicle Emission Simulator (MOVES) is a state-of-the-science emission modeling system that estimates emissions for mobile sources at the national, county, and project level for criteria air pollutants, greenhouse gases, and air toxics, available under EPA’s Open Source Software policy. 


MOVES2014b is the latest version of MOVES available for regulatory purposes. For more information, see [Policy Guidance on the Use of  MOVES2014 for State Implementation  Plan Development, Transportation  Conformity, and Other Purposes](https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100K4EB.txt)

For additional information on MOVES, visit EPA's [MOVES website](https://www.epa.gov/moves).

### Requirements and Set Up ###

This repository WILL CONTAIN all of the source code required to compile and run MOVES. Please note that it does not contain the required MOVES database, which WILL BE found in the [MOVES DB](//TODO: Insert link) repository/website.

MOVES uses MariaDB, Java, and Go. At minimum, you will need the following:

* [MariaDB](https://mariadb.org/download/) (10.2 or greater) or MySQL (5.6-5.8)
* [Java JDK](//TODO: Figure out jdk and put link here) (1.8 or greater)
* [GO](https://golang.org/dl/) (version 1.5 or greater)

To get set up:

1. Clone or download this repository.
2. Edit line 3 of setenv.bat to set JAVA_HOME to the installation path of the JDK.
3. Make note of the default database specified in MOVESConfiguration.txt, on the line specified by `defaultDatabaseName`.
4. Get the correct database from the [MOVES DB repository/website](//TODO: put link here).
5. Copy the database that you need and extract it to your MariaDB data directory.
6. Add the `moves` user to the MariaDB server. This can be done by connecting to the MariaDB server as root and running the following queries:

```sql
CREATE USER 'moves'@'localhost' IDENTIFIED BY 'moves';
GRANT ALL PRIVILEGES ON *.* TO 'moves'@'localhost';
FLUSH PRIVILEGES;
```

7. Open a command prompt, navigate to your MOVES source code directory, and run the following commands to compile MOVES and launch the GUI:

```bash
setenv
ant compileall
ant go
ant rungui
```
8. Hereafter to run MOVES, simply navigate to the MOVES directory and run:

```bash
MOVESMaster.bat
```

### Need help?

Documentation can be found [here](//TODO: put relevant link here). If you have feedback [email the MOVES inbox](mailto:mobile@epa.gov).

### Previous MOVES Versions 

This is the first version of MOVES posted on GitHub. Previous versions of MOVES are available at the [MOVES website](https://www.epa.gov/moves).

MOVES is licensed for use pursuant to the [GNU General Public License (GPL)](http://www.gnu.org/licenses/old-licenses/gpl-2.0.html).

### EPA Disclaimer 

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.


