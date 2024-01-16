# MOVES Default Scale Operating Mode Calculator

The MOVES Default Scale Operating Mode Calculator (called OpModeDistCalc) calculates an operating mode distribution for a given a MOVES runspec. 

When running MOVES at default scale, there is no way to access the underlying operating mode distributions, in full, that are used to calculate emissions from activity. The primary use case for this tool, therefore, is to support calculations and emission rate analysis outside of MOVES where users may want to manually map an operating mode distribution to emission rates in a way that MOVES doesn't support by default. 

More specifically, OpModeDistCalc replicates the MOVES activity calculations for calculating vehicle miles traveled (VMT) and source hours operating (SHO) at the level of detail needed to calculate emissions, which is by speed bin and operating mode. It then saves this output to a csv file, while MOVES continues its calculations and doesn't save the operating mode distribution anywhere that is accessible to the user. More detail on the calculation of activity and operating mode distributions can be found in the MOVES4 Vehicle Population and Activity Technical Report.

**This tool is currently in Beta development. This is not to be used for regulatory purposes. If you have any difficulties using this tool, or have any related feedback, please open an issue on GitHub or email us at mobile@epa.gov.**

There are several notes about OpModeDistCalc which are important to know when running the tool:

1. Because OpModeDistCalc is designed and intended only for use with MOVES Default Scale, it will run only for a Default Scale runspec and ignore any user-specified database. 

2. OpModeDistCalc is intended for calculating onroad operating mode distributions, and therefore does not estimate off-network idle (ONI) activity.

3. OpModeDistCalc is multithreaded and performs all calculations in memory. When running OpModeDistCalc, especially for larger runspecs, expect significant CPU usage and increases in memory. 

4. OpModeDistCalc can calculate national average operating mode distributions or county-specific operating mode distributions. It does not run at the state level. To calculate a state-level operating mode distribution, run every county in the state and aggregate the results manually.

## Command Line Interface

To use MOVES' command line tools, first open the Windows command prompt and navigate to the MOVES directory. Then, run the command `setenv`. Afterwards, OpModeDistCalc can be used by running `OpModeDistCalc <arguments>`. Arguments are provided as command line flags and are explained in more detail below.

For more information on using MOVES' command line tools, see [CommandLineMOVES.md](https://github.com/USEPA/EPA_MOVES_Model/blob/master/docs/CommandLineMOVES.md).

### Input Flags

The input flags are used to provide the Operating Mode Calculator the runspec it should use, database connection information, output aggregation information, and, finally, a location to store the results. 

Flags are passed using two dashes (`--`), the flag name, an equal sign, and then the value. The value should be specified in quotes. Flags left unspecified will use their default values. 

| Flag           | Description                                                  | Default Value                                             |
| -------------- | ------------------------------------------------------------ | --------------------------------------------------------- |
| `runspec`      | Path to MOVES runspec (full absolute path preferred)         | No default value - necessary for run        |
| `outputFolder` | Path to folder for OpModeCalc to write the resulting data and log files (full absolute path preferred) | No default value - necessary for run. Folder must already exist.             |
| `dbname`       | MOVES database to use                                        | `movesdb20240104` (this is the MOVES4.0.1 default database) |
| `mariaUname`   | User MariaDB username                                        | `moves`                                                    |
| `mariaUpass`   | User's MariaDB password                                      | `moves`                                                   |
| `mariaPort`    | The port which is used to connect to the MariaDB server      | The port used by the MOVES installation. When this cannot be determined, the assumed fallback port is 3306.    |
| `aggSpeeds`    | Boolean indicating whether to aggregate speed bin or keep them in the output. | `true` |
| `aggOpModes`   | Boolean indicating whether to aggregate operating modes or keep them in the output | `false` |


### Examples

Below is an example of a call to OpModeCalc with the minimum required number of inputs, a runspec and output folder. 

```
OpModeDistCalc --runspec="./tools/example/examplerunspec.mrs" --outputFolder="./tools/example/"
```

This run will use the default MariaDB connection settings as well as the default aggregation behavior for speed bins and operating modes (which would be aggregated and included, respectively). The run will be using the default database for MOVES4.0.1.

Users can alter the aggregation behavior, or make them more explicit, using the flags. For example, the following command will aggregate both operating modes and speed bins in the calculation of VMT and SHO.

```
OpModeDistCalc --runspec="./tools/example/examplerunspec.mrs" --outputFolder="./tools/example/" --aggSpeeds="true" --aggOpModes="true"
```

Users may specify any database using the `dbname` flag. For example, the following command will use the MOVES4.0.0 default database.

```
OpModeDistCalc --runspec="./tools/example/examplerunspec.mrs" --outputFolder="./tools/example/" --dbname="movesdb20230615"
```

Users may also specify a custom MariaDB server in case they are not using the MOVES4 default installation. That can be done using the flags for a username, password, and port. OpModeCalc is designed only to use databases that are local to the machine its running on.

```
OpModeDistCalc --runspec="./tools/example/examplerunspec.mrs" --outputFolder="./tools/example/" --dbname="movesdb20230615" --mariaUname="customUser" --mariaUpass="customPassword" --mariaPort=3307
```

## Output

`OpModeDistCalc` writes two output files. The first is a log file that includes details about the run. The second is the data output file, which mirrors the *movesoutput* and *movesactivityoutput* tables in MOVES output databases, but is specific for calculating operating mode distributions. 

| Column        | Comment                                                                      |
|---------------|------------------------------------------------------------------------------|
| yearID        | never aggregated                                                             |
| monthID       | aggregation set by runspec                                                   |
| dayID         | aggregation set by runspec                                                   |
| hourID        | aggregation set by runspec                                                   |
| sourceTypeID  | aggregation set by runspec                                                   |
| regClassID    | aggregation set by runspec                                                   |
| fuelTypeID    | aggregation set by runspec                                                   |
| modelYearID   | aggregation set by runspec                                                   |
| countyID      | synonymous with FIPS codes. If the full nation is selected, countyID is 0    |
| roadTypeID    | aggregation set by runspec                                                   |
| avgSpeedBinID | aggregation set by command line flag                                         |
| opModeID      | aggregation set by command line flag                                         |
| VMT           | Vehicle Miles Traveled, or MOVES activityTypeID 1                            |
| SHO           | Source Hours Operating, or MOVES activityTypeID 4                            |

All ID columns can be decoded using MOVES default database. With the exception of avgSpeedBinID and opModeID (which are specified using command line input flags) and the countyID column, which is determined by the geographic selection(s) in the runspec, the output detail is determined by the runspec itself. When an ID column is not specified to be included in the output, the value for all rows in the output file will be 0.
