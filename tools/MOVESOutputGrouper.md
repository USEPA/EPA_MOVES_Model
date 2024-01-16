# MOVES Output Grouper Beta

The MOVES Output Grouper can assist MOVES users in grouping multiple MOVES output databases together. The primary use case for this tool is when running MOVES in a parallel computing environment, it can combine output produced in parallel to be post-processed as if it were produced in series. This is perhaps best illustrated with the following example:

If you want to run MOVES at County Scale for 10 calendar years, the typical approach is to set up the 10 MOVES runs (one run for each calendar year) and write a batch script to execute them sequentially. However, this may take a long time to complete all 10 runs. If you have access to 10 virtual machines (VMs), each with its own MOVES installation, you can speed up your analysis by setting up one run per VM and performing the 10 runs in parallel. This significantly reduces the waiting time to receive MOVES results, but it comes with an added layer of complexity when post-processing the output, as the results are all in separate databases on separate VMs. To solve this problem, zip the databases on each VM (ensuring the zip directly contains the database files with no intermediate directory structure, and give the zip a unique name) and save them to a single shared directory. Then, use the MOVES Output Grouper tool to combine the individual databases into a single database. The tool will increment the MOVESRunID fields in each database to make the resulting grouped database look the same as if the typical approach was used instead (that is, as if all the runs were performed sequentially on a single computer).

**This tool is currently in Beta development. Please make copies of your original databases and store them in a safe place before using this tool, as any bugs or unexpected behavior may result in data loss. If you have any difficulties using this tool, or have any related feedback, please open an issue on GitHub or email us at mobile@epa.gov.**

## Command Line Interface

To use MOVES' command line tools, first open the Windows command prompt and navigate to the MOVES directory. Then, run the command `setenv`. Afterwards, the MOVES Output Grouper tool can be used by running `MOVESOutputGrouper <--dbs, --dir, or --rsearch> <arguments> [switches]`. Each parameter is explained in more detail below.

For more information on using MOVES' command line tools, see [CommandLineMOVES.md](https://github.com/USEPA/EPA_MOVES_Model/blob/master/docs/CommandLineMOVES.md).

### Input Flags

Tell the MOVES Output Grouper tool what databases to group by using one of the following arguments: `--dbs`, `--dir`, or `--rsearch`. 

* Use `--dbs` when the databases to be grouped are already loaded in your local MariaDB server.
* Use `--dir` when the databases to be grouped are all zipped and located in a single directory. 
* `--rsearch` is useful when there are many databases to be grouped, they are all already loaded in your local MariaDB server, and the database names share common elements.

By default, the output of the tool is a grouped database stored in your local MariaDB server. If you want a zipped version for easy sharing, use the `--out` argument.

The table below details all command line arguments for the MOVES Output Grouper tool. Running `MOVESOutputGrouper -h` will provide the same information.

| Input Flag       | Description                                     | Default Value | 
| ---------------- | ----------------------------------------------- | ------------- |
| --dbs             | List of databases in MariaDB to be grouped (names should be comma- or space-separated if quoted). If this flag is supplied, do not use `--dir` or `--rsearch`. | "" |
| --dir             | Input directory containing zipped databases to be grouped. If this flag is supplied, do not use `--dbs` or `--rsearch`. Each .zip file should contain one database with no directory structure (i.e., the root of the .zip file should contain all of the database's .frm, .myi, and .myd files) | "" |
| --rsearch         | Databases matching the regular expression provided using the MariaDB connection will be grouped. If this flag is supplied, do not use `--dbs` or `--dir`. Make sure to escape characters to "\\\\d" instead of "\\d" in your regular expression. Also, use quotes to surround the expression as some characters may cause errors if not. | "" |
| --grouped_name    | The name of the resulting grouped database. If a database with this name already exists in MariaDB, it will be overwritten. | first database name + "\_grouped" |
| --reload_existing | When loading zipped databases, any databases that already exist in the data directory are dropped and reloaded. | `false` |
| --drop_loaded     | Drops databases from MariaDB that were grouped. | `false`       |
| --log_db_struct   | Output database structure is saved to the log file. | `false`   |
| --out             | By default, the resulting grouped database is available in MariaDB. Optionally, use this argument to supply a directory path where the grouped database will also be saved as a .zip file. | "" |
| --verbose         | verbose program output when performing database file management | `false` |
| --version         | display the program version number              | `false`       |
| --u               | Username for the MariaDB server connection.     | `moves`       |
| --p               | Password for the MariaDB server connection.     | `moves`       |
| --net             | Network type for the MariaDB server connection. | `tcp`         |
| --ip              | IP address for the MariaDB server connection.   | `localhost`   |
| --port            | Port number for the MariaDB server connection.  | Uses value in MOVES' `mysql.txt`, otherwise `3306` |

Flags with boolean values, i.e., `true` or `false`, do not need to be given an explicit value. For example, if `--drop_loaded` is present, then it is evaluated as a `true` value. Otherwise, it will be considered `false`.

### Examples

Some examples of different use cases are given below.

1. The output from 10 parallel runs are stored on a shared drive at S:\MOVESOutput\RunGroup1. The following command groups all of them into a database called "demo_out":

   `MOVESOutputGrouper --dir=S:\MOVESOutput\RunGroup1 --grouped_name=demo_out`

2. After running the command above, you will have all of the individual databases in your MariaDB data directory, along with the resulting "demo_out" database. If you rerun the tool, it will save time by not reloading any of the individual zipped databases that are already present. However, if any of the local databases are corrupt or out of date, you can force the tool to reload the zipped databases using the `--reload_existing` flag:

   `MOVESOutputGrouper --dir=S:\MOVESOutput\RunGroup1 --grouped_name=demo_out --reload_existing`

3. To prevent the tool from cluttering your MariaDB data directory, you can use the `--drop_loaded` flag to drop all of the individual databases that are grouped by the tool:

   `MOVESOutputGrouper --dir=S:\MOVESOutput\RunGroup1 --grouped_name=demo_out --drop_loaded`

4. If you want to combine output databases that are present locally (for example, you ran separate MOVES runs that all used separate output databases, but you want to post-process them as if they all used a single output database), you can use the `--dbs` argument. In this example, there are three output databases: 1_demo_out, 2_demo_out, and 3_demo_out:

   `MOVESOutputGrouper --dbs=1_demo_out,2_demo_out,3_demo_out --grouped_name demo_out`

5. If your output databases that are present locally all have similar names, you can use the `--rsearch` argument to select database names using a regular expression instead of using `--dbs`. Continuing the above example, if your database names are 1_demo_out, 2_demo_out, and 3_demo_out, you can group them with the following command:

   `MOVESOutputGrouper --rsearch="\\d+_demo_out" --grouped_name=demo_out`

6. To save the resulting grouped database as a .zip file for easy sharing, use the `--out` argument.

   `MOVESOutputGrouper --dir=S:\MOVESOutput\RunGroup1 --grouped_name=demo_out --out=S:\GroupedOutput`

   The tool will save demo_out.zip to S:\GroupedOutput
