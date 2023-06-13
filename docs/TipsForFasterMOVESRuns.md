# Tips For Faster MOVES Runs

This document is meant to outline tips and techniques that users can leverage to improve MOVES run times. They are listed roughly in order from most to least effective. However, MOVES performance depends on many factors and is especially sensitive to the hardware and database setup and concurrent processes a computer is running. Therefore, while these steps serve as a rough guide for how to speed up MOVES, no guarantee can be made that *any* of these approaches will improve performance for all users. For the same reason, it's also not possible to provide precise information on how much runtime improves for each step. 

## At A Glance

The tables below give a quick overview of the system configuration considerations and RunSpec setup steps that can be taken to improve MOVES runtimes, along with their relative effectiveness.

### System Configuration Considerations

| Step                                                                      | Effectiveness |
| ------------------------------------------------------------------------- | ------------- |
| Run MOVES on a computer with a solid-state hard drive                     | High          |
| Prevent antivirus applications from scanning MOVES temporary files        | High          |
| Run MOVES on a computer with multiple logical processors                  | Medium        |
| Run MOVES with a clean database data folder                               | Medium        |
| Run MOVES on a partition or hard drive separate from the operating system | Low           |
| Run MOVES on a network with multiple worker computers or VMs                     | Low           |

### MOVES Run Construction

| Step                                                      | Effectiveness |
| --------------------------------------------------------- | ------------- |
| Remove unnecessary pollutants and, if possible, processes | High          |
| Run evaporative processes by themselves                   | Medium        |
| Split RunSpecs by source type                             | Medium        |
| Provide drive cycles or opModes for project-scale runs    | Low           |

## System Configuration Considerations

Many of MOVES calculations are done in SQL. These calculations involve querying the default and input databases, creating temporary tables and files, and doing table joins, and writing the outputs. These operations mean that the bottleneck for most MOVES runs is not CPU or RAM availability, but the read/write speed of the computer's hard drive.

### 1. MOVES runs faster with solid-state hard drives

Using hard drives with faster read and write speeds will generally improve MOVES performance. Solid-state hard drives are becoming cheaper and more common, and have read/write speeds that are orders of magnitude faster than hard disk drives. This translates directly to having much faster MOVES runs.

**Note:** in the case of a computer with multiple hard drives, the database must be running on the fast drive in order for it to have an effect.

### 2. Antivirus applications slow MOVES down if they scan MOVES' temporary files

If an active antivirus application must scan each temporary file that MOVES generates before it can be used, runs can easily take twice as long to complete as they normally would. In this case, a significant performance increase may be obtained by excluding the following directories from the antivirus application's checks:

* manyworkers
* MOVESTemporary
* NonroadTemporaryData
* sharedwork
* WorkerFolder

    **Note:** In the default configuration, all of the above folders are found in the MOVES installation directory. However, the following directories can be reconfigured:

    * manyworkers is set via the `workFolderPath` directive in manyworkers.txt
    * sharedwork is set via the `sharedDistributedFolderPath` directive in MOVESConfiguration.txt, manyworkers.txt, and maketodo.txt
    * workerfolder is set via the `workFolderPath` directive in WorkerConfiguration.txt
    
    Therefore, an alternative to adding an exclusion for these specific directories is to reconfigure MOVES to use alternative locations that are already not scanned by antivirus. Note that the MOVESTemporary and NonroadTemporaryData directories are not configurable.

The executables used during MOVES runtime are:

* externalcalculatorgo64.exe
* externalgenerator64.exe
* java.exe 
* nonroad.exe

    **Note:** In the default configuration, java.exe is located in the jre/bin subfolder of the MOVES installation directory. However, if MOVES is running from source, it is likely that java.exe is actually located wherever the Java JDK is installed.

    The other executables (externalcalculatorgo64.exe, externalgenerator64.exe, and nonroad.exe) are run from within subdirectories of manyworkers and workerfolder.

In addition to the above, the MariaDB executables ("mysqld.exe" and "mariadbd.exe") and data folder should also be excluded from the antivirus application's checks.

### 3. MOVES runs faster on computers with multiple processors

MOVES is written to use both multiple CPU threads and multiple SQL connections concurrently. Computers with few logical processors (1 or 2) cannot take advantage of this concurrency, but those with 4 or more logical processors can. 

One way that MOVES takes advantage of concurrent processing is by splitting the MOVES worker from the main MOVES process. In this framework, multiple workers can be [run from the command line](CommandLineMOVES.md#launching-moves-workers) to process TODO files in parallel and speed up a MOVES run. 

There are limits to the performance gains that can be achieved by adding more workers. Not all TODO files generated in a MOVES run are the same size. Often a run will have many small TODO files, and just a few very large ones. The large TODO files are the bottlenecks in this case. If there is a worker running for each TODO file generated, the small files will be processed quickly leaving extra workers idle while the large files are being processed. Additionally, each active worker adds more read and write operations that will exacerbate the bottleneck caused by a slow hard drive. At most a MOVES run can use a number of workers equal to the number of logical processors minus two (one for the operating system and one for the main MOVES program). 

### 4. Run MOVES with a clean database data folder

Database servers work faster if they are maintaining less data. The more data there is to track, the more the database engine uses the hard disk and CPU for overhead operations. Routinely archiving MOVES input and output databases to locations outside the database data folder helps minimize this overhead. 

### 5. MOVES runs faster on drives without operating systems on them

MOVES runs are demanding of disk usage, but many operating systems perform regular disk operations in the background. The operating system will always take priority over MOVES, and this can slow MOVES down. 

Running MOVES on a drive that can handle many read/writes simultaneously which doesn't have an operating system can reduce competition and help MOVES run faster. This is generally only a small improvement.

### 6. Run MOVES on a network with multiple worker computers or VMs

This tip is related to #2 above. That tip discusses running multiple workers on the same computer as the main MOVES process. However, MOVES workers can be running on separate computers from the main MOVES process. MOVES can take advantage of distributed computing resources by using a shared directory to save its work bundles to. To do so:

1. Install the same version of MOVES on all computers (or virtual machines).

2. For all installations, modify the `sharedDistributedFolderPath` setting in the following configuration files to point to a common directory (e.g., `\\sharedlocation\MOVESSharedWork`):

    * maketodo.txt
    * manyworkers.txt
    * MOVESConfiguration.txt
    * WorkerConfiguration.txt

3. After starting a run, log onto the other computers and start the worker(s). See [CommandLineMOVES.md](CommandLineMOVES.md#launching-moves-workers) for specific instructions on how to do this step.

When a run is started, MOVES reads the RunSpec and generates bundles of work. These bundles are named with the randomly determined ID (this changes for every run), along with the bundle number and the label "TODO". They are saved in the `sharedDistributedFolderPath`.

Any MOVES worker that is monitoring the `sharedDistributedFolderPath` will pick up "TODO" bundles. While it is working on the bundle, it will rename it to "InProgress" so that other workers don't pick up the same bundle. When it is done, the worker will rename it to "DONE" and then look for more bundles.

The main MOVES process will look for "DONE" bundles with its ID, and then delete the bundle when it is done absorbing the data from it.

A few additional notes:

* Since MOVES labels bundles with a unique identifier for each run, multiple instances of MOVES can simultaneously use the same `sharedDistributedFolderPath` with the same pool of workers. However, each main instance of MOVES must be running on its own computer (or virtual computer).  
* With the `-Dnoshutdown=1` flag, workers can be run independently of the main MOVES program. That is, a pool of workers can be started and left running in between MOVES runs.
* MOVES and its workers can be identified in the `sharedDistributedFolderPath` by their ID file. The timestamp on these files will update every minute to signify that the process is still running. If a file's timestamp stops updating, it is likely that the process has died.
* Only one instance of MOVES can be run at a time on a single computer.
* One or more MOVES workers can run on a single computer.
* One main instance of MOVES and one or more MOVES workers can be run simultaneously on the same computer, but this is not required.

## MOVES Run Construction

MOVES performance is highly sensitive to the amount of work that needs to be done, so smaller runs go faster than large ones. This is hardly a surprise! It is less intuitive that splitting a single large MOVES run into many smaller ones, despite resulting in the same output, can help to reduce the overall MOVES runtime required to get results. 

Each of these techniques work best when they are paired with running MOVES in [batch mode](CommandLineMOVES.md), so that a single command line operation can run multiple RunSpecs, reducing the need for manual intervention at the beginning and end of each run.

### 1. Remove unnecessary pollutants

Each pollutant and process included in a MOVES run adds additional calculations which take additional time. Some pollutant and process combinations require significantly more calculations than others. For limited-scope runs, this doesn't cause any problems, but it can have a significant impact on runtime for large-scope MOVES runs. 

Therefore, it's good to consider which outputs are truly necessary before adding pollutants and processes to the run. Where possible, remove unnecessary pollutants and processes. 

### 2. If possible, remove unnecessary processes or split them into their own RunSpec

In general, each process runs its own generator. Unlike MOVES calculators, all MOVES generators are run by the master and must be done in series. In general, doing separate runs for each process can speed up the total MOVES runtime, however, there are a few things to note:

- Crankcase processes add very little runtime to their exhaust counterparts and should be paired with them.
- Refueling processes require MOVES to run the running process as well, so splitting refueling away from the running process will not improve runtime.
- Brakewear and tirewear processes also depend on running exhaust. However, adding them to a run with the running process will result in a slower run.

### 3. Split RunSpecs by source type 

Including all source types in a RunSpec results in large intermediate tables that need to be joined together. In SQL, some join operations have an exponential relationship with the number of rows involved in the join (especially if the data in question takes up more space than there is available RAM). This can have a significant impact, particularly when some of the intermediate tables in MOVES contain millions of records.

Therefore, a method of speeding up runs is to include only a single HPMS class or a single source type in a RunSpec. For example, if all source types are to be modeled, 13 RunSpecs could be developed, all identical except for the source type selected. Note that the same input database can be used for each RunSpec, as an input database may contain data for source types that are not present in the current run. While this causes some overhead and redundant calculations, it reduces the number of rows in the SQL joins, resulting in the total runtime of the smaller runs to be faster than the runtime of the single, large run.

### 4. Provide drive cycles or opModes for project-scale runs

In project scale, if only a link average speed is provided, MOVES will find a set of representative drive cycles and then calculate the needed operating mode distributions for each link. These calculations take time and can slow down a run. Providing either the drive cycles or operating mode distributions for each link allows MOVES to skip some of those calculations and can improve project scale run times.  
