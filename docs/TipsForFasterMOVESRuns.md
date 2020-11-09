# Tips For Faster MOVES Runs

This document is meant to outline tips and techniques that users can leverage to improve MOVES run times. They are listed roughly in order from most to least effective. However, MOVES performance depends on many factors and is especially sensitive to the hardware and database setup and concurrent processes a computer is running. Therefore, while these steps serve as a rough guide for how to speed up MOVES, no guarantee can be made that *any* of these approaches will improve performance for all users. For the same reason, it's also not possible to provide precise information on how much runtime improves for each step. 

## At A Glance

The tables below give a quick overview of the hardware considerations, and runspec setup steps that can be taken to improve MOVES runtimes, along with their relative effectiveness.

### Hardware and Database Configuration

| Step                                                                      | effectiveness |
| ------------------------------------------------------------------------- | ------------- |
| Run MOVES on a computer with a solid-state hard drive                     | High          |
| Run MOVES on a computer with multiple logical processors                  | Medium        |
| Run MOVES with a clean database data folder                               | Medium        |
| Run MOVES on a partition or hard drive separate from the operating system | Low           |

### MOVES Run Construction

| Step                                                      | effectiveness |
| --------------------------------------------------------- | ------------- |
| Remove unnecessary pollutants and, if possible, processes | High          |
| Run chemical mechanisms by themselves                     | High          |
| Run evaporative processes by themselves                   | Medium        |
| Split runspecs by source type                             | Medium        |

## Hardware

Many of MOVES calculations are done in SQL. These calculations involve querying the default and input databases, creating temporary tables, and doing table joins, and writing the outputs. These operations mean that the bottleneck for most MOVES runs is not CPU or RAM availability, but the read/write speed of the computer's hard drive.

### 1. MOVES runs faster with solid-state hard drives

Using hard drives with faster read and write speeds will generally improve MOVES performance. Solid-state hard drives are becoming cheaper and more common, and have read/write speeds that are orders of magnitude faster than hard disk drives. This translates directly to having much faster MOVES runs.

**Note:** in the case of a computer with multiple hard drives, the database must be running on the fast drive in order for it to have an effect.

### 2. MOVES runs faster on computers with multiple processors

MOVES is written to use both multiple CPU threads and multiple SQL connections concurrently. Computers with few logical processors (1 or 2) cannot take advantage of this concurrency, but those with 4 or more logical processors can. 

One way that MOVES takes advantage of concurrent processing is by splitting the MOVES worker from the MOVES master. In this framework, multiple workers can be [run from the command line](CommandLineMOVES.md) to process TODO files in parallel and speed up a MOVES run. 

There are limits to the performance gains that can be achieved by adding more workers. Not all TODO files generated in a MOVES run are the same size. Often a run will have many small TODO files, and just a few very large ones. The large TODO files are the bottlenecks in this case. If there is a worker running for each TODO file generated, the small files will be processed quickly leaving extra workers idle while the large files are being processed. Additionally, each active worker adds more read and write operations that will exacerbate the bottleneck caused by a slow hard drive. At most a MOVES run can use a number of workers equal to the number of logical processors minus two (one for the operating system and one for the MOVES master program). 

### 3. Run MOVES with a clean database data folder

Database servers work faster if they are maintaining less data. The more data there is to track, the more the database engine uses the hard disk  and CPU for overhead operations. Routinely archiving MOVES input and output databases to locations outside the database data folder helps minimize this overhead. 

### 4. MOVES runs faster on drives without operating systems on them

MOVES runs are demanding of disk usage, but many operating systems perform regular disk operations in the background. The operating system will always take priority over MOVES, and this can slow MOVES down. 

Running MOVES on a drive that can handle many read/writes simultaneously (not a flash drive) which doesn't have an operating system can reduce competition and help MOVES run faster. This is generally only a small improvement.

## MOVES Run Construction

MOVES performance is highly sensitive to the amount of work that needs to be done, so smaller runs go faster than large ones. This is hardly a surprise! It is less intuitive that splitting a single large MOVES run into many smaller ones, despite resulting in the same output, can help to reduce the overall MOVES runtime required to get results. 

Each of these techniques work best when they are paired with running MOVES in [batch mode](CommandLineMOVES.md), so that a single command line operation can run multiple runspecs, reducing the need for manual intervention at the beginning and end of each run.

### 1. Remove unnecessary pollutants

Each pollutant and process included in a MOVES run adds additional calculations which take additional time. Some pollutant and process combinations require significantly more calculations than others. For limited-scope runs, this doesn't cause any problems, but it can have a significant impact on runtime for large-scope MOVES runs. 

Therefore, it's good to consider which outputs are truly necessary before adding pollutants and processes to the run. Where possible, remove unnecessary pollutants and processes. 

### 2. Run chemical mechanisms by themselves

The above behavior is especially true of the chemical mechanism pollutants (CB5, CB06, etc.). These chemical mechanisms are selected in the runspec as though they were a single pollutant, but they actually result in the output of dozens of different pollutants and have many pre-requisite processes. A MOVES run *without* chemical mechanisms can skip a large swatch of code and processing which are only used for computing chemical mechanisms pollutants. Chemical mechanism speciation is also done entirely in SQL, which can exacerbate any existing bottlenecks with drive read and write speeds.

### 3. If possible, remove unnecessary processes or split them into their own runspec

In general, each process runs its own generator. Unlike MOVES calculators, all MOVES generators are run by the master and must be done in series. In general, doing separate runs for each process can speed up the total MOVES runtime, however, there are a few things to note:

- Crankcase processes add very little runtime to their exhaust counterparts and should be paired with them.
- Refueling processes require MOVES to run the running process as well, so splitting refueling away from the running process will not improve runtime.
- Brakewear and tirewear processes also depend on running exhaust. However, adding them to a run with the running process will result in a slower run.

### 4. Provide drive cycles or opModes for project-scale runs

In project scale, if only a link average speed is provided, MOVES will find a set of representative drive cycles and then calculate the needed operating mode distributions for each link. These calculations take time and can slow down a run significantly. Providing either the drive cycles, or operating mode distributions for each link allows MOVES to skip some of those calculations and can significantly improve project scale run times.  
