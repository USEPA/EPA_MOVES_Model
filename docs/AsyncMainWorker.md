# Running MOVES with Asynchronous Main and Worker Processes

One way that MOVES takes advantage of concurrent processing is by splitting the MOVES worker from the main MOVES process. In a nutshell, the MOVES main process generates TODO files, which are picked up by 1 or more MOVES worker processes. The MOVES worker process processes the file and renames it as a DONE file when it is complete. The MOVES main process then picks up DONE files and performs final aggregation. See the [MOVES Overview Report](https://www.epa.gov/moves/moves-onroad-technical-reports) contains more information on MOVES structure.

If necessary, the MOVES main process can build TODO files separately from starting a worker, and DONE files can be picked up at a later point. **However, it is important to note that no unit conversion, aggregation, or post processing is performed in this mode.** Only use this feature if you understand these ramifications.

1. Use the maketodo command line option to build TODO files without starting a worker. These bundles will get saved to the folder specified as the "saveTODOPath" in `maketodo.txt`.

    ```cmd
    ant maketodo -Drunspec=filename.mrs
    ```

2. Start workers on the command line to process the TODO files into DONE files. You made need to edit `manyworkers.txt` to specify the location of the TODO files as the "sharedDistributedFolderPath".

    ```cmd
    ant 3workers
    ```

    Note that the workers will delete the TODO files that they have processed, and they will save the DONE files to the same location as the TODO files were found.

3. Use the Tools > Process DONE Files menu option and/or use the `ant pickup` command to process the DONE files. The rest of this document explains how to do this.

## Process DONE Files Tool

1. In the MOVES GUI, select Tools > Process Done Files.
2. Click "Browse DONE Files...", locate the directory where the DONE files are located, and open the first one. This will open another popup, displaying some information about the DONE file. Click "OK".
3. Enter the output database name that you wish to use (MOVES will not default to using the one provided in the RunSpec).
4. Click "Use".
5. Click "Save" if you wish to generate a "PDSpec" file for use with the command line version of this tool, or if you wish to come back to the GUI version of the tool at a later point to actually process the DONE files (using the "Load" button)
6. Click "Initiate Processing"

## ant pickup

1. Create a PDSpec XML file (as in, "Process Done Specification" file):

    ```xml
    <pdspec>
        <pdentry>
            <masterid>###</masterid>
            <directory>/path/to/directory/with/DONEfiles</directory>
            <output servername="" databasename="output_database_name" />
        </pdentry>
    </pdspec>
    ```

    Replace `###` with the long numeric prefix given to all DONE files (do not include the bundle number here; DONE files are named like "prefix_bundlenumber_DONE", and we just want the prefix part here).

    Enter the path to the folder where the DONE files are saved in the `<directory>` tag. This can be relative to where MOVES is installed or absolute.

    Enter the output database name in the `<output>` tag's `databasename` attribute.

2. Run the pickup command, using your XML file created in the previous step.

    ```cmd
    ant pickup -Dpdspec=pdSpec.xml
    ```
