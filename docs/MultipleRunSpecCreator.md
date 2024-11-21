# Multiple RunSpec Creator

The Multiple RunSpec Creator is a tool available in the Tools dropdown menu. Please note that this tool was built for earlier versions of MOVES and has not been thoroughly tested with the current version of MOVES, so using it may result in errors.

## Overview

The tool uses the currently loaded RunSpec as a template for the new RunSpecs it will generate. It will duplicate the template RunSpec and make substitutions for county, year, and database selections, pulling the data for these substitutions from the control file. It also creates a BAT file that can be used to automatically execute all the newly created RunSpecs.

## Using the Tool

1. Create your template RunSpec, making all the typical selections that you would make for your analysis. You do not need to save your template RunSpec, though you may if you wish to.
   * If you are making a Default Scale RunSpec, you can select all the calendar years and counties that you will want to make individual RunSpecs for in this step. This will make the next steps easier.
   * If you are making a RunSpec for County Scale or Project Scale, you will only be able to select a single calendar year and county in the RunSpec. This is okay, as you can specify the other calendar years and counties in the next steps.
2. Open the Multiple RunSpec Creator tool and click "Create Template...". This will create a template control file (based on the selections made in your template RunSpec). After following the prompt on the screen (and saving it with the .xlsx extension), locate and open the template control file that you just created. If you had multiple years or counties selected, the template will contain rows for each combination of year and county. Otherwise, it will contain a single row with the county and year selected in your template RunSpec.
3. Add additional rows (if necessary) for the years and counties combinations that you wish to run.
   * CountyID: Enter the countyID (aka FIPS code) in this column
   * County Description: This is an optional field. By default, it contains "County Name, State Abbr (CountyID)", but this format is not enforced
   * Year: enter the calendar year in this column
   * Additional Text Name: text in this column will be used as part of the generated RunSpec's file name
   * Primary MOVES DB: this is the name of the default database
   * Output DB: this is the name of the output database specified in the RunSpec.
   * Domain DB: this is the name of the County or Project Scale database, which is specified on the "Create input database" panel.
   * Advanced Features DB: keep this column empty, as this feature does not work as expected
   * User DB: keep this column empty, as this feature does not work as expected
4. Save your Excel control file. Back in the tool, click Browse and locate your Excel control file.
5. Add a File Name Prefix. The generated RunSpecs will be named as follows: "Prefix_countyID_yearID_AdditionalTextName.mrs"
6. Select an output directory, where the created RunSpecs will be saved to.
7. Click "Create RunSpecs".
8. Review the RunSpecs created by the tool. You can do so by opening them in MOVES and ensuring the selections made are as expected. If necessary, build all expected input county/project databases.
9. Execute the RunSpecs. You can either do this one by one through the GUI, or you can use/modify the ExecuteRunSpecs.bat file created by the tool (saved to the same output directory, and prefixed with the same prefix as given to the RunSpecs).