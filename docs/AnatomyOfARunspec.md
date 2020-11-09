# Anatomy of a MOVES Runspec

This document describes each element found in a MOVES run specification (runspec) and how it is used. It is intended to help users read and interpret a runspec file outside of the MOVES GUI. The MOVES GUI provides layers of error checking to prevent users from creating non-functional runspecs. The details of this error checking are outside the scope of this document. Therefore this document should not be seen as a guide for how to write a runspec by hand. 

MOVES runspecs are formatted as xml and follow standard xml conventions.  Each heading in the rest of the document refers to a specific xml element in a runspec. Subelements are noted with subheadings, with element and subelement names indicated in **bold text**. Likewise, element attributes are indicated in *italic text*. Each element contains example text from a MOVES runspec. For better readability, attribute values are presented in plaintext, but all attribute values should be enclosed in quotation marks (eg. `id="1"`) in a MOVES runspec.

## Description

```xml
<description><![CDATA[Runspec description text goes here]]></description>
```

The description field is entered in the Description section of the MOVES GUI.

## Models

```xml
<models>
    <model value="ONROAD"/>
</models>
```

The **models** element is a required element for a MOVES runspec. It has a single subelement of type **model**. The **model** subelement has only one attribute: *value*. 

Valid entries for *value* are:

* ONROAD - used to select an onroad model run
* NONROAD - used to select a nonroad model run

## Model scale

```xml
<modelscale value="Inv"/>
```

The **modelscale** element is a required element for a MOVES runspec. It has only one attribute: *value*. 

Valid entries for *value* attribute are:

* Inv - used to select inventory calculations and outputs from the MOVES run
* Rates - used to select rates calculations and outputs from the MOVES run

## Model domain

```xml
<modeldomain value="NATIONAL"/>
```

The **modeldomain** element is a required element for a MOVES runspec. It has only one attribute: *value*. 

Valid entries for *value* are:

* NATIONAL - used to select a default scale (also known as national scale) run. This is the only option for a Nonroad run.
* SINGLE - used to select a county scale run
* PROJECT - used to select a project scale run

## Geographic selections

```xml
<geographicselections>
        <geographicselection type="COUNTY" key="8101" description="COLORADO - Pueblo County"/>
</geographicselections>
```

The **geographicselections** element is a required element for a MOVES runspec. It has a single type of subelement: **geographicselection**. For this element there are three attributes: *type*, *key*, and *description*. For an onroad default scale run, more than one **geographicselection** may be added, indicating that the MOVES run will include more than one county.

Valid entries for *type* are:

* COUNTY - this is the default value for most MOVES runs
* NATION - this value is selected in the GUI by choosing "Nation" in the "Advanced Features" section under "Region Aggregation"

The value entered for *key* should be the countyID (which is the same as a county's FIPS code) associated with the specified geographic selection in the county table in the MOVES database.  In the case where `type="NATION"`, the value for *key* should be "0".

The *description* attribute is a user aide and is not used by MOVES at runtime.

## Timespan

```xml
<timespan>
    <year key="2014"/>
    <month id="1"/>
    <day id="2"/>
    <day id="5"/>
    <beginhour id="1"/>
    <endhour id="24"/>
    <aggregateBy key="Hour"/>
</timespan>
```

The **timespan** element is a required element for a MOVES runspec. It defines the timespans(s) covered by the MOVES run. When running MOVES at project scale, only one year,month,day,hour combination can be covered by the runspec. The **timespan** element has seven subelements: **year**, **month**, **day**, **beginhour**, **endhour**, and **aggregateBy**.

### year

The **year** subelement indicates the calendar year for the model run. More than one calendar year can be chosen by including multiple **year** subelements. The **year** subelement has only one attribute: *key*. Entries for *key* are given as a four digit year.

### month

The **month** subelement indicates the month of the year for the model to run. More than one month can be selected by including multiple **month** subelements. Each month chosen will be run for each of the years defined in the runspec. The **month** subelement has only one attribute: *id*. Entries for *id* are given as a one or two digit month (eg. `id="2"`, or `id="12"`) and match the MOVES monthID, which starts at 1 for January and ends at 12 for December.

### day

The **day** subelement indicates the types of days for the model to run. Up to two **day** subelements can be included within **timespan**. Each type of day chosen will be run for every combination of year and month in the runspec. The **day** subelement has only one attribute: *id*.

Valid entries for *id* are:

* 2 - this indicates weekdays

* 5 - this indicates weekends

### beginhour

The **beginhour** subelement indicates the first hour of the day to be included in the model run. Only one **beginhour** subelement can be included within **timespan**. The **beginhour** subelement has only one attribute: *id*. Entries for *id* are given as a one or two digit integer between 1 and 24  (eg. `id="14"`).  

### endhour

The **endhour** subelement indicates the last hour of the day to be included in the model run. Only one **endhour** subelement can be included within **timespan**. The **endhour** subelement has only one attribute: *id*. Entries for *id* are given as a one or two digit integer between 1 and 24 (eg. `id="14"`). 

### aggregateBy

The **aggregateBy** subelement indicates the degree of temporal pre-aggregation that should be done to the input data before the model runs. In the MOVES GUI this is selected in the "Time Aggregation" box of the "Advanced Features" section. The **aggregateBy** subelement has only one attribute: *key*. 

Valid entries for *key* are:

* Hour - This is the default value, and the only value available for a Nonroad or Rates mode run. 
* Day - Pre-aggregate inputs by day of the week
* Month - Pre-aggregate inputs by month of the year
* Year - Pre-aggregate inputs by calendar year

## Onroad vehicle selections

```xml
<onroadvehicleselections>
    <onroadvehicleselection fueltypeid="1" fueltypedesc="Gasoline" sourcetypeid="21" sourcetypename="Passenger Car"/>
    <onroadvehicleselection fueltypeid="2" fueltypedesc="Diesel Fuel" sourcetypeid="31" sourcetypename="Passenger Truck"/>
 </onroadvehicleselections>  
```

The **onroadvehicleselections** element selects the fueltype and sourcetype combinations to be included in the MOVES run. This element and its subelements are only required for onroad MOVES runs. 

The **onroadvehicleselections** element has a single type of subelement: **onroadvehicleselection**. Many **onroadvehicleselection** subelements may be included in the runspec. For **onroadvehicleselection** there are four attributes that should be entered: *fueltypeid*, *fueltypedesc*, *sourcetypeid*, and *sourcetypename*.

Values for *fueltypeid* and *fueltypedesc* should be taken from the `fueltype` table in the MOVES default database. 

Values for *sourcetypeid* and *sourcetypename* should be taken from the `sourceusetype` table of the MOVES default database.

## Offroad vehicle selections

```xml
<offroadvehicleselections>
        <offroadvehicleselection fueltypeid="3" fueltypedesc="Compressed Natural Gas (CNG)" sectorid="2" sectorname="Construction"/>
        <offroadvehicleselection fueltypeid="3" fueltypedesc="Compressed Natural Gas (CNG)" sectorid="3" sectorname="Industrial"/>
</offroadvehicleselections>
```

The **offroadvehicleselections** element selects the fueltype and sector combinations to be included in the MOVES run. This element and its subelements are only required for nonroad MOVES runs. 

The **offroadvehicleselections** element has a single type of subelement: **offroadvehicleselection**. Many **offroadvehicleselection** subelements may be included in the reunspec. For **offroadvehicleselection** there are four attributes that should be entered: *fueltypeid*, *fueltypedesc*, *sectorid*, and *sectorname*.

Values for *fueltypeid* and *fueltypedesc* should be taken from the `nrfueltype` table in the MOVES default database. 

Values for *sectorid* and *sectorname* should be taken from the `sector` table of the MOVES default database.

## Offroad vehicle sccs

```xml
<offroadvehiclesccs>
</offroadvehiclesccs>
```

The **offroadvehiclesccs** element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Road types

```xml
<roadtypes>
        <roadtype roadtypeid="1" roadtypename="Off-Network" modelCombination="M1"/>
        <roadtype roadtypeid="2" roadtypename="Rural Restricted Access" modelCombination="M1"/>
        <roadtype roadtypeid="3" roadtypename="Rural Unrestricted Access" modelCombination="M1"/>
        <roadtype roadtypeid="4" roadtypename="Urban Restricted Access" modelCombination="M1"/>
        <roadtype roadtypeid="5" roadtypename="Urban Unrestricted Access" modelCombination="M1"/>
</roadtypes>
```

The **roadtypes** element is a required element for a MOVES runspec. It defines the road types to be included in the MOVES run. Each included roadtype is specified by a **roadtype** subelement with the attributes: *roadtypeid*, *roadtypename*, and *modelCombination*. When creating a MOVES onroad runspec at the national or county scale all roadtypes except for "Nonroad" should be included in the runspec. Likewise, when creating a MOVES nonroad runspec only the nonroad roadtype (roadtypeID 100) should be selected.

Values for *roadtypeid* and *roadtypename* should be taken from the `roadtype` table of the MOVES default database.

Valid entries for *modelCombination* are:

* M1 - this denotes an onroad run
* M2 -  this denotes a nonroad run

## Pollutant process associations

```xml
<pollutantprocessassociations>
    <pollutantprocessassociation pollutantkey="2" pollutantname="Carbon Monoxide (CO)" processkey="2" processname="Start Exhaust"/>
    <pollutantprocessassociation pollutantkey="3" pollutantname="Oxides of Nitrogen (NOx)" processkey="1" processname="Running Exhaust"/>
</pollutantprocessassociations>
```

The **pollutantprocessassociations** element is is a required element for a MOVES runspec and is typically the largest part of a MOVES runspec. It details all of the combinations of pollutants and processes that should be included in the MOVES run. Many of these combinations have prerequisites that must also be included in this element. All of the pollutant and process combinations are enumerated in the `pollutantprocessassoc` table in the MOVES default database. More information on this table can be found in the [MOVES database documentation](MOVESDatabaseTables.md).

The  **pollutantprocessassociations** element is composed of **pollutantprocessassociation** subelements with the attributes: *pollutantkey*, *pollutantname*, *processkey*, and *processname*.

Values for *pollutantkey* and *pollutantname* should be taken from the `pollutant` table of the MOVES default database.

Values for *processkey* and *processname* should be taken from the `emissionprocess` table of the MOVES default database.

## Database selections

```xml
<databaseselections>
        <databaseselection servername="" databasename="db_for_input" description=""/>
</databaseselections>
```

This element is populated by the "Input Data Sets" box in the "Advance Features" section of the MOVES GUI. MOVES uses values from these input database tables instead of those in the MOVES default database. If no input databases are to be used, this element may be omitted from the runspec. The **databaseselections** element may contain several **databaseselection** attributes: *servername*, *databasename*, and *description*.

The value entered for *servername* should be the address of the SQL server that hosts the selected database. MOVES defaults to the same server that contains the default database if this attribute is empty.

The value for *databasename* should be the schema name for the input database.

The value for *description* is not used at runtime, but is helpful for identifying the inputs used for the MOVES run.

## Internal control strategies

```xml
<internalcontrolstrategies>
</internalcontrolstrategies>
```

The **internalcontrolstrategies** element has been deprecated and may be omitted from the runspec.

## Input database

```xml
<inputdatabase servername="" databasename="" description=""/>
```

The element **inputdatabase** indicates the version of the default MOVES database to use. This element has three attributes: *servername*, *databasename*, and *description*. The default value for each of these attributes is an empty string. If this element is omitted, MOVES will use the default database specified in MOVESConfiguration.txt.

The value entered for *servername* should be the address of the SQL server that hosts the selected database. MOVES defaults to the same server that contains the default database if this attribute is empty.

The value for *databasename* should be the schema name for the input database.

The value for *description* is not used at runtime, but is helpful for identifying the inputs used for the MOVES run.

## Uncertainty parameters

```xml
<uncertaintyparameters uncertaintymodeenabled="false" numberofrunspersimulation="0" numberofsimulations="0"/>
```

The **uncertaintyparameters** element has been deprecated  and may be omitted from the runspec. 

## Geographic output detail

```xml
<geographicoutputdetail description="NATION"/>
```

The **geographicoutputdetail** element is set in the "Output Aggregation" box in the "Output Emissions Detail" section of the MOVES GUI. This element has one attribute: *description*.

Valid entries for *description* are:

* NATION - aggregate outputs at over the entire nation
* STATE - aggregate outputs by state
* COUNTY - aggregate outputs by count
* ZONE - aggregate outputs by zone
* LINK - aggregate outputs by link (Project mode only)

## Output emissions breakdown selection

```xml
<outputemissionsbreakdownselection>
        <modelyear selected="false"/>
        <fueltype selected="false"/>
        <fuelsubtype selected="false"/>
        <emissionprocess selected="false"/>
        <onroadoffroad selected="false"/>
        <roadtype selected="false"/>
        <sourceusetype selected="false"/>
        <movesvehicletype selected="false"/>
        <onroadscc selected="false"/>
        <estimateuncertainty selected="false" numberOfIterations="2" keepSampledData="false" keepIterations="false"/>
        <sector selected="false"/>
        <engtechid selected="false"/>
        <hpclass selected="false"/>
        <regclassid selected="false"/>
</outputemissionsbreakdownselection>
```

The **outputemissionsbreakdownselection** element defines granularity of the MOVEs run output. The **outputemissionsbreakdownselection** element has fourteen subelements: **modelyear**, **fueltype**, **fuelsubtype**, **emissionprocess**, **onroadoffroad**, **roadtype**, **sourceusetype**, **movesvehicletype**, **onroadscc**, **estimateduncertainty**, **sector**, **engtechid**, **hpclass**, and **aggregateBy**. These elements are summarized below, for brevity. All of these elements can be toggled by setting the *selected* attribute to "true". MOVES defaults to having none of the options selected if this element is omitted from the runspec.

| subelement               | description                                                                                                                     |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------- |
| **modelyear**            | Allows output of results by model year. It is selectable for all MOVES runs.                                                    |
| **fueltype**             | Allows output of results by fuel type. It is selectable for all MOVES runs.                                                     |
| **fuelsubtype**          | Allows output of results by fuel subtype. It is selectable for nonroad MOVES runs, but requires that fueltype also be selected. |
| **emissionprocess**      | Allows output of results by emission process. It is selectable for all MOVES runs.                                              |
| **onroadoffroad**        | Deprecated and should never be selected                                                                                         |
| **roadtype**             | Allows output of results by road type. It is only selectable for onroad MOVES runs.                                             |
| **sourceusetype**        | Allows output of results by source use type. It is only selectable for onroad MOVES runs.                                       |
| **movesvehicletype**     | Deprecated and should never be selected                                                                                         |
| **onroadscc**            | Allows output of results by SCC. It is selectable for all MOVES runs.                                                           |
| **estimateduncertainty** | Deprecated and should never be selected                                                                                         |
| **sector**               | Allows output of results by nonroad sector. it is only selectable for nonroad MOVES runs.                                       |
| **engtechid**            | Allows output of results by engine tech ID. It is only selectable for nonroad MOVES runs.                                       |
| **hpclass**              | Allows output of results by horsepower class. It is only selectable for nonroad MOVES runs.                                     |
| **regclass**             | Allows output of results by regulatory class. It is only selectable for onroad MOVES runs.                                      |

## Output database

```xml
<outputdatabase servername="" databasename="demo_county_inv_mo" description=""/>
```

The element **outputdatabase** is a required element for a MOVES runspec. It identifies the database to store the MOVES output in. it contains the attributes: *servername*, *databasename*, and *description*.

The value entered for *servername* should be the address of the SQL server that hosts the selected database. MOVES defaults to the same server that contains the default database if this attribute is empty.

The value for *databasename* should be the schema name for the input database.

The value for *description* is not used at runtime, but is helpful for identifying the inputs used for the MOVES run.

## Output time step

```xml
<outputtimestep value="Hour"/>
```

The element **outputtimestep** is a required element for a MOVES runspec. This element sets the degree of temporal aggregation of the model output.  The **outputtimestep** element has only one attribute: *value*. 

Valid entries for *value* are:

* Hour - aggregate by each hour
* 24-Hour Day - aggregate over a full 24 hour day
* Portion of Week - aggregate by weekday/weekend
* Month - aggregate by month
* Year - aggregate by calendar year

## Output vmt data

```xml
<outputvmtdata value="false"/>
```

Allows output vehicle miles traveled (VMT). It can only be set to "true" for onroad runs. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Output sho

```xml
<outputsho value="false"/>
```

Allows output source hours operating (SHO). It can only be set to "true" for onroad runs. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Output sh

```xml
<outputsh value="false"/>
```

Allows output source hours (SH). It can only be set to "true" for onroad runs. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Output shp

```xml
<outputshp value="false"/>
```

Allows output source hours parked (SHP). It can only be set to "true" for onroad runs. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Output shidling

```xml
<outputshidling value="false"/>
```

Allows output source hours idling (shidling). It can only be set to "true" for onroad runs. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Output starts

```xml
<outputstarts value="false"/>
```

Allows output starts. It can only be set to "true" for onroad runs. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Output population

```xml
<outputpopulation value="false"/>
```

Allows output population. It can only be set to "true" for onroad runs. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Scale input database

```xml
<scaleinputdatabase servername="localhost" databasename="cdb_pueblo_mi" description=""/>
```

This selects the input database used by project and county scale runs. This element only needs to be included for MOVES runs at County Scale or Project Scale.

## PM size

```xml
<pmsize value="0"/>
```

This element is deprecated. This element may be omitted from a runspec, or *value* must be set to 0 otherwise. 

## Output factors

```xml
<outputfactors>
    <timefactors selected="true" units="Portions of Week"/>
    <distancefactors selected="true" units="Miles"/>
    <massfactors selected="true" units="Grams" energyunits="Joules"/>
</outputfactors>
```

The **outputfactors** element sets the units presented in the model output. This element has three subelements: **timefactors**, **distancefactors**, **massfactors**. These subelements are summarized below:

### timefactors

The **timefactors** element indicates the output time units. and should match the setting in **outputtimestep**. The *selected* parameter should always equal `true.`

Valid entries for the *units* parameter are:

* Hours - matching **outputtimestep** *value*: Hour
* Days - matching **outputtimestep** *value*: 24-Hour Day
* Portions of Week - matching **outputtimestep** *value*: Portion of Week
* Month - matching **outputtimestep** *value*: Month
* Years - matching **outputtimestep** *value*: Year

### distancefactors

The **distancefactors** element indicates the output distance units. The *selected* parameter should always equal `true.`

Valid entries for the *units* parameter are:

* Miles - output distance values in miles
* Kilometers - output distance values in kilometers

### massfactors

 The **massfactors** element indicates both the output mass and energy units. The *selected* parameter should always equal `true.`

Valid entries for the *units* parameter are:

* Grams - output mass values in grams
* Kilograms - output mass values in kilograms
* Pounds - output mass values in pounds
* U.S. Ton - output mass values in U.S. tons

Valid entries for the *energyunits* parameter are:

* Joules - output energy values in joules
* KiloJoules - output energy values in kilojoules
* Million BTU - output energy values in million British Thermal Units BTUs.

## Save data

```xml
<savedata>
</savedata>
```

The **savedata** element is typically empty in a runspec and is generally only used for debugging purposes. Checking the "Save Data" boxes in the "Masterloopable Components" box of the "Advance Features" section of the MOVES GUI, adds **class** subelements to **savedata**. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

Valid *name* parameters for these subelements are:

* gov.epa.otaq.moves.master.framework.EmissionCalculator
* gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator
* gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator
* gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy
* gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.RateOfProgressStrategy
* gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG
* gov.epa.otaq.moves.master.implementation.ghg.RatesOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator
* gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator
* gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator

## Do not execute

```xml
<donotexecute>
</donotexecute>
```

The **donotexecute** element is typically empty in a runspec and is generally only used for debugging purposes. Checking the "Don't Execute" boxes in the "Masterloopable Components" box of the "Advance Features" section of the MOVES GUI, adds **class** subelements to **donotexecute**. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

Valid *name* parameters for these subelements are:

* gov.epa.otaq.moves.master.framework.EmissionCalculator
* gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator
* gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator
* gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy
* gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.RateOfProgressStrategy
* gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG
* gov.epa.otaq.moves.master.implementation.ghg.RatesOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator
* gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator
* gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator
* gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator

## Generatordatabase

```xml
<generatordatabase shouldsave="false" servername="" databasename="" description=""/>
```

The **generatordatabase** element specifies the database to save the outputs selected in the **savedata** element. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Donotperformfinalaggregation

```xml
<donotperformfinalaggregation selected="false"/>
```

*selected* is a boolean flag that is used for debugging purposes. If left unselected, MOVES will write data to the output database at the level of detail at which it calculates emissions. This element is not necessary to complete a MOVES run and may be omitted from a runspec.

## Lookuptableflags

```xml
<lookuptableflags scenarioid="" truncateoutput="true" truncateactivity="true" truncatebaserates="true"/>
```

This element is not necessary to complete a MOVES run and may be omitted from a runspec.