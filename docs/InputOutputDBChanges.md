# MOVES Input/Output Database Schema Changes

This document outlines changes between MOVES3 and MOVES4 that have been made to input databases at County or Project Scale and output database changes. All tables mentioned are further documented in the [MOVES Database Documentation](MOVESDatabaseTables.md) and specific columns are defined in the [MOVES Database Glossary](MOVESGlossary.md). More information on how to build valid input databases can be found in the [Technical Guidance](https://www.epa.gov/state-and-local-transportation/policy-and-technical-guidance-state-and-local-transportation#emission) for state and local users. 

## Input Database Changes

### Tables Modified

#### *hotellingActivityDistribution*

Added column `fuelTypeID`, which allows the operating mode distribution to vary by fuel type.

| Field            | Type        | Null | Key  | Default | Comment                                                      |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| zoneID           | int(11)     | NO   | PRI  |         |                                                              |
| fuelTypeID       | smallint(6) | NO   | PRI  |         |                                                              |
| beginModelYearID | smallint(6) | NO   | PRI  |         |                                                              |
| endModelYearID   | smallint(6) | NO   | PRI  |         |                                                              |
| opModeID         | smallint(6) | NO   | PRI  |         | this table only contains the hotelling operating modes (200, 201, 203, and 204) |
| opModeFraction   | float       | NO   |      |         | fraction of time spent in each operating mode, summing to 1 for each zone, fuel type, and model year group |

Also note that the definition of the operating modes used by this table have changed:

| opModeID | opModeName                                       |
| ---------| ------------------------------------------------ |
| 200      | Extended Idling                                  | 
| 201      | Hotelling Diesel Auxiliary Power Unit (APU)      |
| 203      | Hotelling Shore Power (plug in)                  |
| 204      | Hotelling Battery or All Engines/Accessories Off |

#### *zonemonthhour*

Columns *temperatureCV* AND *relativeHumidityCV* were dropped, and column *molWaterFraction* was added. The removed columns columns were always NULL and the new column is also always NULL in user input (it is a calculated field), so these changes do not change the data requirements for this table.

| Field            | Type        | Null | Key | Default | Comment                                                                   |
|------------------|-------------|------|-----|---------|---------------------------------------------------------------------------|
| monthID          | smallint(6) | NO   | PRI | 0       |                                                                           |
| zoneID           | int(11)     | NO   | PRI | 0       |                                                                           |
| hourID           | smallint(6) | NO   | PRI | 0       |                                                                           |
| temperature      | double      | YES  |     |         | average temperature for the month and hour in Fahrenheit                  |
| relHumidity      | double      | YES  |     |         |                                                                           |
| heatIndex        | double      | YES  |     |         | used during MOVES runtime only, in Fahrenheit                             |
| specificHumidity | double      | YES  |     |         | used during MOVES runtime only, in grams of water per kilogram of dry air |
| molWaterFraction | double      | YES  |     |         | used during MOVES runtime only, in moles of water per mole of ambient air |

### Tables Removed

None.

### Tables Added

None.

## Output Database Changes

### Tables Modified

#### *activitytype*

This table has been renamed *translate_activitytype* and the _activityType_ column was renamed _activityTypeName_. These changes were made to make the *activitytype* table consistent with the newly added *translate_\** tables (see below).

Note that the original schema for the *activitytype* table is now available in the MOVES default database. (In previous versions of MOVES, this table was only available in output databases.) Any references to the *activitytype* table in scripts written for older versions of MOVES will need to be updated to either use the table in the MOVES default database or use the new name and schema present in the output database. 

### Tables Removed

None.

### Tables Added

The following tables were added to allow an output database to contain all relevant ID field decoding information:

* translate_avgspeedbin
* translate_county
* translate_day
* translate_engtech
* translate_fuelsubtype
* translate_fueltype
* translate_hp
* translate_pollutant
* translate_process
* translate_regclass
* translate_roadtype
* translate_sector
* translate_sourcetype
* translate_state

While the information contained in these tables are duplicative of their corresponding tables in the MOVES default database, users may find it helpful to refer to these output database tables when decoding their output. Each table has an \*ID column an \*Name column. For example, *translate_avgspeedbin* contains _avgSpeedBinID_ and _avgSpeedBinName_. 