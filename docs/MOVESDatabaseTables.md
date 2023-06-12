# MOVES Database Tables

## Introduction

This document contains information on all the tables in the MOVES default database, MOVES input database, and MOVES output databases. For each table, this document will explain, briefly, its purpose, schema, and will provide extra information where it is necessary. 

In all MOVES databases, the table names are formatted to be full lower-case. Column names, on the other hand, are generally formatted as [camel case](https://en.wikipedia.org/wiki/Camel_case), with the exception that acronyms are generally always capitalized, unless they are at the beginning of a column name. SQL is not a case-sensitive language, so there are likely to be exceptions. To make the document more readable, table names will follow similar formatting as column names.

## MOVES Default and Input Databases

### activityType

activityType defines the MOVES output activity types, mapping IDs to their physical interpretation.

| Field            | Type                 | Null | Key | Default | Comment |
| ---------------- | -------------------- | ---- | --- | ------- | ------- |
| activityTypeID   | smallint(5) unsigned | NO   | PRI |         |         |
| activityType     | char(20)             | NO   |     |         |         |
| activityTypeDesc | char(50)             | YES  |     |         |         |

### ageCategory

ageCategory defines the age categories used in MOVES.

| Field           | Type        | Null | Key | Default | Comment |
| --------------- | ----------- | ---- | --- | ------- | ------- |
| ageID           | smallint(6) | NO   | PRI | 0       |         |
| ageGroupID      | smallint(6) | NO   | MUL | 0       |         |
| ageCategoryName | char(50)    | YES  |     |         |         |

### ageGroup

ageGroup maps ageGroupIDs to their names in MOVES.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| ageGroupID   | smallint(6) | NO   | PRI | 0       |         |
| ageGroupName | char(50)    | YES  |     |         |         |

### atBaseEmissions

atBaseEmissions contains unadjusted emission rates for air toxics.

| Field           | Type        | Null | Key | Default | Comment                                                                                                                                      |
| --------------- | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| polProcessID    | int(11)     | NO   | PRI | 0       |                                                                                                                                              |
| monthGroupID    | smallint(6) | NO   | PRI | 0       |                                                                                                                                              |
| atBaseEmissions | float       | NO   |     | 0       | base emission rate, in grams per relevant unit of activity depending on the polProcessID. Typically, it is source hours operating or  starts |
| dataSourceID    | smallint(6) | YES  |     |         |                                                                                                                                              |

### atRatio

atRatio  is empty in the default database and may be used during MOVES runtime to  calculate air toxics emissions.

| Field             | Type        | Null | Key  | Default | Comment |
| ----------------- | ----------- | ---- | ---- | ------- | ------- |
| fuelTypeID        | smallint(6) | NO   | PRI  |         |         |
| fuelFormulationID | int(11)     | NO   | PRI  |         |         |
| polProcessID      | int(11)     | NO   | PRI  |         |         |
| minModelYearID    | smallint(6) | NO   | PRI  |         |         |
| maxModelYearID    | smallint(6) | NO   | PRI  |         |         |
| ageID             | smallint(6) | NO   | PRI  |         |         |
| monthGroupID      | smallint(6) | NO   | PRI  |         |         |
| atRatio           | double      | YES  |      |         |         |

### atRatioGas2

atRatioGas2  is empty in the default database and may be used during MOVES runtime to  calculate gaseous air toxics emissions.

| Field         | Type        | Null | Key  | Default | Comment  |
| ------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID  | int(11)     | NO   | PRI  | 0       |          |
| sourceTypeID  | smallint(6) | NO   | PRI  | 0       |          |
| fuelSubtypeID | smallint(6) | NO   | PRI  | 0       |          |
| ATRatio       | float       | YES  |      |         |          |
| ATRatioCV     | float       | YES  |      |         | not used |

### atRatioNongas

atRatioNonGas  is used to calculate non-gaseous air toxics emissions based on VOC emissions.

| Field            | Type        | Null | Key  | Default | Comment  |
| ---------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID     | int(11)     | NO   | PRI  | 0       |          |
| sourceTypeID     | smallint(6) | NO   | PRI  | 0       |          |
| fuelSubtypeID    | smallint(6) | NO   | PRI  | 0       |          |
| modelYearGroupID | int(11)     | NO   | PRI  | 0       |          |
| ATRatio          | double      | YES  |      |         |          |
| ATRatioCV        | double      | YES  |      |         | not used |
| dataSourceId     | smallint(6) | YES  |      |         |          |

### auditLog

auditLog is used in MOVES input databases, and is a log of all the data manager and import actions a user performed.

| Field            | Type          | Null | Key | Default | Comment |
| ---------------- | ------------- | ---- | --- | ------- | ------- |
| whenHappened     | datetime      | NO   | MUL |         |         |
| importerName     | varchar(100)  | NO   | MUL |         |         |
| briefDescription | varchar(100)  | YES  |     |         |         |
| fullDescription  | varchar(4096) | YES  |     |         |         |

 Additionally, this table is used to indicate that the "No I/M Program" box is checked on the I/M Programs tab of the County and Project Data Managers. When this box is checked, the auditlog table gets a row where the *importerName* column contains "I/M Programs Flag" and *briefDescription* is "No data needed".

### averageTankGasoline

averageTankGasoline  contains no data in the default database and is used during runtime.

| Field        | Type        | Null | Key  | Default | Comment |
| ------------ | ----------- | ---- | ---- | ------- | ------- |
| zoneID       | int(11)     | NO   | PRI  | 0       |         |
| fuelYearID   | int(11)     | NO   | PRI  | 0       |         |
| monthGroupID | smallint(6) | NO   | PRI  | 0       |         |
| ETOHVolume   | float       | YES  |      |         |         |
| RVP          | float       | YES  |      |         |         |
| fuelTypeID   | smallint(6) | NO   | PRI  | 0       |         |
| isUserInput  | char(1)     | NO   |      | N       |         |

### averageTankTemperature

averageTankTemperature  contains no data in the default database and is used during runtime.

| Field                    | Type        | Null | Key  | Default | Comment |
| ------------------------ | ----------- | ---- | ---- | ------- | ------- |
| tankTemperatureGroupID   | smallint(6) | NO   | PRI  | 0       |         |
| zoneID                   | int(11)     | NO   | PRI  | 0       |         |
| monthID                  | smallint(6) | NO   | PRI  | 0       |         |
| hourDayID                | smallint(6) | NO   | PRI  | 0       |         |
| opModeID                 | smallint(6) | NO   | PRI  | 0       |         |
| averageTankTemperature   | float       | YES  |      |         |         |
| averageTankTemperatureCV | float       | YES  |      |         |         |
| isUserInput              | char(1)     | NO   |      | N       |         |

### avft

avft specifies how many vehicles use particular engine technology and fuel type combinations. It doesn't contain any data by default, but is a user input table.

| Field           | Type        | Null | Key  | Default | Comment                                                      |
| --------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeID    | smallint(6) | NO   | PRI  |         |                                                              |
| modelYearID     | smallint(6) | NO   | PRI  |         |                                                              |
| fuelTypeID      | smallint(6) | NO   | PRI  |         |                                                              |
| engTechID       | smallint(6) | NO   | PRI  |         |                                                              |
| fuelEngFraction | double      | NO   |      |         | fraction of vehicles within source type and model year which  are the fuel type and engine technology combination |

### avgSpeedBin

avgSpeedBin defines the average speed bins, and their integer IDs, that are used in MOVES.

| Field            | Type        | Null | Key | Default | Comment                                                                            |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------------- |
| avgSpeedBinID    | smallint(6) | NO   | PRI | 0       |                                                                                    |
| avgBinSpeed      | float       | YES  |     |         |                                                                                    |
| avgSpeedBinDesc  | char(50)    | YES  |     |         |                                                                                    |
| opModeIDTirewear | smallint(6) | YES  |     |         | opModeID associated with speed bin for the purposes of modeling tirewear emissions |
| opModeIDRunning  | smallint(6) | YES  |     |         |                                                                                    |

### avgSpeedDistribution

avgSpeedDistribution contains the default average speed distributions in MOVES as a fraction of time, and is also a user input table.

| Field            | Type        | Null | Key  | Default | Comment                                                      |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeID     | smallint(6) | NO   | PRI  | 0       |                                                              |
| roadTypeID       | smallint(6) | NO   | PRI  | 0       |                                                              |
| hourDayID        | smallint(6) | NO   | PRI  | 0       |                                                              |
| avgSpeedBinID    | smallint(6) | NO   | PRI  | 0       |                                                              |
| avgSpeedFraction | float       | YES  |      |         | fraction of SHO in the average speed bin. Sums to 1 for each source type, road type, and hourDay combination |

### baseFuel

baseFuel contains the MOVES fuelFormulation used as the basis for the complex and  predictive fuel effects models (MY < 2001 vehicles).

| Field             | Type         | Null | Key  | Default | Comment                                                 |
| ----------------- | ------------ | ---- | ---- | ------- | ------------------------------------------------------- |
| calculationEngine | varchar(100) | NO   | PRI  |         | the specific model the base fuel should be applied into |
| fuelTypeID        | smallint(6)  | NO   | PRI  |         |                                                         |
| modelYearGroupID  | int(11)      | NO   | PRI  | 0       |                                                         |
| fuelFormulationID | int(11)      | NO   |      |         |                                                         |
| description       | varchar(255) | NO   |      |         | a general description of what the base fuel represents  |
| dataSourceID      | smallint(6)  | NO   |      |         |                                                         |

### coldSoakInitialHourFraction

coldSoakInitialHourFraction contains no data in the default database and is used during MOVES runtime to calculate evaporative emissions.

| Field                       | Type        | Null | Key  | Default | Comment |
| --------------------------- | ----------- | ---- | ---- | ------- | ------- |
| sourceTypeID                | smallint(6) | NO   | PRI  | 0       |         |
| zoneID                      | int(11)     | NO   | PRI  | 0       |         |
| monthID                     | smallint(6) | NO   | PRI  | 0       |         |
| hourDayID                   | smallint(6) | NO   | PRI  | 0       |         |
| initialHourDayID            | smallint(6) | NO   | PRI  | 0       |         |
| coldSoakInitialHourFraction | float       | NO   |      | 0       |         |
| isUserInput                 | char(1)     | NO   | MUL  | N       |         |

### coldSoakTankTemperature

coldSoakTankTemperature contains no data in the default database and is used during MOVES runtime to calculate evaporative emissions.

| Field                   | Type        | Null | Key  | Default | Comment |
| ----------------------- | ----------- | ---- | ---- | ------- | ------- |
| zoneID                  | int(11)     | NO   | PRI  | 0       |         |
| monthID                 | smallint(6) | NO   | PRI  | 0       |         |
| hourID                  | smallint(6) | NO   | PRI  | 0       |         |
| coldSoakTankTemperature | float       | NO   |      | 0       |         |

### complexModelParameterName

complexModelParameterName specifies the short property name and the equation used to derive that  property for the MOVES implementation of the Complex fuel effects models.

| Field         | Type         | Null | Key  | Default | Comment                                                      |
| ------------- | ------------ | ---- | ---- | ------- | ------------------------------------------------------------ |
| cmpID         | smallint(6)  | NO   | PRI  | 0       |                                                              |
| cmpName       | char(25)     | NO   |      |         | the component short name                                     |
| cmpExpression | varchar(500) | NO   |      |         | the equation deriving the component as part of the Complex  model algorithm |

### complexModelParameters

complexModelParameters contains the coefficients used for the Complex fuel effects model algorithms.

| Field        | Type        | Null | Key  | Default | Comment                                 |
| ------------ | ----------- | ---- | ---- | ------- | --------------------------------------- |
| polProcessID | int(11)     | NO   | PRI  | 0       |                                         |
| fuelModelID  | smallint(6) | NO   | PRI  | 0       |                                         |
| cmpID        | smallint(6) | NO   | PRI  | 0       |                                         |
| coeff1       | float       | YES  |      |         | see MOVES Fuel Effects Technical Report |
| coeff2       | float       | YES  |      |         | see MOVES Fuel Effects Technical Report |
| coeff3       | float       | YES  |      |         | see MOVES Fuel Effects Technical Report |
| dataSourceID | smallint(6) | YES  |      |         |                                         |

### county

county defines all the counties used in MOVES, and contains basic meteorological and geographic information about each county.

| Field                | Type        | Null | Key  | Default | Comment                                                      |
| -------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| countyID             | int(11)     | NO   | PRI  | 0       |                                                              |
| stateID              | smallint(6) | NO   | PRI  | 0       |                                                              |
| countyName           | char(50)    | YES  |      |         |                                                              |
| altitude             | char(1)     | YES  |      |         |                                                              |
| GPAFract             | float       | YES  |      |         | "Geographical Phase-In Area" fraction, which is used to help characterize gasoline sulfur content from 2004-2006 |
| barometricPressure   | float       | YES  |      |         | average barometric pressure in the county, in units of inches of mercury |
| barometricPressureCV | float       | YES  |      |         | always NULL and not used                                     |
| countyTypeID         | int(11)     | YES  |      |         |                                                              |
| msa                  | char(255)   | YES  |      |         | US Census Metropolitan Statistical Area the county is in, if applicable |

### countyType

countyType defines the county types used in MOVES. For now, there are only two types: counties that are in US Census Metropolitan Areas, and those that are not.

| Field                 | Type      | Null | Key | Default | Comment |
| --------------------- | --------- | ---- | --- | ------- | ------- |
| countyTypeID          | int(11)   | NO   | PRI |         |         |
| countyTypeDescription | char(120) | YES  |     |         |         |

### countyYear

countyyear contains information  on fuel vapor control programs (e.g. "Stage II") for each county, year combination in MOVES.

| Field                       | Type        | Null | Key  | Default | Comment                                                      |
| --------------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| countyID                    | int(11)     | NO   | PRI  | 0       |                                                              |
| yearID                      | smallint(6) | NO   | PRI  | 0       |                                                              |
| refuelingVaporProgramAdjust | float       | NO   |      | 0       | between 0 and 1, indicating the percent reduction of total  potential vapor losses by state or local programs |
| refuelingSpillProgramAdjust | float       | NO   |      | 0       | between 0 and 1, indicating the percent reduction of refueling  spillage losses by state or local programs |

### crankcaseEmissionRatio

crankcaseEmissionRatio contains emission ratios used to calculate crankcase emissions based on their corresponding exhaust processes.

| Field            | Type        | Null | Key  | Default | Comment                                                      |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| polProcessID     | int(11)     | NO   | PRI  |         |                                                              |
| minModelYearID   | smallint(6) | NO   | PRI  |         |                                                              |
| maxModelYearID   | smallint(6) | NO   | PRI  |         |                                                              |
| sourceTypeID     | smallint(6) | NO   | PRI  |         |                                                              |
| regClassID       | smallint(6) | NO   | PRI  |         |                                                              |
| fuelTypeID       | smallint(6) | NO   | PRI  |         |                                                              |
| crankcaseRatio   | float       | NO   |      |         | ratio of crankcase emissions to exhaust emissions which is used to calculate the crankcase inventory |
| crankcaseRatioCV | float       | YES  |      |         | not used                                                     |

### criteriaRatio

criteriaRatio is empty in the default database, and is used during runtime as part of the  Fuel Effects Generator.

| Field             | Type        | Null | Key  | Default | Comment |
| ----------------- | ----------- | ---- | ---- | ------- | ------- |
| fuelTypeID        | smallint(6) | NO   | PRI  |         |         |
| fuelFormulationID | int(11)     | NO   | PRI  |         |         |
| polProcessID      | int(11)     | NO   | PRI  |         |         |
| pollutantID       | smallint(6) | NO   | PRI  |         |         |
| processID         | smallint(6) | NO   | PRI  |         |         |
| sourceTypeID      | smallint(6) | NO   | PRI  |         |         |
| modelYearID       | smallint(6) | NO   | PRI  |         |         |
| ageID             | smallint(6) | NO   | PRI  |         |         |
| ratio             | double      | YES  |      |         |         |
| ratioGPA          | double      | YES  |      |         |         |
| ratioNoSulfur     | double      | YES  |      |         |         |

### cumTVVcoeffs

cumTVVcoeffs contains the coefficients and associated properties needed for the  calculation of cumulative evaporative tank vapor venting (cold soak).

| Field                   | Type          | Null | Key  | Default | Comment                                                      |
| ----------------------- | ------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| regClassID              | smallint(6)   | NO   | PRI  | 0       |                                                              |
| modelYearGroupID        | int(11)       | NO   | PRI  | 0       |                                                              |
| ageGroupID              | smallint(6)   | NO   | PRI  | 0       |                                                              |
| polProcessID            | int(11)       | NO   | PRI  | 0       |                                                              |
| tvvTermA                | float         | YES  |      |         | the A, B, and C coefficients used in the old-style tank vapor generation  / venting model relationship |
| tvvTermB                | float         | YES  |      |         |                                                              |
| tvvTermC                | float         | YES  |      |         |                                                              |
| tvvTermACV              | float         | YES  |      |         | not used                                                     |
| tvvTermBCV              | float         | YES  |      |         | not used                                                     |
| tvvTermCCV              | float         | YES  |      |         | not used                                                     |
| tvvTermAIM              | float         | YES  |      |         |                                                              |
| tvvTermBIM              | float         | YES  |      |         |                                                              |
| tvvTermCIM              | float         | YES  |      |         |                                                              |
| tvvTermAIMCV            | float         | YES  |      |         | not used                                                     |
| tvvTermBIMCV            | float         | YES  |      |         | not used                                                     |
| tvvTermCIMCV            | float         | YES  |      |         | not used                                                     |
| backPurgeFactor         | double        | YES  |      |         | the percentage of vapor recovered during a diurnal canister backpurge  event, per day |
| averageCanisterCapacity | double        | YES  |      |         | the average evaporative vapor capacity of an evaporative canister in  grams |
| tvvEquation             | varchar(4096) | NO   |      |         | the equation used for the new-style (DELTA) tank vapor generation /  venting model relationship (TVG - TVV) |
| leakEquation            | varchar(4096) | NO   |      |         | the equation used for calculating leaking vehicle tank vapor venting (TVG  - TVV) |
| leakFraction            | double        | YES  |      |         | the percentage of vehicles to apply the leak equation onto during tank  venting calculations |
| tankSize                | double        | YES  |      |         | the average size of a vehicle fuel tank in gallons           |
| tankFillFraction        | double        | YES  |      |         | the average fill level of a vehicle fuel tank in percent     |
| leakFractionIM          | double        | YES  |      |         | the percentage of vehicles to apply the leak equation onto during tank  venting calculations in an IM area |

### dataSource

dataSource contains information on the various data sources used to populate the MOVES default database, with some metadata.

| Field        | Type        | Null | Key | Default | Comment                    |
| ------------ | ----------- | ---- | --- | ------- | -------------------------- |
| DataSourceId | smallint(6) | NO   | PRI | 0       |                            |
| Author       | char(25)    | YES  |     |         |                            |
| Date         | date        | YES  |     |         |                            |
| Sponsor      | char(30)    | YES  |     |         |                            |
| DocumentID   | char(150)   | YES  |     |         | description of data source |
| QualityLevel | char(1)     | YES  |     |         | not used                   |

### dayOfAnyWeek

dayOfAnyWeek maps a dayID to the number of days it represents in a week.

| Field        | Type        | Null | Key | Default | Comment                                               |
| ------------ | ----------- | ---- | --- | ------- | ----------------------------------------------------- |
| dayID        | smallint(6) | NO   | PRI | 0       |                                                       |
| dayName      | char(10)    | YES  |     |         |                                                       |
| noOfRealDays | float       | NO   |     | 1       | indicates the number of days for each dayID in a week |

### dayVMTFraction

dayVMTFraction allocates VMT by the day of the week.

| Field          | Type        | Null | Key | Default | Comment                                                                                                         |
| -------------- | ----------- | ---- | --- | ------- | --------------------------------------------------------------------------------------------------------------- |
| sourceTypeID   | smallint(6) | NO   | PRI | 0       |                                                                                                                 |
| monthID        | smallint(6) | NO   | PRI | 0       |                                                                                                                 |
| roadTypeID     | smallint(6) | NO   | PRI | 0       |                                                                                                                 |
| dayID          | smallint(6) | NO   | PRI | 0       |                                                                                                                 |
| dayVMTFraction | float       | YES  |     |         | Fraction of VMT for each type of day, which sums to 1 within each source type, month, and road type combination |

### dioxinEmissionRate

dioxinEmissionRate contains emission rates for dioxins and furans.

| Field            | Type        | Null | Key  | Default | Comment  |
| ---------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID     | int(11)     | NO   | PRI  | 0       |          |
| fuelTypeID       | smallint(6) | NO   | PRI  | 0       |          |
| modelYearGroupID | int(11)     | NO   | PRI  | 0       |          |
| units            | char(30)    | YES  |      |         |          |
| meanBaseRate     | double      | YES  |      |         |          |
| meanBaseRateCV   | double      | YES  |      |         | not used |
| dataSourceId     | smallint(6) | YES  |      |         |          |

### driveSchedule

driveSchedule lists the drive cycles used in MOVES and their average speeds.

| Field             | Type        | Null | Key | Default | Comment                         |
| ----------------- | ----------- | ---- | --- | ------- | ------------------------------- |
| driveScheduleID   | smallint(6) | NO   | PRI | 0       |                                 |
| averageSpeed      | float       | NO   |     | 0       | average speed of drive schedule |
| driveScheduleName | char(50)    | YES  |     |         |                                 |

### driveScheduleAssoc

driveScheduleAssoc maps drive cycles to the source types and road types they are used for.

| Field           | Type        | Null | Key | Default | Comment                                                                                    |
| --------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------------------------------ |
| sourceTypeID    | smallint(6) | NO   | PRI | 0       |                                                                                            |
| roadTypeID      | smallint(6) | NO   | PRI | 0       |                                                                                            |
| driveScheduleID | smallint(6) | NO   | PRI | 0       | each row is a driveScheduleID which can be used for the source type, road type combination |

### driveScheduleSecond

driveScheduleSecond contains the drive schedules themselves, with second-by-second speeds.

| Field           | Type        | Null | Key | Default | Comment      |
| --------------- | ----------- | ---- | --- | ------- | ------------ |
| driveScheduleID | smallint(6) | NO   | PRI | 0       |              |
| second          | smallint(6) | NO   | PRI | 0       |              |
| speed           | float       | YES  |     |         | units of mph |

### driveScheduleSeconLink

driveScheduleSecondLink is the project-scale user input equivalent of driveScheduleSecond, with an added column for road grade at each second. It contains no data by default.

| Field    | Type        | Null | Key | Default | Comment                            |
| -------- | ----------- | ---- | --- | ------- | ---------------------------------- |
| linkID   | int(11)     | NO   | PRI |         |                                    |
| secondID | smallint(6) | NO   | PRI |         |                                    |
| speed    | float       | YES  |     |         |                                    |
| grade    | float       | NO   |     | 0       | expressed in terms of a percentage |

### e10FuelProperties

e10fuelproperties contains fuel properties used in the calculation of specific E85 fuel effects. See the fuelFormulation table for more details on fuel property columns.

|                      |             |      |      |         |                                                              |
| -------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| Field                | Type        | Null | Key  | Default | Comment                                                      |
| fuelRegionID         | int(11)     | NO   | PRI  |         | the fuel region (see regionCounty table) where the e10 fuel  properties apply. Region 0 represents national and must be included. |
| fuelYearID           | int(11)     | NO   | PRI  |         |                                                              |
| monthGroupID         | smallint(6) | NO   | PRI  |         |                                                              |
| RVP                  | double      | YES  |      |         |                                                              |
| sulfurLevel          | double      | YES  |      |         |                                                              |
| ETOHVolume           | double      | YES  |      |         |                                                              |
| MTBEVolume           | double      | YES  |      |         |                                                              |
| ETBEVolume           | double      | YES  |      |         |                                                              |
| TAMEVolume           | double      | YES  |      |         |                                                              |
| aromaticContent      | double      | YES  |      |         |                                                              |
| olefinContent        | double      | YES  |      |         |                                                              |
| benzeneContent       | double      | YES  |      |         |                                                              |
| e200                 | double      | YES  |      |         |                                                              |
| e300                 | double      | YES  |      |         |                                                              |
| BioDieselEsterVolume | double      | YES  |      |         |                                                              |
| CetaneIndex          | double      | YES  |      |         |                                                              |
| PAHContent           | double      | YES  |      |         |                                                              |
| T50                  | double      | YES  |      |         |                                                              |
| T90                  | double      | YES  |      |         |                                                              |

### emissionProcess

emissionProcess lists all the processes in MOVES and contains information on which parts of the model use them.

| Field                 | Type                 | Null | Key | Default | Comment                                             |
| --------------------- | -------------------- | ---- | --- | ------- | --------------------------------------------------- |
| processID             | smallint(6)          | NO   | PRI | 0       |                                                     |
| processName           | char(50)             | YES  |     |         |                                                     |
| SCCProcID             | char(1)              | YES  |     |         |                                                     |
| occursOnRealRoads     | char(1)              | NO   |     | Y       |                                                     |
| shortName             | varchar(50)          | YES  |     |         |                                                     |
| processDisplayGroupID | smallint(6) unsigned | YES  |     |         | used for displaying processes in the MOVES GUI      |
| isAffectedByOnroad    | tinyint(1)           | YES  |     | 1       | 1 if process is used to calculate onroad inventory  |
| isAffectedByNonroad   | tinyint(1)           | YES  |     | 0       | 1 if process is used to calculate nonroad inventory |

### emissionRate

emissionRate contains emission rates for most exhaust-based pollutants which are same regardless of the vehicle age.

| Field            | Type        | Null | Key | Default | Comment                                                          |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------- |
| sourceBinID      | bigint(20)  | NO   | PRI | 0       |                                                                  |
| polProcessID     | int(11)     | NO   | PRI | 0       |                                                                  |
| opModeID         | smallint(6) | NO   | PRI | 0       |                                                                  |
| meanBaseRate     | float       | YES  |     |         | units for total energy consumption is in KJ/hr, g/hr otherwise |
| meanBaseRateCV   | float       | YES  |     |         | not used                                                         |
| meanBaseRateIM   | float       | YES  |     |         | contains the rate for IM programs                                |
| meanBaseRateIMCV | float       | YES  |     |         | not used                                                         |
| dataSourceId     | smallint(6) | YES  |     |         |                                                                  |

### emissionRateAdjustment

emissionRateAdjustment contains multiplicative adjustments that are applied to rates from emissionRate.

| Field                  | Type        | Null | Key | Default | Comment                                                 |
| ---------------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------- |
| polProcessID           | int(11)     | NO   | PRI |         |                                                         |
| sourceTypeID           | smallint(6) | NO   | PRI |         |                                                         |
| regClassID             | smallint(6) | NO   | PRI |         |                                                         |
| fuelTypeID             | smallint(6) | NO   | PRI |         |                                                         |
| beginModelYearID       | smallint(6) | NO   | PRI |         |                                                         |
| endModelYearID         | smallint(6) | NO   | PRI |         |                                                         |
| emissionRateAdjustment | double      | YES  |     |         | raw multiplicative factor used to adjust emission rates |
| dataSourceID           | smallint(6) | YES  |     |         |                                                         |

### emissionRateByAge

emissionRateByAge contains emission rates for most exhaust pollutants in which rates vary by vehicle age.

| Field            | Type        | Null | Key | Default | Comment               |
| ---------------- | ----------- | ---- | --- | ------- | --------------------- |
| sourceBinID      | bigint(20)  | NO   | PRI | 0       |                       |
| polProcessID     | int(11)     | NO   | PRI | 0       |                       |
| opModeID         | smallint(6) | NO   | PRI | 0       |                       |
| ageGroupID       | smallint(6) | NO   | PRI | 0       |                       |
| meanBaseRate     | float       | YES  |     |         |                       |
| meanBaseRateCV   | float       | YES  |     |         | not used              |
| meanBaseRateIM   | float       | YES  |     |         | rate used in IM areas |
| meanBaseRateIMCV | float       | YES  |     |         | not used              |
| dataSourceId     | smallint(6) | YES  |     |         |                       |

### emissionRateByAgeLEV

emissionRateByAgeLEV's schema is identical to emissionRateByAge, and contains rates applicable to vehicles which meet the LEV standards

### emissionRateByAgeNLEV

emissionRateByAgeNLEV's schema is identical to emissionRateByAge, but it contains rates applicable to the National LEV program

### engineSize

engineSize maps the engine size IDs to their concrete value.

| Field       | Type     | Null | Key  | Default | Comment |
| ----------- | -------- | ---- | ---- | ------- | ------- |
| engSizeID   | int(11)  | NO   | PRI  |         |         |
| engSizeName | char(50) | YES  |      |         |         |

### engineTech

engineTech maps IDs to real engine technologies, and contains information about the engine types.

| Field       | Type        | Null | Key  | Default | Comment |
| ----------- | ----------- | ---- | ---- | ------- | ------- |
| engTechID   | smallint(6) | NO   | PRI  | 0       |         |
| tierID      | smallint(6) | YES  |      | 99      |         |
| strokes     | smallint(6) | YES  |      | 99      |         |
| engTechName | char(50)    | YES  |      |         |         |
| engTechDesc | char(80)    | YES  |      |         |         |

### etohBin

etohBin defines the bin boundaries of ethanol content in gasoline fuel for various  nominal values (ie. E0, E10, E15, E85).

| Field            | Type        | Null | Key  | Default | Comment                                                |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------ |
| etohThreshID     | smallint(6) | NO   | PRI  | 0       |                                                        |
| etohThreshLow    | float       | YES  |      |         | the lowest bin value for a given nominal value         |
| etohThreshHigh   | float       | YES  |      |         | the highest bin value for a given nominal value        |
| etohNominalValue | float       | YES  |      |         | the nominal value of ethanol content in volume percent |

### evapRVPTemperatureAdjustment

evapRVPTemperatureAdjustment defines the coefficients used in the temperature adjustment applied to fuel vapor venting sensitive to fuel RVP.

| Field          | Type        | Null | Key  | Default | Comment |
| -------------- | ----------- | ---- | ---- | ------- | ------- |
| processID      | smallint(6) | NO   | PRI  |         |         |
| fuelTypeID     | smallint(6) | NO   | PRI  |         |         |
| RVP            | double      | NO   | PRI  |         |         |
| adjustTerm3    | double      | NO   |      | 0       |         |
| adjustTerm2    | double      | NO   |      | 0       |         |
| adjustTerm1    | double      | NO   |      | 0       |         |
| adjustConstant | double      | NO   |      | 0       |         |

### evapTemperatureAdjustment

evapTemperatureAdjustment defines the coefficients used in the temperature adjustment applied to fuel  vapor venting not sensitive to fuel RVP.

| Field              | Type        | Null | Key  | Default | Comment |
| ------------------ | ----------- | ---- | ---- | ------- | ------- |
| processID          | smallint(6) | NO   | PRI  |         |         |
| tempAdjustTerm3    | double      | NO   |      | 0       |         |
| tempAdjustTerm2    | double      | NO   |      | 0       |         |
| tempAdjustTerm1    | double      | NO   |      | 0       |         |
| tempAdjustConstant | double      | NO   |      | 0       |         |

### evEfficiency

evEfficiency contains battery and charging efficiency values, which increase the energy consumption rates stored in emissionRate.

| Field              | Type        | Null | Key  | Default | Comment                                                                                                                 |
| ------------------ | ----------- | ---- | ---- | ------- | ----------------------------------------------------------------------------------------------------------------------- |
| polProcessID       | int(11)     | NO   | PRI  |         |                                                                                                                         |
| sourceTypeID       | smallint(6) | NO   | PRI  |         |                                                                                                                         |
| regClassID         | smallint(6) | NO   | PRI  |         |                                                                                                                         |
| ageGroupID         | smallint(6) | NO   | PRI  |         |                                                                                                                         |
| beginModelYearID   | smallint(6) | NO   | PRI  |         |                                                                                                                         |
| endModelYearID     | smallint(6) | NO   | PRI  |         |                                                                                                                         |
| batteryEfficiency  | double      | YES  |      |         | accounts for energy losses that occur when the battery is charging and discharging (i.e., charger-to-drivetrain losses) |
| chargingEfficiency | double      | YES  |      |         | accounts for energy losses that occur in the charger (i.e., wall-to-charger losses)                                     |

### evPopICEAdjustLD

evPopICEAdjustLD is used to adjust emission rates for Tier 3 internal combustion engine (ICE) vehicles based on sales of electric vehicles (EV), due to regulatory fleet averaging of emission rates for total hydrocarbons and NOx emissions. For these pollutants, the multiplicative adjustment is calculated as:
$$
adjustment/(1-evFraction*adjustmentWeight)
$$
This table is also used to adjust energy consumption rates. In this use case, the adjustmentWeight is actually a multiplier, so it increases the apparent total number of vehicles, and the multiplicative adjustment is calculated as:
$$
adjustment/(1 - (evFraction*adjustmentWeight)/((1-evFraction) + (evFraction*adjustmentWeight)))
$$

| Field            | Type        | Null | Key  | Default | Comment                                                      |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| polProcessID     | int         | NO   | PRI  |         |                                                              |
| beginModelYearID | smallint(6) | NO   | PRI  |         |                                                              |
| endModelYearID   | smallint(6) | NO   | PRI  |         |                                                              |
| adjustment       | double      | NO   |      | 1       | A scalar adjustment applied to the base emission rate regardless of the EV fraction. |
| adjustmentWeight | double      | NO   |      | 1       | Relative weight EVs get in fleet averaging, compared to ICE vehicles. A weight of 1 indicates equal weighting, and a weight of 0 turns off the adjustment. |

### fuelAdjustment

fuelAdjustment contains no data by default and is used during runtime.

| Field               | Type        | Null | Key  | Default | Comment |
| ------------------- | ----------- | ---- | ---- | ------- | ------- |
| polProcessID        | int(11)     | NO   | PRI  | 0       |         |
| fuelMYGroupID       | int(11)     | NO   | PRI  | 0       |         |
| sourceTypeID        | smallint(6) | NO   | PRI  | 0       |         |
| fuelFormulationID   | int(11)     | NO   | PRI  | 0       |         |
| fuelAdjustment      | float       | YES  |      |         |         |
| fuelAdjustmentCV    | float       | YES  |      |         |         |
| fuelAdjustmentGPA   | float       | YES  |      |         |         |
| fuelAdjustmentGPACV | float       | YES  |      |         |         |

### fuelEngFraction

fuelEngFraction contains no data, but it was a user provided table defining the fraction of vehicles for a given engine technology type using a given fuel type. Currently, the avft and sampleVehiclePopulation tables contain the same information. 

| Field                 | Type        | Null | Key  | Default | Comment                                                      |
| --------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeModelYearID | int(11)     | NO   | PRI  | 0       |                                                              |
| fuelTypeID            | smallint(6) | NO   | PRI  | 0       |                                                              |
| engTechID             | smallint(6) | NO   | PRI  | 0       |                                                              |
| fuelEngFraction       | float       | YES  |      |         | fraction of vehicles within source type and model year which  are the fuel type and engine technology combination |

### fuelEngTechAssoc

fuelEngTechAssoc defines valid combinations of FuelType and Engine Technology. These valid  combinations depend upon SourceType. Table also contains additional  information about how these combinations should be displayed in the AVFT  control strategy GUI.

| Field                | Type        | Null | Key  | Default | Comment           |
| -------------------- | ----------- | ---- | ---- | ------- | ----------------- |
| sourceTypeID         | smallint(6) | NO   | PRI  | 0       |                   |
| fuelTypeID           | smallint(6) | NO   | PRI  | 0       |                   |
| engTechID            | smallint(6) | NO   | PRI  | 0       |                   |
| category             | char(50)    | NO   |      |         |                   |
| categoryDisplayOrder | smallint(6) | NO   |      | 0       | used in MOVES GUI |

### fuelFormulation

fuelFormulation contains fuel properties for all fuels used in the MOVES fuel effects models  (Complex, Predictive, EPACT/GFRE).

| Field                | Type        | Null | Key  | Default | Comment                                                      |
| -------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelFormulationID    | int(11)     | NO   | PRI  | 0       | IDs < 100 are reserved for base fuel properties (see  baseFuel table) |
| fuelSubtypeID        | smallint(6) | NO   |      | 0       |                                                              |
| RVP                  | float       | YES  |      |         | the Reid Vapor Pressure of the given fuel formulation in PSI |
| sulfurLevel          | float       | NO   |      | 30      | the sulfur level of the given fuel formulation in ppm        |
| ETOHVolume           | float       | YES  |      |         | the volume percentage of ethanol content of the given fuel  formulation |
| MTBEVolume           | float       | YES  |      |         | not used                                                     |
| ETBEVolume           | float       | YES  |      |         | not used                                                     |
| TAMEVolume           | float       | YES  |      |         | not used                                                     |
| aromaticContent      | float       | YES  |      |         | the volume percentage of aromatics content of the given fuel  formulation |
| olefinContent        | float       | YES  |      |         | the volume percentage of olefinic content of the given fuel  formulation |
| benzeneContent       | float       | YES  |      |         | the volume percentage of benzene content of the given fuel  formulation |
| e200                 | float       | YES  |      |         | the E200 distillation metric of the given fuel formulation in  percentage |
| e300                 | float       | YES  |      |         | the E300 distillation metric of the given fuel formulation in  percentage |
| volToWtPercentOxy    | float       | YES  |      |         | the ratio of volume to percentage oxygenate content for the  given fuel formulation |
| BioDieselEsterVolume | float       | YES  |      |         | the volume percentage of biodiesel content of the given fuel  formulation |
| CetaneIndex          | float       | YES  |      |         | the cetane index of the given fuel formulation               |
| PAHContent           | float       | YES  |      |         | the poly-aromatic hydrocarbon content of the given fuel  formulation in percentage |
| T50                  | float       | YES  |      |         | the T50 distillation metric of the given fuel formulation in  degrees F |
| T90                  | float       | YES  |      |         | the T90 distillation metric of the given fuel formulation in  degrees F |

### fuelModelName

fuelModelName defines the full model name, abbreviation, and model calculation type for the  Complex and Predictive fuel effects models used in MOVES.

| Field                 | Type         | Null | Key  | Default | Comment                                                      |
| --------------------- | ------------ | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelModelID           | smallint(6)  | NO   | PRI  | 0       |                                                              |
| fuelModelName         | varchar(50)  | NO   |      |         | the full model name of the given fuel model                  |
| fuelModelAbbreviation | varchar(10)  | NO   |      |         | the abbreviated name of the given fuel model                 |
| calculationEngines    | varchar(200) | NO   |      |         | the associated calculation types used by MOVES to generate  fuel effects ratios for the given fuel model (\| is used as a separator) |

### fuelModelWtFactor

fuelModelWtFactor defines the weighted aggregation of the various Complex and Predictive fuel  effects models for a given modelYearGroup and vehicle age.

| Field             | Type        | Null | Key  | Default | Comment         |
| ----------------- | ----------- | ---- | ---- | ------- | --------------- |
| fuelModelID       | smallint(6) | NO   | PRI  | 0       |                 |
| modelYearGroupID  | int(11)     | NO   | PRI  | 0       |                 |
| ageID             | smallint(6) | NO   | PRI  | 0       |                 |
| fuelModelWtFactor | float       | YES  |      |         | between 0 and 1 |
| dataSourceID      | smallint(6) | YES  |      |         |                 |

### fuelModelYearGroup

fuelModelYearGroup specifies model year grouping that are used to calculate emissions and fuel effects and has some metadata for each group.

| Field               | Type      | Null | Key  | Default | Comment  |
| ------------------- | --------- | ---- | ---- | ------- | -------- |
| fuelMYGroupID       | int(11)   | NO   | PRI  | 0       |          |
| fuelMYGroupName     | char(100) | YES  |      |         |          |
| fuelMYGroupFunction | char(200) | YES  |      |         |          |
| maxSulfurLevel      | float     | YES  |      |         |          |
| maxSulfurLevelCV    | float     | YES  |      |         | not used |
| maxSulfurLevelGPA   | float     | YES  |      |         |          |
| maxSulfurLevelGPACV | float     | YES  |      |         | not used |

### fuelParameterName

fuelParameterName defines the units and table column names for fuel properties contained in the  fuelFormulation table.

| Field                   | Type         | Null | Key  | Default | Comment                                                      |
| ----------------------- | ------------ | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelParameterID         | smallint(6)  | NO   | PRI  | 0       |                                                              |
| fuelParameterName       | varchar(25)  | NO   |      |         | the full name of the fuel parameter                          |
| fuelParameterUnits      | varchar(20)  | NO   |      |         | the units of the fuel parameter                              |
| fuelParameterExpression | varchar(500) | NO   |      |         | the fuelFormulation column name (or expression) used by the  fuel effects models |

### fuelSubtype

fuelSubtype lists the fuel subtypes used in MOVES and contains basic fuel properties  specific to the fuel subtype.

| Field                          | Type        | Null | Key  | Default | Comment                                                      |
| ------------------------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelSubtypeID                  | smallint(6) | NO   | PRI  | 0       |                                                              |
| fuelTypeID                     | smallint(6) | NO   | MUL  | 0       |                                                              |
| fuelSubtypeDesc                | char(50)    | YES  |      |         | a short description of what the fuel subtype represents      |
| fuelSubtypePetroleumFraction   | float       | YES  |      |         | the volume fraction of non-oxygenated hydrocarbon content contained in  the fuel |
| fuelSubtypePetroleumFractionCV | float       | YES  |      |         | not used                                                     |
| fuelSubtypeFossilFraction      | float       | YES  |      |         | the volume fraction of conventional hydrocarbon content  (non-renewables) contained in the fuel |
| fuelSubtypeFossilFractionCV    | float       | YES  |      |         | not used                                                     |
| carbonContent                  | float       | YES  |      |         |                                                              |
| oxidationFraction              | float       | YES  |      |         | the volume fraction of fully combustible fuel                |
| carbonContentCV                | float       | YES  |      |         | not used                                                     |
| oxidationFractionCV            | float       | YES  |      |         | not used                                                     |
| energyContent                  | float       | YES  |      |         | the energy content of the given fuel in MJ/kg (lower/net  calorific value) |

### fuelSupply

fuelSupply maps each fuel region (see regionCounty table) to the mix of fuel formulations used in that region.

| Field             | Type        | Null | Key  | Default | Comment                                                      |
| ----------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelRegionID      | int(11)     | NO   | PRI  | 0       | see regioncounty table                                       |
| fuelYearID        | smallint(6) | NO   | PRI  | 0       |                                                              |
| monthGroupID      | smallint(6) | NO   | PRI  | 0       |                                                              |
| fuelFormulationID | int(11)     | NO   | PRI  | 0       | see fuelformulation table                                    |
| marketShare       | float       | YES  |      |         | the volume fraction of a given fuelformulation used in the fuel region. Should sum to 1 for every month, for each fuel type present. |
| marketShareCV     | float       | YES  |      |         | not used                                                     |

### fuelSupplyYear

fuelSupplyYear lists the years with fuel supply information in MOVES.

| Field      | Type    | Null | Key | Default | Comment |
| ---------- | ------- | ---- | --- | ------- | ------- |
| fuelYearID | int(11) | NO   | PRI | 0       |         |

### fuelType

fuelType lists the general fuel types used in MOVES and basic fuel property data.

| Field                     | Type        | Null | Key  | Default | Comment                                                      |
| ------------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelTypeID                | smallint(6) | NO   | PRI  | 0       |                                                              |
| defaultFormulationID      | smallint(6) | NO   |      | 0       | a fallback fuel formulation MOVES will use if no specific fuel  formulation can be found for this fuel type in a given region |
| fuelTypeDesc              | char(50)    | YES  |      |         | a general description of what the given fuel type represents |
| fuelDensity               | float       | YES  |      |         | the density of the given fuel type in grams / gallon         |
| subjectToEvapCalculations | char(1)     | NO   | MUL  | N       | Y if MOVES contains applicable evaporative emissions models  for the given fuel type (other than refueling) |

### fuelUsageFraction

fuelUsageFraction defines the fraction of flexible fuel vehicles using fuels of a given fuel  type.

| Field                | Type        | Null | Key  | Default | Comment                                                      |
| -------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| countyID             | int(11)     | NO   | PRI  |         |                                                              |
| fuelYearID           | int(11)     | NO   | PRI  |         |                                                              |
| modelYearGroupID     | int(11)     | NO   | PRI  |         |                                                              |
| sourceBinFuelTypeID  | smallint(6) | NO   | PRI  |         | the fuel type originally associated with the given source bin |
| fuelSupplyFuelTypeID | smallint(6) | NO   | PRI  |         | the fuel type actually used in the vehicle for this source bin |
| usageFraction        | double      | YES  |      |         | the fraction of vehicles using the given sourceBin fuel type  and fuelSupply fuel type combination. Should sum to 1 for a given  sourceBinFuelType. |

### fuelWizardFactors

fuelWizardFactors contains data used to adjust fuel properties using the Fuels Wizard in the  MOVES GUI.

| Field             | Type        | Null | Key  | Default | Comment                                                      |
| ----------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| adjustedParameter | varchar(4)  | NO   | PRI  |         | the fuel property short name being adjusted by the fuels  wizard |
| minLevel          | double      | NO   | PRI  |         | the minimum property level the adjustment can be applied for |
| maxLevel          | double      | NO   | PRI  |         | the maximum property level the adjust can be applied for      |
| functionType      | varchar(4)  | NO   | PRI  |         | ADD for additive adjustment effects, MULT for multiplicative adjustment effects |
| monthGroupID      | smallint(6) | NO   | PRI  |         |                                                              |
| fuelTypeID        | smallint(6) | NO   | PRI  |         |                                                              |
| RVP_factor        | double      | YES  |      |         | magnitude of adjusted parameter                              |
| sulf_factor       | double      | YES  |      |         |                                                              |
| ETOH_factor       | double      | YES  |      |         |                                                              |
| arom_factor       | double      | YES  |      |         |                                                              |
| olef_factor       | double      | YES  |      |         |                                                              |
| benz_factor       | double      | YES  |      |         |                                                              |
| e200_factor       | double      | YES  |      |         |                                                              |
| e300_factor       | double      | YES  |      |         |                                                              |
| T50_factor        | double      | YES  |      |         |                                                              |
| T90_factor        | double      | YES  |      |         |                                                              |
| units             | varchar(6)  | YES  |      |         |                                                              |
| dataSourceId      | smallint(6) | YES  |      |         |                                                              |

### fullACAdjustment

fullACAdjustment contains adjustments made to the base emission rates when there is full loading of the air conditioning system.

| Field              | Type        | Null | Key  | Default | Comment                                                      |
| ------------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeID       | smallint(6) | NO   | PRI  | 0       |                                                              |
| polProcessID       | int(11)     | NO   | PRI  | 0       |                                                              |
| opModeID           | smallint(6) | NO   | PRI  | 0       |                                                              |
| fullACAdjustment   | float       | YES  |      |         | Adjustment factor, as a fraction of base emissions, by which  emissions increase. A value of 0.1 indicates a 10% increase in emissions, for  example. |
| fullACAdjustmentCV | float       | YES  |      |         | not used                                                     |

### generalFuelRatio

generalFuelRatio defines numeric fuel adjustment ratios that apply emission adjustments depending on fuel properties in use. It is not populated by default but is used at MOVES runtime.

| Field              | Type                 | Null | Key  | Default | Comment                                                      |
| ------------------ | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelTypeID         | smallint(6)          | NO   | PRI  |         |                                                              |
| fuelFormulationID  | int(11)              | NO   | PRI  |         |                                                              |
| polProcessID       | int(11)              | NO   | PRI  |         |                                                              |
| pollutantID        | smallint(6)          | NO   | PRI  |         |                                                              |
| processID          | smallint(6)          | NO   | PRI  |         |                                                              |
| minModelYearID     | smallint(6)          | NO   | PRI  | 1960    | minimum vehicle model year where fuelEffectRatioExpression  equation will be applied |
| maxModelYearID     | smallint(6)          | NO   | PRI  | 2050    | maximum vehicle model year where fuelEffectRatioExpression  equation will be applied |
| minAgeID           | smallint(6)          | NO   | PRI  | 0       | minimum vehicle age where fuelEffectRatioExpression equation  will be applied |
| maxAgeID           | smallint(6)          | NO   | PRI  | 30      | maximum vehicle age where fuelEffectRatioExpression equation  will be applied |
| sourceTypeID       | smallint(6) unsigned | NO   | PRI  |         |                                                              |
| fuelEffectRatio    | double               | NO   |      | 0       | numeric ratio to apply for a given pollutant, process, MY,  age, and sourceType combination |
| fuelEffectRatioGPA | double               | NO   |      | 0       | same effect as fuelEffectRatio, applied in regions falling  under the Geographic Phase-in Area regulations |

### generalFuelRatioExpression

generalfuelratioexpression defines fuel adjustment equations that apply emission adjustments depending  on fuel properties in use. Primarily used to apply EPACT fuel effects models  for MY2001+ vehicles, as well as biodiesel fuel effects models. See MOVES Fuel Effects Technical Report for more detail.

| Field                        | Type           | Null | Key  | Default | Comment                                                      |
| ---------------------------- | -------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelTypeID                   | smallint(6)    | NO   | PRI  |         |                                                              |
| polProcessID                 | int(11)        | NO   | PRI  |         |                                                              |
| minModelYearID               | int(11)        | NO   | PRI  | 1960    | minimum vehicle model year where fuelEffectRatioExpression  equation will be applied |
| maxModelYearID               | int(11)        | NO   | PRI  | 2050    | maximum vehicle model year where fuelEffectRatioExpression  equation will be applied |
| minAgeID                     | int(11)        | NO   | PRI  | 0       | minimum vehicle age where fuelEffectRatioExpression equation  will be applied |
| maxAgeID                     | int(11)        | NO   | PRI  | 30      | maximum vehicle age where fuelEffectRatioExpression equation  will be applied |
| sourceTypeID                 | smallint(6)    | NO   | PRI  | 0       |                                                              |
| fuelEffectRatioExpression    | varchar(32000) | NO   |      |         | specific equation generating fuel effect ratios for a given  pollutant, process, MY, age, and sourceType combination. Regular SQL math can  be used, along with variable matching column names from the fuelFormulation  table |
| fuelEffectRatioGPAExpression | varchar(32000) | NO   |      |         | same effect as fuelEffectRatioExpression, applied in regions  falling under the Geographic Phase-in Area regulations |

### greetManfAndDisposal

greetManfAndDisposal is deprecated and no longer used during a MOVES run.

| Field              | Type        | Null | Key | Default | Comment |
| ------------------ | ----------- | ---- | --- | ------- | ------- |
| GREETVehicleType   | smallint(6) | NO   | PRI | 0       |         |
| modelYearID        | smallint(6) | NO   | PRI | 0       |         |
| pollutantID        | smallint(6) | NO   | PRI | 0       |         |
| EmissionStage      | char(4)     | NO   | PRI |         |         |
| emissionPerVehicle | float       | YES  |     |         |         |

### greetWellToPump

greetWellToPump is deprecated and no longer used during a MOVES run.

| Field                   | Type        | Null | Key | Default | Comment |
| ----------------------- | ----------- | ---- | --- | ------- | ------- |
| yearID                  | smallint(6) | NO   | PRI | 0       |         |
| pollutantID             | smallint(6) | NO   | PRI | 0       |         |
| fuelSubtypeID           | smallint(6) | NO   | PRI | 0       |         |
| emissionRate            | float       | YES  |     |         |         |
| emissionRateUncertainty | float       | YES  |     |         |         |

### grid

grid contains a list of grids in MOVES and is not used in most MOVES runs.

| Field  | Type    | Null | Key | Default | Comment |
| ------ | ------- | ---- | --- | ------- | ------- |
| gridID | int(11) | NO   | PRI | 0       |         |

### gridZoneAssoc

gridZoneAssoc allocates activity in a zone to grids, but is not used in most MOVES runs

| Field           | Type    | Null | Key | Default | Comment                                         |
| --------------- | ------- | ---- | --- | ------- | ----------------------------------------------- |
| zoneID          | int(11) | NO   | PRI | 0       |                                                 |
| gridID          | int(11) | NO   | PRI | 0       |                                                 |
| gridAllocFactor | float   | YES  |     |         | allocates activity within zoneID to each gridID |

### hcPermeationCoeff

hcPermeationCoeff contains multiplicative factors applied to permeation base rates for gasoline with ethanol.

| Field             | Type        | Null | Key  | Default | Comment                   |
| ----------------- | ----------- | ---- | ---- | ------- | ------------------------- |
| polProcessID      | int(11)     | NO   | PRI  | 0       |                           |
| etohThreshID      | smallint(6) | NO   | PRI  | 0       |                           |
| fuelMYGroupID     | int(11)     | NO   | PRI  | 0       |                           |
| fuelAdjustment    | float       | YES  |      |         | multiplicative adjustment |
| fuelAdjustmentGPA | float       | YES  |      |         |                           |
| dataSourceID      | smallint(6) | YES  |      |         |                           |

### hcSpeciation

hcSpeciation is used to speciate hydrocarbon emissions. Specifically, it calculated NMOG (non-methane organic gases) from NMHC (non-methane hydrocarbons) emissions.

| Field              | Type        | Null | Key  | Default | Comment     |
| ------------------ | ----------- | ---- | ---- | ------- | ----------- |
| polProcessID       | int(11)     | NO   | PRI  | 0       |             |
| fuelSubtypeID      | smallint(6) | NO   | PRI  | 0       |             |
| regClassID         | smallint(6) | NO   | PRI  | 0       |             |
| beginModelYearID   | smallint(6) | NO   | PRI  | 0       |             |
| endModelYearID     | smallint(6) | NO   | PRI  | 0       |             |
| speciationConstant | double      | NO   |      | 0       | NMOG / NMHC |
| oxySpeciation      | double      | NO   |      | 0       | deprecated  |
| dataSourceID       | smallint(6) | NO   |      | 0       |             |

### hotellingActivityDistribution

hotellingActivityDistribution applies a hotelling operating mode distribution to hotelling activity. It allocates hotelling activity to extend idle (opModeID 200), diesel APU (201), shore power (203), or battery / engine off (204). It contains national default distributions, but also can be user provided.

| Field            | Type        | Null | Key  | Default | Comment                                                      |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| zoneID           | int(11)     | NO   | PRI  |         |                                                              |
| fuelTypeID       | smallint(6) | NO   | PRI  |         |                                                              |
| beginModelYearID | smallint(6) | NO   | PRI  |         |                                                              |
| endModelYearID   | smallint(6) | NO   | PRI  |         |                                                              |
| opModeID         | smallint(6) | NO   | PRI  |         | this table only contains the hotelling operating modes (200, 201, 203, and 204) |
| opModeFraction   | float       | NO   |      |         | fraction of time spent in each operating mode, summing to 1 for each zone, fuel type, and model year group |

### hotellingAgeFraction

hotellingAgeFraction allocates total hotelling activity by age. The default algorithm allocates hotelling activity according to VMT by age. If local data are available, they can be input into this table to override the default algorithm. Therefore, this table is empty by default.

| Field       | Type        | Null | Key | Default | Comment                                                                     |
| ----------- | ----------- | ---- | --- | ------- | --------------------------------------------------------------------------- |
| zoneID      | int(11)     | NO   | PRI |         |                                                                             |
| ageID       | smallint(6) | NO   | PRI |         |                                                                             |
| ageFraction | double      | NO   |     |         | fraction of hotelling activity done by vehicles of each age within the zone |

### hotellingCalendarYear

hotellingCalendarYear is used to calculate hotelling activity based on VMT by combination long-haul trucks.

| Field         | Type        | Null | Key | Default | Comment                                                          |
| ------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------- |
| yearID        | smallint(6) | NO   | PRI |         |                                                                  |
| hotellingRate | double      | NO   |     |         | rate of hotelling hours per VMT for long-haul combination trucks |

### hotellingHourFraction

hotellingHourFraction allocates hotelling activity by the hour of day. The default algorithm allocates hotelling activity based on VMT hourly activity (see hourVMTFraction). If local data are available, they can be input into this table to override the default algorithm. Therefore, this table is empty by default.

| Field        | Type        | Null | Key | Default | Comment                                                                    |
| ------------ | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------- |
| zoneID       | int(11)     | NO   | PRI |         |                                                                            |
| dayID        | smallint(6) | NO   | PRI |         |                                                                            |
| hourID       | smallint(6) | NO   | PRI |         |                                                                            |
| hourFraction | double      | NO   |     |         | fraction of hotelling that happens in each hour of the day within the zone |

### hotellingHours

hotellingHours is calculated during a MOVES run to store total hotelling activity. It is subsequently used when calculating extended idle, APU, shore power, and battery/off activity and emissions. It cannot be provided by users; the other hotelling tables can be used instead to provide local data (i.e., hotellingActivityDistribution, hotellingAgeFraction, hotellingHourFraction, hotellingHoursPerDay, and hotellingMonthAdjust).

| Field          | Type        | Null | Key  | Default | Comment                                         |
| -------------- | ----------- | ---- | ---- | ------- | ----------------------------------------------- |
| sourceTypeID   | smallint(6) | NO   | PRI  |         |                                                 |
| fuelTypeID     | smallint(6) | NO   | PRI  |         |                                                 |
| hourDayID      | smallint(6) | NO   | PRI  |         |                                                 |
| monthID        | smallint(6) | NO   | PRI  |         |                                                 |
| yearID         | smallint(6) | NO   | PRI  |         |                                                 |
| ageID          | smallint(6) | NO   | PRI  |         |                                                 |
| zoneID         | int(11)     | NO   | PRI  |         |                                                 |
| hotellingHours | double      | YES  |      |         | total hotelling hours per table key             |
| isUserInput    | char(1)     | NO   |      | N       | deprecated (no longer available for user input) |

### hotellingHoursPerDay

hotellingHoursPerDay contains the average hotelling hours per day in a zone. The default algorithm calculates hotelling hours per day based on long-haul combination truck VMT on restricted access road types. If local data are available, they can be input into this table to override the default algorithm. Therefore, this table is empty by default.

| Field                | Type        | Null | Key  | Default | Comment                                                      |
| -------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| yearID               | smallint(6) | NO   | PRI  |         |                                                              |
| zoneID               | int(11)     | NO   | PRI  |         |                                                              |
| dayID                | smallint(6) | NO   | PRI  |         |                                                              |
| hotellingHoursPerDay | double      | NO   |      |         | average number of total hotelling hours per day within each year and zone |

### hotellingMonthAdjust

hotellingMonthAdjust contains multiplicative factors to adjust hotelling activity within a month compared to the annual monthly average activity based on local data. This is a user provided table and is empty by default.

| Field           | Type        | Null | Key | Default | Comment                                                                     |
| --------------- | ----------- | ---- | --- | ------- | --------------------------------------------------------------------------- |
| zoneID          | int(11)     | NO   | PRI |         |                                                                             |
| monthID         | smallint(6) | NO   | PRI |         |                                                                             |
| monthAdjustment | double      | NO   |     |         | raw multiplicative factor used to adjust hotelling hours for<br> each month |

### hourDay

hourDay is used to translate an hourDayID to an hourID and dayID or vice versa.

| Field     | Type        | Null | Key | Default | Comment |
| --------- | ----------- | ---- | --- | ------- | ------- |
| hourDayID | smallint(6) | NO   | PRI | 0       |         |
| dayID     | smallint(6) | NO   | MUL | 0       |         |
| hourID    | smallint(6) | NO   | MUL | 0       |         |

### hourOfAnyDay

hourOfAnyDay maps the hourID to the time to day it represents

| Field    | Type        | Null | Key | Default | Comment |
| -------- | ----------- | ---- | --- | ------- | ------- |
| hourID   | smallint(6) | NO   | PRI | 0       |         |
| hourname | char(50)    | YES  |     |         |         |

### hourVMTFraction

hourVMTFraction is used to allocate daily VMT by hour of the day.

| Field           | Type        | Null | Key | Default | Comment                                                                                  |
| --------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------------------- |
| sourceTypeID    | smallint(6) | NO   | PRI | 0       |                                                                                          |
| roadTypeID      | smallint(6) | NO   | PRI | 0       |                                                                                          |
| dayID           | smallint(6) | NO   | PRI | 0       |                                                                                          |
| hourID          | smallint(6) | NO   | PRI | 0       |                                                                                          |
| hourVMTFraction | float       | YES  |     |         | fraction of VMT allocated to each hour within every source type, road type, and day type |

### hpmsVtype

hpmsVtype lists the HPMS vehicle types.

| Field         | Type        | Null | Key | Default | Comment |
| ------------- | ----------- | ---- | --- | ------- | ------- |
| HPMSVtypeID   | smallint(6) | NO   | PRI | 0       |         |
| HPMSVtypeName | char(50)    | YES  |     |         |         |

### hpmsVtypeDay

hpmsVtypeDay contains VMT by HPMS vehicle type, month, and day. MOVES does not use this table by default, but a user can provide VMT data using this table.

| Field       | Type        | Null | Key | Default | Comment                                                       |
| ----------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------- |
| yearID      | smallint(6) | NO   | PRI |         |                                                               |
| monthID     | smallint(6) | NO   | PRI |         |                                                               |
| dayID       | smallint(6) | NO   | PRI |         |                                                               |
| HPMSVtypeID | smallint(6) | NO   | PRI |         |                                                               |
| VMT         | double      | NO   |     |         | total VMT per HPMS vehicle type for each year, month, and day |

### hpmsVtypeYear

hpmsVtypeYear contains VMT by HPMS vehicle type and year. MOVES uses this table to calculate VMT at default scale, and it can also be used to provide county scale input data.

| Field           | Type        | Null | Key | Default | Comment                                                                                |
| --------------- | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------------------- |
| HPMSVtypeID     | smallint(6) | NO   | PRI | 0       |                                                                                        |
| yearID          | smallint(6) | NO   | PRI | 0       |                                                                                        |
| VMTGrowthFactor | double      | YES  |     |         | growth factor used to scale VMT for future years. Currently, this is not used in MOVES |
| HPMSBaseYearVMT | double      | YES  |     |         | Total VMT by each HPMS vehicle type within the year                                    |

### idleDayAdjust

idleDayAdjust contains raw multiplicative factors to adjust ONI activity up or down based on the day of the week. MOVES doesn't use this at default scale, but users can optionally provide data at County Scale using this table (it intended to be used in conjunction with idleModelYearGrouping).

| Field         | Type        | Null | Key  | Default | Comment                                                      |
| ------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeID  | smallint(6) | NO   | PRI  |         |                                                              |
| dayID         | smallint(6) | NO   | PRI  |         |                                                              |
| idleDayAdjust | double      | NO   |      |         | raw multiplicative adjustment used to scale off-network idle (ONI) activity for each day |

### idleModelYearGrouping

idleModelYearGrouping has the fraction of total SHO that is off-network idle by source type for given model year ranges. MOVES doesn't use this at default scale, but users can optionally provide data at County Scale using this table (it intended to be used in conjunction with idleDayAdjust and idleMonthAdjust).

| Field             | Type        | Null | Key | Default | Comment                                                                        |
| ----------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------------------ |
| sourceTypeID      | smallint(6) | NO   | PRI |         |                                                                                |
| minModelYearID    | smallint(6) | NO   | PRI |         |                                                                                |
| maxModelYearID    | smallint(6) | NO   | PRI |         |                                                                                |
| totalIdleFraction | double      | NO   |     |         | fraction of total SHO that is ONI for each source type and model year grouping |

### idleMonthAdjust

idleMonthAdjust contains raw multiplicative factors to adjust ONI activity up or down for each month. MOVES doesn't use this at default scale, but users can optionally provide data at County Scale using this table (it intended to be used in conjunction with idleModelYearGrouping).

| Field           | Type        | Null | Key | Default | Comment                                                                      |
| --------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------- |
| sourceTypeID    | smallint(6) | NO   | PRI |         |                                                                              |
| monthID         | smallint(6) | NO   | PRI |         |                                                                              |
| idleMonthAdjust | double      | NO   |     |         | raw multiplicative adjustment used to scale ONI idle activity for each month |

### idleRegion

idleRegion defines the idle regions used in MOVES.

| Field                 | Type     | Null | Key | Default | Comment |
| --------------------- | -------- | ---- | --- | ------- | ------- |
| idleRegionID          | int(11)  | NO   | PRI |         |         |
| idleRegionDescription | char(20) | YES  |     |         |         |

### imCoverage

imCoverage  contains information about which pollutant-processes are covered by IM  programs in various counties and calendar years. Data in imCoverage is used  in a default run, and may also be provided by users at County Scale.

| Field            | Type        | Null | Key  | Default | Comment                                                      |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| polProcessID     | int(11)     | NO   | PRI  | 0       |                                                              |
| stateID          | smallint(6) | NO   | PRI  | 0       |                                                              |
| countyID         | int(11)     | NO   | PRI  | 0       |                                                              |
| yearID           | smallint(6) | NO   | PRI  | 0       |                                                              |
| sourceTypeID     | smallint(6) | NO   | PRI  | 0       |                                                              |
| fuelTypeID       | smallint(6) | NO   | PRI  | 0       |                                                              |
| IMProgramID      | smallint(6) | NO   | PRI  | 0       |                                                              |
| begModelYearID   | smallint(6) | NO   |      | 0       |                                                              |
| endModelYearID   | smallint(6) | NO   |      | 0       |                                                              |
| inspectFreq      | smallint(6) | YES  |      |         | Inspection frequency associated with the IMProgramID which is annual, biennial, or continuous/monthly. Note that there are currently no emission benefits assigned to the continuous option. |
| testStandardsID  | smallint(6) | NO   |      | 0       |                                                              |
| useIMyn          | char(1)     | NO   |      | Y       | This allows uses to turn off ("N") or on ("Y") the portion of the I/M program described in that row of the table. |
| complianceFactor | float       | YES  |      |         | The compliance factor is entered as a decimal number from 0 to 100 and represents the percentage of vehicles within a source type that actually receive the benefits of the program. |

### imFactor

imFactor is used to adjust emissions for particular I/M programs by relating their effectiveness with a that of a "reference I/M program".

| Field              | Type        | Null | Key  | Default | Comment                                                      |
| ------------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| polProcessID       | int(11)     | NO   | PRI  |         |                                                              |
| inspectFreq        | smallint(6) | NO   | PRI  |         |                                                              |
| testStandardsID    | smallint(6) | NO   | PRI  |         |                                                              |
| sourceTypeID       | smallint(6) | NO   | PRI  |         |                                                              |
| fuelTypeID         | smallint(6) | NO   | PRI  |         |                                                              |
| IMModelYearGroupID | int(8)      | NO   | PRI  |         |                                                              |
| ageGroupID         | smallint(6) | NO   | PRI  |         |                                                              |
| IMFactor           | float       | YES  |      |         | The ratio of the emissions difference between the IM program and the standard IM difference. ImFactor cannot be less than or equal to 0,  but technically has no upper bounds. |

### imInspectFreq

imInsepctFreq defines the I/M inspection frequencies that can be used in MOVES.

| Field           | Type        | Null | Key  | Default | Comment |
| --------------- | ----------- | ---- | ---- | ------- | ------- |
| inspectFreq     | smallint(6) | NO   | PRI  |         |         |
| inspectFreqDesc | char(50)    | YES  |      |         |         |

### imModelYearGroup

imModelYearGroup defines the model year ranges used in the other I/M tables.

| Field                | Type     | Null | Key  | Default | Comment |
| -------------------- | -------- | ---- | ---- | ------- | ------- |
| IMModelYearGroupID   | int(8)   | NO   | PRI  |         |         |
| IMModelYearGroupDesc | char(40) | NO   |      |         |         |

### imTestStandards

imTestStandards lists the types of IM tests that can be used in MOVES.

| Field             | Type        | Null | Key  | Default | Comment |
| ----------------- | ----------- | ---- | ---- | ------- | ------- |
| testStandardsID   | smallint(6) | NO   | PRI  |         |         |
| testStandardsDesc | char(50)    | NO   |      |         |         |
| shortName         | varchar(50) | YES  |      |         |         |


### imTestType

imTestType defines the I/M test procedures used in MOVES.

| Field        | Type        | Null | Key  | Default | Comment |
| ------------ | ----------- | ---- | ---- | ------- | ------- |
| testTypeID   | smallint(6) | NO   | PRI  |         |         |
| testTypeDesc | char(50)    | NO   |      |         |         |
| shortName    | varchar(50) | YES  |      |         |         |

### importStartsOpModeDistribution

importStartsOpModeDistribution  table is empty by default and is populated and used during runtime to calculate starts emissions.

| Field            | Type        | Null | Key  | Default | Comment |
| ---------------- | ----------- | ---- | ---- | ------- | ------- |
| sourceTypeID     | smallint(6) | NO   | PRI  |         |         |
| hourDayID        | smallint(6) | NO   | PRI  |         |         |
| linkID           | int(11)     | NO   | PRI  |         |         |
| polProcessID     | int(11)     | NO   | PRI  |         |         |
| opModeID         | smallint(6) | NO   | PRI  |         |         |
| opModeFraction   | float       | YES  |      |         |         |
| opModeFractionCV | float       | YES  |      |         |         |
| isUserInput      | char(1)     | NO   |      | N       |         |

### integratedSpeciesSet

integratedSpeciesSet lists the MOVES integrated species for each chemical mechanism. MOVES integrated species are those for which MOVES calculates emissions explicitly, rather than speciating based on other pollutants.

| Field                  | Type        | Null | Key | Default | Comment |
| ---------------------- | ----------- | ---- | --- | ------- | ------- |
| mechanismID            | smallint(6) | NO   | PRI |         |         |
| integratedSpeciesSetID | smallint(6) | NO   | PRI |         |         |
| pollutantID            | smallint(6) | NO   | PRI |         |         |
| useISSyn               | varchar(2)  | YES  |     |         |         |

### integratedSpeciesSetName

integratedSpeciesSetName lists the sets of integrated species and their names.

| Field                    | Type        | Null | Key | Default | Comment |
| ------------------------ | ----------- | ---- | --- | ------- | ------- |
| integratedSpeciesSetID   | smallint(6) | NO   | PRI |         |         |
| integratedSpeciesSetName | varchar(40) | YES  | MUL |         |         |

### link

link is a user-provided table to specify run information at project scale.

| Field           | Type        | Null | Key | Default | Comment |
| --------------- | ----------- | ---- | --- | ------- | ------- |
| linkID          | int(11)     | NO   | PRI | 0       |         |
| countyID        | int(11)     | NO   | MUL | 0       |         |
| zoneID          | int(11)     | YES  | MUL |         |         |
| roadTypeID      | smallint(6) | NO   | MUL | 0       |         |
| linkLength      | float       | YES  |     |         |         |
| linkVolume      | float       | YES  |     |         |         |
| linkAvgSpeed    | float       | YES  |     |         |         |
| linkDescription | varchar(50) | YES  |     |         |         |
| linkAvgGrade    | float       | YES  |     |         |         |

### linkAverageSpeed

linkAverageSpeed contains the average speed for all links in a MOVES run. MOVES no longer uses this table.

| Field        | Type    | Null | Key | Default | Comment |
| ------------ | ------- | ---- | --- | ------- | ------- |
| linkID       | int(11) | NO   | PRI | 0       |         |
| averageSpeed | float   | YES  |     |         |         |

### linkHourVMTFraction

linkHourVMTFraction allocates VMT on a link by hour of day. MOVES no longer uses this table.

| Field        | Type        | Null | Key | Default | Comment                                                                                  |
| ------------ | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------------------- |
| linkID       | int(11)     | NO   | PRI | 0       |                                                                                          |
| monthID      | smallint(6) | NO   | PRI | 0       |                                                                                          |
| sourceTypeID | smallint(6) | NO   | PRI | 0       |                                                                                          |
| dayID        | smallint(6) | NO   | PRI | 0       |                                                                                          |
| hourID       | smallint(6) | NO   | PRI | 0       |                                                                                          |
| VMTFraction  | float       | YES  |     |         | fraction of VMT on each link allocated to each hour within a month, source type, and day |

### linkSourceTypeHour

linkSourceTypeHour describes the activity of every source type on every link.

| Field                  | Type        | Null | Key | Default | Comment                                                                      |
| ---------------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------- |
| linkID                 | int(11)     | NO   | PRI |         |                                                                              |
| sourceTypeID           | smallint(6) | NO   | PRI |         |                                                                              |
| sourceTypeHourFraction | float       | YES  |     |         | Fraction of the hour during which vehicles of each source type are operating |

### m6SulfurCoeff

m6SulfurCoeff contains the coefficients used in the Mobile 6 sulfur model. See MOVES Fuel Effects Technical Report.

| Field             | Type        | Null | Key  | Default | Comment                                                      |
| ----------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| pollutantID       | smallint(6) | NO   | PRI  |         |                                                              |
| minModelYearID    | smallint(6) | NO   | PRI  |         |                                                              |
| maxModelYearID    | smallint(6) | NO   | PRI  |         |                                                              |
| minSulfur         | double      | NO   |      |         | the lower limit of sulfur concentration (in ppm) in which the  model can be applied |
| sulfurLongCoeff   | double      | YES  |      |         | see MOVES Fuel Effects Technical Report                      |
| sulfurIRFactor    | double      | YES  |      |         | see MOVES Fuel Effects Technical Report                      |
| maxIRFactorSulfur | double      | YES  |      |         | see MOVES Fuel Effects Technical Report                      |

### meanFuelParameters

meanFuelParameters  contains statistical normalization values used in the Complex and Predictive  fuel effects models.

| Field            | Type        | Null | Key  | Default | Comment                                                      |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| polProcessID     | int(11)     | NO   | PRI  | 0       |                                                              |
| fuelTypeID       | smallint(6) | NO   | PRI  | 0       |                                                              |
| modelYearGroupID | int(11)     | NO   | PRI  | 0       |                                                              |
| fuelParameterID  | smallint(6) | NO   | PRI  | 0       |                                                              |
| baseValue        | float       | YES  |      |         | the base value of the given fuel parameter in the units of  that parameter |
| centeringValue   | float       | YES  |      |         | the centering value used for the normalization of the given  parameter in the relevant fuel effects model |
| stdDevValue      | float       | YES  |      |         | the standard deviation of the given fuel parameter           |
| dataSourceID     | smallint(6) | YES  |      |         |                                                              |

### mechanismName

mechanismName lists the chemical mechanisms used by MOVES for speciation.

| Field         | Type        | Null | Key | Default | Comment |
| ------------- | ----------- | ---- | --- | ------- | ------- |
| mechanismID   | smallint(6) | NO   | PRI |         |         |
| mechanismName | varchar(40) | YES  | MUL |         |         |

### metalEmissionRate

metalEmissionRate contains the exhaust emission rates for metals, in units of gram per mile.

| Field            | Type        | Null | Key | Default | Comment  |
| ---------------- | ----------- | ---- | --- | ------- | -------- |
| polProcessID     | int(11)     | NO   | PRI | 0       |          |
| fuelTypeID       | smallint(6) | NO   | PRI | 0       |          |
| sourceTypeID     | smallint(6) | NO   | PRI | 0       |          |
| modelYearGroupID | int(11)     | NO   | PRI | 0       |          |
| units            | char(20)    | YES  |     |         |          |
| meanBaseRate     | double      | YES  |     |         |          |
| meanBaseRateCV   | double      | YES  |     |         | not used |
| dataSourceId     | smallint(6) | YES  |     |         |          |

### methaneTHCRatio

methaneTHCRatio is used to calculate methane emissions based on Total Hydrocarbon emissions.

| Field            | Type        | Null | Key | Default | Comment                                  |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------------- |
| processID        | smallint(6) | NO   | PRI | 0       |                                          |
| fuelSubtypeID    | smallint(6) | NO   | PRI | 0       |                                          |
| regClassID       | smallint(6) | NO   | PRI | 0       |                                          |
| beginModelYearID | smallint(6) | NO   | PRI | 0       |                                          |
| endModelYearID   | smallint(6) | NO   | PRI | 0       |                                          |
| CH4THCRatio      | double      | YES  |     |         | ratio of THC emissions which are methane |
| dataSourceID     | smallint(6) | NO   |     | 0       |                                          |

### minorHAPRatio

minorHAPRatio is used to calculate HAP (hazardous air pollutant) emissions from VOC emissions.

| Field            | Type        | Null | Key  | Default | Comment  |
| ---------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID     | int(11)     | NO   | PRI  | 0       |          |
| fuelTypeID       | smallint(6) | NO   | PRI  | 0       |          |
| fuelSubtypeID    | smallint(6) | NO   | PRI  | 0       |          |
| modelYearGroupID | int(11)     | NO   | PRI  | 0       |          |
| atRatio          | double      | YES  |      |         |          |
| atRatioCV        | double      | YES  |      |         | not used |
| dataSourceId     | smallint(6) | YES  |      |         |          |

### modelYear

modelYear lists all the model years used in MOVES.

| Field       | Type        | Null | Key | Default | Comment |
| ----------- | ----------- | ---- | --- | ------- | ------- |
| modelYearID | smallint(6) | NO   | PRI | 0       |         |

### modelYearCutpoints

modelYearCutPoints  defines the cut points for various aspects of a MOVES run, mostly pertaining  to fuel model used.

| Field        | Type         | Null | Key  | Default | Comment |
| ------------ | ------------ | ---- | ---- | ------- | ------- |
| cutPointName | varchar(100) | NO   | PRI  |         |         |
| modelYearID  | smallint(6)  | NO   |      |         |         |

### modelYearGroup

modelYearGroup is used to get the member model years of every model year group (or short model year group).

| Field                   | Type        | Null | Key | Default | Comment |
| ----------------------- | ----------- | ---- | --- | ------- | ------- |
| modelYearGroupID        | int(11)     | NO   | PRI | 0       |         |
| shortModYrGroupID       | smallint(6) | NO   |     | 0       |         |
| modelYearGroupName      | char(50)    | YES  |     |         |         |
| modelYearGroupStartYear | smallint(6) | YES  |     |         |         |
| modelYearGroupEndYear   | smallint(6) | YES  |     |         |         |

### modelYearMapping

modelYearMapping maps model years from user-defined model year ranges to standard model year ranges used by effects tables. This table is intended to be used for MOVES international, but is not actively supported.

| Field                  | Type        | Null | Key | Default | Comment |
| ---------------------- | ----------- | ---- | --- | ------- | ------- |
| startUserModelYear     | smallint(6) | NO   | PRI |         |         |
| endUserModelYear       | smallint(6) | NO   | PRI |         |         |
| startStandardModelYear | smallint(6) | NO   |     |         |         |
| endStandardModelYear   | smallint(6) | NO   |     |         |         |

### monthGroupHour

monthGroupHour  is used to calculate air conditioning demand as a function of heat index.  Together, A, B, and C define a quadratic equation used to calculate AC demand  as a function of heat index. 

| Field             | Type        | Null | Key  | Default | Comment  |
| ----------------- | ----------- | ---- | ---- | ------- | -------- |
| monthGroupID      | smallint(6) | NO   | PRI  | 0       |          |
| hourID            | smallint(6) | NO   | PRI  | 0       |          |
| ACActivityTermA   | float       | YES  |      |         |          |
| ACActivityTermACV | float       | YES  |      |         | not used |
| ACActivityTermB   | float       | YES  |      |         |          |
| ACActivityTermBCV | float       | YES  |      |         | not used |
| ACActivityTermC   | float       | YES  |      |         |          |
| ACActivityTermCCV | float       | YES  |      |         | not used |

### monthGroupOfAnyYear

monthGroupHour describes the month groups used in MOVES. Typically, each month is used as its own group.

| Field          | Type        | Null | Key | Default | Comment |
| -------------- | ----------- | ---- | --- | ------- | ------- |
| monthGroupID   | smallint(6) | NO   | PRI | 0       |         |
| monthGroupName | char(50)    | YES  |     |         |         |

### monthOfAnyYear

monthOfAnyYear contains metadata for each month.

| Field        | Type        | Null | Key | Default | Comment                     |
| ------------ | ----------- | ---- | --- | ------- | --------------------------- |
| monthID      | smallint(6) | NO   | PRI | 0       |                             |
| monthName    | char(10)    | YES  |     |         |                             |
| noOfDays     | smallint(6) | YES  |     |         | number of days in the month |
| monthGroupID | smallint(6) | NO   | MUL | 0       |                             |

### monthVMTFraction

monthVMTFraction is used to allocate VMT by month of the year.

| Field            | Type        | Null | Key | Default | Comment                                                                        |
| ---------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------------------ |
| sourceTypeID     | smallint(6) | NO   | PRI | 0       |                                                                                |
| monthID          | smallint(6) | NO   | PRI | 0       |                                                                                |
| monthVMTFraction | float       | YES  |     |         | fraction of VMT allocated to each month, summing to 1 for each<br> source type |

### noNO2Ratio

noNO2Ratio is used to calculate NO and NO2 emission based on NOx emissions.

| Field            | Type        | Null | Key | Default | Comment                            |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------- |
| polProcessID     | int(11)     | NO   | PRI | 0       |                                    |
| sourceTypeID     | smallint(6) | NO   | PRI | 0       |                                    |
| fuelTypeID       | smallint(6) | NO   | PRI | 0       |                                    |
| modelYearGroupID | int(11)     | NO   | PRI | 0       |                                    |
| NOxRatio         | float       | YES  |     |         | ratio of NOx emissions which is NO |
| NOxRatioCV       | float       | YES  |     |         | not used                           |
| dataSourceId     | smallint(6) | YES  |     |         |                                    |

### nrAgeCategory

nrAgeCategory defines the age categories used in the nonroad model.

| Field           | Type        | Null | Key | Default | Comment |
| --------------- | ----------- | ---- | --- | ------- | ------- |
| ageID           | smallint(6) | NO   | PRI |         |         |
| ageCategoryName | char(50)    | YES  |     |         |         |

### nrATRatio

nrATRatio is the nonroad equivalent of the atRatio table, except that it is not empty by default.

| Field         | Type        | Null | Key  | Default | Comment |
| ------------- | ----------- | ---- | ---- | ------- | ------- |
| pollutantID   | smallint(6) | NO   | PRI  |         |         |
| processID     | smallint(6) | NO   | PRI  |         |         |
| engTechID     | smallint(6) | NO   | PRI  |         |         |
| fuelSubtypeID | smallint(6) | NO   | PRI  |         |         |
| nrHPCategory  | char(1)     | NO   | PRI  |         |         |
| atRatio       | double      | YES  |      |         |         |
| atRatioCV     | double      | YES  |      |         |         |
| dataSourceId  | smallint(6) | YES  |      |         |         |

### nrBaseYearEquipPopulation

nrBaseYearEquipPopulation provides base year equipment populations by NR source type (each source type is a unique SCC/HP bin combination) and state. Populations are grown to other years using the NRGrowthIndex tables and allocated to counties using NRStateSurrogate table.

| Field        | Type        | Null | Key  | Default | Comment                                                      |
| ------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeID | smallint(6) | NO   | PRI  |         |                                                              |
| stateID      | smallint(6) | NO   | PRI  |         |                                                              |
| population   | double      | NO   |      |         | number of equipment                                          |
| NRBaseYearID | smallint(6) | NO   |      |         | The population values in this table are for a "base year"; these are grown to other years using a growth index, which is relative to this base year. |

### nrCrankcaseEmissionRate

nrCrankCaseEmissionRate contains base crankcase emission rates for the nonroad model.

| Field        | Type        | Null | Key  | Default | Comment |
| ------------ | ----------- | ---- | ---- | ------- | ------- |
| polProcessID | int(11)     | NO   | PRI  |         |         |
| SCC          | char(10)    | NO   | PRI  |         |         |
| hpMin        | smallint(6) | NO   | PRI  |         |         |
| hpMax        | smallint(6) | NO   | PRI  |         |         |
| modelYearID  | smallint(6) | NO   | PRI  |         |         |
| engTechID    | smallint(6) | NO   | PRI  |         |         |
| meanBaseRate | float       | YES  |      |         |         |
| units        | varchar(12) | YES  |      |         |         |
| dataSourceID | smallint(6) | NO   |      |         |         |

### nrDayAllocation

nrDayAllocation allocates nonroad activity by the day of the week.

| Field       | Type        | Null | Key | Default | Comment                                                  |
| ----------- | ----------- | ---- | --- | ------- | -------------------------------------------------------- |
| scc         | char(10)    | NO   | PRI |         |                                                          |
| dayID       | smallint(6) | NO   | PRI |         |                                                          |
| dayFraction | float       | NO   |     |         | fraction of total activity associated with each day type |

### nrDeterioration

nrDeterioration contains emissions increases due to equipment aging in nonroad according to a deterioration equation. This table contains the coefficients for that equation, which vary by pollutant and engine tier.

| Field         | Type        | Null | Key | Default | Comment                                                                                                                                           |
| ------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| polProcessID  | int(11)     | NO   | PRI |         |                                                                                                                                                   |
| engTechID     | smallint(6) | NO   | PRI |         |                                                                                                                                                   |
| DFCoefficient | float       | YES  |     |         | DF = 1 + (DFCoefficient) * (Age Factor)^(DFAgeExponent) for Age Factor <= 1; DF = 1 + (DFCoefficient) for Age Factor > 1                          |
| DFAgeExponent | float       | YES  |     |         | DF = 1 + (DFCoefficient) * (Age Factor)^(DFAgeExponent) for Age Factor <= 1; DF = 1 + (DFCoefficient) for Age Factor > 1                          |
| emissionCap   | smallint(6) | NO   |     |         | Values can be 1 (deterioration is capped at end of engine's median life) or 2 (deterioration is not capped). No cap for all non-handheld engines. |

### nrDioxinEmissionRate

nrDioxinEmissionRate contains base dioxin emission rates for the nonroad model.

| Field          | Type        | Null | Key | Default | Comment  |
| -------------- | ----------- | ---- | --- | ------- | -------- |
| pollutantID    | smallint(6) | NO   | PRI |         |          |
| processID      | smallint(6) | NO   | PRI |         |          |
| fuelTypeID     | smallint(6) | NO   | PRI |         |          |
| engtechID      | smallint(6) | NO   | PRI |         |          |
| nrHPCategory   | char(1)     | NO   | PRI |         |          |
| units          | char(30)    | YES  |     |         |          |
| meanBaseRate   | double      | YES  |     |         |          |
| meanBaseRateCV | double      | YES  |     |         | not used |
| dataSourceID   | smallint(6) | YES  |     |         |          |

### nrEmissionRate

nrEmissionRate contains most exhaust emission rates used in the nonroad model.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| polProcessID | int(11)     | NO   | PRI |         |         |
| SCC          | char(10)    | NO   | PRI |         |         |
| hpMin        | smallint(6) | NO   | PRI |         |         |
| hpMax        | smallint(6) | NO   | PRI |         |         |
| modelYearID  | smallint(6) | NO   | PRI |         |         |
| engTechID    | smallint(6) | NO   | PRI |         |         |
| meanBaseRate | float       | YES  |     |         |         |
| units        | varchar(12) | YES  |     |         |         |
| dataSourceID | smallint(6) | NO   |     |         |         |

### nrEngTechFraction

nrEngTechFraction allocates nonroad engines by model year to engine tier (which is represented by engineTechID).

| Field             | Type        | Null | Key  | Default | Comment                                                      |
| ----------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| SCC               | char(10)    | NO   | PRI  |         |                                                              |
| hpMin             | smallint(6) | NO   | PRI  |         |                                                              |
| hpMax             | smallint(6) | NO   | PRI  |         |                                                              |
| modelYearID       | smallint(6) | NO   | PRI  |         |                                                              |
| processGroupID    | smallint(6) | NO   | PRI  |         |                                                              |
| engTechID         | smallint(6) | NO   | PRI  |         |                                                              |
| NREngTechFraction | float       | YES  |      |         | fraction of engines of the engine technology, summing to 1 for each SCC, hpMin, hpMax, modelYearID, processGroupID combination |

### nrEquipmentType

nrEquipmentType lists the nonroad equipment types in MOVES and has some metadata.

| Field               | Type        | Null | Key  | Default | Comment                                                      |
| ------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| NREquipTypeID       | smallint(6) | NO   | PRI  |         |                                                              |
| description         | char(40)    | YES  |      |         |                                                              |
| sectorID            | smallint(6) | NO   |      |         |                                                              |
| useDefaultScrappage | char(1)     | YES  |      |         | 'Y' if the equipment type should use the default scrappage curves |
| surrogateID         | smallint(6) | YES  |      |         |                                                              |

### nrEvapEmissionRate

nrEvapEmissionRate contains evaporative emission rates used in the nonroad model.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| polProcessID | int(11)     | NO   | PRI |         |         |
| SCC          | char(10)    | NO   | PRI |         |         |
| hpMin        | smallint(6) | NO   | PRI |         |         |
| hpMax        | smallint(6) | NO   | PRI |         |         |
| modelYearID  | smallint(6) | NO   | PRI |         |         |
| engTechID    | smallint(6) | NO   | PRI |         |         |
| meanBaseRate | float       | YES  |     |         |         |
| units        | varchar(12) | YES  |     |         |         |
| dataSourceID | smallint(6) | NO   |     |         |         |

### nrFuelSubtype

nrFuelSubtype lists the nonroad fuel sub types and their properties. For column definitions, see the fuelSubType table.

| Field                          | Type        | Null | Key  | Default | Comment  |
| ------------------------------ | ----------- | ---- | ---- | ------- | -------- |
| fuelSubtypeID                  | smallint(6) | NO   | PRI  | 0       |          |
| fuelTypeID                     | smallint(6) | NO   | MUL  | 0       |          |
| fuelSubtypeDesc                | char(50)    | YES  |      |         |          |
| fuelSubtypePetroleumFraction   | float       | YES  |      |         |          |
| fuelSubtypePetroleumFractionCV | float       | YES  |      |         | not used |
| fuelSubtypeFossilFraction      | float       | YES  |      |         |          |
| fuelSubtypeFossilFractionCV    | float       | YES  |      |         | not used |
| carbonContent                  | float       | YES  |      |         |          |
| oxidationFraction              | float       | YES  |      |         |          |
| carbonContentCV                | float       | YES  |      |         | not used |
| oxidationFractionCV            | float       | YES  |      |         | not used |
| energyContent                  | float       | YES  |      |         |          |

### nrFuelSupply

nrFuelSupply lists the nonroad fuels used in each fuel region and their market share. For column information, see fuelSupply.

| Field             | Type        | Null | Key | Default | Comment  |
| ----------------- | ----------- | ---- | --- | ------- | -------- |
| fuelRegionID      | int(11)     | NO   | PRI | 0       |          |
| fuelYearID        | int(11)     | NO   | PRI | 0       |          |
| monthGroupID      | smallint(6) | NO   | PRI | 0       |          |
| fuelFormulationID | int(11)     | NO   | PRI | 0       |          |
| marketShare       | float       | YES  |     |         |          |
| marketShareCV     | float       | YES  |     |         | not used |

### nrFueltype

nrFueltype lists the nonroad fuel types and contains some of their properties and metadata. For column information, see fuelType.

| Field                     | Type        | Null | Key | Default | Comment  |
| ------------------------- | ----------- | ---- | --- | ------- | -------- |
| fuelTypeID                | smallint(6) | NO   | PRI | 0       |          |
| defaultFormulationID      | smallint(6) | NO   |     | 0       |          |
| fuelTypeDesc              | char(50)    | YES  |     |         |          |
| fuelDensity               | float       | YES  |     |         |          |
| subjectToEvapCalculations | char(1)     | NO   |     | N       |          |

### nrGrowthIndex

nrGrowthIndex contains the equipment population growth data. 

| Field           | Type        | Null | Key  | Default | Comment                                                      |
| --------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| growthPatternID | smallint(6) | NO   | PRI  |         |                                                              |
| yearID          | smallint(6) | NO   | PRI  |         |                                                              |
| growthIndex     | smallint(6) | YES  |      |         | Each growth pattern is indexed to 1000 in 1990. A growth index value of 1500 in 2000 means that the equipment population is 1.5x larger in 2000 than it was in 1990. |

### nrGrowthPattern

nrGrowthPattern maps a growth pattern ID to its description.

| Field           | Type        | Null | Key | Default | Comment |
| --------------- | ----------- | ---- | --- | ------- | ------- |
| growthPatternID | smallint(6) | NO   | PRI |         |         |
| description     | char(80)    | YES  |     |         |         |

### nrGrowthPatternFinder

nrGrowthPatternFinder associates growth patterns with the states and SCCs for which they should be used

| Field           | Type        | Null | Key | Default | Comment                                    |
| --------------- | ----------- | ---- | --- | ------- | ------------------------------------------ |
| SCC             | char(10)    | NO   | PRI |         |                                            |
| stateID         | smallint(6) | NO   | PRI |         |                                            |
| growthPatternID | smallint(6) | NO   |     |         | growth pattern used for each SCC and month |

### nrHCSpeciation

nrHCSpeciation is the nonroad equivalent of the hcSpeciation table.

| Field                | Type        | Null | Key  | Default | Comment   |
| -------------------- | ----------- | ---- | ---- | ------- | --------- |
| pollutantID          | smallint(6) | NO   | PRI  |         |           |
| processID            | smallint(6) | NO   | PRI  |         |           |
| engTechID            | smallint(6) | NO   | PRI  |         |           |
| fuelSubtypeID        | smallint(6) | NO   | PRI  |         |           |
| nrHPCategory         | char(1)     | NO   | PRI  |         |           |
| speciationConstant   | double      | YES  |      |         | NMOG/NMHC |
| speciationConstantCV | double      | YES  |      |         | not used  |
| dataSourceId         | smallint(6) | YES  |      |         |           |

### nrHourAllocation

nrhourallocation is used to allocate nonroad activity by the hour of the day, for various allocation patterns.

| Field                | Type        | Null | Key  | Default | Comment                                  |
| -------------------- | ----------- | ---- | ---- | ------- | ---------------------------------------- |
| NRHourAllocPatternID | smallint(6) | NO   | PRI  |         |                                          |
| hourID               | smallint(6) | NO   | PRI  |         |                                          |
| hourFraction         | float       | NO   |      |         | fraction of total activity for each hour |

### nrHourAllocpattern

nrHourAllocPattern  lists nonroad hourly activity patterns with text descriptions.

| Field                | Type        | Null | Key  | Default | Comment |
| -------------------- | ----------- | ---- | ---- | ------- | ------- |
| NRHourAllocPatternID | smallint(6) | NO   | PRI  |         |         |
| description          | char(255)   | NO   |      |         |         |

### nrHourPatternFinder

nrHourPatternFinder associates an  appropriate hourly activity pattern (NRAllocPattern) with each nonroad  equipment type.

| Field                | Type        | Null | Key  | Default | Comment |
| -------------------- | ----------- | ---- | ---- | ------- | ------- |
| NREquipTypeID        | smallint(6) | NO   | PRI  |         |         |
| NRHourAllocPatternID | smallint(6) | YES  |      |         |         |

### nrHPCategory

nrHPCategory  lists the horsepower categories used for each engine technology (or emission tier).

| Field          | Type        | Null | Key  | Default | Comment                                                      |
| -------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| nrhprangebinid | smallint(6) | NO   | PRI  |         |                                                              |
| engtechid      | smallint(6) | NO   | PRI  |         |                                                              |
| nrhpcategory   | char(1)     | YES  |      |         | a horsepower  category used to categorize vehicles by horsepower (either S=Small or  L=Large) |

### nrHPRangeBin

nrHPRangeBin lists the horsepower ranges for each horsepower bin and maps them to an ID integer.

| Field          | Type        | Null | Key  | Default | Comment                                                      |
| -------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| NRHPRangeBinID | smallint(6) | NO   | PRI  |         |                                                              |
| binName        | char(20)    | YES  |      |         |                                                              |
| hpMin          | smallint(6) | YES  |      |         |                                                              |
| hpMax          | smallint(6) | YES  |      |         |                                                              |
| engSizeID      | smallint(6) | NO   |      |         |                                                              |

### nrIntegratedspecies

nrIntegratedSpecies lists the integrated species (pollutants) in MOVES nonroad model.

| Field       | Type        | Null | Key  | Default | Comment |
| ----------- | ----------- | ---- | ---- | ------- | ------- |
| pollutantID | smallint(6) | NO   | PRI  |         |         |

### nrMetalEmissionRate

nrmetalemissionrate contains the exhaust rates for metals for nonroad.

| Field          | Type        | Null | Key  | Default | Comment  |
| -------------- | ----------- | ---- | ---- | ------- | -------- |
| pollutantID    | smallint(6) | NO   | PRI  |         |          |
| processID      | smallint(6) | NO   | PRI  |         |          |
| fuelTypeID     | smallint(6) | NO   | PRI  |         |          |
| engTechID      | smallint(6) | NO   | PRI  |         |          |
| nrHPCategory   | char(1)     | NO   | PRI  |         |          |
| units          | char(12)    | YES  |      |         |          |
| meanBaseRate   | double      | YES  |      |         |          |
| meanBaseRateCV | double      | YES  |      |         | not used |
| dataSourceId   | smallint(6) | YES  |      |         |          |

### nrMethaneTHCRatio

nrMethaneTHCRatio is used to calcualte methane emisisons from total hydrocarbon emissions.

| Field         | Type        | Null | Key  | Default | Comment                                     |
| ------------- | ----------- | ---- | ---- | ------- | ------------------------------------------- |
| processID     | smallint(6) | NO   | PRI  |         |                                             |
| engTechID     | smallint(6) | NO   | PRI  |         |                                             |
| fuelSubtypeID | smallint(6) | NO   | PRI  |         |                                             |
| nrHPCategory  | char(1)     | NO   | PRI  |         |                                             |
| CH4THCRatio   | double      | YES  |      |         | fraction of THC emissions which are methane |
| CH4THCRatioCV | double      | YES  |      |         | not used                                    |
| dataSourceId  | smallint(6) | YES  |      |         |                                             |

### nrMonthAllocation

nrMonthAllocation allocates nonroad activity by month. 

| Field         | Type        | Null | Key  | Default | Comment                                                      |
| ------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| SCC           | char(10)    | NO   | PRI  |         |                                                              |
| stateID       | smallint(6) | NO   | PRI  |         |                                                              |
| monthID       | smallint(6) | NO   | PRI  |         |                                                              |
| monthFraction | float       | NO   |      |         | fraction of activity which takes place in each month, summing  to 1 by SCC and state |

### nrPAHGasRatio

nrPAHGasRatio is the nonroad equivalent of the pahGasRatio table.

| Field        | Type        | Null | Key  | Default | Comment  |
| ------------ | ----------- | ---- | ---- | ------- | -------- |
| pollutantID  | smallint(6) | NO   | PRI  |         |          |
| processid    | smallint(6) | NO   | PRI  |         |          |
| fuelTypeID   | smallint(6) | NO   | PRI  |         |          |
| engTechID    | smallint(6) | NO   | PRI  |         |          |
| nrHPCategory | char(1)     | NO   | PRI  |         |          |
| atratio      | double      | YES  |      |         |          |
| atratioCV    | double      | YES  |      |         | not used |
| dataSourceId | smallint(6) | YES  |      |         |          |

### nrPAHParticleRatio

nrPAHParticleRatio is the nonroad equivalent of the pahParticleRatio table.

| Field        | Type        | Null | Key  | Default | Comment  |
| ------------ | ----------- | ---- | ---- | ------- | -------- |
| pollutantID  | smallint(6) | NO   | PRI  |         |          |
| processid    | smallint(6) | NO   | PRI  |         |          |
| fuelTypeID   | smallint(6) | NO   | PRI  |         |          |
| engTechID    | smallint(6) | NO   | PRI  |         |          |
| nrHPCategory | char(1)     | NO   | PRI  |         |          |
| atratio      | double      | YES  |      |         |          |
| atratioCV    | double      | YES  |      |         | not used |
| datasourceId | smallint(6) | YES  |      |         |          |

### nrProcessGroup

nrProcessGroup is the nonroad equivalent of processGroupID.

| Field            | Type        | Null | Key  | Default | Comment |
| ---------------- | ----------- | ---- | ---- | ------- | ------- |
| processGroupID   | smallint(6) | NO   | PRI  | 0       |         |
| processGroupDesc | char(20)    | YES  |      |         |         |

### nrRetrofitFactors

nrRetrofitFactors is not populated by default, but can be provided by users for custom nonroad runs to account for retrofit programs, similar to the onroadRetrofit table.

| Field                     | Type        | Null | Key  | Default | Comment                                                      |
| ------------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| retrofitStartYear         | smallint(6) | NO   |      |         |                                                              |
| retrofitEndYear           | smallint(6) | NO   |      |         |                                                              |
| StartModelYear            | smallint(6) | NO   |      |         |                                                              |
| EndModelYear              | smallint(6) | NO   |      |         |                                                              |
| SCC                       | char(10)    | NO   | PRI  |         |                                                              |
| engTechID                 | smallint(6) | NO   | PRI  |         |                                                              |
| hpMin                     | smallint(6) | NO   | PRI  |         |                                                              |
| hpMax                     | smallint(6) | NO   | PRI  |         |                                                              |
| pollutantID               | smallint(6) | NO   | PRI  |         |                                                              |
| retrofitID                | smallint(6) | NO   | PRI  |         | arbitrary ID used to model separate retrofit programs        |
| annualFractionRetrofit    | float       | YES  |      |         | fraction of vehicles retrofitted                             |
| retrofitEffectiveFraction | float       | YES  |      |         | effectiveness of the retrofit in reducing emissions. Can be  between 0 and 1 - a value of 0.1 is a 10% reduction in emissions. |

### nrROCSpeciation

nrROCSpeciation (ROC stands for Reactive Organic Carbon) lists the profile from the SPECIATE database that is used to speciate NMOG (non-methane organic gases) and PM to lumped species by fuel sub type, tier, number of strokes, and engine technology, and emission process.

| Field                  | Type        | Null | Key | Default | Comment                                                                               |
|------------------------|-------------|------|-----|---------|---------------------------------------------------------------------------------------|
| fuelSubtypeID          | smallint(6) | NO   | PRI |         |                                                                                       |
| tierID                 | smallint(6) | NO   | PRI |         |                                                                                       |
| strokes                | smallint(6) | NO   | PRI |         |                                                                                       |
| engTechID              | int(11)     | NO   | PRI |         |                                                                                       |
| processID              | int(11)     | NO   | PRI |         |                                                                                       |
| pmSpeciationProfileID  | varchar(10) | YES  |     |         | SPECIATE profile used for PM                                                          |
| CROCCode               | varchar(10) | YES  |     |         | alternate profile ID formulation                                                      |
| CROCOMRatio            | double      | YES  |     |         | conversion factor used to convert from PM organic matter to total Condensible ROC     |
| togSpeciationProfileID | varchar(10) | YES  |     |         | SPECIATE profile used for organic gases                                               |
| GROCCode               | varchar(10) | YES  |     |         | alternate profile ID formulation                                                      |
| GROCNMOGRatio          | double      | YES  |     |         | conversion factor used to convert from non-methane organic gases to total Gaseous ROC |


### nrSCC

nrSCC maps nonroad Source Classification Codes to their component identifications

| Field         | Type        | Null | Key  | Default | Comment                                                      |
| ------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| SCC           | char(10)    | NO   | PRI  |         | 10 digits describe mobile source, fuel type, segment, and  equip type, e.g., 2265004010. |
| NREquipTypeID | smallint(6) | NO   |      |         |                                                              |
| description   | char(40)    | YES  |      |         |                                                              |
| fuelTypeID    | smallint(6) | NO   |      |         |                                                              |

### nrScrappageCurve

nrScrappageCurve  contains a set of fraction of median life used values with corresponding  percent scrapped values for nonroad equipment. Only needed for equipment  types that do not use DefaultScrappage.

| Field              | Type        | Null | Key  | Default | Comment                                                      |
| ------------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| NREquipTypeID      | smallint(6) | NO   | PRI  |         |                                                              |
| fractionLifeused   | float       | NO   | PRI  |         | Value indicating the fraction of the useful life of the equipment that has passed. It can be be greater than 1. |
| percentageScrapped | float       | YES  |      |         | percent of equipment that is scrapped by the fraction of life  used. This column is cumulative, not additive. |

### nrSourceUseType

nrSourceUseType lists the source types used for nonroad, with metadata included.

| Field                          | Type        | Null | Key  | Default | Comment |
| ------------------------------ | ----------- | ---- | ---- | ------- | ------- |
| sourceTypeID                   | smallint(6) | NO   | PRI  |         |         |
| SCC                            | char(10)    | NO   |      |         |         |
| NRHPRangeBinID                 | smallint(6) | NO   |      |         |         |
| medianLifeFullLoad             | float       | YES  |      |         |         |
| hoursUsedPerYear               | float       | YES  |      |         |         |
| loadFactor                     | float       | YES  |      |         |         |
| hpAvg                          | float       | YES  |      |         |         |
| isPumpFilled                   | char(1)     | YES  |      |         |         |
| tankUnits                      | char(7)     | YES  |      |         |         |
| tankSize                       | float       | YES  |      |         |         |
| tankFillFrac                   | float       | YES  |      |         |         |
| tankMetalFrac                  | float       | YES  |      |         |         |
| hoseLength                     | float       | YES  |      |         |         |
| hoseDiameter                   | float       | YES  |      |         |         |
| hoseMetalFrac                  | float       | YES  |      |         |         |
| marineFillNeckHoseLength       | float       | YES  |      |         |         |
| marineFillNeckHoseDiameter     | float       | YES  |      |         |         |
| marineSupplyHoseLength         | float       | YES  |      |         |         |
| marineSupplyHoseDiameter       | float       | YES  |      |         |         |
| marineVentHoseLength           | float       | YES  |      |         |         |
| marineVentHoseDiameter         | float       | YES  |      |         |         |
| hotSoaksPerSHO                 | float       | YES  |      |         |         |
| nonInstMarineTankFrac          | float       | YES  |      |         |         |
| marineInstPlasticTankTrailFrac | float       | NO   |      |         |         |
| marineInstPlasticTankWaterFrac | float       | YES  |      |         |         |
| marineInstMetalTankTrailerFrac | float       | YES  |      |         |         |
| marineInstMetalTankWaterFrac   | float       | YES  |      |         |         |
| e10TankPermeationAdjFac        | float       | YES  |      |         |         |
| e10HosePermeationAdjFac        | float       | YES  |      |         |         |
| e10MarineFillNeckPermAdjFac    | float       | YES  |      |         |         |
| e10MarineSupplyHosePermAdjFac  | float       | YES  |      |         |         |
| e10MarineVentHosePermAdjFac    | float       | YES  |      |         |         |

### nrStateSurrogate

nrStateSurrogate is used to calculate changes in emissions activity based on surrogate data, ie from economic indexes related to the  emissions generating activity.

| Field           | Type        | Null | Key  | Default | Comment                                   |
| --------------- | ----------- | ---- | ---- | ------- | ----------------------------------------- |
| surrogateID     | smallint(6) | NO   | PRI  | 0       |                                           |
| stateID         | smallint(6) | NO   | PRI  | 0       |                                           |
| countyID        | int(11)     | NO   | PRI  | 0       |                                           |
| surrogatequant  | double      | NO   |      |         | Total value of  surrogate for each state. |
| surrogateYearID | smallint(6) | NO   | PRI  | 2002    | First year the surrogateQuant is used     |


### nrSulfurAdjustment

nrSulfurAdjustment contains the nonroad diesel base sulfur level and sulfate PM conversion rate  by tech type.

| Field                     | Type        | Null | Key  | Default | Comment                                                      |
| ------------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelTypeID                | smallint(6) | NO   | PRI  |         |                                                              |
| engTechID                 | smallint(6) | NO   | PRI  |         |                                                              |
| PMBaseSulfur              | float       | NO   |      |         |                                                              |
| sulfatePMConversionFactor | float       | NO   |      |         | Nonroad diesel base sulfur level and sulfate PM conversion rate by tech  type. |

### nrSurrogate

nrSurrogate lists the Nonroad geographic allocation surrogates with a description of each. Surrogates are typically economic projections of various sectors, used as a surrogate for emissions generation.

| Field         | Type        | Null | Key  | Default | Comment |
| ------------- | ----------- | ---- | ---- | ------- | ------- |
| surrogateID   | smallint(6) | NO   | PRI  |         |         |
| description   | char(255)   | YES  |      |         |         |
| surrogateAbbr | char(3)     | YES  |      |         |         |


### nrUSMonthAllocation

nrUSMonthAllocation allocates activty in a month by state and equipment type.

| Field         | Type        | Null | Key  | Default | Comment                                                      |
| ------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| SCC           | char(10)    | NO   | PRI  |         |                                                              |
| stateID       | smallint(6) | NO   | PRI  |         |                                                              |
| monthID       | smallint(6) | NO   | PRI  |         |                                                              |
| monthFraction | float       | NO   |      |         | Fraction of annual activity in each month, by state and  nonroad equipment type. Sums to 1 for each state, SCC combination |

### offNetworkLink

offNetworkLink contains user-provided information about off-network activity in project scale.

| Field                 | Type        | Null | Key  | Default | Comment                                                      |
| --------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeID          | smallint(6) | NO   | PRI  |         |                                                              |
| zoneID                | int(11)     | NO   | PRI  | 0       |                                                              |
| vehiclePopulation     | float       | YES  |      |         | total number of vehicles present in the project scale run    |
| startFraction         | float       | YES  |      |         | fraction of population that start during the hour            |
| extendedIdleFraction  | float       | YES  |      |         | fraction of the time the vehicle population is hotelling during the hour |
| parkedVehicleFraction | float       | YES  |      |         | fraction of vehicles parked during the hour                  |

### omdgPolProcessRepresented

omdgPolProcessRepresented  is used during runtime to optimize the Operating Mode Distribution geneartor at Project Scale.

| Field                    | Type    | Null | Key  | Default | Comment |
| ------------------------ | ------- | ---- | ---- | ------- | ------- |
| polProcessID             | int(11) | NO   | PRI  |         |         |
| representingPolProcessID | int(11) | NO   | MUL  |         |         |

### onRoadRetrofit

onroadRetrofit contains no data by default, but is a table users can provide to model the impact of retrofit programs.

| Field                     | Type        | Null | Key  | Default | Comment                                                   |
| ------------------------- | ----------- | ---- | ---- | ------- | --------------------------------------------------------- |
| pollutantID               | smallint(6) | NO   | PRI  |         |                                                           |
| processID                 | smallint(6) | NO   | PRI  |         |                                                           |
| fuelTypeID                | smallint(6) | NO   | PRI  |         |                                                           |
| sourceTypeID              | smallint(6) | NO   | PRI  |         |                                                           |
| retrofitYearID            | smallint(6) | NO   | PRI  |         | synonym for calendar yearID                               |
| beginModelYearID          | smallint(6) | NO   | PRI  |         |                                                           |
| endModelYearID            | smallint(6) | NO   | PRI  |         |                                                           |
| cumFractionRetrofit       | double      | NO   |      | 0       | fraction of vehicles retrofit                             |
| retrofitEffectiveFraction | double      | NO   |      | 0       | effectiveness of the retrofit technology, between 0 and 1 |

### operatingMode

operatingMode defines the operating modes used in MOVES, for all emission processes, and the ways they are assigned to activity.

| Field         | Type        | Null | Key | Default | Comment |
| ------------- | ----------- | ---- | --- | ------- | ------- |
| opModeID      | smallint(6) | NO   | PRI | 0       |         |
| opModeName    | char(50)    | YES  |     |         |         |
| VSPLower      | float       | YES  |     |         |         |
| VSPUpper      | float       | YES  |     |         |         |
| speedLower    | float       | YES  |     |         |         |
| speedUpper    | float       | YES  |     |         |         |
| brakeRate1Sec | float       | YES  |     |         |         |
| brakeRate3Sec | float       | YES  |     |         |         |
| minSoakTime   | smallint(6) | YES  |     |         |         |
| maxSoakTime   | smallint(6) | YES  |     |         |         |

### opModeDistribution

opModeDistribution is a user input table used at project scale, to be used when the user has full operating mode level detail of activity.

| Field            | Type        | Null | Key | Default | Comment                                                                                                                            |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| sourceTypeID     | smallint(6) | NO   | PRI | 0       |                                                                                                                                    |
| hourDayID        | smallint(6) | NO   | PRI | 0       |                                                                                                                                    |
| linkID           | int(11)     | NO   | PRI | 0       |                                                                                                                                    |
| polProcessID     | int(11)     | NO   | PRI | 0       |                                                                                                                                    |
| opModeID         | smallint(6) | NO   | PRI | 0       |                                                                                                                                    |
| opModeFraction   | float       | YES  |     |         | fraction of time (not VMT) spent in each operating mode. Sums to 1 for each source type, hourDay, link, and polProcess combination |
| opModeFractionCV | float       | YES  |     |         | not used                                                                                                                           |
| isUserInput      | char(1)     | NO   |     | N       |                                                                                                                                    |

### opModePolProcAssoc

opModePolProcAssoc contains a list of all the relevant operating modes for calculating emissions from each pollutant and process combination.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| polProcessID | int(11)     | NO   | PRI | 0       |         |
| opModeID     | smallint(6) | NO   | PRI | 0       |         |

### oxyThreshName

oxythreshname contains the parameters necessary for classifying a given fuel as  'oxygenated' (1 represents oxygenated, 0 represents non-oxygenated).

| Field         | Type        | Null | Key  | Default | Comment |
| ------------- | ----------- | ---- | ---- | ------- | ------- |
| oxyThreshID   | smallint(6) | NO   | PRI  | 0       |         |
| oxyThreshName | char(100)   | YES  |      |         |         |

### pahGasRatio

pahGasRatio is used to calculate emissions for gaseous PAHs (Polycyclic Aromatic Hydrocarbons) from VOC emissions.

| Field            | Type        | Null | Key  | Default | Comment  |
| ---------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID     | int(11)     | NO   | PRI  | 0       |          |
| fuelTypeID       | smallint(6) | NO   | PRI  | 0       |          |
| modelYearGroupID | int(11)     | NO   | PRI  | 0       |          |
| atRatio          | double      | YES  |      |         |          |
| atRatioCV        | double      | YES  |      |         | not used |
| dataSourceId     | smallint(6) | YES  |      |         |          |

### pahParticleRatio

pahParticleRatio is used to calculate emissions for particulate PAHs (Polycyclic Aromatic Hydrocarbons) from VOC emissions.

| Field            | Type        | Null | Key  | Default | Comment  |
| ---------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID     | int(11)     | NO   | PRI  | 0       |          |
| fuelTypeID       | smallint(6) | NO   | PRI  | 0       |          |
| modelYearGroupID | int(11)     | NO   | PRI  | 0       |          |
| atRatio          | double      | YES  |      |         |          |
| atRatioCV        | double      | YES  |      |         | not used |
| dataSourceId     | smallint(6) | YES  |      |         |          |

### pm10EmissionRatio

pm10EmissionRatio is used to calculate PM10 from PM2.5 emissions.

| Field           | Type        | Null | Key  | Default | Comment                                                      |
| --------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| polProcessID    | int(11)     | NO   | PRI  |         |                                                              |
| sourceTypeID    | smallint(6) | NO   | PRI  |         |                                                              |
| fuelTypeID      | smallint(6) | NO   | PRI  |         |                                                              |
| PM10PM25Ratio   | float       | NO   |      |         | The mass ratio of PM10 emissions to PM2.5 emissions. A value of 1 is 1 gram of PM10 per gram of PM2.5. |
| PM10PM25RatioCV | float       | YES  |      |         | not used                                                     |
| minModelYearID  | smallint(6) | NO   | PRI  | 1940    |                                                              |
| maxModelYearID  | smallint(6) | NO   | PRI  | 2050    |                                                              |

### pmSpeciation

pmspeciation  is used to speciate particulate matter emissions (PM2.5 only) to individual  species.

| Field                | Type        | Null | Key  | Default | Comment                                                      |
| -------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| processID            | smallint(6) | NO   | PRI  |         |                                                              |
| inputPollutantID     | smallint(6) | NO   | PRI  |         |                                                              |
| sourceTypeID         | smallint(6) | NO   | PRI  |         |                                                              |
| fuelTypeID           | smallint(6) | NO   | PRI  |         |                                                              |
| minModelYearID       | smallint(6) | NO   | PRI  |         |                                                              |
| maxModelYearID       | smallint(6) | NO   | PRI  |         |                                                              |
| outputPollutantID    | smallint(6) | NO   | PRI  |         |                                                              |
| pmSpeciationFraction | double      | NO   |      |         | fraction of inputPollutantID mass that is the  outputPollutantID. Sums to 1 for each process, input pollutant, source type, fuel type, min model year and max model year. |

### pollutant

pollutant defines every pollutant used in MOVES and contains additional metadata.

| Field                   | Type        | Null | Key | Default | Comment                                                                                                                                                                                            |
| ----------------------- | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| pollutantID             | smallint(6) | NO   | PRI | 0       |                                                                                                                                                                                                    |
| pollutantName           | char(50)    | YES  |     |         |                                                                                                                                                                                                    |
| energyOrMass            | char(6)     | NO   |     |         |                                                                                                                                                                                                    |
| globalWarmingPotential  | smallint(6) | YES  |     |         | Global warming potential is only defined for greenhouse gases. The global warming potential is the heat absorbed by the gaseous pollutant in the atmosphere relative to CO2 over a 100 year period |
| NEIPollutantCode        | char(10)    | YES  |     |         |                                                                                                                                                                                                    |
| pollutantDisplayGroupID | smallint(6) | YES  |     |         |                                                                                                                                                                                                    |
| shortName               | varchar(50) | YES  |     |         |                                                                                                                                                                                                    |
| isAffectedByOnroad      | tinyint(1)  | YES  |     | 1       | 1 (pollutant emissions are calculated for onroad runs), or 0 (pollutant is not considered in onroad runs)                                                                                          |
| isAffectedByNonroad     | tinyint(1)  | YES  |     | 0       | nonroad equivalent of `isAffectedByOnroad`                                                                                                                                                         |

### pollutantDisplayGroup

pollutantDisplayGroup is used by the MOVES GUI to create the Pollutants and Processes Panel.

| Field                     | Type        | Null | Key | Default | Comment |
| ------------------------- | ----------- | ---- | --- | ------- | ------- |
| pollutantDisplayGroupID   | smallint(6) | NO   | PRI | 0       |         |
| pollutantDisplayGroupName | char(50)    | NO   |     |         |         |
| disPlayAsGroup            | char(1)     | NO   |     | N       |         |

### pollutantProcessAssoc

pollutantProcessAssoc lists all the pollutant and process pairs that can be run in MOVES, and contains metadata and runtime information for each pair.

| Field                 | Type        | Null | Key  | Default | Comment                                                      |
| --------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| polProcessID          | int(11)     | NO   | PRI  | 0       |                                                              |
| processID             | smallint(6) | NO   | MUL  | 0       |                                                              |
| pollutantID           | smallint(6) | NO   | MUL  | 0       |                                                              |
| isAffectedByExhaustIM | char(1)     | NO   |      | N       | defines, with a 'Y' or 'N', whether the polProcess emission rates are changed by exhaust IM programs |
| isAffectedByEvapIM    | char(1)     | NO   |      | N       | defines, with a 'Y' or 'N', whether the polProcess emission rates are changed by evaporative IM programs |
| chainedto1            | int(11)     | YES  |      |         | polProcessID used during MOVES runtime to calculate the keyed polProcessID |
| chainedto2            | int(11)     | YES  |      |         | polProcessID used during MOVES runtime to calculate the keyed polProcessID |
| isAffectedByOnroad    | tinyint(1)  | YES  |      | 1       |                                                              |
| isAffectedByNonroad   | tinyint(1)  | YES  |      | 0       |                                                              |
| nrChainedTo1          | int(11)     | YES  |      |         | nonroad equivalent of `chainedTo1`                           |
| nrChainedTo2          | int(11)     | YES  |      |         | nonroad equivalent of `chainedTo2`                           |

### pollutantProcessModelYear

pollutantProcessModelYear lists every polProcessID and modelYearID combination, and maps that to fuel year and IM model year groupings for the purposes of adjusting emissions.

| Field              | Type        | Null | Key | Default | Comment |
| ------------------ | ----------- | ---- | --- | ------- | ------- |
| polProcessID       | int(11)     | NO   | PRI | 0       |         |
| modelYearID        | smallint(6) | NO   | PRI | 0       |         |
| modelYearGroupID   | int(11)     | NO   |     | 0       |         |
| fuelMYGroupID      | int(11)     | YES  |     |         |         |
| IMModelYearGroupID | int(11)     | YES  |     |         |         |

### processDisplayGroup

processDisplayGroup is used by the MOVES GUI to render the Pollutants and Processes panel.

| Field                   | Type        | Null | Key | Default | Comment |
| ----------------------- | ----------- | ---- | --- | ------- | ------- |
| processDisplayGroupID   | smallint(6) | NO   | PRI |         |         |
| processDisplayGroupName | char(50)    | NO   |     |         |         |
| disPlayAsGroup          | char(1)     | NO   |     |         |         |

### processGroupID

processGroupID defines two process groups - Exhaust or Evaporative.

| Field            | Type        | Null | Key  | Default | Comment |
| ---------------- | ----------- | ---- | ---- | ------- | ------- |
| processGroupID   | smallint(6) | NO   | PRI  |         |         |
| processGroupName | char(15)    | NO   |      |         |         |

### refuelingControlTechnology

refuelingControlTechnology is used to adjust refueling vapor loss emissions (processID 18) for vehicles with Onboard Refueling Vapor Recovery (ORVR).

| Field                   | Type        | Null | Key  | Default | Comment                                                        |
| ----------------------- | ----------- | ---- | ---- | ------- | -------------------------------------------------------------- |
| processID               | smallint(6) | NO   | PRI  |         |                                                                |
| modelYearID             | smallint(6) | NO   | PRI  |         |                                                                |
| regClassID              | smallint(6) | NO   | PRI  |         |                                                                |
| sourceTypeID            | smallint(6) | NO   | PRI  |         |                                                                |
| fuelTypeID              | smallint(6) | NO   | PRI  |         |                                                                |
| ageID                   | smallint(6) | NO   | PRI  |         |                                                                |
| refuelingTechAdjustment | float       | NO   |      | 0       | the fraction of vehicles with functional ORVR systems          |
| controlledRefuelingRate | float       | NO   |      | 0       | the refueling vapor losses (g/gal) in a functional ORVR system. 0 if the fraction of vehicles with functional ORVR is also 0. |

### refuelingFactors

refuelingFactors contains terms and coefficients used to calculate the vapor displacement rate for refueling emissions.

| Field                     | Type        | Null | Key  | Default | Comment  |
| ------------------------- | ----------- | ---- | ---- | ------- | -------- |
| fuelTypeID                | smallint(6) | NO   | PRI  |         |          |
| defaultFormulationID      | smallint(6) | YES  |      |         |          |
| vaporTermA                | float       | NO   |      | 0       |          |
| vaporTermB                | float       | NO   |      | 0       |          |
| vaporTermC                | float       | NO   |      | 0       |          |
| vaporTermD                | float       | NO   |      | 0       |          |
| vaporTermE                | float       | NO   |      | 0       |          |
| vaporTermF                | float       | NO   |      | 0       |          |
| vaporLowTLimit            | float       | NO   |      | 0       |          |
| vaporHighTLimit           | float       | NO   |      | 0       |          |
| tankTDiffLimit            | float       | NO   |      | 0       |          |
| minimumRefuelingVaporLoss | float       | NO   |      | 0       |          |
| refuelingSpillRate        | float       | NO   |      | 0       |          |
| refuelingSpillRateCV      | float       | NO   |      | 0       | not used |
| displacedVaporRateCV      | float       | NO   |      | 0       | not used |

### regClassFraction

regClassFraction is used to allocate activity in source types which can have multiple reg classes (most heavy-duty source types in addition to light trucks) to each regulatory class for the purposes of finding emission rates. This table is empty by default, and the same information can be found in sampleVehiclePopulation.

| Field                 | Type        | Null | Key | Default | Comment                                                                                                    |
| --------------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------------------------------------- |
| sourceTypeModelYearID | int(6)      | NO   | PRI | 0       |                                                                                                            |
| fuelTypeID            | smallint(6) | NO   | PRI | 0       |                                                                                                            |
| engTechID             | smallint(6) | NO   | PRI | 0       |                                                                                                            |
| regClassID            | smallint(6) | NO   | PRI | 0       |                                                                                                            |
| regClassFraction      | float       | NO   |     | 0       | fraction of vehicles which are of each reg class within the source type, model year, and engine technology |

### region

region is used to define the regionIDs (also called fuelRegionIDs) used in MOVES to identify fuel supply regions. The IDs themselves are concatenations of smaller IDs which help delineate properties of each region.

| Field       | Type         | Null | Key | Default | Comment |
| ----------- | ------------ | ---- | --- | ------- | ------- |
| regionID    | int(11)      | NO   | PRI |         |         |
| VV          | smallint(6)  | YES  |     |         |         |
| WW          | smallint(6)  | YES  |     |         |         |
| XX          | smallint(6)  | YES  |     |         |         |
| YY          | smallint(6)  | YES  |     |         |         |
| ZZ          | smallint(6)  | YES  |     |         |         |
| description | varchar(150) | YES  |     |         |         |

### regionCode

regionCode defines the regionCodeIDs used in MOVES, which are used to distinguish onroad and nonroad fuels.

| Field                 | Type         | Null | Key | Default | Comment |
| --------------------- | ------------ | ---- | --- | ------- | ------- |
| regionCodeID          | int(11)      | NO   | PRI |         |         |
| regionCodeDescription | varchar(200) | NO   |     |         |         |

### regionCounty

regionCounty defines the fuels used in each county by mapping them to a fuel region.

| Field        | Type    | Null | Key | Default | Comment                  |
| ------------ | ------- | ---- | --- | ------- | ------------------------ |
| regionID     | int(11) | NO   | PRI |         | synonym for fuelRegionID |
| countyID     | int(11) | NO   | PRI |         |                          |
| regionCodeID | int(11) | NO   | PRI |         |                          |
| fuelYearID   | int(11) | NO   | PRI |         |                          |

### regulatoryClass

regulatoryClass defines the regulatory classes used in MOVES for the purposes of looking up emission rates. 

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| regClassID   | smallint(6) | NO   | PRI | 0       |         |
| regClassName | char(25)    | YES  |     |         |         |
| regClassDesc | char(100)   | YES  |     |         |         |

### retrofitInputAssociations

retrofitInputAssociations lists pollutants, processes, source types, and fuel types relevant for retrofit calculations.

| Field      | Type        | Null | Key  | Default | Comment                                                      |
| ---------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| listName   | varchar(20) | NO   | PRI  |         | specifies either pollutant, process, fuel type, or source type |
| commonName | varchar(50) | NO   | PRI  |         | descriptions                                                 |
| idealName  | varchar(50) | NO   |      |         | IDs from the relevant ID mapping tables                      |

### roadIdleFraction

roadIdleFraction  is a pre-processed table used in the ONI Tool for rates mode users to  estimate ONI activity. It is not used during a MOVES run.

| Field            | Type    | Null | Key  | Default | Comment                                                |
| ---------------- | ------- | ---- | ---- | ------- | ------------------------------------------------------ |
| monthID          | int(11) | NO   | PRI  |         |                                                        |
| dayID            | int(11) | NO   | PRI  |         |                                                        |
| sourceTypeID     | int(11) | NO   | PRI  |         |                                                        |
| roadTypeID       | int(11) | NO   | PRI  |         |                                                        |
| avgSpeedBinID    | int(11) | NO   | PRI  |         |                                                        |
| roadIdleFraction | double  | YES  |      |         | fraction of onroad SHO which is  opModeID 1, or idling |

### roadType

roadType defines the road types that are used in MOVES to calculate or allocate activity, and contains metadata for each road type.

| Field               | Type        | Null | Key | Default | Comment            |
| ------------------- | ----------- | ---- | --- | ------- | ------------------ |
| roadTypeID          | smallint(6) | NO   | PRI | 0       |                    |
| roadDesc            | char(50)    | YES  |     |         |                    |
| isAffectedByOnroad  | tinyint(1)  | YES  |     | 1       |                    |
| isAffectedByNonroad | tinyint(1)  | YES  |     | 0       |                    |
| shouldDisplay       | tinyint(1)  | YES  |     | 1       | used for MOVES GUI |

### roadTypeDistribution

roadTypeDistribution allocates total activity to each road type. It contains default values and is a user provided table at county scale.

| Field               | Type        | Null | Key | Default | Comment                                           |
| ------------------- | ----------- | ---- | --- | ------- | ------------------------------------------------- |
| sourceTypeID        | smallint(6) | NO   | PRI | 0       |                                                   |
| roadTypeID          | smallint(6) | NO   | PRI | 0       |                                                   |
| roadTypeVMTFraction | float       | YES  |     |         | fraction of VMT on each road type per source type |

### rocSpeciation

rocSpeciation (ROC stands for Reactive Organic Carbon) lists the profile from the SPECIATE database that is used to speciate NMOG (non-methane organic gases) and PM to lumped species by fuel sub type, regulatory class, process, and model year.

| Field                  | Type        | Null | Key | Default | Comment                                                                               |
|------------------------|-------------|------|-----|---------|---------------------------------------------------------------------------------------|
| fuelSubtypeID          | smallint(6) | NO   | PRI |         |                                                                                       |
| regClassID             | smallint(6) | NO   | PRI |         |                                                                                       |
| processID              | smallint(6) | NO   | PRI |         |                                                                                       |
| minModelYearID         | int(11)     | NO   | PRI |         |                                                                                       |
| maxModelYearID         | int(11)     | NO   | PRI |         |                                                                                       |
| pmSpeciationProfileID  | varchar(10) | YES  |     |         | SPECIATE profile used for PM                                                          |
| CROCCode               | varchar(10) | YES  |     |         | alternate profile ID formulation                                                      |
| CROCOMRatio            | double      | YES  |     |         | conversion factor used to convert from PM organic matter to total Condensible ROC     |
| togSpeciationProfileID | varchar(10) | YES  |     |         | SPECIATE profile used for organic gases                                               |
| GROCCode               | varchar(10) | YES  |     |         | alternate profile ID formulation                                                      |
| GROCNMOGRatio          | double      | YES  |     |         | conversion factor used to convert from non-methane organic gases to total Gaseous ROC |


### sampleVehicleDay

sampleVehicleDay  contains a sample population of vehicles, each with an ID, which are used to  represent vehicle soaking activity for calculating evaporative emissions.

| Field        | Type        | Null | Key  | Default | Comment                                                      |
| ------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| vehID        | int(11)     | NO   | PRI  | 0       |                                                              |
| dayID        | smallint(6) | NO   | PRI  | 0       | indicates the dayIDs in which the vehicle is part of the  population |
| sourceTypeID | smallint(6) | NO   |      | 0       |                                                              |

### sampleVehiclePopulation

sampleVehiclePopulation  defines the default fleet mix in MOVES, breaking source type populations down  by reg class and fuel type by model year.

| Field                 | Type                 | Null | Key  | Default | Comment                                                      |
| --------------------- | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeModelYearID | int(10) unsigned     | NO   | PRI  |         |                                                              |
| sourceTypeID          | smallint(6)          | NO   | MUL  | 0       |                                                              |
| modelYearID           | smallint(6)          | NO   |      | 0       |                                                              |
| fuelTypeID            | smallint(5) unsigned | NO   | PRI  |         |                                                              |
| engTechID             | smallint(6)          | NO   | PRI  |         |                                                              |
| regClassID            | smallint(5) unsigned | NO   | PRI  |         |                                                              |
| stmyFuelEngFraction   | double               | NO   |      |         | fraction of vehicles within the source type, model year, fuel  type combination which will be allocated to the given reg class |
| stmyFraction          | double               | NO   |      |         | fraction of vehicles within the source type, model year  combination which are of the given reg class and fuel tpe |

### sampleVehicleSoaking

sampleVehicleSoaking is empty in the default database and is used during runtime to calculate  activity for evaporative emissions.

| Field        | Type        | Null | Key  | Default | Comment |
| ------------ | ----------- | ---- | ---- | ------- | ------- |
| soakDayID    | smallint(6) | NO   | PRI  |         |         |
| sourceTypeID | smallint(6) | NO   | PRI  |         |         |
| dayID        | smallint(6) | NO   | PRI  |         |         |
| hourID       | smallint(6) | NO   | PRI  |         |         |
| soakFraction | double      | YES  |      |         |         |

### sampleVehicleSoakingDay

sampleVehicleSoakingDay is empty in the default database and is used during runtime to calculate  activity for evaporative emissions.

| Field        | Type        | Null | Key  | Default | Comment |
| ------------ | ----------- | ---- | ---- | ------- | ------- |
| soakDayID    | smallint(6) | NO   | PRI  |         |         |
| sourceTypeID | smallint(6) | NO   | PRI  |         |         |
| dayID        | smallint(6) | NO   | PRI  |         |         |
| F            | double      | YES  |      |         |         |

### sampleVehicleSoakingDayBasis

sampleVehicleSoakingDayBasis is empty in the default database and is used during runtime to calculate  activity for evaporative emissions.

| Field     | Type        | Null | Key  | Default | Comment |
| --------- | ----------- | ---- | ---- | ------- | ------- |
| soakDayID | smallint(6) | NO   | PRI  |         |         |
| dayID     | smallint(6) | NO   | PRI  |         |         |
| F         | double      | YES  |      |         |         |

### sampleVehicleSoakingDayBasisUsed

sampleVehicleSoakingDayBasisUsed contains default data for long-soaking vehicles.

| Field     | Type        | Null | Key  | Default | Comment                                                      |
| --------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| soakDayID | smallint(6) | NO   | PRI  |         | number of days soaking                                       |
| dayID     | smallint(6) | NO   | PRI  |         |                                                              |
| F         | double      | YES  |      |         | fraction of vehicles soaking on the last hour of the soaking  day |

### sampleVehicleSoakingDayUsed

sampleVehicleSoakingDayUsed  is empty in the default database and is used during runtime to calculate  activity for evaporative emissions.

| Field        | Type        | Null | Key  | Default | Comment |
| ------------ | ----------- | ---- | ---- | ------- | ------- |
| soakDayID    | smallint(6) | NO   | PRI  |         |         |
| sourceTypeID | smallint(6) | NO   | PRI  |         |         |
| dayID        | smallint(6) | NO   | PRI  |         |         |
| F            | double      | YES  |      |         |         |

### sampleVehicleTrip

sampleVehicleTrip  contains the key-on and key-off times used in the Fuel Tank Temperature  Generator.

| Field       | Type        | Null | Key  | Default | Comment |
| ----------- | ----------- | ---- | ---- | ------- | ------- |
| vehID       | int(11)     | NO   | PRI  | 0       |         |
| dayID       | smallint(6) | NO   | PRI  | 0       |         |
| tripID      | smallint(6) | NO   | PRI  | 0       |         |
| hourID      | smallint(6) | YES  |      |         |         |
| priorTripID | smallint(6) | YES  | MUL  |         |         |
| keyontime   | int(11)     | YES  |      |         |         |
| keyOffTime  | int(11)     | NO   |      | 0       |         |

### scc

scc defines the onroad mobile source Source Classification Codes in MOVES and breaks each one into its component parts.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| SCC          | char(10)    | NO   | PRI |         |         |
| fuelTypeID   | smallint(6) | NO   | MUL | 0       |         |
| sourceTypeID | smallint(6) | NO   | MUL | 0       |         |
| roadTypeID   | smallint(6) | NO   | MUL | 0       |         |
| processID    | smallint(6) | NO   | MUL | 0       |         |

### sector

sector defines the economic sectors that nonroad equipment can be assigned to, such as Recreational, Construction, Commercial, Lawn & Garden, Farm, Logging, Airports, Underground Mining, Recreational Marine, Rail Maintenance.

| Field       | Type        | Null | Key | Default | Comment |
| ----------- | ----------- | ---- | --- | ------- | ------- |
| sectorID    | smallint(6) | NO   | PRI |         |         |
| description | char(40)    | YES  |     |         |         |

### sho

sho is a table used during MOVES runtime to represent engine running activity (Source Hours Operating, or SHO) for the keyed columns.

| Field        | Type        | Null | Key  | Default | Comment                                                      |
| ------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| hourDayID    | smallint(6) | NO   | PRI  | 0       |                                                              |
| monthID      | smallint(6) | NO   | PRI  | 0       |                                                              |
| yearID       | smallint(6) | NO   | PRI  | 0       |                                                              |
| ageID        | smallint(6) | NO   | PRI  | 0       |                                                              |
| linkID       | int(11)     | NO   | PRI  | 0       |                                                              |
| sourceTypeID | smallint(6) | NO   | PRI  | 0       |                                                              |
| SHO          | float       | YES  |      |         |                                                              |
| SHOCV        | float       | YES  |      |         | not used                                                     |
| distance     | float       | YES  |      |         | distance traveled during the time in the SHO column, used to calculate average speed |
| isUserInput  | char(1)     | NO   |      | N       |                                                              |

### sizeWeightFraction

sizeWeightFraction is no longer used.

| Field                 | Type        | Null | Key  | Default | Comment |
| --------------------- | ----------- | ---- | ---- | ------- | ------- |
| sourceTypeModelYearID | int(11)     | NO   | PRI  | 0       |         |
| fuelTypeID            | smallint(6) | NO   | PRI  | 0       |         |
| engTechID             | smallint(6) | NO   | PRI  | 0       |         |
| engSizeID             | smallint(6) | NO   | PRI  | 0       |         |
| weightClassID         | smallint(6) | NO   | PRI  | 0       |         |
| sizeWeightFraction    | float       | YES  |      |         |         |

### soakActivityFraction

soakActivityFraction is empty in the default database and is used during runtime to calculate  activity for evaporative emissions.

| Field                  | Type        | Null | Key  | Default | Comment  |
| ---------------------- | ----------- | ---- | ---- | ------- | -------- |
| sourceTypeID           | smallint(6) | NO   | PRI  | 0       |          |
| zoneID                 | int(11)     | NO   | PRI  | 0       |          |
| monthID                | smallint(6) | NO   | PRI  | 0       |          |
| hourDayID              | smallint(6) | NO   | PRI  | 0       |          |
| opModeID               | smallint(6) | NO   | PRI  | 0       |          |
| soakActivityFraction   | float       | YES  |      |         |          |
| soakActivityFractionCV | float       | YES  |      |         | not used |
| isUserInput            | char(1)     | NO   |      | N       |          |

### sourceBin

sourceBin is empty by default, but can be used to define source bins and break them down into their component parts.

| Field            | Type        | Null | Key | Default | Comment |
| ---------------- | ----------- | ---- | --- | ------- | ------- |
| sourceBinID      | bigint(20)  | NO   | PRI | 0       |         |
| fuelTypeID       | smallint(6) | NO   | MUL | 0       |         |
| engTechID        | smallint(6) | NO   |     | 0       |         |
| regClassID       | smallint(6) | NO   |     | 0       |         |
| modelYearGroupID | int(11)     | NO   | MUL | 0       |         |
| engSizeID        | smallint(6) | NO   |     | 0       |         |
| weightClassID    | smallint(6) | NO   |     | 0       |         |

### sourceBinDistribution

sourceBinDistribution is used during MOVES runtime to define the source bins in each source type and model year combination.

| Field                       | Type       | Null | Key | Default | Comment                                                                        |
| --------------------------- | ---------- | ---- | --- | ------- | ------------------------------------------------------------------------------ |
| sourceTypeModelYearID       | int(11)    | NO   | PRI | 0       |                                                                                |
| polProcessID                | int(11)    | NO   | PRI | 0       |                                                                                |
| sourceBinID                 | bigint(20) | NO   | PRI | 0       |                                                                                |
| sourceBinActivityFraction   | float      | YES  |     |         | sums to 1 for each source type, model year, pollutant, and process combination |
| sourceBinActivityFractionCV | float      | YES  |     |         | not used                                                                       |
| isUserInput                 | char(1)    | NO   |     | N       |                                                                                |
| ### sourcehours             |            |      |     |         |                                                                                |

### sourceHours

sourceHours is used during MOVES runtime to calculate the total hours used in calculating evaporative emissions. Unlike like SHO (source hours *operating*), source hours can include all activity, whether the engine is not running or not.

| Field         | Type        | Null | Key | Default | Comment  |
| ------------- | ----------- | ---- | --- | ------- | -------- |
| hourDayID     | smallint(6) | NO   | PRI | 0       |          |
| monthID       | smallint(6) | NO   | PRI | 0       |          |
| yearID        | smallint(6) | NO   | PRI | 0       |          |
| ageID         | smallint(6) | NO   | PRI | 0       |          |
| linkID        | int(11)     | NO   | PRI | 0       |          |
| sourceTypeID  | smallint(6) | NO   | PRI | 0       |          |
| sourceHours   | float       | YES  |     |         |          |
| sourceHoursCV | float       | YES  |     |         | not used |
| isUserInput   | char(1)     | NO   |     | N       |          |

### sourceTypeAge

sourceTypeAge allocates VMT within an HPMS class by vehicle age, using the Relative Mileage Accumulation Rate, which is a mileage accumulation rate of a source type and age combination relative to other source type and age combinations within its HPMS class.

| Field                   | Type        | Null | Key  | Default | Comment                                                      |
| ----------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| ageID                   | smallint(6) | NO   | PRI  | 0       |                                                              |
| sourceTypeID            | smallint(6) | NO   | PRI  | 0       |                                                              |
| survivalRate            | double      | YES  |      |         | not directly used by MOVES. See the Age Distribution appendix in the Population and Activity of Onroad Vehicles technical report for information about how base survival rates are used |
| relativeMAR             | double      | YES  |      |         | the  mileage accumulation of a source type and age combination relative to a  common source type, age combination in the HPMS class |
| functioningACFraction   | double      | YES  |      |         | fraction of vehicles with functional AC systems              |
| functioningACFractionCV | double      | YES  |      |         | not used                                                     |

### sourceTypeAgeDistribution

sourceTypeAgeDistribution defines the fraction of vehicles of a given age within a source type. Unlike sourceTypeAge, the units of this table are in terms of population, not VMT.

| Field        | Type        | Null | Key | Default | Comment                                                        |
| ------------ | ----------- | ---- | --- | ------- | -------------------------------------------------------------- |
| sourceTypeID | smallint(6) | NO   | PRI | 0       |                                                                |
| yearID       | smallint(6) | NO   | PRI | 0       |                                                                |
| ageID        | smallint(6) | NO   | PRI | 0       |                                                                |
| ageFraction  | double      | YES  |     |         | fraction of vehicles of each age for each source type and year |

### sourceTypeDayVMT

sourceTypeDayVMT is used to allocate VMT by the day of the week. It is an optional user-provided table at county scale.

| Field        | Type        | Null | Key | Default | Comment                                    |
| ------------ | ----------- | ---- | --- | ------- | ------------------------------------------ |
| yearID       | smallint(6) | NO   | PRI |         |                                            |
| monthID      | smallint(6) | NO   | PRI |         |                                            |
| dayID        | smallint(6) | NO   | PRI |         |                                            |
| sourceTypeID | smallint(6) | NO   | PRI |         |                                            |
| VMT          | double      | NO   |     |         | total VMT by each source type for each day |

### sourceTypeHour

sourceTypeHour is used to allocate default hotelling activity by hour. Note that this table only holds default data; users may assign their own hotelling hour distribution via the hotellingHourFraction table.

| Field         | Type        | Null | Key  | Default | Comment                                            |
| ------------- | ----------- | ---- | ---- | ------- | -------------------------------------------------- |
| sourceTypeID  | smallint(6) | NO   | PRI  | 0       |                                                    |
| hourDayID     | smallint(6) | NO   | PRI  | 0       |                                                    |
| idleSHOFactor | float       | YES  |      |         | deprecated                                         |
| hotellingdist | double      | YES  |      |         | fraction of daily hotelling activity for each hour |

### sourceTypeModelYear

sourceTypeModelYear defines the source type and model year combinations in MOVES, and the market penetration of AC systems for each source type in that model year.

| Field                   | Type        | Null | Key | Default | Comment                                                                      |
| ----------------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------- |
| sourceTypeModelYearID   | int(11)     | NO   | PRI | 0       |                                                                              |
| modelYearID             | smallint(6) | NO   | MUL | 0       |                                                                              |
| sourceTypeID            | smallint(6) | NO   | MUL | 0       |                                                                              |
| ACPenetrationFraction   | float       | YES  |     |         | fraction of vehicles within the model year with air<br> conditioning systems |
| ACPenetrationFractionCV | float       | YES  |     |         | not used                                                                     |

### sourceTypeModelYearGroup

sourceTypeModelYearGroup forms an association between SourceTypes, ModelYearGroups and TankTemperatureGroups. A combination of a modelYearGroupID (one used for a pollutant-process involving the Permeation process) and a sourceTypeID determines a tankTemperatureGroupID. 

| Field                  | Type        | Null | Key  | Default | Comment |
| ---------------------- | ----------- | ---- | ---- | ------- | ------- |
| sourceTypeID           | smallint(6) | NO   | PRI  | 0       |         |
| modelYearGroupID       | int(11)     | NO   | PRI  | 0       |         |
| tankTemperatureGroupID | smallint(6) | NO   |      | 0       |         |

### sourceTypePolProcess

sourceTypePolProcess describes which vehicle aspects are needed to calculate emissions for each  source type and polProcessID combination.

| Field            | Type        | Null | Key  | Default | Comment                                                |
| ---------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------ |
| sourceTypeID     | smallint(6) | NO   | PRI  | 0       |                                                        |
| polProcessID     | int(11)     | NO   | PRI  | 0       |                                                        |
| isSizeWeightReqd | char(1)     | YES  |      |         | always 'N'                                             |
| isRegClassReqd   | char(1)     | YES  |      |         | Y' if regClassID should be used to find emission rates |
| isMYGroupReqd    | char(1)     | YES  |      |         | Y' if model year should be used to find emission rates |

### sourceTypeTechAdjustment

sourceTypeTechAdjustment is used to adjust refueling spillage emissions (processID 19) to account for ORVR. Note that the `refuelingTechAdjustment` values in this table are different than those in the RefuelingTechAdjustment table because these values have to be weighted to account for the lack of the regClassID dimension in this table.

| Field                   | Type        | Null | Key  | Default | Comment                                                      |
| ----------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| processID               | smallint(6) | NO   | PRI  |         |                                                              |
| sourceTypeID            | smallint(6) | NO   | PRI  |         |                                                              |
| modelYearID             | smallint(6) | NO   | PRI  |         |                                                              |
| refuelingTechAdjustment | float       | NO   |      | 0       | the fraction of vehicles with functional ORVR systems.       |

### sourceTypeYear

sourceTypeYear contains the vehicle population of each source type for a given year. The default database contains national default values, and it is a user input at County Scale.

| Field                | Type        | Null | Key | Default | Comment |
| -------------------- | ----------- | ---- | --- | ------- | ------- |
| yearID               | smallint(6) | NO   | PRI | 0       |         |
| sourceTypeID         | smallint(6) | NO   | PRI | 0       |         |
| salesGrowthFactor    | double      | YES  |     |         |         |
| sourceTypePopulation | double      | YES  |     |         |         |
| migrationrate        | double      | YES  |     |         |         |

### sourceTypeYearVMT

sourceTypeYearVMT contains the VMT for each source type for a given year. It is a user input at County Scale.

| Field        | Type        | Null | Key | Default | Comment                                      |
| ------------ | ----------- | ---- | --- | ------- | -------------------------------------------- |
| yearID       | smallint(6) | NO   | PRI |         |                                              |
| sourceTypeID | smallint(6) | NO   | PRI |         |                                              |
| VMT          | double      | NO   |     |         | Total VMT for each source type within a year |

### sourceUseType

sourceUseType defines the source types used in MOVES, and places them in the appropriate HPMS vehicle type category.

| Field          | Type        | Null | Key | Default | Comment |
| -------------- | ----------- | ---- | --- | ------- | ------- |
| sourceTypeID   | smallint(6) | NO   | PRI | 0       |         |
| HPMSVtypeID    | smallint(6) | NO   | MUL | 0       |         |
| sourceTypeName | char(50)    | YES  |     |         |         |

### sourceUseTypePhysics

sourceUseTypePhysics contains the road load coefficients for every source type that can be used to calculate an operating mode distribution from a drive cycle. One source type can have several sets of road load coefficients, varying by model year and regulatory class.

| Field            | Type        | Null | Key | Default | Comment                                        |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------------------- |
| sourceTypeID     | smallint(6) | NO   | PRI |         |                                                |
| regClassID       | smallint(6) | NO   | PRI |         |                                                |
| beginModelYearID | smallint(6) | NO   | PRI |         |                                                |
| endModelYearID   | smallint(6) | NO   | PRI |         |                                                |
| rollingTermA     | float       | YES  |     |         | kW/(m/s)                                       |
| rotatingTermB    | float       | YES  |     |         | kW/(m/s)^2                                     |
| dragTermC        | float       | YES  |     |         | kW/(m/s)^3                                     |
| sourceMass       | float       | YES  |     |         | metric tons                                    |
| fixedMassFactor  | float       | YES  |     |         | unitless, but can be thought of as metric tons |

### starts

starts is the user input table which defines starts activity at the finest level of detail. MOVES populates this table from other input tables if it is not provided.

| Field        | Type        | Null | Key | Default | Comment  |
| ------------ | ----------- | ---- | --- | ------- | -------- |
| hourDayID    | smallint(6) | NO   | PRI | 0       |          |
| monthID      | smallint(6) | NO   | PRI | 0       |          |
| yearID       | smallint(6) | NO   | PRI | 0       |          |
| ageID        | smallint(6) | NO   | PRI | 0       |          |
| zoneID       | int(11)     | NO   | PRI | 0       |          |
| sourceTypeID | smallint(6) | NO   | PRI | 0       |          |
| starts       | float       | YES  |     |         |          |
| StartsCV     | float       | YES  |     |         | not used |
| isUserInput  | char(1)     | NO   |     | N       |          |

### startsAgeAdjustment

startsAgeAdjustment allocates starts for each source type by age. The values are normalized at runtime so that total starts are conserved after allocation.

| Field         | Type        | Null | Key | Default | Comment                                                                                   |
| ------------- | ----------- | ---- | --- | ------- | ----------------------------------------------------------------------------------------- |
| sourceTypeID  | smallint(6) | NO   | PRI | 0       |                                                                                           |
| ageID         | smallint(6) | NO   | PRI | 0       |                                                                                           |
| ageAdjustment | double      | YES  |     |         | Adjustment applied to the average number of starts to get<br> starts for a particular age |

### startsHourFraction

startsHourFraction allocates starts activity for a day and source type by hour.

| Field              | Type        | Null | Key | Default | Comment                                                                                                  |
| ------------------ | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------------------------------------- |
| dayID              | smallint(6) | NO   | PRI |         |                                                                                                          |
| hourID             | smallint(6) | NO   | PRI |         |                                                                                                          |
| sourceTypeID       | smallint(6) | NO   | PRI | 0       |                                                                                                          |
| allocationFraction | double      | NO   |     |         | fraction of starts in the given hour, which should always sum to 1 for each day, source type combination |

### startsMonthAdjust

startsMonthAdjust adjusts the number of starts per month to better reflect seasonal trends. Unlike `startsAgeAdjustment`, the multiplication factors are not normalized.

| Field           | Type        | Null | Key | Default | Comment                                                                                           |
| --------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------------------------------------- |
| monthID         | smallint(6) | NO   | PRI |         |                                                                                                   |
| sourceTypeID    | smallint(6) | NO   | PRI | 0       |                                                                                                   |
| monthAdjustment | double      | NO   |     |         | raw multiplicative factor to get starts activity in a month<br> based on the average for the year |

### startsOpModeDistribution

startsOpModeDistribution contains the operating mode distribution (specifically, soak time distributions) for starts activity by source type, day, hour, and age. The default database has national defaults, but this table can also be provided as a County Scale input.

| Field          | Type        | Null | Key | Default | Comment                                                              |
| -------------- | ----------- | ---- | --- | ------- | -------------------------------------------------------------------- |
| dayID          | smallint(6) | NO   | PRI | 0       |                                                                      |
| hourID         | smallint(6) | NO   | PRI | 0       |                                                                      |
| sourceTypeID   | smallint(6) | NO   | PRI | 0       |                                                                      |
| ageID          | smallint(6) | NO   | PRI | 0       |                                                                      |
| opModeID       | smallint(6) | NO   | PRI | 0       |                                                                      |
| opModeFraction | double      | YES  |     |         | Should sum to 1 for each day, hour, source type, and age combination |
| isUserInput    | char(1)     | NO   |     | N       |                                                                      |

### startsPerDay

startsPerDay contains total starts, across all vehicles, per day for each source type. This is not used by default, but is a user input table at County Scale.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| dayID        | smallint(6) | NO   | PRI | 0       |         |
| sourceTypeID | smallint(6) | NO   | PRI | 0       |         |
| startsPerDay | double      | YES  |     |         |         |

### startsPerDayPerVehicle

startsPerDayPerVehicle contains starts per day per average vehicle in a source type. This is populated with default national data, but can also be used as a user input table at County Scale.

| Field                  | Type        | Null | Key | Default | Comment |
| ---------------------- | ----------- | ---- | --- | ------- | ------- |
| dayID                  | smallint(6) | NO   | PRI | 0       |         |
| sourceTypeID           | smallint(6) | NO   | PRI | 0       |         |
| startsPerDayPerVehicle | double      | YES  |     |         |         |

### startsPerVehicle

startsPerVehicle contains average starts per vehicle in a source type, for each hour and day. It is not used by default, but can be used as a user input table at County Scale.

| Field              | Type        | Null | Key | Default | Comment  |
| ------------------ | ----------- | ---- | --- | ------- | -------- |
| sourceTypeID       | smallint(6) | NO   | PRI | 0       |          |
| hourDayID          | smallint(6) | NO   | PRI | 0       |          |
| startsPerVehicle   | float       | YES  |     |         |          |
| startsPerVehicleCV | float       | YES  |     |         | not used |

### startsSourceTypeFraction

startsSourceTypeFraction allocates all starts by source type. It is not used by default, but can be used as a user input table at County Scale.

| Field              | Type        | Null | Key | Default | Comment |
| ------------------ | ----------- | ---- | --- | ------- | ------- |
| sourceTypeID       | smallint(6) | NO   | PRI |         |         |
| allocationFraction | double      | NO   |     |         |         |

### startTempAdjustment

startTempAdjustment adjusts start emission rates based on the ambient air temperature. Adjustments may be additive or multiplicative.

| Field                 | Type        | Null | Key  | Default | Comment                                                      |
| --------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelTypeID            | smallint(6) | NO   | PRI  | 0       |                                                              |
| polProcessID          | int(11)     | NO   | PRI  | 0       |                                                              |
| modelYearGroupID      | int(11)     | NO   | PRI  | 0       |                                                              |
| opModeID              | smallint(6) | NO   | PRI  | 0       |                                                              |
| tempAdjustTermA       | float       | YES  |      |         |                                                              |
| tempAdjustTermACV     | float       | YES  |      |         | not used                                                     |
| tempAdjustTermB       | float       | YES  |      |         |                                                              |
| tempAdjustTermBCV     | float       | YES  |      |         | not used                                                     |
| tempAdjustTermC       | float       | YES  |      |         |                                                              |
| tempAdjustTermCCV     | float       | YES  |      |         | not used                                                     |
| startTempEquationType | varchar(4)  | YES  |      |         | defines whether the coefficients are used in the polynomial  function `Grams = A*(T-75) + B*(T-75)^2` or log-linear function `Grams =  B*e^(A*(T-75)) + C` |

### state

state defines the states used in MOVES, along with some metadata. The stateID is based on each state's FIPS code.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| stateID      | smallint(6) | NO   | PRI | 0       |         |
| stateName    | char(25)    | YES  |     |         |         |
| stateAbbr    | char(2)     | YES  |     |         |         |
| idleRegionID | int(11)     | YES  |     |         |         |

### sulfateEmissionRate

sulfateEmissionRate contains the emission rates for sulfur dioxide (pollutantID 31) based on the process, fuel type, and model year.

| Field            | Type        | Null | Key  | Default | Comment  |
| ---------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID     | int(11)     | NO   | PRI  | 0       |          |
| fuelTypeID       | smallint(6) | NO   | PRI  | 0       |          |
| modelYearGroupID | int(11)     | NO   | PRI  | 0       |          |
| meanBaseRate     | float       | YES  |      |         |          |
| meanBaseRateCV   | float       | YES  |      |         | not used |
| dataSourceID     | smallint(6) | YES  |      |         |          |

### sulfateFractions

sulfateFractions contains the parameters used in the sulfate calculator. The calculator, and its parameters, are documented in the MOVES Fuel Effects Technical Report.

| Field                   | Type        | Null | Key  | Default | Comment |
| ----------------------- | ----------- | ---- | ---- | ------- | ------- |
| processID               | smallint(6) | NO   | PRI  |         |         |
| fuelTypeID              | smallint(6) | NO   | PRI  |         |         |
| sourceTypeID            | smallint(6) | NO   | PRI  |         |         |
| minModelYearID          | smallint(6) | NO   | PRI  |         |         |
| maxModelYearID          | smallint(6) | NO   | PRI  |         |         |
| SulfatenonECPMFraction  | double      | NO   |      |         |         |
| H2OnonECPMFraction      | double      | NO   |      |         |         |
| BaseFuelSulfurLevel     | double      | NO   |      |         |         |
| BaseFuelSulfateFraction | double      | NO   |      |         |         |
| DataSourceId            | smallint(6) | NO   | 0    |         |         |

### sulfurBase

sulfurBase  contains the baseline sulfur concentration in MOVES base fuels.

| Field            | Type    | Null | Key  | Default | Comment                                                    |
| ---------------- | ------- | ---- | ---- | ------- | ---------------------------------------------------------- |
| modelYearGroupID | int(11) | NO   | PRI  | 0       |                                                            |
| sulfurBase       | float   | YES  |      |         | sulfur level assumed by model year                         |
| sulfurBasis      | float   | YES  |      | 30      | ppm concentration of sulfur used for calculating emissions |
| sulfurGPAMax     | float   | YES  |      | 330     |                                                            |

### sulfurCapAmount

sulfurCapAmount contains the highest concentration of sulfur for gasoline.

| Field      | Type        | Null | Key  | Default | Comment     |
| ---------- | ----------- | ---- | ---- | ------- | ----------- |
| fuelTypeID | smallint(6) | NO   | PRI  |         |             |
| sulfurCap  | double      | YES  |      |         | unit is PPM |

### sulfurModelCoeff

sulfurModelCoeff contains the coefficients used as part of the M6 sulfur fuel effects model  algorithm. The M6Sulf model is documented in the MOVES Fuel Effects Technical Report.

| Field            | Type        | Null | Key  | Default | Comment |
| ---------------- | ----------- | ---- | ---- | ------- | ------- |
| processID        | smallint(6) | NO   | PRI  |         |         |
| pollutantID      | smallint(6) | NO   | PRI  |         |         |
| M6emitterID      | smallint(6) | NO   | PRI  |         |         |
| sourceTypeID     | smallint(6) | NO   | PRI  |         |         |
| fuelMYGroupID    | int(8)      | NO   | PRI  |         |         |
| sulfurFunctionID | smallint(6) | NO   | PRI  |         |         |
| sulfurCoeff      | float       | YES  |      |         |         |
| lowSulfurCoeff   | double      | YES  |      |         |         |

### sulfurModelName

sulfurModelName decodes the M6EmitterIDs and sulfurFunctionIDs in the sulfurModelCoeff table.

| Field              | Type        | Null | Key  | Default | Comment |
| ------------------ | ----------- | ---- | ---- | ------- | ------- |
| M6EmitterID        | smallint(6) | NO   | PRI  | 0       |         |
| sulfurFunctionID   | smallint(6) | NO   | PRI  | 0       |         |
| M6emitterName      | char(10)    | YES  |      |         |         |
| sulfurFunctionName | char(10)    | YES  |      |         |         |

### tankTemperatureGroup

tankTemperatureGroup categorizes fuel tank technologies in terms of how the temperature in the fuel tank increases during vehicle operation.

| Field                    | Type        | Null | Key | Default | Comment |
| ------------------------ | ----------- | ---- | --- | ------- | ------- |
| tankTemperatureGroupID   | smallint(6) | NO   | PRI | 0       |         |
| tankTemperatureGroupName | char(50)    | NO   |     |         |         |

### tankTemperatureRise

tankTemperatureRise  contains the coefficients for each tankTemperatureGroup that are used to  calculate the tank temperature at the next key-on time based on the last  key-off time.

| Field                      | Type        | Null | Key  | Default | Comment  |
| -------------------------- | ----------- | ---- | ---- | ------- | -------- |
| tankTemperatureGroupID     | smallint(6) | NO   | PRI  | 0       |          |
| tankTemperatureRiseTermA   | float       | YES  |      |         |          |
| tankTemperatureRiseTermACV | float       | YES  |      |         | not used |
| tankTemperatureRiseTermB   | float       | YES  |      |         |          |
| tankTemperatureRiseTermBCV | float       | YES  |      |         | not used |

### tankVaporGenCoeffs

tankVaporGenCoeffs contains the coefficients used in the Tank Vapor Generator to estimate evaporative emissions. There is a set of coefficients for each ethanol level and altitude used in MOVES.

| Field          | Type        | Null | Key  | Default | Comment                                           |
| -------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------- |
| ethanolLevelID | smallint(6) | NO   | PRI  | 0       | synonym for ethanolLevel, where 10 is 10% ethanol |
| altitude       | char(1)     | NO   | PRI  |         |                                                   |
| tvgTermA       | float       | YES  |      |         |                                                   |
| tvgTermB       | float       | YES  |      |         |                                                   |
| tvgTermC       | float       | YES  |      |         |                                                   |

### temperatureAdjustment

temperatureAdjustment contains the temperature adjustment applied to permeation and start energy consumption  rates. These adjustments are multiplicative relative to the base rate. The  adjustments are based on the polynomial equation outlined in the startTempAdjustment table, and added to 1 to convert from an additive  adjustment to a multiplicative one.

| Field             | Type        | Null | Key  | Default | Comment  |
| ----------------- | ----------- | ---- | ---- | ------- | -------- |
| polProcessID      | int(11)     | NO   | PRI  | 0       |          |
| fuelTypeID        | smallint(6) | NO   | PRI  | 0       |          |
| tempAdjustTermA   | float       | YES  |      |         |          |
| tempAdjustTermACV | float       | YES  |      |         | not used |
| tempAdjustTermB   | float       | YES  |      |         |          |
| tempAdjustTermBCV | float       | YES  |      |         | not used |
| tempAdjustTermC   | float       | YES  |      |         |          |
| tempAdjustTermCCV | float       | YES  |      |         | not used |
| minModelYearID    | smallint(6) | NO   | PRI  | 1960    |          |
| maxModelYearID    | smallint(6) | NO   | PRI  | 2050    |          |

### temperatureProfileID

temperatureProfileID maps temperature profiles to their zones and month. These temperature profiles are used in the ratePerProfile table in the MOVES output database. It is empty by default.

| Field                | Type        | Null | Key | Default | Comment |
| -------------------- | ----------- | ---- | --- | ------- | ------- |
| temperatureProfileID | bigint(20)  | NO   | PRI |         |         |
| zoneID               | int(11)     | NO   | MUL |         |         |
| monthID              | smallint(6) | NO   | MUL |         |         |

### togSpeciationProfileName

togSpeciationProfileName describes some mobile-source speciation profiles.

| Field                    | Type         | Null | Key | Default | Comment |
| ------------------------ | ------------ | ---- | --- | ------- | ------- |
| togSpeciationProfileID   | varchar(10)  | NO   | PRI | 0       |         |
| TOGSpeciationProfileName | varchar(100) | YES  | MUL |         |         |
| dataSourceId             | smallint(6)  | YES  |     |         |         |

### totalIdleFraction

totalIdleFraction contains the fraction of total engine-on activity that is represented as off-network idle.

| Field             | Type    | Null | Key  | Default | Comment                                                  |
| ----------------- | ------- | ---- | ---- | ------- | -------------------------------------------------------- |
| sourceTypeID      | int(11) | NO   | PRI  |         |                                                          |
| minModelYearID    | int(11) | NO   | PRI  |         |                                                          |
| maxModelYearID    | int(11) | NO   | PRI  |         |                                                          |
| monthID           | int(11) | NO   | PRI  |         |                                                          |
| dayID             | int(11) | NO   | PRI  |         |                                                          |
| idleRegionID      | int(11) | NO   | PRI  |         |                                                          |
| countyTypeID      | int(11) | NO   | PRI  |         |                                                          |
| totalIdleFraction | double  | YES  |      |         | fraction of total SHO which is off-network idle activity |

### weightClass

weightClass defines the weight classes that are used in MOVES, mapping IDs to both weight ranges and mean weights.

| Field           | Type        | Null | Key  | Default | Comment                             |
| --------------- | ----------- | ---- | ---- | ------- | ----------------------------------- |
| weightClassID   | smallint(6) | NO   | PRI  | 0       |                                     |
| weightClassName | char(50)    | YES  |      |         | defines the full weight class range |
| midpointWeight  | float       | YES  |      |         |                                     |

### year

year defines the calendar years used in MOVES.

| Field      | Type        | Null | Key | Default | Comment |
| ---------- | ----------- | ---- | --- | ------- | ------- |
| yearID     | smallint(6) | NO   | PRI | 0       |         |
| isBaseYear | char(1)     | YES  | MUL |         |         |
| fuelYearID | int(11)     | NO   | 0   |         |         |

### zone

zone defines the default zones used in MOVES, which are the same as counties. It also contains data used to allocate activity by county.

| Field            | Type    | Null | Key  | Default | Comment                                                      |
| ---------------- | ------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| zoneID           | int(11) | NO   | PRI  | 0       |                                                              |
| countyID         | int(11) | NO   | MUL  | 0       |                                                              |
| startAllocFactor | double  | YES  |      |         | used to allocate national start activity                     |
| idleAllocFactor  | double  | YES  |      |         | deprecated                                                   |
| SHPAllocFactor   | double  | YES  |      |         | used to allocate allocate activity used in evaporative emissions |

### zoneMonthHour

zoneMonthHour defines the meteorological conditions used in MOVES for every county/zone.

| Field            | Type        | Null | Key | Default | Comment                                                                   |
|------------------|-------------|------|-----|---------|---------------------------------------------------------------------------|
| monthID          | smallint(6) | NO   | PRI | 0       |                                                                           |
| zoneID           | int(11)     | NO   | PRI | 0       |                                                                           |
| hourID           | smallint(6) | NO   | PRI | 0       |                                                                           |
| temperature      | double      | YES  |     |         | average temperature for the month and hour in Farenheit                   |
| relHumidity      | double      | YES  |     |         |                                                                           |
| heatIndex        | double      | YES  |     |         | used during MOVES runtime only, in Farenheit                              |
| specificHumidity | double      | YES  |     |         | used during MOVES rumtime only, in grams of water per kilogram of dry air |
| molWaterFraction | double      | YES  |     |         | used during MOVES runtime only, in moles of water per mole of ambient air |

### zoneRoadType

zoneRoadType is used to allocate national onroad activity (SHO) on each road type to each county/zone.

| Field          | Type        | Null | Key  | Default | Comment                                                      |
| -------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| zoneID         | int(11)     | NO   | PRI  | 0       |                                                              |
| roadTypeID     | smallint(6) | NO   | PRI  | 0       |                                                              |
| SHOAllocFactor | double      | YES  |      |         | fraction of national SHO for a given road type which takes place in each county |

## MOVES Output Database

### baseRateOutput

baseRateOutput contains MOVES emission rates that are used during runtime to calculate emissions. By default, it is not populated, but it can be populated for debugging purposes.

| Field         | Type                 | Null | Key  | Default | Comment                                             |
| ------------- | -------------------- | ---- | ---- | ------- | --------------------------------------------------- |
| MOVESRunID    | smallint(5) unsigned | NO   |      |         |                                                     |
| iterationID   | smallint(5) unsigned | YES  | 1    |         |                                                     |
| zoneID        | int(11)              | NO   | 0    |         |                                                     |
| linkID        | int(11)              | NO   | 0    |         |                                                     |
| sourceTypeID  | smallint(6)          | NO   | 0    |         |                                                     |
| SCC           | char(10)             | NO   |      |         |                                                     |
| roadTypeID    | smallint(6)          | NO   | 0    |         |                                                     |
| avgSpeedBinID | smallint(6)          | NO   | 0    |         |                                                     |
| monthID       | smallint(6)          | NO   | 0    |         |                                                     |
| hourDayID     | smallint(6)          | NO   | 0    |         |                                                     |
| pollutantID   | smallint(5) unsigned | YES  |      |         |                                                     |
| processID     | smallint(5) unsigned | YES  |      |         |                                                     |
| modelYearID   | smallint(6)          | NO   | 0    |         |                                                     |
| yearID        | smallint(6)          | NO   |      |         |                                                     |
| fuelTypeID    | smallint(6)          | NO   | 0    |         |                                                     |
| regClassID    | smallint(6)          | NO   | 0    |         |                                                     |
| meanBaseRate  | float                | YES  |      |         | rate used to calculate emissions before adjustments |
| emissionRate  | float                | YES  |      |         | rate used to calculate emissions after adjustments  |

### baseRateUnits

baseRateUnits defines the units used in the output database for each moves run.

| Field                            | Type                 | Null | Key  | Default | Comment |
| -------------------------------- | -------------------- | ---- | ---- | ------- | ------- |
| MOVESRunID                       | smallint(5) unsigned | NO   |      |         |         |
| pollutantID                      | smallint(5) unsigned | YES  |      |         |         |
| processID                        | smallint(5) unsigned | YES  |      |         |         |
| meanBaseRateUnitsNumerator       | varchar(50)          | YES  |      |         |         |
| meanBaseRateUnitsDenominator     | varchar(50)          | YES  |      |         |         |
| emissionBaseRateUnitsNumerator   | varchar(50)          | YES  |      |         |         |
| emissionBaseRateUnitsDenominator | varchar(50)          | YES  |      |         |         |

### bundleTracking

bundleTracking is a diagnostic table, used to track the Java and SQL portions of MOVES bundles, including their contents and runtime. Note that this table does not capture all of MOVES processing.

| Field                | Type                 | Null | Key | Default | Comment |
| -------------------- | -------------------- | ---- | --- | ------- | ------- |
| MOVESRunID           | smallint(5) unsigned | NO   | MUL |         |         |
| hostType             | char(1)              | NO   |     |         |         |
| loopableClassName    | varchar(200)         | NO   |     |         |         |
| workerVersion        | varchar(100)         | NO   |     |         |         |
| workerComputerID     | varchar(255)         | NO   |     |         |         |
| workerID             | varchar(255)         | NO   |     |         |         |
| bundleNumber         | int(11)              | NO   | 0   |         |         |
| isCleanUp            | char(1)              | NO   | N   |         |         |
| iterationID          | smallint(5) unsigned | YES  |     |         |         |
| processID            | smallint(5) unsigned | YES  |     |         |         |
| roadTypeID           | smallint(5) unsigned | YES  |     |         |         |
| linkID               | int(10) unsigned     | YES  |     |         |         |
| zoneID               | int(10) unsigned     | YES  |     |         |         |
| countyID             | int(10) unsigned     | YES  |     |         |         |
| stateID              | smallint(5) unsigned | YES  |     |         |         |
| yearID               | smallint(5) unsigned | YES  |     |         |         |
| monthID              | smallint(5) unsigned | YES  |     |         |         |
| dayID                | smallint(5) unsigned | YES  |     |         |         |
| hourID               | smallint(5) unsigned | YES  |     |         |         |
| executionGranularity | varchar(10)          | YES  |     |         |         |
| executionPriority    | smallint(5) unsigned | YES  |     |         |         |
| durationSeconds      | float                | YES  |     |         |         |

### movesActivityOutput

movesActivityOutput contains the activity output from a MOVES runs, such as VMT, at the level of granularity defined in the runspec. It will only be populated for each activity type specified in the "General Output" panel of the GUI.

| Field          | Type                 | Null | Key  | Default | Comment                                                      |
| -------------- | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| MOVESRunID     | smallint(5) unsigned | NO   |      |         |                                                              |
| iterationID    | smallint(5) unsigned | YES  | 1    |         |                                                              |
| yearID         | smallint(5) unsigned | YES  |      |         |                                                              |
| monthID        | smallint(5) unsigned | YES  |      |         |                                                              |
| dayID          | smallint(5) unsigned | YES  |      |         |                                                              |
| hourID         | smallint(5) unsigned | YES  |      |         |                                                              |
| stateID        | smallint(5) unsigned | YES  |      |         |                                                              |
| countyID       | int(10) unsigned     | YES  |      |         |                                                              |
| zoneID         | int(10) unsigned     | YES  |      |         |                                                              |
| linkID         | int(10) unsigned     | YES  |      |         |                                                              |
| sourceTypeID   | smallint(5) unsigned | YES  |      |         |                                                              |
| regClassID     | smallint(5) unsigned | YES  |      |         |                                                              |
| fuelTypeID     | smallint(5) unsigned | YES  |      |         |                                                              |
| fuelSubTypeID  | smallint(5) unsigned | YES  |      |         |                                                              |
| modelYearID    | smallint(5) unsigned | YES  |      |         |                                                              |
| roadTypeID     | smallint(5) unsigned | YES  |      |         |                                                              |
| SCC            | char(10)             | YES  |      |         |                                                              |
| engTechID      | smallint(5) unsigned | YES  |      |         |                                                              |
| sectorID       | smallint(5) unsigned | YES  |      |         |                                                              |
| hpID           | smallint(5) unsigned | YES  |      |         |                                                              |
| activityTypeID | smallint(6)          | NO   |      |         |                                                              |
| activity       | float                | YES  |      |         | units depend on the activity type, but are generally easy to infer from the activity type |
| activityMean   | float                | YES  |      |         | not used                                                     |
| activitySigma  | float                | YES  |      |         | not used                                                     |

### movesError

movesError contains the error messages that are returned during a MOVES run, and also contains some context about what may have caused the error.

| Field        | Type                 | Null | Key  | Default | Comment                                                 |
| ------------ | -------------------- | ---- | ---- | ------- | ------------------------------------------------------- |
| MOVESErrorID | int(10) unsigned     | NO   | PRI  |         | an auto-incremented field to assign an ID to each error |
| MOVESRunID   | smallint(5) unsigned | NO   | MUL  |         |                                                         |
| yearID       | smallint(5) unsigned | YES  |      |         |                                                         |
| monthID      | smallint(5) unsigned | YES  |      |         |                                                         |
| dayID        | smallint(5) unsigned | YES  |      |         |                                                         |
| hourID       | smallint(5) unsigned | YES  |      |         |                                                         |
| stateID      | smallint(5) unsigned | YES  |      |         |                                                         |
| countyID     | int(10) unsigned     | YES  |      |         |                                                         |
| zoneID       | int(10) unsigned     | YES  |      |         |                                                         |
| linkID       | int(10) unsigned     | YES  |      |         |                                                         |
| pollutantID  | smallint(5) unsigned | YES  |      |         |                                                         |
| processID    | smallint(5) unsigned | YES  |      |         |                                                         |
| errorMessage | varchar(255)         | NO   |      |         |                                                         |

### movesEventLog

movesEventLog  is deprecated and no longer used.

| Field         | Type                 | Null | Key  | Default | Comment |
| ------------- | -------------------- | ---- | ---- | ------- | ------- |
| EventRecordID | int(10) unsigned     | NO   | PRI  |         |         |
| MOVESRunID    | smallint(5) unsigned | NO   | PRI  |         |         |
| EventName     | char(255)            | NO   |      |         |         |
| WhenStarted   | int(10) unsigned     | NO   |      |         |         |
| WhenStopped   | int(10) unsigned     | YES  |      |         |         |
| Duration      | int(10) unsigned     | YES  |      |         |         |

### movesOutput

movesOutput is the workhorse table of a MOVES output database, containing the emissions inventory of a run. 

| Field              | Type                 | Null | Key  | Default | Comment                                                      |
| ------------------ | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| MOVESRunID         | smallint(5) unsigned | NO   |      |         |                                                              |
| iterationID        | smallint(5) unsigned | YES  | 1    |         |                                                              |
| yearID             | smallint(5) unsigned | YES  |      |         |                                                              |
| monthID            | smallint(5) unsigned | YES  |      |         |                                                              |
| dayID              | smallint(5) unsigned | YES  |      |         |                                                              |
| hourID             | smallint(5) unsigned | YES  |      |         |                                                              |
| stateID            | smallint(5) unsigned | YES  |      |         |                                                              |
| countyID           | int(10) unsigned     | YES  |      |         |                                                              |
| zoneID             | int(10) unsigned     | YES  |      |         |                                                              |
| linkID             | int(10) unsigned     | YES  |      |         |                                                              |
| pollutantID        | smallint(5) unsigned | YES  |      |         |                                                              |
| processID          | smallint(5) unsigned | YES  |      |         |                                                              |
| sourceTypeID       | smallint(5) unsigned | YES  |      |         |                                                              |
| regClassID         | smallint(5) unsigned | YES  |      |         |                                                              |
| fuelTypeID         | smallint(5) unsigned | YES  |      |         |                                                              |
| fuelSubTypeID      | smallint(5) unsigned | YES  |      |         |                                                              |
| modelYearID        | smallint(5) unsigned | YES  |      |         |                                                              |
| roadTypeID         | smallint(5) unsigned | YES  |      |         |                                                              |
| SCC                | char(10)             | YES  |      |         |                                                              |
| engTechID          | smallint(5) unsigned | YES  |      |         |                                                              |
| sectorID           | smallint(5) unsigned | YES  |      |         |                                                              |
| hpID               | smallint(5) unsigned | YES  |      |         |                                                              |
| emissionQuant      | float                | YES  |      |         | The units will match the units specified in the runspec, and will depend on the pollutant |
| emissionQuantMean  | float                | YES  |      |         | not used                                                     |
| emissionQuantSigma | float                | YES  |      |         | not used                                                     |

### movesRun

movesRun defines all the MOVES runs which write to the output database, with some metadata for each run.

| Field                | Type                 | Null | Key      | Default | Comment                                                      |
| -------------------- | -------------------- | ---- | -------- | ------- | ------------------------------------------------------------ |
| MOVESRunID           | smallint(5) unsigned | NO   | PRI      |         | an auto-incremented ID used to identify the output of specific moves runs in other tables |
| outputTimePeriod     | char(5)              | YES  |          |         |                                                              |
| timeUnits            | char(5)              | YES  |          |         |                                                              |
| distanceUnits        | char(5)              | YES  |          |         |                                                              |
| massUnits            | char(5)              | YES  |          |         |                                                              |
| energyUnits          | char(5)              | YES  |          |         |                                                              |
| runSpecFileName      | varchar(500)         | YES  |          |         |                                                              |
| runSpecDescription   | text                 | YES  |          |         |                                                              |
| runSpecFileDateTime  | datetime             | YES  |          |         |                                                              |
| runDateTime          | datetime             | YES  |          |         |                                                              |
| scale                | char(5)              | YES  |          |         |                                                              |
| minutesDuration      | float                | YES  |          |         |                                                              |
| defaultDatabaseUsed  | varchar(200)         | YES  |          |         |                                                              |
| masterVersion        | varchar(100)         | YES  |          |         |                                                              |
| masterComputerID     | varchar(255)         | YES  |          |         |                                                              |
| masterIDNumber       | varchar(255)         | YES  |          |         |                                                              |
| domain               | char(10)             | YES  | DEFAULT |         |                                                              |
| domainCountyID       | int(10) unsigned     | YES  |          |         |                                                              |
| domainCountyName     | varchar(50)          | YES  |          |         |                                                              |
| domainDatabaseServer | varchar(100)         | YES  |          |         |                                                              |
| domainDatabaseName   | varchar(200)         | YES  |          |         |                                                              |
| expectedDONEFiles    | int(10) unsigned     | YES  |          |         |                                                              |
| retrievedDONEFiles   | int(10) unsigned     | YES  |          |         |                                                              |
| models               | varchar(40)          | NO   | onroad   |         |                                                              |

### movesTablesUsed

movesTablesUsed is a diagnostic table that contains information on which tables from which database used during MOVES runs (or when MOVES tools are used).

| Field                    | Type                 | Null | Key  | Default | Comment        |
| ------------------------ | -------------------- | ---- | ---- | ------- | -------------- |
| MOVESRunID               | smallint(5) unsigned | NO   | PRI  |         |                |
| databaseServer           | varchar(100)         | NO   | PRI  |         |                |
| databaseName             | varchar(200)         | NO   | PRI  |         |                |
| tableName                | varchar(200)         | NO   | PRI  |         |                |
| dataFileSize             | int(10) unsigned     | YES  |      |         |                |
| dataFileModificationDate | datetime             | YES  |      |         |                |
| tableUseSequence         | int(10) unsigned     | NO   |      |         | auto_increment |

### movesWorkersUsed

movesWorkersUsed is a diagnostic table that contains metadata on each MOVES worker used during a MOVES and tracks the number of bundles processed by each worker.

| Field             | Type                 | Null | Key  | Default | Comment |
| ----------------- | -------------------- | ---- | ---- | ------- | ------- |
| MOVESRunID        | smallint(5) unsigned | NO   | PRI  |         |         |
| workerVersion     | varchar(100)         | NO   | PRI  |         |         |
| workerComputerID  | varchar(255)         | NO   | PRI  |         |         |
| workerID          | varchar(255)         | NO   | PRI  |         |         |
| bundleCount       | int(10) unsigned     | NO   |      | 0       |         |
| failedBundleCount | int(10) unsigned     | NO   |      | 0       |         |

### ratePerDistance

ratePerDistance contains data for MOVES rates run. It is populated with emission factors for relevant processes by distance.

| Field           | Type                 | Null | Key  | Default | Comment                                                      |
| --------------- | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| MOVESScenarioID | varchar(40)          | NO   |      |         |                                                              |
| MOVESRunID      | smallint(5) unsigned | NO   |      |         |                                                              |
| yearID          | smallint(5) unsigned | YES  |      |         |                                                              |
| monthID         | smallint(5) unsigned | YES  |      |         |                                                              |
| dayID           | smallint(5) unsigned | YES  |      |         |                                                              |
| hourID          | smallint(5) unsigned | YES  |      |         |                                                              |
| linkID          | int(10) unsigned     | YES  |      |         |                                                              |
| pollutantID     | smallint(5) unsigned | YES  |      |         |                                                              |
| processID       | smallint(5) unsigned | YES  |      |         |                                                              |
| sourceTypeID    | smallint(5) unsigned | YES  |      |         |                                                              |
| regClassID      | smallint(5) unsigned | YES  |      |         |                                                              |
| SCC             | char(10)             | YES  |      |         |                                                              |
| fuelTypeID      | smallint(5) unsigned | YES  |      |         |                                                              |
| modelYearID     | smallint(5) unsigned | YES  |      |         |                                                              |
| roadTypeID      | smallint(5) unsigned | YES  |      |         |                                                              |
| avgSpeedBinID   | smallint(6)          | YES  |      |         |                                                              |
| temperature     | float                | YES  |      |         |                                                              |
| relHumidity     | float                | YES  |      |         |                                                              |
| ratePerDistance | float                | YES  |      |         | the unit of this column depends on the units selected in the runspec |

### ratePerHour

ratePerHour contains data for MOVES rates run. It is populated with emission factors for relevant processes by hour. 

| Field           | Type                 | Null | Key  | Default | Comment                                                      |
| --------------- | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| MOVESScenarioID | varchar(40)          | NO   |      |         |                                                              |
| MOVESRunID      | smallint(5) unsigned | NO   |      |         |                                                              |
| yearID          | smallint(5) unsigned | YES  |      |         |                                                              |
| monthID         | smallint(5) unsigned | YES  |      |         |                                                              |
| dayID           | smallint(5) unsigned | YES  |      |         |                                                              |
| hourID          | smallint(5) unsigned | YES  |      |         |                                                              |
| linkID          | int(10) unsigned     | YES  |      |         |                                                              |
| pollutantID     | smallint(5) unsigned | YES  |      |         |                                                              |
| processID       | smallint(5) unsigned | YES  |      |         |                                                              |
| sourceTypeID    | smallint(5) unsigned | YES  |      |         |                                                              |
| regClassID      | smallint(5) unsigned | YES  |      |         |                                                              |
| SCC             | char(10)             | YES  |      |         |                                                              |
| fuelTypeID      | smallint(5) unsigned | YES  |      |         |                                                              |
| modelYearID     | smallint(5) unsigned | YES  |      |         |                                                              |
| roadTypeID      | smallint(5) unsigned | YES  |      |         |                                                              |
| temperature     | float                | YES  |      |         |                                                              |
| relHumidity     | float                | YES  |      |         |                                                              |
| ratePerHour     | float                | YES  |      |         | the numerator of the unit will depend on the the units selected in the runspec, but the denominator will always be one hour |

### ratePerProfile

ratePerProfile contains data for MOVES rates run. It is populated with emission factors by temperature profile ID, used for evaporative emissions.

| Field                | Type                 | Null | Key  | Default | Comment                                                      |
| -------------------- | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| MOVESScenarioID      | varchar(40)          | NO   |      |         |                                                              |
| MOVESRunID           | smallint(5) unsigned | NO   |      |         |                                                              |
| temperatureProfileID | bigint(20)           | YES  |      |         |                                                              |
| yearID               | smallint(5) unsigned | YES  |      |         |                                                              |
| dayID                | smallint(5) unsigned | YES  |      |         |                                                              |
| hourID               | smallint(5) unsigned | YES  |      |         |                                                              |
| pollutantID          | smallint(5) unsigned | YES  |      |         |                                                              |
| processID            | smallint(5) unsigned | YES  |      |         |                                                              |
| sourceTypeID         | smallint(5) unsigned | YES  |      |         |                                                              |
| regClassID           | smallint(5) unsigned | YES  |      |         |                                                              |
| SCC                  | char(10)             | YES  |      |         |                                                              |
| fuelTypeID           | smallint(5) unsigned | YES  |      |         |                                                              |
| modelYearID          | smallint(5) unsigned | YES  |      |         |                                                              |
| temperature          | float                | YES  |      |         |                                                              |
| relHumidity          | float                | YES  |      |         |                                                              |
| ratePerVehicle       | float                | YES  |      |         | the numerator of the unit will depend on the the units selected in the runspec, but the denominator will always be one vehicle |

### ratePerStart

ratePerStart contains data for MOVES rates run. It is populated with emission factors for relevant processes by start. 

| Field           | Type                 | Null | Key  | Default | Comment                                                      |
| --------------- | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| MOVESScenarioID | varchar(40)          | NO   |      |         |                                                              |
| MOVESRunID      | smallint(5) unsigned | NO   |      |         |                                                              |
| yearID          | smallint(5) unsigned | YES  |      |         |                                                              |
| monthID         | smallint(5) unsigned | YES  |      |         |                                                              |
| dayID           | smallint(5) unsigned | YES  |      |         |                                                              |
| hourID          | smallint(5) unsigned | YES  |      |         |                                                              |
| zoneID          | int(10) unsigned     | YES  |      |         |                                                              |
| sourceTypeID    | smallint(5) unsigned | YES  |      |         |                                                              |
| regClassID      | smallint(5) unsigned | YES  |      |         |                                                              |
| SCC             | char(10)             | YES  |      |         |                                                              |
| fuelTypeID      | smallint(5) unsigned | YES  |      |         |                                                              |
| modelYearID     | smallint(5) unsigned | YES  |      |         |                                                              |
| pollutantID     | smallint(5) unsigned | YES  |      |         |                                                              |
| processID       | smallint(5) unsigned | YES  |      |         |                                                              |
| temperature     | float                | YES  |      |         |                                                              |
| relHumidity     | float                | YES  |      |         |                                                              |
| ratePerStart    | float                | YES  |      |         | the numerator of the unit will depend on the the units selected in the runspec, but the denominator will always be one start |

### ratePerVehicle

ratePerVehicle contains data for MOVES rates run. It is populated with emission factors for relevant processes by population. 

| Field           | Type                 | Null | Key  | Default | Comment                                                      |
| --------------- | -------------------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| MOVESScenarioID | varchar(40)          | NO   |      |         |                                                              |
| MOVESRunID      | smallint(5) unsigned | NO   |      |         |                                                              |
| yearID          | smallint(5) unsigned | YES  |      |         |                                                              |
| monthID         | smallint(5) unsigned | YES  |      |         |                                                              |
| dayID           | smallint(5) unsigned | YES  |      |         |                                                              |
| hourID          | smallint(5) unsigned | YES  |      |         |                                                              |
| zoneID          | int(10) unsigned     | YES  |      |         |                                                              |
| pollutantID     | smallint(5) unsigned | YES  |      |         |                                                              |
| processID       | smallint(5) unsigned | YES  |      |         |                                                              |
| sourceTypeID    | smallint(5) unsigned | YES  |      |         |                                                              |
| regClassID      | smallint(5) unsigned | YES  |      |         |                                                              |
| SCC             | char(10)             | YES  |      |         |                                                              |
| fuelTypeID      | smallint(5) unsigned | YES  |      |         |                                                              |
| modelYearID     | smallint(5) unsigned | YES  |      |         |                                                              |
| temperature     | float                | YES  |      |         |                                                              |
| relHumidity     | float                | YES  |      |         |                                                              |
| ratePerVehicle  | float                | YES  |      |         | the numerator of the unit will depend on the the units selected in the runspec, but the denominator will always be one vehicle |

### startsPerVehicle

startsPerVehicle contains data for a MOVES rates run, defining the number of starts per vehicle, which can then be paired with the ratePerStart table to produce inventories.

| Field            | Type                 | Null | Key | Default | Comment |
| ---------------- | -------------------- | ---- | --- | ------- | ------- |
| MOVESScenarioID  | varchar(40)          | NO   |     |         |         |
| MOVESRunID       | smallint(5) unsigned | NO   |     |         |         |
| yearID           | smallint(5) unsigned | YES  |     |         |         |
| monthID          | smallint(5) unsigned | YES  |     |         |         |
| dayID            | smallint(5) unsigned | YES  |     |         |         |
| hourID           | smallint(5) unsigned | YES  |     |         |         |
| zoneID           | int(10) unsigned     | YES  |     |         |         |
| sourceTypeID     | smallint(5) unsigned | YES  |     |         |         |
| regClassID       | smallint(5) unsigned | YES  |     |         |         |
| SCC              | char(10)             | YES  |     |         |         |
| fuelTypeID       | smallint(5) unsigned | YES  |     |         |         |
| modelYearID      | smallint(5) unsigned | YES  |     |         |         |
| startsPerVehicle | float                | YES  |     |         |         |

### translate_ActivityType

translate_ActivityType is a convenience copy of the definitions contained in the activityType table from the default database.

| Field            | Type                 | Null | Key | Default | Comment |
| ---------------- | -------------------- | ---- | --- | ------- | ------- |
| activityTypeID   | smallint(5) unsigned | NO   | PRI |         |         |
| activityTypeName | varchar(20)          | NO   |     |         |         |
| activityTypeDesc | varchar(50)          | YES  |     |         |         |

### translate_AvgSpeedBin

translate_AvgSpeedBin is a convenience copy of the definitions contained in the avgSpeedBin table from the default database.

| Field            | Type        | Null | Key | Default | Comment                                                                            |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------------- |
| avgSpeedBinID    | smallint(6) | NO   | PRI | 0       |                                                                                    |
| avgSpeedBinName  | varchar(50) | YES  |     |         |                                                                                    |
| avgBinSpeed      | float       | YES  |     |         |                                                                                    |

### translate_County

translate_County is a convenience copy of the definitions contained in the county table from the default database. It only includes the county records that appear in the output database.

| Field                | Type        | Null | Key  | Default | Comment                                                      |
| -------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| countyID             | int(11)     | NO   | PRI  | 0       |                                                              |
| stateID              | smallint(6) | NO   | PRI  | 0       |                                                              |
| countyName           | varchar(50) | YES  |      |         |                                                              |

### translate_Day

translate_Day is a convenience copy of the definitions contained in the dayOfAnyWeek table from the default database.

| Field        | Type        | Null | Key | Default | Comment                                               |
| ------------ | ----------- | ---- | --- | ------- | ----------------------------------------------------- |
| dayID        | smallint(6) | NO   | PRI | 0       |                                                       |
| dayName      | varchar(10) | YES  |     |         |                                                       |

### translate_EngTech

translate_EngTech is a convenience copy of the definitions contained in the engineTech table from the default database.

| Field       | Type        | Null | Key  | Default | Comment |
| ----------- | ----------- | ---- | ---- | ------- | ------- |
| engTechID   | smallint(6) | NO   | PRI  | 0       |         |
| engTechName | varchar(50) | YES  |      |         |         |
| engTechDesc | varchar(80) | YES  |      |         |         |

### translate_FuelSubtype

translate_FuelSubtype is a convenience copy of the definitions contained in the fuelSubtype and nrFuelSubtype tables from the default database.

| Field                          | Type        | Null | Key  | Default | Comment                                                      |
| ------------------------------ | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelSubtypeID                  | smallint(6) | NO   | PRI  | 0       |                                                              |
| fuelTypeID                     | smallint(6) | NO   | MUL  | 0       |                                                              |
| fuelSubtypeName                | varchar(50) | YES  |      |         | a short description of what the fuel subtype represents      |

### translate_FuelType

translate_FuelType is a convenience copy of the definitions contained in the fuelType and nrFuelType tables from the default database.

| Field                     | Type        | Null | Key  | Default | Comment                                                      |
| ------------------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| fuelTypeID                | smallint(6) | NO   | PRI  | 0       |                                                              |
| fuelTypeName              | varchar(50) | YES  |      |         | a general description of what the given fuel type represents |

### translate_NRSCC

translate_NRSCC is a convenience copy of the definitions contained in the nrSCC table from the default database.

| Field          | Type        | Null | Key  | Default | Comment                                                      |
| -------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| scc            | char(10)    | NO   | PRI  |         |                                                              |
| sccName        | varchar(40) | YES  |      |         |                                                              |

### translate_HP

translate_HP is a convenience copy of the definitions contained in the nrHPRangeBin table from the default database.

| Field          | Type        | Null | Key  | Default | Comment                                                      |
| -------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| hpID           | smallint(6) | NO   | PRI  |         |                                                              |
| hpName         | varchar(20) | YES  |      |         |                                                              |

### translate_Pollutant

translate_Pollutant is a convenience copy of the definitions contained in the pollutant table from the default database.

| Field                   | Type        | Null | Key | Default | Comment                                                                                                                                                                                            |
| ----------------------- | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| pollutantID             | smallint(6) | NO   | PRI | 0       |                                                                                                                                                                                                    |
| pollutantName           | varchar(50) | YES  |     |         |                                                                                                                                                                                                    |
| pollutantShortName      | varchar(50) | YES  |     |         |                                                                                                                                                                                                    |

### translate_Process

translate_Process is a convenience copy of the definitions contained in the emissionProcess table from the default database.

| Field                 | Type                 | Null | Key | Default | Comment                                             |
| --------------------- | -------------------- | ---- | --- | ------- | --------------------------------------------------- |
| processID             | smallint(6)          | NO   | PRI | 0       |                                                     |
| processName           | varchar(50)          | YES  |     |         |                                                     |
| processShortName      | varchar(50)          | YES  |     |         |                                                     |

### translate_RegClass

translate_RegClass is a convenience copy of the definitions contained in the regulatoryClass table from the default database.

| Field        | Type         | Null | Key | Default | Comment |
| ------------ | ------------ | ---- | --- | ------- | ------- |
| regClassID   | smallint(6)  | NO   | PRI | 0       |         |
| regClassName | varchar(25)  | YES  |     |         |         |
| regClassDesc | varchar(100) | YES  |     |         |         |

### translate_RoadType

translate_RoadType is a convenience copy of the definitions contained in the roadType table from the default database.

| Field               | Type        | Null | Key | Default | Comment            |
| ------------------- | ----------- | ---- | --- | ------- | ------------------ |
| roadTypeID          | smallint(6) | NO   | PRI | 0       |                    |
| roadTypeName        | varchar(50) | YES  |     |         |                    |

### translate_Sector

translate_Sector is a convenience copy of the definitions contained in the sector table from the default database.

| Field       | Type        | Null | Key | Default | Comment |
| ----------- | ----------- | ---- | --- | ------- | ------- |
| sectorID    | smallint(6) | NO   | PRI |         |         |
| sectorName  | varchar(40) | YES  |     |         |         |

### translate_SourceType

translate_SourceType is a convenience copy of the definitions contained in the sourceusetype table from the default database.

| Field          | Type        | Null | Key | Default | Comment |
| -------------- | ----------- | ---- | --- | ------- | ------- |
| sourceTypeID   | smallint(6) | NO   | PRI | 0       |         |
| HPMSVtypeID    | smallint(6) | NO   | MUL | 0       |         |
| sourceTypeName | varchar(50) | YES  |     |         |         |

### translate_State

translate_State is a convenience copy of the definitions contained in the state table from the default database.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| stateID      | smallint(6) | NO   | PRI | 0       |         |
| stateName    | varchar(25) | YES  |     |         |         |
| stateAbbr    | char(2)     | YES  |     |         |         |
