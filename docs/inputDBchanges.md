# MOVES Input Database Schema Changes

This document outlines changes that have been made to input databases at County or Project Scale. All tables mentioned are further documented in the [MOVES Database Documentation](MOVESDatabaseTables.md) and specific columns are defined in the [MOVES Database Glossary](MOVESGlossary.md). More information on how to use these new and modified tables and build valid input databases can be found in the [Technical Guidance](https://www.epa.gov/state-and-local-transportation/policy-and-technical-guidance-state-and-local-transportation#emission) for state and local users. 

## Tables Modified

#### *county*

Added columns `countyTypeID` and `msa`. `countyTypeID` is a 1 or 0, where 1 indicates the county is in a U.S. Census Metropolitan Statistical Area. When `countyTypeID` is 1, the `msa` column indicates its corresponding MSA.

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


#### *hotellingActivityDistribution*

Added column `zoneID`, which allows the operating mode distribution to change from county to county.

| Field            | Type        | Null | Key | Default | Comment                                                                                        |
| ---------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------------------------- |
| zoneID           | int(11)     | NO   | PRI |         |                                                                                                |
| beginModelYearID | smallint(6) | NO   | PRI |         |                                                                                                |
| endModelYearID   | smallint(6) | NO   | PRI |         |                                                                                                |
| opModeID         | smallint(6) | NO   | PRI |         | this table only contains the hotelling operating modes (200-204)                               |
| opModeFraction   | float       | NO   |     |         | fraction of time spend in each operating mode, summing to 1 for each zone and model year group |

#### *startsHourFraction*

The `zoneID` column is removed, and the column `sourceTypeID` is added.

| Field              | Type        | Null | Key | Default | Comment                                                                                                  |
| ------------------ | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------------------------------------- |
| dayID              | smallint(6) | NO   | PRI |         |                                                                                                          |
| hourID             | smallint(6) | NO   | PRI |         |                                                                                                          |
| sourceTypeID       | smallint(6) | NO   | PRI | 0       |                                                                                                          |
| allocationFraction | double      | NO   |     |         | fraction of starts in the given hour, which should always sum to 1 for each day, source type combination |

#### *startsMonthAdjust*

Column `sourceTypeID` is added to allow start adjustments to vary by source type.

| Field           | Type        | Null | Key | Default | Comment                                                                                           |
| --------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------------------------------------- |
| monthID         | smallint(6) | NO   | PRI |         |                                                                                                   |
| sourceTypeID    | smallint(6) | NO   | PRI | 0       |                                                                                                   |
| monthAdjustment | double      | NO   |     |         | raw multiplicative factor to get starts activity in a month<br> based on the average for the year |


#### *startsPerDay*

The `zoneID` column was removed and the `sourceTypeID` column was added.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| dayID        | smallint(6) | NO   | PRI | 0       |         |
| sourceTypeID | smallint(6) | NO   | PRI | 0       |         |
| startsPerDay | double      | YES  |     |         |         |

#### *state*

Column *idleRegionID* was added to account for geographic variation in Off-Network Idle (ONI) activity.

| Field        | Type        | Null | Key | Default | Comment |
| ------------ | ----------- | ---- | --- | ------- | ------- |
| stateID      | smallint(6) | NO   | PRI | 0       |         |
| stateName    | char(25)    | YES  |     |         |         |
| stateAbbr    | char(2)     | YES  |     |         |         |
| idleRegionID | int(11)     | YES  |     |         |         |

## Tables Removed

#### *hotellingHours*

This table is still in the default database, but is built by MOVES based on the tables *hotellingHoursPerDay*, *hotellingHourFraction*, and *hotellingMonthAdjust*.

#### *roadType*

This table also exists in the default database, but should not be included in a county or project scale input database.

#### *importStartsOpModeDistribution*

This table was replaced (albeit with a different schema) by the table *startsOpModeDistribution*.

#### *startsSourceTypeFraction*

This table was removed because other added/modified tables allow the user to specify starts per source type more explicitly.

## Tables Added

#### *hotellingAgeFraction*

| Field       | Type        | Null | Key | Default | Comment                                                                     |
| ----------- | ----------- | ---- | --- | ------- | --------------------------------------------------------------------------- |
| zoneID      | int(11)     | NO   | PRI |         |                                                                             |
| ageID       | smallint(6) | NO   | PRI |         |                                                                             |
| ageFraction | double      | NO   |     |         | fraction of hotelling activity done by vehicles of each age within the zone |

#### *hotellingHourFraction*

| Field        | Type        | Null | Key | Default | Comment                                                                    |
| ------------ | ----------- | ---- | --- | ------- | -------------------------------------------------------------------------- |
| zoneID       | int(11)     | NO   | PRI |         |                                                                            |
| dayID        | smallint(6) | NO   | PRI |         |                                                                            |
| hourID       | smallint(6) | NO   | PRI |         |                                                                            |
| hourFraction | double      | NO   |     |         | fraction of hotelling that happens in each hour of the day within the zone |

#### *hotellingHoursPerDay*
| Field                | Type        | Null | Key | Default | Comment                                                             |
| -------------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------- |
| yearID               | smallint(6) | NO   | PRI |         |                                                                     |
| zoneID               | int(11)     | NO   | PRI |         |                                                                     |
| dayID                | smallint(6) | NO   | PRI |         |                                                                     |
| hotellingHoursPerDay | double      | NO   |     |         | average number of hotelling hours per day within each year and zone |


#### *hotellingMonthAdjust*

| Field           | Type        | Null | Key | Default | Comment                                                                     |
| --------------- | ----------- | ---- | --- | ------- | --------------------------------------------------------------------------- |
| zoneID          | int(11)     | NO   | PRI |         |                                                                             |
| monthID         | smallint(6) | NO   | PRI |         |                                                                             |
| monthAdjustment | double      | NO   |     |         | raw multiplicative factor used to adjust hotelling hours for<br> each month |

#### *idleDayAdjust*
| Field         | Type        | Null | Key  | Default | Comment                                                      |
| ------------- | ----------- | ---- | ---- | ------- | ------------------------------------------------------------ |
| sourceTypeID  | smallint(6) | NO   | PRI  |         |                                                              |
| dayID         | smallint(6) | NO   | PRI  |         |                                                              |
| idleDayAdjust | double      | NO   |      |         | raw multiplicative adjustment used to scale off-network idle (ONI) activity for each day |


#### *idleModelYearGrouping*
| Field             | Type        | Null | Key | Default | Comment                                                                        |
| ----------------- | ----------- | ---- | --- | ------- | ------------------------------------------------------------------------------ |
| sourceTypeID      | smallint(6) | NO   | PRI |         |                                                                                |
| minModelYearID    | smallint(6) | NO   | PRI |         |                                                                                |
| maxModelYearID    | smallint(6) | NO   | PRI |         |                                                                                |
| totalIdleFraction | double      | NO   |     |         | fraction of total SHO that is ONI for each source type and model year grouping |

#### *idleMonthAdjust*
| Field           | Type        | Null | Key | Default | Comment                                                                      |
| --------------- | ----------- | ---- | --- | ------- | ---------------------------------------------------------------------------- |
| sourceTypeID    | smallint(6) | NO   | PRI |         |                                                                              |
| monthID         | smallint(6) | NO   | PRI |         |                                                                              |
| idleMonthAdjust | double      | NO   |     |         | raw multiplicative adjustment used to scale ONI idle activity for each month |


#### *startsAgeAdjustment*
| Field         | Type        | Null | Key | Default | Comment                                                                                   |
| ------------- | ----------- | ---- | --- | ------- | ----------------------------------------------------------------------------------------- |
| sourceTypeID  | smallint(6) | NO   | PRI | 0       |                                                                                           |
| ageID         | smallint(6) | NO   | PRI | 0       |                                                                                           |
| ageAdjustment | double      | YES  |     |         | Adjustment applied to the average number of starts to get<br> starts for a particular age |

#### *startsOpModeDistribution*
| Field          | Type        | Null | Key | Default | Comment                                                              |
| -------------- | ----------- | ---- | --- | ------- | -------------------------------------------------------------------- |
| dayID          | smallint(6) | NO   | PRI | 0       |                                                                      |
| hourID         | smallint(6) | NO   | PRI | 0       |                                                                      |
| sourceTypeID   | smallint(6) | NO   | PRI | 0       |                                                                      |
| ageID          | smallint(6) | NO   | PRI | 0       |                                                                      |
| opModeID       | smallint(6) | NO   | PRI | 0       |                                                                      |
| opModeFraction | double      | YES  |     |         | Should sum to 1 for each day, hour, source type, and age combination |
| isUserInput    | char(1)     | NO   |     | N       |                                                                      |

#### *startsPerDayPerVehicle*
| Field                  | Type        | Null | Key | Default | Comment |
| ---------------------- | ----------- | ---- | --- | ------- | ------- |
| dayID                  | smallint(6) | NO   | PRI | 0       |         |
| sourceTypeID           | smallint(6) | NO   | PRI | 0       |         |
| startsPerDayPerVehicle | double      | YES  |     |         |         |

#### *totalIdleFraction*

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


## Glossary

| Name                   | Description                                                  |
| ---------------------- | ------------------------------------------------------------ |
| ageAdjustment          | adjustment multiplier to get age-based activity. Typical average value   across all ages is 1 |
| ageFraction            | The fraction   of activity allocated to vehicles of each age |
| ageID                  | vehicle age, ranging from 0 to 31. This is defined as calendar year - model year |
| allocationFraction     | Fraction of   activity that takes place in that hour (used in startshourfraction) |
| altitude               | "H"   or "L", for high altitude or low altitude              |
| barometricPressure     | Expressed in inches of mercury                               |
| beginModelYearID       | first of 2   model years bracketing like model years in terms of activity distribution |
| countyID               | 1000 *   FIPS state code + FIPS county identification code.  |
| countyTypeID           | County type as   defined for ONI. 1 is for an urban county (defined as bein in an MSA), 0 for   rural |
| dayID                  | Identifies a   kind of day of the week. There may be no more than 7 such categories.   5=weekdays and 2=weekends |
| endModelYearID         | second of 2   model years bracketing like model years in terms of activity distribution |
| GPAFract               | Fraction of   county in the fuel geographic phase-in area    |
| hotellingHoursPerDay   | hotelling   hours per typical day for the   dayID in the zone, across the entire population |
| hourFraction           | Fraction of   activity allocated to each hour of the day     |
| hourID                 | Identifies the   hour of the day. 1 = midnight - 1AM and 24 = 11 PM to midnight |
| idleDayAdjust          | raw   multiplicative factor used to scale idle activity for the day |
| idleMonthAdjust        | adjustment   multiplier to distribute activity by month. This is a raw multiplier, and is   not normalized. The average value across months is typically 1 |
| idleRegionID           | Region which   state belongs to for the purposes of allocating ONI activity |
| isUserInput            | Y for Yes and N for No                                       |
| maxModelYearID         | second of 2 model years bracketing like model years in terms of activity distribution |
| minModelYearID         | first of 2 model years bracketing like model years in terms of activity distribution |
| monthAdjust            | Raw multiplier used to scale activity for each month. Typical average across all months is 1 |
| monthAdjustment        | adjustment multiplier to distribute activity by month. This is a raw multiplier, and is not normalized. The average value across months is typically 1 |
| monthID                | MonthIDs will range from 1 (representing January) through 12 (representing December) |
| msa                    | US Census Metropolitan Statistical Area for county, if there is one |
| opModeFraction         | Fraction of total activity associated with operating mode    |
| opModeID               | operating mode used to look up emission rates based on different types of vehicle operation |
| sourceTypeID           | Source use type identification number                        |
| startsPerDay           | Starts per typical day for source type, across the entire population |
| startsPerDayPerVehicle | Starts per vehicle in a *typical* day for each source type   |
| stateAbbr              | 2 character state abbreviation                               |
| stateID                | FIPS state identification code                               |
| totalIdleFraction      | Fraction of total engine operation time which is spent doing off-network idle |
| yearID                 | identical to 4-digit year                                    |
| zoneID                 | zone or county. Typically countyID with an appended 0        |










