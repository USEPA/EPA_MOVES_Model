# MOVES Database Glossary

Below is a list of the column names used in the MOVES default database followed by brief definitions. Also see the [MOVES Database Tables](MOVESDatabaseTables.md) document for schema descriptions for each table in the MOVES default database.

**adjustmentCap**     The maximum limit to how much the fleet averaging algorithm can increase emission rates from internal combustion engines

**ageCategory**    A source age classification

**ageGroup**    A group of vehicle ages grouped together for the purposes of modeling their emission rates as a unit

**ageGroupID**    an integer indicator of an age group. Typically, the ageGroupID is the concatenation of the beginning and ending ages of the group. For example, ageGroupID 607 includes ages 6 and 7

**ageID**    a single integer value indicating the age of a vehicle, defined as the current calendar year minus the model year. In MOVES, ages from 0 to 30 are used

**altitude**    either "H" or "L" for high altitude or low altitude

**atBaseEmissions**   Synonym for meanBaseRate. Used for some air toxics and some hydrocarbon groupings.

**atRatio**    A fraction, or ratio, of emissions of a given toxic relative to Volatile Organic Compounds (VOC, or pollutantID ) emissions

**avft**    Alternative Vehicle Fuels and Technologies, used for user input of alternate mix of vehicle fuels types.

**avgSpeedBin**    a bin of average speeds

**avgSpeedBinID**    an integer ID for an average speed bin

**beginModelYearID**    always paired with endModelYearID. Together, they define a model year grouping for calculation purposes. They are inclusive

**cmpID**    an integer ID given to a particular component of the Complex Model

**countyID**    synonym for a county's FIPS code

**countyTypeID**    a binary value where 0 indicates a county is not in an US Census Metropolitan Census Area and 1 indicates that it is in an MSA

**countyYear**    One year in one county

**dataSourceID**    an ID indicating the data source for the values in a given table. These IDs are matched to rows in the table

**dayID**    an ID indicating type of day. It is either 5 (weekday) or 2 (weekend)

**defaultFormulationID**    see fuelFormulationID. The default fuel formulation used by MOVES.

**driveSchedule**    Also often referred to as a drive cycle. A speed vs time function indicating second-by-second vehicle speed vs time

**driveScheduleID**    An ID integer used to refer to a specific pre-defined drive cycle used in MOVES

**emissionProcess**    A unique emission pathway. Some examples are: Running Exhaust, Start Exhaust, Extended Idle, Crankcase, Evaporative Vapor Venting, Brake Wear, Tire Wear. The set of processes in the model is defined by this table

**endModelYearID**    Always paired with beginModelYearID. Together, they define a model year grouping for calculation purposes. They are inclusive.

**engineSize**    An engine size category.

**engineTech**    An engine or power technology classification. e.g. Conventional, electric, "Phase 1 gas 2-stroke handheld Class III"

**engSizeID**    an integer ID associated with an engine size or size category, which can be found in the enginesize table

**engTechID**    an integer ID associated with an engine tech or tech category, which can be mapped using the enginetech table

**ethanolLevelID**    an integer ID associated with an ethanol level or category of levels

**etohThreshID**    an integer ID associated with an ethanol threshold or category of thresholds, which can be mapped to thresholds with the etohbin table

**evMultiplier**    EPA greenhouse gas regulations define production multipliers available for certain alternative fuel vehicles. These multipliers allow manufacturers to increase the volume of credits created by each vehicle during the compliance process. This is important when calculating fleet average emission rates

**fleetAvgGroupID**     an integer ID used when calculating electric vehicle fractions to determine fleet average emission rates. These IDs are assigned by regulatory class

**fractionLifeused**    the fraction of a vehicle useful life that has passed. It can be greater than 1

**fuelAdjustment**    an adjustment to an emission rate (usually multiplicative) to account for different fuels

**fuelFormulation**    Specific fuel formulation described by a given set of fuel properties.

**fuelFormulationID**    An integer ID associated with a particular fuel formulation, which can be matched to specific fuel properties with the fuelformulation table

**fuelModelID**    an integer ID referring to a specific fuel effects model

**fuelMYGroupID**    an integer ID referring to a specific fuel model year group, which can be found in the fuelmodelyeargroup table. Note, not all IDs are defined in fuelmodelyeargroup. In these instances, the ID can be decoded. If it is 4 digits long, it is a direct model year number, and no decoding is necessary. If it is 8 digits long, the first 4 digits are the begin model year and the last 4 digits are the end model year.

**fuelParameterID**    an integer ID associated with a particular fuel parameter. These can be mapped to parameters with the fuelparameter table

**fuelRegionID**    an integer ID for a specific fuel region, which can be mapped using the fuelSupply table

**fuelSubtype**    A more detailed classification of a fuelType which may have nominal characteristics. Still not a physical batch or of fuel having measured characteristics. e.g. fuelSubtypes of Diesel Fuel might include: Fischer-Tropsch diesel (FTD100), Biodiesel, and Conventional diesel fuel

**fuelSubtypeID**   an integer ID referring to a specific fuel sub type, which can be mapped using the fuelSubType table

**fuelSupply**    Represents the Supply of Fuel of a given type at a given time and place in terms of fuel formulations.

**fuelSupplyFuelTypeID**    an integer fuel type ID associated with a particular fuel supply, as distinguished from sourceBinFuelTypeID in FuelUsageFraction table

**fuelSupplyYear**    A year for which fuel supply data has been entered in the fuelSupply table. May be used for multiple calendar years

**fuelType**    A general type of fuel. Since fuelType is used as a source bin discriminator, distinctions must be avoided which would cause the source bin distributions to vary by time or location or which would cause different activity patterns. Therefore this will be a rather gross-level categorization based on what sourceTypes are made to consume, e.g., Gasoline, Diesel Fuel, LPG, Hydrogen, and Electricity. For E-85/gasoline flexible-fueled vehicles (FFVs), the input fueltype refers to the E-85 fuel the vehicle is capable of using, while the output fueltype refers to the fuel actually used. (See **usageFraction**)

**fuelTypeID**    an integer id referring to a specific fuel type, which can be mapped using the fueltype table

**fuelYearID**    an ID mapping from calendar years with unique fuels to all calendar years

**grid**    A user-defined portion of a zone. Intended to represent a grid cell. Not used 

**gridID**    integer ID referring to a specific grid cell. Not used

**gridZoneAssoc**    The association of one grid with one zone. Not used

**growthPatternID**    an integer ID referring to a specific growth pattern, which can be mapped in nrgrowthpattern

**hourDayID**    an integer ID mapping to a specific hour-day combination

**hourID**    an integer ID referring to a specific hour of the day. It varies from 1 (midnight-1AM) to 24 (11 PM to midnight) based on the hour it represents

**hpMax**    always paired with hpMin. Together, they define a horsepower window used for classification purposes. They are inclusive

**hpMin**    always paired with hpMax. Together, they define a horsepower window used for classification purposes. They are inclusive

**hpmsVtypeID**    an ID integer mapping to a specific HPMS vehicle type, which can be mapped using the hpmsvtype table

**idleRegionID**    an integer ID used to refer to one of 5 idle regions, used for the purposes of calculating off-network idle (ONI). Each ID can be mapped to a specific region using the idleregion table

**imCoverage**    This table contains the information about the existence and effectiveness of vehicle I/M programs at specific times and places

**imModelYearGroup**    this table defines the group of model years which are grouped for the purposes of categorization in regards to IM programs

**imModelYearGroupID**    an integer ID referring to a specific IM model year group, which can be mapped using the immodelyeargroup table. Note, not all IDs are defined in immodelyeargroup. In these instances, the ID can be decoded. If it is 4 digits long, it is a direct model year number, and no decoding is necessary. If it is 8 digits long, the first 4 digits are the begin model year and the last 4 digits are the end model year.

**imProgramID**    an integer ID referring to a specific IM program

**inspectFreq**    frequency of inspections for an I/M program. Can be annual, biennial, or continuous/monthly.

**integratedspeciesset**    real chemical molecules for which MOVES produces emissions and are individually speciated into chemical mechanism species

**integratedSpeciesSetID**   an ID referring to a specific set of integrated species, which can be mapped using the integratedspeciesset table

**integratedspeciessetname**    name for a group of integrated species

**isUserInput**    either "Y" if the data is provided by the user, or "N" if it is not

**isAffectedByOnroad**    1 (pollutant emissions are calculated for onroad runs), or 0 (pollutant is not considered in onroad runs)

**isAffectedByNonroad**    nonroad equivalent of isAffectedByOnroad

**iterationID**    an user-specified ID field used to identify MOVES outputs separate from the movesRunID

**Link**    At the project scale, Links are user-defined roadway segments. They are also used to represent off-network activity

**linkID**    an integer ID referring to a specific link

**lumpedSpeciesID**    an integer ID referring to a specific lumped species for the purposes of speciation, which can be mapped using the lumpedspeciesname table

**lumpedSpeciesName**    a name for a lumped species, used in chemical mechanism speciation

**m6EmitterID**    an integer ID corresponding to an emitter level used in the M6 Sulfur Model.  1 corresponds to a normal emitter, while 2 corresponds to a high emitter

**maxAgeID**    paired with a minAgeID. Together, they define a range of ages which are grouped together. They are inclusive

**maxLevel**    paired with minLevel. Together, they define an interval of a continuous variable used

**maxModelYearID**    synonym of endModelYearID

**meanBaseRate**    base rate for calculating emissions purposes, before any adjustments. Typically, meanBaseRate is in units of g/start or g/hr, however this is not strictly true. Exceptions are noted in each table's documentation or in a table's column name "unit"

**mechanismID**    an integer ID referring to a specific chemical mechanism, which can be mapped using the mechanismname table

**mechanismName**    a name for a chemical mechanism that is used within MOVES

**minAgeID**    paired with a maxAgeID. Together, the define a window of ages which are grouped together. They are inclusive

**minLevel**    paired with maxLevel. Together, they define an interval of a continuous variable used

**minModelYearID**    synonym of beginModelYearID

**modelYearGroup**    this table defines a group of model years, which may be specific to particular pollutant-processes. The idea is that different vintages of Source Types have emissions rate differences which are not fully captured by other source bin discriminators

**modelYearGroupID**    an integer ID referring to a group of model years, which can be mapped using the modelyeargroup table. Note, not all IDs are defined in modelyeargroup. In these instances, the ID can be decoded. If it is 4 digits long, it is a direct model year number, and no decoding is necessary. If it is 8 digits long, the first 4 digits are the begin model year and the last 4 digits are the end model year.

**modelYearID**    refers to a specific model year

**monthGroupID**    synonym for a monthID

**monthID**    an integer ID for each month, corresponding to their position in the year (for example, January is 1 and October is 10)

**movesRunID**    an integer ID used by MOVES to refer to specific runs in the output database. MOVES increments this automatically and it should not be set by users

**nrAgeCategory**    nonroad equivalent of ageCategory

**nratratio**    nonroad equivalent of atRatio

**nrEquipmentType**    A specific type of nonroad equipment, such as "snowmobile" or "forklift"

**nrEquipTypeID**    an ID number for a specific nonroad equipment type

**nrFuelSubtype**    nonroad equivalent of fuelSubType

**nrFuelSupply**    nonroad equivalent of fuelSupply

**nrFuelType**    nonroad equivalent of fuelType

**nrGrowthPatternFinder**    Associates an appropriate growthPattern with each nonroad SCC and state.

**nrHourAllocPatternID**    an integer id referring to a specific nonroad hour allocation pattern, which can be mapped using the nrhourallocation table

**nrHPBinRangeID**    an integer ID referring to a specific horsepower range defined in nrHPBinRange

**nrPollutantProcessModelYear**    the nonroad equivalent to pollutantProcessModelYear

**nrScrappageCurve**    A set of fraction of median life used values with corresponding percent scrapped values for nonroad equipment. Only needed for equipment types that do not use defaultScrappage

**nrSourceUseType**    Nonroad equipment descriptor including SCC (fuel type, equipment type) and Hp bin

**operatingMode**    A category of Total Activity (for one or more pollutants and emission processes) having distinct emission rates

**opModeDistribution**    A distribution of Total Activity of a sourceUseType into operatingModes (for a particular pollutant and process).  Operating Modes Distributions are also allowed to vary by pollutant, e.g. exhaust running CO2 may necessitate fewer VHO categories than exhaust running NOx. These distributions may depend on time (Day Group and Hour Group) and location (Link) 

**opModeID**    an ID integer referring to a specific operating mode, defined in the operatingMode table

**pollutantID**    an ID integer referring to a specific pollutant, defined in the pollutant table

**pollutantProcessModelYear**    a distinct pollutant, process, and model year combination

**polProcessID**    an ID referring to a specific pollutant and process combination, with their IDs concatenated together, accounting for leading 0s where necessary

**processGroupID**   either a 1 or 2 for Exhaust or Evaporative processes respectively

**processID**    an integer ID referring to a specific process, defined in the emissionProcess table

**regClassID**    an integer ID referring to a specific regulatory class, defined in the regulatoryClass table

**regionCodeID**    code is 1 if region is considered for onroad and 2 if the region is considered for nonroad

**regionID**    a synonym for fuelRegionID

**retrofitID**    an ID integer referring to a specific retrofit technology

**roadTypeID**    an integer ID referring to a specific road type, defined in the roadType table

**RVP**    Reid Vapor Pressure (units of psi)

**scc**    Source Classification Code

**secondID**    an ID integer referring to a specific second, usually of a drive cycle, which is more or less auto-incremented

**sectorID**    an integer ID referring to a specific economic sector, defined in the sector table

**sho**    Source Hours Operating

**soakDayID**    an integer corresponding to the number of days a vehicle has been soaking

**sourceBin**    sourceBin is an abstraction used to differentiate emission levels within source use types by discriminating characteristics such as weight class, fuel type, emission standard, etc. The categories are independent of the source use types to which they may apply (so a single source bin can represent parts of multiple source use types)

**sourceBinDistribution**    The distribution of the total activity within a source use type to source bins used to calculate emissions

**sourceBinFuelTypeID** the fuel type ID associated with a particular sourceBin, as distinguished from fuelSupplyFuelTypeID in the FuelUsageFraction table

**sourceBinID** an integer ID used to identify a single source bin, which is usually a combination of several other IDs, such as regClassID, modelYearID, fuelTypeID, etc.

**sourceUseTypeModelYear**     a combination of a source type and model year, which can be a useful abstraction for certain emission rate calculations and activity allocations

**sourceType**    a synonym of sourceUseType

**sourceTypeHour**    The combination of a sourceUseType and an hourGroup.

**sourceTypeID**    a MOVES source type, which is commonly used to group like vehicles. The integer IDs are mapped to their physical interpretation in the sourceUseType table

**sourceTypeModelYearID**    a concatenation of a sourceTypeID and modelYearID

**sourceTypePolProcessID**    a concatenation of a sourceTypeID, pollutantID, and processID

**sourceUseType**    a specific class of on-road vehicles or off-road equipment having distinct activity patterns. The on-road vehicle SourceUseTypes used in MOVES are elaborated from six HPMS vehicle classes. SourceUseTypes may be referred to more briefly as either SourceTypes or (rarely) UseTypes

**stateID**    an integer ID referring to a specific state, based on its FIPS code

**subjectToEvapCalculations**    a binary integer value where 1 indicates the fuel type has evaporative emissions, and 0 indicates it does not

**sulfurFunctionID**    an ID integer referring to a specific sulfur function, defined in the sulfurModelName table

**surrogateID**    an ID integer referring to a specific surrogate that is used to estimate nonroad activity

**surrogateYearID**    yearID used to map nonroad county activity surrogate values to specific calendar years

**tankTemperatureGroupID**    an integer ID referring to tank temperature group as defined in the tankTemperatureGroup table

**testStandardsID**    an integer ID referring to a specific set of Inspection Maintenance program test standards, defined in the imTestStandards table

**testTypeID**    an integer ID referring to a specific type of IM test, defined in the imTestType table

**togSpeciationProfileID**    an integer ID referring to a specific speciation profile, defined in the togSpeciationProfileName table

**tripID**  A specific trip made by a specific vehicle is the sample population used to calculate activity for evaporative emissions

**units**    provides the units of the relevant column in the table, typically for emission rates or a type of activity

**UsageFraction** The fraction of E-85 use among E-85 capable vehicles

**vehID**    an ID integer referring to a specific vehicle in the sample population used to calculate activity for evaporative emissions

**weightClassID**    an integer ID referring to a specific weight class, defined in the weightClass table

**yearID**    equivalent to calendar year to be modeled and selected in the Run Spec 

**zone**    An area within a County. At the default (national) scale there will be one Zone for each County, so at that scale Zones are equivalent to Counties

**zoneID**    an integer ID referring to a specific zone
