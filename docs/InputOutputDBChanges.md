# MOVES Input/Output Database Schema Changes

This document outlines changes between MOVES4 and MOVES5 that have been made to input and output databases. All tables mentioned are further documented in the [MOVES Database Documentation](MOVESDatabaseTables.md) and specific columns are defined in the [MOVES Database Glossary](MOVESGlossary.md). More information on how to build valid input databases can be found in the [Technical Guidance](https://www.epa.gov/state-and-local-transportation/policy-and-technical-guidance-state-and-local-transportation#emission) for state and local users. 

## Input Database Changes

### Tables Modified

All tables now use UTF-8 encoding, which supports international use of MOVES as well as changes in Java libraries used by MOVES. Specifically, MOVES input databases have a default charset of *utf8mb4* and use the *utf8mb4_unicode_ci* collation.

### Tables Removed

None.

### Tables Added

Added in MOVES4.0.1, Default Scale user input databases can contain the **ZoneRoadType** and **Zone** tables.

At Default Scale, MOVES calculates VMT and VPOP for each county in the RunSpec by applying the **ZoneRoadType** and **Zone** fractions to the national VMT and VPOP values. If you want to supply user-input VMT and VPOP values at Default Scale, you must also supply user-input **ZoneRoadType** and **Zone** tables. 

If a single county is selected in the RunSpec, **ZoneRoadType** should have a *SHOAllocFactor* of 1.0 for each road type, and **Zone** should have 1.0 for all of the *AllocFactor* columns. This will, in effect, allocate 100% of the VMT and VPOP (supplied through the Vehicle Type VMT and Source Type Population tabs, respectively) to the selected county.

If more than one county is selected in the RunSpec:

1. Supply VMT and VPOP inputs that reflect the combined VMT and VPOP for all selected counties.
2. Calculate each individual county's proportion of the combined VMT.
2. In **ZoneRoadType**, supply these values for each county's *SHOAllocFactor* on each road type. (The *SHOAllocFactor* column should sum to 4.0, because there are 4 road types in this table.)
3. In **Zone**, supply these values for each county's *AllocFactor* columns.

If state pre-aggregation is used, the VMT and VPOP inputs should reflect the total state VMT and VPOP, and the **ZoneRoadType** and **Zone** tables should be populated following the instructions above for multiple counties, as if each county in the state was selected individually in the RunSpec. At runtime, MOVES will use these allocation factors as weighting values when pre-aggregating other inputs, such as meteorology data. The output will be at the state level.

## Output Database Changes

### Tables Modified

All tables now use UTF-8 encoding, which supports international use of MOVES as well as changes in Java libraries used by MOVES. Specifically, MOVES output databases have a default charset of *utf8mb4* and use the *utf8mb4_unicode_ci* collation.

### Tables Removed

None.

### Tables Added

None.
