-- This MySQL script produces emission rates for PM10 as grams per vehicle-mile for each defined link 
-- 
-- The MySQL table produced in the output database is called: pm10_Grams_Per_veh_Mile
-- 
-- Users should consult Section 4 of EPA's "Transportation Conformity Guidance for Quantitative Hot-spot
-- Analyses in PM2.5 and PM10 Nonattainment and Maintenance Areas" for guidance on filling out
-- the MOVES RunSpec and importing the appropriate inputs
--

Drop   table if exists pm10_grams_per_veh_mile;
Create table pm10_grams_per_veh_mile
Select   movesRunId,
         yearId,
         monthId,
         hourId,
         linkId,
         'Total PM10' as pollutant,
         sum(rateperdistance) as GramsPerVehMile
From     rateperdistance
where    pollutantId in (100,106,107)
Group by movesRunId,
         yearId,
         monthId,
         hourId,
         linkId;


