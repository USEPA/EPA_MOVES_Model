-- This MySQL script produces emission rates for CO as grams per vehicle-mile for each defined link 
-- 
-- The MySQL table produced in the output database is called: CO_Grams_Per_Veh_Mile
-- 
-- Users should consult Section 2 of EPA's document "Using MOVES in Project-Level Carbon Monoxide Analyses"
-- for guidance on filling out the MOVES RunSpec and importing the appropriate inputs
--

FLUSH TABLES;
SELECT CURRENT_TIME;

Drop   table if exists CO_Grams_Per_Veh_Mile;
Create table CO_Grams_Per_Veh_Mile
Select   movesRunId,
         yearId,
         monthId,
         hourId,
         linkId,
         'Total CO' as pollutant,
         sum(rateperdistance) as GramsPerVehMile
From     rateperdistance
where    pollutantId in (2)
Group by movesRunId,
         yearId,
         monthId,
         hourId,
         linkId;
