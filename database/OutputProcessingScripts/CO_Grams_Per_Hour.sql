-- This MySQL script produces emission rates for CO as grams per hour for each defined link 
-- 
-- The MySQL table produced in the output database is called: CO_Grams_Per_Hour
-- 
-- Users should consult Section 2 of EPA's "Using MOVES in Project-Level Carbon Monoxide Analyses"
-- for guidance on filling out the MOVES RunSpec and importing the appropriate inputs
--

FLUSH TABLES;
SELECT CURRENT_TIME;

Drop   table if exists CO_Grams_Per_Hour;
Create table CO_Grams_Per_Hour
Select   movesRunId,
         yearId,
         monthId,
         hourId,
         linkId,
         'Total CO' as pollutant,
         sum(emissionQuant) as gramsPerHour
From     movesOutput
where    pollutantId in (2)
Group by movesRunId,
         yearId,
         monthId,
         hourId,
         linkId;

