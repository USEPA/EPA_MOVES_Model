-- This MySQL script produces emission rates for PM10 as grams per hour for each defined link 
-- 
-- The MySQL table produced in the output database is called: pm10_Grams_Per_Hour
-- 
-- Users should consult Section 4 of EPA's "Transportation Conformity Guidance for Quantitative Hot-spot
-- Analyses in PM2.5 and PM10 Nonattainment and Maintenance Areas" for guidance on filling out
-- the MOVES RunSpec and importing the appropriate inputs
--

FLUSH TABLES;
SELECT CURRENT_TIME;

Drop   table if exists pm10_Grams_Per_Hour;
Create table pm10_Grams_Per_Hour
Select   movesRunId,
         yearId,
         monthId,
         hourId,
         linkId,
         'Total PM10' as pollutant,
         sum(emissionQuant) as gramsPerHour
From     movesOutput
where    pollutantId in (100,106,107)
Group by movesRunId,
         yearId,
         monthId,
         hourId,
         linkId;

