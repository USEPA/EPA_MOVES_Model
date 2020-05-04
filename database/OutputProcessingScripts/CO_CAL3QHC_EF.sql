-- This MySQL script produces emission rates for CO for use in the CAL3QHC air quality model 
-- 
-- For free-flow links (i.e., where the average speed is greater than 0 mph)
-- emission factors will be generated as grams per vehicle-mile
-- 
-- For queue links (i.e., where the average speed is 0 mph)
-- emission factors will be generated as grams per vehicle-hour
-- 
-- The MySQL table produced in the output database is called: CO_EmissionFactors
-- 
-- Users should consult Section 2 of EPA's "Using MOVES in Project-Level Carbon Monoxide Analyses"
-- for guidance on filling out the MOVES RunSpec and importing the appropriate inputs
--

FLUSH TABLES;
SELECT CURRENT_TIME;

Drop   table if exists CO_EmissionFactorsT;
Create table CO_EmissionFactorsT
Select   a.movesRunId,
         a.yearId,
         a.monthId,
         a.dayId,
         a.hourId,
         a.linkId,
         'CO' as pollutant,
         sum(a.emissionQuant) as co
From     MovesOutput          as a
Group by a.movesRunId,
         a.yearId,
         a.linkId;


Alter table CO_EmissionFactorsT add column distance        real;
Alter table CO_EmissionFactorsT add column population      real;
Alter table CO_EmissionFactorsT add column GramsPerVehMile real;
Alter table CO_EmissionFactorsT add column GramsPerVehHour real;


Update CO_EmissionFactorsT as a set distance = (select sum(b.activity)
                                                from   movesActivityOutput as b
                                                where  a.movesRunId = b.movesRunId
                                                  and  a.yearId     = b.yearId
                                                  and  a.linkId     = b.linkId
                                                  and  b.activityTypeId = 1);

Update CO_EmissionFactorsT as a set population = (select sum(b.activity)
                                    from   movesActivityOutput as b
                                    where  a.movesRunId = b.movesRunId
                                      and  a.yearId     = b.yearId
                                      and  a.linkId     = b.linkId
                                      and  b.activityTypeId = 6);

Update CO_EmissionFactorsT set GramsPerVehMile = CO / Distance    where Distance >  0.0;
Update CO_EmissionFactorsT set GramsPerVehHour = CO / Population  where Distance <= 0.0;

Drop   table if exists CO_EmissionFactors;
Create table CO_EmissionFactors
Select movesRunId,
       yearId,
       monthId,
       dayId,
       hourId,
       linkId,
       pollutant,
       GramsPerVehMile,
       GramsPerVehHour
From   CO_EmissionFactorsT;

Drop   table if exists CO_EmissionFactorsT;  
