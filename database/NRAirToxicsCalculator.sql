-- Author Wesley Faler
-- Version 2015-04-07

-- @algorithm
-- @owner Nonroad Air toxics Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

##create.nrATRatio##;
TRUNCATE nrATRatio;

##create.nrDioxinEmissionRate##;
TRUNCATE nrDioxinEmissionRate;

##create.nrIntegratedSpecies##;
TRUNCATE nrIntegratedSpecies;

##create.nrMetalEmissionRate##;
TRUNCATE nrMetalEmissionRate;

##create.nrPAHGasRatio##;
TRUNCATE nrPAHGasRatio;

##create.nrPAHParticleRatio##;
TRUNCATE nrPAHParticleRatio;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

-- Section UsenrATRatio
cache select pollutantID, processID, engTechID, fuelSubtypeID, nrHPCategory, atRatio
into outfile '##nratratio##'
from nrATRatio
where (pollutantID*100+processID) in (##outputnrATRatio##);
-- End Section UsenrATRatio

-- Section UsenrDioxinEmissionRate
cache select pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, meanBaseRate
into outfile '##nrdioxinemissionrate##'
from nrDioxinEmissionRate
where (pollutantID*100+processID) in (##outputnrDioxinEmissionRate##);
-- End Section UsenrDioxinEmissionRate

-- Section UseNonHAPTOG
cache select pollutantID
into outfile '##nrintegratedspecies##'
from nrIntegratedSpecies;
-- End Section UseNonHAPTOG

-- Section UsenrMetalEmissionRate
cache select pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, meanBaseRate
into outfile '##nrmetalemissionrate##'
from nrMetalEmissionRate
where (pollutantID*100+processID) in (##outputnrMetalEmissionRate##);
-- End Section UsenrMetalEmissionRate

-- Section UsenrPAHGasRatio
cache select pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, atratio
into outfile '##nrpahgasratio##'
from nrPAHGasRatio
where (pollutantID*100+processID) in (##outputnrPAHGasRatio##);
-- End Section UsenrPAHGasRatio

-- Section UsenrPAHParticleRatio
cache select pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, atratio
into outfile '##nrpahparticleratio##'
from nrPAHParticleRatio
where (pollutantID*100+processID) in (##outputnrPAHParticleRatio##);
-- End Section UsenrPAHParticleRatio

-- End Section Extract Data

-- Section Processing

-- All processing logic is done in the external calculator.

-- End Section Processing

-- Section Cleanup

-- End Section Cleanup

-- Section Final Cleanup
-- End Section Final Cleanup
