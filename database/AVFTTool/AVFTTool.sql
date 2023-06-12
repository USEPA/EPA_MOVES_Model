-- --------------------------------------------------------------------------------------
-- Doc string
-- --------------------------------------------------------------------------------------
drop procedure if exists AVFTTool_CreateDefaultAVFT;

BeginBlock
create procedure AVFTTool_CreateDefaultAVFT()
begin
    DROP TABLE IF EXISTS defaultAVFT;
    CREATE TABLE defaultAVFT LIKE ##defaultdb##.avft;
    INSERT INTO defaultAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, SUM(stmyFraction) AS fuelEngFraction
    FROM ##defaultdb##.samplevehiclepopulation
    GROUP BY sourceTypeID, modelYearID, fuelTypeID, engTechID;
end
EndBlock

-- ---------------------------------------------------------------------
-- normalize inputs so that distributions sum to 1 before we do anything
-- ---------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_NormalizeInputs()
begin
    CREATE TABLE inputsNormalizedAVFT
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS totalFuelEngFraction
    FROM inputavft
    GROUP BY sourceTypeID, modelYearID;

    ALTER TABLE inputavft ADD COLUMN totalFuelEngFraction DOUBLE NOT NULL DEFAULT 1;
    UPDATE inputavft, inputsNormalizedAVFT
    SET inputavft.totalFuelEngFraction = inputsNormalizedAVFT.totalFuelEngFraction
    WHERE inputavft.sourceTypeID = inputsNormalizedAVFT.sourceTypeID
    AND inputavft.modelYearID = inputsNormalizedAVFT.modelYearID;

    INSERT INTO messages
    SELECT CONCAT('Warning: fuelEngFraction sums to ', totalFuelEngFraction, ' instead of 1.0000 for ', sourceTypeID, ' / ', modelYearID, '. Tool is renormalizing this fraction before continuing.') AS message
    FROM inputavft
    WHERE ABS(totalFuelEngFraction - 1.0000) > 0.00001
    GROUP BY sourceTypeID, modelYearID;

    UPDATE inputavft 
    SET fuelEngFraction = fuelEngFraction / totalFuelEngFraction;

    ALTER TABLE inputavft DROP COLUMN totalFuelEngFraction;
    DROP TABLE inputsNormalizedAVFT;
end
EndBlock


-- --------------------------------------------------------------------------------------
-- gap fill input AVFT with defaults
-- --------------------------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_GapFilling_WithDefaults(
    IN baseYearIN int,
    IN sourceTypeIN int
)
begin
    -- make sure inputGapFilledAVFT exists as it would at the end of this function (so rest of function works the same
    -- if this is the first time this procedure is called or subsequent calls)
    CREATE TABLE IF NOT EXISTS inputGapFilledAVFT (
        sourceTypeID int,
        modelYearID int,
        fuelTypeID int,
        engTechID int,
        fuelEngFraction double
    );

    -- add isUserInput column
    ALTER TABLE inputGapFilledAVFT ADD COLUMN isUserInput varchar(1) AFTER fuelEngFraction;

    -- Combine user inputs with defaults
    INSERT INTO inputGapFilledAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, i.fuelEngFraction, 'Y' AS isUserInput
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN baseYearIN-30 AND baseYearIN AND i.fuelEngFraction IS NOT NULL
      AND d.sourceTypeID = sourceTypeIN
    UNION
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, d.fuelEngFraction, 'N' AS isUserInput
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN baseYearIN-30 AND baseYearIN AND i.fuelEngFraction IS NULL
      AND d.sourceTypeID = sourceTypeIN;

    -- renormalize input values, preserving default values
    DROP TABLE if exists inputGapFilledAVFTtmp;
    CREATE table inputGapFilledAVFTtmp
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, SUM(fuelEngFraction) AS sumOfDefaults
    FROM inputGapFilledAVFT
    WHERE isUserInput = 'N'
    GROUP BY sourceTypeID, modelYearID;

    ALTER TABLE inputGapFilledAVFT ADD COLUMN sumOfDefaults DOUBLE NOT NULL DEFAULT 0;
    UPDATE inputGapFilledAVFT t, inputGapFilledAVFTtmp t_tmp 
    SET t.sumOfDefaults = t_tmp.sumOfDefaults
    WHERE t.sourcetypeID = t_tmp.sourceTypeID AND t.modelYearID = t_tmp.modelYearID;

    UPDATE inputGapFilledAVFT SET fuelEngFraction = fuelEngFraction * (1-sumOfDefaults)
    WHERE inputGapFilledAVFT.isUserInput = 'Y';

    -- clean up
    ALTER TABLE inputGapFilledAVFT DROP COLUMN isUserInput;
    ALTER TABLE inputGapFilledAVFT DROP COLUMN sumOfDefaults;
    DROP TABLE inputGapFilledAVFTtmp;

    INSERT INTO messages
    SELECT CONCAT('ERROR: source type ', sourceTypeID, ' fuel distribution does not sum to 1 for MY', modelYearID, '. Please correct this in the input data.')
    FROM inputgapfilledavft
    GROUP BY sourceTypeID, modelYearID
    HAVING ABS(SUM(fuelEngFraction)-1.0000) > 0.00001 AND SUM(fuelEngFraction) <> 0 AND sourceTypeID = sourceTypeIN
    LIMIT 1;
end
EndBlock


-- --------------------------------------------------------------------------------------
-- gap fill input AVFT with zeros
-- --------------------------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_GapFilling_With0s(
    IN baseYearIN int,
    IN sourceTypeIN int
)
begin
    -- make sure inputGapFilledAVFT exists as it would at the end of this function or the gap fill with defaults function
    -- (so rest of function works the same if this is the first time this procedure is called or subsequent calls)
    CREATE TABLE IF NOT EXISTS inputGapFilledAVFT (
        sourceTypeID int,
        modelYearID int,
        fuelTypeID int,
        engTechID int,
        fuelEngFraction double
    );

    INSERT INTO inputGapFilledAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, i.fuelEngFraction
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN baseYearIN-30 AND baseYearIN AND i.fuelEngFraction IS NOT NULL
      AND d.sourceTypeID = sourceTypeIN
    UNION
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, 0 AS fuelEngFraction
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN baseYearIN-30 AND baseYearIN AND i.fuelEngFraction IS NULL
      AND d.sourceTypeID = sourceTypeIN;

    INSERT INTO messages
    SELECT CONCAT('ERROR: source type ', sourceTypeID, ' fuel distribution does not sum to 1 for MY', modelYearID, '. Please correct this in the input data.')
    FROM inputgapfilledavft
    GROUP BY sourceTypeID, modelYearID
    HAVING ABS(SUM(fuelEngFraction)-1.0000) > 0.00001 AND SUM(fuelEngFraction) <> 0 AND sourceTypeID = sourceTypeIN
    LIMIT 1;
    
    INSERT INTO messages
    SELECT CONCAT('ERROR: source type ', sourceTypeID, ' has no input fuel distributions for MY', modelYearID, '. Either manually fix in the input data or select the Use Defaults gap-filling method.')
    FROM inputgapfilledavft
    GROUP BY sourceTypeID, modelYearID
    HAVING SUM(fuelEngFraction) = 0 AND sourceTypeID = sourceTypeIN
    LIMIT 1;
end
EndBlock


-- --------------------------
-- Constant Projection Method
-- --------------------------
BeginBlock
create procedure AVFTTool_Projection_Constant(
    IN baseYearIN int,
    IN analysisYearIN int,
    IN sourceTypeIN int
)
begin
    CREATE TABLE IF NOT EXISTS outputAVFT LIKE ##defaultdb##.avft;

    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM inputGapFilledAVFT
    WHERE sourceTypeID = sourceTypeIN 
      AND modelYearID <= baseYearIN;

    INSERT INTO outputAVFT
    SELECT i.sourceTypeID, d.modelYearID, i.fuelTypeID, i.engTechID, i.fuelEngFraction
    FROM inputGapFilledAVFT i
    join defaultavft d USING (sourceTypeID, fuelTypeID, engTechID)
    WHERE i.sourceTypeID = sourceTypeIN
      AND i.modelYearID = baseYearIN
      AND d.modelYearID BETWEEN baseYearIN + 1 AND analysisYearIN;
end
EndBlock

-- --------------------------
-- National Projection Method
-- --------------------------
BeginBlock
create procedure AVFTTool_Projection_National(
    IN baseYearIN int,
    IN analysisYearIN int,
    IN sourceTypeIN int
)
begin
    CREATE TABLE IF NOT EXISTS outputAVFT LIKE ##defaultdb##.avft;

    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM inputGapFilledAVFT
    WHERE sourceTypeID = sourceTypeIN 
      AND modelYearID <= baseYearIN;

    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM defaultAVFT
    WHERE sourceTypeID = sourceTypeIN
      AND modelYearID BETWEEN baseYearIN + 1 AND analysisYearIN;
end
EndBlock


-- --------------------------
-- Proportional Projection Method using ratios.
-- --------------------------
BeginBlock
create procedure AVFTTool_Projection_Proportional(
    IN baseYearIN int,
    IN analysisYearIN int,
    IN sourceTypeIN int
)
begin
    SET @boundaryRatioLimit = 2;

    CREATE TABLE IF NOT EXISTS outputAVFT LIKE ##defaultdb##.avft;

    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM inputGapFilledAVFT
    WHERE sourceTypeID = sourceTypeIN 
      AND modelYearID <= baseYearIN;
      
    -- calculate ratio of input to default data for base year and set upper boundary condition
    -- this is the "proportional" scaling factor applied to the default fractions
    CREATE TABLE proportional_scaling_factors
    SELECT d2.sourceTypeID, d2.modelYearID, d2.fuelTypeID, d2.engTechID, 
           LEAST(COALESCE(i1.fuelEngFraction/d2.fuelEngFraction, 1), @boundaryRatioLimit) AS fuelEngFractionRatio
    FROM inputGapFilledAVFT i1, defaultavft d2
    WHERE i1.modelYearID = baseYearIN
      AND d2.modelYearID = baseYearIN
      AND i1.sourceTypeID = sourceTypeIN
      AND d2.sourceTypeID = sourceTypeIN
      AND i1.fuelTypeID = d2.fuelTypeID AND i1.engTechID = d2.engTechID;

    -- scale the default fractions to calculate unnormalized projection years
    INSERT INTO outputAVFT
    SELECT d.sourceTypeID, d.modelYearID, d.fuelTypeID, d.engTechID, 
           d.fuelEngFraction * p.fuelEngFractionRatio AS fuelEngFraction
    FROM defaultavft d
    JOIN proportional_scaling_factors p ON (d.sourceTypeID = p.sourceTypeID AND
                                            d.fuelTypeID = p.fuelTypeID AND
                                            d.engTechID = p.engTechID)
    WHERE d.modelYearID BETWEEN baseYearIN+1 AND analysisYearIN;

    -- enforce minimum boundary
    UPDATE outputavft o, defaultavft d
    SET o.fuelEngFraction = d.fuelEngFraction / @boundaryRatioLimit
    WHERE o.sourceTypeID = sourceTypeIN
      AND d.sourceTypeID = sourceTypeIN
      AND o.fuelTypeID = d.fuelTypeID
      AND o.engTechID = d.engTechID
      AND o.modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
      AND o.modelYearID = d.modelYearID
      AND o.fuelEngFraction < d.fuelEngFraction / @boundaryRatioLimit;
      
    -- normalize projected data
    CREATE TABLE outputAVFT_grouped
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS totalFuelEngFraction
    FROM outputAVFT
    WHERE modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
      AND sourceTypeID = sourceTypeIN
    GROUP BY sourceTypeID, modelYearID;

    ALTER TABLE outputAVFT ADD COLUMN totalFuelEngFraction DOUBLE NOT NULL DEFAULT 1;
    UPDATE outputAVFT, outputAVFT_grouped
    SET outputAVFT.totalFuelEngFraction = outputAVFT_grouped.totalFuelEngFraction
    WHERE outputAVFT.sourceTypeID = outputAVFT_grouped.sourceTypeID
      AND outputAVFT.modelYearID = outputAVFT_grouped.modelYearID
      AND outputAVFT.modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
      AND outputAVFT.sourceTypeID = sourceTypeIN;

    UPDATE outputAVFT 
    SET fuelEngFraction = fuelEngFraction / totalFuelEngFraction
    WHERE modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
      AND sourceTypeID = sourceTypeIN;

    ALTER TABLE outputAVFT DROP COLUMN totalFuelEngFraction;
    DROP TABLE proportional_scaling_factors;
    DROP TABLE outputAVFT_grouped;
end
EndBlock

-- --------------------------------
-- Known Fraction Projection Method
-- --------------------------------
BeginBlock
create procedure AVFTTool_Projection_KnownFractions(
    IN baseYearIN int,
    IN analysisYearIN int,
    IN sourceTypeIN int
)
begin
    SET @boundaryRatioLimit = 2;

    CREATE TABLE IF NOT EXISTS outputAVFT LIKE ##defaultdb##.avft;
 
    -- load base AVFT table
    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM inputGapFilledAVFT
    WHERE sourceTypeID = sourceTypeIN 
    AND modelYearID <= baseYearIN;

    -- load known fractions and label as such
    ALTER TABLE outputAVFT ADD COLUMN isKnownFraction CHAR(1) DEFAULT 'N';
    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction, 'Y' as isKnownFraction
    FROM knownAVFT
    WHERE sourceTypeID = sourceTypeIN 
    AND modelYearID BETWEEN baseYearIN+1 AND analysisYearIN;
    
    -- calculate ratio of input to default data for base year and set upper boundary condition
    -- this is the "proportional" scaling factor applied to the default fractions
    CREATE TABLE proportional_scaling_factors
    SELECT d2.sourceTypeID, d2.modelYearID, d2.fuelTypeID, d2.engTechID, 
           LEAST(COALESCE(i1.fuelEngFraction/d2.fuelEngFraction, 1), @boundaryRatioLimit) AS fuelEngFractionRatio
    FROM inputGapFilledAVFT i1, defaultavft d2
    WHERE i1.modelYearID = baseYearIN
      AND d2.modelYearID = baseYearIN
      AND i1.sourceTypeID = sourceTypeIN
      AND d2.sourceTypeID = sourceTypeIN
      AND i1.fuelTypeID = d2.fuelTypeID AND i1.engTechID = d2.engTechID;

    -- scale the default fractions for known fractions = 'N' to calculate unnormalized projection years
    INSERT IGNORE INTO outputAVFT
    SELECT d.sourceTypeID, d.modelYearID, d.fuelTypeID, d.engTechID, 
           d.fuelEngFraction * p.fuelEngFractionRatio AS fuelEngFraction, 'N'
    FROM defaultavft d
    JOIN proportional_scaling_factors p ON (d.sourceTypeID = p.sourceTypeID AND
                                            d.fuelTypeID = p.fuelTypeID AND
                                            d.engTechID = p.engTechID)
    WHERE d.modelYearID BETWEEN baseYearIN+1 AND analysisYearIN;

    -- enforce minimum boundary (only for known fractions = 'N')
    UPDATE outputavft o, defaultavft d
    SET o.fuelEngFraction = d.fuelEngFraction / @boundaryRatioLimit
    WHERE o.sourceTypeID = sourceTypeIN
      AND d.sourceTypeID = sourceTypeIN
      AND o.fuelTypeID = d.fuelTypeID
      AND o.engTechID = d.engTechID
      AND o.modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
      AND o.modelYearID = d.modelYearID
      AND o.fuelEngFraction < d.fuelEngFraction / @boundaryRatioLimit
	  AND o.isKnownFraction = 'N';

    -- normalize projected data (preserving the known fractions, and warning if the KnownSum is missing any model years, as this means the Known Fractions don't cover all model years being projected)
    CREATE TABLE outputAVFT_grouped
    SELECT sourceTypeID, modelYearID, KnownSum, NotKnownSum
    FROM (SELECT sourceTypeID, modelYearID, sum(fuelEngFraction) as KnownSum
        FROM outputAVFT
            WHERE modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
            AND sourceTypeID = sourceTypeIN
            AND isKnownFraction = 'Y'
        GROUP BY sourceTypeID, modelYearID
    ) AS t1 RIGHT JOIN (
        SELECT sourceTypeID, modelYearID, sum(fuelEngFraction) as NotKnownSum
        FROM outputAVFT
            WHERE modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
            AND sourceTypeID = sourceTypeIN
            AND isKnownFraction = 'N'
        GROUP BY sourceTypeID, modelYearID
    ) AS t2 USING (sourceTypeID, modelYearID);

    INSERT INTO messages
    SELECT CONCAT('Warning: Known Fractions for ', sourceTypeID, 's are missing values for ', GROUP_CONCAT(modelYearID ORDER BY modelYearID SEPARATOR ', '), '. The proportional method will be used for these instead.') AS message
    FROM outputAVFT_grouped
    WHERE KnownSum IS NULL
    GROUP BY sourceTypeID;
    UPDATE outputAVFT_grouped SET KnownSum = 0 WHERE KnownSum IS NULL;

    ALTER TABLE outputAVFT ADD COLUMN KnownSum DOUBLE NOT NULL DEFAULT 1;
    ALTER TABLE outputAVFT ADD COLUMN NotKnownSum DOUBLE NOT NULL DEFAULT 1;
    UPDATE outputAVFT, outputAVFT_grouped
    SET outputAVFT.KnownSum = outputAVFT_grouped.KnownSum,
        outputAVFT.NotKnownSum = outputAVFT_grouped.NotKnownSum
    WHERE outputAVFT.sourceTypeID = outputAVFT_grouped.sourceTypeID
    AND outputAVFT.modelYearID = outputAVFT_grouped.modelYearID
    AND outputAVFT.modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
    AND outputAVFT.sourceTypeID = sourceTypeIN;

    UPDATE outputAVFT 
    SET fuelEngFraction = fuelEngFraction / NotKnownSum * (1 - KnownSum)
    WHERE modelYearID BETWEEN baseYearIN+1 AND analysisYearIN
    AND sourceTypeID = sourceTypeIN
    AND isKnownFraction = 'N';

    ALTER TABLE outputAVFT DROP COLUMN isKnownFraction;
    ALTER TABLE outputAVFT DROP COLUMN KnownSum;
    ALTER TABLE outputAVFT DROP COLUMN NotKnownSum;
    DROP TABLE proportional_scaling_factors;
    DROP TABLE outputAVFT_grouped;

end
EndBlock


-- ----------------------------------------------------------------
-- No projection options for motorcycles, so just handle separately
-- ----------------------------------------------------------------
BeginBlock
create procedure AVFTTool_HandleMotorcycles()
begin
    -- include all model years currently in the output AVFT table for motorcycles
    INSERT INTO outputAVFT
    SELECT DISTINCT 11 AS sourceTypeID, modelYearID, 1 AS fuelTypeID, 1 AS engTechID, 1 AS fuelEngFraction
    FROM outputAVFT;
end
EndBlock

-- -----------------
-- Order the results
-- -----------------
BeginBlock
create procedure AVFTTool_OrderResults()
begin
    ALTER TABLE outputAVFT
    ORDER BY sourceTypeID, modelYearID, fuelTypeID, engTechID;
end
EndBlock
