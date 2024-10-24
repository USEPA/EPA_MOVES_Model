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
-- normalize inputs so that distributions sum to 1
-- ---------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_NormalizeInputs(IN sourceTypeIN int)
begin
    CREATE TABLE inputsNormalizedAVFT
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS totalFuelEngFraction
    FROM inputavft
    WHERE inputavft.sourceTypeID = sourceTypeIN
    GROUP BY sourceTypeID, modelYearID;

    ALTER TABLE inputavft ADD COLUMN totalFuelEngFraction DOUBLE NOT NULL DEFAULT 1;
    UPDATE inputavft, inputsNormalizedAVFT
    SET inputavft.totalFuelEngFraction = inputsNormalizedAVFT.totalFuelEngFraction
    WHERE inputavft.sourceTypeID = inputsNormalizedAVFT.sourceTypeID
    AND inputavft.modelYearID = inputsNormalizedAVFT.modelYearID;

    INSERT INTO messages
    SELECT CONCAT('Warning: fuelEngFraction sums to ', ROUND(totalFuelEngFraction, 4), ' instead of 1.0000 for ', sourceTypeID, ' / ', modelYearID, '. Tool is renormalizing this fraction before filling with 0s.') AS message
    FROM inputavft
    WHERE ABS(totalFuelEngFraction - 1.0000) > 0.00001
      AND inputavft.sourceTypeID = sourceTypeIN
    GROUP BY sourceTypeID, modelYearID;

    UPDATE inputavft 
    SET fuelEngFraction = COALESCE(fuelEngFraction / totalFuelEngFraction, 0);

    ALTER TABLE inputavft DROP COLUMN totalFuelEngFraction;
    DROP TABLE inputsNormalizedAVFT;
end
EndBlock


-- --------------------------------------------------------------------------------------
-- gap fill input AVFT with defaults and renormalize the input values
-- --------------------------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_GapFilling_Defaults_Renormalize_Inputs(
    IN lastCompleteModelYearIN int,
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
    WHERE d.modelYearID BETWEEN 1950 AND lastCompleteModelYearIN AND i.fuelEngFraction IS NOT NULL
      AND d.sourceTypeID = sourceTypeIN
    UNION
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, d.fuelEngFraction, 'N' AS isUserInput
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN 1950 AND lastCompleteModelYearIN AND i.fuelEngFraction IS NULL
      AND d.sourceTypeID = sourceTypeIN;

    -- renormalize input values, preserving default values
    DROP TABLE if exists tmpSumOfDefaults;
    CREATE table tmpSumOfDefaults
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS sumOfDefaults
	    FROM inputGapFilledAVFT
	    WHERE isUserInput = 'N'
	    GROUP BY sourceTypeID, modelYearID;
    ALTER TABLE inputGapFilledAVFT ADD COLUMN sumOfDefaults DOUBLE NOT NULL DEFAULT 0;
    UPDATE inputGapFilledAVFT t, tmpSumOfDefaults t_tmp 
    SET t.sumOfDefaults = t_tmp.sumOfDefaults
    WHERE t.sourceTypeID = t_tmp.sourceTypeID AND t.modelYearID = t_tmp.modelYearID;

    DROP TABLE if exists tmpSumOfInputs;
    CREATE table tmpSumOfInputs
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS sumOfInputs
	    FROM inputGapFilledAVFT
	    WHERE isUserInput = 'Y'
	    GROUP BY sourceTypeID, modelYearID;
    ALTER TABLE inputGapFilledAVFT ADD COLUMN sumOfInputs DOUBLE NOT NULL DEFAULT 0;
    UPDATE inputGapFilledAVFT t, tmpSumOfInputs t_tmp 
    SET t.sumOfInputs = t_tmp.sumOfInputs
    WHERE t.sourceTypeID = t_tmp.sourceTypeID AND t.modelYearID = t_tmp.modelYearID;

    UPDATE inputGapFilledAVFT SET fuelEngFraction = fuelEngFraction / sumOfInputs * (1-sumOfDefaults)
    WHERE inputGapFilledAVFT.isUserInput = 'Y';

    -- clean up
    ALTER TABLE inputGapFilledAVFT DROP COLUMN isUserInput;
    ALTER TABLE inputGapFilledAVFT DROP COLUMN sumOfDefaults;
    ALTER TABLE inputGapFilledAVFT DROP COLUMN sumOfInputs;
    DROP TABLE if exists tmpSumOfDefaults;
    DROP TABLE if exists tmpSumOfInputs;

    INSERT INTO messages
    SELECT CONCAT('ERROR: source type ', sourceTypeID, ' fuel distribution does not sum to 1 for MY', modelYearID, ' after running AVFTTool_GapFilling_Defaults_Renormalize_Inputs.')
    FROM inputgapfilledavft
    GROUP BY sourceTypeID, modelYearID
    HAVING ABS(SUM(fuelEngFraction)-1.0000) > 0.00001 AND SUM(fuelEngFraction) <> 0 AND sourceTypeID = sourceTypeIN
    LIMIT 1;
end
EndBlock

-- --------------------------------------------------------------------------------------
-- gap fill input AVFT with defaults and renormalize the default values
-- --------------------------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_GapFilling_Defaults_Preserve_Inputs(
    IN lastCompleteModelYearIN int,
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
    WHERE d.modelYearID BETWEEN 1950 AND lastCompleteModelYearIN AND i.fuelEngFraction IS NOT NULL
      AND d.sourceTypeID = sourceTypeIN
    UNION
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, d.fuelEngFraction, 'N' AS isUserInput
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN 1950 AND lastCompleteModelYearIN AND i.fuelEngFraction IS NULL
      AND d.sourceTypeID = sourceTypeIN;

    -- renormalize default values, preserving input values
    DROP TABLE if exists tmpSumOfDefaults;
    CREATE table tmpSumOfDefaults
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS sumOfDefaults
	    FROM inputGapFilledAVFT
	    WHERE isUserInput = 'N'
	    GROUP BY sourceTypeID, modelYearID;
    ALTER TABLE inputGapFilledAVFT ADD COLUMN sumOfDefaults DOUBLE NOT NULL DEFAULT 0;
    UPDATE inputGapFilledAVFT t, tmpSumOfDefaults t_tmp 
    SET t.sumOfDefaults = t_tmp.sumOfDefaults
    WHERE t.sourceTypeID = t_tmp.sourceTypeID AND t.modelYearID = t_tmp.modelYearID;

    DROP TABLE if exists tmpSumOfInputs;
    CREATE table tmpSumOfInputs
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS sumOfInputs
	    FROM inputGapFilledAVFT
	    WHERE isUserInput = 'Y'
	    GROUP BY sourceTypeID, modelYearID;
    ALTER TABLE inputGapFilledAVFT ADD COLUMN sumOfInputs DOUBLE NOT NULL DEFAULT 0;
    UPDATE inputGapFilledAVFT t, tmpSumOfInputs t_tmp 
    SET t.sumOfInputs = t_tmp.sumOfInputs
    WHERE t.sourceTypeID = t_tmp.sourceTypeID AND t.modelYearID = t_tmp.modelYearID;

    UPDATE inputGapFilledAVFT SET fuelEngFraction = fuelEngFraction / sumOfDefaults * (1-sumOfInputs)
    WHERE inputGapFilledAVFT.isUserInput = 'N';

    -- clean up
    ALTER TABLE inputGapFilledAVFT DROP COLUMN isUserInput;
    ALTER TABLE inputGapFilledAVFT DROP COLUMN sumOfDefaults;
    ALTER TABLE inputGapFilledAVFT DROP COLUMN sumOfInputs;
    DROP TABLE if exists tmpSumOfDefaults;
    DROP TABLE if exists tmpSumOfInputs;

    INSERT INTO messages
    SELECT CONCAT('ERROR: source type ', sourceTypeID, ' fuel distribution does not sum to 1 for MY', modelYearID, ' after running AVFTTool_GapFilling_Defaults_Preserve_Inputs.')
    FROM inputgapfilledavft
    GROUP BY sourceTypeID, modelYearID
    HAVING ABS(SUM(fuelEngFraction)-1.0000) > 0.00001 AND SUM(fuelEngFraction) <> 0 AND sourceTypeID = sourceTypeIN
    LIMIT 1;
end
EndBlock

-- --------------------------------------------------------------------------------------
-- gap fill input AVFT with zeros. Calls `AVFTTool_NormalizeInputs` as first step.
-- --------------------------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_GapFilling_With0s(
    IN lastCompleteModelYearIN int,
    IN sourceTypeIN int,
    IN ignoreMissingMY boolean
)
begin
    CALL AVFTTool_NormalizeInputs(sourceTypeIN);

    -- make sure inputGapFilledAVFT exists as it would at the end of this function or the gap fill with defaults function
    -- (so rest of function works the same if this is the first time this procedure is called or subsequent calls)
    CREATE TABLE IF NOT EXISTS inputGapFilledAVFT (
        sourceTypeID int,
        modelYearID int,
        fuelTypeID int,
        engTechID int,
        fuelEngFraction double
    );

    -- left join the input AVFT with the default AVFT table to ensure all keys are available
    -- use values from the input table where possible, otherwise use 0s 
    INSERT INTO inputGapFilledAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, i.fuelEngFraction
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN 1950 AND lastCompleteModelYearIN AND i.fuelEngFraction IS NOT NULL
      AND d.sourceTypeID = sourceTypeIN
    UNION
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, 0 AS fuelEngFraction
    FROM defaultAVFT d
    LEFT JOIN inputAVFT i USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE d.modelYearID BETWEEN 1950 AND lastCompleteModelYearIN AND i.fuelEngFraction IS NULL
      AND d.sourceTypeID = sourceTypeIN;

    -- delete st/my groups that are all 0s if desired (when used as part of the "automatic" gap-filling method)
    IF (ignoreMissingMY) THEN
        CREATE TABLE inputGapFilledAVFT_tmp
        SELECT sourceTypeID, modelYearID, sum(fuelEngFraction) as totalFuelEngFraction
        FROM inputGapFilledAVFT
        WHERE sourceTypeID = sourceTypeIN
        GROUP BY sourceTypeID, modelYearID;

        ALTER TABLE inputGapFilledAVFT ADD COLUMN totalFuelEngFraction double;
        UPDATE inputGapFilledAVFT t, inputGapFilledAVFT_tmp t_tmp
        SET t.totalFuelEngFraction = t_tmp.totalFuelEngFraction
        WHERE t.sourceTypeID = t_tmp.sourceTypeID AND t.modelYearID = t_tmp.modelYearID;

        DELETE FROM inputGapFilledAVFT WHERE totalFuelEngFraction = 0 AND sourceTypeID = sourceTypeIN;
        ALTER TABLE inputGapFilledAVFT DROP COLUMN totalFuelEngFraction;
        DROP TABLE inputGapFilledAVFT_tmp;
    END IF;

    INSERT INTO messages
    SELECT CONCAT('ERROR: source type ', sourceTypeID, ' fuel distribution does not sum to 1 for MY', modelYearID, '. Please correct this in the input data.')
    FROM inputgapfilledavft
    GROUP BY sourceTypeID, modelYearID
    HAVING ABS(SUM(fuelEngFraction)-1.0000) > 0.00001 AND SUM(fuelEngFraction) <> 0 AND sourceTypeID = sourceTypeIN
    LIMIT 1;
    
    INSERT INTO messages
    SELECT CONCAT('ERROR: source type ', sourceTypeID, ' has no input fuel distributions for MY', modelYearID, '. Either manually fix in the input data or select a different gap-filling method.')
    FROM inputgapfilledavft
    GROUP BY sourceTypeID, modelYearID
    HAVING SUM(fuelEngFraction) = 0 AND sourceTypeID = sourceTypeIN
    LIMIT 1;
end
EndBlock

-- --------------------------------------------------------------------------------------
-- gap fill input AVFT with zeros where possible, and with defaults where that fails
-- --------------------------------------------------------------------------------------
BeginBlock
create procedure AVFTTool_GapFilling_Automatic(
    IN lastCompleteModelYearIN int,
    IN sourceTypeIN int
)
begin
    -- emit a message if this source type is completely missing from the input file. To check for this condition,
    -- left join the source type definition table with the input AVFT file, filtering and grouping for the current source
    -- type. Also filter for null model years to get one resulting row (due to the filter for one source type and the group)
    -- if there are no matching rows in the input AVFT file, or a 0 row set if there are matching rows in the input 
    -- AVFT file (due to having non-null model years).
    INSERT INTO messages
    SELECT CONCAT('Note: sourceTypeID ', sourceTypeID, ' is missing from inputs. The automatic gap-filling will use defaults for it.') AS message
    FROM ##defaultdb##.sourceusetype
    LEFT JOIN inputAVFT USING (sourceTypeID)
    WHERE sourceTypeID = sourceTypeIN AND modelYearID IS NULL
    GROUP BY sourceTypeID;

    CALL AVFTTool_GapFilling_With0s(lastCompleteModelYearIN, sourceTypeIN, 1); -- pass TRUE for ignoreMissingMY

    -- Both AVFTTool_GapFilling_With0s and AVFTTool_GapFilling_Defaults_Renormalize_Inputs read
    -- inputAVFT and populate inputGapFilledAVFT. Since we're chaining these functions together,
    -- put the output from AVFTTool_GapFilling_With0s back into inputAVFT for AVFTTool_GapFilling_Defaults_Renormalize_Inputs
    DELETE FROM inputAVFT WHERE sourceTypeID = sourceTypeIN;
    INSERT INTO inputAVFT SELECT * FROM inputGapFilledAVFT WHERE sourceTypeID = sourceTypeIN;
    DELETE FROM inputGapFilledAVFT WHERE sourceTypeID = sourceTypeIN;

    CALL AVFTTool_GapFilling_Defaults_Renormalize_Inputs(lastCompleteModelYearIN, sourceTypeIN);

end
EndBlock


-- --------------------------
-- Constant Projection Method
-- --------------------------
BeginBlock
create procedure AVFTTool_Projection_Constant(
    IN lastCompleteModelYearIN int,
    IN analysisYearIN int,
    IN sourceTypeIN int
)
begin
    CREATE TABLE IF NOT EXISTS outputAVFT LIKE ##defaultdb##.avft;

    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM inputGapFilledAVFT
    WHERE sourceTypeID = sourceTypeIN 
      AND modelYearID <= lastCompleteModelYearIN
      AND modelYearID <= analysisYearIN; -- handle case where last complete model year is after the analysis year

    INSERT INTO outputAVFT
    SELECT i.sourceTypeID, d.modelYearID, i.fuelTypeID, i.engTechID, i.fuelEngFraction
    FROM inputGapFilledAVFT i
    join defaultavft d USING (sourceTypeID, fuelTypeID, engTechID)
    WHERE i.sourceTypeID = sourceTypeIN
      AND i.modelYearID = lastCompleteModelYearIN
      AND d.modelYearID BETWEEN lastCompleteModelYearIN + 1 AND analysisYearIN;
end
EndBlock

-- --------------------------
-- National Projection Method
-- --------------------------
BeginBlock
create procedure AVFTTool_Projection_National(
    IN lastCompleteModelYearIN int,
    IN analysisYearIN int,
    IN sourceTypeIN int
)
begin
    CREATE TABLE IF NOT EXISTS outputAVFT LIKE ##defaultdb##.avft;

    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM inputGapFilledAVFT
    WHERE sourceTypeID = sourceTypeIN 
      AND modelYearID <= lastCompleteModelYearIN
      AND modelYearID <= analysisYearIN; -- handle case where last complete model year is after the analysis year

    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction
    FROM defaultAVFT
    WHERE sourceTypeID = sourceTypeIN
      AND modelYearID BETWEEN lastCompleteModelYearIN + 1 AND analysisYearIN;
end
EndBlock


-- --------------------------
-- Proportional Projection Method using ratios.
-- --------------------------
BeginBlock
create procedure AVFTTool_Projection_Proportional(
    IN lastCompleteModelYearIN int,
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
      AND modelYearID <= lastCompleteModelYearIN
      AND modelYearID <= analysisYearIN; -- handle case where last complete model year is after the analysis year
      
    -- calculate ratio of input to default data for base year and set upper boundary condition
    -- this is the "proportional" scaling factor applied to the default fractions
    CREATE TABLE proportional_scaling_factors
    SELECT d2.sourceTypeID, d2.modelYearID, d2.fuelTypeID, d2.engTechID, 
           LEAST(COALESCE(i1.fuelEngFraction/d2.fuelEngFraction, 1), @boundaryRatioLimit) AS fuelEngFractionRatio
    FROM inputGapFilledAVFT i1, defaultavft d2
    WHERE i1.modelYearID = lastCompleteModelYearIN
      AND d2.modelYearID = lastCompleteModelYearIN
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
    WHERE d.modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN;

    -- enforce minimum boundary
    UPDATE outputavft o, defaultavft d
    SET o.fuelEngFraction = d.fuelEngFraction / @boundaryRatioLimit
    WHERE o.sourceTypeID = sourceTypeIN
      AND d.sourceTypeID = sourceTypeIN
      AND o.fuelTypeID = d.fuelTypeID
      AND o.engTechID = d.engTechID
      AND o.modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
      AND o.modelYearID = d.modelYearID
      AND o.fuelEngFraction < d.fuelEngFraction / @boundaryRatioLimit;
      
    -- normalize projected data
    CREATE TABLE outputAVFT_grouped
    SELECT sourceTypeID, modelYearID, SUM(fuelEngFraction) AS totalFuelEngFraction
    FROM outputAVFT
    WHERE modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
      AND sourceTypeID = sourceTypeIN
    GROUP BY sourceTypeID, modelYearID;

    ALTER TABLE outputAVFT ADD COLUMN totalFuelEngFraction DOUBLE NOT NULL DEFAULT 1;
    UPDATE outputAVFT, outputAVFT_grouped
    SET outputAVFT.totalFuelEngFraction = outputAVFT_grouped.totalFuelEngFraction
    WHERE outputAVFT.sourceTypeID = outputAVFT_grouped.sourceTypeID
      AND outputAVFT.modelYearID = outputAVFT_grouped.modelYearID
      AND outputAVFT.modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
      AND outputAVFT.sourceTypeID = sourceTypeIN;

    UPDATE outputAVFT 
    SET fuelEngFraction = fuelEngFraction / totalFuelEngFraction
    WHERE modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
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
    IN lastCompleteModelYearIN int,
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
      AND modelYearID <= lastCompleteModelYearIN
      AND modelYearID <= analysisYearIN; -- handle case where last complete model year is after the analysis year

    -- load known fractions and label as such
    ALTER TABLE outputAVFT ADD COLUMN isKnownFraction CHAR(1) DEFAULT 'N';
    INSERT INTO outputAVFT
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, fuelEngFraction, 'Y' as isKnownFraction
    FROM knownAVFT
    WHERE sourceTypeID = sourceTypeIN 
    AND modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN;
    
    -- calculate ratio of input to default data for base year and set upper boundary condition
    -- this is the "proportional" scaling factor applied to the default fractions
    CREATE TABLE proportional_scaling_factors
    SELECT d2.sourceTypeID, d2.modelYearID, d2.fuelTypeID, d2.engTechID, 
           LEAST(COALESCE(i1.fuelEngFraction/d2.fuelEngFraction, 1), @boundaryRatioLimit) AS fuelEngFractionRatio
    FROM inputGapFilledAVFT i1, defaultavft d2
    WHERE i1.modelYearID = lastCompleteModelYearIN
      AND d2.modelYearID = lastCompleteModelYearIN
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
    WHERE d.modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN;

    -- enforce minimum boundary (only for known fractions = 'N')
    UPDATE outputavft o, defaultavft d
    SET o.fuelEngFraction = d.fuelEngFraction / @boundaryRatioLimit
    WHERE o.sourceTypeID = sourceTypeIN
      AND d.sourceTypeID = sourceTypeIN
      AND o.fuelTypeID = d.fuelTypeID
      AND o.engTechID = d.engTechID
      AND o.modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
      AND o.modelYearID = d.modelYearID
      AND o.fuelEngFraction < d.fuelEngFraction / @boundaryRatioLimit
	  AND o.isKnownFraction = 'N';

    -- normalize projected data (preserving the known fractions, and warning if the KnownSum is missing any model years, as this means the Known Fractions don't cover all model years being projected)
    CREATE TABLE outputAVFT_grouped
    SELECT sourceTypeID, modelYearID, KnownSum, NotKnownSum
    FROM (SELECT sourceTypeID, modelYearID, sum(fuelEngFraction) as KnownSum
        FROM outputAVFT
            WHERE modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
            AND sourceTypeID = sourceTypeIN
            AND isKnownFraction = 'Y'
        GROUP BY sourceTypeID, modelYearID
    ) AS t1 RIGHT JOIN (
        SELECT sourceTypeID, modelYearID, sum(fuelEngFraction) as NotKnownSum
        FROM outputAVFT
            WHERE modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
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
    AND outputAVFT.modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
    AND outputAVFT.sourceTypeID = sourceTypeIN;

    UPDATE outputAVFT 
    SET fuelEngFraction = fuelEngFraction / NotKnownSum * (1 - KnownSum)
    WHERE modelYearID BETWEEN lastCompleteModelYearIN+1 AND analysisYearIN
    AND sourceTypeID = sourceTypeIN
    AND isKnownFraction = 'N';

    ALTER TABLE outputAVFT DROP COLUMN isKnownFraction;
    ALTER TABLE outputAVFT DROP COLUMN KnownSum;
    ALTER TABLE outputAVFT DROP COLUMN NotKnownSum;
    DROP TABLE proportional_scaling_factors;
    DROP TABLE outputAVFT_grouped;

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
