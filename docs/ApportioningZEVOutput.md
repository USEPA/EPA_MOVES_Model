# Apportioning MOVES HD ZEV energy consumption, activity, and emissions between BEVs and FCEVs

MOVES models both battery electric vehicles (BEVs) and fuel cell electric vehicles (FCEVs) for heavy-duty applications.[^apportioningevoutput-1] Together, we refer to them as heavy-duty (HD) zero-emission vehicles (ZEVs). BEVs and FCEVs are modeled separately with their own sales fractions and emissions rates but MOVES aggregates them together in its output under the electricity fuel type (fuelTypeID 9).

[^apportioningevoutput-1]: MOVES does not model light-duty fuel cell vehicles, so light-duty EV output is not discussed in this document.

For some analyses, it may be helpful to have HD ZEV energy consumption, emissions, and activity estimates separated by engine technology (BEV vs FCEV). This information can be generated from MOVES if MOVES output is post-processed according to the instructions in this document.

# 1. Generate MOVES Output

MOVES output must be at the appropriate level of detail to make the apportionment of ZEV output possible. While it is acceptable for MOVES output to be more detailed, it must be distinguished by at least source type, regulatory class, and model year. This is the level of detail at which MOVES models ZEV adoption.

# 2. Calculate ZEV Technology Fractions

Apportioning ZEV output requires calculating the proportion of ZEVs that are BEVs compared to FCEVs by source type, model year, and regulatory class from MOVES inputs (referred to here as the `EVEngTechFraction`). BEVs and FCEVs are differentiated in MOVES inputs by engTechID: BEVs are associated with engTechID 30 and FCEVs are associated with engTechID 40.

If MOVES was run at Default Scale with no user-supplied fuel type distributions, the fraction of vehicles associated with each engTechID can be calculated from the default database’s SampleVehiclePopulation table. The following query assumes the MOVES default database is already selected (e.g., with a `USE` command):

``` sql
SELECT sourceTypeID,modelYearID,regClassID,engTechID,sum(stmyFraction)/ totalEVFraction as EVEngTechFraction
    FROM samplevehiclepopulation
    JOIN (SELECT sourceTypeID,modelYearID,regClassID,sum(stmyFraction) AS totalEVFraction
        FROM samplevehiclepopulation
          WHERE fuelTypeID = 9 
          GROUP BY sourceTypeID,modelYearID,regClassID
    ) t2 USING (sourceTypeID,modelYearID,regClassID)
WHERE fuelTypeID = 9 
GROUP BY sourceTypeID,modelYearID,regClassID,engTechID;
```

However, if MOVES was run at County Scale (or at Default Scale with user-supplied fuel type distributions), a different query is required. This is because user-supplied fuel type distributions are provided by source type, model year, and engine technology using the AVFT table. The AVFT table must be joined with the default database SampleVehiclePopulation table to calculate the proportion of BEVs and FCEVs by regulatory class. The following query demonstrates how to perform this calculation. Like above, this command assumes the MOVES default database already selected. It also assumes the user input database is named `input_db`. You may need to change this to match your input database name.

``` sql
SELECT sourceTypeID, regClassID, modelYearID, fuelTypeID, engTechID, coalesce(stmyFraction/totalEVFraction, 0) as EVEngTechFraction
FROM (
    SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, regClassID, fuelEngFraction * stmyFuelEngFraction AS stmyFraction
    FROM input_db.avft
    JOIN samplevehiclepopulation USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE fuelTypeID = 9
) AS svp JOIN (
    SELECT sourceTypeID, modelYearID, regClassID, SUM(fuelEngFraction * stmyFuelEngFraction) AS totalEVFraction
    FROM input_db.avft
    JOIN samplevehiclepopulation USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
    WHERE fuelTypeID = 9
    GROUP BY sourceTypeID, regClassID, modelYearID
) AS svp_total USING (sourceTypeID, regClassID, modelYearID);
```

# 3. Use the ZEV Technology Fractions to Allocate ZEV Activity, Emissions, and Energy

Using the fraction of vehicles associated with each engTechID, calculate the splitting proportion for each engTechID with the following equation:

``` math
Proportion_{engTechID}=\frac{EVEngTechFraction_{engTechID}}{EVEngTechFraction_{BEV}+FCEVFactor*EVEngTechFraction_{FCEV}}
```

In the equation, the `FCEVFactor` refers to the relative weight given to FECVs, relative to BEVs. It varies by activity and emissions process. For example, FCEVs consume 1.25 times more energy than a BEV on average, so the `FCEVFactor` is 1.25 for running energy consumption.[^apportioningevoutput-2] In cases where the `FCEVFactor` is 1.0, the equation simplifies such that MOVES output can be apportioned using the sales proportion directly. The following table shows the `FCEVFactor` for each MOVES activity and emissions process modeled for ZEVs.

[^apportioningevoutput-2]: Currently, MOVES has a single `FCEVFactor` for running energy consumption for all model years and regulatory classes. Future `FCEVFactors` may vary by regulatory class and/or model year. In this case, the approach outlined in the equation above will generalize to greater levels of detail

| Activity/Emissions Output     | `FCEVFactor` |
|-------------------------------|--------------|
| All activity                  | 1            |
| Brake and tire wear emissions | 1            |
| Running Energy Consumption    | 1.25         |
| Hotelling Energy Consumption  | 1            |

Finally, multiply the MOVES output for ZEVs (fuelTypeID 9) by the ProportionengTechID for each engine technology to apportion between BEVs and FCEVs. Once these calculations are complete at the required level of detail (source type, regulatory class, model year, and activity type or emission process), the results may be further aggregated if desired.

If you are doing work which requires apportioning HD ZEV output by engine technology, we recommend reviewing how BEV and FCEV energy consumption rates are related in the Greenhouse Gas and Energy Consumption technical report. If you have further questions or clarifications on how to apportion EV output, please post a Github issue or reach out to the MOVES inbox.
