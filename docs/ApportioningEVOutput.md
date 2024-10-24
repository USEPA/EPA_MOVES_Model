# Apportioning MOVES EV energy consumption, activity, and emissions between battery and fuel cell electric vehicles

MOVES4 and later have the capability to model two different types of electric vehicles (EVs) for heavy-duty[^1] applications: battery electric vehicles (BEVs) and fuel cell electric vehicles (FCEVs). While they are modeled separately in MOVES and have their own sales fractions and emission rates, both engine technologies are always aggregated together under the electricity fuel type (fuelTypeID 9) in MOVES output.

For some analyses, it may be helpful to have separate energy consumption, emissions, and activity estimates for heavy-duty electric vehicles by engine technology. This information is only available if the MOVES output is post-processed. This document explains how to perform that post-processing. 

To accurately attribute energy consumption, emissions, and activity within the electricity fuel type, the MOVES output must include source type, regulatory class, and model year details. 

At default scale, for MOVES4.0.0, there is a simplifying assumption that all long-haul heavy-duty electric vehicles (that is, all single-unit and combination long-haul trucks) are FCEVs while all remaining electric vehicles, of all source types, are BEVs. If you are using the default fuel fractions, you can apportion EV output between BEVs and FCEVs based on the sourceTypeID column. Everything associated with sourceTypeIDs 53 and 62 can be assigned to FCEVs and everything else to BEVs. 

If you aren't using the default fuel fractions, the first step is to calculate the proportion of electric vehicles that are BEVs and the proportion that are FCEVs by source type, model year, and regulatory class. As a reminder, user-supplied fuel type distributions are provided by source type, model year, fuel type, and engine technology using the AVFT table, and BEVs are associated with engTechID 30 and FCEVs are associated with engTechID 40. 

Therefore, the first step requires combining your input AVFT table with the default regulatory class distributions, which are stored in the default database's SampleVehiclePopulation table. The following example query shows how to perform this calculation. Note that this command is assumed to be run with the MOVES default database already selected (e.g., with a `USE` command), and the user input database is named "input_db", which will need to be changed to match your input database name.

```sql
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

All activity, brakewear emissions, tirewear emissions, and hotelling energy consumption can be apportioned between BEVs and FCEVs using the sales proportions (i.e., EVEngTechFraction as calculated above) by source type, regulatory class, and model year. 

Apportioning energy consumption requires an additional step. In MOVES4, FCEV energy consumption is 1.25 times greater than BEVs for all vehicle types.[^2] Therefore, the proportion of running energy consumption for each engine technology can be calculated using the following equations:

```math
EnergyProportion_{BEV} = \frac{SalesProportion_{BEV}}{SalesProportion_{BEV}+1.25 \times SalesProportion_{FCEV}}
```

```math
EnergyProportion_{FCEV} = \frac{1.25 \times SalesProportion_{FCEV}}{SalesProportion_{BEV}+1.25 \times SalesProportion_{FCEV}}
```

Once the energy proportions for BEV and FCEV are calculated, simply multiply the running energy consumption associated with fuelTypeID 9 in the MOVES output by the appropriate energy proportions to calculate BEV and FCEV running energy consumption, respectively, by source type, regulatory class, and model year:

```math
EnergyConsumption_{BEV}=EnergyConsumption_{EV} \times EnergyProportion_{BEV}
```

```math
EnergyConsumption_{FCEV}=EnergyConsumption_{EV} \times EnergyProportion_{FCEV}
```

We recommend reading and understanding how BEV and FCEV energy consumption rates are related in the [Greenhouse Gas and Energy Consumption technical report](https://www.epa.gov/moves/moves-onroad-technical-reports). If you have questions or clarifications on how to apportion EV energy consumption, please post a Github issue or reach out to the MOVES inbox.

[^1]: Note that since MOVES4 cannot model light-duty FCEV, light-duty vehicles are not discussed in this document.
[^2]: The single ratio between BEV and FCEV energy consumption rates is likely to change in future versions of MOVES. In future versions, the ratio may vary by regulatory class and model year, but approach outlined in the equations above will generalize to greater levels of detail, such as by regulatory class.
