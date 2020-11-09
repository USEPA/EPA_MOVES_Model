/*
Perform Onroad HC speciation calculations for methane (5), NMHC (79), NMOG (80), TOG (86), and VOC (87).
Special cases are included for E10 fuel formulations.
@author Wesley Faler
@version 2017-08-26
*/
package hcspeciation

import (
	"fmt"
	"calc/mwo"
	"calc/parse"
)

/**
 * @algorithm
 * @owner HC Speciation Calculator
 * @calculator
**/

// Key for THC (1)'s mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
type THCPollutantProcessMappedModelYearKey struct {
	polProcessID, modelYearID int
}

// Data for THC (1)'s mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
type THCPollutantProcessMappedModelYearDetail struct {
	modelYearGroupID, fuelMYGroupID, IMModelYearGroupID int
}

// THC (1)'s mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
var THCPollutantProcessMappedModelYear map[THCPollutantProcessMappedModelYearKey]*THCPollutantProcessMappedModelYearDetail

// Key for the ratio of methane (5) to THC (1)
type methaneTHCRatioKey struct {
	processID, fuelSubTypeID, regClassID, modelYearID int
}

// Ratio of methane (5) to THC (1)
type methaneTHCRatioDetail struct {
	CH4THCRatio float64
}

// Lookup table of the ratio of methane (5) to THC (1)
var methaneTHCRatio map[methaneTHCRatioKey]*methaneTHCRatioDetail

// Key for general mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group, adjusted for internationalization.
type HCPollutantProcessMappedModelYearKey struct {
	polProcessID, modelYearID int
}

// Data for general mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group, adjusted for internationalization.
type HCPollutantProcessMappedModelYearDetail struct {
	modelYearGroupID, fuelMYGroupID, IMModelYearGroupID int
}

// General mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group, adjusted for internationalization.
var HCPollutantProcessMappedModelYear map[HCPollutantProcessMappedModelYearKey]*HCPollutantProcessMappedModelYearDetail

// Key for general mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
type HCPollutantProcessModelYearKey struct {
	polProcessID, modelYearID int
}

// Data for general mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
type HCPollutantProcessModelYearDetail struct {
	modelYearGroupID, fuelMYGroupID, IMModelYearGroupID int
}

// General mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
var HCPollutantProcessModelYear map[HCPollutantProcessModelYearKey]*HCPollutantProcessModelYearDetail

// Key for the HC speciation equation terms
type HCSpeciationKey struct {
	polProcessID, fuelSubTypeID, regClassID, modelYearID int
}

// HC speciation equation terms
type HCSpeciationDetail struct {
	speciationConstant, oxySpeciation float64
}

// Lookup table of the HC speciation equation terms
var HCSpeciation map[HCSpeciationKey]*HCSpeciationDetail

// Initialize package-level variables, creating empty lookup tables.
func init() {
	THCPollutantProcessMappedModelYear = make(map[THCPollutantProcessMappedModelYearKey]*THCPollutantProcessMappedModelYearDetail)
	methaneTHCRatio = make(map[methaneTHCRatioKey]*methaneTHCRatioDetail)
	HCPollutantProcessMappedModelYear = make(map[HCPollutantProcessMappedModelYearKey]*HCPollutantProcessMappedModelYearDetail)
	HCPollutantProcessModelYear = make(map[HCPollutantProcessModelYearKey]*HCPollutantProcessModelYearDetail)
	HCSpeciation = make(map[HCSpeciationKey]*HCSpeciationDetail)
}

/*
Populate lookup tables and assign oxygen thresholds to the global fuel formulation list.
Reads the hcetohbin file to set the ETOH bin ID in the global fuel formulation list.
Reads the thcpollutantprocessmappedmodelyear file to populate the THCPollutantProcessMappedModelYear lookup table.
Reads the methanethcratio file to populate the methaneTHCRatio lookup table.
Reads the hcpollutantprocessmappedmodelyear file to populate the HCPollutantProcessMappedModelYear lookup table.
Reads the hcpollutantprocessmappedmodelyear file to populate the HCPollutantProcessMappedModelYear lookup table.
Reads the hcpollutantprocessmodelyear file to populate the HCPollutantProcessModelYear lookup table.
Reads the hcspeciation file to populate the HCSpeciation lookup table.
*/
func StartSetup() {
	// THCPollutantProcessMappedModelYear
	parse.ReadAndParseFile("thcpollutantprocessmappedmodelyear", func(parts []string) {
		// polProcessID, modelYearID
		// modelYearGroupID, fuelMYGroupID, IMModelYearGroupID
		if len(parts) < 5 {
			return
		}
		k := THCPollutantProcessMappedModelYearKey{parse.GetInt(parts[0]),parse.GetInt(parts[1])}
		v := new(THCPollutantProcessMappedModelYearDetail)
		v.modelYearGroupID = parse.GetInt(parts[2])
		v.fuelMYGroupID = parse.GetInt(parts[3])
		v.IMModelYearGroupID = parse.GetInt(parts[4])
		THCPollutantProcessMappedModelYear[k] = v
	})

	// methaneTHCRatio
	parse.ReadAndParseFile("methanethcratio", func(parts []string) {
		// processID, fuelSubtypeID, regClassID, beginModelYearID, endModelYearID, CH4THCRatio, dataSourceID
		if len(parts) < 7 {
			return
		}
		beginModelYearID := parse.GetInt(parts[3])
		endModelYearID := parse.GetInt(parts[4])
		k := methaneTHCRatioKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),0}
		v := new(methaneTHCRatioDetail)
		v.CH4THCRatio = parse.GetFloat(parts[5])
		for modelYearID:=beginModelYearID;modelYearID<=endModelYearID;modelYearID++ {
			k.modelYearID = modelYearID
			methaneTHCRatio[k] = v
		}
	})

	// HCPollutantProcessMappedModelYear
	parse.ReadAndParseFile("hcpollutantprocessmappedmodelyear", func(parts []string) {
		// polProcessID, modelYearID
		// modelYearGroupID, fuelMYGroupID, IMModelYearGroupID
		if len(parts) < 5 {
			return
		}
		k := HCPollutantProcessMappedModelYearKey{parse.GetInt(parts[0]),parse.GetInt(parts[1])}
		v := new(HCPollutantProcessMappedModelYearDetail)
		v.modelYearGroupID = parse.GetInt(parts[2])
		v.fuelMYGroupID = parse.GetInt(parts[3])
		v.IMModelYearGroupID = parse.GetInt(parts[4])
		HCPollutantProcessMappedModelYear[k] = v
	})

	// HCPollutantProcessModelYear
	parse.ReadAndParseFile("hcpollutantprocessmodelyear", func(parts []string) {
		// polProcessID, modelYearID
		// modelYearGroupID, fuelMYGroupID, IMModelYearGroupID
		if len(parts) < 5 {
			return
		}
		k := HCPollutantProcessModelYearKey{parse.GetInt(parts[0]),parse.GetInt(parts[1])}
		v := new(HCPollutantProcessModelYearDetail)
		v.modelYearGroupID = parse.GetInt(parts[2])
		v.fuelMYGroupID = parse.GetInt(parts[3])
		v.IMModelYearGroupID = parse.GetInt(parts[4])
		HCPollutantProcessModelYear[k] = v
	})

	// HCSpeciation
	parse.ReadAndParseFile("hcspeciation", func(parts []string) {
		// polProcessID, fuelSubtypeID, regClassID, beginModelYearID, endModelYearID,
		// speciationConstant, oxySpeciation, dataSourceID
		if len(parts) < 8 {
			return
		}
		beginModelYearID := parse.GetInt(parts[3])
		endModelYearID := parse.GetInt(parts[4])
		k := HCSpeciationKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),0}
		v := new(HCSpeciationDetail)
		v.speciationConstant = parse.GetFloat(parts[5])
		v.oxySpeciation = parse.GetFloat(parts[6])
		for modelYearID:=beginModelYearID;modelYearID<=endModelYearID;modelYearID++ {
			k.modelYearID = modelYearID
			if HCSpeciation[k] != nil {
				fmt.Println("HCSpeciation[",k,"] already populated with ",HCSpeciation[k]," instead of ",v)
			}
			HCSpeciation[k] = v
		}
	})
}

// Complete any pending asychronous operations initiated during StartSetup().
func FinishSetup() {
	// There are no asynchronous reads, so nothing to do here.

	// Done
	fmt.Println("HCSpeciation finished reading setup files.")
}

// Launch computation threads. Each thread reads from the inputBlocks channel and writes to the outputBlocks channel.
func StartCalculating(howManyThreads int, inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for i := 0; i<howManyThreads; i++ {
		go calculate(inputBlocks,outputBlocks)
	}
}

// Perform HC speciation calculations. Read from the inputBlocks channel. Write to the outputBlocks channel.
// Process until the inputBlocks channel is closed then quit.
func calculate(inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for {
		b := <- inputBlocks
		var newFuelBlocks []*mwo.FuelBlock // created on demand in order to save memory
		for _, fb := range b.FuelBlocks {
			// Skip any block that doesn't have a required input pollutant
			if fb.Key.PollutantID != 1 && fb.Key.PollutantID != 10001 {
				continue
			}
			// Make all possible output pollutants from the input and its fuel formulation-specific emissions
			innerFuelBlocks := make(map[int]*mwo.FuelBlock)

			for _, e := range fb.Emissions { // For every fuel formulation's emissions...
				ff := mwo.FuelFormulations[e.FuelFormulationID]
				if ff == nil {
					fmt.Println("Unable to find fuel formulation ",e.FuelFormulationID)
					continue
				}
				totalOxygenate := ff.MTBEVolume + ff.ETBEVolume + ff.TAMEVolume + ff.ETOHVolume
				emissions := make(map[int]*mwo.MWOEmission)
				ppid := 0

				if fb.Key.PollutantID == 1 {
					r := methaneTHCRatio[methaneTHCRatioKey{fb.Key.ProcessID, e.FuelSubTypeID, fb.Key.RegClassID, fb.Key.ModelYearID}]
					if r != nil {
						// Methane (5)
						// @algorithm methane (5) = THC * CH4THCRatio
						// @input methaneTHCRatio
						// @input PollutantProcessModelYear
						ppid = 5*100 + fb.Key.ProcessID
						if(mwo.NeededPolProcessIDs[ppid]) {
							emissions[5] = mwo.NewEmissionScaled(e,r.CH4THCRatio)
						}
						// NMHC (79)
						// @algorithm NMHC (79) = THC * (1-CH4THCRatio)
						// @input methaneTHCRatio
						// @input PollutantProcessModelYear
						ppid = 79*100 + fb.Key.ProcessID
						if(mwo.NeededPolProcessIDs[ppid]) {
							emissions[79] = mwo.NewEmissionScaled(e,1-r.CH4THCRatio)
						}

						if !((e.FuelSubTypeID == 50 || e.FuelSubTypeID == 51 || e.FuelSubTypeID == 52) &&
								(fb.Key.ProcessID == 1 || fb.Key.ProcessID == 2) && fb.Key.FuelTypeID == 5 && fb.Key.ModelYearID >= 2001) {
							/*
							@algorithm NMOG (80) = NMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume)).
							@condition When (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0. Otherwise, NMOG = 0.
							@condition NOT (Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001).
							@input FuelFormulation
							@input HCSpeciation
							@input PollutantProcessModelYear
							*/
							ppid = 80*100 + fb.Key.ProcessID
							if(mwo.NeededPolProcessIDs[ppid]) {
								hcs := HCSpeciation[HCSpeciationKey{ppid,e.FuelSubTypeID,fb.Key.RegClassID,fb.Key.ModelYearID}]
								if hcs != nil {
									factor := hcs.speciationConstant + hcs.oxySpeciation * ff.VolToWtPercentOxy * totalOxygenate
									emissions[80] = mwo.NewEmissionScaled(emissions[79],factor)
								} else {
									emissions[80] = mwo.NewEmissionScaled(e,0)
								}
							}
							/*
							@algorithm VOC (87) = NMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume)).
							@condition When (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0. Otherwise, VOC = 0.
							@condition NOT (Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001).
							@input FuelFormulation
							@input HCSpeciation
							@input PollutantProcessModelYear
							*/
							ppid = 87*100 + fb.Key.ProcessID
							if(mwo.NeededPolProcessIDs[ppid]) {
								hcs := HCSpeciation[HCSpeciationKey{ppid,e.FuelSubTypeID,fb.Key.RegClassID,fb.Key.ModelYearID}]
								if hcs != nil {
									factor := hcs.speciationConstant + hcs.oxySpeciation * ff.VolToWtPercentOxy * totalOxygenate
									emissions[87] = mwo.NewEmissionScaled(emissions[79],factor)
								} else {
									emissions[87] = mwo.NewEmissionScaled(e,0)
								}
							}
						}
					}
				}

				/*
				@algorithm Calculate altNMHC (10079) from altTHC (10001) using E10's ratios.
				altNMHC (pollutant 10079) = altTHC (10001) * (1-CH4THCRatio[E10 fuel subtype]).
				@condition Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001.
				@input methaneTHCRatio
				@input PollutantProcessModelYear
				*/
				if (e.FuelSubTypeID == 50 || e.FuelSubTypeID == 51 || e.FuelSubTypeID == 52) && fb.Key.PollutantID == 10001 &&
						(fb.Key.ProcessID == 1 || fb.Key.ProcessID == 2) && fb.Key.FuelTypeID == 5 && fb.Key.ModelYearID >= 2001 {
					ethanolR := methaneTHCRatio[methaneTHCRatioKey{fb.Key.ProcessID, 12 /*12==E10 fuelsubtypeID*/, fb.Key.RegClassID, fb.Key.ModelYearID}]
					if ethanolR != nil {
						ppid = 79*100 + fb.Key.ProcessID
						if(mwo.NeededPolProcessIDs[ppid]) {
							emissions[10079] = mwo.NewEmissionScaled(e,1-ethanolR.CH4THCRatio)
						}

						/*
						@algorithm Calculate NMOG from altNMHC (10079) that originates from altTHC (10001). Use E10's factors even though the fuel is Ethanol.
						This is done by joining to HCSpeciation using E10's values rather than the current fuel formulation's values.
						NMOG = altNMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10)).
						@condition Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001.
						@input FuelFormulation
						@input HCSpeciation
						@input PollutantProcessModelYear
						*/
						ppid = 80*100 + fb.Key.ProcessID
						if(mwo.NeededPolProcessIDs[ppid]) {
							hcs := HCSpeciation[HCSpeciationKey{ppid,12,fb.Key.RegClassID,fb.Key.ModelYearID}]
							if hcs != nil {
								factor := hcs.speciationConstant + hcs.oxySpeciation * ff.VolToWtPercentOxy * totalOxygenate
								emissions[80] = mwo.NewEmissionScaled(emissions[10079],factor)
							} else {
								emissions[80] = mwo.NewEmissionScaled(e,0)
							}
						}
						/*
						@algorithm Calculate VOC from altNMHC (10079) that originates from altTHC (10001). Use E10's factors even though the fuel is Ethanol.
						This is done by joining to HCSpeciation using E10's values rather than the current fuel formulation's values.
						VOC = altNMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10)).
						@condition Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001.
						@input FuelFormulation
						@input HCSpeciation
						@input PollutantProcessModelYear
						*/
						ppid = 87*100 + fb.Key.ProcessID
						if(mwo.NeededPolProcessIDs[ppid]) {
							hcs := HCSpeciation[HCSpeciationKey{ppid,12,fb.Key.RegClassID,fb.Key.ModelYearID}]
							if hcs != nil {
								factor := hcs.speciationConstant + hcs.oxySpeciation * ff.VolToWtPercentOxy * totalOxygenate
								emissions[87] = mwo.NewEmissionScaled(emissions[10079],factor)
							} else {
								emissions[87] = mwo.NewEmissionScaled(e,0)
							}
						}
					}
				}

				// @algorithm TOG (86) = NMOG (80) + Methane (5)
				ppid = 86*100 + fb.Key.ProcessID
				if(mwo.NeededPolProcessIDs[ppid]) {
					tog := mwo.NewEmissionSum(emissions[80],emissions[5])
					if tog != nil {
						emissions[86] = tog
					}
				}

				for pollutantID, e := range emissions {
					if e == nil {
						continue
					}
					// Make a FuelBlock for the pollutant if one doesn't already exist.
					// Copy everything in the key and change the key's pollutantID.
					nfb := innerFuelBlocks[pollutantID]
					if nfb == nil {
						nfb = mwo.NewFuelBlock(fb)
						nfb.Key.PollutantID = pollutantID
						nfb.Key.PolProcessID = pollutantID * 100 + nfb.Key.ProcessID
						innerFuelBlocks[pollutantID] = nfb
					}
					// Add the emissions to the fuel block.
					nfb.AddEmission(e)
				}
			}
			// Add new values to the list of new fuel blocks
			for _, nfb := range innerFuelBlocks {
				if nfb.NeedsGFRE {
					// Apply any GeneralFuelRatio[Expression] at the fuel formulation level
					// No GFRE is used by the HCSpeciation calculator.
				}
				if newFuelBlocks == nil {
					newFuelBlocks = make([]*mwo.FuelBlock,0,cap(b.FuelBlocks))
				}
				newFuelBlocks = append(newFuelBlocks,nfb)
			}
		}
		// Add new fuel blocks to the main block
		if newFuelBlocks != nil {
			for _, nfb := range newFuelBlocks {
				b.AddFuelBlock(nfb)
			}
		}

		outputBlocks <- b
	}
}
