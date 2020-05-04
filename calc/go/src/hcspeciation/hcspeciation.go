package hcspeciation

import (
	"fmt"
	"mwo"
	"parse"
)

/**
 * @algorithm
 * @owner HC Speciation Calculator
 * @calculator
**/

type THCPollutantProcessMappedModelYearKey struct {
	polProcessID, modelYearID int
}

type THCPollutantProcessMappedModelYearDetail struct {
	modelYearGroupID, fuelMYGroupID, IMModelYearGroupID int
}

var THCPollutantProcessMappedModelYear map[THCPollutantProcessMappedModelYearKey]*THCPollutantProcessMappedModelYearDetail

type methaneTHCRatioKey struct {
	processID, fuelTypeID, sourceTypeID, modelYearGroupID, ageGroupID int
}

type methaneTHCRatioDetail struct {
	CH4THCRatio, CH4THCRatioCV float64
}

var methaneTHCRatio map[methaneTHCRatioKey]*methaneTHCRatioDetail

type HCPollutantProcessMappedModelYearKey struct {
	polProcessID, modelYearID int
}

type HCPollutantProcessMappedModelYearDetail struct {
	modelYearGroupID, fuelMYGroupID, IMModelYearGroupID int
}

var HCPollutantProcessMappedModelYear map[HCPollutantProcessMappedModelYearKey]*HCPollutantProcessMappedModelYearDetail

type HCPollutantProcessModelYearKey struct {
	polProcessID, modelYearID int
}

type HCPollutantProcessModelYearDetail struct {
	modelYearGroupID, fuelMYGroupID, IMModelYearGroupID int
}

var HCPollutantProcessModelYear map[HCPollutantProcessModelYearKey]*HCPollutantProcessModelYearDetail

type HCSpeciationKey struct {
	polProcessID, fuelMYGroupID, fuelSubTypeID, etohThreshID, oxyThreshID int
}

type HCSpeciationDetail struct {
	speciationConstant, oxySpeciation float64
}

var HCSpeciation map[HCSpeciationKey]*HCSpeciationDetail

func init() {
	THCPollutantProcessMappedModelYear = make(map[THCPollutantProcessMappedModelYearKey]*THCPollutantProcessMappedModelYearDetail)
	methaneTHCRatio = make(map[methaneTHCRatioKey]*methaneTHCRatioDetail)
	HCPollutantProcessMappedModelYear = make(map[HCPollutantProcessMappedModelYearKey]*HCPollutantProcessMappedModelYearDetail)
	HCPollutantProcessModelYear = make(map[HCPollutantProcessModelYearKey]*HCPollutantProcessModelYearDetail)
	HCSpeciation = make(map[HCSpeciationKey]*HCSpeciationDetail)
}

func StartSetup() {
	/*
	Assign oxyThreshID
	select * from movesdb20140731.oxythreshname;
	oxyThreshID|oxyThreshName                                                                     
	0          |ETOHVolume >  0.0 OR (MTBEVolume = 0.0 and ETBEVolume = 0.0 and TAMEVolume = 0.0) 
	1          |ETOHVolume <= 0.0 and (MTBEVolume > 0.0 or  ETBEVolume > 0.0 or  TAMEVolume > 0.0)
	*/
	for _, f := range mwo.FuelFormulations {
		if f.ETOHVolume > 0 || (f.MTBEVolume == 0 && f.ETBEVolume == 0 && f.TAMEVolume == 0) {
			f.OxyThreshID = 0
		} else if f.ETOHVolume <= 0 && (f.MTBEVolume > 0 || f.ETBEVolume > 0 || f.TAMEVolume > 0) {
			f.OxyThreshID = 1
		} else {
			f.OxyThreshID = 0
		}
		f.ETOHThreshID = 0
	}

	// hcETOHBin
	parse.ReadAndParseFile("hcetohbin", func(parts []string) {
		// etohThreshID etohThreshLow etohThreshHigh etohNominalValue
		if len(parts) < 4 {
			return
		}
		//fmt.Println("ETOHBin:",parts)
		etohThreshID := parse.GetInt(parts[0])
		etohThreshLow := parse.GetFloat(parts[1])
		etohThreshHigh := parse.GetFloat(parts[2])
		//etohNominalValue := parse.GetFloat(parts[3])
		
		for _, f := range mwo.FuelFormulations {
			/*
			@algorithm Assign etohThreshID to each fuel formulation.
			etohThreshLow <= ETOHVolume < etohThreshHigh
			*/
			if etohThreshLow <= f.ETOHVolume && f.ETOHVolume < etohThreshHigh {
				f.ETOHThreshID = etohThreshID
			} 
		}
	})

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
		// processID, fuelTypeID, sourceTypeID, modelYearGroupID, ageGroupID
		// CH4THCRatio, CH4THCRatioCV
		if len(parts) < 7 {
			return
		}
		k := methaneTHCRatioKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetInt(parts[4])}
		v := new(methaneTHCRatioDetail)
		v.CH4THCRatio = parse.GetFloat(parts[5])
		v.CH4THCRatioCV = parse.GetFloat(parts[6])
		methaneTHCRatio[k] = v
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
		// polProcessID, fuelMYGroupID, fuelSubTypeID, etohThreshID, oxyThreshID,
		// speciationConstant, oxySpeciation
		if len(parts) < 7 {
			return
		}
		k := HCSpeciationKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetInt(parts[4])}
		v := new(HCSpeciationDetail)
		v.speciationConstant = parse.GetFloat(parts[5])
		v.oxySpeciation = parse.GetFloat(parts[6])
		if HCSpeciation[k] != nil {
			fmt.Println("HCSpeciation[",k,"] already populated with ",HCSpeciation[k]," instead of ",v)
		}
		HCSpeciation[k] = v
	})
}

func FinishSetup() {
	// There are no asynchronous reads, so nothing to do here.
	
	// Done
	fmt.Println("HCSpeciation finished reading setup files.")
}

func StartCalculating(howManyThreads int, inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for i := 0; i<howManyThreads; i++ {
		go calculate(inputBlocks,outputBlocks)
	}
}

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
			thcPolProcessID := 1*100 + fb.Key.ProcessID
			thcPPMY := THCPollutantProcessMappedModelYear[THCPollutantProcessMappedModelYearKey{thcPolProcessID, fb.Key.ModelYearID}]
			var r *methaneTHCRatioDetail
			if thcPPMY != nil {
				r = methaneTHCRatio[methaneTHCRatioKey{fb.Key.ProcessID, fb.Key.FuelTypeID, fb.Key.SourceTypeID, thcPPMY.modelYearGroupID, fb.Key.AgeGroupID}]
			} else {
				fmt.Println("Unknown THCPollutantProcessMappedModelYear for ",THCPollutantProcessMappedModelYearKey{thcPolProcessID, fb.Key.ModelYearID})
			}

			for _, e := range fb.Emissions { // For every fuel formulation's emissions...
				ff := mwo.FuelFormulations[e.FuelFormulationID]
				if ff == nil {
					fmt.Println("Unable to find fuel formulation ",e.FuelFormulationID)
					continue
				}
				totalOxygenate := ff.MTBEVolume + ff.ETBEVolume + ff.TAMEVolume + ff.ETOHVolume					
				emissions := make(map[int]*mwo.MWOEmission)
				ppid := 0
				if fb.Key.PollutantID == 1 && r != nil {
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
							if totalOxygenate < 0 {
								emissions[80] = mwo.NewEmissionScaled(e,0)
							} else {
								ppmy := HCPollutantProcessModelYear[HCPollutantProcessModelYearKey{ppid, fb.Key.ModelYearID}]
								if ppmy != nil {
									hcs := HCSpeciation[HCSpeciationKey{ppid,ppmy.fuelMYGroupID,e.FuelSubTypeID,ff.ETOHThreshID,ff.OxyThreshID}]
									if hcs != nil {
										factor := hcs.speciationConstant + hcs.oxySpeciation * ff.VolToWtPercentOxy * totalOxygenate
										emissions[80] = mwo.NewEmissionScaled(emissions[79],factor)
									} else {
										emissions[80] = mwo.NewEmissionScaled(e,0)
									}
								}
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
							if totalOxygenate < 0 {
								emissions[87] = mwo.NewEmissionScaled(e,0)
							} else {
								ppmy := HCPollutantProcessMappedModelYear[HCPollutantProcessMappedModelYearKey{ppid, fb.Key.ModelYearID}]
								if ppmy != nil {
									hcs := HCSpeciation[HCSpeciationKey{ppid,ppmy.fuelMYGroupID,e.FuelSubTypeID,ff.ETOHThreshID,ff.OxyThreshID}]
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
				}
				
				/*
				@algorithm Calculate altNMHC (10079) from altTHC (10001) using E10's ratios.
				altNMHC (pollutant 10079) = altTHC (10001) * (1-CH4THCRatio[E10 fuel subtype]).
				@condition Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001.
				@input methaneTHCRatio
				@input PollutantProcessModelYear
				*/
				if (e.FuelSubTypeID == 50 || e.FuelSubTypeID == 51 || e.FuelSubTypeID == 52) && fb.Key.PollutantID == 10001 && 
						(fb.Key.ProcessID == 1 || fb.Key.ProcessID == 2) && fb.Key.FuelTypeID == 5 && fb.Key.ModelYearID >= 2001 && 
						thcPPMY != nil {
					ethanolR := methaneTHCRatio[methaneTHCRatioKey{fb.Key.ProcessID, 1, fb.Key.SourceTypeID, thcPPMY.modelYearGroupID, fb.Key.AgeGroupID}]
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
							if totalOxygenate < 0 {
								emissions[80] = mwo.NewEmissionScaled(e,0)
							} else {
								ppmy := HCPollutantProcessModelYear[HCPollutantProcessModelYearKey{ppid, fb.Key.ModelYearID}]
								if ppmy != nil {
									hcs := HCSpeciation[HCSpeciationKey{ppid,ppmy.fuelMYGroupID,12,3,0}]
									if hcs != nil {
										factor := hcs.speciationConstant + hcs.oxySpeciation * ff.VolToWtPercentOxy * totalOxygenate
										emissions[80] = mwo.NewEmissionScaled(emissions[10079],factor)
									} else {
										emissions[80] = mwo.NewEmissionScaled(e,0)
									}
								}
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
							if totalOxygenate < 0 {
								emissions[87] = mwo.NewEmissionScaled(e,0)
							} else {
								ppmy := HCPollutantProcessMappedModelYear[HCPollutantProcessMappedModelYearKey{ppid, fb.Key.ModelYearID}]
								if ppmy != nil {
									hcs := HCSpeciation[HCSpeciationKey{ppid,ppmy.fuelMYGroupID,12,3,0}]
									if hcs != nil {
										factor := hcs.speciationConstant + hcs.oxySpeciation * ff.VolToWtPercentOxy * totalOxygenate
										emissions[87] = mwo.NewEmissionScaled(emissions[10079],factor)
									} else {
										emissions[87] = mwo.NewEmissionScaled(e,0)
									}
								}
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
