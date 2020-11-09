// Nonroad HC speciation
package nrhcspeciation

import (
	"fmt"
	"calc/mwo"
	"calc/parse"
)

/**
 * @algorithm
 * @owner Nonroad HC Speciation Calculator
 * @calculator
**/

// Key for methane-to-THC ratio lookup
type methaneTHCRatioKey struct {
	processID, engTechID, fuelSubTypeID int
	nrHPCategory uint8
}

// Detail for methane-to-THC ratio lookup
type methaneTHCRatioDetail struct {
	CH4THCRatio float64
}

// methane-to-THC ratio lookup
var methaneTHCRatio map[methaneTHCRatioKey]*methaneTHCRatioDetail

// Key for speciation constant lookup
type NRHCSpeciationKey struct {
	pollutantID, processID, engTechID, fuelSubTypeID int
	nrHPCategory uint8
}

// Detail for speciation constant lookup
type HCSpeciationDetail struct {
	speciationConstant float64
}

// Speciation constant lookup
var HCSpeciation map[NRHCSpeciationKey]*HCSpeciationDetail

// Setup variables.
func init() {
	methaneTHCRatio = make(map[methaneTHCRatioKey]*methaneTHCRatioDetail)
	HCSpeciation = make(map[NRHCSpeciationKey]*HCSpeciationDetail)
}

// Read files to populate the lookup objects.
func StartSetup() {
	// nrMethaneTHCRatio
	parse.ReadAndParseFile("nrmethanethcratio", func(parts []string) {
		// processID,engTechID,fuelSubtypeID,nrHPCategory,CH4THCRatio
		if len(parts) < 5 {
			return
		}
		k := methaneTHCRatioKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetString(parts[3])[0]}
		v := new(methaneTHCRatioDetail)
		v.CH4THCRatio = parse.GetFloat(parts[4])
		methaneTHCRatio[k] = v
	})

	// nrHCSpeciation
	parse.ReadAndParseFile("nrhcspeciation", func(parts []string) {
		// pollutantID,processID,engTechID,fuelSubTypeID,nrHPCategory,speciationConstant
		if len(parts) < 6 {
			return
		}
		k := NRHCSpeciationKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetString(parts[4])[0]}
		v := new(HCSpeciationDetail)
		v.speciationConstant = parse.GetFloat(parts[5])
		if HCSpeciation[k] != nil {
			fmt.Println("HCSpeciation[",k,"] already populated with ",HCSpeciation[k]," instead of ",v)
		}
		HCSpeciation[k] = v
	})
}

// Complete any asynchronous operations begun in StartSetup().
func FinishSetup() {
	// There are no asynchronous reads, so nothing to do here.
	
	// Done
	fmt.Println("NRHCSpeciation finished reading setup files.")
}

// Create goroutines to perform Nonroad HC speciation calculations.
func StartCalculating(howManyThreads int, inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for i := 0; i<howManyThreads; i++ {
		go calculate(inputBlocks,outputBlocks)
	}
}

// Perform Nonroad HC speciation calculations.
func calculate(inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for {
		b := <- inputBlocks
		var newFuelBlocks []*mwo.FuelBlock // created on demand in order to save memory
		for _, fb := range b.FuelBlocks {
			// Skip any block that doesn't have a required input pollutant
			if fb.Key.PollutantID != 1 {
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
				var r *methaneTHCRatioDetail

				hpCategory := mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]
				r = methaneTHCRatio[methaneTHCRatioKey{fb.Key.ProcessID, fb.Key.EngTechID, ff.FuelSubTypeID, hpCategory}]
				if r == nil {
					fmt.Println("Unable to find nrMethaneTHCRatio entry for ",methaneTHCRatioKey{fb.Key.ProcessID, fb.Key.EngTechID, ff.FuelSubTypeID, hpCategory}," HP ",fb.Key.HPID)
					continue
				}
				emissions := make(map[int]*mwo.MWOEmission)
				ppid := 0
				if fb.Key.PollutantID == 1 && r != nil {
					// Methane (5)
					// @algorithm methane (5) = THC * CH4THCRatio
					// @input nrMethaneTHCRatio
					ppid = 5*100 + fb.Key.ProcessID
					if(mwo.NeededPolProcessIDs[ppid]) {
						emissions[5] = mwo.NewEmissionScaled(e,r.CH4THCRatio)
					}
					// NMHC (79)
					// @algorithm NMHC (79) = THC * (1-CH4THCRatio)
					// @input nrMethaneTHCRatio
					ppid = 79*100 + fb.Key.ProcessID
					if(mwo.NeededPolProcessIDs[ppid]) {
						emissions[79] = mwo.NewEmissionScaled(e,1-r.CH4THCRatio)
					}

					/*
					@algorithm NMOG (80) = NMHC*speciationConstant.
					@input nrHCSpeciation
					@input nrHPCategory
					*/
					ppid = 80*100 + fb.Key.ProcessID
					if(mwo.NeededPolProcessIDs[ppid]) {
						hcs := HCSpeciation[NRHCSpeciationKey{80,fb.Key.ProcessID,fb.Key.EngTechID,e.FuelSubTypeID,hpCategory}]
						if hcs != nil {
							emissions[80] = mwo.NewEmissionScaled(emissions[79],hcs.speciationConstant)
						} else {
							emissions[80] = mwo.NewEmissionScaled(e,0)
						}
					}
					/*
					@algorithm VOC (87) = NMHC*speciationConstant.
					@input nrHCSpeciation
					@input nrHPCategory
					*/
					ppid = 87*100 + fb.Key.ProcessID
					if(mwo.NeededPolProcessIDs[ppid]) {
						hcs := HCSpeciation[NRHCSpeciationKey{87,fb.Key.ProcessID,fb.Key.EngTechID,e.FuelSubTypeID,hpCategory}]
						if hcs != nil {
							emissions[87] = mwo.NewEmissionScaled(emissions[79],hcs.speciationConstant)
						} else {
							emissions[87] = mwo.NewEmissionScaled(e,0)
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
