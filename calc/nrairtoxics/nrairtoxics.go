// Nonroad air toxics computations.
package nrairtoxics

import (
	"fmt"
	"os"
	"calc/mwo"
	"calc/parse"
)

/**
 * @algorithm
 * @owner Nonroad Air toxics Calculator
 * @calculator
**/

// Key for Nonroad Air Toxics ratio lookup
type NRATRatioKey struct {
	processID, engTechID, fuelSubTypeID int
	nrHPCategory uint8
}

// Detail for Nonroad Air Toxics ratio lookup
type NRATRatioDetail struct {
	pollutantID, polProcessID int
	atRatio float64
}

// Nonroad Air Toxics ratio lookup
var NRATRatio map[NRATRatioKey][]*NRATRatioDetail

// ProcessIDs that should have air toxics lookups performed
var NRATRatioProcesses map[int]bool

// Key for Nonroad process, fuel, engtech lookup
type NRProcFuelEngHPKey struct {
	processID, fuelTypeID, engTechID int
	nrHPCategory uint8
}

// Detail for Nonroad process, fuel, engtech lookup
type NRProcFuelEngHPDetail struct {
	pollutantID, polProcessID int
	atRatio float64
}

// Gaseous PAH lookup
var NRPAHGasRatio map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail
// ProcessIDs for gaseous PAH lookup
var NRPAHGasRatioProcesses map[int]bool

// Particulate PAH lookup
var NRPAHParticleRatio map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail
// ProcessIDs for particulate PAH lookup
var NRPAHParticleRatioProcesses map[int]bool

// Dioxin lookup
var NRDioxinEmissionRate map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail
// ProcessIDs for dioxin lookup
var NRDioxinEmissionRateProcesses map[int]bool

// Metals lookup
var NRMetalEmissionRate map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail
// ProcessIDs for metals lookup
var NRMetalEmissionRateProcesses map[int]bool

// Keyed by pollutantID, data is true when pollutantID must be subtracted from NMOG (80) to make NonHAPTOG (88)
var NRIntegratedSpecies map[int]bool

// Initialize variables.
func init() {
	NRATRatio = make(map[NRATRatioKey][]*NRATRatioDetail)
	NRATRatioProcesses = make(map[int]bool)

	NRPAHGasRatio = make(map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail)
	NRPAHGasRatioProcesses = make(map[int]bool)

	NRPAHParticleRatio = make(map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail)
	NRPAHParticleRatioProcesses = make(map[int]bool)

	NRDioxinEmissionRate = make(map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail)
	NRDioxinEmissionRateProcesses  = make(map[int]bool)
	
	NRMetalEmissionRate  = make(map[NRProcFuelEngHPKey][]*NRProcFuelEngHPDetail)
	NRMetalEmissionRateProcesses  = make(map[int]bool)

	NRIntegratedSpecies = make(map[int]bool)
}

// Read all supporting data files.
func StartSetup() {
	// NRATRatio
	if _, err := os.Stat("nratratio"); err == nil {
		parse.ReadAndParseFile("nratratio", func(parts []string) {
			// pollutantID, processID, engTechID, fuelSubtypeID, nrHPCategory, atRatio
			if len(parts) < 6 {
				return
			}
			k := NRATRatioKey{parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetString(parts[4])[0]}
			v := new(NRATRatioDetail)
			v.pollutantID = parse.GetInt(parts[0])
			v.polProcessID = v.pollutantID * 100 + k.processID
			v.atRatio = parse.GetFloat(parts[5])
	
			values := NRATRatio[k]
			if values == nil {
				values = make([]*NRATRatioDetail,0,20)
			}
			values = append(values,v)
			NRATRatio[k] = values
			
			NRATRatioProcesses[k.processID] = true
		})
	}
	fmt.Printf("Sizes: NRATRatio: %d, NRATRatioProcesses: %d\n",len(NRATRatio),len(NRATRatioProcesses))

	// NRPAHGasRatio
	if _, err := os.Stat("nrpahgasratio"); err == nil {
		parse.ReadAndParseFile("nrpahgasratio", func(parts []string) {
			// pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, atratio
			if len(parts) < 6 {
				return
			}
			k := NRProcFuelEngHPKey{parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetString(parts[4])[0]}
			v := new(NRProcFuelEngHPDetail)
			v.pollutantID = parse.GetInt(parts[0])
			v.polProcessID = v.pollutantID * 100 + k.processID
			v.atRatio = parse.GetFloat(parts[5])

			values := NRPAHGasRatio[k]
			if values == nil {
				values = make([]*NRProcFuelEngHPDetail,0,20)
			}
			values = append(values,v)
			NRPAHGasRatio[k] = values

			NRPAHGasRatioProcesses[k.processID] = true
		})
	}
	fmt.Printf("Sizes: NRPAHGasRatio: %d, NRPAHGasRatioProcesses: %d\n",len(NRPAHGasRatio),len(NRPAHGasRatioProcesses))

	// NRPAHParticleRatio
	if _, err := os.Stat("nrpahparticleratio"); err == nil {
		parse.ReadAndParseFile("nrpahparticleratio", func(parts []string) {
			// pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, atratio
			if len(parts) < 6 {
				return
			}
			k := NRProcFuelEngHPKey{parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetString(parts[4])[0]}
			v := new(NRProcFuelEngHPDetail)
			v.pollutantID = parse.GetInt(parts[0])
			v.polProcessID = v.pollutantID * 100 + k.processID
			v.atRatio = parse.GetFloat(parts[5])

			values := NRPAHParticleRatio[k]
			if values == nil {
				values = make([]*NRProcFuelEngHPDetail,0,20)
			}
			values = append(values,v)
			NRPAHParticleRatio[k] = values

			NRPAHParticleRatioProcesses[k.processID] = true
		})
	}
	fmt.Printf("Sizes: NRPAHParticleRatio: %d, NRPAHParticleRatioProcesses: %d\n",len(NRPAHParticleRatio),len(NRPAHParticleRatioProcesses))

	// NRDioxinEmissionRate
	if _, err := os.Stat("nrdioxinemissionrate"); err == nil {
		parse.ReadAndParseFile("nrdioxinemissionrate", func(parts []string) {
			// pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, meanBaseRate
			if len(parts) < 6 {
				return
			}
			k := NRProcFuelEngHPKey{parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetString(parts[4])[0]}
			v := new(NRProcFuelEngHPDetail)
			v.pollutantID = parse.GetInt(parts[0])
			v.polProcessID = v.pollutantID * 100 + k.processID
			v.atRatio = parse.GetFloat(parts[5])

			values := NRDioxinEmissionRate[k]
			if values == nil {
				values = make([]*NRProcFuelEngHPDetail,0,20)
			}
			values = append(values,v)
			NRDioxinEmissionRate[k] = values

			NRDioxinEmissionRateProcesses[k.processID] = true
		})
	}
	fmt.Printf("Sizes: NRDioxinEmissionRate: %d, NRDioxinEmissionRateProcesses: %d\n",len(NRDioxinEmissionRate),len(NRDioxinEmissionRateProcesses))

	// NRMetalEmissionRate
	if _, err := os.Stat("nrmetalemissionrate"); err == nil {
		parse.ReadAndParseFile("nrmetalemissionrate", func(parts []string) {
			// pollutantID, processID, fuelTypeID, engTechID, nrHPCategory, meanBaseRate
			if len(parts) < 6 {
				return
			}
			k := NRProcFuelEngHPKey{parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3]),parse.GetString(parts[4])[0]}
			v := new(NRProcFuelEngHPDetail)
			v.pollutantID = parse.GetInt(parts[0])
			v.polProcessID = v.pollutantID * 100 + k.processID
			v.atRatio = parse.GetFloat(parts[5])

			values := NRMetalEmissionRate[k]
			if values == nil {
				values = make([]*NRProcFuelEngHPDetail,0,20)
			}
			values = append(values,v)
			NRMetalEmissionRate[k] = values

			NRMetalEmissionRateProcesses[k.processID] = true
		})
	}
	fmt.Printf("Sizes: NRMetalEmissionRate: %d, NRMetalEmissionRateProcesses: %d\n",len(NRMetalEmissionRate),len(NRMetalEmissionRateProcesses))

	// NRIntegratedSpecies
	if _, err := os.Stat("nrintegratedspecies"); err == nil {
		parse.ReadAndParseFile("nrintegratedspecies", func(parts []string) {
			// pollutantID
			if len(parts) < 1 {
				return
			}
			pollutantID := parse.GetInt(parts[0])
			NRIntegratedSpecies[pollutantID] = true
		})
	}
	fmt.Printf("Sizes: NRIntegratedSpecies: %d\n",len(NRIntegratedSpecies))
}

// Complete any asynchonous operations started within StartSetup().
func FinishSetup() {
	// There are no asynchronous reads, so nothing to do here.
	
	// Done
	fmt.Println("NRAirToxics finished reading setup files.")
}

// Create goroutines to perform the calculations
func StartCalculating(howManyThreads int, inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for i := 0; i<howManyThreads; i++ {
		go calculate(inputBlocks,outputBlocks)
	}
}

// Do Nonroad air toxics calculations
func calculate(inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for {
		b := <- inputBlocks
		var newFuelBlocks []*mwo.FuelBlock // created on demand in order to save memory
		for _, fb := range b.FuelBlocks {
			// Skip any block that doesn't have a required input pollutant
			if fb.Key.PollutantID != 87 && fb.Key.PollutantID != 110 && !(fb.Key.PollutantID == 99 && fb.Key.ProcessID == 1) { // Only VOC, PM2.5, or Fuel Consumption inputs are wanted
				continue
			}
			if fb.Key.PollutantID == 87 && !NRATRatioProcesses[fb.Key.ProcessID] && !NRPAHGasRatioProcesses[fb.Key.ProcessID] { // Only processes with air toxics are wanted
				//fmt.Println("PollutantID 87 skipping process",fb.Key.ProcessID)
				continue
			}
			if fb.Key.PollutantID == 110 && !NRPAHParticleRatioProcesses[fb.Key.ProcessID] { // Only processes with PAH particles are wanted
				//fmt.Println("PollutantID 110 skipping process",fb.Key.ProcessID)
				continue
			}
			if fb.Key.PollutantID == 99 && fb.Key.ProcessID == 1 { // Check the process is needed by metals and dioxins
				if !NRMetalEmissionRateProcesses[fb.Key.ProcessID] && !NRDioxinEmissionRateProcesses[fb.Key.ProcessID] {
					//fmt.Println("PollutantID 99 skipping process",fb.Key.ProcessID)
					continue
				}
			}

			// Make all possible output pollutants from the input and its fuel formulation-specific emissions
			innerFuelBlocks := make(map[int]*mwo.FuelBlock) // keyed by pollutantID, in the same process as fb.Key.ProcessID

			for _, e := range fb.Emissions { // For every fuel formulation's emissions...
				ff := mwo.FuelFormulations[e.FuelFormulationID]
				if ff == nil {
					fmt.Println("Unable to find fuel formulation ",e.FuelFormulationID)
					continue
				}
				emissions := make(map[int]*mwo.MWOEmission) // key is pollutantID

				if fb.Key.PollutantID == 87 {
					atRatios := NRATRatio[NRATRatioKey{fb.Key.ProcessID,fb.Key.EngTechID,ff.FuelSubTypeID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}]
					if atRatios != nil {
						for _, atRatioEntry := range atRatios { // For every product pollutant...
							if(mwo.NeededPolProcessIDs[atRatioEntry.polProcessID]) {
								/*
								@algorithm output[pollutantID] = VOC (87) * ATRatio
								@input nrATRatio
								@input nrHPCategory
								*/
								emissions[atRatioEntry.pollutantID] = mwo.NewEmissionScaled(e,atRatioEntry.atRatio)
							}
						}
					} else {
						// Not really an error. Missing data happens when no pollutant needing this table are selected.
						//fmt.Println("Missing NRATRatio for",NRATRatioKey{fb.Key.ProcessID,fb.Key.EngTechID,ff.FuelSubTypeID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}," HP",fb.Key.HPID," Eng",fb.Key.EngTechID)
					}
					pahGasRatios := NRPAHGasRatio[NRProcFuelEngHPKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.EngTechID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}]
					if pahGasRatios != nil {
						for _, atRatioEntry := range pahGasRatios { // For every product pollutant...
							if(mwo.NeededPolProcessIDs[atRatioEntry.polProcessID]) {
								/*
								@algorithm output[pollutantID] = VOC (87) * ATRatio
								@input nrPAHGasRatio
								@input nrHPCategory
								*/
								emissions[atRatioEntry.pollutantID] = mwo.NewEmissionScaled(e,atRatioEntry.atRatio)
							}
						}
					} else {
						// Not really an error. Missing data happens when no pollutant needing this table are selected.
						//fmt.Println("Missing NRPAHGasRatio for",NRProcFuelEngHPKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.EngTechID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}," HP",fb.Key.HPID," Eng",fb.Key.EngTechID)
					}
				}
				if fb.Key.PollutantID == 110 {
					pahParticleRatios := NRPAHParticleRatio[NRProcFuelEngHPKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.EngTechID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}]
					if pahParticleRatios != nil {
						for _, atRatioEntry := range pahParticleRatios { // For every product pollutant...
							if(mwo.NeededPolProcessIDs[atRatioEntry.polProcessID]) {
								/*
								@algorithm output[pollutantID] = PM2.5 (110) * ATRatio
								@input nrPAHParticleRatio
								@input nrHPCategory
								*/
								emissions[atRatioEntry.pollutantID] = mwo.NewEmissionScaled(e,atRatioEntry.atRatio)
							}
						}
					} else {
						// Not really an error. Missing data happens when no pollutant needing this table are selected.
						//fmt.Println("Missing NRPAHParticleRatio for",NRProcFuelEngHPKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.EngTechID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}," HP",fb.Key.HPID," Eng",fb.Key.EngTechID)
					}
				}
				if fb.Key.PollutantID == 99 && fb.Key.ProcessID == 1 { // Handle items chained to fuel consumption
					// Fuel consumption is in grams but ratios are to gallons, so convert to gallons
					gallonsFactor := 1.0/453.592
					switch fb.Key.FuelTypeID {
						case 1: // Gasoline
							// @algorithm gallons[gasoline] = grams gasoline fuel / (453.592*6.17)
							gallonsFactor /= 6.17
						case 2: // Diesel
							// @algorithm gallons[diesel(all)] = grams diesel fuel / (453.592*7.1)
							gallonsFactor /= 7.1
						case 23: // Diesel
							gallonsFactor /= 7.1
						case 24: // Diesel
							gallonsFactor /= 7.1
						case 3: // CNG
							// @algorithm gallons[CNG] = grams gasoline / (453.592*0.0061)
							gallonsFactor /= 0.0061
						case 4: // LPG
							// @algorithm gallons[LPG] = grams gasoline / (453.592*4.507)
							gallonsFactor /= 4.507
						default:
							gallonsFactor = 1
					}

					dioxinRatios := NRDioxinEmissionRate[NRProcFuelEngHPKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.EngTechID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}]
					if dioxinRatios != nil {
						for _, atRatioEntry := range dioxinRatios { // For every product pollutant...
							if(mwo.NeededPolProcessIDs[atRatioEntry.polProcessID]) {
								/*
								@algorithm dioxin output[pollutantID] = gallons[fuel type] * meanBaseRate
								@input nrDioxinEmissionRate
								@input nrHPCategory
								*/
								emissions[atRatioEntry.pollutantID] = mwo.NewEmissionScaled(e,atRatioEntry.atRatio*gallonsFactor)
							}
						}
					}

					metalRatios := NRMetalEmissionRate[NRProcFuelEngHPKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.EngTechID,mwo.NRHPCategory[mwo.NRHPCategoryKey{fb.Key.HPID,fb.Key.EngTechID}]}]
					if metalRatios != nil {
						for _, atRatioEntry := range metalRatios { // For every product pollutant...
							if(mwo.NeededPolProcessIDs[atRatioEntry.polProcessID]) {
								/*
								@algorithm metal output[pollutantID] = gallons[fuel type] * meanBaseRate
								@input nrMetalEmissionRate
								@input nrHPCategory
								*/
								emissions[atRatioEntry.pollutantID] = mwo.NewEmissionScaled(e,atRatioEntry.atRatio*gallonsFactor)
							}
						}
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

// Create goroutines to do Nonroad NonHAP TOG calculations
func StartCalculatingNonHAPTOG(howManyThreads int, inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for i := 0; i<howManyThreads; i++ {
		go calculateNonHAPTOG(inputBlocks,outputBlocks)
	}
}

// Do Nonroad NonHAP TOG calculations
func calculateNonHAPTOG(inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for {
		b := <- inputBlocks
		var newFuelBlocks []*mwo.FuelBlock // created on demand in order to save memory
		for _, fb := range b.FuelBlocks {
			ppid := 88 * 100 + fb.Key.ProcessID
			// Skip any process that doesn't need NonHAPTOG
			if !mwo.NeededPolProcessIDs[ppid] {
				continue
			}
			// Skip any pollutant that is not an integrated species or NMOG (80)
			if fb.Key.PollutantID != 80 && !NRIntegratedSpecies[fb.Key.PollutantID] {
				continue
			}

			innerFuelBlocks := make(map[int]*mwo.FuelBlock)

			for _, e := range fb.Emissions { // For every fuel formulation's emissions...
				var emissions *mwo.MWOEmission

				if fb.Key.PollutantID == 80 {
					/*
					@algorithm NonHAPTOG(88)(partial) = NMOG (80)
					*/
					emissions = mwo.NewEmissionScaled(e,1)
				} else {
					/*
					@algorithm NonHAPTOG(88)(partial) = -emissions[integratedSpeciesPollutantID]
					@input nrIntegratedSpecies
					*/
					emissions = mwo.NewEmissionScaled(e,-1)
				}

				if emissions != nil {
					// Make a FuelBlock for the pollutant if one doesn't already exist.
					// Copy everything in the key and change the key's pollutantID.
					nfb := innerFuelBlocks[88]
					if nfb == nil {
						nfb = mwo.NewFuelBlock(fb)
						nfb.Key.PollutantID = 88
						nfb.Key.PolProcessID = 88 * 100 + nfb.Key.ProcessID
						innerFuelBlocks[88] = nfb
					}
					// Add the emissions to the fuel block.
					nfb.AddEmission(emissions)
				}
			}
			// Add new values to the list of new fuel blocks
			for _, nfb := range innerFuelBlocks {
				if nfb.NeedsGFRE {
					// Apply any GeneralFuelRatio[Expression] at the fuel formulation level
					// No GFRE is used here.
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
