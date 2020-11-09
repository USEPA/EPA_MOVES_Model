/*
Perform Onroad Perform Air Toxics calculations.
@author Wesley Faler
@version 2016-12-08
*/
package airtoxics

import (
	"fmt"
	"os"
	"calc/parse"

	"calc/mwo"
)

/**
 * @algorithm
 * @owner Air toxics Calculator
 * @calculator
**/

// Key for minor HAP ratio lookup
type minorHAPRatioKey struct {
	processID, fuelSubTypeID, modelYearID int
}

// Detail for minor HAP ratio lookup
type minorHAPRatioDetail struct {
	outputPollutantID int
	atRatio float64
}

// minor HAP ratio lookup
var minorHAPRatio map[minorHAPRatioKey][]*minorHAPRatioDetail

// Key for PAH gas ratio lookup
type PAHGasRatioKey struct {
	processID, fuelTypeID, modelYearID int
}

// Detail for PAH gas ratio lookup
type PAHGasRatioDetail struct {
	outputPollutantID int
	atRatio float64
}

// PAH gas ratio lookup
var PAHGasRatio map[PAHGasRatioKey][]*PAHGasRatioDetail

// Key for PAH particle ratio lookup
type PAHParticleRatioKey struct {
	processID, fuelTypeID, modelYearID int
}

// Detail for PAH particle ratio lookup
type PAHParticleRatioDetail struct {
	outputPollutantID int
	atRatio float64
}

// PAH particle ratio lookup
var PAHParticleRatio map[PAHParticleRatioKey][]*PAHParticleRatioDetail

// Output of a chained connection
type chainedToDetail struct {
	outputPolProcessID, outputPollutantID, outputProcessID int
}

// ATRatioGas1ChainedTo lookup table, key is input polProcessID
var ATRatioGas1ChainedTo map[int][]*chainedToDetail

// ATRatioGas2ChainedTo lookup table, key is input polProcessID
var ATRatioGas2ChainedTo map[int][]*chainedToDetail

// ATRatioNonGasChainedTo lookup table, key is input polProcessID
var ATRatioNonGasChainedTo map[int][]*chainedToDetail

// Key for the ATRatio lookup table
type ATRatioKey struct {
	fuelFormulationID, monthID, modelYearID, outputPolProcessID int
}

// ATRatio lookup table
var ATRatio map[ATRatioKey]float64

// Key for the ATRatioGas2 lookup table
type ATRatioGas2Key struct {
	outputPolProcessID, sourceTypeID, fuelSubTypeID int
}

// ATRatioGas2 lookup table
var ATRatioGas2 map[ATRatioGas2Key]float64

// Key for the ATRatioNonGas lookup table
type ATRatioNonGasKey struct {
	outputPolProcessID, sourceTypeID, fuelSubtypeID, modelYearID int
}

// ATRatioNonGas lookup table
var ATRatioNonGas map[ATRatioNonGasKey]float64

// Initialize package-level variables, creating empty lookup tables.
func init() {
	minorHAPRatio = make(map[minorHAPRatioKey][]*minorHAPRatioDetail)
	PAHGasRatio = make(map[PAHGasRatioKey][]*PAHGasRatioDetail)
	PAHParticleRatio = make(map[PAHParticleRatioKey][]*PAHParticleRatioDetail)
	ATRatioGas1ChainedTo = make(map[int][]*chainedToDetail)
	ATRatioGas2ChainedTo = make(map[int][]*chainedToDetail)
	ATRatioNonGasChainedTo = make(map[int][]*chainedToDetail)
	ATRatio = make(map[ATRatioKey]float64)
	ATRatioGas2 = make(map[ATRatioGas2Key]float64)
	ATRatioNonGas = make(map[ATRatioNonGasKey]float64)
}

/*
Populate lookup tables and assign oxygen thresholds to the global fuel formulation list.
Reads the minorhapratiogo file to populate the minorHAPRatio lookup table.
*/
func StartSetup() {
	if _, err := os.Stat("minorhapratiogo"); err == nil {
		recordCount := 0
		parse.ReadAndParseFile("minorhapratiogo", func(parts []string) {
			// processID, outputPollutantID, fuelSubTypeID, modelYearID, atRatio
			if len(parts) < 5 {
				return
			}
			k := minorHAPRatioKey{parse.GetInt(parts[0]),parse.GetInt(parts[2]),parse.GetInt(parts[3])}
			v := new(minorHAPRatioDetail)
			v.outputPollutantID = parse.GetInt(parts[1])
			v.atRatio = parse.GetFloat(parts[4])
			list, found := minorHAPRatio[k]
			if !found {
				list = make([]*minorHAPRatioDetail,0,5)
			}
			list = append(list,v)
			minorHAPRatio[k] = list
			recordCount++
		})
		fmt.Println("airtoxics len(minorHAPRatio)=",len(minorHAPRatio),"read=",recordCount)
	}

	if _, err := os.Stat("pahgasratio"); err == nil {
		recordCount := 0
		parse.ReadAndParseFile("pahgasratio", func(parts []string) {
			// processID, outputPollutantID, fuelTypeID, modelYearID, atRatio
			if len(parts) < 5 {
				return
			}
			k := PAHGasRatioKey{parse.GetInt(parts[0]),parse.GetInt(parts[2]),parse.GetInt(parts[3])}
			v := new(PAHGasRatioDetail)
			v.outputPollutantID = parse.GetInt(parts[1])
			v.atRatio = parse.GetFloat(parts[4])
			list, found := PAHGasRatio[k]
			if !found {
				list = make([]*PAHGasRatioDetail,0,5)
			}
			list = append(list,v)
			PAHGasRatio[k] = list
			recordCount++
		})
		fmt.Println("airtoxics len(PAHGasRatio)=",len(PAHGasRatio),"read=",recordCount)
	}
	if _, err := os.Stat("pahparticleratio"); err == nil {
		recordCount := 0
		parse.ReadAndParseFile("pahparticleratio", func(parts []string) {
			// processID, outputPollutantID, fuelTypeID, modelYearID, atRatio
			if len(parts) < 5 {
				return
			}
			k := PAHParticleRatioKey{parse.GetInt(parts[0]),parse.GetInt(parts[2]),parse.GetInt(parts[3])}
			v := new(PAHParticleRatioDetail)
			v.outputPollutantID = parse.GetInt(parts[1])
			v.atRatio = parse.GetFloat(parts[4])
			list, found := PAHParticleRatio[k]
			if !found {
				list = make([]*PAHParticleRatioDetail,0,5)
			}
			list = append(list,v)
			PAHParticleRatio[k] = list
			recordCount++
		})
		fmt.Println("airtoxics len(PAHParticleRatio)=",len(PAHParticleRatio),"read=",recordCount)
	}
	readChainedToFile("atratiogas1chainedto",ATRatioGas1ChainedTo)
	readChainedToFile("atratiogas2chainedto",ATRatioGas2ChainedTo)
	readChainedToFile("atrationongaschainedto",ATRatioNonGasChainedTo)

	if _, err := os.Stat("atratio"); err == nil {
		recordCount := 0
		parse.ReadAndParseFile("atratio", func(parts []string) {
			// fuelTypeID,fuelFormulationID,
			// polProcessID,minModelYearID,maxModelYearID,
			// ageID,monthID,atRatio,modelYearID
			if len(parts) < 9 {
				return
			}
			// fuelFormulationID, monthID, modelYearID, outputPolProcessID
			k := ATRatioKey{parse.GetInt(parts[1]),parse.GetInt(parts[6]),parse.GetInt(parts[8]),parse.GetInt(parts[2])}
			ATRatio[k] = parse.GetFloat(parts[7])
			recordCount++
		})
		fmt.Println("airtoxics len(ATRatio)=",len(ATRatio),"read=",recordCount)
	}
	if _, err := os.Stat("atratiogas2"); err == nil {
		recordCount := 0
		parse.ReadAndParseFile("atratiogas2", func(parts []string) {
			// polProcessID, sourceTypeID, fuelSubTypeID, atRatio, atRatioCV
			if len(parts) < 5 {
				return
			}
			// outputPolProcessID, sourceTypeID, fuelSubTypeID
			k := ATRatioGas2Key{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2])}
			ATRatioGas2[k] = parse.GetFloat(parts[3])
			recordCount++
		})
		fmt.Println("airtoxics len(ATRatioGas2)=",len(ATRatioGas2),"read=",recordCount)
	}
	if _, err := os.Stat("atrationongas"); err == nil {
		recordCount := 0
		parse.ReadAndParseFile("atrationongas", func(parts []string) {
			// polProcessID, sourceTypeID, fuelSubtypeID, modelYearID, ATRatio
			if len(parts) < 5 {
				return
			}
			// outputPolProcessID, sourceTypeID, fuelSubtypeID, modelYearID
			k := ATRatioNonGasKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3])}
			ATRatioNonGas[k] = parse.GetFloat(parts[4])
			recordCount++
		})
		fmt.Println("airtoxics len(ATRatioNonGas)=",len(ATRatioNonGas),"read=",recordCount)
	}
}

// Read chaining data. All such files have the same format
// since they come from the RunSpecChainedTo table.
func readChainedToFile(fileName string, lookup map[int][]*chainedToDetail) {
	if _, err := os.Stat(fileName); err == nil {
		recordCount := 0
		parse.ReadAndParseFile(fileName, func(parts []string) {
			// outputPolProcessID,outputPollutantID,outputProcessID,inputPolProcessID,inputPollutantID,inputProcessID
			if len(parts) < 6 {
				return
			}
			k := parse.GetInt(parts[3])
			v := new(chainedToDetail)
			v.outputPolProcessID = parse.GetInt(parts[0])
			v.outputPollutantID = parse.GetInt(parts[1])
			v.outputProcessID = parse.GetInt(parts[2])
			list, found := lookup[k]
			if !found {
				list = make([]*chainedToDetail,0,5)
			}
			list = append(list,v)
			lookup[k] = list
			recordCount++
		})
		fmt.Println("airtoxics len(",fileName,")=",len(lookup),"read=",recordCount)
	}
}

// Complete any pending asychronous operations initiated during StartSetup().
func FinishSetup() {
	// There are no asynchronous reads, so nothing to do here.

	// Done
	fmt.Println("AirToxics finished reading setup files.")
}

// Launch computation threads. Each thread reads from the inputBlocks channel and writes to the outputBlocks channel.
func StartCalculating(howManyThreads int, inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	for i := 0; i<howManyThreads; i++ {
		go calculate(inputBlocks,outputBlocks)
	}
}

// Add a scaled emission to a fuel block, creating a new fuel block as needed for
// the output pollutant/process combination.
func addEmission(newFuelBlocks map[int]*mwo.FuelBlock, fb *mwo.FuelBlock, outputProcessID, outputPollutantID int, ratio float64, e *mwo.MWOEmission) {
	outputPolProcessID := outputPollutantID*100 + outputProcessID
	nfb := newFuelBlocks[outputPolProcessID]
	if nfb == nil {
		nfb = mwo.NewFuelBlock(fb)
		nfb.Key.PollutantID = outputPollutantID
		nfb.Key.PolProcessID = outputPolProcessID
		newFuelBlocks[nfb.Key.PolProcessID] = nfb
	}
	nfb.AddEmission(mwo.NewEmissionScaled(e,ratio))
}

// Scale all emissions, creating a new fuel block as needed for
// the output pollutant/process combination.
func addEmissions(newFuelBlocks map[int]*mwo.FuelBlock, fb *mwo.FuelBlock, outputProcessID, outputPollutantID int, ratio float64) {
	for _, e := range fb.Emissions { // For every fuel formulation's emissions...
		addEmission(newFuelBlocks,fb,outputProcessID,outputPollutantID,ratio,e)
	}
}

// Perform air toxics calculations. Read from the inputBlocks channel. Write to the outputBlocks channel.
// Process until the inputBlocks channel is closed then quit.
func calculate(inputBlocks chan *mwo.MWOBlock, outputBlocks chan *mwo.MWOBlock) {
	useMinorHAPRatio := len(minorHAPRatio) > 0 && mwo.NeedsModule("ATC_UseMinorHAPRatio")
	usePAHGasRatio := len(PAHGasRatio) > 0 && mwo.NeedsModule("ATC_UsePAHGasRatio")
	usePAHParticleRatio := len(PAHParticleRatio) > 0 && mwo.NeedsModule("ATC_UsePAHParticleRatio")
	useATRatioGas1 := len(ATRatio) > 0 && mwo.NeedsModule("ATC_UseATRatioGas1")
	useATRatioGas2 := len(ATRatioGas2) > 0 && mwo.NeedsModule("ATC_UseATRatioGas2")
	useATRatioNonGas := len(ATRatioNonGas) > 0 && mwo.NeedsModule("ATC_UseATRatioNonGas")

	for {
		b := <- inputBlocks

		fuelBlocksToAdd := make([]*mwo.FuelBlock,0,10)

		for _, fb := range b.FuelBlocks {
			newFuelBlocks := make(map[int]*mwo.FuelBlock) // keyed by polProcessID
			// Apply Minor HAP ratio
			if fb.Key.PollutantID == 87 && useMinorHAPRatio {
				for _, e := range fb.Emissions { // For every fuel formulation's emissions...
					k := minorHAPRatioKey{fb.Key.ProcessID,e.FuelSubTypeID,fb.Key.ModelYearID}
					ratioList, found := minorHAPRatio[k]
					if found {
						for _, d := range ratioList {
							// @algorithm minor HAP emissions[outputPollutantID] = VOC (87) * ATRatio
							addEmission(newFuelBlocks,fb,fb.Key.ProcessID,d.outputPollutantID,d.atRatio,e)
						}
					}
				}
			}
			// Apply PAH Gas ratio
			if fb.Key.PollutantID == 87 && usePAHGasRatio {
				k := PAHGasRatioKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.ModelYearID}
				ratioList, found := PAHGasRatio[k]
				if found {
					for _, d := range ratioList {
						// @algorithm PAH gas emissions[outputPollutantID] = VOC (87) * ATRatio
						addEmissions(newFuelBlocks,fb,fb.Key.ProcessID,d.outputPollutantID,d.atRatio)
					}
				}
			}
			// Apply PAH Particle ratio
			if fb.Key.PollutantID == 111 && usePAHParticleRatio {
				k := PAHParticleRatioKey{fb.Key.ProcessID,fb.Key.FuelTypeID,fb.Key.ModelYearID}
				ratioList, found := PAHParticleRatio[k]
				if found {
					for _, d := range ratioList {
						// @algorithm PAH particle emissions[outputPollutantID] = Organic Carbon (111) * ATRatio
						addEmissions(newFuelBlocks,fb,fb.Key.ProcessID,d.outputPollutantID,d.atRatio)
					}
				}
			}
			// Apply ATRatio
			if useATRatioGas1 {
				toMake, found := ATRatioGas1ChainedTo[fb.Key.PolProcessID]
				if found {
					for _, c := range toMake {
						var nfb *mwo.FuelBlock
						for _, e := range fb.Emissions { // For every fuel formulation's emissions...
							ratio, found := ATRatio[ATRatioKey{e.FuelFormulationID,fb.Key.MonthID,fb.Key.ModelYearID,c.outputPolProcessID}]
							if found {
								if nfb == nil {
									nfb = newFuelBlocks[c.outputPolProcessID]
									if nfb == nil {
										nfb = mwo.NewFuelBlock(fb)
										nfb.Key.ProcessID = c.outputProcessID
										nfb.Key.PollutantID = c.outputPollutantID
										nfb.Key.PolProcessID = c.outputPolProcessID
										newFuelBlocks[nfb.Key.PolProcessID] = nfb
									}
								}
								// @algorithm emissions[outputPollutantID] = emissions[inputPollutantID] * ATRatio
								// @input ATRatio
								nfb.AddEmission(mwo.NewEmissionScaled(e,ratio))
							}
						}
					}
				}
			}
			// Apply ATRatioGas2
			if useATRatioGas2 {
				toMake, found := ATRatioGas2ChainedTo[fb.Key.PolProcessID]
				if found {
					for _, c := range toMake {
						var nfb *mwo.FuelBlock
						for _, e := range fb.Emissions { // For every fuel formulation's emissions...
							ratio, found := ATRatioGas2[ATRatioGas2Key{c.outputPolProcessID,fb.Key.SourceTypeID,e.FuelSubTypeID}]
							if found {
								if nfb == nil {
									nfb = newFuelBlocks[c.outputPolProcessID]
									if nfb == nil {
										nfb = mwo.NewFuelBlock(fb)
										nfb.Key.ProcessID = c.outputProcessID
										nfb.Key.PollutantID = c.outputPollutantID
										nfb.Key.PolProcessID = c.outputPolProcessID
										newFuelBlocks[nfb.Key.PolProcessID] = nfb
									}
								}
								// @algorithm emissions[outputPollutantID] = emissions[inputPollutantID] * ATRatio
								// @input ATRatioGas2
								nfb.AddEmission(mwo.NewEmissionScaled(e,ratio))
							}
						}
					}
				}
			}
			// Apply ATRatioNonGas
			if useATRatioNonGas {
				toMake, found := ATRatioNonGasChainedTo[fb.Key.PolProcessID]
				if found {
					for _, c := range toMake {
						var nfb *mwo.FuelBlock
						for _, e := range fb.Emissions { // For every fuel formulation's emissions...
							ratio, found := ATRatioNonGas[ATRatioNonGasKey{c.outputPolProcessID,fb.Key.SourceTypeID,e.FuelSubTypeID,fb.Key.ModelYearID}]
							if found {
								if nfb == nil {
									nfb = newFuelBlocks[c.outputPolProcessID]
									if nfb == nil {
										nfb = mwo.NewFuelBlock(fb)
										nfb.Key.ProcessID = c.outputProcessID
										nfb.Key.PollutantID = c.outputPollutantID
										nfb.Key.PolProcessID = c.outputPolProcessID
										newFuelBlocks[nfb.Key.PolProcessID] = nfb
									}
								}
								// @algorithm emissions[outputPollutantID] = emissions[inputPollutantID] * ATRatio
								// @input ATRatioNonGas
								nfb.AddEmission(mwo.NewEmissionScaled(e,ratio))
							}
						}
					}
				}
			}
			if len(newFuelBlocks) > 0 {
				for _, nfb := range newFuelBlocks {
					if nfb.NeedsGFRE {
						// Apply any GeneralFuelRatio[Expression] at the fuel formulation level
						// No GFRE is used by the AirToxics calculator.
					}
					fuelBlocksToAdd = append(fuelBlocksToAdd,nfb)
				}
			}
		}
		// Add new fuel blocks to the main block
		for _, nfb := range fuelBlocksToAdd {
			b.AddFuelBlock(nfb)
		}

		outputBlocks <- b
	}
}
