/*
externalcalculatorgo - Perform calculations for a single MOVES bundle on a MOVES worker computer.
Calculation modules include: Onroad HC speciation ("hcspeciation" package),
Nonroad HC speciation ("nrhcspeciation" package), Nonroad Air toxics ("nrairtoxics" package),
Onroad Air toxics ("airtoxics" package), and Onroad Baserate calculations ("baseratecalculator" package).
@author Wesley Faler
@version 2016-12-04
*/
package main

import (
	"fmt"
	"runtime"
	"time"

	"calc/configuration"
	"calc/globalevents"
	"calc/mwo"

	"calc/airtoxics"
	"calc/baseratecalculator"
	"calc/hcspeciation"
	"calc/nrhcspeciation"
	"calc/nrairtoxics"
)

// Setup the calculation pipeline and wait for computations to complete.
func main() {
	start := time.Now()
	var memory runtime.MemStats
	var maxMemory uint64

	fmt.Println("ExternalCalculatorGo starting...")
	configuration.Singleton.Setup()

	mwo.ReadModules()

	globalevents.SetReadingStarted() // Ensure main will not exit until the external files have been read

	blockMemoryScaleFactor := 1.0

	needsDetailOutput := mwo.NeedsModule("outputfulldetail")
	fmt.Println("needsDetailOutput",needsDetailOutput)

	needsBaseRateCalculator := mwo.NeedsModule("BaseRateCalculator")
	fmt.Println("needsBaseRateCalculator",needsBaseRateCalculator)

	needsAirToxics := mwo.NeedsModule("AirToxicsCalculator")
	fmt.Println("needsAirToxics",needsAirToxics)
	if(needsAirToxics) {
		blockMemoryScaleFactor += 1.0
	}

	needsHCSpeciation := mwo.NeedsModule("HCSpeciationCalculator")
	fmt.Println("needsHCSpeciation",needsHCSpeciation)
	if(needsHCSpeciation) {
		blockMemoryScaleFactor += 1.0
	}

	needsNRHCSpeciation := mwo.NeedsModule("NRHCSpeciationCalculator")
	fmt.Println("needsNRHCSpeciation",needsNRHCSpeciation)
	if(needsNRHCSpeciation) {
		blockMemoryScaleFactor += 1.0
	}

	needsNRAirToxics := mwo.NeedsModule("NRAirToxicsCalculator")
	fmt.Println("needsNRAirToxics",needsNRAirToxics)
	if(needsNRAirToxics) {
		blockMemoryScaleFactor += 4.0
	}

	needsNRNonHAPTOG := mwo.NeedsModule("nrNonHAPTOG") // a special case within NRAirToxicsCalculator
	fmt.Println("needsNRNonHAPTOG",needsNRNonHAPTOG)
	if(needsNRNonHAPTOG) {
		blockMemoryScaleFactor += 1.0
	}

	if(mwo.NeedsModule("FuelSubType")) {
		blockMemoryScaleFactor *= 1.25
	}
	howManyBlocksAllowed := (int)(2000 / blockMemoryScaleFactor)
	if(howManyBlocksAllowed < 100) {
		howManyBlocksAllowed = 100
	}
	fmt.Println("howManyBlocksAllowed",howManyBlocksAllowed)
	mwo.AddBlockPermissions(howManyBlocksAllowed)
	mwo.AddActivityBlockPermissions(howManyBlocksAllowed)

	inputBlocks := make(chan *mwo.MWOBlock,2000)
	inputActivityBlocks := make(chan *mwo.MWOActivityBlock,2000)
	mwo.StartReading("movesworkeroutput",inputBlocks)
	mwo.StartReadingActivity("movesworkeractivityoutput",inputActivityBlocks)
	if needsBaseRateCalculator {
		baseratecalculator.StartSetup()
		baseratecalculator.FinishSetup()
		baseratecalculator.StartCalculating(4,inputBlocks)
	}
	writeBlocks := make(chan *mwo.MWOBlock,1000)
	writeActivityBlocks := make(chan *mwo.MWOActivityBlock,1000)

	// Start all chained calculators to reading their inputs
	if(needsHCSpeciation) {
		hcspeciation.StartSetup()
	}
	if(needsNRHCSpeciation) {
		nrhcspeciation.StartSetup()
	}
	if(needsNRAirToxics) {
		nrairtoxics.StartSetup()
	}
	if(needsAirToxics) {
		airtoxics.StartSetup()
	}
	// other chained calculators should start their setup here

	// Finish all asynchronous setup
	if(needsHCSpeciation) {
		hcspeciation.FinishSetup()
	}
	if(needsNRHCSpeciation) {
		nrhcspeciation.FinishSetup()
	}
	if(needsNRAirToxics) {
		nrairtoxics.FinishSetup()
	}
	if(needsAirToxics) {
		airtoxics.FinishSetup()
	}
	// other chained calculators should finish their setup here

	runtime.ReadMemStats(&memory)
	if maxMemory < memory.Alloc {
		maxMemory = memory.Alloc
	}
	fmt.Printf("Memory: current=%v\n",memory.Alloc)

	// Add chained calculators with small non-blocking channels between each. Use 100 blocks per channel.
	// Be sure to spin up several go routines for each chained calculator.
	// Doing so will ensure that if one calculator is a bottleneck, it will at least run
	// concurrently with itself.
	preCalculatorChannel := inputBlocks
	postCalculatorChannel := make(chan *mwo.MWOBlock,100)

	preCalculatorActivityChannel := inputActivityBlocks

	// Onroad
	if(needsHCSpeciation) {
		if(postCalculatorChannel == nil) {
			postCalculatorChannel = make(chan *mwo.MWOBlock,100)
		}
		hcspeciation.StartCalculating(2,preCalculatorChannel,postCalculatorChannel)
		preCalculatorChannel = postCalculatorChannel // tell the next stage to use this one's output as it's input
		postCalculatorChannel = nil // Consume the channel so the next stage makes a new output channel
	}
	if(needsAirToxics) { // Must come after HC speciation since VOC is an input
		if(postCalculatorChannel == nil) {
			postCalculatorChannel = make(chan *mwo.MWOBlock,100)
		}
		airtoxics.StartCalculating(2,preCalculatorChannel,postCalculatorChannel)
		preCalculatorChannel = postCalculatorChannel // tell the next stage to use this one's output as it's input
		postCalculatorChannel = nil // Consume the channel so the next stage makes a new output channel
	}
	// Nonroad
	if(needsNRHCSpeciation) {
		if(postCalculatorChannel == nil) {
			postCalculatorChannel = make(chan *mwo.MWOBlock,100)
		}
		nrhcspeciation.StartCalculating(2,preCalculatorChannel,postCalculatorChannel)
		preCalculatorChannel = postCalculatorChannel // tell the next stage to use this one's output as it's input
		postCalculatorChannel = nil // Consume the channel so the next stage makes a new output channel
	}
	if(needsNRAirToxics) {
		if(postCalculatorChannel == nil) {
			postCalculatorChannel = make(chan *mwo.MWOBlock,100)
		}
		nrairtoxics.StartCalculating(2,preCalculatorChannel,postCalculatorChannel)
		preCalculatorChannel = postCalculatorChannel // tell the next stage to use this one's output as it's input
		postCalculatorChannel = nil // Consume the channel so the next stage makes a new output channel
	}
	if(needsNRNonHAPTOG) {
		if(postCalculatorChannel == nil) {
			postCalculatorChannel = make(chan *mwo.MWOBlock,100)
		}
		nrairtoxics.StartCalculatingNonHAPTOG(2,preCalculatorChannel,postCalculatorChannel)
		preCalculatorChannel = postCalculatorChannel // tell the next stage to use this one's output as it's input
		postCalculatorChannel = nil // Consume the channel so the next stage makes a new output channel
	}
	// remember to update the EndOfChain call to use the output from the last chained calculator

	// Finish the chained calculators and write blocks to disk.
	mwo.EndOfChain(preCalculatorChannel,writeBlocks) // Read from the last chained calculator's output channel, now stored in preCalculatorChannel
	mwo.StartWriting("newmovesworkeroutput",writeBlocks,needsDetailOutput)

	mwo.EndOfActivityChain(preCalculatorActivityChannel,writeActivityBlocks)  // Read from the last chained calculator's output channel, now stored in preCalculatorActivityChannel
	mwo.StartActivityWriting("newmovesworkeractivityoutput",writeActivityBlocks)

	runtime.ReadMemStats(&memory)
	if maxMemory < memory.Alloc {
		maxMemory = memory.Alloc
	}
	fmt.Printf("Memory: current=%v\n",memory.Alloc)

	eventCount := 0
	for {
		globalevents.Get()
		eventCount++
		// Quit when there is nothing else to do
		if globalevents.IsDone() {
			fmt.Println("End conditions detected.")
			runtime.ReadMemStats(&memory)
			if maxMemory < memory.Alloc {
				maxMemory = memory.Alloc
			}
			break
		}
	}
	mwo.FinishWriting()
	mwo.FinishActivityWriting()
	fmt.Printf("ExternalCalculatorGo done with %d events.\n",eventCount)
	globalevents.PrintCounts()

	runtime.ReadMemStats(&memory)
	if maxMemory < memory.Alloc {
		maxMemory = memory.Alloc
	}
	fmt.Printf("Memory: max=%v, current=%v\n",maxMemory,memory.Alloc)

	end := time.Now()
	delta := end.Sub(start)
	fmt.Printf("externalcalculatorgo took this amount of time: %s\n", delta)
}
