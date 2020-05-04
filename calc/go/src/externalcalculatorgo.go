package main

import (
	"fmt"
	"configuration"
	"time"
	"globalevents"
	"mwo"
	"hcspeciation"
	"nrhcspeciation"
	"nrairtoxics"
	"runtime"
)

func main() {
	start := time.Now()
	var memory runtime.MemStats
	var maxMemory uint64

	fmt.Println("ExternalCalculatorGo starting...")
	configuration.Singleton.Setup()
	
	mwo.ReadModules()
	
	mwoReadCount := 0

	mwoInternalCount := 0
	maxMWOInternalCount := 0
	
	mwoWriteCount := 0
	maxMWOWriteCount := 0
	
	mwoReadCount++ // Ensure main will not exit until the external file has been read
	
	blockMemoryScaleFactor := 1.0

	needsDetailOutput := mwo.NeedsModule("outputfulldetail")
	fmt.Println("needsDetailOutput",needsDetailOutput)

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
	
	inputBlocks := make(chan *mwo.MWOBlock,2000)
	mwo.StartReading("movesworkeroutput",inputBlocks)
	writeBlocks := make(chan *mwo.MWOBlock,1000)

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

	if(needsHCSpeciation) {
		if(postCalculatorChannel == nil) {
			postCalculatorChannel = make(chan *mwo.MWOBlock,100)
		}
		hcspeciation.StartCalculating(2,preCalculatorChannel,postCalculatorChannel)
		preCalculatorChannel = postCalculatorChannel // tell the next stage to use this one's output as it's input
		postCalculatorChannel = nil // Consume the channel so the next stage makes a new output channel
	}
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

	runtime.ReadMemStats(&memory)
	if maxMemory < memory.Alloc {
		maxMemory = memory.Alloc
	}
	fmt.Printf("Memory: current=%v\n",memory.Alloc)
	
	eventCount := 0
	var event string
	for {
		event = <- globalevents.Events
		eventCount++
		//fmt.Println("Event: ",event)
		if event == globalevents.MWOReadEnd {
			mwoReadCount--
			runtime.ReadMemStats(&memory)
			if maxMemory < memory.Alloc {
				maxMemory = memory.Alloc
			}
		} else if event == globalevents.MWOBlockBegin {
			mwoInternalCount++
			if mwoInternalCount > maxMWOInternalCount {
				maxMWOInternalCount = mwoInternalCount
			}
		} else if event == globalevents.MWOBlockEnd {
			mwoInternalCount--
		} else if event == globalevents.MWOWriteBegin {
			mwoWriteCount++
			if mwoWriteCount > maxMWOWriteCount {
				maxMWOWriteCount = mwoWriteCount
			}
		} else if event == globalevents.MWOWriteEnd {
			mwoWriteCount--
		}
		if mwoReadCount <= 0 && mwoInternalCount <= 0 && mwoWriteCount <= 0 {
			runtime.ReadMemStats(&memory)
			if maxMemory < memory.Alloc {
				maxMemory = memory.Alloc
			}
			fmt.Println("End conditions detected.")
			break
		}
	}
	mwo.FinishWriting()
	fmt.Printf("ExternalCalculatorGo done with %d events.\n",eventCount)
	fmt.Printf("maxMWOInternalCount=%d, maxMWOWriteCount=%d\n",maxMWOInternalCount,maxMWOWriteCount)
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
