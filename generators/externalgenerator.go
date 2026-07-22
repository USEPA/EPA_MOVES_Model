/*
 Package main is the entry point to the external generator.
 Command line parameter example:
 -class=MODULETORUN -db=EXECUTIONDATABASE -dbuser=USER -dbpwd=PASSWORD -params=3,14,15
 @author Wesley Faler
 @version 2016-07-25
*/
package main

import (
	"fmt"
	"runtime"
	"strings"
	"time"

	"gen/baserategenerator"
	"gen/configuration"
	"gen/globalevents"
	"gen/sourcetypephysics"
	"gen/sqlutility"
)

// main arranges for command line arguments, the reading of supporting data to memory, creates
// all channels and threads, and terminates the application when all operations have completed.
func main() {
	start := time.Now()
	var memory runtime.MemStats
	var maxMemory uint64

	fmt.Println("MOVES external generator starting...")
	fmt.Println("GOMAXPROCS=",runtime.GOMAXPROCS(0))
	fmt.Println("NumCPU=",runtime.NumCPU())

	runtime.ReadMemStats(&memory)
	if maxMemory < memory.Alloc {
		maxMemory = memory.Alloc
	}
	fmt.Printf("Memory: current=%v\n",memory.Alloc)

	configuration.Singleton.Setup()

	// Do system tests
	configuration.TestExecutionDatabase()

	// Start the SQL queue system
	sqlToWrite := make(chan string,200000)
	sqlutility.StartWriting(16,sqlToWrite)

	// Start the required generator(s).
	usesSourceTypePhysics := strings.EqualFold(configuration.Singleton.ClassName,"SourceTypePhysics.updateOperatingModeDistribution.RatesOpModeDistribution")
	usesBaseRateGenerator := strings.EqualFold(configuration.Singleton.ClassName,"BaseRateGenerator.generateBaseRates")
	if usesSourceTypePhysics {
		sourcetypephysics.UpdateOperatingModeDistribution_RatesOpModeDistribution(sqlToWrite)
	} else if usesBaseRateGenerator {
		baserategenerator.BaseRateGeneratorFromRatesOpModeDistribution(sqlToWrite)
	}

	eventCount := 0
	for {
		// Quit when there is nothing else to do
		if(globalevents.IsDone()) {
			break
		}

		globalevents.Get()
		eventCount++

		// Track memory consumption
		runtime.ReadMemStats(&memory)
		if maxMemory < memory.Alloc {
			maxMemory = memory.Alloc
		}
	}

	// Finish writing to the output
	sqlutility.FinishWriting()

	// Print statistics
	fmt.Printf("MOVES external generator done with %d events.\n",eventCount)
	globalevents.PrintCounts()

	end := time.Now()
	delta := end.Sub(start)
	fmt.Printf("MOVES external generator took this amount of time: %s\n", delta)
	fmt.Printf("Memory: max=%v\n",maxMemory)
}
