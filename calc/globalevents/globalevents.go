// Event counters for work-in-process within the computation pipeline.
package globalevents

import (
	"fmt"
	"sync/atomic"
)

// Added after reading the last data from the MOVESWorkerOutput file
var MWOReadEndCount *atomic.Int64

// Added before and after a block is to be processed internally
var MWOBlockBeginCount, MWOBlockEndCount *atomic.Int64

// Added before and after a block is to be written to the new MOVESWorkerOutput file
var MWOWriteBeginCount, MWOWriteEndCount *atomic.Int64

// Added after reading the last data from the MOVESWorkerActivityOutput file
var MWOActivityReadEndCount *atomic.Int64

// Added before and after an activity block is to be processed internally
var MWOActivityBlockBeginCount, MWOActivityBlockEndCount *atomic.Int64

// Added before and after an activity block is to be written to the new MOVESWorkerOutput file
var MWOActivityWriteBeginCount, MWOActivityWriteEndCount *atomic.Int64

// Counts for all steps. When all counts are zero, all work is done.
var mwoReadCount, mwoInternalCount, mwoWriteCount, mwoActivityReadCount, mwoActivityInternalCount, mwoActivityWriteCount *atomic.Int64

// Buffered, asynchronous channel of events to be handled by the main thread.
var Events chan string

// Setup the event queue and initialize atomic counters
func init() {
	MWOReadEndCount = &atomic.Int64{}
	MWOBlockBeginCount = &atomic.Int64{}
	MWOBlockEndCount = &atomic.Int64{}
	MWOWriteBeginCount = &atomic.Int64{}
	MWOWriteEndCount = &atomic.Int64{}
	MWOActivityReadEndCount = &atomic.Int64{}
	MWOActivityBlockBeginCount = &atomic.Int64{}
	MWOActivityBlockEndCount = &atomic.Int64{}
	MWOActivityWriteBeginCount = &atomic.Int64{}
	MWOActivityWriteEndCount = &atomic.Int64{}
	mwoReadCount = &atomic.Int64{}
	mwoInternalCount = &atomic.Int64{}
	mwoWriteCount = &atomic.Int64{}
	mwoActivityReadCount = &atomic.Int64{}
	mwoActivityInternalCount = &atomic.Int64{}
	mwoActivityWriteCount = &atomic.Int64{}

	Events = make(chan string, 1000)
	Events <- "EventsStarted"
}

// IsDone checks the counters to see if all work has been
// completed. It returns true if there is no remaining work
// in any queue.
func IsDone() bool {
	return mwoReadCount.Load() <= 0 && mwoInternalCount.Load() <= 0 && mwoWriteCount.Load() <= 0 &&
		mwoActivityReadCount.Load() <= 0 && mwoActivityInternalCount.Load() <= 0 && mwoActivityWriteCount.Load() <= 0
}

// Ensure main will not exit until external files have been read
func SetReadingStartedJustRates() {
	mwoReadCount.Add(1) // Ensure main will not exit until the external file has been read
}

// Ensure main will not exit until external files have been read
func SetReadingStarted() {
	mwoReadCount.Add(1)         // Ensure main will not exit until the external file has been read
	mwoActivityReadCount.Add(1) // Ensure main will not exit until the external activity file has been read
}

// Get retrieves the oldest string from the Events channel, waiting for a message to be available.
func Get() string {
	return <-Events
}

// Reading of an input file has completed.
func MWOReadDone() {
	MWOReadEndCount.Add(1)
	mwoReadCount.Add(-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// A block has been created to hold pollution data read from an input file.
func MWOBlockCreated() {
	MWOBlockBeginCount.Add(1)
	mwoInternalCount.Add(1)
}

// A pollution data block is no longer needed.
func MWOBlockDone() {
	MWOBlockEndCount.Add(1)
	mwoInternalCount.Add(-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// Writing to the pollution output has begun.
func MWOWriteStarted() {
	MWOWriteBeginCount.Add(1)
	mwoWriteCount.Add(1)
}

// Writing to the pollution output has completed.
func MWOWriteDone() {
	MWOWriteEndCount.Add(1)
	mwoWriteCount.Add(-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// Reading of the activity input has completed.
func MWOActivityReadDone() {
	MWOActivityReadEndCount.Add(1)
	mwoActivityReadCount.Add(-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// A block has been created to hold activity data read from an input file.
func MWOActivityBlockCreated() {
	MWOActivityBlockBeginCount.Add(1)
	mwoActivityInternalCount.Add(1)
}

// An activity data block is no longer needed.
func MWOActivityBlockDone() {
	MWOActivityBlockEndCount.Add(1)
	mwoActivityInternalCount.Add(-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// Writing to the activity output has begun.
func MWOActivityWriteStarted() {
	MWOActivityWriteBeginCount.Add(1)
	mwoActivityWriteCount.Add(1)
}

// Writing to the activity output has completed.
func MWOActivityWriteDone() {
	MWOActivityWriteEndCount.Add(1)
	mwoActivityWriteCount.Add(-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// Print collected statistics.
func PrintCounts() {
	fmt.Printf("MWOReadEnds: %d,MWOBlockBegins: %d,MWOBlockEnds: %d,MWOWriteBegins: %d,MWOWriteEnds: %d\n", MWOReadEndCount, MWOBlockBeginCount, MWOBlockEndCount, MWOWriteBeginCount, MWOWriteEndCount)
	fmt.Printf("MWOActivityReadEnds: %d,MWOActivityBlockBegins: %d,MWOActivityBlockEnds: %d,MWOActivityWriteBegins: %d,MWOActivityWriteEnds: %d\n", MWOActivityReadEndCount, MWOActivityBlockBeginCount, MWOActivityBlockEndCount, MWOActivityWriteBeginCount, MWOActivityWriteEndCount)
}
