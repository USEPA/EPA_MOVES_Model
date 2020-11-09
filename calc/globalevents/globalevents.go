// Event counters for work-in-process within the computation pipeline.
package globalevents

import (
	"fmt"
	"sync/atomic"
)

// Added after reading the last data from the MOVESWorkerOutput file
var MWOReadEndCount int32

// Added before and after a block is to be processed internally
var MWOBlockBeginCount, MWOBlockEndCount int32

// Added before and after a block is to be written to the new MOVESWorkerOutput file
var MWOWriteBeginCount, MWOWriteEndCount int32

// Added after reading the last data from the MOVESWorkerActivityOutput file
var MWOActivityReadEndCount int32

// Added before and after an activity block is to be processed internally
var MWOActivityBlockBeginCount, MWOActivityBlockEndCount int32

// Added before and after an activity block is to be written to the new MOVESWorkerOutput file
var MWOActivityWriteBeginCount, MWOActivityWriteEndCount int32

// Counts for all steps. When all counts are zero, all work is done.
var mwoReadCount, mwoInternalCount, mwoWriteCount, mwoActivityReadCount, mwoActivityInternalCount, mwoActivityWriteCount int32

// Buffered, asynchronous channel of events to be handled by the main thread.
var Events chan string

// Setup the event queue
func init() {
	Events = make(chan string,1000)
	Events <- "EventsStarted"
}

// IsDone checks the counters to see if all work has been
// completed. It returns true if there is no remaining work
// in any queue.
func IsDone() bool {
	return mwoReadCount <= 0 && mwoInternalCount <= 0 && mwoWriteCount <= 0 &&
				mwoActivityReadCount <= 0 && mwoActivityInternalCount <= 0 && mwoActivityWriteCount <= 0
}

// Ensure main will not exit until external files have been read
func SetReadingStartedJustRates() {
	atomic.AddInt32(&mwoReadCount,1) // Ensure main will not exit until the external file has been read
}

// Ensure main will not exit until external files have been read
func SetReadingStarted() {
	atomic.AddInt32(&mwoReadCount,1) // Ensure main will not exit until the external file has been read
	atomic.AddInt32(&mwoActivityReadCount,1) // Ensure main will not exit until the external activity file has been read
}

// Get retrieves the oldest string from the Events channel, waiting for a message to be available.
func Get() string {
	return <- Events
}

// Reading of an input file has completed.
func MWOReadDone() {
	atomic.AddInt32(&MWOReadEndCount,1)
	atomic.AddInt32(&mwoReadCount,-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// A block has been created to hold pollution data read from an input file.
func MWOBlockCreated() {
	atomic.AddInt32(&MWOBlockBeginCount,1)
	atomic.AddInt32(&mwoInternalCount,1)
}

// A pollution data block is no longer needed.
func MWOBlockDone() {
		atomic.AddInt32(&MWOBlockEndCount,1)
		atomic.AddInt32(&mwoInternalCount,-1)
		if IsDone() {
			Events <- "IsDone"
		}
}

// Writing to the pollution output has begun.
func MWOWriteStarted() {
	atomic.AddInt32(&MWOWriteBeginCount,1)
	atomic.AddInt32(&mwoWriteCount,1)
}

// Writing to the pollution output has completed.
func MWOWriteDone() {
	atomic.AddInt32(&MWOWriteEndCount,1)
	atomic.AddInt32(&mwoWriteCount,-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// Reading of the activity input has completed.
func MWOActivityReadDone() {
	atomic.AddInt32(&MWOActivityReadEndCount,1)
	atomic.AddInt32(&mwoActivityReadCount,-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// A block has been created to hold activity data read from an input file.
func MWOActivityBlockCreated() {
	atomic.AddInt32(&MWOActivityBlockBeginCount,1)
	atomic.AddInt32(&mwoActivityInternalCount,1)
}

// An activity data block is no longer needed.
func MWOActivityBlockDone() {
	atomic.AddInt32(&MWOActivityBlockEndCount,1)
	atomic.AddInt32(&mwoActivityInternalCount,-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// Writing to the activity output has begun.
func MWOActivityWriteStarted() {
	atomic.AddInt32(&MWOActivityWriteBeginCount,1)
	atomic.AddInt32(&mwoActivityWriteCount,1)
}

// Writing to the activity output has completed.
func MWOActivityWriteDone() {
	atomic.AddInt32(&MWOActivityWriteEndCount,1)
	atomic.AddInt32(&mwoActivityWriteCount,-1)
	if IsDone() {
		Events <- "IsDone"
	}
}

// Print collected statistics.
func PrintCounts() {
	fmt.Printf("MWOReadEnds: %d,MWOBlockBegins: %d,MWOBlockEnds: %d,MWOWriteBegins: %d,MWOWriteEnds: %d\n",MWOReadEndCount,MWOBlockBeginCount,MWOBlockEndCount,MWOWriteBeginCount,MWOWriteEndCount)
	fmt.Printf("MWOActivityReadEnds: %d,MWOActivityBlockBegins: %d,MWOActivityBlockEnds: %d,MWOActivityWriteBegins: %d,MWOActivityWriteEnds: %d\n",MWOActivityReadEndCount,MWOActivityBlockBeginCount,MWOActivityBlockEndCount,MWOActivityWriteBeginCount,MWOActivityWriteEndCount)
}
