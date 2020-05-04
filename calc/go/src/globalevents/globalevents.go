package globalevents

import (
	"fmt"
	"sync/atomic"
)

// Added after reading the last data from the MOVESWorkerOutput file
const MWOReadEnd = "MWOReadEnd"
var MWOReadEndCount int32

// Added before and after a block is to be processed internally
const MWOBlockBegin, MWOBlockEnd = "MWOBlockBegin", "MWOBlockEnd" 
var MWOBlockBeginCount, MWOBlockEndCount int32

// Added before and after a block is to be written to the new MOVESWorkerOutput file
const MWOWriteBegin, MWOWriteEnd = "MWOWriteBegin", "MWOWriteEnd"
var MWOWriteBeginCount, MWOWriteEndCount int32

var Events chan string // buffered asynchronous channel

func init() {
	Events = make(chan string,1000)
	Events <- "EventsStarted"
}

func Send(message string) {
	if message == MWOReadEnd {
		atomic.AddInt32(&MWOReadEndCount,1)
	} else if message == MWOBlockBegin {
		atomic.AddInt32(&MWOBlockBeginCount,1)
	} else if message == MWOBlockEnd {
		atomic.AddInt32(&MWOBlockEndCount,1)
	} else if message == MWOWriteBegin {
		atomic.AddInt32(&MWOWriteBeginCount,1)
	} else if message == MWOWriteEnd {
		atomic.AddInt32(&MWOWriteEndCount,1)
	} 
	Events <- message
}

func PrintCounts() {
	fmt.Printf("MWOReadEnds: %d,MWOBlockBegins: %d,MWOBlockEnds: %d,MWOWriteBegins: %d,MWOWriteEnds: %d\n",MWOReadEndCount,MWOBlockBeginCount,MWOBlockEndCount,MWOWriteBeginCount,MWOWriteEndCount)
}

