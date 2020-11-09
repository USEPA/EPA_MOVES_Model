// Package globalevents tracks the completeness of the system
// as data moves throughout multiple threads and queues.
// @author Wesley Faler
// @version 2016-03-04
package globalevents

import (
	"fmt"
	"sync/atomic"
)

// Channel of event strings
var Events chan string // buffered asynchronous channel

var GeneratorBusyCount int32
var PendingSqlCount int32
var TotalSqlCount int64
var MaxPendingSqlCount int32

// init creates the Events channel and initializes it with 
// the message "EventsStarted" to alert the worker goroutines that the
// channel is active.
func init() {
	Events = make(chan string,25000)
	Events <- "EventsStarted"
}

// IsDone checks the counters to see if all work has been
// completed. It returns true if there is no remaining work
// in any queue.
func IsDone() bool {
	return 0 == GeneratorBusyCount && 0 == PendingSqlCount
}

// max returns the maximum of two integer values.
func max(a, b int32) int32 {
	if a > b {
		return a
	}
	return b
}

// GeneratorStarting records the fact that a generator has begun.
func GeneratorStarting() {
	atomic.AddInt32(&GeneratorBusyCount,1)
}

// GeneratorDone records the fact that a generator has completed.
// When there is no more work, the Events channel is populated.
func GeneratorDone() {
	atomic.AddInt32(&GeneratorBusyCount,-1)
	if IsDone() {
		Send("GeneratorDone")
	}
}

// SqlStarting records the fact that a SQL statement has been queued.
func SqlStarting() {
	atomic.AddInt32(&PendingSqlCount,1)
	atomic.AddInt64(&TotalSqlCount,1)
	MaxPendingSqlCount = max(MaxPendingSqlCount,PendingSqlCount)
}

// SqlDone records the fact that a SQL statement has completed.
// When there is no more work, the Events channel is populated.
func SqlDone() {
	atomic.AddInt32(&PendingSqlCount,-1)
	if IsDone() {
		Send("SqlDone")
	}
}
// Send adds a string to the Events channel
func Send(message string) {
	Events <- message
}

// Get retrieves the oldest string from the Events channel, waiting for a message to be available.
func Get() string {
	return <- Events
}

// PrintCounts outputs the event counts and maximum concurrent counts to the console.
func PrintCounts() {
	fmt.Printf("GeneratorBusyCount: %d\n",GeneratorBusyCount)
	fmt.Printf("PendingSqlCount: %d, TotalSqlCount: %d, MaxPendingSqlCount: %d\n",PendingSqlCount, TotalSqlCount, MaxPendingSqlCount)
}
