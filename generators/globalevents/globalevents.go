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

var GeneratorBusyCount *atomic.Int64
var PendingSqlCount *atomic.Int64
var TotalSqlCount *atomic.Int64
var MaxPendingSqlCount *atomic.Int64

// init creates the Events channel and initializes it with
// the message "EventsStarted" to alert the worker goroutines that the
// channel is active. It also initializes the atomic counters.
func init() {
	GeneratorBusyCount = &atomic.Int64{}
	PendingSqlCount = &atomic.Int64{}
	TotalSqlCount = &atomic.Int64{}
	MaxPendingSqlCount = &atomic.Int64{}

	Events = make(chan string, 25000)
	Events <- "EventsStarted"
}

// IsDone checks the counters to see if all work has been
// completed. It returns true if there is no remaining work
// in any queue.
func IsDone() bool {
	return GeneratorBusyCount.Load() == 0 && PendingSqlCount.Load() == 0
}

// max atomically returns the maximum of two integer values.
func max(a, b int64) int64 {
	if a > b {
		return a
	}
	return b
}

// GeneratorStarting records the fact that a generator has begun.
func GeneratorStarting() {
	GeneratorBusyCount.Add(1)
}

// GeneratorDone records the fact that a generator has completed.
// When there is no more work, the Events channel is populated.
func GeneratorDone() {
	GeneratorBusyCount.Add(-1)
	if IsDone() {
		Send("GeneratorDone")
	}
}

// SqlStarting records the fact that a SQL statement has been queued.
func SqlStarting() {
	pendingSqlCountVal := PendingSqlCount.Add(1)
	totalSqlCountVal := TotalSqlCount.Add(1)
	MaxPendingSqlCount.Store(max(pendingSqlCountVal, totalSqlCountVal))
}

// SqlDone records the fact that a SQL statement has completed.
// When there is no more work, the Events channel is populated.
func SqlDone() {
	PendingSqlCount.Add(-1)
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
	return <-Events
}

// PrintCounts outputs the event counts and maximum concurrent counts to the console.
func PrintCounts() {
	fmt.Printf("GeneratorBusyCount: %d\n", GeneratorBusyCount)
	fmt.Printf("PendingSqlCount: %d, TotalSqlCount: %d, MaxPendingSqlCount: %d\n", PendingSqlCount, TotalSqlCount, MaxPendingSqlCount)
}
