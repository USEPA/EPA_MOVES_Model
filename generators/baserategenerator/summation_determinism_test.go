/*
Unit tests for floating-point summation-order determinism in the base rate generator.

Background: Go randomizes map iteration order on every run and platform. Wherever the
generator sums floating-point values while ranging over a map, floating-point
non-associativity ((a+b)+c != a+(b+c)) makes the accumulated result vary from run to
run in its least-significant bits. combineDriveScheduleOpModeFractions removes that
variation for the drive-schedule-weighted operating mode fractions (which feed the
RatesOpModeDistribution and base rate outputs) by summing over driveScheduleID in a
fixed ascending order.

These tests verify:
 1. The accumulated result is numerically correct.
 2. Repeated calls with the same inputs are bit-for-bit identical.
 3. The result is independent of the map iteration order that would otherwise be
    seen by ranging over scheduleFractions directly (i.e. it matches a reference
    computed in ascending driveScheduleID order), demonstrated with input values
    chosen so that a different summation order produces a different float64.
*/
package baserategenerator

import (
	"math"
	"sort"
	"testing"
)

// referenceCombine sums the drive-schedule-weighted opMode fractions in ascending
// driveScheduleID order. This is the canonical order the production helper must match.
func referenceCombine(scheduleFractions map[int]float64,
	perSchedule map[int]map[int]float64) map[int]float64 {
	ids := make([]int, 0, len(scheduleFractions))
	for id := range scheduleFractions {
		ids = append(ids, id)
	}
	sort.Ints(ids)
	dest := make(map[int]float64)
	for _, id := range ids {
		frac := scheduleFractions[id]
		for opModeID, opModeFraction := range perSchedule[id] {
			dest[opModeID] = dest[opModeID] + frac*opModeFraction
		}
	}
	return dest
}

// callCombine drives the production helper against plain maps.
func callCombine(scheduleFractions map[int]float64,
	perSchedule map[int]map[int]float64) map[int]float64 {
	dest := make(map[int]float64)
	combineDriveScheduleOpModeFractions(dest, scheduleFractions,
		func(driveScheduleID int) map[int]float64 {
			return perSchedule[driveScheduleID]
		})
	return dest
}

// makeSummationCase builds a single opMode's contributions across many drive
// schedules using values whose sum is order-sensitive in float64: a large term
// interleaved with many tiny terms loses precision differently depending on the
// order in which the tiny terms are added.
func makeSummationCase() (map[int]float64, map[int]map[int]float64) {
	const opModeID = 22
	scheduleFractions := make(map[int]float64)
	perSchedule := make(map[int]map[int]float64)

	// One dominant term.
	scheduleFractions[0] = 1.0
	perSchedule[0] = map[int]float64{opModeID: 1e16}

	// Many tiny terms; individually negligible against 1e16, but their placement
	// relative to the big term changes the rounded result.
	for id := 1; id <= 200; id++ {
		scheduleFractions[id] = 1.0
		perSchedule[id] = map[int]float64{opModeID: 1.0}
	}
	return scheduleFractions, perSchedule
}

// TestCombineIsCorrect checks a small, exactly-representable case.
func TestCombineIsCorrect(t *testing.T) {
	scheduleFractions := map[int]float64{
		10: 0.5,
		20: 0.25,
		30: 0.25,
	}
	perSchedule := map[int]map[int]float64{
		10: {1: 4, 2: 8},
		20: {1: 4, 2: 8},
		30: {2: 8},
	}
	got := callCombine(scheduleFractions, perSchedule)

	// opMode 1: 0.5*4 + 0.25*4          = 3
	// opMode 2: 0.5*8 + 0.25*8 + 0.25*8 = 8
	want := map[int]float64{1: 3, 2: 8}
	if len(got) != len(want) {
		t.Fatalf("result size = %d, want %d (%v)", len(got), len(want), got)
	}
	for k, v := range want {
		if got[k] != v {
			t.Errorf("opMode %d = %v, want %v", k, got[k], v)
		}
	}
}

// TestCombineMatchesAscendingReference verifies the helper's result equals the
// ascending-order reference for an order-sensitive input, and that the chosen
// input really is order-sensitive (so the test would catch a regression to
// map-order summation).
func TestCombineMatchesAscendingReference(t *testing.T) {
	scheduleFractions, perSchedule := makeSummationCase()

	got := callCombine(scheduleFractions, perSchedule)
	want := referenceCombine(scheduleFractions, perSchedule)

	for opModeID, wantVal := range want {
		if math.Float64bits(got[opModeID]) != math.Float64bits(wantVal) {
			t.Errorf("opMode %d = %v, want %v (bit patterns differ)", opModeID, got[opModeID], wantVal)
		}
	}

	// Sanity: confirm the input is order-sensitive so this test has teeth.
	// Summing the big term last differs from summing it first in float64.
	bigFirst := 1e16
	for i := 0; i < 200; i++ {
		bigFirst += 1.0
	}
	smallFirst := 0.0
	for i := 0; i < 200; i++ {
		smallFirst += 1.0
	}
	smallFirst += 1e16
	if bigFirst == smallFirst {
		t.Fatal("test input is not order-sensitive; it cannot detect map-order regressions")
	}
}

// TestCombineIsRepeatable verifies repeated calls produce bit-identical results.
func TestCombineIsRepeatable(t *testing.T) {
	scheduleFractions, perSchedule := makeSummationCase()

	first := callCombine(scheduleFractions, perSchedule)
	for i := 0; i < 100; i++ {
		got := callCombine(scheduleFractions, perSchedule)
		if len(got) != len(first) {
			t.Fatalf("iter %d: result size changed", i)
		}
		for opModeID, v := range first {
			if math.Float64bits(got[opModeID]) != math.Float64bits(v) {
				t.Errorf("iter %d: opMode %d = %v, first was %v (not reproducible)",
					i, opModeID, got[opModeID], v)
			}
		}
	}
}

// TestCombineSkipsMissingSchedules verifies that a driveScheduleID whose
// opModeFractions lookup returns nil contributes nothing and does not panic.
func TestCombineSkipsMissingSchedules(t *testing.T) {
	scheduleFractions := map[int]float64{1: 0.5, 2: 0.5, 3: 0.5}
	perSchedule := map[int]map[int]float64{
		1: {7: 2},
		3: {7: 2},
		// id 2 intentionally absent -> lookup returns nil
	}
	dest := make(map[int]float64)
	combineDriveScheduleOpModeFractions(dest, scheduleFractions,
		func(driveScheduleID int) map[int]float64 {
			return perSchedule[driveScheduleID] // nil for id 2
		})

	// opMode 7: 0.5*2 + 0.5*2 = 2 (schedule 2 skipped)
	if dest[7] != 2 {
		t.Errorf("opMode 7 = %v, want 2", dest[7])
	}
}
