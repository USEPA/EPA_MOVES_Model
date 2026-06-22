/*
Unit tests for opMode assignment determinism in the base rate generator.

Background: Go randomizes map iteration order at process startup by seeding the map PRNG
from OS entropy. On macOS and Linux with aggressive ASLR, this means that ranging over
operatingModes directly produces a different opMode iteration order on every run —
and therefore different emission rates for drive-cycle seconds that fall within the
boundary region between two adjacent opModes.

The fix introduces assignOpModeID(), which iterates a pre-sorted []int slice of opMode IDs
rather than the map, guaranteeing the same first-match result on every platform and run.

These tests verify:
  1. assignOpModeID returns the expected opMode for a variety of (VSP, speed) inputs.
  2. A second with (VSP, speed) that would match two modes if iteration were unordered
     is assigned to the lower-numbered mode (first match in ascending order).
  3. A second with no matching opMode returns -1.
  4. After rebuildOperatingModeIDsSorted, operatingModeIDsSorted is sorted ascending.
*/
package baserategenerator

import (
	"sort"
	"testing"
)

// rebuildOperatingModeIDsSorted repopulates operatingModeIDsSorted from the current
// operatingModes map. Used by tests that manipulate the package-level globals directly.
func rebuildOperatingModeIDsSorted() {
	operatingModeIDsSorted = make([]int, 0, len(operatingModes))
	for id := range operatingModes {
		operatingModeIDsSorted = append(operatingModeIDsSorted, id)
	}
	sort.Ints(operatingModeIDsSorted)
}

// setupTestOpModes loads a representative subset of the real MOVES operating modes into
// the package-level globals. Values match the OperatingMode table from movesdb20241112.
// All ranges use the same semantics as the production code:
//   VSPLower (null=no lower bound), VSPUpper (null=no upper bound),
//   speedLower (null=no lower bound), speedUpper (null=no upper bound).
func setupTestOpModes() {
	operatingModes = map[int]*operatingMode{
		// opModeID 11: VSP < 0, speed in [25, 50)
		11: {opModeID: 11, VSPLower: 0, VSPUpper: 0, speedLower: 25, speedUpper: 50,
			isnullVSPLower: true, isnullVSPUpper: false, isnullSpeedLower: false, isnullSpeedUpper: false},
		// opModeID 21: VSP in [0, 3), speed in [25, 50)
		21: {opModeID: 21, VSPLower: 0, VSPUpper: 3, speedLower: 25, speedUpper: 50,
			isnullVSPLower: false, isnullVSPUpper: false, isnullSpeedLower: false, isnullSpeedUpper: false},
		// opModeID 22: VSP in [3, 6), speed in [25, 50)
		22: {opModeID: 22, VSPLower: 3, VSPUpper: 6, speedLower: 25, speedUpper: 50,
			isnullVSPLower: false, isnullVSPUpper: false, isnullSpeedLower: false, isnullSpeedUpper: false},
		// opModeID 33: VSP < 6, speed >= 50 (the opMode with the column-swap bug in earlier testdata)
		33: {opModeID: 33, VSPLower: 0, VSPUpper: 6, speedLower: 50, speedUpper: 0,
			isnullVSPLower: true, isnullVSPUpper: false, isnullSpeedLower: false, isnullSpeedUpper: true},
		// opModeID 37: VSP in [12, 18), speed >= 50
		37: {opModeID: 37, VSPLower: 12, VSPUpper: 18, speedLower: 50, speedUpper: 0,
			isnullVSPLower: false, isnullVSPUpper: false, isnullSpeedLower: false, isnullSpeedUpper: true},
		// opModeID 38: VSP in [18, 24), speed >= 50
		38: {opModeID: 38, VSPLower: 18, VSPUpper: 24, speedLower: 50, speedUpper: 0,
			isnullVSPLower: false, isnullVSPUpper: false, isnullSpeedLower: false, isnullSpeedUpper: true},
	}
	rebuildOperatingModeIDsSorted()
}

// TestSortedOpModeIDsAreSorted verifies that rebuildOperatingModeIDsSorted produces
// an ascending-sorted slice from an arbitrarily-ordered map.
func TestSortedOpModeIDsAreSorted(t *testing.T) {
	setupTestOpModes()
	for i := 1; i < len(operatingModeIDsSorted); i++ {
		if operatingModeIDsSorted[i] <= operatingModeIDsSorted[i-1] {
			t.Errorf("operatingModeIDsSorted not ascending at index %d: %v",
				i, operatingModeIDsSorted)
		}
	}
}

// TestAssignOpModeIDBasicMatches checks unambiguous (VSP, speed) cases where exactly
// one opMode should match.
func TestAssignOpModeIDBasicMatches(t *testing.T) {
	setupTestOpModes()
	cases := []struct {
		vsp, speed  float64
		wantOpMode  int
		description string
	}{
		// VSP=-5, speed=30: matches opMode 11 (VSP<0, speed in [25,50))
		{vsp: -5, speed: 30, wantOpMode: 11, description: "negative VSP mid-speed"},
		// VSP=1.5, speed=30: matches opMode 21 (VSP in [0,3), speed in [25,50))
		{vsp: 1.5, speed: 30, wantOpMode: 21, description: "VSP in [0,3) mid-speed"},
		// VSP=4, speed=30: matches opMode 22 (VSP in [3,6), speed in [25,50))
		{vsp: 4, speed: 30, wantOpMode: 22, description: "VSP in [3,6) mid-speed"},
		// VSP=2, speed=55: matches opMode 33 (VSP<6, speed>=50)
		{vsp: 2, speed: 55, wantOpMode: 33, description: "low VSP high speed"},
		// VSP=15, speed=55: matches opMode 37 (VSP in [12,18), speed>=50)
		{vsp: 15, speed: 55, wantOpMode: 37, description: "VSP in [12,18) high speed"},
		// VSP=20, speed=55: matches opMode 38 (VSP in [18,24), speed>=50)
		{vsp: 20, speed: 55, wantOpMode: 38, description: "VSP in [18,24) high speed"},
		// No match: VSP=50, speed=30 (no opMode covers this range)
		{vsp: 50, speed: 30, wantOpMode: -1, description: "no matching opMode"},
	}
	for _, tc := range cases {
		got := assignOpModeID(tc.vsp, tc.speed)
		if got != tc.wantOpMode {
			t.Errorf("%s: assignOpModeID(vsp=%.1f, speed=%.1f) = %d, want %d",
				tc.description, tc.vsp, tc.speed, got, tc.wantOpMode)
		}
	}
}

// TestAssignOpModeIDOverlapDeterminism verifies that when two modes could both match
// a given (VSP, speed), the lower-numbered mode always wins (ascending first-match).
// This is the key property broken by random map iteration: on any run where opMode 21
// happened to be visited before opMode 11, a second with VSP=-1/speed=30 would
// be misassigned to opMode 21 instead of opMode 11.
func TestAssignOpModeIDOverlapDeterminism(t *testing.T) {
	// Create two overlapping modes to simulate what could happen with bad data
	// (or boundary ambiguity): both opMode 10 and opMode 20 match vsp=-1, speed=30.
	operatingModes = map[int]*operatingMode{
		20: {opModeID: 20, isnullVSPLower: true, isnullVSPUpper: false, VSPUpper: 6,
			isnullSpeedLower: false, speedLower: 25, isnullSpeedUpper: false, speedUpper: 50},
		10: {opModeID: 10, isnullVSPLower: true, isnullVSPUpper: true,
			isnullSpeedLower: false, speedLower: 25, isnullSpeedUpper: false, speedUpper: 50},
	}
	rebuildOperatingModeIDsSorted()

	// With ascending sort, opMode 10 is iterated first and should always win.
	for i := 0; i < 100; i++ {
		got := assignOpModeID(-1, 30)
		if got != 10 {
			t.Errorf("iter %d: got opMode %d, want 10 — ascending sort must give deterministic first-match", i, got)
		}
	}

	// Verify the opposite: if we reverse the slice (descending), opMode 20 wins.
	// This proves that sort order — not the map — controls the outcome.
	reversed := make([]int, len(operatingModeIDsSorted))
	copy(reversed, operatingModeIDsSorted)
	for i, j := 0, len(reversed)-1; i < j; i, j = i+1, j-1 {
		reversed[i], reversed[j] = reversed[j], reversed[i]
	}
	operatingModeIDsSorted = reversed
	got := assignOpModeID(-1, 30)
	if got != 20 {
		t.Errorf("with reversed sort order: got opMode %d, want 20", got)
	}
}

// TestAssignOpModeIDIsConsistentAcrossRepeatedCalls verifies that repeated calls with
// the same inputs always return the same result — i.e., there is no per-call randomness.
func TestAssignOpModeIDIsConsistentAcrossRepeatedCalls(t *testing.T) {
	setupTestOpModes()
	inputs := [][2]float64{
		{-5, 30}, {1.5, 30}, {4, 30}, {2, 55}, {15, 55}, {20, 55},
	}
	for _, inp := range inputs {
		first := assignOpModeID(inp[0], inp[1])
		for i := 0; i < 50; i++ {
			got := assignOpModeID(inp[0], inp[1])
			if got != first {
				t.Errorf("non-deterministic: assignOpModeID(%.1f, %.1f) returned %d on iter %d, first was %d",
					inp[0], inp[1], got, i, first)
			}
		}
	}
}
