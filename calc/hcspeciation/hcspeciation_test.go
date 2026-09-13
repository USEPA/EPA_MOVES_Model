package hcspeciation

import (
	"math"
	"testing"

	"calc/mwo"
)

// The altTHC branch uses E10's factors even though the fuel is E70 or E85, and
// HCSpeciationCalculator.sql spells the oxygenate term for it as
// (MTBEVolume+ETBEVolume+TAMEVolume+10), with the literal 10 standing for E10's
// ethanol volume. The @algorithm comments on the Go branches say the same.
// These check the code agrees with them.
func TestAltEthanolBranchUsesE10OxygenateVolume(t *testing.T) {
	const (
		fuelFormulationID = 9001
		fuelSubTypeID     = 51 // E85
		regClassID        = 30
		modelYearID       = 2015
		processID         = 1 // running exhaust
		altNMHC           = 95.0

		speciationConstant = 0.9
		oxySpeciation      = 0.002
		volToWtPercentOxy  = 0.35
		ch4THCRatio        = 0.0
	)

	for _, tc := range []struct {
		name       string
		etohVolume float64
	}{
		// The oxygenate term must not depend on the formulation's own ethanol
		// volume in this branch, so both rows expect the same factor.
		{"E85 formulation", 79.0},
		{"E10-like formulation, control", 10.0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			mwo.FuelFormulations = map[int]*mwo.FuelFormulation{
				fuelFormulationID: {
					FuelFormulationID: fuelFormulationID,
					FuelSubTypeID:     fuelSubTypeID,
					ETOHVolume:        tc.etohVolume,
					VolToWtPercentOxy: volToWtPercentOxy,
					FuelTypeID:        5,
				},
			}
			mwo.NeededPolProcessIDs = map[int]bool{
				79*100 + processID: true,
				80*100 + processID: true,
			}
			methaneTHCRatio = map[methaneTHCRatioKey]*methaneTHCRatioDetail{
				{processID, 12, regClassID, modelYearID}: {CH4THCRatio: ch4THCRatio},
			}
			HCSpeciation = map[HCSpeciationKey]*HCSpeciationDetail{
				{80*100 + processID, 12, regClassID, modelYearID}: {
					speciationConstant: speciationConstant,
					oxySpeciation:      oxySpeciation,
				},
			}

			in := make(chan *mwo.MWOBlock, 1)
			out := make(chan *mwo.MWOBlock, 1)
			in <- &mwo.MWOBlock{FuelBlocks: []*mwo.FuelBlock{{
				Key: mwo.MWOKey{
					RegClassID:  regClassID,
					FuelTypeID:  5,
					ModelYearID: modelYearID,
					PollutantID: 10001, // altTHC
					ProcessID:   processID,
				},
				Emissions: []*mwo.MWOEmission{{
					FuelSubTypeID:     fuelSubTypeID,
					FuelFormulationID: fuelFormulationID,
					EmissionQuant:     altNMHC,
				}},
			}}}
			// calculate() receives without a comma-ok, so closing the channel
			// would hand it a nil block. Run it as production does instead.
			go calculate(in, out)

			block := <-out
			var nmog float64
			var found bool
			for _, fb := range block.FuelBlocks {
				if fb.Key.PollutantID == 80 && fb.Key.ProcessID == processID {
					for _, e := range fb.Emissions {
						nmog = e.EmissionQuant
						found = true
					}
				}
			}
			if !found {
				t.Fatal("no NMOG (80) block was produced, so the branch under test never ran")
			}

			gotFactor := nmog / altNMHC
			wantFactor := speciationConstant + oxySpeciation*volToWtPercentOxy*10

			if math.Abs(gotFactor-wantFactor) > 1e-9 {
				t.Errorf("oxygenate term used the formulation's ethanol volume: factor %.9f, want %.9f (per HCSpeciationCalculator.sql and the @algorithm comment)",
					gotFactor, wantFactor)
			}
		})
	}
}
