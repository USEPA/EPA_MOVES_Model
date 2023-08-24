/*
Calculate rates using the rates-first methodology.
@author Wesley Faler
@version 2017-04-23
*/
package baseratecalculator

import (
	"fmt"
	"math"
	"os"
	"sync"
	"sync/atomic"

	"calc/globalevents"
	"calc/mwo"
	"calc/parse"
)

/**
 * @algorithm
 * @owner BaseRateCalculator
 * @calculator
**/

// key for ExtendedIdleEmissionRateFraction
type ExtendedIdleFractionKey struct {
	modelYearID, fuelTypeID int
}

// extended idle adjustments, data is opModeID 200 extended idle usage fraction
var ExtendedIdleEmissionRateFraction map[ExtendedIdleFractionKey]float64

// key for apuEmissionRateFraction
type APUFractionKey struct {
	modelYearID, fuelTypeID int
}

// APU adjustments, data is opModeID 201 (APU) usage fraction
var apuEmissionRateFraction map[APUFractionKey]float64

// key for ShorepowerEmissionRateFraction
type ShorepowerFractionKey struct {
	modelYearID, fuelTypeID int
}

// shorepower adjustments, data is opModeID 203 (shorepower) usage fraction
var ShorepowerEmissionRateFraction map[ShorepowerFractionKey]float64

// Key for ZoneMonthHour
type ZoneMonthHourKey struct {
	monthID, zoneID, hourID int
}

// Data for ZoneMonthHour
type ZoneMonthHourDetail struct {
	temperature, relHumidity, heatIndex, specificHumidity, molWaterFraction float64
}

// ZoneMonthHour data
var ZoneMonthHour map[ZoneMonthHourKey]*ZoneMonthHourDetail

// Key for mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
type PollutantProcessMappedModelYearKey struct {
	polProcessID, modelYearID int
}

// Data for mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
type PollutantProcessMappedModelYearDetail struct {
	modelYearGroupID, fuelMYGroupID, IMModelYearGroupID int
}

// Mapping from pollutant, process, and model year to model year group, fuel model year group, and I/M model year group.
var PollutantProcessMappedModelYear map[PollutantProcessMappedModelYearKey]*PollutantProcessMappedModelYearDetail

// Key for start temperature adjustments
type StartTempAdjustmentKey struct {
	fuelTypeID, polProcessID, modelYearGroupID, opModeID int
}

// Data for start temperture adjustments
type StartTempAdjustmentDetail struct {
	tempAdjustTermA, tempAdjustTermB, tempAdjustTermC float64
	isLog, isPoly                                     bool
}

// Start temperature adjustments
var StartTempAdjustment map[StartTempAdjustmentKey]*StartTempAdjustmentDetail

// County detail
type CountyDetail struct {
	//countyID, stateID, countyName, altitude, GPAFract, barometricPressure, barometricPressureCV
	GPAFract, barometricPressure float64
}

// County
var County map[int]*CountyDetail

// General Fuel Ratio key
type GeneralFuelRatioKey struct {
	fuelFormulationID, polProcessID, sourceTypeID int
}

// General Fuel Ratio inner detail
type GeneralFuelRatioInnerDetail struct {
	minModelYearID, maxModelYearID, minAgeID, maxAgeID int
	fuelEffectRatio, fuelEffectRatioGPA                float64
}

// General Fuel Ratio data
type GeneralFuelRatioDetail struct {
	details []*GeneralFuelRatioInnerDetail
}

// General Fuel Ratio
var GeneralFuelRatio map[GeneralFuelRatioKey]*GeneralFuelRatioDetail

// Key for CriteriaRatio and AltCriteriaRatio
type CriteriaRatioKey struct {
	fuelFormulationID, polProcessID, sourceTypeID, modelYearID, ageID int
}

// Detail for CriteriaRatio and AltCriteriaRatio
type CriteriaRatioDetail struct {
	ratio, ratioGPA, ratioNoSulfur float64
}

// CriteriaRatio
var CriteriaRatio map[CriteriaRatioKey]*CriteriaRatioDetail

// Alternate CritieraRatio
var AltCriteriaRatio map[CriteriaRatioKey]*CriteriaRatioDetail

// Key for temperature adjustments
type TemperatureAdjustmentKey struct {
	polProcessID, fuelTypeID, modelYearID int
}

// Detail for temperature adjustments
type TemperatureAdjustmentDetail struct {
	tempAdjustTermA, tempAdjustTermB, tempAdjustTermC float64
}

// Temperature adjustments
var TemperatureAdjustment map[TemperatureAdjustmentKey]*TemperatureAdjustmentDetail

// Default temperature adjustment, used for all entries not explicitly given in the TemperatureAdjustment table.
var defaultTemperatureAdjustment TemperatureAdjustmentDetail

// Detail for the NOx humidity adjustments
type NOxHumidityAdjustDetail struct {
	humidityTermA, humidityTermB, humidityLowBound, humidityUpBound float64
	humidityNOxEq, humidityUnits                                    string
}

// NOx humidity adjustments. Key is fuelTypeID
var NOxHumidityAdjust map[int]*NOxHumidityAdjustDetail

// Key for air conditioning usage
type ZoneACFactorKey struct {
	hourID, sourceTypeID, modelYearID int
}

// Air conditioning fraction
var ZoneACFactor map[ZoneACFactorKey]float64

// Key for IM factor
type IMFactorKey struct {
	polProcessID                 int
	inspectFreq, testStandardsID int
	sourceTypeID, fuelTypeID     int
	IMModelYearGroupID           int
	ageGroupID                   int
}

// IM factor
var IMFactor map[IMFactorKey]float64

// IM Coverage key
type IMCoverageKey struct {
	polProcessID, modelYearID, sourceTypeID, fuelTypeID int
}

// IMAdjustFract, scaled by complianceFactor (itself scale from percent to fraction) and IM factor
var IMCoverage map[IMCoverageKey]float64

// Emission rate adjustment key
type EmissionRateAdjustmentKey struct {
	polProcessID, sourceTypeID, regClassID, fuelTypeID, modelYearID int
}

// Emission rate adjustments
var EmissionRateAdjustment map[EmissionRateAdjustmentKey]float64

// EV Efficiency key
type EVEfficiencyKey struct {
	polProcessID, sourceTypeID, regClassID, fuelTypeID, modelYearID int
}

// EV Efficiency values
type EVEfficiencyDetail struct {
	batteryEfficiency, chargingEfficiency float64
}

// EV Efficiency
var EVEfficiency map[EVEfficiencyKey]*EVEfficiencyDetail

// Unique FuelBlock objects, used for aggregating data.
var uniqueFuelBlocks map[mwo.MWOKey]*mwo.FuelBlock

// Multithread guard for UniqueFuelBlocks
var uniqueFuelBlocksGuard = &sync.Mutex{}

// Channel of flags indicating no more fuel blocks are awaiting processing.
var fuelBlocksDone chan int

// Number of fuel blocks awaiting processing.
var fuelBlockCount int32

// Number of outstanding fuel block readers
var fuelBlockReaderCount int32

// universalActivity key
type universalActivityKey struct {
	hourDayID, modelYearID, sourceTypeID int
}

// universalActivity
var universalActivity map[universalActivityKey]float64

// All hourDayID values found in the universalActivity data.
var universalActivityHourDayIDs map[int]bool

// Activity weights key
type activityWeightKey struct {
	hourDayID, modelYearID, sourceTypeID, fuelTypeID, regClassID int
}

// Activity weights detail
type activityWeightDetail struct {
	smfrFraction, smfrRatesFraction float64
}

// Activity weights
var activityWeight map[activityWeightKey]*activityWeightDetail

// Initialize package-level variables, creating empty lookup tables.
func init() {
	ExtendedIdleEmissionRateFraction = make(map[ExtendedIdleFractionKey]float64)
	apuEmissionRateFraction = make(map[APUFractionKey]float64)
	ShorepowerEmissionRateFraction = make(map[ShorepowerFractionKey]float64)
	ZoneMonthHour = make(map[ZoneMonthHourKey]*ZoneMonthHourDetail)
	PollutantProcessMappedModelYear = make(map[PollutantProcessMappedModelYearKey]*PollutantProcessMappedModelYearDetail)
	StartTempAdjustment = make(map[StartTempAdjustmentKey]*StartTempAdjustmentDetail)
	County = make(map[int]*CountyDetail)
	GeneralFuelRatio = make(map[GeneralFuelRatioKey]*GeneralFuelRatioDetail)
	CriteriaRatio = make(map[CriteriaRatioKey]*CriteriaRatioDetail)
	AltCriteriaRatio = make(map[CriteriaRatioKey]*CriteriaRatioDetail)
	TemperatureAdjustment = make(map[TemperatureAdjustmentKey]*TemperatureAdjustmentDetail)
	NOxHumidityAdjust = make(map[int]*NOxHumidityAdjustDetail)
	ZoneACFactor = make(map[ZoneACFactorKey]float64)
	IMFactor = make(map[IMFactorKey]float64)
	IMCoverage = make(map[IMCoverageKey]float64)
	EmissionRateAdjustment = make(map[EmissionRateAdjustmentKey]float64)
	EVEfficiency = make(map[EVEfficiencyKey]*EVEfficiencyDetail)
	uniqueFuelBlocks = make(map[mwo.MWOKey]*mwo.FuelBlock)
	fuelBlocksDone = make(chan int, 100)
	fuelBlockCount = 0
	fuelBlockReaderCount = 0
	universalActivity = make(map[universalActivityKey]float64)
	universalActivityHourDayIDs = make(map[int]bool)
	activityWeight = make(map[activityWeightKey]*activityWeightDetail)
}

/*
Reads the ExtendedIdleEmissionRateFraction file to populate the ExtendedIdleEmissionRateFraction lookup table.
Reads the apuEmissionRateFraction file to populate the apuEmissionRateFraction lookup table.
Reads the ShorepowerEmissionRateFraction file to populate the ShorepowerEmissionRateFraction lookup table.
Reads the ZoneMonthHour file to populate the ZoneMonthHour lookup table.
Reads the PollutantProcessMappedModelYear file to populate the PollutantProcessMappedModelYear lookup table.
Reads the StartTempAdjustment file to populate the StartTempAdjustment lookup table.
Reads the County file to populate the County lookup table.
Reads the GeneralFuelRatio file to populate the GeneralFuelRatio lookup table.
Reads the CriteriaRatio file to populate the CriteriaRatio lookup table.
Reads the AltCriteriaRatio file to populate the AltCriteriaRatio lookup table.
Reads the zoneACFactor file to populate the ZoneACFactor lookup table.
Reads the IMFactor file to populate the IMFactor lookup table.
Reads the IMCoverage file to populate the IMCoverage lookup table.
Reads the EmissionRateAdjustmentWorker file to populate the EmissionRateAdjustment lookup table.
Reads the evefficiencyWorker file to populate the EVEfficiency lookup table.
Reads the universalActivity file to populate the universalActivity and universalActivityHourDayIDs lookup tables.
*/
func StartSetup() {
	if _, err := os.Stat("extendedidleemissionratefraction"); err == nil {
		parse.ReadAndParseFile("extendedidleemissionratefraction", func(parts []string) {
			// modelYearID fuelTypeID hourFractionAdjust
			if len(parts) < 3 {
				return
			}
			k := ExtendedIdleFractionKey{parse.GetInt(parts[0]), parse.GetInt(parts[1])}
			hourFractionAdjust := parse.GetFloat(parts[2])
			if _, alreadyExists := ExtendedIdleEmissionRateFraction[k]; alreadyExists {
				fmt.Println("ERROR: Already exists: ExtendedIdleEmissionRateFraction[modelYearID, fuelTypeID]", parse.GetInt(parts[0]), parse.GetInt(parts[1]))
			}
			ExtendedIdleEmissionRateFraction[k] = hourFractionAdjust
		})
	}
	if _, err := os.Stat("apuemissionratefraction"); err == nil {
		parse.ReadAndParseFile("apuemissionratefraction", func(parts []string) {
			// modelYearID fuelTypeID hourFractionAdjust
			if len(parts) < 3 {
				return
			}
			k := APUFractionKey{parse.GetInt(parts[0]), parse.GetInt(parts[1])}
			hourFractionAdjust := parse.GetFloat(parts[2])
			if _, alreadyExists := apuEmissionRateFraction[k]; alreadyExists {
				fmt.Println("ERROR: Already exists: apuEmissionRateFraction[modelYearID, fuelTypeID]", parse.GetInt(parts[0]), parse.GetInt(parts[1]))
			}
			apuEmissionRateFraction[k] = hourFractionAdjust
		})
	}
	if _, err := os.Stat("ShorepowerEmissionRateFraction"); err == nil {
		parse.ReadAndParseFile("ShorepowerEmissionRateFraction", func(parts []string) {
			// modelYearID fuelTypeID hourFractionAdjust
			if len(parts) < 3 {
				return
			}
			k := ShorepowerFractionKey{parse.GetInt(parts[0]), parse.GetInt(parts[1])}
			hourFractionAdjust := parse.GetFloat(parts[2])
			if _, alreadyExists := ShorepowerEmissionRateFraction[k]; alreadyExists {
				fmt.Println("ERROR: Already exists: ShorepowerEmissionRateFraction[modelYearID, fuelTypeID]", parse.GetInt(parts[0]), parse.GetInt(parts[1]))
			}
			ShorepowerEmissionRateFraction[k] = hourFractionAdjust
		})
	}
	if _, err := os.Stat("zonemonthhour"); err == nil {
		parse.ReadAndParseFile("zonemonthhour", func(parts []string) {
			// monthID, zoneID, hourID
			// temperature, relHumidity, heatIndex, specificHumidity, molWaterFraction
			if len(parts) < 8 {
				return
			}
			k := ZoneMonthHourKey{parse.GetInt(parts[0]), parse.GetInt(parts[1]), parse.GetInt(parts[2])}
			v := new(ZoneMonthHourDetail)
			v.temperature = parse.GetFloat(parts[3])
			v.relHumidity = parse.GetFloat(parts[4])
			v.heatIndex = parse.GetFloat(parts[5])
			v.specificHumidity = parse.GetFloat(parts[6])
			v.molWaterFraction = parse.GetFloat(parts[7])
			if _, alreadyExists := ZoneMonthHour[k]; alreadyExists {
				fmt.Println("ERROR: Already exists ZoneMonthHour[k]", k)
			}
			ZoneMonthHour[k] = v
		})
	}
	if _, err := os.Stat("pollutantprocessmappedmodelyear"); err == nil {
		parse.ReadAndParseFile("pollutantprocessmappedmodelyear", func(parts []string) {
			// polProcessID, modelYearID, modelYearGroupID, fuelMYGroupID, IMModelYearGroupID
			if len(parts) < 5 {
				return
			}
			// Key: polProcessID, modelYearID
			k := PollutantProcessMappedModelYearKey{parse.GetInt(parts[0]), parse.GetInt(parts[1])}
			v := new(PollutantProcessMappedModelYearDetail)
			v.modelYearGroupID = parse.GetInt(parts[2])
			v.fuelMYGroupID = parse.GetInt(parts[3])
			v.IMModelYearGroupID = parse.GetInt(parts[4])
			if _, alreadyExists := PollutantProcessMappedModelYear[k]; alreadyExists {
				fmt.Println("ERROR: Already exists PollutantProcessMappedModelYear[k]", k)
			}
			PollutantProcessMappedModelYear[k] = v
		})
	}
	if _, err := os.Stat("starttempadjustment"); err == nil {
		parse.ReadAndParseFile("starttempadjustment", func(parts []string) {
			// fuelTypeID, polProcessID, modelYearGroupID, opModeID,
			// tempAdjustTermA, tempAdjustTermACV,
			// tempAdjustTermB, tempAdjustTermBCV,
			// tempAdjustTermC, tempAdjustTermCCV,
			// startTempEquationType
			if len(parts) < 11 {
				return
			}
			k := StartTempAdjustmentKey{parse.GetInt(parts[0]), parse.GetInt(parts[1]), parse.GetInt(parts[2]), parse.GetInt(parts[3])}
			v := new(StartTempAdjustmentDetail)
			v.tempAdjustTermA = parse.GetFloat(parts[4])
			v.tempAdjustTermB = parse.GetFloat(parts[6])
			v.tempAdjustTermC = parse.GetFloat(parts[8])
			equationType := parse.GetString(parts[10])
			if equationType == "LOG" {
				v.isLog = true
			} else if equationType == "POLY" {
				v.isPoly = true
			}
			if _, alreadyExists := StartTempAdjustment[k]; alreadyExists {
				fmt.Println("ERROR: Already exists StartTempAdjustment[k]", k)
			}
			StartTempAdjustment[k] = v
		})
	}
	if _, err := os.Stat("county"); err == nil {
		parse.ReadAndParseFile("county", func(parts []string) {
			//countyID, stateID, countyName, altitude, GPAFract, barometricPressure, barometricPressureCV
			if len(parts) < 7 {
				return
			}
			k := parse.GetInt(parts[0])
			v := new(CountyDetail)
			v.GPAFract = parse.GetFloat(parts[4])
			v.barometricPressure = parse.GetFloat(parts[5])
			if _, alreadyExists := County[k]; alreadyExists {
				fmt.Println("ERROR: Already exists County[k]", k)
			}
			County[k] = v
		})
	}
	if _, err := os.Stat("generalfuelratio"); err == nil {
		parse.ReadAndParseFile("generalfuelratio", func(parts []string) {
			// fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,
			// minModelYearID, maxModelYearID,
			// minAgeID, maxAgeID,
			// sourceTypeID,
			// fuelEffectRatio, fuelEffectRatioGPA
			if len(parts) < 12 {
				return
			}
			// Key: fuelFormulationID, polProcessID, sourceTypeID
			k := GeneralFuelRatioKey{parse.GetInt(parts[1]), parse.GetInt(parts[2]), parse.GetInt(parts[9])}
			v := new(GeneralFuelRatioDetail)
			v.details = make([]*GeneralFuelRatioInnerDetail, 0, 20)
			iv := new(GeneralFuelRatioInnerDetail)
			iv.minModelYearID = parse.GetInt(parts[5])
			iv.maxModelYearID = parse.GetInt(parts[6])
			iv.minAgeID = parse.GetInt(parts[7])
			iv.maxAgeID = parse.GetInt(parts[8])
			iv.fuelEffectRatio = parse.GetFloat(parts[10])
			iv.fuelEffectRatioGPA = parse.GetFloat(parts[11])
			v.details = append(v.details, iv)

			// TODO
			if _, alreadyExists := GeneralFuelRatio[k]; alreadyExists {
				fmt.Println("ERROR (but maybe should be tolerated) already exists GeneralFuelRatio[k]", k)
			}
			GeneralFuelRatio[k] = v
		})
	}
	if _, err := os.Stat("criteriaratio"); err == nil {
		parse.ReadAndParseFile("criteriaratio", func(parts []string) {
			/*  fuelTypeID,
			fuelFormulationID,
			polProcessID,
			pollutantID,
			processID,
			sourceTypeID,
			MYRMAP(modelYearID) as modelYearID,
			ageID,
			ratio,
			ratioGPA,
			ratioNoSulfur */
			if len(parts) < 11 {
				return
			}
			// Key: fuelFormulationID, polProcessID, sourceTypeID, modelYearID, ageID
			k := CriteriaRatioKey{parse.GetInt(parts[1]), parse.GetInt(parts[2]), parse.GetInt(parts[5]), parse.GetInt(parts[6]), parse.GetInt(parts[7])}
			v := new(CriteriaRatioDetail)
			v.ratio = parse.GetFloat(parts[8])
			v.ratioGPA = parse.GetFloat(parts[9])
			v.ratioNoSulfur = parse.GetFloat(parts[10])
			if _, alreadyExists := CriteriaRatio[k]; alreadyExists {
				fmt.Println("ERROR: already exists CriteriaRatio[k]", k)
			}
			CriteriaRatio[k] = v
		})
	}
	if _, err := os.Stat("altcriteriaratio"); err == nil {
		parse.ReadAndParseFile("altcriteriaratio", func(parts []string) {
			/*  fuelTypeID,
			fuelFormulationID,
			polProcessID,
			pollutantID,
			processID,
			sourceTypeID,
			MYRMAP(modelYearID) as modelYearID,
			ageID,
			ratio,
			ratioGPA,
			ratioNoSulfur */
			if len(parts) < 11 {
				return
			}
			// Key: fuelFormulationID, polProcessID, sourceTypeID, modelYearID, ageID
			k := CriteriaRatioKey{parse.GetInt(parts[1]), parse.GetInt(parts[2]), parse.GetInt(parts[5]), parse.GetInt(parts[6]), parse.GetInt(parts[7])}
			v := new(CriteriaRatioDetail)
			v.ratio = parse.GetFloat(parts[8])
			v.ratioGPA = parse.GetFloat(parts[9])
			v.ratioNoSulfur = parse.GetFloat(parts[10])
			if _, alreadyExists := AltCriteriaRatio[k]; alreadyExists {
				fmt.Println("ERROR: Already exists AltCriteriaRatio[k]", k)
			}
			AltCriteriaRatio[k] = v
		})
	}
	if _, err := os.Stat("temperatureadjustment"); err == nil {
		parse.ReadAndParseFile("temperatureadjustment", func(parts []string) {
			/*  polProcessID, fuelTypeID,
			tempAdjustTermA, tempAdjustTermACV,
			tempAdjustTermB, tempAdjustTermBCV,
			tempAdjustTermC, tempAdjustTermCCV,
			minModelYearID, maxModelYearID*/
			if len(parts) < 10 {
				return
			}
			polProcessID := parse.GetInt(parts[0])
			fuelTypeID := parse.GetInt(parts[1])
			minModelYearID := maxInt(1960, parse.GetInt(parts[8]))
			maxModelYearID := minInt(2060, parse.GetInt(parts[9]))
			tempAdjustTermA := parse.GetFloat(parts[2])
			tempAdjustTermB := parse.GetFloat(parts[4])
			tempAdjustTermC := parse.GetFloat(parts[6])
			for modelYearID := minModelYearID; modelYearID <= maxModelYearID; modelYearID++ {
				k := TemperatureAdjustmentKey{polProcessID, fuelTypeID, modelYearID}
				v := new(TemperatureAdjustmentDetail)
				v.tempAdjustTermA = tempAdjustTermA
				v.tempAdjustTermB = tempAdjustTermB
				v.tempAdjustTermC = tempAdjustTermC
				if _, alreadyExists := TemperatureAdjustment[k]; alreadyExists {
					fmt.Println("ERROR: Already exists TemperatureAdjustment[k]", k)
				}
				TemperatureAdjustment[k] = v
			}
		})
	}
	if _, err := os.Stat("noxhumidityadjust"); err == nil {
		parse.ReadAndParseFile("noxhumidityadjust", func(parts []string) {
			/* fuelTypeID, humidityNOxEq, humidityTermA, humidityTermB, humidityLowBound, humidityUpBound, humidityUnits */
			if len(parts) < 7 {
				return
			}
			fuelTypeID := parse.GetInt(parts[0])
			v := new(NOxHumidityAdjustDetail)
			v.humidityNOxEq = parts[1]
			v.humidityTermA = parse.GetFloat(parts[2])
			v.humidityTermB = parse.GetFloat(parts[3])
			v.humidityLowBound = parse.GetFloat(parts[4])
			v.humidityUpBound = parse.GetFloat(parts[5])
			v.humidityUnits = parts[6]

			if _, alreadyExists := NOxHumidityAdjust[fuelTypeID]; alreadyExists {
				fmt.Println("ERROR: Already exists NOxHumidityAdjust[fuelTypeID]", fuelTypeID)
			}
			NOxHumidityAdjust[fuelTypeID] = v
		})
	}

	if _, err := os.Stat("zoneacfactor"); err == nil {
		parse.ReadAndParseFile("zoneacfactor", func(parts []string) {
			// hourID, sourceTypeID, modelYearID, ACFactor
			if len(parts) < 4 {
				return
			}
			// Key: hourID, sourceTypeID, modelYearID
			k := ZoneACFactorKey{parse.GetInt(parts[0]), parse.GetInt(parts[1]), parse.GetInt(parts[2])}
			if _, alreadyExists := ZoneACFactor[k]; alreadyExists {
				fmt.Println("ERROR: Already exists ZoneACFactor[k]", k)
			}
			ZoneACFactor[k] = parse.GetFloat(parts[3])
		})
	}
	if _, err := os.Stat("imfactor"); err == nil {
		parse.ReadAndParseFile("imfactor", func(parts []string) {
			/*
				0: polProcessID,
				1: inspectFreq, testStandardsID,
				3: sourceTypeID, fuelTypeID,
				5: IMModelYearGroupID,
				6: ageGroupID,
				7: IMFactor
			*/
			if len(parts) < 8 {
				return
			}
			// Key: polProcessID, inspectFreq, testStandardsID, sourceTypeID, fuelTypeID, IMModelYearGroupID, ageGroupID
			k := IMFactorKey{parse.GetInt(parts[0]), parse.GetInt(parts[1]), parse.GetInt(parts[2]), parse.GetInt(parts[3]), parse.GetInt(parts[4]), parse.GetInt(parts[5]), parse.GetInt(parts[6])}
			if _, alreadyExists := IMFactor[k]; alreadyExists {
				fmt.Println("ERROR: Already exists IMFactor[k]", k)
			}
			IMFactor[k] = parse.GetFloat(parts[7])
		})
	}
	if _, err := os.Stat("imcoverage"); err == nil {
		parse.ReadAndParseFile("imcoverage", func(parts []string) {
			/*
				0: polProcessID
				1: stateID, countyID
				3: yearID
				4: sourceTypeID, fuelTypeID
				6: IMProgramID
				7: begModelYearID, endModelYearID
				9: inspectFreq, testStandardsID
				11: useIMyn
				12: complianceFactor
			*/
			if len(parts) < 13 {
				return
			}
			polProcessID := parse.GetInt(parts[0])
			sourceTypeID := parse.GetInt(parts[4])
			fuelTypeID := parse.GetInt(parts[5])
			begModelYearID := maxInt(1960, parse.GetInt(parts[7]))
			endModelYearID := minInt(2060, parse.GetInt(parts[8]))
			inspectFreq := parse.GetInt(parts[9])
			testStandardsID := parse.GetInt(parts[10])
			complianceFactor := 0.01 * parse.GetFloat(parts[12])
			for modelYearID := begModelYearID; modelYearID <= endModelYearID; modelYearID++ {
				// Key: polProcessID, modelYearID
				ppa := PollutantProcessMappedModelYear[PollutantProcessMappedModelYearKey{polProcessID, modelYearID}]
				if ppa == nil {
					continue
				}
				ageGroupID := mwo.AgeGroups[mwo.Constants.YearID-modelYearID]
				// Key: polProcessID, inspectFreq, testStandardsID, sourceTypeID, fuelTypeID, IMModelYearGroupID, ageGroupID
				imf, found := IMFactor[IMFactorKey{polProcessID, inspectFreq, testStandardsID, sourceTypeID, fuelTypeID, ppa.IMModelYearGroupID, ageGroupID}]
				if !found {
					continue
				}
				// Key: polProcessID, modelYearID, sourceTypeID, fuelTypeID
				imk := IMCoverageKey{polProcessID, modelYearID, sourceTypeID, fuelTypeID}
				IMAdjustFract, found := IMCoverage[imk]
				if found {
					IMAdjustFract += imf * complianceFactor
				} else {
					IMAdjustFract = imf * complianceFactor
				}
				if _, alreadyExists := IMCoverage[imk]; alreadyExists {
					fmt.Println("ERROR: Already exists IMCoverage[imk]", imk)
				}
				IMCoverage[imk] = IMAdjustFract
			}
		})
	}
	if _, err := os.Stat("emissionrateadjustmentworker"); err == nil {
		parse.ReadAndParseFile("emissionrateadjustmentworker", func(parts []string) {
			//polProcessID, sourceTypeID, regClassID, fuelTypeID, modelYearID
			//emissionRateAdjustment
			if len(parts) < 6 {
				return
			}
			// Key: polProcessID, sourceTypeID, regClassID, fuelTypeID, modelYearID
			k := EmissionRateAdjustmentKey{parse.GetInt(parts[0]), parse.GetInt(parts[1]), parse.GetInt(parts[2]), parse.GetInt(parts[3]), parse.GetInt(parts[4])}
			if _, alreadyExists := EmissionRateAdjustment[k]; alreadyExists {
				fmt.Println("ERROR: Already exists EmissionRateAdjustment[k]", k)
			}
			EmissionRateAdjustment[k] = parse.GetFloat(parts[5])
		})
	}

	if _, err := os.Stat("evefficiencyworker"); err == nil {
		parse.ReadAndParseFile("evefficiencyworker", func(parts []string) {
			//polProcessID, sourceTypeID, regClassID, fuelTypeID, modelYearID, batteryEfficiency, chargingEfficiency
			if len(parts) < 7 {
				return
			}
			// Key: polProcessID, sourceTypeID, regClassID, fuelTypeID, modelYearID
			k := EVEfficiencyKey{parse.GetInt(parts[0]), parse.GetInt(parts[1]), parse.GetInt(parts[2]), parse.GetInt(parts[3]), parse.GetInt(parts[4])}
			v := new(EVEfficiencyDetail)
			v.batteryEfficiency = parse.GetFloat(parts[5])
			v.chargingEfficiency = parse.GetFloat(parts[6])
			if _, alreadyExists := EVEfficiency[k]; alreadyExists {
				fmt.Println("ERROR: Already exists EVEfficiency[k]", k)
			}
			EVEfficiency[k] = v
		})
	}
	if _, err := os.Stat("universalactivity"); err == nil {
		parse.ReadAndParseFile("universalactivity", func(parts []string) {
			//hourDayID, modelYearID, sourceTypeID
			//activity
			if len(parts) < 4 {
				return
			}
			// Key: hourDayID, modelYearID, sourceTypeID
			k := universalActivityKey{parse.GetInt(parts[0]), parse.GetInt(parts[1]), parse.GetInt(parts[2])}
			if _, alreadyExists := universalActivity[k]; alreadyExists {
				fmt.Println("ERROR: Already exists universalActivity[k]", k)
			}
			universalActivity[k] = parse.GetFloat(parts[3])
			universalActivityHourDayIDs[k.hourDayID] = true
		})
	}
}

// Maximum of two integers
func maxInt(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

// Minimum of two integers
func minInt(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

// Complete any pending asychronous operations initiated during StartSetup().
func FinishSetup() {
	// There are no asynchronous reads, so nothing to do here.

	// Report the amount of data read into memory
	fmt.Println("len(baseratecalculator.ExtendedIdleEmissionRateFraction)=", len(ExtendedIdleEmissionRateFraction))
	fmt.Println("len(baseratecalculator.apuEmissionRateFraction)=", len(apuEmissionRateFraction))
	fmt.Println("len(baseratecalculator.ShorepowerEmissionRateFraction)=", len(ShorepowerEmissionRateFraction))
	fmt.Println("len(baseratecalculator.ZoneMonthHour)=", len(ZoneMonthHour))
	fmt.Println("len(baseratecalculator.PollutantProcessMappedModelYear)=", len(PollutantProcessMappedModelYear))
	fmt.Println("len(baseratecalculator.StartTempAdjustment)=", len(StartTempAdjustment))
	fmt.Println("len(baseratecalculator.County)=", len(County))
	fmt.Println("len(baseratecalculator.GeneralFuelRatio)=", len(GeneralFuelRatio))
	fmt.Println("len(baseratecalculator.CriteriaRatio)=", len(CriteriaRatio))
	fmt.Println("len(baseratecalculator.AltCriteriaRatio)=", len(AltCriteriaRatio))
	fmt.Println("len(baseratecalculator.TemperatureAdjustment)=", len(TemperatureAdjustment))
	fmt.Println("len(baseratecalculator.NOxHumidityAdjust)=", len(NOxHumidityAdjust))
	fmt.Println("len(baseratecalculator.ZoneACFactor)=", len(ZoneACFactor))
	fmt.Println("len(baseratecalculator.IMFactor)=", len(IMFactor))
	fmt.Println("len(baseratecalculator.IMCoverage)=", len(IMCoverage))
	fmt.Println("len(baseratecalculator.EmissionRateAdjustment)=", len(EmissionRateAdjustment))
	fmt.Println("len(baseratecalculator.EVEfficiency)=", len(EVEfficiency))
	fmt.Println("len(baseratecalculator.universalActivity)=", len(universalActivity))
	fmt.Println("len(baseratecalculator.universalActivityHourDayIDs)=", len(universalActivityHourDayIDs))
	fmt.Println("len(baseratecalculator.activityWeight)=", len(activityWeight))

	// Done
	fmt.Println("BaseRateCalculator finished reading setup files.")
}

// Launch computation threads. Each thread reads from data files and writes to the outputBlocks channel.
func StartCalculating(howManyThreads int, outputBlocks chan *mwo.MWOBlock) {
	// The following block comment written by Evan Murray:
	/* The concurrency here is likely of little value, and may even be counterproductive to increasing
			performance. Each goroutine function depends on data sent by the previous one, so the goroutines
			end up waiting on data from the previous goroutine the same way as if the code were written serially.
			The channels are buffered, which means they can hold some data before this psedo-deadlock occurs, but
			I think the buffers of 1000 are relatively small compared to the function throughput. For large runs,
			a small buffer makes no significant impact on performance becuase the same pseudo-deadlock occurs as
			if the channels are unbuffered, just a second or so later. These buffers can come with a memory allocation
			cost, so using buffered channels can be _slower_ here than using unbuffered channels. I do not know for
			certain whether this is the case.
	Moreover, the outputBlocks channel must also be buffered, or the doCalculationPipeline function becomes the
			bottleneck. The buffer, right now, is only 2000, which seems to me to be smaller than the throughput of the
			functions sending data to it.
	This concurrency can work well if the buffers are large enough, but it's tough to know whether they are or not without
			doing more benchmarking. In the future, it's probably better to try and set the buffer size according to the
			estimate of the size of the MOVES run.
	*/

	internalQueueBeforeAccumulator := make(chan *mwo.MWOBlock, 1000)
	internalQueueAfterAccumulator := make(chan *mwo.MWOBlock, 1000)
	activityReady := make(chan int, 1+howManyThreads)

	globalevents.SetReadingStartedJustRates() // balanced at end of this function

	// Start computing the activity weight
	globalevents.SetReadingStartedJustRates() // balanced in calculateActivityWeight()
	go calculateActivityWeight(activityReady)

	for i := 0; i < howManyThreads; i++ {
		go calculateAndAccumulate(internalQueueBeforeAccumulator)
		go aggregateAndApplyActivity(activityReady, internalQueueAfterAccumulator, outputBlocks)
	}
	globalevents.SetReadingStartedJustRates() // balanced in doCalculationPipeline()
	go doCalculationPipeline(internalQueueBeforeAccumulator, internalQueueAfterAccumulator, outputBlocks)

	globalevents.MWOReadDone() // balance globalevents.SetReadingStartedJustRates()
}

// Read and process age-based rates.
// Read and process non-age-based rates.
func doCalculationPipeline(internalQueueBeforeAccumulator, internalQueueAfterAccumulator, outputBlocks chan *mwo.MWOBlock) {
	defer globalevents.MWOReadDone()

	// Handle age-based rates
	fmt.Println("baseratecalculator Reading age-based rates...")
	fuelBlockReaderCount = 1
	globalevents.SetReadingStartedJustRates()
	streamBaseRateByAge(internalQueueBeforeAccumulator)
	fuelBlockReaderCount = 0
	// Wait for all fuel blocks to be accumulated
	for {
		if fuelBlockCount <= 0 {
			break
		}
		<-fuelBlocksDone // Wait for an event
	}
	// Disburse the accumulated blocks
	fmt.Println("baseratecalculator Disbursing accumulated age-based blocks...")
	fmt.Println("baseratecalculator len(uniqueFuelBlocks)=", len(uniqueFuelBlocks))
	disburseAccumulatedBlocks(internalQueueAfterAccumulator)

	// Handle non-age-based rates
	fmt.Println("baseratecalculator Reading non-age-based rates...")
	fuelBlockReaderCount = 1
	globalevents.SetReadingStartedJustRates()
	streamBaseRate(internalQueueBeforeAccumulator)
	fuelBlockReaderCount = 0
	// Wait for all fuel blocks to be accumulated
	for {
		if fuelBlockCount <= 0 {
			break
		}
		<-fuelBlocksDone // Wait for an event
	}
	// Disburse the accumulated blocks
	fmt.Println("baseratecalculator Disbursing accumulated non-age-based blocks...")
	fmt.Println("baseratecalculator len(uniqueFuelBlocks)=", len(uniqueFuelBlocks))
	disburseAccumulatedBlocks(internalQueueAfterAccumulator)

	// Done
	fmt.Println("baseratecalculator done reading and disbursing blocks")
	close(internalQueueBeforeAccumulator)
	close(internalQueueAfterAccumulator)
}

// Read BaseRateByAge data and create blocks for processing.
func streamBaseRateByAge(outputBlocks chan *mwo.MWOBlock) {
	defer globalevents.MWOReadDone()
	recordCount := 0
	parse.ReadAndParseFile("baseratebyage", func(parts []string) {
		// 0: br.sourceTypeID,
		// 1: br.roadTypeID,br.avgSpeedBinID,br.hourDayID,
		// 4: br.polProcessID,br.pollutantID,br.processID,
		// 7: br.modelYearID,br.fuelTypeID,br.ageGroupID,br.regClassID,
		// 11: br.opModeID,br.meanBaseRate,br.meanBaseRateIM,
		// 14: br.emissionRate,br.emissionRateIM,
		// 16: br.meanBaseRateACAdj,br.meanBaseRateIMACAdj,
		// 18: br.emissionRateACAdj,br.emissionRateIMACAdj,
		// 20: br.opModeFraction,br.opModeFractionRate
		if len(parts) < 22 {
			return
		}
		recordCount++
		b := mwo.New() // get a blank mwoBlock
		fb := b.Add()
		atomic.AddInt32(&fuelBlockCount, 1)
		fb.SetupForBaseRates()
		fb.Key.StateID = mwo.Constants.StateID
		fb.Key.CountyID = mwo.Constants.CountyID
		fb.Key.ZoneID = mwo.Constants.ZoneID
		fb.Key.LinkID = mwo.Constants.LinkID
		fb.Key.YearID = mwo.Constants.YearID
		fb.Key.MonthID = mwo.Constants.MonthID

		fb.Key.SourceTypeID = parse.GetInt(parts[0])
		fb.Key.RoadTypeID = parse.GetInt(parts[1])
		//EM - read the average speed bin ID into the MWOKey object as part of EMT-809 rates fix
		fb.Key.AvgSpeedBinID = parse.GetInt(parts[2])
		fb.Key.HourDayID = parse.GetInt(parts[3])
		fb.Key.DayID = fb.Key.HourDayID % 10
		fb.Key.HourID = fb.Key.HourDayID / 10
		fb.Key.PolProcessID = parse.GetInt(parts[4])
		fb.Key.PollutantID = parse.GetInt(parts[5])
		fb.Key.ProcessID = parse.GetInt(parts[6])
		fb.Key.ModelYearID = parse.GetInt(parts[7])
		fb.Key.FuelTypeID = parse.GetInt(parts[8])
		fb.Key.RegClassID = parse.GetInt(parts[10])
		fb.OpMode.OpModeID = parse.GetInt(parts[11])
		meanBaseRate := parse.GetFloat(parts[12])
		meanBaseRateIM := parse.GetFloat(parts[13])
		emissionRate := parse.GetFloat(parts[14])
		emissionRateIM := parse.GetFloat(parts[15])
		meanBaseRateACAdj := parse.GetFloat(parts[16])
		meanBaseRateIMACAdj := parse.GetFloat(parts[17])
		emissionRateACAdj := parse.GetFloat(parts[18])
		emissionRateIMACAdj := parse.GetFloat(parts[19])
		fb.OpMode.GeneralFraction = parse.GetFloat(parts[20])
		fb.OpMode.GeneralFractionRate = parse.GetFloat(parts[21])

		fsDetail := mwo.FuelSupply[mwo.FuelSupplyKey{fb.Key.CountyID, fb.Key.YearID, fb.Key.MonthID, fb.Key.FuelTypeID}]
		if fsDetail == nil {
			return
		}
		for _, fsd := range fsDetail {
			br := new(mwo.MWOBaseRate)
			//br.AvgSpeedBinID = avgSpeedBinID //EM - this line is outdated due to rates fix EMT-809 12/20/2018
			br.FuelSubTypeID = fsd.FuelSubTypeID
			br.FuelFormulationID = fsd.FuelFormulationID
			br.MarketShare = fsd.MarketShare
			br.MeanBaseRate = meanBaseRate
			br.MeanBaseRateIM = meanBaseRateIM
			br.EmissionRate = emissionRate
			br.EmissionRateIM = emissionRateIM
			br.MeanBaseRateACAdj = meanBaseRateACAdj
			br.MeanBaseRateIMACAdj = meanBaseRateIMACAdj
			br.EmissionRateACAdj = emissionRateACAdj
			br.EmissionRateIMACAdj = emissionRateIMACAdj
			fb.OpMode.BaseRates = append(fb.OpMode.BaseRates, br)
		}

		fb.Key.CalcIDs()
		globalevents.MWOBlockCreated()
		outputBlocks <- b // queue the newly parsed block
	})
	fmt.Println("baseratecalculator.streamBaseRateByAge recordCount=", recordCount)
}

// Read BaseRate data and create blocks for processing.
func streamBaseRate(outputBlocks chan *mwo.MWOBlock) {
	defer globalevents.MWOReadDone()
	recordCount := 0
	parse.ReadAndParseFile("baserate", func(parts []string) {
		// 0: br.sourceTypeID,
		// 1: br.roadTypeID,br.avgSpeedBinID,br.hourDayID,
		// 4: br.polProcessID,br.pollutantID,br.processID,
		// 7: br.modelYearID,br.fuelTypeID,br.regClassID,
		// 10: br.opModeID,br.meanBaseRate,br.meanBaseRateIM,
		// 13: br.emissionRate,br.emissionRateIM,
		// 15: br.meanBaseRateACAdj,br.meanBaseRateIMACAdj,
		// 17: br.emissionRateACAdj,br.emissionRateIMACAdj,
		// 19: br.opModeFraction,br.opModeFractionRate
		if len(parts) < 21 {
			return
		}
		recordCount++
		b := mwo.New() // get a blank mwoBlock
		fb := b.Add()
		atomic.AddInt32(&fuelBlockCount, 1)
		fb.SetupForBaseRates()
		fb.Key.StateID = mwo.Constants.StateID
		fb.Key.CountyID = mwo.Constants.CountyID
		fb.Key.ZoneID = mwo.Constants.ZoneID
		fb.Key.LinkID = mwo.Constants.LinkID
		fb.Key.YearID = mwo.Constants.YearID
		fb.Key.MonthID = mwo.Constants.MonthID

		fb.Key.SourceTypeID = parse.GetInt(parts[0])
		fb.Key.RoadTypeID = parse.GetInt(parts[1])
		// avgSpeedBinID := parse.GetInt(parts[2]) //EM - this line is outdated due to rates fix EMT-809 12/20/2018
		//EM - as part of EMT-809 rates fix, AvgSpeedBinID should be read into the MWOKey object now 12/20/2018
		fb.Key.AvgSpeedBinID = parse.GetInt(parts[2])
		fb.Key.HourDayID = parse.GetInt(parts[3])
		fb.Key.DayID = fb.Key.HourDayID % 10
		fb.Key.HourID = fb.Key.HourDayID / 10
		fb.Key.PolProcessID = parse.GetInt(parts[4])
		fb.Key.PollutantID = parse.GetInt(parts[5])
		fb.Key.ProcessID = parse.GetInt(parts[6])
		fb.Key.ModelYearID = parse.GetInt(parts[7])
		fb.Key.FuelTypeID = parse.GetInt(parts[8])
		fb.Key.RegClassID = parse.GetInt(parts[9])
		fb.OpMode.OpModeID = parse.GetInt(parts[10])
		meanBaseRate := parse.GetFloat(parts[11])
		meanBaseRateIM := parse.GetFloat(parts[12])
		emissionRate := parse.GetFloat(parts[13])
		emissionRateIM := parse.GetFloat(parts[14])
		meanBaseRateACAdj := parse.GetFloat(parts[15])
		meanBaseRateIMACAdj := parse.GetFloat(parts[16])
		emissionRateACAdj := parse.GetFloat(parts[17])
		emissionRateIMACAdj := parse.GetFloat(parts[18])
		fb.OpMode.GeneralFraction = parse.GetFloat(parts[19])
		fb.OpMode.GeneralFractionRate = parse.GetFloat(parts[20])

		fsDetail := mwo.FuelSupply[mwo.FuelSupplyKey{fb.Key.CountyID, fb.Key.YearID, fb.Key.MonthID, fb.Key.FuelTypeID}]
		if fsDetail == nil {
			return
		}
		for _, fsd := range fsDetail {
			br := new(mwo.MWOBaseRate)
			//br.AvgSpeedBinID = avgSpeedBinID //EM - this line is outdated as AvgSpeedBinID is in the MWOKey now from EMT-809 12/20/2018
			br.FuelSubTypeID = fsd.FuelSubTypeID
			br.FuelFormulationID = fsd.FuelFormulationID
			br.MarketShare = fsd.MarketShare
			br.MeanBaseRate = meanBaseRate
			br.MeanBaseRateIM = meanBaseRateIM
			br.EmissionRate = emissionRate
			br.EmissionRateIM = emissionRateIM
			br.MeanBaseRateACAdj = meanBaseRateACAdj
			br.MeanBaseRateIMACAdj = meanBaseRateIMACAdj
			br.EmissionRateACAdj = emissionRateACAdj
			br.EmissionRateIMACAdj = emissionRateIMACAdj
			fb.OpMode.BaseRates = append(fb.OpMode.BaseRates, br)
		}

		fb.Key.CalcIDs()
		globalevents.MWOBlockCreated()
		outputBlocks <- b // queue the newly parsed block
	})
	fmt.Println("baseratecalculator.streamBaseRate recordCount=", recordCount)
}

// Perform base rate calculations. Read from the inputBlocks channel.
// Process until the inputBlocks channel is closed then quit.
func calculateAndAccumulate(inputBlocks chan *mwo.MWOBlock) {
	fmt.Println("baseratecalculator.calculateAndAccumulate starting...")
	recordCount := 0
	for {
		b := <-inputBlocks
		if b == nil {
			break
		}

		var newFuelBlocks []*mwo.FuelBlock // created on demand in order to save memory
		for _, fb := range b.FuelBlocks {
			ppmy := PollutantProcessMappedModelYear[PollutantProcessMappedModelYearKey{fb.Key.PolProcessID, fb.Key.ModelYearID}]
			zmh := ZoneMonthHour[ZoneMonthHourKey{fb.Key.MonthID, fb.Key.ZoneID, fb.Key.HourID}]
			nha := NOxHumidityAdjust[fb.Key.FuelTypeID]
			ft := mwo.FuelTypes[fb.Key.FuelTypeID]
			// @algorithm Extended Idle hourly rates have not been scaled by the Extended Idle operating mode (200) fraction.
			// Inventory, but not emission rates, must be multiplied by the opModeFraction for opMode 200
			// hourFractionAdjust is opModeFraction for opMode 200.
			// @condition Extended Idle process hourly rates
			// @input ExtendedIdleEmissionRateFraction
			if fb.Key.ProcessID == 90 && len(ExtendedIdleEmissionRateFraction) > 0 {
				hourFractionAdjust, found := ExtendedIdleEmissionRateFraction[ExtendedIdleFractionKey{fb.Key.ModelYearID, fb.Key.FuelTypeID}]
				if found {
					for _, br := range fb.OpMode.BaseRates {
						// Note: Do not change EmissionRate* values here, just MeanBaseRate* fields.
						// ----- This is a quirk of the APU calculations, not a general statement.
						br.MeanBaseRate *= hourFractionAdjust
						br.MeanBaseRateIM *= hourFractionAdjust
						br.MeanBaseRateACAdj *= hourFractionAdjust
						br.MeanBaseRateIMACAdj *= hourFractionAdjust
					}
				}
			}
			// @algorithm APU hourly rates have not been scaled by the APU operating mode (201) fraction.
			// Inventory, but not emission rates, must be multiplied by the opModeFraction for opModes 201
			// hourFractionAdjust is opModeFraction for opMode 201
			// @condition APU process hourly rates and opModeID == 201
			// @input apuEmissionRateFraction
			if fb.Key.ProcessID == 91 && fb.OpMode.OpModeID == 201 && len(apuEmissionRateFraction) > 0 {
				hourFractionAdjust, found := apuEmissionRateFraction[APUFractionKey{fb.Key.ModelYearID, fb.Key.FuelTypeID}]
				if found {
					for _, br := range fb.OpMode.BaseRates {
						// Note: Do not change EmissionRate* values here, just MeanBaseRate* fields.
						// ----- This is a quirk of the APU calculations, not a general statement.
						br.MeanBaseRate *= hourFractionAdjust
						br.MeanBaseRateIM *= hourFractionAdjust
						br.MeanBaseRateACAdj *= hourFractionAdjust
						br.MeanBaseRateIMACAdj *= hourFractionAdjust
					}
				}
			}
			// @algorithm Shorepower hourly rates have not been scaled by the Shorepower operating mode (203) fraction.
			// Inventory, but not emission rates, must be multiplied by the opModeFraction for opModes 203. Also,
			// change processID to a "new" processID 93, so that the output for this operating mode isn't aggregated together with opModeID 201
			// hourFractionAdjust is opModeFraction for opMode 203
			// @condition APU process hourly rates and opModeID == 203
			// @input ShorepowerEmissionRateFraction
			if fb.Key.ProcessID == 91 && fb.OpMode.OpModeID == 203 && len(ShorepowerEmissionRateFraction) > 0 {
				fb.Key.ProcessID = 93
				hourFractionAdjust, found := ShorepowerEmissionRateFraction[ShorepowerFractionKey{fb.Key.ModelYearID, fb.Key.FuelTypeID}]
				if found {
					for _, br := range fb.OpMode.BaseRates {
						// Note: Do not change EmissionRate* values here, just MeanBaseRate* fields.
						// ----- This is a quirk of the APU calculations, not a general statement.
						br.MeanBaseRate *= hourFractionAdjust
						br.MeanBaseRateIM *= hourFractionAdjust
						br.MeanBaseRateACAdj *= hourFractionAdjust
						br.MeanBaseRateIMACAdj *= hourFractionAdjust
					}
				}
			}
			// Apply temperature adjustments for Starts (2)
			if fb.Key.ProcessID == 2 {
				if zmh != nil && ft != nil && ppmy != nil {
					sta := StartTempAdjustment[StartTempAdjustmentKey{fb.Key.FuelTypeID, fb.Key.PolProcessID, ppmy.modelYearGroupID, fb.OpMode.OpModeID}]
					if sta != nil {
						// @algorithm Do Start Temperature adjustments by opModeID. PM uses multiplicative factors.
						// Everything else uses additive factors.
						// The additive part needs to be weighted by opModeFraction (stored in generalFraction).  Being a rate, sourceBinActivityFraction
						// is not required for the weighting since activity would have been weighted similarly.
						// For polProcessIDs (11202,11802): rate = rate*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC.
						// For all other polProcessIDs with startTempEquationType of 'LOG': rate = rate + generalFraction * (tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC).
						// For all other polProcessIDs with startTempEquationType of 'POLY': rate = rate + generalFraction * ((LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))).
						// @condition Start Exhaust (2) process.
						for _, br := range fb.OpMode.BaseRates {
							br.MeanBaseRate = sta.startTempAdjust(br.MeanBaseRate, fb.Key.PolProcessID, fb.OpMode.GeneralFraction, zmh.temperature)
							br.MeanBaseRateIM = sta.startTempAdjust(br.MeanBaseRateIM, fb.Key.PolProcessID, fb.OpMode.GeneralFraction, zmh.temperature)
							br.EmissionRate = sta.startTempAdjust(br.EmissionRate, fb.Key.PolProcessID, fb.OpMode.GeneralFractionRate, zmh.temperature)
							br.EmissionRateIM = sta.startTempAdjust(br.EmissionRateIM, fb.Key.PolProcessID, fb.OpMode.GeneralFractionRate, zmh.temperature)
							br.MeanBaseRateACAdj = sta.startTempAdjust(br.MeanBaseRateACAdj, fb.Key.PolProcessID, fb.OpMode.GeneralFraction, zmh.temperature)
							br.MeanBaseRateIMACAdj = sta.startTempAdjust(br.MeanBaseRateIMACAdj, fb.Key.PolProcessID, fb.OpMode.GeneralFraction, zmh.temperature)
							br.EmissionRateACAdj = sta.startTempAdjust(br.EmissionRateACAdj, fb.Key.PolProcessID, fb.OpMode.GeneralFractionRate, zmh.temperature)
							br.EmissionRateIMACAdj = sta.startTempAdjust(br.EmissionRateIMACAdj, fb.Key.PolProcessID, fb.OpMode.GeneralFractionRate, zmh.temperature)
						}
					}
				}
			}
			// @algorithm Apply the County's GPAFract to the general fuel adjustment,
			// fuelEffectRatio=ifnull(fuelEffectRatio,1)+GPAFract*(ifnull(fuelEffectRatioGPA,1)-ifnull(fuelEffectRatio,1))
			// then apply GeneralFuelRatio to the rates. rate = rate * fuelEffectRatio.
			if len(GeneralFuelRatio) > 0 {
				for _, br := range fb.OpMode.BaseRates {
					gr := GeneralFuelRatio[GeneralFuelRatioKey{br.FuelFormulationID, fb.Key.PolProcessID, fb.Key.SourceTypeID}]
					if gr != nil {
						for _, grd := range gr.details {
							if fb.Key.ModelYearID >= grd.minModelYearID &&
								fb.Key.ModelYearID <= grd.maxModelYearID &&
								fb.Key.AgeID >= grd.minAgeID &&
								fb.Key.AgeID <= grd.maxAgeID {
								r := grd.fuelEffectRatio + County[fb.Key.CountyID].GPAFract*(grd.fuelEffectRatioGPA-grd.fuelEffectRatio)
								br.MeanBaseRate *= r
								br.MeanBaseRateIM *= r
								br.EmissionRate *= r
								br.EmissionRateIM *= r
								br.MeanBaseRateACAdj *= r
								br.MeanBaseRateIMACAdj *= r
								br.EmissionRateACAdj *= r
								br.EmissionRateIMACAdj *= r
							}
						}
					}
				}
			}
			// @algorithm Apply the County's GPAFract to the criteria ratio,
			// criteria ratio=ifnull(ratio,1)+GPAFract*(ifnull(ratioGPA,1)-ifnull(ratio,1))
			// then apply criteria ratio to the rates.
			// rate = rate * criteria ratio[fuelTypeID,fuelFormulationID,polProcessID,sourceTypeID,modelYearID,ageID]
			// @condition Running Exhaust (1) and Start Exhaust (2).
			if (fb.Key.ProcessID == 1 || fb.Key.ProcessID == 2) && len(CriteriaRatio) > 0 {
				for _, br := range fb.OpMode.BaseRates {
					cr := CriteriaRatio[CriteriaRatioKey{br.FuelFormulationID, fb.Key.PolProcessID, fb.Key.SourceTypeID, fb.Key.ModelYearID, fb.Key.AgeID}]
					if cr != nil {
						r := cr.ratio + County[fb.Key.CountyID].GPAFract*(cr.ratioGPA-cr.ratio)
						br.MeanBaseRate *= r
						br.MeanBaseRateIM *= r
						br.EmissionRate *= r
						br.EmissionRateIM *= r
						br.MeanBaseRateACAdj *= r
						br.MeanBaseRateIMACAdj *= r
						br.EmissionRateACAdj *= r
						br.EmissionRateIMACAdj *= r
					}
				}
			}
			// @algorithm Calculate humidity adjustment factor K and apply temperature adjustment.
			// For processes (1,2) and pollutants (118,112): rate=rate*exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end)).
			// For all others: rate=rate*((1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)).
			// @input TemperatureAdjustment
			var ta *TemperatureAdjustmentDetail
			var taUsingDefault bool
			if len(TemperatureAdjustment) > 0 {
				ta = TemperatureAdjustment[TemperatureAdjustmentKey{fb.Key.PolProcessID, fb.Key.FuelTypeID, fb.Key.ModelYearID}]
			}
			if ta == nil {
				ta = &defaultTemperatureAdjustment
				taUsingDefault = true
			}

			if zmh != nil && ft != nil && ta != nil {
				// if we check for the nox adjustment being nil in the line above with the other objects,
				// we will skip the temp adjustment for EVs
				var k float64 = 1.0
				if nha != nil {
					k = calculateNOxK(zmh, nha)
				}
				for _, br := range fb.OpMode.BaseRates {
					factor := ta.generalTempAdjust(fb.Key.ProcessID, fb.Key.PollutantID, fb.Key.FuelTypeID, fb.Key.SourceTypeID, fb.Key.RegClassID, fb.Key.ModelYearID, k, zmh.temperature, zmh.heatIndex, br.MeanBaseRate, taUsingDefault)
					br.MeanBaseRate *= factor
					br.MeanBaseRateIM *= factor
					br.EmissionRate *= factor
					br.EmissionRateIM *= factor
					br.MeanBaseRateACAdj *= factor
					br.MeanBaseRateIMACAdj *= factor
					br.EmissionRateACAdj *= factor
					br.EmissionRateIMACAdj *= factor
				}
			}

			if fb.Key.ProcessID != 2 {
				// @algorithm Apply air conditioning.
				// meanBaseRate = meanBaseRate + (meanBaseRateACAdj * ACFactor[hourID,sourceTypeID,modelYearID]).
				// meanBaseRateIM = meanBaseRateIM + (meanBaseRateIMACAdj * ACFactor[hourID,sourceTypeID,modelYearID]).
				// emissionRate = emissionRate + (emissionRateACAdj * ACFactor[hourID,sourceTypeID,modelYearID]).
				// emissionRateIM = emissionRateIM + (emissionRateIMACAdj * ACFactor[hourID,sourceTypeID,modelYearID]).
				// @condition Not Start Exhaust (2).
				// @input zoneACFactor

				factor, found := ZoneACFactor[ZoneACFactorKey{fb.Key.HourID, fb.Key.SourceTypeID, fb.Key.ModelYearID}]
				if found && factor > 0 {
					for _, br := range fb.OpMode.BaseRates {
						br.MeanBaseRate += factor * br.MeanBaseRateACAdj
						br.MeanBaseRateIM += factor * br.MeanBaseRateIMACAdj
						br.EmissionRate += factor * br.EmissionRateACAdj
						br.EmissionRateIM += factor * br.EmissionRateIMACAdj
					}
				}
			}

			// @algorithm Apply I/M programs.
			// meanBaseRate=meanBaseRateIM*IMAdjustFract + meanBaseRate*(1-IMAdjustFract).
			// emissionRate=emissionRateIM*IMAdjustFract + emissionRate*(1-IMAdjustFract).
			IMAdjustFract, found := IMCoverage[IMCoverageKey{fb.Key.PolProcessID, fb.Key.ModelYearID, fb.Key.SourceTypeID, fb.Key.FuelTypeID}]
			if found {
				for _, br := range fb.OpMode.BaseRates {
					br.MeanBaseRate = (1.0-IMAdjustFract)*br.MeanBaseRate + IMAdjustFract*br.MeanBaseRateIM
					br.EmissionRate = (1.0-IMAdjustFract)*br.EmissionRate + IMAdjustFract*br.EmissionRateIM
				}
			}

			// Apply the EmissionRateAdjustment before duplicating records for E85 THC below
			if mwo.NeedsModule("BRC_EmissionRateAdjustment") && len(EmissionRateAdjustment) > 0 {
				// @algorithm emissionRate=emissionRate*EmissionRateAdjustment,
				// meanbaserate=meanbaserate*EmissionRateAdjustment
				// @input EmissionRateAdjustment
				for _, fb := range b.FuelBlocks {
					a, found := EmissionRateAdjustment[EmissionRateAdjustmentKey{fb.Key.PolProcessID, fb.Key.SourceTypeID, fb.Key.RegClassID, fb.Key.FuelTypeID, fb.Key.ModelYearID}]
					if found {
						for _, br := range fb.OpMode.BaseRates {
							br.MeanBaseRate *= a
							br.EmissionRate *= a
						}
					}
				}
			}

			// Handle E85 THC that is created from E10's RVP instead of E85's RVP.
			if (fb.Key.ProcessID == 1 || fb.Key.ProcessID == 2) && len(AltCriteriaRatio) > 0 && fb.Key.ModelYearID >= 2001 {
				var newFB *mwo.FuelBlock
				for _, br := range fb.OpMode.BaseRates {
					fuelFormulation := mwo.FuelFormulations[br.FuelFormulationID]
					if fuelFormulation == nil {
						continue
					}
					if fuelFormulation.FuelSubTypeID == 51 || fuelFormulation.FuelSubTypeID == 52 {
						acr := AltCriteriaRatio[CriteriaRatioKey{br.FuelFormulationID, fb.Key.PolProcessID, fb.Key.SourceTypeID, fb.Key.ModelYearID, fb.Key.AgeID}]
						cr := CriteriaRatio[CriteriaRatioKey{br.FuelFormulationID, fb.Key.PolProcessID, fb.Key.SourceTypeID, fb.Key.ModelYearID, fb.Key.AgeID}]
						if cr != nil && acr != nil {
							// @algorithm Apply the County's GPAFract to the alternate criteria ratio and criteria ratio,
							// alternate criteria ratio=ifnull(ratio,1)+GPAFract*(ifnull(ratioGPA,1)-ifnull(ratio,1)).
							// criteria ratio=ifnull(ratio,1)+GPAFract*(ifnull(ratioGPA,1)-ifnull(ratio,1)).
							// Determine the scaling effect of E10-RVP-based fuel effects to E85-RVP-based fuel effects.
							// alt criteria scale = alternate criteria ratio / criteria ratio
							// @condition Running Exhaust (1) and Start Exhaust (2).
							// @input criteriaRatio
							// @input altCriteriaRatio
							ar := acr.ratio + County[fb.Key.CountyID].GPAFract*(acr.ratioGPA-acr.ratio)
							r := cr.ratio + County[fb.Key.CountyID].GPAFract*(cr.ratioGPA-cr.ratio)
							var arToR float64
							if r > 0 {
								arToR = ar / r
							} else {
								arToR = 0
							}

							// @algorithm Make THC records from the E10 RVP by using the E85-based THC.
							// The output pollutant is 10001.
							// rate for pollutant 10001 = rate * alt criteria scale.
							// @condition Running Exhaust (1) and Start Exhaust (2).

							// Make a new FuelBlock if one was not made yet
							if newFB == nil {
								newFB = mwo.NewFuelBlock(fb)
								newFB.SetupForBaseRates()
								newFB.Key.PollutantID = 10000 + fb.Key.PollutantID
								newFB.Key.PolProcessID = newFB.Key.PollutantID*100 + newFB.Key.ProcessID

								newFB.OpMode.OpModeID = fb.OpMode.OpModeID
								newFB.OpMode.GeneralFraction = fb.OpMode.GeneralFraction
								newFB.OpMode.GeneralFractionRate = fb.OpMode.GeneralFractionRate
							}
							// Add a new BaseRate object to the new FuelBlock,
							// scaling the rates by arToR.
							newBR := new(mwo.MWOBaseRate)
							*newBR = *br
							newBR.MeanBaseRate *= arToR
							newBR.EmissionRate *= arToR
							newFB.OpMode.BaseRates = append(newFB.OpMode.BaseRates, newBR)
						}
					}
				}
				if newFB != nil {
					// Store newFB into newFuelBlocks
					if newFuelBlocks == nil {
						newFuelBlocks = make([]*mwo.FuelBlock, 0, 3)
					}
					newFuelBlocks = append(newFuelBlocks, newFB)
				}
			}
			// Done for now. Other steps require operation on all fuel blocks,
			// including the new one(s) created above.
		}
		// Add new fuel blocks to the main block
		if newFuelBlocks != nil {
			for _, nfb := range newFuelBlocks {
				b.AddFuelBlock(nfb)
			}
		}

		if mwo.NeedsModule("BRC_evefficiency") && len(EVEfficiency) > 0 {
			// @algorithm emissionRate=emissionRate/batteryEfficiency*chargingEfficiency,
			// meanbaserate=meanbaserate/batteryEfficiency*chargingEfficiency
			// @input EVEfficiency
			for _, fb := range b.FuelBlocks {
				a, found := EVEfficiency[EVEfficiencyKey{fb.Key.PolProcessID, fb.Key.SourceTypeID, fb.Key.RegClassID, fb.Key.FuelTypeID, fb.Key.ModelYearID}]
				if found {
					if a.batteryEfficiency*a.chargingEfficiency != 0 {
						for _, br := range fb.OpMode.BaseRates {
							br.MeanBaseRate /= (a.batteryEfficiency * a.chargingEfficiency)
							br.EmissionRate /= (a.batteryEfficiency * a.chargingEfficiency)
						}
					} else {
						fmt.Println("ERROR: cannot apply an efficiency of 0 (divide by 0 error)")
					}
				}
			}
		}

		// Update the set of unique FuelBlock keys, omitting operating mode
		// Lock the global data structure
		uniqueFuelBlocksGuard.Lock()
		for _, fb := range b.FuelBlocks {
			// Find the entry with fb.Key, or make this FuelBlock the data.
			nfb, found := uniqueFuelBlocks[fb.Key]
			if found {
				// Append fb's OpMode.BaseRates to the list associated with the key
				for _, br := range fb.OpMode.BaseRates {
					nfb.OpMode.BaseRates = append(nfb.OpMode.BaseRates, br)
				}
			} else {
				// Make the current fuel block the data since this is the first
				// use of the key.
				uniqueFuelBlocks[fb.Key] = fb
			}
		}
		// Unlock the global data structure
		uniqueFuelBlocksGuard.Unlock()
		// Remove all fuel blocks from the current MWOBlock
		b.FuelBlocks = b.FuelBlocks[:0]
		// Discard the current MWOBlock, decrementing the global counter
		b.Recycle()
		globalevents.MWOBlockDone()
		recordCount++

		// Decrement the fuel blocks count status and queue any notices of work being done
		atomic.AddInt32(&fuelBlockCount, -1)
		if fuelBlockCount <= 0 && fuelBlockReaderCount <= 0 {
			fuelBlocksDone <- 1
		}
	}
	fmt.Println("baseratecalculator.calculateAndAccumulate done, recordCount=", recordCount)
}

// Apply start temperature adjustment equations.
func (this *StartTempAdjustmentDetail) startTempAdjust(baseValue float64,
	polProcessID int, weightFraction, temperature float64) float64 {
	if polProcessID == 11202 || polProcessID == 11802 {
		return baseValue*this.tempAdjustTermB*math.Exp(this.tempAdjustTermA*(72.0-math.Min(temperature, 72))) + this.tempAdjustTermC
	} else if this.isLog {
		return baseValue + weightFraction*(this.tempAdjustTermB*math.Exp(this.tempAdjustTermA*(math.Min(temperature, 75)-75))+this.tempAdjustTermC)
	} else if this.isPoly {
		return baseValue + weightFraction*(math.Min(temperature, 75)-75)*(this.tempAdjustTermA+(math.Min(temperature, 75)-75)*(this.tempAdjustTermB+(math.Min(temperature, 75)-75)*this.tempAdjustTermC))
	} else {
		return baseValue + weightFraction*(math.Min(temperature, 75)-75)*(this.tempAdjustTermA+(math.Min(temperature, 75)-75)*(this.tempAdjustTermB+(math.Min(temperature, 75)-75)*this.tempAdjustTermC))
	}
}

// Apply general temperature adjustment equations.
func (this *TemperatureAdjustmentDetail) generalTempAdjust(processID, pollutantID, fuelTypeID, sourceTypeID, regClassID, modelYearID int,
	k, temperature, heatIndex, baserate float64, taUsingDefault bool) float64 {
	// This block does PM
	if (processID == 1 || processID == 2) && (pollutantID == 118 || pollutantID == 112) {
		if temperature <= 72.0 {
			return math.Exp(this.tempAdjustTermA * (72.0 - temperature))
		} else {
			return 1.0
		}
	}

	// This block adjusts running EV energy consumption for temperature
	if processID == 1 && fuelTypeID == 9 && pollutantID == 91 {
		// only apply a temperature adjustment for light duty for cold temperatures
		// AC usage is defined by the coefficients in monthgrouphour and the heat index
		// we only apply the cold adjustment if the AC usage is "less than 0", meaning it's cold outside
		if sourceTypeID < 40 {
			var adj float64
			// this is the light duty adjustment, which we only apply if the temperature is low
			if heatIndex > 67 {
				adj = 0
			} else {
				adj = (temperature - 72.0) * (this.tempAdjustTermA + this.tempAdjustTermB*(temperature-72.0))
			}
			if adj < 0 {
				adj = 0
			}
			// This is commented out because it breaks the rates-inventory reconciliation. Because inventory mode aggregates
			// speed bins together very early in the calculation process, in practice the baserate is never < 0. Rates
			// mode runs at the speed bin level, so in practice baserate can be less than 0 (and often is). Because the
			// modes run this block at different levels of aggregation, the rates and inventories don't match.
			// The original intent of this block is to make sure MOVES doesn't implicitly assume regen braking is more
			// effective in cold temperatures. At project scale, this adjustment can be important to make sure realistic
			// rates are returned for links with high amounts of braking. In the future, we would like to activate this
			// code for project scale, but not county or default scale.
			// If we do put this in for project scale, the code needs to be activated here and in the heavy-duty block
			// about 10 below.
			/* if baserate < 0 {
				return 1.0 - adj
			} */
			return 1.0 + adj
		}
		// this is the heavy-duty adjustment, which is applied at all temperatures
		adj := (temperature - 72.0) * (this.tempAdjustTermA + this.tempAdjustTermC*(temperature-72.0))
		if adj < 0 {
			adj = 0
		}
		/* if baserate < 0 {
			return 1.0 - adj
		} */
		return 1.0 + adj
	}

	// This block handles NOx for running, extended idle, and APUs
	if (processID == 1 || processID == 90 || processID == 91) && pollutantID == 3 {
		var tempAdjust float64

		// diesel temperature adjustment is based on reg class
		// note: no adjustment for HD above 25C; no adjustment at all for LD
		if fuelTypeID == 2 {
			switch regClassID {
			case 42:
				tempAdjust = (77.0 - temperature) * (this.tempAdjustTermA)
				if temperature > 77.0 {
					tempAdjust = 0
				}
			case 46:
				tempAdjust = (77.0 - temperature) * (this.tempAdjustTermB)
				if temperature > 77.0 {
					tempAdjust = 0
				}
			case 47:
				tempAdjust = (77.0 - temperature) * (this.tempAdjustTermC)
				if temperature > 77.0 {
					tempAdjust = 0
				}
			case 48:
				// hardcoded because we don't have a tempAdjustTermD column in temperatureadjustment to use
				if modelYearID >= 2027 && !taUsingDefault {
					tempAdjust = (77.0 - temperature) * (0.008396619)
					if temperature > 77.0 {
						tempAdjust = 0
					}
				} else {
					tempAdjust = 0
				}
			default:
				tempAdjust = 0
			}
		} else { // adjustments for all fuels other than diesel
			tempAdjust = (temperature - 75.0) * (this.tempAdjustTermA + (temperature-75.0)*this.tempAdjustTermB)
		}

		// apply humidity adjustment
		return (1.0 + tempAdjust) * k
	}

	// Finally, use the standard calculation for all other cases
	return 1.0 + (temperature-75.0)*(this.tempAdjustTermA+(temperature-75.0)*this.tempAdjustTermB)
}

// calculateNOxK calculates the k variable that is used to adjust NOx emissions in the genearlTempAdjust function,
// based on values from zonemonthhour and noxhumidityadjust
func calculateNOxK(zmh *ZoneMonthHourDetail, nha *NOxHumidityAdjustDetail) float64 {
	// the input humidity is in grams H2O per kilogram of dry air
	specHum_gPerKG := zmh.specificHumidity
	specHum_moleFrac := zmh.molWaterFraction

	switch nha.humidityNOxEq {
	case "CFR 86":
		// enforce the bounds
		if specHum_gPerKG < nha.humidityLowBound {
			specHum_gPerKG = nha.humidityLowBound
		}
		if specHum_gPerKG > nha.humidityUpBound {
			specHum_gPerKG = nha.humidityUpBound
		}

		// calculate the equation
		return 1.0 - nha.humidityTermA*(specHum_gPerKG-10.71)
	case "CFR 1065":
		// enforce the bounds
		if specHum_moleFrac < nha.humidityLowBound {
			specHum_moleFrac = nha.humidityLowBound
		}
		if specHum_moleFrac > nha.humidityUpBound {
			specHum_moleFrac = nha.humidityUpBound
		}

		// calculate the equation
		return 1.0 / (nha.humidityTermA*specHum_moleFrac + nha.humidityTermB)
	}

	return 1.0
}

// Stream accumulated fuel blocks to a queue and functions that will finish the
// aggregation by operating mode.
func disburseAccumulatedBlocks(internalQueueAfterAccumulator chan *mwo.MWOBlock) {
	fmt.Println("baseratecalculator disburseAccumulatedBlocks starting...")
	fmt.Println("baseratecalculator len(uniqueFuelBlocks)=", len(uniqueFuelBlocks))
	for _, fb := range uniqueFuelBlocks {
		b := mwo.New() // get a blank mwoBlock
		globalevents.MWOBlockCreated()
		b.AddFuelBlock(fb)
		internalQueueAfterAccumulator <- b
	}
	// Clear the accumulated data
	uniqueFuelBlocks = make(map[mwo.MWOKey]*mwo.FuelBlock)
	fmt.Println("baseratecalculator disburseAccumulatedBlocks done.")
}

// Aggregate rates to eliminate operating mode detail.
func aggregateOpModes(fb *mwo.FuelBlock) {
	// @algorithm Remove opModeID.
	// emissionQuant = sum(meanBaseRate * marketShare).
	// emissionRate = sum(emissionRate * marketShare).

	emissions := make(map[int]*mwo.MWOEmission) // Emissions by FuelFormulationID
	for _, br := range fb.OpMode.BaseRates {
		e := emissions[br.FuelFormulationID]
		if e == nil {
			e = mwo.NewEmission()
			e.FuelSubTypeID = br.FuelSubTypeID
			e.FuelFormulationID = br.FuelFormulationID
			emissions[br.FuelFormulationID] = e
		}
		e.EmissionQuant += br.MeanBaseRate * br.MarketShare
		e.EmissionRate += br.EmissionRate * br.MarketShare
	}
	fb.Emissions = make([]*mwo.MWOEmission, 0, maxInt(1, len(emissions)))
	for _, e := range emissions {
		fb.AddEmission(e)
	}
	// Eliminate the operating mode data. All subsequent steps
	// should use the emissions, which have detail by fuel formulation.
	fb.OpMode = nil
}

// Calculate activity weights
func calculateActivityWeight(readyChannel chan int) {
	defer globalevents.MWOReadDone()

	fmt.Println("baseratecalculator calculateActivityWeight starting...")
	if !mwo.NeedsModule("BRC_AggregateSMFR") {
		fmt.Println("baseratecalculator calculateActivityWeight done because it is not needed")
		// Let the caller know that activity weight is ready
		readyChannel <- 1
		return
	}

	activityTotal := make(map[activityWeightKey]*activityWeightDetail)

	// Read the smfrSBDSummary file
	if _, err := os.Stat("smfrsbdsummary"); err == nil {
		parse.ReadAndParseFile("smfrsbdsummary", func(parts []string) {
			//sourceTypeID, modelYearID, fuelTypeID, regClassID
			//sbdTotal
			if len(parts) < 5 {
				return
			}
			sourceTypeID := parse.GetInt(parts[0])
			modelYearID := parse.GetInt(parts[1])
			fuelTypeID := parse.GetInt(parts[2])
			regClassID := parse.GetInt(parts[3])
			sbdTotal := parse.GetFloat(parts[4])
			// Find all matching universalActivity records
			for hourDayID, _ := range universalActivityHourDayIDs {
				activity, found := universalActivity[universalActivityKey{hourDayID, modelYearID, sourceTypeID}]
				if !found {
					continue
				}
				k := activityWeightKey{hourDayID, modelYearID, sourceTypeID, fuelTypeID, regClassID}
				d, found := activityWeight[k]
				if !found {
					d = new(activityWeightDetail)
					activityWeight[k] = d
				}
				d.smfrFraction += activity * sbdTotal
				d.smfrRatesFraction += activity * sbdTotal
			}
		})
	}
	fmt.Println("baseratecalculator calculateActivityWeight len(activityWeight)=", len(activityWeight))

	if mwo.NeedsModule("BRC_AdjustExtendedIdleEmissionRate") {
		// @algorithm When aggregating ExtendedIdle emission rates to remove source type, model year, fuel type, or regclass,
		// the activity used to weight the rates must be adjusted. The input activity includes extended idling
		// and instead must be restricted to just hours spent using a diesel APU. This is a model year-based effect.
		for k, d := range activityWeight {
			eiAdjust, found := ExtendedIdleEmissionRateFraction[ExtendedIdleFractionKey{k.modelYearID, k.fuelTypeID}]
			if found && eiAdjust != 0 {
				d.smfrRatesFraction /= eiAdjust // is "*=" when eiAdjust == 1/opModeFraction
			}
		}
	}
	if mwo.NeedsModule("BRC_AdjustAPUEmissionRate") {
		// @algorithm When aggregating APU emission rates to remove source type, model year, fuel type, or regclass,
		// the activity used to weight the rates must be adjusted. The input activity includes all hotelling
		// and instead must be restricted to just hours spent using a diesel APU or shorepower. This is a model year-based effect.
		for k, d := range activityWeight {
			apuAdjust, _ := apuEmissionRateFraction[APUFractionKey{k.modelYearID, k.fuelTypeID}]
			spAdjust, _ := ShorepowerEmissionRateFraction[ShorepowerFractionKey{k.modelYearID, k.fuelTypeID}]
			adjustment := apuAdjust + spAdjust
			if adjustment != 0 {
				d.smfrRatesFraction /= adjustment
			}
		}
	}

	// Aggregate activity according to runspec flags
	discardSourceTypeID := mwo.NeedsModule("BRC_DiscardSourceTypeID")
	discardModelYearID := mwo.NeedsModule("BRC_DiscardModelYearID")
	discardFuelTypeID := mwo.NeedsModule("BRC_DiscardFuelTypeID")
	discardRegClassID := mwo.NeedsModule("BRC_DiscardRegClassID")
	for dk, dd := range activityWeight {
		tk := activityWeightKey{dk.hourDayID, dk.modelYearID, dk.sourceTypeID, dk.fuelTypeID, dk.regClassID}
		if discardSourceTypeID {
			tk.sourceTypeID = 0
		}
		if discardModelYearID {
			tk.modelYearID = 0
		}
		if discardFuelTypeID {
			tk.fuelTypeID = 0
		}
		if discardRegClassID {
			tk.regClassID = 0
		}
		td := activityTotal[tk]
		if td == nil {
			td = new(activityWeightDetail)
			activityTotal[tk] = td
		}
		td.smfrFraction += dd.smfrFraction
		td.smfrRatesFraction += dd.smfrRatesFraction
	}
	fmt.Println("baseratecalculator calculateActivityWeight len(activityTotal)=", len(activityTotal))

	// @algorithm When aggregating rates to remove source type, model year, fuel type, or regclass, calculate an activity distribution.
	// smfrFraction[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID] = activity[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID] / activityTotal[aggregated]
	for dk, dd := range activityWeight {
		tk := activityWeightKey{dk.hourDayID, dk.modelYearID, dk.sourceTypeID, dk.fuelTypeID, dk.regClassID}
		if discardSourceTypeID {
			tk.sourceTypeID = 0
		}
		if discardModelYearID {
			tk.modelYearID = 0
		}
		if discardFuelTypeID {
			tk.fuelTypeID = 0
		}
		if discardRegClassID {
			tk.regClassID = 0
		}
		td := activityTotal[tk]
		if td == nil {
			dd.smfrFraction = 0
			dd.smfrRatesFraction = 0
		} else {
			if td.smfrFraction > 0 {
				dd.smfrFraction /= td.smfrFraction
			} else {
				dd.smfrFraction = 0
			}
			if td.smfrRatesFraction > 0 {
				dd.smfrRatesFraction /= td.smfrRatesFraction
			} else {
				dd.smfrRatesFraction = 0
			}
		}
	}

	fmt.Println("baseratecalculator calculateActivityWeight done.")
	// Let the caller know that activity weight is ready
	readyChannel <- 1
}

// Aggregate by operating mode and apply activity to generate inventory-ready blocks
func aggregateAndApplyActivity(activityReady chan int, internalQueueAfterAccumulator, outputBlocks chan *mwo.MWOBlock) {
	fmt.Println("baseratecalculator aggregateAndApplyActivity starting...")

	// Wait for activity weights to be ready
	<-activityReady
	// Put back a ready token so others don't wait
	activityReady <- 1
	fmt.Println("baseratecalculator aggregateAndApplyActivity activity is ready")

	aggregateSMFR := mwo.NeedsModule("BRC_AggregateSMFR")
	adjustEmissionRateOnly := mwo.NeedsModule("BRC_AdjustEmissionRateOnly")
	adjustMeanBaseRateAndEmissionRate := mwo.NeedsModule("BRC_AdjustMeanBaseRateAndEmissionRate")
	applyActivity := mwo.NeedsModule("BRC_ApplyActivity")
	recordCount := 0

	for {
		b := <-internalQueueAfterAccumulator
		if b == nil {
			break
		}

		for _, fb := range b.FuelBlocks {
			recordCount++
			aggregateOpModes(fb)
			if aggregateSMFR || applyActivity {
				meanBaseRateScale := 1.0
				emissionRateScale := 1.0
				if aggregateSMFR {
					wd := activityWeight[activityWeightKey{fb.Key.HourDayID, fb.Key.ModelYearID, fb.Key.SourceTypeID, fb.Key.FuelTypeID, fb.Key.RegClassID}]
					if wd != nil {
						if adjustEmissionRateOnly {
							// @algorithm When aggregating rates to remove source type, model year, fuel type, or regclass, weight emissions by the activity distribution.
							// emissionRate = emissionRate * smfrRatesFraction[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID].
							// @condition Starts, Extended Idle, Aux Power Exhaust
							emissionRateScale *= wd.smfrRatesFraction
						} else if adjustMeanBaseRateAndEmissionRate {
							// @algorithm When aggregating rates to remove source type, model year, fuel type, or regclass, weight emissions by the activity distribution.
							// meanBaseRate = meanBaseRate * smfrFraction[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID].
							// emissionRate = emissionRate * smfrRatesFraction[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID].
							// @condition neither Starts, nor Extended Idle, nor Aux Power Exhaust
							meanBaseRateScale *= wd.smfrFraction
							emissionRateScale *= wd.smfrRatesFraction
						}
					}
				}
				if applyActivity {
					// @algorithm When creating an inventory or certain rates, convert BaseRateOutput to an inventory.
					// meanBaseRate = meanBaseRate * activity[processID,hourDayID,modelYearID,sourceTypeID(,month,year,location)]
					activity, found := universalActivity[universalActivityKey{fb.Key.HourDayID, fb.Key.ModelYearID, fb.Key.SourceTypeID}]
					if found {
						meanBaseRateScale *= activity
					}
				}
				fb.ScaleEmissions(meanBaseRateScale, emissionRateScale)
			}
		}

		// Queue the block for chained processing or writing to output
		outputBlocks <- b
	}
	fmt.Println("baseratecalculator aggregateAndApplyActivity done, recordCount=", recordCount)
	globalevents.PrintCounts()
}
