package mwo

import (
	"globalevents"
	"os"
	"bufio"
	"strconv"
	"fmt"
	"sync/atomic"
)

var NeededPolProcessIDs map[int]bool
var NeededModules map[string]bool

var FuelSupply map[FuelSupplyKey][]*FuelSupplyDetail
var NRFuelSupply map[FuelSupplyKey][]*FuelSupplyDetail
var defaultEmissionsPerFuelBlock int

var FuelTypes map[int]*FuelType
var FuelSubTypes map[int]*FuelSubType
var FuelFormulations map[int]*FuelFormulation

var NRFuelTypes map[int]*FuelType
var NRFuelSubTypes map[int]*FuelSubType

var AgeGroups map[int]int // keyed by AgeID, value is AgeGroupID

type FuelSupplyKey struct {
	CountyID, YearID, MonthID, FuelTypeID int
}

type FuelSupplyDetail struct {
	FuelSubTypeID, FuelFormulationID int
	MarketShare float64
	
	FuelFormulation *FuelFormulation
}

type FuelType struct {
	FuelTypeID int
	HumidityCorrectionCoeff, FuelDensity float64
	SubjectToEvapCalculations bool
}

type FuelSubType struct {
	FuelSubTypeID, FuelTypeID int
	FuelSubtypePetroleumFraction, FuelSubtypeFossilFraction, CarbonContent, OxidationFraction, EnergyContent float64
	
	FuelType *FuelType
}

type FuelFormulation struct {
	FuelFormulationID, FuelSubTypeID int
	RVP, SulfurLevel float64
	ETOHVolume, MTBEVolume, ETBEVolume, TAMEVolume float64
	AromaticContent, OlefinContent, BenzeneContent float64
	E200, E300 float64
	VolToWtPercentOxy, BioDieselEsterVolume, CetaneIndex, PAHContent float64
	T50, T90 float64
	
	FuelTypeID int
	FuelSubType *FuelSubType
	
	// HCSpeciation calculator
	ETOHThreshID int
	OxyThreshID int
}

type NRSCCDetail struct {
	SCC string
	NREquipTypeID, FuelTypeID int
}

var NRSCC map[string]*NRSCCDetail

type NRHPCategoryKey struct {
	HPID, EngTechID int
}

var NRHPCategory map[NRHPCategoryKey]uint8

type MWOKey struct {
	YearID, MonthID, DayID, HourID, StateID, CountyID, ZoneID, LinkID, RoadTypeID int
	SourceTypeID, RegClassID, FuelTypeID, ModelYearID int
	SCC string
	EngTechID, SectorID, HPID int
	PollutantID, ProcessID int

	// Derived values often used for lookups
	PolProcessID, HourDayID, AgeID, AgeGroupID int
}

type MWOEmission struct {
	FuelSubTypeID, FuelFormulationID int
	EmissionQuant, EmissionRate float64
}

type FuelSubTypeBlock struct {
	TotalEmissionQuant, TotalEmissionRate float64
}

type FuelBlock struct {
	TotalEmissionQuant, TotalEmissionRate float64
	Key MWOKey
	HasBeenWritten, NeedsGFRE bool
	Emissions []*MWOEmission
	TotalsByFuelSubType map[int]*FuelSubTypeBlock
}

type MWOBlock struct {
	FuelBlocks []*FuelBlock
}

var outputFile *os.File
var outputWriter *bufio.Writer
var linesWritten int32
var emissionBlocksConsidered int32
var fuelBlocksConsidered int32
var mwoBlocksConsidered int32

var detailOutputFile *os.File
var detailOutputWriter *bufio.Writer
var detailLinesWritten int32

var mwoPermissions chan int

func init() {
	NeededPolProcessIDs = make(map[int]bool)
	NeededModules = make(map[string]bool)

	FuelSupply = make(map[FuelSupplyKey][]*FuelSupplyDetail)
	NRFuelSupply = make(map[FuelSupplyKey][]*FuelSupplyDetail)
	defaultEmissionsPerFuelBlock = 20
	
	FuelTypes = make(map[int]*FuelType)
	FuelSubTypes = make(map[int]*FuelSubType)
	FuelFormulations = make(map[int]*FuelFormulation)

	NRFuelTypes = make(map[int]*FuelType)
	NRFuelSubTypes = make(map[int]*FuelSubType)

	AgeGroups = make(map[int]int) // keyed by AgeID, value is AgeGroupID

	NRSCC = make(map[string]*NRSCCDetail)

	NRHPCategory = make(map[NRHPCategoryKey]uint8)
	
	mwoPermissions = make(chan int,10000)
}

func AddBlockPermissions(howManyBlocks int) {
	if(howManyBlocks > cap(mwoPermissions)) {
		howManyBlocks = cap(mwoPermissions)
	}
	for i:=0; i<howManyBlocks; i++ {
		mwoPermissions <- 1
	}
}

func (b *MWOBlock) Recycle() {
	// Add the block and all child blocks to internal queue channels.
	// Remember to clear all slices and reset all variables.
	// TODO
	// Give permission to create another block
	mwoPermissions <- 1
}

func New() *MWOBlock {
	// Get permission to create another block
	<- mwoPermissions

	// TODO Use data from a recycled block, including a recycled slice that has been set to 0 length
	b := new(MWOBlock)
	const defaultFuelBlocksPerMWOBlock = 5
	b.FuelBlocks = make([]*FuelBlock,0,defaultFuelBlocksPerMWOBlock)
	return b
}

func NewFuelBlock(other *FuelBlock) *FuelBlock {
	// TODO Use data from a recycled block, including a recycled slice that has been set to 0 length
	fb := new(FuelBlock)
	fb.TotalEmissionQuant = 0
	fb.TotalEmissionRate = 0
	fb.NeedsGFRE = true
	if other != nil {
		fb.Key = other.Key
		fb.Emissions = make([]*MWOEmission,0,cap(other.Emissions))
	} else {
		// Leave fb.Key with default values
		fb.Emissions = make([]*MWOEmission,0,defaultEmissionsPerFuelBlock)
	}
	if NeedsModule("FuelSubType") {
		fb.TotalsByFuelSubType = make(map[int]*FuelSubTypeBlock)
	}

	return fb
}

func (b *MWOBlock) Add() *FuelBlock {
	fb := NewFuelBlock(nil)
	b.FuelBlocks = append(b.FuelBlocks,fb)
	return fb
}

func (b *MWOBlock) AddFuelBlock(fb *FuelBlock) {
	b.FuelBlocks = append(b.FuelBlocks,fb)
}

func (fb *FuelBlock) Add(fuelSubTypeID, fuelFormulationID int, emissionQuant, emissionRate float64) {
	// TODO Use data from a recycled block
	e := new(MWOEmission)
	e.FuelSubTypeID = fuelSubTypeID
	e.FuelFormulationID = fuelFormulationID
	e.EmissionQuant = emissionQuant
	e.EmissionRate = emissionRate
	fb.TotalEmissionQuant += emissionQuant
	fb.TotalEmissionRate += emissionRate
	fb.Emissions = append(fb.Emissions,e)
	if fb.TotalsByFuelSubType != nil {
		var fst = fb.TotalsByFuelSubType[fuelSubTypeID]
		if fst == nil {
			fst = new(FuelSubTypeBlock)
			fst.TotalEmissionQuant = 0
			fst.TotalEmissionRate = 0
			fb.TotalsByFuelSubType[fuelSubTypeID] = fst
		}
		fst.TotalEmissionQuant += emissionQuant
		fst.TotalEmissionRate += emissionRate
	}
}

func (fb *FuelBlock) AddEmission(e *MWOEmission) {
	fb.TotalEmissionQuant += e.EmissionQuant
	fb.TotalEmissionRate += e.EmissionRate
	fb.Emissions = append(fb.Emissions,e)
	if fb.TotalsByFuelSubType != nil {
		var fst = fb.TotalsByFuelSubType[e.FuelSubTypeID]
		if fst == nil {
			fst = new(FuelSubTypeBlock)
			fst.TotalEmissionQuant = 0
			fst.TotalEmissionRate = 0
			fb.TotalsByFuelSubType[e.FuelSubTypeID] = fst
		}
		fst.TotalEmissionQuant += e.EmissionQuant
		fst.TotalEmissionRate += e.EmissionRate
	}
}

func (fb *FuelBlock) SetAsInput() {
	fb.HasBeenWritten = true
	fb.NeedsGFRE = false
}

func (k *MWOKey) CalcIDs() {
	k.PolProcessID = k.PollutantID * 100 + k.ProcessID
	k.HourDayID = k.HourID * 10 + k.DayID
	k.AgeID = k.YearID - k.ModelYearID
	k.AgeGroupID = AgeGroups[k.AgeID]
}

func NewEmission() *MWOEmission {
	// TODO Use a recycled block
	e := new(MWOEmission)
	e.EmissionQuant = 0
	e.EmissionRate = 0
	return e
}

func NewEmissionScaled(other *MWOEmission, factor float64) *MWOEmission {
	// TODO Use a recycled block
	e := new(MWOEmission)
	e.EmissionQuant = factor * other.EmissionQuant
	e.EmissionRate = factor * other.EmissionRate
	e.FuelSubTypeID = other.FuelSubTypeID
	e.FuelFormulationID = other.FuelFormulationID
	return e
}

func NewEmissionSum(other1 *MWOEmission, other2 *MWOEmission) *MWOEmission {
	if other1 == nil && other2 == nil {
		return nil
	}
	// TODO Use a recycled block
	e := new(MWOEmission)
	if other1 != nil && other2 != nil {
		e.EmissionQuant = other1.EmissionQuant + other2.EmissionQuant
		e.EmissionRate = other1.EmissionRate + other2.EmissionRate
		e.FuelSubTypeID = other1.FuelSubTypeID
		e.FuelFormulationID = other1.FuelFormulationID
	} else if other1 != nil && other2 == nil {
		e.EmissionQuant = other1.EmissionQuant
		e.EmissionRate = other1.EmissionRate
		e.FuelSubTypeID = other1.FuelSubTypeID
		e.FuelFormulationID = other1.FuelFormulationID
	} else if other1 == nil && other2 != nil {
		e.EmissionQuant = other2.EmissionQuant
		e.EmissionRate = other2.EmissionRate
		e.FuelSubTypeID = other2.FuelSubTypeID
		e.FuelFormulationID = other2.FuelFormulationID
	}
	return e
}

func EndOfChain(inputBlocks chan *MWOBlock, blocksToWrite chan *MWOBlock) {
	for i := 0; i<3; i++ {
		go endOfChainCore(inputBlocks,blocksToWrite)
	}
}

func endOfChainCore(inputBlocks chan *MWOBlock, blocksToWrite chan *MWOBlock) {
	for {
		b := <- inputBlocks
		globalevents.Send(globalevents.MWOWriteBegin)
		blocksToWrite <- b
		globalevents.Send(globalevents.MWOBlockEnd)
	}
}

func StartWriting(fileName string, blocksToWrite chan *MWOBlock, needsDetailOutput bool) {
	f, err := os.Create(fileName)
	if err != nil {
		panic(err)
	}
	outputFile = f
	outputWriter = bufio.NewWriter(outputFile) 

	if needsDetailOutput {
		detailFileName := fileName + "_detail"
		df, derr := os.Create(detailFileName)
		if derr != nil {
			panic(derr)
		}
		detailOutputFile = df
		detailOutputWriter = bufio.NewWriter(detailOutputFile)
		fmt.Println("Started writing detail output to: ",detailFileName)
	}
	go writeCore(blocksToWrite)
	fmt.Println("Started writing output to: ",fileName)
}

func FinishWriting() {
	outputWriter.Flush()
	outputFile.Close()
	
	if detailOutputWriter != nil {
		detailOutputWriter.Flush()
	}
	if detailOutputFile != nil {
		detailOutputFile.Close()
	}

	fmt.Printf("Considered %d MWOBlocks for writing.\n",mwoBlocksConsidered)
	fmt.Printf("Considered %d FuelBlocks for writing.\n",fuelBlocksConsidered)
	fmt.Printf("Considered %d MWOEmissions for writing.\n",emissionBlocksConsidered)
	fmt.Printf("Wrote %d lines to the output file.\n",linesWritten)
	fmt.Printf("Wrote %d lines to the detail output file.\n",detailLinesWritten)
}

func writeCore(blocksToWrite chan *MWOBlock) {
	for {
		b := <- blocksToWrite
		atomic.AddInt32(&mwoBlocksConsidered,1)
		for _, fb := range b.FuelBlocks {
			atomic.AddInt32(&fuelBlocksConsidered,1)
			atomic.AddInt32(&emissionBlocksConsidered,int32(len(fb.Emissions)))
			/*
			if fb.Key.PollutantID >= 10000 { // Skip pseudo-pollutants
				continue
			}
			*/
			if detailOutputFile != nil { // If full detailed logging is required at the fuel formulation level...
				/*
				0,1: MOVESRunID,iterationID,
				2,3,4,5: yearID,monthID,dayID,hourID,
				6,7,8,9: stateID,countyID,zoneID,linkID,
				10,11: pollutantID,processID,
				12,13: sourceTypeID,regClassID,
				14,15: fuelTypeID,modelYearID,
				16,17: roadTypeID,SCC,
				18,19,20: engTechID,sectorID,hpID,
				21,22: emissionQuant,emissionRate
				23,24: fuelSubTypeID, fuelFormulationID
				*/
				for _, e := range fb.Emissions {
					line := "0\t0\t" +
						strconv.Itoa(fb.Key.YearID) + "\t" + strconv.Itoa(fb.Key.MonthID) + "\t" + strconv.Itoa(fb.Key.DayID) + "\t" + strconv.Itoa(fb.Key.HourID) + "\t" +
						strconv.Itoa(fb.Key.StateID) + "\t" + strconv.Itoa(fb.Key.CountyID) + "\t" + strconv.Itoa(fb.Key.ZoneID) + "\t" + strconv.Itoa(fb.Key.LinkID) + "\t" +
						strconv.Itoa(fb.Key.PollutantID) + "\t" + strconv.Itoa(fb.Key.ProcessID) + "\t" + strconv.Itoa(fb.Key.SourceTypeID) + "\t" + strconv.Itoa(fb.Key.RegClassID) + "\t" +
						strconv.Itoa(fb.Key.FuelTypeID) + "\t" + strconv.Itoa(fb.Key.ModelYearID) + "\t" + strconv.Itoa(fb.Key.RoadTypeID) + "\t" + fb.Key.SCC + "\t" +
						strconv.Itoa(fb.Key.EngTechID) + "\t" + strconv.Itoa(fb.Key.SectorID) + "\t" + strconv.Itoa(fb.Key.HPID) + "\t" +
						strconv.FormatFloat(e.EmissionQuant,'e',-1,64) + "\t" + strconv.FormatFloat(e.EmissionRate,'e',-1,64) + "\t" +
						strconv.Itoa(e.FuelSubTypeID) + "\t" + strconv.Itoa(e.FuelFormulationID) + "\n"
					detailOutputFile.WriteString(line)
					atomic.AddInt32(&detailLinesWritten,1)
				}
			}
			// Write anything not already written.
			// Always write data when it needs to be split by fuel subtype. Do this because input
			// data is by fuel type only, not fuel subtype, so even the fuel subtype split is new
			// information that should be written.
			if !fb.HasBeenWritten || fb.TotalsByFuelSubType != nil {
				/*
				0,1: MOVESRunID,iterationID,
				2,3,4,5: yearID,monthID,dayID,hourID,
				6,7,8,9: stateID,countyID,zoneID,linkID,
				10,11: pollutantID,processID,
				12,13: sourceTypeID,regClassID,
				14,15: fuelTypeID,modelYearID,
				16,17: roadTypeID,SCC,
				18,19,20: engTechID,sectorID,hpID,
				21,22: emissionQuant,emissionRate
				[23: fuelSubTypeID]
				*/
				linePrefix := "0\t0\t" +
					strconv.Itoa(fb.Key.YearID) + "\t" + strconv.Itoa(fb.Key.MonthID) + "\t" + strconv.Itoa(fb.Key.DayID) + "\t" + strconv.Itoa(fb.Key.HourID) + "\t" +
					strconv.Itoa(fb.Key.StateID) + "\t" + strconv.Itoa(fb.Key.CountyID) + "\t" + strconv.Itoa(fb.Key.ZoneID) + "\t" + strconv.Itoa(fb.Key.LinkID) + "\t" +
					strconv.Itoa(fb.Key.PollutantID) + "\t" + strconv.Itoa(fb.Key.ProcessID) + "\t" + strconv.Itoa(fb.Key.SourceTypeID) + "\t" + strconv.Itoa(fb.Key.RegClassID) + "\t" +
					strconv.Itoa(fb.Key.FuelTypeID) + "\t" + strconv.Itoa(fb.Key.ModelYearID) + "\t" + strconv.Itoa(fb.Key.RoadTypeID) + "\t" + fb.Key.SCC + "\t" +
					strconv.Itoa(fb.Key.EngTechID) + "\t" + strconv.Itoa(fb.Key.SectorID) + "\t" + strconv.Itoa(fb.Key.HPID) + "\t"
				if fb.TotalsByFuelSubType != nil {
					for fuelSubTypeID, fst := range fb.TotalsByFuelSubType {
						line := linePrefix +
							strconv.FormatFloat(fst.TotalEmissionQuant,'e',-1,64) + "\t" + strconv.FormatFloat(fst.TotalEmissionRate,'e',-1,64) +  "\t" +
							strconv.Itoa(fuelSubTypeID) + "\n"
						outputFile.WriteString(line)
						atomic.AddInt32(&linesWritten,1)
					}
				} else {
					line := linePrefix +
						strconv.FormatFloat(fb.TotalEmissionQuant,'e',-1,64) + "\t" + strconv.FormatFloat(fb.TotalEmissionRate,'e',-1,64) + "\n"
					outputFile.WriteString(line)
					atomic.AddInt32(&linesWritten,1)
				}
			}
		}
		b.Recycle()
		globalevents.Send(globalevents.MWOWriteEnd)
	}
}

func NeedsModule(moduleName string) bool {
	return NeededModules[moduleName]
}
