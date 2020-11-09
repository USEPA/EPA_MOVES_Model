// Read and write the MOVES Worker pollution and activity Output files.
package mwo

import (
	"calc/globalevents"
	"os"
	"bufio"
	"strconv"
	"fmt"
	"sync/atomic"
	"bytes"
)

// Pollutant/Process IDs that are needed.
var NeededPolProcessIDs map[int]bool

// Calculation modules that are needed.
var NeededModules map[string]bool

// Onroad fuel supply information
var FuelSupply map[FuelSupplyKey][]*FuelSupplyDetail

// Nonroad fuel supply information
var NRFuelSupply map[FuelSupplyKey][]*FuelSupplyDetail

// Number of emission entries per fuel block by default. This is used to tune memory usage.
var defaultEmissionsPerFuelBlock int

// Onroad fuel types
var FuelTypes map[int]*FuelType

// Onroad fuel subtypes
var FuelSubTypes map[int]*FuelSubType

// Fuel formulations for both onroad and non-road fuels
var FuelFormulations map[int]*FuelFormulation

// Nonroad fuel types
var NRFuelTypes map[int]*FuelType

// Nonroad fuel subtypes
var NRFuelSubTypes map[int]*FuelSubType

// Age groups, keyed by AgeID, value is AgeGroupID
var AgeGroups map[int]int

// Context of the current bundle
type MWOConstants struct {
	StateID, CountyID, ZoneID, LinkID int
	YearID, MonthID int
}

// Context of the current bundle
var Constants MWOConstants

// Key for fuel supply lookup
type FuelSupplyKey struct {
	CountyID, YearID, MonthID, FuelTypeID int
}

// Default for fuel supply lookup
type FuelSupplyDetail struct {
	FuelSubTypeID, FuelFormulationID int
	MarketShare float64

	FuelFormulation *FuelFormulation
}

// Fuel tyupe
type FuelType struct {
	FuelTypeID int
	HumidityCorrectionCoeff, FuelDensity float64
	SubjectToEvapCalculations bool
}

// Fuel subtype
type FuelSubType struct {
	FuelSubTypeID, FuelTypeID int
	FuelSubtypePetroleumFraction, FuelSubtypeFossilFraction, CarbonContent, OxidationFraction, EnergyContent float64

	FuelType *FuelType
}

// Fuel formulation
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

// SCC defaults for Nonroad
type NRSCCDetail struct {
	SCC string
	NREquipTypeID, FuelTypeID int
}

// Nonroad SCC details by SCC text
var NRSCC map[string]*NRSCCDetail

// Nonroad Horse-Power category key
type NRHPCategoryKey struct {
	HPID, EngTechID int
}

// Nonroad Horse-Power lookup
var NRHPCategory map[NRHPCategoryKey]uint8

// Key information for an emissions data block
type MWOKey struct {
	YearID, MonthID, DayID, HourID, StateID, CountyID, ZoneID, LinkID, RoadTypeID int
	SourceTypeID, RegClassID, FuelTypeID, ModelYearID int
	SCC string
	EngTechID, SectorID, HPID int
	PollutantID, ProcessID int
	
	//EM - add this to the MOWKey to allow writing of data in Rates mode 12/20/2018
	AvgSpeedBinID int //this comes from the MWOBaseRate type, where it was removed

	// Derived values often used for lookups
	PolProcessID, HourDayID, AgeID, AgeGroupID int
}

// Emissions quantity
type MWOEmission struct {
	FuelSubTypeID, FuelFormulationID int
	EmissionQuant, EmissionRate float64
}

// Base Rates
type MWOBaseRate struct {
	//AvgSpeedBinID int //EM - this was moved into the MOWKey struct as part of the bugfix for EMT-809 12/20/2018
	FuelSubTypeID, FuelFormulationID int
	MarketShare float64
	MeanBaseRate, MeanBaseRateIM float64
	EmissionRate, EmissionRateIM float64
	MeanBaseRateACAdj, MeanBaseRateIMACAdj float64
	EmissionRateACAdj, EmissionRateIMACAdj float64
}

// Operating Mode and fractions for base rate calculation
type MWOOpMode struct {
	OpModeID int
	GeneralFraction, GeneralFractionRate float64
	BaseRates []*MWOBaseRate
}

// Emissions for a fuel subtype
type FuelSubTypeBlock struct {
	TotalEmissionQuant, TotalEmissionRate float64
}

// Emissions for a fuel type
type FuelBlock struct {
	TotalEmissionQuant, TotalEmissionRate float64
	Key MWOKey
	HasBeenWritten, NeedsGFRE bool
	Emissions []*MWOEmission
	TotalsByFuelSubType map[int]*FuelSubTypeBlock

	// Fields used by base rate calculations
	OpMode *MWOOpMode
}

// A group of emissions. Records are read from the MOVES worker and placed into
// these groups, striking a balance between the overhead of having a group per
// record and a the lack of parallelism of having a single large group.
type MWOBlock struct {
	FuelBlocks []*FuelBlock
}

// --------------------------------

// Key information for an activity data block
type MWOActivityKey struct {
	YearID, MonthID, DayID, HourID, StateID, CountyID, ZoneID, LinkID, RoadTypeID int
	SourceTypeID, RegClassID, FuelTypeID, ModelYearID int
	SCC string
	EngTechID, SectorID, HPID int
	ActivityTypeID int

	// Derived values often used for lookups
	HourDayID, AgeID, AgeGroupID int
}

// Activity detail
type MWOActivity struct {
	FuelSubTypeID, FuelFormulationID int
	Activity float64
}

// Activity for a fuel subtype
type ActivityFuelSubTypeBlock struct {
	TotalActivity float64
}

// Activity for a fuel type
type ActivityFuelBlock struct {
	TotalActivity float64
	Key MWOActivityKey
	HasBeenWritten, NeedsGFRE bool
	Activity []*MWOActivity
	TotalsByFuelSubType map[int]*ActivityFuelSubTypeBlock
}

// A group of activity records. Records are read from the MOVES worker and placed into
// these groups, striking a balance between the overhead of having a group per
// record and a the lack of parallelism of having a single large group.
type MWOActivityBlock struct {
	ActivityFuelBlocks []*ActivityFuelBlock
}

// The pollution output file
var outputFile *os.File

//EM - this is the rates mode output file for rates fix EMT-809 12/20/2018
var baseRateOutputFile *os.File

// Writer to the pollution output file
var outputWriter *bufio.Writer

//EM - this is the rates output writer for EMT-809 12/20/2018
var baseRateOutputWriter *bufio.Writer

// Number of lines written to the pollution output file
var linesWritten int32

// Number of MWOEmission objects processed to make the pollution output file
var emissionBlocksConsidered int32

// Number of FuelBlock objects processed to make the pollution output file
var fuelBlocksConsidered int32

// Number of MWOBlock objects processed to make the pollution output file
var mwoBlocksConsidered int32

// Buffered data to be written to the pollution output file
var outputBuffer bytes.Buffer

//EM - this is the buffered data to write the rates file for EMT-809 12/20/2018
var baseRateOutputBuffer bytes.Buffer

// The activity output file
var activityOutputFile *os.File

// Writer for the activity output file
var activityOutputWriter *bufio.Writer

// Number of lines written to the activity output file
var activityLinesWritten int32

// Number of MWOActivity objects processed to the make the activity output file
var activityBlocksConsidered int32

// Number of ActivityFuelBlock objects processed to the make the activity output file
var activityFuelBlocksConsidered int32

// Number of MWOActivityBlock objects processed to make the activity output file
var mwoActivityBlocksConsidered int32

// Buffered data to be written to the activity output file
var activityBuffer bytes.Buffer

// The detailed output file, with pollution by fuel formulation
var detailOutputFile *os.File

// Writer for the detailed pollution output file
var detailOutputWriter *bufio.Writer

// Number of lines written to the detailed pollution output file
var detailLinesWritten int32

//EM - number of lines written to the base rate output file as part of rates fix EMT-809 12/20/2018
var baseRateLinesWritten int32

// Buffered data to be written to the detailed pollution output file
var detailOutputBuffer bytes.Buffer

// Asynchronous, buffered channel holding tokens, each granting permission to instantiate a MWOBlock object. Used to manage memory.
var mwoPermissions chan int

// Asynchronous, buffered channel holding tokens, each granting permission to instantiate a MWOActivityBlock object. Used to manage memory.
var mwoActivityPermissions chan int

// Amount of data to buffer for output files
var bufferMaxSize int

// Threshold at which buffered data should be written to disk
var bufferWriteLimit int

// Setup global variables
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
	mwoActivityPermissions = make(chan int,10000)

	bufferMaxSize = 4*1024*1024
	bufferWriteLimit = bufferMaxSize - 2048
}

// Add permissions to create MWOBlock objects
func AddBlockPermissions(howManyBlocks int) {
	if(howManyBlocks > cap(mwoPermissions)) {
		howManyBlocks = cap(mwoPermissions)
	}
	for i:=0; i<howManyBlocks; i++ {
		mwoPermissions <- 1
	}
}

// Recycle a MWOBlock that is no longer required. Grant permission to instantiate another.
func (b *MWOBlock) Recycle() {
	// Add the block and all child blocks to internal queue channels.
	// Remember to clear all slices and reset all variables.
	// TODO
	// Give permission to create another block
	mwoPermissions <- 1
}

// Create a new MWOBlock object, waiting for permission to do so.
func New() *MWOBlock {
	// Get permission to create another block
	<- mwoPermissions

	// TODO Use data from a recycled block, including a recycled slice that has been set to 0 length
	b := new(MWOBlock)
	const defaultFuelBlocksPerMWOBlock = 5
	b.FuelBlocks = make([]*FuelBlock,0,defaultFuelBlocksPerMWOBlock)
	return b
}

// Instantiate a FuelBlock object, optionally copying data from another existing object.
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

// Add a new FuelBlock to an existing MWOBlock.
func (b *MWOBlock) Add() *FuelBlock {
	fb := NewFuelBlock(nil)
	b.FuelBlocks = append(b.FuelBlocks,fb)
	return fb
}

// Add an existing FuelBlock to an existing MWOBlock.
func (b *MWOBlock) AddFuelBlock(fb *FuelBlock) {
	b.FuelBlocks = append(b.FuelBlocks,fb)
}

// Add items needed for base rate calculations
func (fb *FuelBlock) SetupForBaseRates() {
	fb.OpMode = new(MWOOpMode)
	fb.OpMode.BaseRates = make([]*MWOBaseRate,0,defaultEmissionsPerFuelBlock)
}

// Add detailed emissions (a MWOEmission object) to a FuelBlock.
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

// Add an existing MWOEmission object to a FuelBlock.
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

// Adjust emission quantity and rate.
func (fb *FuelBlock) ScaleEmissions(emissionQuantScale, emissionRateScale float64) {
	fb.TotalEmissionQuant *= emissionQuantScale
	fb.TotalEmissionRate *= emissionRateScale
	for _, e := range fb.Emissions {
		e.EmissionQuant *= emissionQuantScale
		e.EmissionRate *= emissionRateScale
	}
	if fb.TotalsByFuelSubType != nil {
		for _, fst := range fb.TotalsByFuelSubType {
			fst.TotalEmissionQuant *= emissionQuantScale
			fst.TotalEmissionRate *= emissionRateScale
		}
	}
}

// Note that a FuelBlock's data is actually an input from the MOVES worker and not subject to GFRE (General Fuel Ratio Effects).
func (fb *FuelBlock) SetAsInput() {
	fb.HasBeenWritten = true
	fb.NeedsGFRE = false
}

// Compute the calculated values of a MWOKey. Such values streamline lookups.
func (k *MWOKey) CalcIDs() {
	k.PolProcessID = k.PollutantID * 100 + k.ProcessID
	k.HourDayID = k.HourID * 10 + k.DayID
	k.AgeID = k.YearID - k.ModelYearID
	k.AgeGroupID = AgeGroups[k.AgeID]
}

// Create a new MWOEmission object.
func NewEmission() *MWOEmission {
	// TODO Use a recycled block
	e := new(MWOEmission)
	e.EmissionQuant = 0
	e.EmissionRate = 0
	return e
}

// Create a new MWOEmission object by doing a linear scaling on the emissions from another block.
func NewEmissionScaled(other *MWOEmission, factor float64) *MWOEmission {
	// TODO Use a recycled block
	e := new(MWOEmission)
	e.EmissionQuant = factor * other.EmissionQuant
	e.EmissionRate = factor * other.EmissionRate
	e.FuelSubTypeID = other.FuelSubTypeID
	e.FuelFormulationID = other.FuelFormulationID
	return e
}

// Create a new MWOEmission object by adding the emissions of two other blocks.
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

// Add permissions to create MWOActivityBlock objects
func AddActivityBlockPermissions(howManyBlocks int) {
	if(howManyBlocks > cap(mwoActivityPermissions)) {
		howManyBlocks = cap(mwoActivityPermissions)
	}
	for i:=0; i<howManyBlocks; i++ {
		mwoActivityPermissions <- 1
	}
}

// Recycle a MWOActivityBlock that is no longer required. Grant permission to instantiate another.
func (b *MWOActivityBlock) Recycle() {
	// Add the block and all child blocks to internal queue channels.
	// Remember to clear all slices and reset all variables.
	// TODO
	// Give permission to create another block
	mwoActivityPermissions <- 1
}

// Create a new MWOBlock object, waiting for permission to do so.
func NewActivityBlock() *MWOActivityBlock {
	// Get permission to create another block
	<- mwoActivityPermissions

	// TODO Use data from a recycled block, including a recycled slice that has been set to 0 length
	b := new(MWOActivityBlock)
	const defaultFuelBlocksPerMWOBlock = 5
	b.ActivityFuelBlocks = make([]*ActivityFuelBlock,0,defaultFuelBlocksPerMWOBlock)
	return b
}

// Create a new ActivityFuelBlock, optionally copying information from another object.
func NewActivityFuelBlock(other *ActivityFuelBlock) *ActivityFuelBlock {
	// TODO Use data from a recycled block, including a recycled slice that has been set to 0 length
	fb := new(ActivityFuelBlock)
	fb.TotalActivity = 0
	fb.NeedsGFRE = true
	if other != nil {
		fb.Key = other.Key
		fb.Activity = make([]*MWOActivity,0,cap(other.Activity))
	} else {
		// Leave fb.Key with default values
		fb.Activity = make([]*MWOActivity,0,defaultEmissionsPerFuelBlock)
	}
	if NeedsModule("FuelSubType") {
		fb.TotalsByFuelSubType = make(map[int]*ActivityFuelSubTypeBlock)
	}

	return fb
}

// Add a new ActivityFuelBlock to a MWOActivityBlock.
func (b *MWOActivityBlock) Add() *ActivityFuelBlock {
	fb := NewActivityFuelBlock(nil)
	b.ActivityFuelBlocks = append(b.ActivityFuelBlocks,fb)
	return fb
}

// Add an existing ActivityFuelBlock to a MWOActivityBlock.
func (b *MWOActivityBlock) AddActivityFuelBlock(fb *ActivityFuelBlock) {
	b.ActivityFuelBlocks = append(b.ActivityFuelBlocks,fb)
}

// Add detailed activity (a MWOActivity object) to an ActivityFuelBlock.
func (fb *ActivityFuelBlock) Add(fuelSubTypeID, fuelFormulationID int, activity float64) {
	// TODO Use data from a recycled block
	e := new(MWOActivity)
	e.FuelSubTypeID = fuelSubTypeID
	e.FuelFormulationID = fuelFormulationID
	e.Activity = activity
	fb.TotalActivity += activity
	fb.Activity = append(fb.Activity,e)
	if fb.TotalsByFuelSubType != nil {
		var fst = fb.TotalsByFuelSubType[fuelSubTypeID]
		if fst == nil {
			fst = new(ActivityFuelSubTypeBlock)
			fst.TotalActivity = 0
			fb.TotalsByFuelSubType[fuelSubTypeID] = fst
		}
		fst.TotalActivity += activity
	}
}

// Add an existing MWOActivity object to an ActivityFuelBlock.
func (fb *ActivityFuelBlock) AddActivity(e *MWOActivity) {
	fb.TotalActivity += e.Activity
	fb.Activity = append(fb.Activity,e)
	if fb.TotalsByFuelSubType != nil {
		var fst = fb.TotalsByFuelSubType[e.FuelSubTypeID]
		if fst == nil {
			fst = new(ActivityFuelSubTypeBlock)
			fst.TotalActivity = 0
			fb.TotalsByFuelSubType[e.FuelSubTypeID] = fst
		}
		fst.TotalActivity += e.Activity
	}
}

// Note that an ActivityFuelBlock's data is actually an input from the MOVES worker and not subject to GFRE (General Fuel Ratio Effects).
func (fb *ActivityFuelBlock) SetAsInput() {
	fb.HasBeenWritten = true
	fb.NeedsGFRE = false
}

// Compute the calculated values of a MWOKey. Such values streamline lookups.
func (k *MWOActivityKey) CalcIDs() {
	k.HourDayID = k.HourID * 10 + k.DayID
	k.AgeID = k.YearID - k.ModelYearID
	k.AgeGroupID = AgeGroups[k.AgeID]
}

// Create a new MWOActivity to hold detailed activity amounts.
func NewActivity() *MWOActivity {
	// TODO Use a recycled block
	e := new(MWOActivity)
	e.Activity = 0
	return e
}

// Create a new MWOActivity object by linearly scaling activity data from another object.
func NewActivityScaled(other *MWOActivity, factor float64) *MWOActivity {
	// TODO Use a recycled block
	e := new(MWOActivity)
	e.Activity = factor * other.Activity
	e.FuelSubTypeID = other.FuelSubTypeID
	e.FuelFormulationID = other.FuelFormulationID
	return e
}

// Create a new MWOActivity object by summing the activity data from two other objects.
func NewActivitySum(other1 *MWOActivity, other2 *MWOActivity) *MWOActivity {
	if other1 == nil && other2 == nil {
		return nil
	}
	// TODO Use a recycled block
	e := new(MWOActivity)
	if other1 != nil && other2 != nil {
		e.Activity = other1.Activity + other2.Activity
		e.FuelSubTypeID = other1.FuelSubTypeID
		e.FuelFormulationID = other1.FuelFormulationID
	} else if other1 != nil && other2 == nil {
		e.Activity = other1.Activity
		e.FuelSubTypeID = other1.FuelSubTypeID
		e.FuelFormulationID = other1.FuelFormulationID
	} else if other1 == nil && other2 != nil {
		e.Activity = other2.Activity
		e.FuelSubTypeID = other2.FuelSubTypeID
		e.FuelFormulationID = other2.FuelFormulationID
	}
	return e
}

// EndOfChain builds the goroutines that write pollution data to disk.
func EndOfChain(inputBlocks chan *MWOBlock, blocksToWrite chan *MWOBlock) {
	for i := 0; i<3; i++ {
		go endOfChainCore(inputBlocks,blocksToWrite)
	}
}

// Move pollution blocks to the queue for writing to disk, notifying the global event system.
func endOfChainCore(inputBlocks chan *MWOBlock, blocksToWrite chan *MWOBlock) {
	for {
		b := <- inputBlocks
		globalevents.MWOWriteStarted()
		blocksToWrite <- b
		globalevents.MWOBlockDone()
	}
}

// EndOfActivityChain builds the goroutines that write activity data to disk.
func EndOfActivityChain(inputBlocks chan *MWOActivityBlock, blocksToWrite chan *MWOActivityBlock) {
	for i := 0; i<3; i++ {
		go endOfActivityChainCore(inputBlocks,blocksToWrite)
	}
}

// Move activity blocks to the queue for writing to disk, notifying the global event system.
func endOfActivityChainCore(inputBlocks chan *MWOActivityBlock, blocksToWrite chan *MWOActivityBlock) {
	for {
		b := <- inputBlocks
		globalevents.MWOActivityWriteStarted()
		blocksToWrite <- b
		globalevents.MWOActivityBlockDone()
	}
}

// Create the pollution output and detailed pollution output files and all supporting objects.
// A goroutine is started to asynchronously write data to the files.
func StartWriting(fileName string, blocksToWrite chan *MWOBlock, needsDetailOutput bool) {
	if bufferMaxSize > outputBuffer.Len() {
		outputBuffer.Grow(bufferMaxSize - outputBuffer.Len())
	}
	if bufferMaxSize > detailOutputBuffer.Len() {
		detailOutputBuffer.Grow(bufferMaxSize - detailOutputBuffer.Len())
	}
	//EM - This block added for rates fix EMT-809 12/20/2018
	if bufferMaxSize > baseRateOutputBuffer.Len() {
		baseRateOutputBuffer.Grow(bufferMaxSize - baseRateOutputBuffer.Len())
	}

	f, err := os.Create(fileName)
	if err != nil {
		panic(err)
	}
	outputFile = f
	outputWriter = bufio.NewWriter(outputFile)
	
	//EM - this block also added for EMT-809 rates fix 12/29/2018
	bf, berr := os.Create("newbaserateoutput")
	if berr != nil {
		panic(berr)
	}
	baseRateOutputFile = bf
	baseRateOutputWriter = bufio.NewWriter(baseRateOutputFile)

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

// Write pending data to the pollution files and close all access to the pollution output files.
func FinishWriting() {
	if outputBuffer.Len() > 0 {
		outputBuffer.WriteTo(outputWriter)
		//outputWriter.WriteString(outputBuffer.String())
		outputBuffer.Reset()
	}

	outputWriter.Flush()
	outputFile.Close()
	
	//EM - add a similar block to ensure the rates file gets written as well for EMT-809 12/20/2018
	if baseRateOutputBuffer.Len() > 0 {
		baseRateOutputBuffer.WriteTo(baseRateOutputWriter)
		baseRateOutputBuffer.Reset()
	}
	baseRateOutputWriter.Flush()
	baseRateOutputFile.Close()

	if detailOutputBuffer.Len() > 0 {
		detailOutputBuffer.WriteTo(detailOutputWriter)
		//detailOutputWriter.WriteString(detailOutputBuffer.String())
		detailOutputBuffer.Reset()
	}

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
	//EM - add this line to track rates file lines written as part of EMT-809 12/20/2018
	fmt.Printf("Wrote %d lines to the base rate output file.\n", baseRateLinesWritten)
}

// Write a pollution block to the pollution output file and to the detailed pollution file.
// The pollution block is recycled when done.
func writeCore(blocksToWrite chan *MWOBlock) {
	//The following blcok comment written by Evan Murray:
	/* There is a notable omission of any opmodes from the writing of this file, which may seems surpsising
			becuase techinically they are included in the fb.Key. However, an earlier function assigns the 
			pointer to nil, so that the FuelBlock is no longer carrying the opmode data. This does make the
			code faster and more lightweight, but it comes at the cost of having opModeID out of reach of the
			master becuase it can't be written into the temp files.
	 This aggregation is done in the baseratecalculator, in teh aggregateOpModes function, called by the 
			aggregateAndApplyActivity function, which is in turn called by the StartCalculating function. This is
			called in a goroutine, and passes the data to subsequenct goroutines. It may be possible to just not
			use the aggregateOpModes function in aggregateAndApplyActivity if we ever want to write OpModes here, but
			this is unlikely to work due to all the subsequent function calls and will probably come with a heavy 
			computation time cost.
	*/

	//EM - this checks to see if the rates file should be written as part of EMT-809 12/20/2018
	shouldWriteRates := NeedsModule("BRC_Rates")
	
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
			
			
			//EM - this code actually writes the rate file row if necessary for EMT-809
			if shouldWriteRates { // If rates-mode output is required...
				/*
				0,1: MOVESRunID,iterationID,
				2,3,4: yearID,monthID,hourDayID,
				5,6: zoneID,linkID,
				7,8: pollutantID,processID,
				9,10: sourceTypeID,regClassID,
				11,12: fuelTypeID,modelYearID,
				13,14: roadTypeID,SCC,
				15: avgSpeedBinID,
				16,17: meanBaseRate,emissionRate
				*/
				for _, e := range fb.Emissions {
					line := "0\t0\t" +
						strconv.Itoa(fb.Key.YearID) + "\t" + strconv.Itoa(fb.Key.MonthID) + "\t" + strconv.Itoa(fb.Key.HourDayID) + "\t" +
						strconv.Itoa(fb.Key.ZoneID) + "\t" + strconv.Itoa(fb.Key.LinkID) + "\t" +
						strconv.Itoa(fb.Key.PollutantID) + "\t" + strconv.Itoa(fb.Key.ProcessID) + "\t" + 
						strconv.Itoa(fb.Key.SourceTypeID) + "\t" + strconv.Itoa(fb.Key.RegClassID) + "\t" +
						strconv.Itoa(fb.Key.FuelTypeID) + "\t" + strconv.Itoa(fb.Key.ModelYearID) + "\t" + 
						strconv.Itoa(fb.Key.RoadTypeID) + "\t" + fb.Key.SCC + "\t" +
						strconv.Itoa(fb.Key.AvgSpeedBinID) + "\t" + 
						strconv.FormatFloat(e.EmissionQuant,'e',-1,64) + "\t" + strconv.FormatFloat(e.EmissionRate,'e',-1,64) + "\n"
					baseRateOutputBuffer.WriteString(line)
					atomic.AddInt32(&baseRateLinesWritten,1)
				}
				if baseRateOutputBuffer.Len() >= bufferWriteLimit {
					baseRateOutputBuffer.WriteTo(baseRateOutputWriter)
					baseRateOutputBuffer.Reset()
				}
			}
			
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
					//detailOutputWriter.WriteString(line)
					detailOutputBuffer.WriteString(line)
					atomic.AddInt32(&detailLinesWritten,1)
				}
				if detailOutputBuffer.Len() >= bufferWriteLimit {
					detailOutputBuffer.WriteTo(detailOutputWriter)
					detailOutputBuffer.Reset()
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
						//outputWriter.WriteString(line)
						outputBuffer.WriteString(line)
						atomic.AddInt32(&linesWritten,1)
					}
				} else {
					line := linePrefix +
						strconv.FormatFloat(fb.TotalEmissionQuant,'e',-1,64) + "\t" + strconv.FormatFloat(fb.TotalEmissionRate,'e',-1,64) + "\n"
					//outputWriter.WriteString(line)
					outputBuffer.WriteString(line)
					atomic.AddInt32(&linesWritten,1)
				}
				if outputBuffer.Len() >= bufferWriteLimit {
					outputBuffer.WriteTo(outputWriter)
					outputBuffer.Reset()
				}
			}
		}
		b.Recycle()
		globalevents.MWOWriteDone()
	}
}

// Create the activity output file and all supporting objects.
// A goroutine is started to asynchronously write data to the file.
func StartActivityWriting(fileName string, blocksToWrite chan *MWOActivityBlock) {
	if bufferMaxSize > activityBuffer.Len() {
		activityBuffer.Grow(bufferMaxSize - activityBuffer.Len())
	}

	f, err := os.Create(fileName)
	if err != nil {
		panic(err)
	}
	activityOutputFile = f
	activityOutputWriter = bufio.NewWriter(activityOutputFile)

	go writeActivityCore(blocksToWrite)
	fmt.Println("Started writing activity output to: ",fileName)
}

// Write pending data to the pollution files and close all access to the pollution output files.
func FinishActivityWriting() {
	if activityBuffer.Len() > 0 {
		activityOutputWriter.WriteString(activityBuffer.String())
		activityBuffer.Reset()
	}

	activityOutputWriter.Flush()
	activityOutputFile.Close()

	fmt.Printf("Considered %d MWOActivityBlocks for writing.\n",mwoActivityBlocksConsidered)
	fmt.Printf("Considered %d ActivityFuelBlocks for writing.\n",activityFuelBlocksConsidered)
	fmt.Printf("Considered %d MWOActivity for writing.\n",activityBlocksConsidered)
	fmt.Printf("Wrote %d lines to the activity output file.\n",activityLinesWritten)
}

// Write an activity block to the activity output file.
// The activity block is recycled when done.
func writeActivityCore(blocksToWrite chan *MWOActivityBlock) {
	for {
		b := <- blocksToWrite
		atomic.AddInt32(&mwoActivityBlocksConsidered,1)
		for _, fb := range b.ActivityFuelBlocks {
			atomic.AddInt32(&activityFuelBlocksConsidered,1)
			atomic.AddInt32(&activityBlocksConsidered,int32(len(fb.Activity)))
			// Write anything not already written.
			// Always write data when it needs to be split by fuel subtype. Do this because input
			// data is by fuel type only, not fuel subtype, so even the fuel subtype split is new
			// information that should be written.
			if !fb.HasBeenWritten || fb.TotalsByFuelSubType != nil {
				/*
				0,1: MOVESRunID,iterationID,
				2,3,4,5: yearID,monthID,dayID,hourID,
				6,7,8,9: stateID,countyID,zoneID,linkID,
				10,11: sourceTypeID,regClassID,
				12,13: fuelTypeID,modelYearID,
				14,15: roadTypeID,SCC,
				16,17,18: engTechID,sectorID,hpID,
				19,20: activityTypeID,activity
				[21: fuelSubTypeID]
				*/
				linePrefix := "0\t0\t" +
					strconv.Itoa(fb.Key.YearID) + "\t" + strconv.Itoa(fb.Key.MonthID) + "\t" + strconv.Itoa(fb.Key.DayID) + "\t" + strconv.Itoa(fb.Key.HourID) + "\t" +
					strconv.Itoa(fb.Key.StateID) + "\t" + strconv.Itoa(fb.Key.CountyID) + "\t" + strconv.Itoa(fb.Key.ZoneID) + "\t" + strconv.Itoa(fb.Key.LinkID) + "\t" +
					strconv.Itoa(fb.Key.SourceTypeID) + "\t" + strconv.Itoa(fb.Key.RegClassID) + "\t" +
					strconv.Itoa(fb.Key.FuelTypeID) + "\t" + strconv.Itoa(fb.Key.ModelYearID) + "\t" + strconv.Itoa(fb.Key.RoadTypeID) + "\t" + fb.Key.SCC + "\t" +
					strconv.Itoa(fb.Key.EngTechID) + "\t" + strconv.Itoa(fb.Key.SectorID) + "\t" + strconv.Itoa(fb.Key.HPID) + "\t" +
					strconv.Itoa(fb.Key.ActivityTypeID) + "\t"
				if fb.TotalsByFuelSubType != nil {
					for fuelSubTypeID, fst := range fb.TotalsByFuelSubType {
						line := linePrefix +
							strconv.FormatFloat(fst.TotalActivity,'e',-1,64) + "\t" + strconv.Itoa(fuelSubTypeID) + "\n"
						//activityOutputWriter.WriteString(line)
						activityBuffer.WriteString(line)
						atomic.AddInt32(&activityLinesWritten,1)
					}
				} else {
					line := linePrefix +
						strconv.FormatFloat(fb.TotalActivity,'e',-1,64) + "\n"
					//activityOutputWriter.WriteString(line)
					activityBuffer.WriteString(line)
					atomic.AddInt32(&activityLinesWritten,1)
				}
				if activityBuffer.Len() >= bufferWriteLimit {
					activityBuffer.WriteTo(activityOutputWriter)
					activityBuffer.Reset()
				}
			}
		}
		b.Recycle()
		globalevents.MWOActivityWriteDone()
	}
}

// Lookup a module name in NeededModules.
func NeedsModule(moduleName string) bool {
	return NeededModules[moduleName]
}
