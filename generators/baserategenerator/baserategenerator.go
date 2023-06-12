/*
 Functions for the Base Rate Generator module.
 @author Wesley Faler
 @version 2017-07-03
*/
package baserategenerator

import (
	"bufio"
	"database/sql"
	"fmt"
	"os"
	"math"
	"path/filepath"
	"strconv"
	"sync"

	"gen/configuration"
	"gen/globalevents"
)

// Set to false normally, allowing fast computation of operating mode distribution
// from drive cycles. When true, the Java-code must supply a fully populated
// RatesOpModeDistribution table.
const ALWAYS_USE_ROMD_TABLE = false

/**
 * @algorithm
 * @owner Base Rate Generator
 * @generator
**/

// Information from the SourceUseTypePhysicsMapping table.
// RealSourceTypeID is the source type traditionally used.
// TempSourceTypeID is a temporary source type that works for a model year range and regclass combination.
// OpModeIDOffset is used to make new operationg modes good just for the temporary source type.
type SourceUseTypePhysicsMappingDetail struct {
	RealSourceTypeID, TempSourceTypeID, OpModeIDOffset int
	regClassID, beginModelYearID, endModelYearID int
	rollingTermA, rotatingTermB, dragTermC, sourceMass, fixedMassFactor float64
}

// All SourceUseTypePhysicsMappingDetail records
var SourceUseTypePhysicsMapping []*SourceUseTypePhysicsMappingDetail

// SourceUseTypePhysicsMappingDetail organized by TempSourceTypeID.
// Each value is unique as there is a unique TempSourceTypeID for each SourceUseTypePhysicsMappingDetail.
var SourceUseTypePhysicsMappingByTempSourceType map[int]*SourceUseTypePhysicsMappingDetail

// SourceUseTypePhysicsMappingDetail organized by RealSourceTypeID.
// As RealSourceTypeID is shared by one or more SourceUseTypePhysicsMappingDetail objects,
// only one of the SourceUseTypePhysicsMappingDetail objects will be stored here. As such,
// do not use the temporary source type from this detail. Rather, use its existence to control
// logic that discards old records.
var SourceUseTypePhysicsMappingByRealSourceType map[int]*SourceUseTypePhysicsMappingDetail

// Flags and Identifiers controlling the table join logic.
type externalFlags struct {
	keepOpModeID, useAvgSpeedBin, useAvgSpeedFraction, useSumSBD, useSumSBDRaw bool
	processID, yearID, roadTypeID int
}

// Flags and Identifiers controlling the table join logic as read from the "-parameters=" command line option.
var flags externalFlags

// Average speed bin. Data is avgBinSpeed keyed by AvgSpeedBinID
var avgSpeedBin map[int]float64

// Drive schedules. Data is averageSpeed keyed by DriveScheduleID
var driveSchedule map[int]float64

// Unique keys for AvgSpeedDistribution records.
type avgSpeedDistributionKey struct {
	sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID int
}

// Detail for AvgSpeedDistribution
type avgSpeedDistributionDetail struct {
	avgSpeedFraction, avgBinSpeed float64
}

// Unique keys for DriveScheduleAssoc records.
type driveScheduleAssocKey struct {
	sourceTypeID, roadTypeID int
}

// DriveScheduleAssoc. Data is list of driveScheduleID.
var driveScheduleAssoc map[driveScheduleAssocKey][]int

// Average speed distribution
var avgSpeedDistribution map[avgSpeedDistributionKey]*avgSpeedDistributionDetail

// Unique keys for RatesOpModeDistribution records.
type romdKey struct {
	sourceTypeID, polProcessID, roadTypeID, hourDayID, opModeID, avgSpeedBinID int
	beginModelYearID, endModelYearID, regClassID int
}

// RatesOpModeDistribution record
type romdBlock struct {
	key romdKey
	opModeFraction, avgBinSpeed, avgSpeedFraction float64
}

// Combinations already processed for the RatesOpModeDistribution table.
var romdKeys map[romdKey]bool

// Unique suffix for each temporary file.
var fileNumber int

// Synchronization guard for the fileNumber variable.
var fileNumberGuard *sync.Mutex

// Name of the table holding BaseRate data
var baseRateTableName string
// Path and base name of the temporary files holding BaseRate data
var baseRateTableFileBase string
// Name of the table holding BaseRateByAge data
var baseRateByAgeTableName string
// Path and base name of the temporary files holding BaseRateByAge data
var baseRateByAgeTableFileBase string

// A key from the SBWeightedEmissionRate[ByAge] tables
type sbWeightedEmissionRateByAgeKey struct {
	sourceTypeID, polProcessID, opModeID int
}

// A record from the SBWeightedEmissionRate[ByAge] tables
type sbWeightedEmissionRateByAgeDetail struct {
	sourceTypeID, polProcessID, opModeID int
	modelYearID, fuelTypeID, ageGroupID, regClassID int
	sumSBD,	sumSBDRaw float64
	meanBaseRate, meanBaseRateIM, meanBaseRateACAdj, meanBaseRateIMACAdj float64
}

// All records from the SBWeightedEmissionRateByAge table
var sbWeightedEmissionRateByAge map[sbWeightedEmissionRateByAgeKey][]*sbWeightedEmissionRateByAgeDetail

// All records from the SBWeightedEmissionRate table. ageGroupID = 0 in this data.
var sbWeightedEmissionRate map[sbWeightedEmissionRateByAgeKey][]*sbWeightedEmissionRateByAgeDetail

// List of roadTypeID's, does not include offnetwork (roadtypeID=1) if present in the runspec
var runSpecRoadType []int

// List of roadTypeID's, does include offnetwork (roadtypeID=1) if present in the runspec
var runSpecRoadTypeWithOffNetwork []int

// List of hourdayID's
var runSpecHourDay []int

// List of sourceTypeID's
var runSpecSourceType []int

// List of polProcessID's that need driving cycles for the current process
var runSpecPolProcessID []int

// Set of allowed model years
var runSpecModelYear map[int]bool

// Operating Mode
type operatingMode struct {
	opModeID int
	VSPLower,VSPUpper,speedLower,speedUpper float64
	isnullVSPLower,isnullVSPUpper,isnullSpeedLower,isnullSpeedUpper bool
}

// Operating mode defintions. Only modes > 1 and < 100 are present in this set.
var operatingModes map[int]*operatingMode

// Create global variables
func init() {
	avgSpeedBin = make(map[int]float64)
	driveSchedule = make(map[int]float64)
	driveScheduleAssoc = make(map[driveScheduleAssocKey][]int)
	avgSpeedDistribution = make(map[avgSpeedDistributionKey]*avgSpeedDistributionDetail)
	romdKeys = make(map[romdKey]bool)
	fileNumber = 1
	fileNumberGuard = new(sync.Mutex)
	sbWeightedEmissionRateByAge = make(map[sbWeightedEmissionRateByAgeKey][]*sbWeightedEmissionRateByAgeDetail)
	sbWeightedEmissionRate = make(map[sbWeightedEmissionRateByAgeKey][]*sbWeightedEmissionRateByAgeDetail)
	runSpecRoadType = make([]int,0,20)
	runSpecRoadTypeWithOffNetwork = make([]int,0,20)
	runSpecHourDay = make([]int,0,48)
	runSpecSourceType = make([]int,0,30)
	runSpecPolProcessID = make([]int,0,500)
	runSpecModelYear = make(map[int]bool)
	operatingModes = make(map[int]*operatingMode)
}

// Create and use the RatesOpModeDistribution operating mode distribution records, replacing
// temporary source types with real source types and changing real operating modes
// to modelyear-specific modes. Once created, use these records to compute the base rates.
func BaseRateGeneratorFromRatesOpModeDistribution(sqlToWrite chan string) {
	if !readExternalFlags() {
		return
	}
	setupTables()

	romdForBaseRateQueue := make(chan *romdBlock,200000)
	romdForBaseRateByAgeQueue := make(chan *romdBlock,200000)

	shouldProcessDriveCycles := false
	// EM - processID==9 added to make sure the drive cycles are processed for brakewear (as well as running) for EMT-633 12/21/2018
	if !configuration.Singleton.IsProject && (flags.processID == 1 || flags.processID == 9) {
		shouldProcessDriveCycles = true
	}

	if ALWAYS_USE_ROMD_TABLE {
		shouldProcessDriveCycles = false // force use of old code
	}

	if shouldProcessDriveCycles {
		globalevents.GeneratorStarting()
		go processDriveCycles(romdForBaseRateQueue,romdForBaseRateByAgeQueue,sqlToWrite)
	} else {
		globalevents.GeneratorStarting()
		go coreBaseRateGeneratorFromRatesOpModeDistribution(romdForBaseRateQueue,romdForBaseRateByAgeQueue)
	}

	globalevents.GeneratorStarting()
	go makeBaseRateFromSourceBinRates(romdForBaseRateQueue, sqlToWrite)

	globalevents.GeneratorStarting()
	go makeBaseRateByAgeFromSourceBinRates(romdForBaseRateByAgeQueue, sqlToWrite)

	globalevents.GeneratorStarting()
	go makeBaseRateFromDistanceRates(sqlToWrite)
}

// Read parameters and populate the flags variable with identifiers and
// flags that control the join logic.
func readExternalFlags() bool {
	if len(configuration.Singleton.Parameters) < 8 {
		// Not enough parameters provided, there must be a version mismatch.
		// Complain and quit.
		fmt.Println("ERROR: Expected at least 8 CSV parameters, cannot proceed")
		return false
	}
	for i:=0;i<len(configuration.Singleton.Parameters)-3;i++ {
		switch configuration.Singleton.Parameters[i] {
		case "yOp":
			flags.keepOpModeID = true
		case "nOp":
			flags.keepOpModeID = false
		case "yASB":
			flags.useAvgSpeedBin = true
		case "nASB":
			flags.useAvgSpeedBin = false
		case "yASF":
			flags.useAvgSpeedFraction = true
		case "nASF":
			flags.useAvgSpeedFraction = false
		case "ySBD":
			flags.useSumSBD = true
		case "nSBD":
			flags.useSumSBD = false
		case "yRaw":
			flags.useSumSBDRaw = true
		case "nRaw":
			flags.useSumSBDRaw = false
		}
	}
	t, err := strconv.Atoi(configuration.Singleton.Parameters[len(configuration.Singleton.Parameters)-3])
	configuration.CheckErr(err)
	flags.processID = t
	t, err = strconv.Atoi(configuration.Singleton.Parameters[len(configuration.Singleton.Parameters)-2])
	configuration.CheckErr(err)
	flags.yearID = t
	t, err = strconv.Atoi(configuration.Singleton.Parameters[len(configuration.Singleton.Parameters)-1])
	configuration.CheckErr(err)
	flags.roadTypeID = t

	return true
}

// Update the RatesOpModeDistribution operating mode distribution table, replacing
// temporary source types with real source types and changing real operating modes
// to modelyear-specific modes.
func coreBaseRateGeneratorFromRatesOpModeDistribution(romdForBaseRateQueue, romdForBaseRateByAgeQueue chan *romdBlock) {
	defer globalevents.GeneratorDone()

	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	b := new(romdBlock)

	fmt.Println("Querying RatesOpModeDistribution...")

	// Note: The ORDER BY is important here. Records with opModeID >= 1000 must be processed before
	// ----- those in the normal 0-100 range. This ORDER BY is the most efficient given the table's
	// primary key (it is the exact desc of all fields so is fast to process).
	// Without the ORDER BY, that natural order does not ensure the required sequence.
	querySql := "select sourceTypeID,roadTypeID,avgSpeedBinID,hourDayID,polProcessID,opModeID,opModeFraction,coalesce(opModeFractionCV,0) as opModeFractionCV,avgBinSpeed,avgSpeedFraction" +
			" from RatesOpModeDistribution"
	// Add WHERE with restrictions based upon passed parameters, some of which may be 0 and not used.
	hasWhere := false
	if flags.processID > 0 {
		if hasWhere {
			querySql += " and"
		} else {
			hasWhere = true
			querySql += " where"
		}
		querySql += " (polProcessID % 100) = " +  strconv.Itoa(flags.processID)
	}
	if flags.roadTypeID > 0 {
		if hasWhere {
			querySql += " and"
		} else {
			hasWhere = true
			querySql += " where"
		}
		querySql += " roadTypeID = " +  strconv.Itoa(flags.roadTypeID)
	}
	// Finish and query the table
	querySql += " order by sourceTypeID desc, polProcessID desc, roadTypeID desc, hourDayID desc, opModeID desc, avgSpeedBinID desc"
	rows, err := db.Query(querySql)
	configuration.CheckErr(err)
    defer rows.Close()

    rowCount := 0
	queueCount := 0
    outputRowCount := 0
	var sourceTypeID,roadTypeID,avgSpeedBinID,hourDayID,polProcessID,opModeID int
	var opModeFraction,opModeFractionCV,avgBinSpeed,avgSpeedFraction float64
	var shouldWrite, didHandle bool
	var avgSpeedKey avgSpeedDistributionKey

	var previousSourceTypeID, previousPolProcesID int

    for rows.Next() {
    	if rowCount == 0 {
			fmt.Println("Got first row from RatesOpModeDistribution")
    	}
    	rowCount++
    	shouldWrite = false
    	didHandle = false

        err = rows.Scan(&sourceTypeID,&roadTypeID,&avgSpeedBinID,&hourDayID,&polProcessID,&opModeID,&opModeFraction,&opModeFractionCV,&avgBinSpeed,&avgSpeedFraction)
		configuration.CheckErr(err)

		if previousSourceTypeID != sourceTypeID || previousPolProcesID != polProcessID {
			// Empty romdKeys
			romdKeys = make(map[romdKey]bool)
		}
		previousSourceTypeID = sourceTypeID
		previousPolProcesID = polProcessID

		if avgSpeedFraction <= 0 {
			avgSpeedKey.sourceTypeID = sourceTypeID
			avgSpeedKey.roadTypeID = roadTypeID
			avgSpeedKey.hourDayID = hourDayID
			avgSpeedKey.avgSpeedBinID = avgSpeedBinID
			d, found := avgSpeedDistribution[avgSpeedKey]
			if found {
				avgSpeedFraction = d.avgSpeedFraction
				avgBinSpeed = d.avgBinSpeed
			}
		}

		tempSourceTypeDetail := SourceUseTypePhysicsMappingByTempSourceType[sourceTypeID]
		realSourceTypeDetail := SourceUseTypePhysicsMappingByRealSourceType[sourceTypeID]

		// Delete wildcard placeholders
		if !didHandle && polProcessID < 0 {
			didHandle = true
			shouldWrite = false
		}

		// Change source types for any new operating modes
		if !didHandle && tempSourceTypeDetail != nil &&
				opModeID >= (0+tempSourceTypeDetail.OpModeIDOffset) && opModeID < (100+tempSourceTypeDetail.OpModeIDOffset) &&
				(polProcessID < 0 || polProcessID % 100 == 1 || polProcessID % 100 == 9) {
			// This record should be updated
			didHandle = true
			shouldWrite = true
			sourceTypeID = tempSourceTypeDetail.RealSourceTypeID
		}
		
		// Promote old operating modes and change source types
		// This statement must fail (which is ok and can be ignored) if
		// entries exist with extended operating modes already. That is,
		// if entries already exist with extended operating modes, they
		// must be used and this record ignored.
		if !didHandle && tempSourceTypeDetail != nil &&
				opModeID >= 0 && opModeID < 100 &&
				(polProcessID < 0 || polProcessID % 100 == 1 || polProcessID % 100 == 9) {
			// This record should be updated
			didHandle = true
			shouldWrite = true
			sourceTypeID = tempSourceTypeDetail.RealSourceTypeID
			opModeID += tempSourceTypeDetail.OpModeIDOffset
		}

		if !didHandle && tempSourceTypeDetail != nil && tempSourceTypeDetail.OpModeIDOffset > 0 &&
				opModeID >= 0 && opModeID < 100 &&
				(polProcessID < 0 || polProcessID % 100 == 1 || polProcessID % 100 == 9) {
			// This record should be deleted
			didHandle = true
			shouldWrite = false
		}

		// // tempSourceTypeID never equals realSourceTypeID any more, so get rid of real source type operating modes
		if !didHandle && tempSourceTypeDetail == nil && realSourceTypeDetail != nil && realSourceTypeDetail.OpModeIDOffset > 0 &&
				opModeID >= 0 && opModeID < 100 &&
				(polProcessID < 0 || polProcessID % 100 == 1 || polProcessID % 100 == 9) {
			// This record should be deleted
			didHandle = true
			shouldWrite = false
		}

		// Anything not already handled should continue to be used.
		if !didHandle {
			shouldWrite = true
		}

		if shouldWrite {
			if b == nil {
				b = new(romdBlock)
			}
			// ROMD key: sourceTypeID, polProcessID, roadTypeID, hourDayID, opModeID, avgSpeedBinID, [modelYearID=0], [regClassID=0]
			b.key.sourceTypeID = sourceTypeID
			b.key.polProcessID = polProcessID
			b.key.roadTypeID = roadTypeID
			b.key.hourDayID = hourDayID
			b.key.opModeID = opModeID
			b.key.avgSpeedBinID = avgSpeedBinID
			b.key.beginModelYearID = 0
			b.key.endModelYearID = 0
			b.key.regClassID = 0
			_, found := romdKeys[b.key]
			if !found {
				b.opModeFraction = opModeFraction

				// Make sure the average speed information is known.
				if avgSpeedFraction <= 0 {
					avgSpeedKey.sourceTypeID = sourceTypeID
					avgSpeedKey.roadTypeID = roadTypeID
					avgSpeedKey.hourDayID = hourDayID
					avgSpeedKey.avgSpeedBinID = avgSpeedBinID
					d, found := avgSpeedDistribution[avgSpeedKey]
					if found {
						avgSpeedFraction = d.avgSpeedFraction
						avgBinSpeed = d.avgBinSpeed
					}
				}
				b.avgBinSpeed = avgBinSpeed
				b.avgSpeedFraction = avgSpeedFraction

				romdKeys[b.key] = true
				outputRowCount++

				// Enqueue b
				romdForBaseRateQueue <- b
				romdForBaseRateByAgeQueue <- b
				b = nil
				queueCount++
			}
		}
    }
	// Close the queues so downstream threads know that no more data is coming.
	close(romdForBaseRateQueue)
	close(romdForBaseRateByAgeQueue)

	fmt.Println("Done reading RatesOpModeDistribution. Row Count=",rowCount,"Queue Count=",queueCount)
}

// Obtain a unique number for use as in naming a temporary file.
func nextFileNumber() string {
	fileNumberGuard.Lock()
	fileNumber++
	result := fileNumber
	fileNumberGuard.Unlock()
	return "_temp" + strconv.Itoa(result)
}

// Provide temporary data files holding SQL data to be LOAD'd into the database.
type tempFiles struct {
	fileIndex, outputRowCount                      int
	fileName, baseFileName, tableName, columnNames string
	outputFile                                     *os.File
	outputWriter                                   *bufio.Writer
	sqlToWrite                                     chan string
}

// Create a new tempFiles object that is ready to be used.
func newTempFiles(baseFileName, tableName, columnNames string, sqlToWrite chan string) *tempFiles {
	t := new(tempFiles)
	t.baseFileName = baseFileName
	t.tableName = tableName
	t.columnNames = columnNames
	t.sqlToWrite = sqlToWrite

	t.fileIndex = 0
	t.fileName = t.baseFileName + nextFileNumber()
	t.outputRowCount = 0

	f, err := os.Create(t.fileName)
	if err != nil {
		panic(err)
	}
	t.outputFile = f
	t.outputWriter = bufio.NewWriter(t.outputFile)

	return t
}

// Write a line to the temporary file system, committing when needed based
// upon file size.
func (this *tempFiles) writeLine(line string) {
	this.outputRowCount++
	this.outputWriter.WriteString(line)
}

// Finish all operations on the temporary data, writing anything remaining
// to the database.
func (this *tempFiles) close() {
	this.outputWriter.Flush()
	this.outputFile.Close()

	globalevents.SqlStarting()
	this.sqlToWrite <- "load data infile '" + filepath.ToSlash(this.fileName) + "'" +
		" ignore into table " + this.tableName + " (" + this.columnNames + ")"
}

// Grouping for accumulation of baseRate[ByAge] data.
type baseRateOutputKey struct {
	sourceTypeID, polProcessID, roadTypeID, hourDayID, avgSpeedBinID int
	modelYearID, fuelTypeID, ageGroupID, regClassID, opModeID int
}

// Data for a row in the BaseRate or BaseRateByAge tables.
type baseRateOutputRecord struct {
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, pollutantID, processID,
	modelYearID, fuelTypeID, ageGroupID, regClassID, opModeID int

	meanBaseRate, meanBaseRateIM, meanBaseRateACAdj, meanBaseRateIMACAdj, emissionRate, emissionRateIM,
	emissionRateACAdj, emissionRateIMACAdj, opModeFraction, opModeFractionRate float64
}

// Create a SQL-textual version of a BaseRate[ByAge] record.
func (this *baseRateOutputRecord) writeLine(useAge bool, files *tempFiles) {
	// Build output line. Must match the order of columns in the temporary files.
	// sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, ageGroupID, regClassID, opModeID,
	// opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID
	line := strconv.Itoa(this.sourceTypeID) +
			"\t" + strconv.Itoa(this.roadTypeID) +
			"\t" + strconv.Itoa(this.avgSpeedBinID) +
			"\t" + strconv.Itoa(this.hourDayID) +
			"\t" + strconv.Itoa(this.polProcessID) +
			"\t" + strconv.Itoa(this.modelYearID) +
			"\t" + strconv.Itoa(this.fuelTypeID)
	if useAge {
		line += "\t" + strconv.Itoa(this.ageGroupID)
	}
	line += "\t" + strconv.Itoa(this.regClassID) +
			"\t" + strconv.Itoa(this.opModeID) +
			"\t" + strconv.FormatFloat(this.opModeFraction,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.opModeFractionRate,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.meanBaseRate,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.meanBaseRateIM,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.meanBaseRateACAdj,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.meanBaseRateIMACAdj,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.emissionRate,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.emissionRateIM,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.emissionRateACAdj,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.emissionRateIMACAdj,'e',-1,64) +
			"\t" + strconv.Itoa(this.processID) +
			"\t" + strconv.Itoa(this.pollutantID) +
			"\n"
	files.writeLine(line)
}

// Write accumulated data to SQL files.
func writeLines(outputRecords map[baseRateOutputKey]*baseRateOutputRecord, useAge bool, files *tempFiles) {
	for k, v := range outputRecords {
		delete(outputRecords,k)
		v.writeLine(useAge,files)
	}
}

// Copy fields from ROMD
func (this *baseRateOutputKey) fromRomdKey(other *romdKey) {
	this.sourceTypeID = other.sourceTypeID
	this.polProcessID = other.polProcessID
	this.roadTypeID = other.roadTypeID
	this.hourDayID = other.hourDayID
	this.opModeID = other.opModeID
	this.avgSpeedBinID = other.avgSpeedBinID
}

// Copy fields from SBWeightedEmissionRate[ByAge]
func (this *baseRateOutputKey) fromSBbyAge(other *sbWeightedEmissionRateByAgeDetail) {
	this.modelYearID = other.modelYearID
	this.fuelTypeID = other.fuelTypeID
	this.ageGroupID = other.ageGroupID
	this.regClassID = other.regClassID
}

/**
 * @step 101
 * @algorithm avgSpeedFractionClause=coalesce(avgSpeedFraction,0) when conditions are met, 1 otherwise.
 * @condition Non-Project domain; Inventory; Running exhaust, Brakewear, or Tirewear.
**/

/**
 * @step 101
 * @algorithm sumSBDClause=sumSBD when conditions are met, 1 otherwise.
 * @condition Inventory or Starts or Extended Idling or Auxiliary Power.
**/

/**
 * @step 101
 * @algorithm quantAdjustClause=sumSBDRaw when conditions are met, 1 otherwise.
 * @condition Rates for Starts, Extended Idle, or Auxiliary Power.
**/

// Populate the BaseRateByAge table from source-bin weighted emission rates.
func makeBaseRateByAgeFromSourceBinRates(romdQueue chan *romdBlock, sqlToWrite chan string) {
	defer globalevents.GeneratorDone()

	outputRecords := make(map[baseRateOutputKey]*baseRateOutputRecord)
	files := newTempFiles(baseRateByAgeTableFileBase,baseRateByAgeTableName,
			"sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, ageGroupID, regClassID, opModeID," +
			"opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID",
			sqlToWrite)
	var romdCount, writeCount int

	var currentKey, previousKey romdKey
	hasPreviousKey := false

	for romd := range romdQueue {
    	romdCount++
		// Look for changes in ROMD's key so accumulated data can write written to the database.
		currentKey.sourceTypeID = romd.key.sourceTypeID
		currentKey.polProcessID = romd.key.polProcessID
		currentKey.roadTypeID = romd.key.roadTypeID
		currentKey.hourDayID = romd.key.hourDayID
		if flags.keepOpModeID {
			currentKey.opModeID = romd.key.opModeID
		}
		if flags.useAvgSpeedBin {
			currentKey.avgSpeedBinID = romd.key.avgSpeedBinID
		}
		// Everytime the ROMD's portion of the output table's unique key
		// changes, all accumulated data can be written to disk as it
		// will never be needed again.
		if hasPreviousKey && currentKey != previousKey {
			writeLines(outputRecords,true,files)
		}
		hasPreviousKey = true
		previousKey = currentKey

		// Get all rates that match sourceTypeID, polProcessID, and opModeID.
		var sbKey sbWeightedEmissionRateByAgeKey
		sbKey.sourceTypeID = romd.key.sourceTypeID
		sbKey.polProcessID = romd.key.polProcessID
		sbKey.opModeID = romd.key.opModeID
		rates, found := sbWeightedEmissionRateByAge[sbKey]
		if !found && romd.key.opModeID >= 1000 {
			/*
			The ratesopmodedistribution contains entries for all vehicle combinations, expressed as opmode offsets, that exist at any time, 
			rather than just in the 30 year window of the runspec. However, other tables don't contain entries for these offset opmodes 
			because they ARE based on the runspec window. On top of this, the Go code handles this too elegantly, looking for non-offset 
			opmodes when it doesn't find the offset opmode that isn't relevant for the runspec. This leads to double-counting. 
			Putting in a continue, instead of checking for a non-offset opmode, prevents this double-counting from taking place.
			*/
			if (romd.key.polProcessID % 100) != 9 {
				/* we only want to continue for non-brakewear polProcesses because the brakewear polProcessID has rates by op mode 
				   rates only for non-offset operating mode IDs. We tried to write the code to never offset brakewear op modes in 
				   the first place, but that doesn't work for reasons we don't fully understand 
			    */
				continue
			}
			sbKey.opModeID = romd.key.opModeID % 100
			rates, found = sbWeightedEmissionRateByAge[sbKey]
		}
		if !found {
			continue
		}
		var outputKey baseRateOutputKey
		for _, rate := range rates {
			if romd.key.regClassID > 0 && romd.key.regClassID != rate.regClassID {
				continue
			}
			if romd.key.beginModelYearID > 0 && romd.key.endModelYearID > 0 &&
					(rate.modelYearID < romd.key.beginModelYearID || rate.modelYearID > romd.key.endModelYearID) {
				continue
			}
			outputKey.fromRomdKey(&currentKey)
			outputKey.fromSBbyAge(rate)
			outputRecord := outputRecords[outputKey]
			if outputRecord == nil {
				writeCount++
				outputRecord = new(baseRateOutputRecord)
				outputRecords[outputKey] = outputRecord

				outputRecord.sourceTypeID = romd.key.sourceTypeID
				outputRecord.roadTypeID = romd.key.roadTypeID
				if flags.useAvgSpeedBin {
					outputRecord.avgSpeedBinID = romd.key.avgSpeedBinID
				}
				outputRecord.hourDayID = romd.key.hourDayID
				outputRecord.polProcessID = romd.key.polProcessID
				outputRecord.processID = outputRecord.polProcessID % 100;
				outputRecord.pollutantID = outputRecord.polProcessID / 100;
				/* if flags.keepOpModeID {
					outputRecord.opModeID = romd.key.opModeID
				} else {
					outputRecord.opModeID = 0
				} */
				
				// EM- the above check does not work, becuase MOVES is run not to keep the opModes
				// regardless of whether its run in rates or inventory mode. The flags.keepOpModeID 
				// in other words, is always false. However, rates mode needs the opModeID to be kept.
				// Therefore, if we put in a check on the keepAvgSpeedBinID (which does change from 
				// rates to inventory, we should get the effect we're looking for without increasing
				// the runtime for inventory mode.
				if flags.useAvgSpeedBin || flags.keepOpModeID {
					outputRecord.opModeID = romd.key.opModeID
				} else {
					outputRecord.opModeID = 0
				}
				

				outputRecord.modelYearID = rate.modelYearID
				outputRecord.fuelTypeID = rate.fuelTypeID
				outputRecord.ageGroupID = rate.ageGroupID
				outputRecord.regClassID = rate.regClassID
			}
			// Accumulate output data

			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge without operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Not Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge without operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Not Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge with operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge with operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			sumSBD := rate.sumSBD
			if !flags.useSumSBD {
				sumSBD = 1
			}
			sumSBDRaw := rate.sumSBDRaw
			if !flags.useSumSBDRaw {
				sumSBDRaw = 1
			}
			opModeFraction := romd.opModeFraction
			avgBinSpeed := romd.avgBinSpeed
			avgSpeedFraction := romd.avgSpeedFraction
			if !flags.useAvgSpeedFraction {
				avgSpeedFraction = 1
			}
			
			t := opModeFraction * avgSpeedFraction * sumSBD
			if flags.keepOpModeID {
				outputRecord.opModeFraction += t * sumSBDRaw
			} else {
				outputRecord.opModeFraction += t
			}
			outputRecord.opModeFractionRate += t

			t = opModeFraction * avgSpeedFraction * sumSBDRaw
			outputRecord.meanBaseRate += rate.meanBaseRate * t
			outputRecord.meanBaseRateIM += rate.meanBaseRateIM * t
			outputRecord.meanBaseRateACAdj += rate.meanBaseRateACAdj * t
			outputRecord.meanBaseRateIMACAdj += rate.meanBaseRateIMACAdj * t
			if flags.useAvgSpeedBin {
				if avgBinSpeed > 0 {
					t = opModeFraction * avgSpeedFraction / avgBinSpeed
					outputRecord.emissionRate += rate.meanBaseRate * t
					outputRecord.emissionRateIM += rate.meanBaseRateIM * t
					outputRecord.emissionRateACAdj += rate.meanBaseRateACAdj * t
					outputRecord.emissionRateIMACAdj += rate.meanBaseRateIMACAdj * t
				}
			} else {
				t = opModeFraction * avgSpeedFraction
				outputRecord.emissionRate += rate.meanBaseRate * t
				outputRecord.emissionRateIM += rate.meanBaseRateIM * t
				outputRecord.emissionRateACAdj += rate.meanBaseRateACAdj * t
				outputRecord.emissionRateIMACAdj += rate.meanBaseRateIMACAdj * t
			}
		}
    }
	writeLines(outputRecords,true,files)
	files.close()

	fmt.Println("makeBaseRateByAgeFromSourceBinRates done, romdCount=",romdCount,", writeCount=",writeCount)
}

// Populate the BaseRate table from source-bin weighted emission rates.
func makeBaseRateFromSourceBinRates(romdQueue chan *romdBlock, sqlToWrite chan string) {
	defer globalevents.GeneratorDone()

	outputRecords := make(map[baseRateOutputKey]*baseRateOutputRecord)
	files := newTempFiles(baseRateTableFileBase,baseRateTableName,
			"sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, regClassID, opModeID," +
			"opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID",
			sqlToWrite)
	var romdCount, writeCount int

	var currentKey, previousKey romdKey
	hasPreviousKey := false

	for romd := range romdQueue {
    	romdCount++
		// Look for changes in ROMD's key so accumulated data can write written to the database.
		currentKey.sourceTypeID = romd.key.sourceTypeID
		currentKey.polProcessID = romd.key.polProcessID
		currentKey.roadTypeID = romd.key.roadTypeID
		currentKey.hourDayID = romd.key.hourDayID
		if flags.keepOpModeID {
			currentKey.opModeID = romd.key.opModeID
		}
		if flags.useAvgSpeedBin {
			currentKey.avgSpeedBinID = romd.key.avgSpeedBinID
		}
		// Everytime the ROMD's portion of the output table's unique key
		// changes, all accumulated data can be written to disk as it
		// will never be needed again.
		if hasPreviousKey && currentKey != previousKey {
			writeLines(outputRecords,false,files)
		}
		hasPreviousKey = true
		previousKey = currentKey

		// Get all rates that match sourceTypeID, polProcessID, and opModeID.
		var sbKey sbWeightedEmissionRateByAgeKey
		sbKey.sourceTypeID = romd.key.sourceTypeID
		sbKey.polProcessID = romd.key.polProcessID
		sbKey.opModeID = romd.key.opModeID
		rates, found := sbWeightedEmissionRate[sbKey]
		if !found && romd.key.opModeID >= 1000 {
			/*
			The ratesopmodedistribution contains entries for all vehicle combinations, expressed as opmode offsets, that exist at any time, 
			rather than just in the 30 year window of the runspec. However, other tables don't contain entries for these offset opmodes 
			because they ARE based on the runspec window. On top of this, the Go code handles this too elegantly, looking for non-offset 
			opmodes when it doesn't find the offset opmode that isn't relevant for the runspec. This leads to double-counting. 
			Putting in a continue, instead of checking for a non-offset opmode, prevents this double-counting from taking place. We are 
			leaving the original code (commented out) for added context to this comment.
			*/
			continue
			// sbKey.opModeID = romd.key.opModeID % 100
			// rates, found = sbWeightedEmissionRate[sbKey]
		}
		if !found {
			continue
		}
		var outputKey baseRateOutputKey
		var outputRecord *baseRateOutputRecord
		
		for _, rate := range rates {
			if romd.key.regClassID > 0 && romd.key.regClassID != rate.regClassID {
				continue
			}
			if romd.key.beginModelYearID > 0 && romd.key.endModelYearID > 0 &&
					(rate.modelYearID < romd.key.beginModelYearID || rate.modelYearID > romd.key.endModelYearID) {
				continue
			}
			outputKey.fromRomdKey(&currentKey)
			outputKey.fromSBbyAge(rate)
			outputRecord = outputRecords[outputKey]
			
			
			if outputRecord == nil {
				writeCount++
				outputRecord = new(baseRateOutputRecord)
				outputRecords[outputKey] = outputRecord

				outputRecord.sourceTypeID = romd.key.sourceTypeID
				outputRecord.roadTypeID = romd.key.roadTypeID
				if flags.useAvgSpeedBin {
					outputRecord.avgSpeedBinID = romd.key.avgSpeedBinID
				}
				outputRecord.hourDayID = romd.key.hourDayID
				outputRecord.polProcessID = romd.key.polProcessID
				outputRecord.processID = outputRecord.polProcessID % 100;
				outputRecord.pollutantID = outputRecord.polProcessID / 100;
				
				
				/* if flags.keepOpModeID {
					outputRecord.opModeID = romd.key.opModeID
				} else {
					outputRecord.opModeID = 0
				} */
				// EM- the above check does not work, becuase MOVES is run not to keep the opModes
				// regardless of whether its run in rates or inventory mode. The flags.keepOpModeID 
				// in other words, is always false. However, rates mode needs the opModeID to be kept.
				// Therefore, if we put in a check on the keepAvgSpeedBinID (which does change from 
				// rates to inventory, we should get the effect we're looking for without increasing
				// the runtime for inventory mode. This was copied from the makeBaseRateByAgeFromSourceBinRates
				// function.
				if flags.useAvgSpeedBin || flags.keepOpModeID {
					outputRecord.opModeID = romd.key.opModeID
				} else {
					outputRecord.opModeID = 0
				}

				outputRecord.modelYearID = rate.modelYearID
				outputRecord.fuelTypeID = rate.fuelTypeID
				outputRecord.ageGroupID = 0 // rate.ageGroupID
				outputRecord.regClassID = rate.regClassID
			}
			/**
			 * @step 110
			 * @algorithm Calculate BaseRate without operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRate
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Not Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate without operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Not Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate with operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate with operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			// Accumulate output data
			sumSBD := rate.sumSBD
			if !flags.useSumSBD {
				sumSBD = 1
			}
			sumSBDRaw := rate.sumSBDRaw
			if !flags.useSumSBDRaw {
				sumSBDRaw = 1
			}
			opModeFraction := romd.opModeFraction
			avgBinSpeed := romd.avgBinSpeed
			avgSpeedFraction := romd.avgSpeedFraction
			if !flags.useAvgSpeedFraction {
				avgSpeedFraction = 1
			}

			t := opModeFraction * avgSpeedFraction * sumSBD
			if flags.keepOpModeID {
				outputRecord.opModeFraction += t * sumSBDRaw
			} else {
				outputRecord.opModeFraction += t
			}
			outputRecord.opModeFractionRate += t

			t = opModeFraction * avgSpeedFraction * sumSBDRaw
			
			//EM - for ONI, we need to write an exception to undo the weighting
			//	This is becuase the weight causes the rates in ratePerVehicle to
			//	disagree with inventory for refueling, which is tied to energy consumption
			//	This is a corner case with a written exception, and should probably be 
			//	refactored in the near-medium term. 
			if flags.useAvgSpeedBin && outputRecord.roadTypeID == 1 && outputRecord.processID == 1 && outputRecord.pollutantID == 91 {
				//We only use pollutantID 91 (total energy consumption) because that's what refueling is chaind to
				t *= rate.sumSBDRaw
			}
			
			outputRecord.meanBaseRate += rate.meanBaseRate * t
			outputRecord.meanBaseRateIM += rate.meanBaseRateIM * t
			outputRecord.meanBaseRateACAdj += rate.meanBaseRateACAdj * t
			outputRecord.meanBaseRateIMACAdj += rate.meanBaseRateIMACAdj * t
			if flags.useAvgSpeedBin {
				if avgBinSpeed > 0 {
					t = opModeFraction * avgSpeedFraction / avgBinSpeed
					outputRecord.emissionRate += rate.meanBaseRate * t
					outputRecord.emissionRateIM += rate.meanBaseRateIM * t
					outputRecord.emissionRateACAdj += rate.meanBaseRateACAdj * t
					outputRecord.emissionRateIMACAdj += rate.meanBaseRateIMACAdj * t
				}
			} else {
				t = opModeFraction * avgSpeedFraction
				outputRecord.emissionRate += rate.meanBaseRate * t
				outputRecord.emissionRateIM += rate.meanBaseRateIM * t
				outputRecord.emissionRateACAdj += rate.meanBaseRateACAdj * t
				outputRecord.emissionRateIMACAdj += rate.meanBaseRateIMACAdj * t
			}
		}
    }
	writeLines(outputRecords,false,files)
	files.close()

	fmt.Println("makeBaseRateFromSourceBinRates done, romdCount=",romdCount,",writeCount=",writeCount)
}

// Populate the BaseRate table from distance-based source-bin weighted emission rates.
func makeBaseRateFromDistanceRates(sqlToWrite chan string) {
	defer globalevents.GeneratorDone()

	outputRecords := make(map[baseRateOutputKey]*baseRateOutputRecord)
	files := newTempFiles(baseRateTableFileBase,baseRateTableName,
			"sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, regClassID, opModeID," +
			"opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID",
			sqlToWrite)

	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	fmt.Println("Querying SBWeightedDistanceRate...")
	sql := "select sourceTypeID,avgSpeedBinID,polProcessID,modelYearID,fuelTypeID,regClassID," +
			" meanBaseRate,meanBaseRateIM,meanBaseRateACAdj,meanBaseRateIMACAdj,sumSBD,sumSBDRaw" +
			" from SBWeightedDistanceRate" +
			" where mod(polProcessID,100)=" + strconv.Itoa(flags.processID) +
			" order by sourceTypeID,polProcessID,modelYearID,fuelTypeID,regClassID,avgSpeedBinID"
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0

	var sourceTypeID,avgSpeedBinID,polProcessID,modelYearID,fuelTypeID,regClassID int
	var rateMeanBaseRate,rateMeanBaseRateIM,rateMeanBaseRateACAdj,rateMeanBaseRateIMACAdj,rateSumSBD,rateSumSBDRaw float64
	var roadTypeID, hourDayID int
	var rateAvgBinSpeed, rateAvgSpeedFraction float64
	var avgSpeedKey avgSpeedDistributionKey

	type distanceKey struct {
		sourceTypeID, polProcessID int
	}
	var currentKey, previousKey distanceKey
	hasPreviousKey := false

	for rows.Next() {
		rowCount++
		err = rows.Scan(&sourceTypeID,&avgSpeedBinID,&polProcessID,&modelYearID,&fuelTypeID,&regClassID,
				&rateMeanBaseRate,&rateMeanBaseRateIM,&rateMeanBaseRateACAdj,&rateMeanBaseRateIMACAdj,&rateSumSBD,&rateSumSBDRaw)
		configuration.CheckErr(err)

		currentKey.sourceTypeID = sourceTypeID
		currentKey.polProcessID = polProcessID
		if hasPreviousKey && currentKey != previousKey {
			writeLines(outputRecords,false,files)
		}
		hasPreviousKey = true
		previousKey = currentKey

		for _, roadTypeID = range runSpecRoadType {
			for _, hourDayID = range runSpecHourDay {
				avgSpeedKey.sourceTypeID = sourceTypeID
				avgSpeedKey.roadTypeID = roadTypeID
				avgSpeedKey.hourDayID = hourDayID
				avgSpeedKey.avgSpeedBinID = avgSpeedBinID
				avgSpeedDetail := avgSpeedDistribution[avgSpeedKey]
				if avgSpeedDetail != nil {
					rateAvgBinSpeed = avgSpeedDetail.avgBinSpeed
					rateAvgSpeedFraction = avgSpeedDetail.avgSpeedFraction
				} else {
					rateAvgBinSpeed = 0
					rateAvgSpeedFraction = 0
				}

				var outputKey baseRateOutputKey
				outputKey.sourceTypeID = sourceTypeID
				outputKey.roadTypeID = roadTypeID
				outputKey.polProcessID = polProcessID
				outputKey.hourDayID = hourDayID
				outputKey.modelYearID = modelYearID
				outputKey.fuelTypeID = fuelTypeID
				outputKey.regClassID = regClassID
				if flags.useAvgSpeedBin {
					outputKey.avgSpeedBinID = avgSpeedBinID
				}
				outputRecord := outputRecords[outputKey]
				if outputRecord == nil {
					outputRecord = new(baseRateOutputRecord)
					outputRecords[outputKey] = outputRecord

					outputRecord.sourceTypeID = sourceTypeID
					outputRecord.roadTypeID = roadTypeID
					outputRecord.polProcessID = polProcessID
					outputRecord.hourDayID = hourDayID
					outputRecord.modelYearID = modelYearID
					outputRecord.fuelTypeID = fuelTypeID
					outputRecord.regClassID = regClassID
					if flags.useAvgSpeedBin {
						outputRecord.avgSpeedBinID = avgSpeedBinID
					}
					outputRecord.ageGroupID = 0

					outputRecord.processID = outputRecord.polProcessID % 100;
					outputRecord.pollutantID = outputRecord.polProcessID / 100;
					if flags.keepOpModeID {
						outputRecord.opModeID = 300
					} else {
						outputRecord.opModeID = 0
					}
				}
				/**
				 * @step 110
				 * @algorithm Calculate BaseRate without operating mode, retaining average speed bin.
				 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause).
				 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
				 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * @output BaseRate
				 * @input SBWeightedDistanceRate
				 * @condition Not Start Exhaust
				 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
				**/

				/**
				 * @step 110
				 * @algorithm Calculate BaseRate without operating mode, aggregating average speed bins.
				 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause).
				 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
				 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * emissionRate=sum(MeanBaseRate * avgSpeedFractionClause).
				 * emissionRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause).
				 * emissionRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause).
				 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause).
				 * @output BaseRateByAge
				 * @input SBWeightedDistanceRate
				 * @condition Not Start Exhaust
				 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
				**/

				/**
				 * @step 110
				 * @algorithm Calculate BaseRate with operating mode 300, retaining average speed bin.
				 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
				 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
				 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
				 * @output BaseRateByAge
				 * @input RatesOpModeDistribution
				 * @input SBWeightedDistanceRate
				 * @condition Start Exhaust
				 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
				**/

				/**
				 * @step 110
				 * @algorithm Calculate BaseRate with operating mode 300, aggregating average speed bins.
				 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
				 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
				 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
				 * emissionRate=sum(MeanBaseRate * avgSpeedFractionClause).
				 * emissionRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause).
				 * emissionRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause).
				 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause).
				 * @output BaseRateByAge
				 * @input RatesOpModeDistribution
				 * @input SBWeightedDistanceRate
				 * @condition Start Exhaust
				 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
				**/

				// Accumulate output data
				sumSBD := rateSumSBD
				if !flags.useSumSBD {
					sumSBD = 1
				}
				sumSBDRaw := rateSumSBDRaw
				if !flags.useSumSBDRaw {
					sumSBDRaw = 1
				}
				avgBinSpeed := rateAvgBinSpeed
				avgSpeedFraction := rateAvgSpeedFraction
				if !flags.useAvgSpeedFraction {
					avgSpeedFraction = 1
				}

				t := avgSpeedFraction * sumSBD // opModeFraction == 1 for distance rates since only 1 operating mode (300)
				outputRecord.opModeFraction += t
				outputRecord.opModeFractionRate += t

				t = avgSpeedFraction * sumSBDRaw // opModeFraction == 1 for distance rates since only 1 operating mode (300)
				outputRecord.meanBaseRate += rateMeanBaseRate * t
				outputRecord.meanBaseRateIM += rateMeanBaseRateIM * t
				outputRecord.meanBaseRateACAdj += rateMeanBaseRateACAdj * t
				outputRecord.meanBaseRateIMACAdj += rateMeanBaseRateIMACAdj * t
				if flags.useAvgSpeedBin {
					if avgBinSpeed > 0 {
						t = avgSpeedFraction / avgBinSpeed // opModeFraction == 1 for distance rates since only 1 operating mode (300)
						outputRecord.emissionRate += rateMeanBaseRate * t
						outputRecord.emissionRateIM += rateMeanBaseRateIM * t
						outputRecord.emissionRateACAdj += rateMeanBaseRateACAdj * t
						outputRecord.emissionRateIMACAdj += rateMeanBaseRateIMACAdj * t
					}
				} else {
					t = avgSpeedFraction // opModeFraction == 1 for distance rates since only 1 operating mode (300)
					outputRecord.emissionRate += rateMeanBaseRate * t
					outputRecord.emissionRateIM += rateMeanBaseRateIM * t
					outputRecord.emissionRateACAdj += rateMeanBaseRateACAdj * t
					outputRecord.emissionRateIMACAdj += rateMeanBaseRateIMACAdj * t
				}
			}
		}
	}
	writeLines(outputRecords,false,files)
	files.close()

	fmt.Println("Done reading SBWeightedDistanceRate. Row Count=",rowCount)
}

// Create the required temporary table(s) and set and final SQL to move data to the primary table(s).
// Read the SourceUseTypePhysicsMapping table into memory.
func setupTables() {
	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	baseRateTableName = "baseRate_" + strconv.Itoa(flags.processID) + "_" + strconv.Itoa(flags.yearID)
	baseRateByAgeTableName = "baseRateByAge_" + strconv.Itoa(flags.processID) + "_" + strconv.Itoa(flags.yearID)

	baseRateTableFileBase, _ = filepath.Abs(baseRateTableName)
	baseRateByAgeTableFileBase, _ = filepath.Abs(baseRateByAgeTableName)

	readAvgSpeedDistribution(db)
	readSourceUseTypePhysicsMapping(db)
	readSBWeightedEmissionRateByAge(db)
	readSBWeightedEmissionRate(db)
	readRunSpecRoadType(db)
	readRunSpecHourDay(db)
	readRunSpecSourceType(db)
	readRunSpecPollutantProcess(db)
	readRunSpecModelYear(db)

	readAvgSpeedBin(db)
	readDriveSchedule(db)
	readDriveScheduleAssoc(db)
	readOperatingMode(db)
}

// Read the AvgSpeedBin table into memory.
func readAvgSpeedBin(db *sql.DB) {
	fmt.Println("Querying AvgSpeedBin...")
	sql := "select avgSpeedBinID, avgBinSpeed from avgspeedbin"
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	var k int
	var d float64
	for rows.Next() {
		rowCount++
		err = rows.Scan(&k, &d)
		configuration.CheckErr(err)
		avgSpeedBin[k] = d
	}
	fmt.Println("Done reading AvgSpeedBin. Row Count=",rowCount)
}

// Read the DriveSchedule table into memory.
func readDriveSchedule(db *sql.DB) {
	fmt.Println("Querying DriveSchedule...")
	sql := "select driveScheduleID, averageSpeed from driveSchedule"
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	var k int
	var d float64
	for rows.Next() {
		rowCount++
		err = rows.Scan(&k, &d)
		configuration.CheckErr(err)
		driveSchedule[k] = d
	}
	fmt.Println("Done reading DriveSchedule. Row Count=",rowCount)
}

// Read the AvgSpeedDistribution table into memory.
func readAvgSpeedDistribution(db *sql.DB) {
	fmt.Println("Querying AvgSpeedDistribution...")
	sql := "select sourceTypeID,roadTypeID,hourDayID,avgSpeedBinID,avgSpeedFraction,avgBinSpeed" +
			" from avgSpeedDistribution" +
			" inner join avgSpeedBin using (avgSpeedBinID)"
	if flags.roadTypeID > 0 {
		sql += " where roadTypeID=" + strconv.Itoa(flags.roadTypeID)
	}
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	var k avgSpeedDistributionKey
	for rows.Next() {
		rowCount++
		d := new(avgSpeedDistributionDetail)
		err = rows.Scan(&k.sourceTypeID, &k.roadTypeID, &k.hourDayID, &k.avgSpeedBinID, &d.avgSpeedFraction, &d.avgBinSpeed)
		configuration.CheckErr(err)
		avgSpeedDistribution[k] = d
	}
	fmt.Println("Done reading AvgSpeedDistribution. Row Count=",rowCount)
}

// Read the SourceUseTypePhysicsMapping table into memory.
func readSourceUseTypePhysicsMapping(db *sql.DB) {
	fmt.Println("Querying SourceUseTypePhysicsMapping...")
	SourceUseTypePhysicsMapping = make([]*SourceUseTypePhysicsMappingDetail,0,10000)
	SourceUseTypePhysicsMappingByTempSourceType = make(map[int]*SourceUseTypePhysicsMappingDetail)
	SourceUseTypePhysicsMappingByRealSourceType = make(map[int]*SourceUseTypePhysicsMappingDetail)
	rows, err := db.Query("select distinct realSourceTypeID, tempSourceTypeID, opModeIDOffset," +
			" regClassID, beginModelYearID, endModelYearID," +
			" rollingTermA, rotatingTermB, dragTermC, sourceMass, fixedMassFactor" +
			" from sourceUseTypePhysicsMapping" +
			//" where realSourceTypeID <> tempSourceTypeID" +
			" order by realSourceTypeID, beginModelYearID")
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		d := new(SourceUseTypePhysicsMappingDetail)
		err = rows.Scan(&d.RealSourceTypeID,&d.TempSourceTypeID,&d.OpModeIDOffset,
				&d.regClassID, &d.beginModelYearID, &d.endModelYearID,
				&d.rollingTermA, &d.rotatingTermB, &d.dragTermC, &d.sourceMass, &d.fixedMassFactor)
		configuration.CheckErr(err)
		SourceUseTypePhysicsMapping = append(SourceUseTypePhysicsMapping,d)
		SourceUseTypePhysicsMappingByTempSourceType[d.TempSourceTypeID] = d
		SourceUseTypePhysicsMappingByRealSourceType[d.RealSourceTypeID] = d // ok to overwrite something else
	}
	fmt.Println("Done reading SourceUseTypePhysicsMapping. Row Count=",rowCount)
}

// Read the SBWeightedEmissionRateByAge table into memory.
func readSBWeightedEmissionRateByAge(db *sql.DB) {
	fmt.Println("Querying SBWeightedEmissionRateByAge...")
	rows, err := db.Query("select sourceTypeID, polProcessID, opModeID," +
			" modelYearID, fuelTypeID, ageGroupID, regClassID," +
			" sumSBD, sumSBDRaw, " +
			" meanBaseRate, meanBaseRateIM, meanBaseRateACAdj, meanBaseRateIMACAdj " +
			" from SBWeightedEmissionRateByAge" +
			" where mod(polProcessID,100) = " + strconv.Itoa(flags.processID))
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		d := new(sbWeightedEmissionRateByAgeDetail)
		err = rows.Scan(&d.sourceTypeID, &d.polProcessID, &d.opModeID,
					&d.modelYearID, &d.fuelTypeID, &d.ageGroupID, &d.regClassID,
					&d.sumSBD, &d.sumSBDRaw,
					&d.meanBaseRate, &d.meanBaseRateIM, &d.meanBaseRateACAdj, &d.meanBaseRateIMACAdj)
		configuration.CheckErr(err)
		var k sbWeightedEmissionRateByAgeKey
		k.sourceTypeID = d.sourceTypeID
		k.polProcessID = d.polProcessID
		k.opModeID = d.opModeID
		detailList, found := sbWeightedEmissionRateByAge[k]
		if !found {
			detailList = make([]*sbWeightedEmissionRateByAgeDetail,0,20)
		}
		detailList = append(detailList,d)
		sbWeightedEmissionRateByAge[k] = detailList
	}
	fmt.Println("Done reading SBWeightedEmissionRateByAge. Row Count=",rowCount)
}

// Read the SBWeightedEmissionRate table into memory.
func readSBWeightedEmissionRate(db *sql.DB) {
	fmt.Println("Querying SBWeightedEmissionRate...")
	rows, err := db.Query("select sourceTypeID, polProcessID, opModeID," +
			" modelYearID, fuelTypeID, regClassID," +
			" sumSBD, sumSBDRaw, " +
			" meanBaseRate, meanBaseRateIM, meanBaseRateACAdj, meanBaseRateIMACAdj " +
			" from SBWeightedEmissionRate" +
			" where mod(polProcessID,100) = " + strconv.Itoa(flags.processID))
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		d := new(sbWeightedEmissionRateByAgeDetail)
		err = rows.Scan(&d.sourceTypeID, &d.polProcessID, &d.opModeID,
					&d.modelYearID, &d.fuelTypeID, &d.regClassID,
					&d.sumSBD, &d.sumSBDRaw,
					&d.meanBaseRate, &d.meanBaseRateIM, &d.meanBaseRateACAdj, &d.meanBaseRateIMACAdj)
		configuration.CheckErr(err)
		var k sbWeightedEmissionRateByAgeKey
		k.sourceTypeID = d.sourceTypeID
		k.polProcessID = d.polProcessID
		k.opModeID = d.opModeID
		detailList, found := sbWeightedEmissionRate[k]
		if !found {
			detailList = make([]*sbWeightedEmissionRateByAgeDetail,0,20)
		}
		detailList = append(detailList,d)
		sbWeightedEmissionRate[k] = detailList

		if k.opModeID >= 1000 {
			k.opModeID = k.opModeID % 100
			detailList, found := sbWeightedEmissionRate[k]
			if !found {
				detailList = make([]*sbWeightedEmissionRateByAgeDetail,0,20)
			}
			detailList = append(detailList,d)
			sbWeightedEmissionRate[k] = detailList
		}
	}
	fmt.Println("Done reading SBWeightedEmissionRate. Row Count=",rowCount)
}

// Read the runSpecRoadType table into memory.
func readRunSpecRoadType(db *sql.DB) {
	fmt.Println("Querying runSpecRoadType...")
	sql := "select roadTypeID" +
			" from runSpecRoadType" +
			" where roadTypeID > 0 and roadTypeID < 100"
	if flags.roadTypeID > 0 {
		sql += " and roadTypeID=" + strconv.Itoa(flags.roadTypeID)
	}
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		roadTypeID := 0
		err = rows.Scan(&roadTypeID)
		configuration.CheckErr(err)
		if roadTypeID != 1 {
			runSpecRoadType = append(runSpecRoadType,roadTypeID)
		}
		runSpecRoadTypeWithOffNetwork = append(runSpecRoadTypeWithOffNetwork,roadTypeID)
	}
	fmt.Println("Done reading runSpecRoadType. Row Count=",rowCount)
}

// Read the runSpecHourDay table into memory.
func readRunSpecHourDay(db *sql.DB) {
	fmt.Println("Querying runSpecHourDay...")
	sql := "select hourDayID" +
			" from runSpecHourDay"
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		hourDayID := 0
		err = rows.Scan(&hourDayID)
		configuration.CheckErr(err)
		runSpecHourDay = append(runSpecHourDay,hourDayID)
	}
	fmt.Println("Done reading runSpecHourDay. Row Count=",rowCount)
}

// Read the runSpecSourceType table into memory.
func readRunSpecSourceType(db *sql.DB) {
	fmt.Println("Querying runSpecSourceType...")
	sql := "select sourceTypeID" +
			" from runSpecSourceType"
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		sourceTypeID := 0
		err = rows.Scan(&sourceTypeID)
		configuration.CheckErr(err)
		runSpecSourceType = append(runSpecSourceType,sourceTypeID)
	}
	fmt.Println("Done reading runSpecSourceType. Row Count=",rowCount)
}

// Read the runSpecPollutantProcess table into memory.
// Data is filtered to only those entries required for the current processID
// and further to those that need drive cycle calculations (operating modes 0-99).
func readRunSpecPollutantProcess(db *sql.DB) {
	fmt.Println("Querying runSpecPollutantProcess...")
	rows, err := db.Query("select distinct polProcessID" +
			" from runspecpollutantprocess" +
			" inner join opmodepolprocassoc using (polProcessID)" +
			" where polProcessID > 0" +
			" and opModeID >= 0 and opModeID < 100" +
			" and mod(polProcessID,100)=" + strconv.Itoa(flags.processID))
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		polProcessID := 0
		err = rows.Scan(&polProcessID)
		configuration.CheckErr(err)
		runSpecPolProcessID = append(runSpecPolProcessID,polProcessID)
	}
	fmt.Println("Done reading runSpecPollutantProcess. Row Count=",rowCount)
}

// Read the runSpecModelYear table into memory.
func readRunSpecModelYear(db *sql.DB) {
	fmt.Println("Querying runSpecModelYear...")
	rows, err := db.Query("select modelYearID from runspecmodelyear")
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	for rows.Next() {
		rowCount++
		modelYearID := 0
		err = rows.Scan(&modelYearID)
		configuration.CheckErr(err)
		runSpecModelYear[modelYearID] = true
	}
	fmt.Println("Done reading runSpecModelYear. Row Count=",rowCount)
}

// Read the DriveScheduleAssoc table into memory.
func readDriveScheduleAssoc(db *sql.DB) {
	fmt.Println("Querying DriveScheduleAssoc...")
	sql := "select sourceTypeID, roadTypeID, driveScheduleID from driveScheduleAssoc inner join runspecRoadType using (roadTypeID)"
	if flags.roadTypeID > 0 {
		sql += " where roadTypeID=" + strconv.Itoa(flags.roadTypeID)
	}
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	var k driveScheduleAssocKey
	var d int
	for rows.Next() {
		rowCount++
		err = rows.Scan(&k.sourceTypeID, &k.roadTypeID, &d)
		configuration.CheckErr(err)
		detailList, found := driveScheduleAssoc[k]
		if !found {
			detailList = make([]int,0,20)
		}
		detailList = append(detailList,d)
		driveScheduleAssoc[k] = detailList
	}
	fmt.Println("Done reading DriveScheduleAssoc. Row Count=",rowCount)
}

// Read the OperatingMode table into memory. Only modes > 1 and < 100 are read.
func readOperatingMode(db *sql.DB) {
	fmt.Println("Querying OperatingMode...")
	sql := "select opModeID,ifnull(VSPLower,0),ifnull(VSPUpper,0),ifnull(speedLower,0),ifnull(speedUpper,0)," +
	 		" isnull(VSPLower),isnull(VSPUpper),isnull(speedLower),isnull(speedUpper)" +
			" from operatingMode" +
			" where opModeID > 1 and opModeID < 100 and opModeID not in (26,36)"
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	var k int
	var d *operatingMode
	for rows.Next() {
		rowCount++
		d = new(operatingMode)
		err = rows.Scan(&k,&d.VSPLower,&d.VSPUpper,&d.speedLower,&d.speedUpper,
			&d.isnullVSPLower,&d.isnullVSPUpper,&d.isnullSpeedLower,&d.isnullSpeedUpper)
		configuration.CheckErr(err)
		d.opModeID = k
		operatingModes[k] = d
	}
	fmt.Println("Done reading OperatingMode. Row Count=",rowCount)
}

// Unique key for a bracketed speedbin
type DriveCycleBracketedBinKey struct {
	pDetail *SourceUseTypePhysicsMappingDetail
	roadTypeID, avgSpeedBinID int
}

// A bracketed speedbin
type DriveCycleBracketedBinDetail struct {
	scheduleFractions map[int]float64
	opModeFractions map[int]float64
}

// Unique key for a physics/driveschedule combination
type DriveScheduleOpModeDistributionKey struct {
	pDetail *SourceUseTypePhysicsMappingDetail
	driveScheduleID int
}

// Operating mode distribution for a physics/driveschedule combination
type DriveScheduleOpModeDistributionDetail struct {
	opModeFractions map[int]float64
}

// Find drive cycles that bracket each source type, road type, and speed bin
func findDriveCycles(db *sql.DB,
		driveCycleBracketedBins map[DriveCycleBracketedBinKey]*DriveCycleBracketedBinDetail,
		driveScheduleOpModeDistributions map[DriveScheduleOpModeDistributionKey]*DriveScheduleOpModeDistributionDetail) {
	fmt.Println("findDriveCycles...")
	defer fmt.Println("findDriveCycles Done.")

	allowedRoadTypes := make(map[int]bool)
	for _, roadTypeID := range runSpecRoadType {
		allowedRoadTypes[roadTypeID] = true
	}
	// For every combination of sourceTypeID, roadTypeID...
	for srKey, srCycles := range driveScheduleAssoc {
		if !allowedRoadTypes[srKey.roadTypeID] {
			continue
		}
		// For every average speed bin...
		for avgSpeedBinID, avgBinSpeed := range avgSpeedBin {
			// Find the bracketing drive schedules associated with s/r
			bestLowDriveScheduleID := -1 // -1 == no available cycle
			bestLowDriveScheduleSpeed := -100.0
			bestHighDriveScheduleID := -1 // -1 == no available cycle
			bestHighDriveScheduleSpeed := 100000.0
			lowDriveScheduleFraction := 0.0
			highDriveScheduleFraction := 0.0

			for _, driveScheduleID := range srCycles {
				driveScheduleSpeed := driveSchedule[driveScheduleID]
				if driveScheduleSpeed <= avgBinSpeed {
					if driveScheduleSpeed > bestLowDriveScheduleSpeed {
						bestLowDriveScheduleSpeed = driveScheduleSpeed
						bestLowDriveScheduleID = driveScheduleID
					}
				}
				if driveScheduleSpeed >= avgBinSpeed {
					if driveScheduleSpeed < bestHighDriveScheduleSpeed {
						bestHighDriveScheduleSpeed = driveScheduleSpeed
						bestHighDriveScheduleID = driveScheduleID
					}
				}
			}
			// Calculate the fraction of each drive cycle. The farther the cycle's
			// speed is from the bin's speed, the less the cycle influences the results.
			totalSpan := bestHighDriveScheduleSpeed - bestLowDriveScheduleSpeed
			if totalSpan <= 0 {
				lowDriveScheduleFraction = 1.0
				highDriveScheduleFraction = 0.0
			} else if bestLowDriveScheduleID < 0 { // If no lower bounding cycle...
				lowDriveScheduleFraction = 0
				highDriveScheduleFraction = 1
				// Complain about missing data
				fmt.Println("WARNING: All driving cycles for avgSpeedBinID " + strconv.Itoa(avgSpeedBinID) +
						" for sourcetype " + strconv.Itoa(srKey.sourceTypeID) +
						" on roadtype " + strconv.Itoa(srKey.roadTypeID) +
						" were too fast." +
						" MOVES results for this speed were extrapolated from the closest available driving cycles.")
			} else if bestHighDriveScheduleID < 0 { // If no upper bounding cycle...
				lowDriveScheduleFraction = 1
				highDriveScheduleFraction = 0
				// Complain about missing data
				fmt.Println("WARNING: All driving cycles for avgSpeedBinID " + strconv.Itoa(avgSpeedBinID) +
						" for sourcetype " + strconv.Itoa(srKey.sourceTypeID) +
						" on roadtype " + strconv.Itoa(srKey.roadTypeID) +
						" were too slow." +
						" MOVES results for this speed were extrapolated from the closest available driving cycles.")
			} else {
				// Assign proportions. Note the swapped variables.
				lowDriveScheduleFraction = (bestHighDriveScheduleSpeed - avgBinSpeed) / totalSpan
				highDriveScheduleFraction = (avgBinSpeed - bestLowDriveScheduleSpeed) / totalSpan
			}

			cyclesToUse := make([]int,0,2)
			fractionsToUse := make([]float64,0,2)
			if lowDriveScheduleFraction > 0 {
				cyclesToUse = append(cyclesToUse,bestLowDriveScheduleID)
				fractionsToUse = append(fractionsToUse,lowDriveScheduleFraction)
			}
			if highDriveScheduleFraction > 0 {
				cyclesToUse = append(cyclesToUse,bestHighDriveScheduleID)
				fractionsToUse = append(fractionsToUse,highDriveScheduleFraction)
			}
			// For every source type physics that applies to the base source type...
			//var SourceUseTypePhysicsMapping []*SourceUseTypePhysicsMappingDetail
			for _, pDetail := range SourceUseTypePhysicsMapping {
				if pDetail.RealSourceTypeID != srKey.sourceTypeID {
					continue
				}
				// Record the physics mapping for each of the cycles to be used
				for ci, _ := range cyclesToUse {
					scheduleID := cyclesToUse[ci]
					fractionToUse := fractionsToUse[ci]
					//fmt.Println("physrc",pDetail.TempSourceTypeID,"src",srKey.sourceTypeID,"road",srKey.roadTypeID,"bin",avgSpeedBinID,"schedule",scheduleID,"fraction",fractionToUse)

					// Record the combinations of physics/driveschedule/roadtype/avgSpeedBin
					dcbKey := DriveCycleBracketedBinKey{pDetail,srKey.roadTypeID,avgSpeedBinID}
					dcbDetail := driveCycleBracketedBins[dcbKey]
					if dcbDetail == nil {
						dcbDetail = new(DriveCycleBracketedBinDetail)
						dcbDetail.scheduleFractions = make(map[int]float64)
						dcbDetail.opModeFractions = make(map[int]float64)
						driveCycleBracketedBins[dcbKey] = dcbDetail
					}
					dcbDetail.scheduleFractions[scheduleID] = fractionToUse

					// Record the unique combinations of physics/driveschedule
					dsodKey := DriveScheduleOpModeDistributionKey{pDetail,scheduleID}
					dsodDetail := driveScheduleOpModeDistributions[dsodKey]
					if dsodDetail == nil {
						dsodDetail = new(DriveScheduleOpModeDistributionDetail)
						dsodDetail.opModeFractions = make(map[int]float64)
						driveScheduleOpModeDistributions[dsodKey] = dsodDetail
					}
				}
			}
		}
	}
}

// Calculate the operating mode distribution for a drive cycle given the physics factors to be used.
// Operating mode 501 is a special case for zero speed seconds. When used for pollutant/process
// 11609, it should stay operating mode 501. When used for any other pollutant/process,
// it should be converted to opModeID 1 and summed into its fraction. Doing calculations this way
// prevents any need to do second-by-second calculations for each pollutant/process.
func calculateDriveCycleOpModeDistribution(db *sql.DB,pDetail *SourceUseTypePhysicsMappingDetail,
		driveScheduleID int, opModeFractions map[int]float64) {
	type secondDetail struct {
		hasOpMode bool
		second, opModeID int
		speed, acceleration, vsp float64
		speedMPH, accelerationMPH float64 // Miles Per Hour-based speed and acceleration
	}
	details := make(map[int]*secondDetail)
	opModeTotals := make(map[int]int)
	totalSeconds := 0

	shouldDebug := false
	/*
	if driveScheduleID==101 && pDetail.RealSourceTypeID==21 && pDetail.beginModelYearID==1960 {
		shouldDebug = true
	}
	*/

	// Read the speed for each second...
	sql := "select second, speed from driveScheduleSecond where driveScheduleID=" + strconv.Itoa(driveScheduleID)
	rows, err := db.Query(sql)
	configuration.CheckErr(err)

	defer rows.Close()
	rowCount := 0
	var k, firstSecond, lastSecond int
	var d float64
	firstSecond = 999999
	lastSecond = -999999
	for rows.Next() {
		rowCount++
		err = rows.Scan(&k, &d)
		configuration.CheckErr(err)
		detail, found := details[k]
		if !found {
			detail = new(secondDetail)
			details[k] = detail
		}
		detail.speed = d
		detail.speedMPH = d * 0.44704
		/**
		 * @algorithm Assign the Idle operating mode (1) to speeds < 1 m/s.
		 * OpModeID=IF(speed=0 and polProcessID=11609,501,if(speed<1.0,1,opModeID)).
		 * @output OpModeIDBySecond
		**/
		if detail.speed == 0.0 {
			//EM - the behaviour here should be different between project scale and national scale, per EMT-443
			if configuration.Singleton.IsProject {
				// at project scale, we set the opModeID to 501 because we don't want any emissions
				detail.opModeID = 501
			} else {
				// at national/county scale, we se the opModeID to 1 becuase that matches the actual operations
				detail.opModeID = 1
			}
			detail.hasOpMode = true
		} else if detail.speed < 1.0 {
			detail.opModeID = 1
			detail.hasOpMode = true
		}
		// Keep track of the range of seconds covered by the drive cycle
		if firstSecond > k {
			firstSecond = k
		}
		if lastSecond < k {
			lastSecond = k
		}
	}

	/**
	 * @algorithm Get the acceleration of every second beyond the first.
	 * acceleration[t] = speed[t] - speed[t-1].
	**/
	for second := firstSecond+1; second <= lastSecond; second++ {
		now := details[second]
		then := details[second-1]
		if now != nil && then != nil {
			now.acceleration = now.speed - then.speed
			now.accelerationMPH = now.speedMPH - then.speedMPH
		}
	}
	/**
	 * @step 150
	 * @algorithm Get the acceleration of the first second.
	 * acceleration[fist second] = speed[2nd second] - speed[first second].
	**/
	details[firstSecond].acceleration = details[firstSecond+1].acceleration
	details[firstSecond].accelerationMPH = details[firstSecond+1].accelerationMPH

	for second := firstSecond; second <= lastSecond; second++ {
		now := details[second]
		back1 := details[second-1]
		back2 := details[second-2]
		if now != nil && !now.hasOpMode {
			/**
			 * @algorithm Find braking events, copying information from time t that meet braking conditions
			 * and setting opModeID=0.
			 * @condition acceleration[t] <= -2 or (acceleration[t] < -1 and acceleration[t-1] < -1 and acceleration[t-2] < -1)
			**/
			if now.acceleration <= -2 {
				now.opModeID = 0
				now.hasOpMode = true
			} else if back1 != nil && back2 != nil {
				if now.acceleration < -1 && back1.acceleration < -1 && back2.acceleration < -1 {
					now.opModeID = 0
					now.hasOpMode = true
				}
			}

			/**
			 * @algorithm Calculate VSP for each second of each drive schedule, using sourceTypeID-specific terms.
			 * VSP=(rollingTermA * speedMPH + rotatingTermB * POW(speedMPH,2) + dragTermC * POW(speedMPH,3) + sourceMass * speedMPH * accelerationMPH) / fixedMassFactor.
			 * @output VSP
			 * @input SourceTypeDriveSchedule
			 * @input DriveScheduleSecond
			 * @input sourceUseTypePhysicsMapping
			**/
			if !now.hasOpMode && pDetail.sourceMass > 0 && pDetail.fixedMassFactor > 0 {
				now.vsp = (pDetail.rollingTermA * now.speedMPH + pDetail.rotatingTermB * math.Pow(now.speedMPH,2) + pDetail.dragTermC * math.Pow(now.speedMPH,3) + pDetail.sourceMass * now.speedMPH * now.accelerationMPH) / pDetail.fixedMassFactor
			}

			/**
			 * @algorithm Assign an opModeID to each second based upon operating mode VSP and speed
			 * information.
			 * @input VSP
			 * @output opModeID
			 * @condition 1 < opModeID < 100, opModeID not previously assigned
			**/
			if !now.hasOpMode {
				for opModeID, d := range operatingModes {
					if !d.isnullVSPLower && !(now.vsp >= d.VSPLower) {
						// If VSPLower matters and now's VSP doesn't fit, try another operating mode.
						continue
					}
					if !d.isnullVSPUpper && !(now.vsp < d.VSPUpper) {
						// If VSPUpper matters and now's VSP doesn't fit, try another operating mode.
						continue
					}
					if !d.isnullSpeedLower && !(now.speed >= d.speedLower) {
						// If speedLower matters and now's speed doesn't fit, try another operating mode.
						continue
					}
					if !d.isnullSpeedUpper && !(now.speed < d.speedUpper) {
						// If speedUpper matters and now's speed doesn't fit, try another operating mode.
						continue
					}
					// Everything that matters matches now's information, so use the operating mode.
					now.hasOpMode = true
					now.opModeID = opModeID
					break // Dont' try another operating mode
				}
			}
		}
		if now != nil && now.hasOpMode && second > 0 { // ">0" clause added to mimic quirk of Java code
			opModeTotals[now.opModeID] = 1 + opModeTotals[now.opModeID]
			totalSeconds++
		}
	}

	if totalSeconds > 0 {
		oneOverTotalSeconds := 1.0/float64(totalSeconds)
		if shouldDebug {
			fmt.Println("driveScheduleID=",driveScheduleID," totalSeconds=",totalSeconds)
			fmt.Println("opModeID seconds")
		}
		for opModeID, secondsInOpMode := range opModeTotals {
			opModeFractions[opModeID] = float64(secondsInOpMode) * oneOverTotalSeconds
			if shouldDebug {
				fmt.Println(opModeID," ",secondsInOpMode)
			}
		}
		if shouldDebug {
			fmt.Println("end driveScheduleID=",driveScheduleID)
		}
	}
}

// Make operating mode distributions for drive cycles
func processDriveCycles(romdForBaseRateQueue, romdForBaseRateByAgeQueue chan *romdBlock, sqlToWrite chan string) {
	fmt.Println("processDriveCycles...")
	defer fmt.Println("processDriveCycles Done.")

	defer globalevents.GeneratorDone()

	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	var files *tempFiles
	if configuration.Singleton.SaveROMD {
		createROMDTable(sqlToWrite)
		romdTableName := "ratesOpModeDistributionDetail"
		romdFileBase, _ := filepath.Abs(romdTableName)
		files = newTempFiles(romdFileBase,romdTableName,
				"sourceTypeID, polProcessID, roadTypeID, hourDayID, opModeID, avgSpeedBinID," +
				"beginModelYearID, endModelYearID, regClassID," +
				"opModeFraction, avgBinSpeed, avgSpeedFraction",
				sqlToWrite)
	}

	driveCycleBracketedBins := make(map[DriveCycleBracketedBinKey]*DriveCycleBracketedBinDetail)
	driveScheduleOpModeDistributions := make(map[DriveScheduleOpModeDistributionKey]*DriveScheduleOpModeDistributionDetail)

	// Find the drive cycles to be used
	findDriveCycles(db,driveCycleBracketedBins,driveScheduleOpModeDistributions)
	fmt.Println("driveCycleBracketedBins=",len(driveCycleBracketedBins))
	fmt.Println("driveScheduleOpModeDistributions=",len(driveScheduleOpModeDistributions))

	// Calculate the operating mode distributions for each combination of physics/drivecycle
	didPrint := false
	for k, d := range driveScheduleOpModeDistributions {
		calculateDriveCycleOpModeDistribution(db,k.pDetail,k.driveScheduleID,d.opModeFractions)
		if !didPrint && len(d.opModeFractions) > 0 {
			didPrint = true
			/*
			fmt.Println("For drive schedule ",k.driveScheduleID)
			for opModeID, opModeFraction := range d.opModeFractions {
				fmt.Println("opmode[",opModeID,"]=",opModeFraction)
			}
			*/
		}
	}

	// Combine the drive cycle operating mode distributions.
	for dcbKey, dcbDetail := range driveCycleBracketedBins {
		for driveScheduleID, driveScheduleFraction := range dcbDetail.scheduleFractions {
			dsoKey := DriveScheduleOpModeDistributionKey{dcbKey.pDetail,driveScheduleID}
			dsoDetail := driveScheduleOpModeDistributions[dsoKey]
			if dsoDetail == nil {
				continue
			}
			for opModeID, opModeFraction := range dsoDetail.opModeFractions {
				dcbDetail.opModeFractions[opModeID] = dcbDetail.opModeFractions[opModeID] + driveScheduleFraction*opModeFraction
			}
		}
	}

	opModesToIterate := make([]int,0,100)
	opModesToIterate = append(opModesToIterate,0)
	opModesToIterate = append(opModesToIterate,1)
	opModesToIterate = append(opModesToIterate,501)
	for opModeID, _ := range operatingModes {
		opModesToIterate = append(opModesToIterate,opModeID)
	}

	// Speedup access to bracketed bins
	type dcbFastKey struct {
		sourceTypeID, roadTypeID, avgSpeedBinID int
	}
	driveCycleBracketedBinsFast := make(map[dcbFastKey][]*DriveCycleBracketedBinKey)
	for dcbKey, _ := range driveCycleBracketedBins {
		k := dcbFastKey{dcbKey.pDetail.RealSourceTypeID,dcbKey.roadTypeID,dcbKey.avgSpeedBinID}
		v, found := driveCycleBracketedBinsFast[k]
		if !found {
			v = make([]*DriveCycleBracketedBinKey,0,20)
		}
		var dk DriveCycleBracketedBinKey
		dk = dcbKey
		v = append(v,&dk)
		driveCycleBracketedBinsFast[k] = v
	}

	// Store the idle fractions, needed for off-network idling (ONI).

	/**
	* @algorithm drivingIdleFraction = Sum(opModeFraction[opMode=1]*avgSpeedFraction)/Sum(opModeFraction[all opModes]*avgSpeedFraction).
	* @input operating mode distribution from drive cycles
	* @output DrivingIdleFraction
	**/
	idleFractionTableName := "drivingIdleFraction"
	idleFractionFileBase, _ := filepath.Abs(idleFractionTableName)
	idleFractionFiles := newTempFiles(idleFractionFileBase,idleFractionTableName,
			"hourDayID,yearID,roadTypeID,sourceTypeID,drivingIdleFraction",
			sqlToWrite)
	var avgSpeedKey avgSpeedDistributionKey
	var dcbfk dcbFastKey
	for _, sourceTypeID := range runSpecSourceType {
		for _, roadTypeID := range runSpecRoadType { // only use roads with driving cycles
			for _, hourDayID := range runSpecHourDay {
				// Note: AgeID is skipped as idling is unaffected by source type physics
				// ----- and thus does not vary by model year.
				idlingFraction := 0.0
				notIdlingFraction := 0.0
				for _, opModeID := range opModesToIterate {
					for avgSpeedBinID, _ := range avgSpeedBin {
						avgSpeedKey.sourceTypeID = sourceTypeID 
						avgSpeedKey.roadTypeID = roadTypeID
						avgSpeedKey.hourDayID = hourDayID
						avgSpeedKey.avgSpeedBinID = avgSpeedBinID
						avgSpeedDetail, found := avgSpeedDistribution[avgSpeedKey]
						if !found {
							continue
						}
						if avgSpeedDetail.avgSpeedFraction <= 0 {
							continue
						}

						dcbfk.sourceTypeID = sourceTypeID
						dcbfk.roadTypeID = roadTypeID
						dcbfk.avgSpeedBinID = avgSpeedBinID
						dcbKeys, found := driveCycleBracketedBinsFast[dcbfk]
						if found {
							for _, dcbKey := range dcbKeys {
								dcbDetail := driveCycleBracketedBins[*dcbKey]
								if len(dcbDetail.opModeFractions) == 0 {
									continue
								}
								opModeFraction := dcbDetail.opModeFractions[opModeID]
								// opModeFraction += dcbDetail.opModeFractions[501]
								if opModeFraction <= 0 {
									continue
								}
								if opModeID == 1 || opModeID == 501 {
									idlingFraction += opModeFraction * avgSpeedDetail.avgSpeedFraction
								} else {
									notIdlingFraction += opModeFraction * avgSpeedDetail.avgSpeedFraction
								}
							}
						}
					}
				}
				// Normalize the fractions
				totalFraction := idlingFraction + notIdlingFraction
				if totalFraction > 0 {
					// Normalize the idling fraction
					idlingFraction = idlingFraction / totalFraction
					// Write the idling fraction
					line := strconv.Itoa(hourDayID) +
							"\t" + strconv.Itoa(flags.yearID) +
							"\t" + strconv.Itoa(roadTypeID) +
							"\t" + strconv.Itoa(sourceTypeID) +
							"\t" + strconv.FormatFloat(idlingFraction,'e',-1,64) +
							"\n"
					idleFractionFiles.writeLine(line)
				}
			}
		}
	}

	// For ONI, the set of allowed regclasses for each source type must be known.
	// Discover this by iterating through drive cycle brackets which have both.
	regClassesBySourceType := make(map[int][]int)
	for dcbKey, _ := range driveCycleBracketedBins {
		regClasses, found := regClassesBySourceType[dcbKey.pDetail.RealSourceTypeID]
		if !found {
			regClasses = make([]int,0,10)
		}
		foundMatch := false
		for _, r := range regClasses {
			if r == dcbKey.pDetail.regClassID {
				foundMatch = true
				break
			}
		}
		if !foundMatch {
			regClasses = append(regClasses,dcbKey.pDetail.regClassID)
			regClassesBySourceType[dcbKey.pDetail.RealSourceTypeID] = regClasses
		}
	}
		
	// Enumerate the results, emitting them as RatesOpModeDistribution entries.
	// This must be done for each pollutant/process and source type.
	// Operating mode 501 is a special case for zero speed seconds. When used for pollutant/process
	// 11609, it should stay operating mode 501. When used for any other pollutant/process,
	// it should be converted to opModeID 1 and summed into its fraction. Doing calculations this way
	// prevents any need to do second-by-second calculations for each pollutant/process.
	// Enumeration MUST be equivalent to this SQL ORDER BY clause:
	// 	order by sourceTypeID, polProcessID, roadTypeID, hourDayID, opModeID, avgSpeedBinID
	// Ascending or descending within a category does not matter. The implied nesting is
	// essential. For instance, opModeID will only change after all avgSpeedBinID's
	// have been emitted.
	didPrint = false
	queueCount := 0

	for _, sourceTypeID := range runSpecSourceType {
		for _, polProcessID := range runSpecPolProcessID {
			for _, roadTypeID := range runSpecRoadTypeWithOffNetwork {
				if roadTypeID == 1 && polProcessID % 100 != 1 {
					// Only Running Exhaust (processID=1) should use off network roads (roadTypeID=1) here
					continue
				}
				for _, hourDayID := range runSpecHourDay {
					if roadTypeID == 1 {
						regClasses, found := regClassesBySourceType[sourceTypeID]
						if found {
							for _, regClassID := range regClasses {
								b := new(romdBlock)
								b.avgSpeedFraction = 1.0 // 100% at the lowest speed bin
								b.avgBinSpeed = 1.0 //EM - by setting this to 1 mph, the perdistance rate is now the same as the perhour rate. Easy math!
								b.key.sourceTypeID = sourceTypeID
								b.key.regClassID = regClassID
								b.key.roadTypeID = roadTypeID
								b.key.hourDayID = hourDayID
								b.key.avgSpeedBinID = 0 //EM- in order for the workers to pick up emissions on roadTypeID 1, the avgSpeedBinID needs to be 0
								b.key.polProcessID = polProcessID
								b.key.beginModelYearID = 1960
								b.key.endModelYearID = 2060
								b.key.opModeID = 1 // Idle
								b.opModeFraction = 1.0 // 100% at idle
								// Enqueue b
								romdForBaseRateQueue <- b
								romdForBaseRateByAgeQueue <- b
								queueCount++
		
								if files != nil {
									b.writeLine(files)
								}
							}
						}
						continue
					}
					for _, opModeID := range opModesToIterate {
						if polProcessID != 11609 && opModeID == 501 {
							continue
						}
						for avgSpeedBinID, _ := range avgSpeedBin {
							avgSpeedKey.sourceTypeID = sourceTypeID // dcbKey.pDetail.RealSourceTypeID
							avgSpeedKey.roadTypeID = roadTypeID // dcbKey.roadTypeID
							avgSpeedKey.hourDayID = hourDayID
							avgSpeedKey.avgSpeedBinID = avgSpeedBinID // dcbKey.avgSpeedBinID
							avgSpeedDetail, found := avgSpeedDistribution[avgSpeedKey]
							if !found {
								continue
							}
							if avgSpeedDetail.avgSpeedFraction <= 0 {
								continue
							}

							dcbfk.sourceTypeID = sourceTypeID
							dcbfk.roadTypeID = roadTypeID
							dcbfk.avgSpeedBinID = avgSpeedBinID
							dcbKeys, found := driveCycleBracketedBinsFast[dcbfk]
							if found {
								for _, dcbKey := range dcbKeys {
									dcbDetail := driveCycleBracketedBins[*dcbKey]
									if len(dcbDetail.opModeFractions) == 0 {
										continue
									}
									opModeFraction := dcbDetail.opModeFractions[opModeID]
									if polProcessID != 11609 {
										if opModeID == 501 {
											continue
										} else if opModeID == 1 {
											opModeFraction += dcbDetail.opModeFractions[501]
										}
									}
									if opModeFraction <= 0 {
										continue
									}
									b := new(romdBlock)
									b.avgSpeedFraction = avgSpeedDetail.avgSpeedFraction
									b.avgBinSpeed = avgSpeedDetail.avgBinSpeed
									b.key.sourceTypeID = sourceTypeID
									b.key.regClassID = dcbKey.pDetail.regClassID
									b.key.roadTypeID = roadTypeID
									b.key.hourDayID = hourDayID
									b.key.avgSpeedBinID = avgSpeedBinID
									b.key.polProcessID = polProcessID
									b.key.beginModelYearID = dcbKey.pDetail.beginModelYearID
									b.key.endModelYearID = dcbKey.pDetail.endModelYearID
									b.key.opModeID = opModeID
									b.opModeFraction = opModeFraction
									// Enqueue b
									romdForBaseRateQueue <- b
									romdForBaseRateByAgeQueue <- b
									queueCount++
	
									if files != nil {
										b.writeLine(files)
									}
									}
							}
						}
					}
				}
			}
		}
	}
	// Close the queues so downstream threads know that no more data is coming.
	close(romdForBaseRateQueue)
	close(romdForBaseRateByAgeQueue)

	if files != nil {
		files.close()
	}
	if idleFractionFiles != nil {
		idleFractionFiles.close()
	}

	fmt.Println("Done populating RatesOpModeDistribution. Queue Count=",queueCount)
}

// Create ROMD table to hold debugging information
func createROMDTable(sqlToWrite chan string) {
	globalevents.SqlStarting()
	sqlToWrite <- "create table if not exists ratesOpModeDistributionDetail (" +
			"sourceTypeID int," +
			"polProcessID int," +
			"roadTypeID int," +
			"hourDayID int," +
			"opModeID int," +
			"avgSpeedBinID int," +
			"beginModelYearID int," +
			"endModelYearID int," +
			"regClassID int," +
			"opModeFraction double," +
			"avgBinSpeed double," +
			"avgSpeedFraction double" +
			")"
}

// Create a SQL-textual version of a romdBlock record.
func (this *romdBlock) writeLine(files *tempFiles) {
	// Build output line. Must match the order of columns in the temporary files.
	// sourceTypeID, polProcessID, roadTypeID, hourDayID, opModeID, avgSpeedBinID,
	// beginModelYearID, endModelYearID, regClassID,
	// opModeFraction, avgBinSpeed, avgSpeedFraction
	line := strconv.Itoa(this.key.sourceTypeID) +
			"\t" + strconv.Itoa(this.key.polProcessID) +
			"\t" + strconv.Itoa(this.key.roadTypeID) +
			"\t" + strconv.Itoa(this.key.hourDayID) +
			"\t" + strconv.Itoa(this.key.opModeID) +
			"\t" + strconv.Itoa(this.key.avgSpeedBinID) +
			"\t" + strconv.Itoa(this.key.beginModelYearID) +
			"\t" + strconv.Itoa(this.key.endModelYearID) +
			"\t" + strconv.Itoa(this.key.regClassID) +
			"\t" + strconv.FormatFloat(this.opModeFraction,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.avgBinSpeed,'e',-1,64) +
			"\t" + strconv.FormatFloat(this.avgSpeedFraction,'e',-1,64) +
			"\n"
	files.writeLine(line)
}
