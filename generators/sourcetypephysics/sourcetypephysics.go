/*
 Functions for the SourceTypePhysics module.
 @author Wesley Faler
 @version 2016-07-25
*/
package sourcetypephysics

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	"gen/configuration"
	"gen/globalevents"
	"gen/sqlutility"
)

// Information from the SourceUseTypePhysicsMapping table.
// RealSourceTypeID is the source type traditionally used.
// TempSourceTypeID is a temporary source type that works for a model year range and regclass combination.
// OpModeIDOffset is used to make new operationg modes good just for the temporary source type.
type SourceUseTypePhysicsMappingDetail struct {
	RealSourceTypeID, TempSourceTypeID, OpModeIDOffset int
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

// Update the RatesOpModeDistribution operating mode distribution table, replacing
// temporary source types with real source types and changing real operating modes
// to modelyear-specific modes.
func UpdateOperatingModeDistribution_RatesOpModeDistribution(sqlToWrite chan string) {
	globalevents.GeneratorStarting()
	textFilePath, _ := filepath.Abs("genRatesOpModeDistribution")
	setupTables(textFilePath)
	go coreUpdateOperatingModeDistribution_RatesOpModeDistribution(sqlToWrite,textFilePath)
}

// Update the RatesOpModeDistribution operating mode distribution table, replacing
// temporary source types with real source types and changing real operating modes
// to modelyear-specific modes.
func coreUpdateOperatingModeDistribution_RatesOpModeDistribution(sqlToWrite chan string, textFilePath string) {
	defer globalevents.GeneratorDone()

	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	fileIndex := 0
	fileName := textFilePath + strconv.Itoa(fileIndex)

	var outputFile *os.File
	var outputWriter *bufio.Writer
	var outputBuffer bytes.Buffer
	bufferMaxSize := 4*1024*1024
	bufferWriteLimit := bufferMaxSize - 2048
	if bufferMaxSize > outputBuffer.Len() {
		outputBuffer.Grow(bufferMaxSize - outputBuffer.Len())
	}
	f, err := os.Create(fileName)
	if err != nil {
		panic(err)
	}
	outputFile = f
	outputWriter = bufio.NewWriter(outputFile)

	fmt.Println("Querying RatesOpModeDistribution...")

	// Note: The ORDER BY is important here. Records with opModeID >= 1000 must be processed before
	// ----- those in the normal 0-100 range. This ORDER BY is the most efficient given the table's
	// primary key (it is the exact desc of all fields so is fast to process).
	// Without the ORDER BY, that natural order does not ensure the required sequence.
	querySql := "select sourceTypeID,roadTypeID,avgSpeedBinID,hourDayID,polProcessID,opModeID,opModeFraction,coalesce(opModeFractionCV,0) as opModeFractionCV,avgBinSpeed,avgSpeedFraction" +
			" from RatesOpModeDistribution" +
			" order by sourceTypeID desc, polProcessID desc, roadTypeID desc, hourDayID desc, opModeID desc, avgSpeedBinID desc"
	rows, err := db.Query(querySql)
	configuration.CheckErr(err)
    defer rows.Close()

    rowCount := 0
    outputRowCount := 0
	var sourceTypeID,roadTypeID,avgSpeedBinID,hourDayID,polProcessID,opModeID int
	var opModeFraction,opModeFractionCV,avgBinSpeed,avgSpeedFraction float64
	var shouldWrite, didHandle bool
    for rows.Next() {
    	if rowCount == 0 {
			fmt.Println("Got first row from RatesOpModeDistribution")
    	}
    	rowCount++
    	shouldWrite = false
    	didHandle = false

        err = rows.Scan(&sourceTypeID,&roadTypeID,&avgSpeedBinID,&hourDayID,&polProcessID,&opModeID,&opModeFraction,&opModeFractionCV,&avgBinSpeed,&avgSpeedFraction)
		configuration.CheckErr(err)

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
				(polProcessID < 0 || (polProcessID % 100) == 1) {
			// This record should be updated
			didHandle = true
			shouldWrite = true
			sourceTypeID = tempSourceTypeDetail.RealSourceTypeID
		}
		// Change source types for brakewear
		if !didHandle && polProcessID == 11609 {
			didHandle = true
			if tempSourceTypeDetail != nil {
				shouldWrite = true
				sourceTypeID = tempSourceTypeDetail.RealSourceTypeID
			}
		}
		// Promote old operating modes and change source types
		// This statement must fail (which is ok and can be ignored) if
		// entries exist with extended operating modes already. That is,
		// if entries already exist with extended operating modes, they
		// must be used and this record ignored.
		if !didHandle && tempSourceTypeDetail != nil &&
				opModeID >= 0 && opModeID < 100 &&
				(polProcessID < 0 || (polProcessID % 100) == 1) {
			// This record should be updated
			didHandle = true
			shouldWrite = true
			sourceTypeID = tempSourceTypeDetail.RealSourceTypeID
			opModeID += tempSourceTypeDetail.OpModeIDOffset
		}

		if !didHandle && tempSourceTypeDetail != nil && tempSourceTypeDetail.OpModeIDOffset > 0 &&
				opModeID >= 0 && opModeID < 100 &&
				(polProcessID < 0 || (polProcessID % 100) == 1) {
			// This record should be deleted
			didHandle = true
			shouldWrite = false
		}

		// // tempSourceTypeID never equals realSourceTypeID any more, so get rid of real source type operating modes
		if !didHandle && tempSourceTypeDetail == nil && realSourceTypeDetail != nil && realSourceTypeDetail.OpModeIDOffset > 0 &&
				opModeID >= 0 && opModeID < 100 &&
				(polProcessID < 0 || (polProcessID % 100) == 1) {
			// This record should be deleted
			didHandle = true
			shouldWrite = false
		}

		// Anything not already handled should continue to be used.
		if !didHandle {
			shouldWrite = true
		}

		if shouldWrite {
			if outputRowCount == 0 {
				fmt.Println("Writing first row to",fileName)
			}
			outputRowCount++
			line := strconv.Itoa(sourceTypeID) +
					"\t" + strconv.Itoa(roadTypeID) +
					"\t" + strconv.Itoa(avgSpeedBinID) +
					"\t" + strconv.Itoa(hourDayID) +
					"\t" + strconv.Itoa(polProcessID) +
					"\t" + strconv.Itoa(opModeID) +
					"\t" + strconv.FormatFloat(opModeFraction,'e',-1,64) +
					"\t" + strconv.FormatFloat(opModeFractionCV,'e',-1,64) +
					"\t" + strconv.FormatFloat(avgBinSpeed,'e',-1,64) +
					"\t" + strconv.FormatFloat(avgSpeedFraction,'e',-1,64) +
					"\n"
			outputBuffer.WriteString(line)
			if outputBuffer.Len() >= bufferWriteLimit {
				outputBuffer.WriteTo(outputWriter)
				outputBuffer.Reset()
			}
		}

		if outputRowCount >= 2000000 {
			if outputBuffer.Len() > 0 {
				outputBuffer.WriteTo(outputWriter)
				outputBuffer.Reset()
			}
			outputRowCount = 0
			outputWriter.Flush()
			outputFile.Close()

			globalevents.SqlStarting()
			sqlToWrite <- "load data infile '" + filepath.ToSlash(fileName) + "'" +
					" ignore into table genRatesOpModeDistribution (" +
					" sourceTypeID,roadTypeID,avgSpeedBinID,hourDayID,polProcessID,opModeID,opModeFraction,opModeFractionCV,avgBinSpeed,avgSpeedFraction" +
					")"

			fileIndex++
			fileName = textFilePath + strconv.Itoa(fileIndex)

			f, err := os.Create(fileName)
			if err != nil {
				panic(err)
			}
			outputFile = f
			outputWriter = bufio.NewWriter(outputFile)
		}
    }
    hasFinalRecords := false
	if outputBuffer.Len() > 0 {
		outputBuffer.WriteTo(outputWriter)
		outputBuffer.Reset()
		hasFinalRecords = true
	}
	outputWriter.Flush()
	outputFile.Close()

	if hasFinalRecords {
		globalevents.SqlStarting()
		sqlToWrite <- "load data infile '" + filepath.ToSlash(fileName) + "'" +
				" ignore into table genRatesOpModeDistribution (" +
				" sourceTypeID,roadTypeID,avgSpeedBinID,hourDayID,polProcessID,opModeID,opModeFraction,opModeFractionCV,avgBinSpeed,avgSpeedFraction" +
				")"
	}

	sqlutility.AddFinalSql("drop table if exists RatesOpModeDistributionBackup")
	sqlutility.AddFinalSql("rename table RatesOpModeDistribution to RatesOpModeDistributionBackup")

	sqlutility.AddFinalSql("drop table if exists RatesOpModeDistribution")
	sqlutility.AddFinalSql("rename table genRatesOpModeDistribution to RatesOpModeDistribution")

	fmt.Println("Done reading RatesOpModeDistribution. Row Count=",rowCount)
}

// Create the required temporary table(s) and set and final SQL to move data to the primary table(s).
// Read the SourceUseTypePhysicsMapping table into memory.
func setupTables(textFilePath string) {
	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	db.Exec("drop table if exists genRatesOpModeDistribution")
	db.Exec("create table genRatesOpModeDistribution like RatesOpModeDistribution")

	// Read the SourceUseTypePhysicsMapping table into memory.
	fmt.Println("Querying SourceUseTypePhysicsMapping...")
	SourceUseTypePhysicsMapping = make([]*SourceUseTypePhysicsMappingDetail,0,10000)
	SourceUseTypePhysicsMappingByTempSourceType = make(map[int]*SourceUseTypePhysicsMappingDetail)
	SourceUseTypePhysicsMappingByRealSourceType = make(map[int]*SourceUseTypePhysicsMappingDetail)
	rows, err := db.Query("select distinct realSourceTypeID, tempSourceTypeID, opModeIDOffset" +
			" from sourceUseTypePhysicsMapping" +
			" where realSourceTypeID <> tempSourceTypeID" +
			" order by realSourceTypeID, beginModelYearID")
	configuration.CheckErr(err)

    defer rows.Close()
    rowCount := 0
    for rows.Next() {
    	rowCount++
    	d := new(SourceUseTypePhysicsMappingDetail)
        err = rows.Scan(&d.RealSourceTypeID,&d.TempSourceTypeID,&d.OpModeIDOffset)
		configuration.CheckErr(err)
		SourceUseTypePhysicsMapping = append(SourceUseTypePhysicsMapping,d)
		SourceUseTypePhysicsMappingByTempSourceType[d.TempSourceTypeID] = d
		SourceUseTypePhysicsMappingByRealSourceType[d.RealSourceTypeID] = d // ok to overwrite something else
    }
	fmt.Println("Done reading SourceUseTypePhysicsMapping. Row Count=",rowCount)
}
