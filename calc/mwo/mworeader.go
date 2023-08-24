package mwo

import (
	"bufio"
	"io"
	"os"
	"calc/globalevents"
	"fmt"
	"strings"
	"calc/parse"
)

// Read the list of required modules from the extmodules text file.
func ReadModules() {
	parse.ReadAndParseFile("extmodules", func(parts []string) {
		// moduleName
		if len(parts) < 1 {
			return
		}
		moduleName := parse.GetString(parts[0])
		NeededModules[moduleName] = true
		fmt.Println("Needs module",moduleName)
	})
}

// Read supporting data files then begin asychronously reading the pollution data from the MOVES worker.
func StartReading(fileName string, mwoBlocks chan *MWOBlock) {
	// Read the fuel supply information and supporting data
	readSupportTables()
	readFuelTables()
	readFuelSupply()

	go func() {
		// Read the MOVESWorkerOutput information, splitting each entry
		// by the fuel supply market share.
		readMOVESWorkerOutput(fileName, mwoBlocks)
	}()
}

// Begin asychronously reading the activity data from the MOVES worker.
func StartReadingActivity(fileName string, mwoActivityBlocks chan *MWOActivityBlock) {
	go func() {
		// Read the MOVESWorkerActivityOutput information, splitting each entry
		// by the fuel supply market share.
		readMOVESWorkerActivityOutput(fileName, mwoActivityBlocks)
	}()
}

// Read supporting data text files.
func readSupportTables() {
	parse.ReadAndParseFile("extconstants", func(parts []string) {
		// stateID countyID zoneID linkID yearID monthID
		if len(parts) < 6 {
			return
		}
		Constants.StateID = parse.GetInt(parts[0])
		Constants.CountyID = parse.GetInt(parts[1])
		Constants.ZoneID = parse.GetInt(parts[2])
		Constants.LinkID = parse.GetInt(parts[3])
		Constants.YearID = parse.GetInt(parts[4])
		Constants.MonthID = parse.GetInt(parts[5])
	})

	parse.ReadAndParseFile("extagecategory", func(parts []string) {
		// ageID ageGroupID
		if len(parts) < 2 {
			return
		}
		ageID := parse.GetInt(parts[0])
		ageGroupID := parse.GetInt(parts[1])
		AgeGroups[ageID] = ageGroupID
	})

	parse.ReadAndParseFile("extpollutantprocess", func(parts []string) {
		// polProcessID
		if len(parts) < 1 {
			return
		}
		polProcessID := parse.GetInt(parts[0])
		NeededPolProcessIDs[polProcessID] = true
	})

	if _, err := os.Stat("extnrscc"); err == nil {
		parse.ReadAndParseFile("extnrscc", func(parts []string) {
			// SCC, NREquipTypeID, fuelTypeID
			if len(parts) < 3 {
				return
			}
			e := new(NRSCCDetail)
			e.SCC = parse.GetString(parts[0])
			e.NREquipTypeID = parse.GetInt(parts[1])
			e.FuelTypeID = parse.GetInt(parts[2])
			NRSCC[e.SCC] = e
		})
	}

	if _, err := os.Stat("extnrhpcategory"); err == nil {
		parse.ReadAndParseFile("extnrhpcategory", func(parts []string) {
			// nrhprangebinid, engtechid, nrhpcategory
			if len(parts) < 3 {
				return
			}
			e := NRHPCategoryKey{parse.GetInt(parts[0]),parse.GetInt(parts[1])}
			hpCategory := parse.GetString(parts[2])[0]
			NRHPCategory[e] = hpCategory
		})
	}
}

// Read fuel data text files.
func readFuelTables() {
	if _, err := os.Stat("extfueltype"); err == nil {
		parse.ReadAndParseFile("extfueltype", func(parts []string) {
			// fuelTypeID, fuelDensity, subjectToEvapCalculations
			if len(parts) < 3 {
				return
			}
			e := new(FuelType)
			e.FuelTypeID = parse.GetInt(parts[0])
			e.FuelDensity = parse.GetFloat(parts[1])
			e.SubjectToEvapCalculations = parse.GetBool(parts[2])
			FuelTypes[e.FuelTypeID] = e
		})
	}

	if _, err := os.Stat("extnrfueltype"); err == nil {
		parse.ReadAndParseFile("extnrfueltype", func(parts []string) {
			// fuelTypeID, fuelDensity, subjectToEvapCalculations
			if len(parts) < 3 {
				return
			}
			e := new(FuelType)
			e.FuelTypeID = parse.GetInt(parts[0])
			e.FuelDensity = parse.GetFloat(parts[1])
			e.SubjectToEvapCalculations = parse.GetBool(parts[2])
			NRFuelTypes[e.FuelTypeID] = e
		})
	}

	if _, err := os.Stat("extfuelsubtype"); err == nil {
		parse.ReadAndParseFile("extfuelsubtype", func(parts []string) {
			// fuelSubtypeID, fuelTypeID, fuelSubtypePetroleumFraction, fuelSubtypeFossilFraction,
			// carbonContent, oxidationFraction, energyContent
			if len(parts) < 7 {
				return
			}
			e := new(FuelSubType)
			e.FuelSubTypeID = parse.GetInt(parts[0])
			e.FuelTypeID = parse.GetInt(parts[1])
			e.FuelSubtypePetroleumFraction = parse.GetFloat(parts[2])
			e.FuelSubtypeFossilFraction = parse.GetFloat(parts[3])
			e.CarbonContent = parse.GetFloat(parts[4])
			e.OxidationFraction = parse.GetFloat(parts[5])
			e.EnergyContent = parse.GetFloat(parts[6])

			e.FuelType = FuelTypes[e.FuelTypeID]
			FuelSubTypes[e.FuelSubTypeID] = e
		})
	}

	if _, err := os.Stat("extnrfuelsubtype"); err == nil {
		parse.ReadAndParseFile("extnrfuelsubtype", func(parts []string) {
			// fuelSubtypeID, fuelTypeID, fuelSubtypePetroleumFraction, fuelSubtypeFossilFraction,
			// carbonContent, oxidationFraction, energyContent
			if len(parts) < 7 {
				return
			}
			e := new(FuelSubType)
			e.FuelSubTypeID = parse.GetInt(parts[0])
			e.FuelTypeID = parse.GetInt(parts[1])
			e.FuelSubtypePetroleumFraction = parse.GetFloat(parts[2])
			e.FuelSubtypeFossilFraction = parse.GetFloat(parts[3])
			e.CarbonContent = parse.GetFloat(parts[4])
			e.OxidationFraction = parse.GetFloat(parts[5])
			e.EnergyContent = parse.GetFloat(parts[6])

			e.FuelType = NRFuelTypes[e.FuelTypeID]
			if e.FuelType == nil {
				e.FuelType = FuelTypes[e.FuelTypeID]
			}
			NRFuelSubTypes[e.FuelSubTypeID] = e
		})
	}

	if _, err := os.Stat("extfuelformulation"); err == nil {
		parse.ReadAndParseFile("extfuelformulation", func(parts []string) {
			// 0,1: fuelFormulationID, fuelSubtypeID,
			// 2,3: RVP, sulfurLevel,
			// 4-7: ETOHVolume, MTBEVolume, ETBEVolume, TAMEVolume,
			// 8-10: aromaticContent, olefinContent, benzeneContent,
			// 11,12: e200, e300,
			// 13-16: volToWtPercentOxy, BioDieselEsterVolume, CetaneIndex, PAHContent,
			// 17,18: T50, T90
			if len(parts) < 19 {
				return
			}
			e := new(FuelFormulation)

			e.FuelFormulationID = parse.GetInt(parts[0])
			e.FuelSubTypeID = parse.GetInt(parts[1])
			e.RVP = parse.GetFloat(parts[2])
			e.SulfurLevel = parse.GetFloat(parts[3])
			e.ETOHVolume = parse.GetFloat(parts[4])
			e.MTBEVolume = parse.GetFloat(parts[5])
			e.ETBEVolume = parse.GetFloat(parts[6])
			e.TAMEVolume = parse.GetFloat(parts[7])
			e.AromaticContent = parse.GetFloat(parts[8])
			e.OlefinContent = parse.GetFloat(parts[9])
			e.BenzeneContent = parse.GetFloat(parts[10])
			e.E200 = parse.GetFloat(parts[11])
			e.E300 = parse.GetFloat(parts[12])
			e.VolToWtPercentOxy = parse.GetFloat(parts[13])
			e.BioDieselEsterVolume = parse.GetFloat(parts[14])
			e.CetaneIndex = parse.GetFloat(parts[15])
			e.PAHContent = parse.GetFloat(parts[16])
			e.T50 = parse.GetFloat(parts[17])
			e.T90 = parse.GetFloat(parts[18])

			e.FuelSubType = FuelSubTypes[e.FuelSubTypeID]
			if e.FuelSubType != nil {
				e.FuelTypeID = e.FuelSubType.FuelTypeID
			}
			FuelFormulations[e.FuelFormulationID] = e
			//fmt.Println("FuelFormulation:",e)
		})
	}

	if _, err := os.Stat("extnrfuelformulation"); err == nil {
		parse.ReadAndParseFile("extnrfuelformulation", func(parts []string) {
			// 0,1: fuelFormulationID, fuelSubtypeID,
			// 2,3: RVP, sulfurLevel,
			// 4-7: ETOHVolume, MTBEVolume, ETBEVolume, TAMEVolume,
			// 8-10: aromaticContent, olefinContent, benzeneContent,
			// 11,12: e200, e300,
			// 13-16: volToWtPercentOxy, BioDieselEsterVolume, CetaneIndex, PAHContent,
			// 17,18: T50, T90
			if len(parts) < 19 {
				return
			}
			fuelFormulationID := parse.GetInt(parts[0])
			if FuelFormulations[fuelFormulationID] != nil {
				// The fuel formulation has already been loaded, so skip it.
				return
			}

			e := new(FuelFormulation)

			e.FuelFormulationID = fuelFormulationID
			e.FuelSubTypeID = parse.GetInt(parts[1])
			e.RVP = parse.GetFloat(parts[2])
			e.SulfurLevel = parse.GetFloat(parts[3])
			e.ETOHVolume = parse.GetFloat(parts[4])
			e.MTBEVolume = parse.GetFloat(parts[5])
			e.ETBEVolume = parse.GetFloat(parts[6])
			e.TAMEVolume = parse.GetFloat(parts[7])
			e.AromaticContent = parse.GetFloat(parts[8])
			e.OlefinContent = parse.GetFloat(parts[9])
			e.BenzeneContent = parse.GetFloat(parts[10])
			e.E200 = parse.GetFloat(parts[11])
			e.E300 = parse.GetFloat(parts[12])
			e.VolToWtPercentOxy = parse.GetFloat(parts[13])
			e.BioDieselEsterVolume = parse.GetFloat(parts[14])
			e.CetaneIndex = parse.GetFloat(parts[15])
			e.PAHContent = parse.GetFloat(parts[16])
			e.T50 = parse.GetFloat(parts[17])
			e.T90 = parse.GetFloat(parts[18])

			e.FuelSubType = NRFuelSubTypes[e.FuelSubTypeID]
			if e.FuelSubType == nil {
				e.FuelSubType = FuelSubTypes[e.FuelSubTypeID]
			}
			if e.FuelSubType != nil {
				e.FuelTypeID = e.FuelSubType.FuelTypeID
			}
			FuelFormulations[e.FuelFormulationID] = e
			//fmt.Println("FuelFormulation:",e)
		})
	}

	fmt.Printf("Loaded %d fuel types, %d fuel subtypes, %d fuelformulations\n",len(FuelTypes), len(FuelSubTypes), len(FuelFormulations))
}

// Read fuel supply text files.
func readFuelSupply() {
	/* Format:
	countyID yearID monthID fuelTypeID fuelSubTypeID fuelFormulationID marketShare

	cache select ##context.iterLocation.countyRecordID##, ##context.year##, ##context.monthID##,
		fst.fuelTypeID, fst.fuelSubTypeID, ff.fuelFormulationID, fs.marketShare
	into outfile '##ExtFuelSupply##'
	from year
	inner join fuelSupply fs on (fs.fuelYearID=year.fuelYearID)
	inner join monthOfAnyYear moay on (moay.monthGroupID=fs.monthGroupID)
	inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)
	inner join fuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)
	where yearID = ##context.year##
	and fs.fuelRegionID = ##context.fuelRegionID##
	and moay.monthID = ##context.monthID##
	and fst.fuelTypeID in (##macro.csv.all.fuelTypeID##);

	cache select ##context.iterLocation.countyRecordID##, ##context.year##, ##context.monthID##,
		fst.fuelTypeID, fst.fuelSubTypeID, ff.fuelFormulationID, fs.marketShare
	into outfile '##ExtNRFuelSupply##'
	from nrFuelSupply fs
	inner join monthOfAnyYear m using (monthGroupID)
	inner join nrFuelYearRange fy using (fuelYearRangeID)
	inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)
	inner join fuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)
	where fs.countyID = ##context.iterLocation.countyRecordID##
	and m.monthID = ##context.monthID##
	and fy.nrFuelStartYear <= ##context.year##
	and fy.nrFuelEndYear >= ##context.year##
	and fst.fuelTypeID in (##macro.csv.all.fuelTypeID##);
	*/
	maxEntriesPerFuel := 0

	if _, err := os.Stat("extfuelsupply"); err == nil {
		parse.ReadAndParseFile("extfuelsupply", func(parts []string) {
			if len(parts) < 7 {
				return
			}
			// [countyID yearID monthID fuelTypeID] = [fuelSubTypeID fuelFormulationID marketShare]
			key := FuelSupplyKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3])}
			value := new(FuelSupplyDetail)
			value.FuelSubTypeID = parse.GetInt(parts[4])
			value.FuelFormulationID = parse.GetInt(parts[5])
			value.MarketShare = parse.GetFloat(parts[6])
			value.FuelFormulation = FuelFormulations[value.FuelFormulationID]
			//fmt.Println("FuelSupplyDetail:",value)
			if value.FuelFormulation == nil {
				return
			}

			values := FuelSupply[key]
			if values == nil {
				values = make([]*FuelSupplyDetail,0,100)
			}
			values = append(values,value)
			FuelSupply[key] = values
			if len(values) > maxEntriesPerFuel {
				maxEntriesPerFuel = len(values)
			}
		})
	}

	if _, err := os.Stat("extnrfuelsupply"); err == nil {
		parse.ReadAndParseFile("extnrfuelsupply", func(parts []string) {
			if len(parts) < 7 {
				return
			}
			// [countyID yearID monthID fuelTypeID] = [fuelSubTypeID fuelFormulationID marketShare]
			key := FuelSupplyKey{parse.GetInt(parts[0]),parse.GetInt(parts[1]),parse.GetInt(parts[2]),parse.GetInt(parts[3])}
			value := new(FuelSupplyDetail)
			value.FuelSubTypeID = parse.GetInt(parts[4])
			value.FuelFormulationID = parse.GetInt(parts[5])
			value.MarketShare = parse.GetFloat(parts[6])
			value.FuelFormulation = FuelFormulations[value.FuelFormulationID]
			//fmt.Println("FuelSupplyDetail:",value)
			if value.FuelFormulation == nil {
				return
			}

			values := NRFuelSupply[key]
			if values == nil {
				values = make([]*FuelSupplyDetail,0,100)
			}
			values = append(values,value)
			NRFuelSupply[key] = values
			if len(values) > maxEntriesPerFuel {
				maxEntriesPerFuel = len(values)
			}
		})
	}

	if maxEntriesPerFuel > 0 {
		defaultEmissionsPerFuelBlock = maxEntriesPerFuel
	}

	fmt.Printf("Loaded %d fuel supply entries, %d Nonroad fuel supply entries, maxEntriesPerFuel = %d\n", len(FuelSupply), len(NRFuelSupply), maxEntriesPerFuel)
}

// Read the pollution output file, creating and queuing MWOBlock objects from the records.
func readMOVESWorkerOutput(fileName string, mwoBlocks chan *MWOBlock) {
	defer globalevents.MWOReadDone()

	totalLinesRead := 0
	totalLineSetsRead := 0

	const linesPerSet = 25 // was 100
	const lineSetCount = 100

	lines := make(chan []string,lineSetCount)
	linesReturn := make(chan []string,lineSetCount)
	parseTokens := make(chan int,lineSetCount)
	parseTokensSent := 0

	for i:=0; i<lineSetCount; i++ {
		linesReturn <- make([]string,0,linesPerSet)
	}

	inputFile, inputError := os.Open(fileName)
	if inputError != nil {
		fmt.Printf("An error occurred on opening the file %s\n", fileName)
		return // exit the function on error
	}
	defer inputFile.Close()
	for i:= 0; i<5; i++ {
		go parseLines(lines,linesReturn,mwoBlocks,parseTokens)
	}
	inputReader := bufio.NewReader(inputFile)
	currentLineSet := <- linesReturn
	for {
		inputString, readerError := inputReader.ReadString('\n')
		if readerError == io.EOF {
			break
		}
		inputString = strings.TrimSuffix(inputString,"\n")
		inputString = strings.TrimSuffix(inputString,"\r")
		if len(inputString) > 0 {
			totalLinesRead++

			if currentLineSet == nil {
				currentLineSet = <- linesReturn
				// Clear all items from the array
				currentLineSet = currentLineSet[0:0]
			}
			currentLineSet = append(currentLineSet,inputString)
			if len(currentLineSet) >= linesPerSet {
				parseTokensSent++
				lines <- currentLineSet
				currentLineSet = nil
				totalLineSetsRead++
			}
			// Drain the parse responses but don't wait for parsing
			for parseTokensSent > 0 {
				select {
					case <- parseTokens :
						parseTokensSent--
					default:
						break
				}
			}
		}
	}
	if currentLineSet != nil && len(currentLineSet) > 0 {
		parseTokensSent++
		lines <- currentLineSet
		currentLineSet = nil
		totalLineSetsRead++
	}
	close(lines)
	// Wait for all parsing to complete
	for parseTokensSent > 0 {
		<- parseTokens
		parseTokensSent--
	}
	close(linesReturn)
	close(parseTokens)

	fmt.Printf("MOVESWorkerOutput had %d lines, parsed as %d sets.\n",totalLinesRead,totalLineSetsRead)
}

// Split the pollution text lines into objects.
func parseLines(lines chan []string, linesReturn chan []string, mwoBlocks chan *MWOBlock, parseTokens chan int) {
	for {
		ls, ok := <- lines // get a line set
		if !ok {
			break
		}
		b := New() // get a blank mwoBlock
		for _, line := range ls {
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
			*/
			parts := strings.Split(line,"\t")
			if parts == nil || len(parts) < 23 {
				continue
			}
			// fmt.Println(line)
			fb := b.Add()
			fb.Key.YearID = parse.GetInt(parts[2])
			fb.Key.MonthID = parse.GetInt(parts[3])
			fb.Key.DayID = parse.GetInt(parts[4])
			fb.Key.HourID = parse.GetInt(parts[5])
			fb.Key.StateID = parse.GetInt(parts[6])
			fb.Key.CountyID = parse.GetInt(parts[7])
			fb.Key.ZoneID = parse.GetInt(parts[8])
			fb.Key.LinkID = parse.GetInt(parts[9])
			fb.Key.RoadTypeID = parse.GetInt(parts[16])
			fb.Key.SourceTypeID = parse.GetInt(parts[12])
			fb.Key.RegClassID = parse.GetInt(parts[13])
			fb.Key.FuelTypeID = parse.GetInt(parts[14])
			fb.Key.ModelYearID = parse.GetInt(parts[15])
			fb.Key.SCC = parse.GetString(parts[17])
			fb.Key.EngTechID = parse.GetInt(parts[18])
			fb.Key.SectorID = parse.GetInt(parts[19])
			fb.Key.HPID = parse.GetInt(parts[20])
			fb.Key.PollutantID = parse.GetInt(parts[10])
			fb.Key.ProcessID = parse.GetInt(parts[11])
			fb.Key.CalcIDs()
			fb.SetAsInput()

			emissionQuant := parse.GetFloat(parts[21])
			emissionRate := parse.GetFloat(parts[22])

			if fb.Key.SectorID > 0 { // if Nonroad, then split using Nonroad fuel supply...
				fsDetail := NRFuelSupply[FuelSupplyKey{fb.Key.CountyID, fb.Key.YearID, fb.Key.MonthID, fb.Key.FuelTypeID}]
				if fsDetail == nil {
					continue
				}
				for _, fsd := range fsDetail {
					fb.Add(fsd.FuelSubTypeID,fsd.FuelFormulationID,emissionQuant*fsd.MarketShare,emissionRate*fsd.MarketShare)
				}
			} else { // if onroad, split using onroad fuel supply...
				fsDetail := FuelSupply[FuelSupplyKey{fb.Key.CountyID, fb.Key.YearID, fb.Key.MonthID, fb.Key.FuelTypeID}]
				if fsDetail == nil {
					continue
				}
				for _, fsd := range fsDetail {
					fb.Add(fsd.FuelSubTypeID,fsd.FuelFormulationID,emissionQuant*fsd.MarketShare,emissionRate*fsd.MarketShare)
				}
			}
		}
		globalevents.MWOBlockCreated()
		mwoBlocks <- b // queue the newly parsed block
		linesReturn <- ls // recycle the line set
		parseTokens <- 1 // signal parsing is complete for one line set
	}
}

// Read the activity output file, creating and queuing MWOActivityBlock objects from the records.
func readMOVESWorkerActivityOutput(fileName string, mwoActivityBlocks chan *MWOActivityBlock) {
	defer globalevents.MWOActivityReadDone()

	totalLinesRead := 0
	totalLineSetsRead := 0

	const linesPerSet = 25 // was 100
	const lineSetCount = 100

	lines := make(chan []string,lineSetCount)
	linesReturn := make(chan []string,lineSetCount)
	parseTokens := make(chan int,lineSetCount)
	parseTokensSent := 0

	for i:=0; i<lineSetCount; i++ {
		linesReturn <- make([]string,0,linesPerSet)
	}

	inputFile, inputError := os.Open(fileName)
	if inputError != nil {
		// Activity input files don't always exist, so this condition occurs normally.
		//fmt.Printf("An error occurred on opening the file %s\n", fileName)
		return // exit the function on error
	}
	defer inputFile.Close()
	for i:= 0; i<5; i++ {
		go parseActivityLines(lines,linesReturn,mwoActivityBlocks,parseTokens)
	}
	inputReader := bufio.NewReader(inputFile)
	currentLineSet := <- linesReturn
	for {
		inputString, readerError := inputReader.ReadString('\n')
		if readerError == io.EOF {
			break
		}
		inputString = strings.TrimSuffix(inputString,"\n")
		inputString = strings.TrimSuffix(inputString,"\r")
		if len(inputString) > 0 {
			totalLinesRead++

			if currentLineSet == nil {
				currentLineSet = <- linesReturn
				// Clear all items from the array
				currentLineSet = currentLineSet[0:0]
			}
			currentLineSet = append(currentLineSet,inputString)
			if len(currentLineSet) >= linesPerSet {
				parseTokensSent++
				lines <- currentLineSet
				currentLineSet = nil
				totalLineSetsRead++
			}
			// Drain the parse responses but don't wait for parsing
			for parseTokensSent > 0 {
				select {
					case <- parseTokens :
						parseTokensSent--
					default:
						break
				}
			}
		}
	}
	if currentLineSet != nil && len(currentLineSet) > 0 {
		parseTokensSent++
		lines <- currentLineSet
		currentLineSet = nil
		totalLineSetsRead++
	}
	close(lines)
	// Wait for all parsing to complete
	for parseTokensSent > 0 {
		<- parseTokens
		parseTokensSent--
	}
	close(linesReturn)
	close(parseTokens)

	fmt.Printf("MOVESWorkerActivityOutput had %d lines, parsed as %d sets.\n",totalLinesRead,totalLineSetsRead)
}

// Split the activity text lines into objects.
func parseActivityLines(lines chan []string, linesReturn chan []string, mwoActivityBlocks chan *MWOActivityBlock, parseTokens chan int) {
	for {
		ls, ok := <- lines // get a line set
		if !ok {
			break
		}
		b := NewActivityBlock() // get a blank mwoActivityBlock
		for _, line := range ls {
			/*
			0,1: MOVESRunID,iterationID,
			2,3,4,5: yearID,monthID,dayID,hourID,
			6,7,8,9: stateID,countyID,zoneID,linkID,
			10,11: sourceTypeID,regClassID,
			12,13: fuelTypeID,modelYearID,
			14,15: roadTypeID,SCC,
			16,17,18: engTechID,sectorID,hpID,
			19,20: activityTypeID,activity
			*/
			parts := strings.Split(line,"\t")
			if parts == nil || len(parts) < 21 {
				continue
			}
			// fmt.Println(line)
			fb := b.Add()
			fb.Key.YearID = parse.GetInt(parts[2])
			fb.Key.MonthID = parse.GetInt(parts[3])
			fb.Key.DayID = parse.GetInt(parts[4])
			fb.Key.HourID = parse.GetInt(parts[5])
			fb.Key.StateID = parse.GetInt(parts[6])
			fb.Key.CountyID = parse.GetInt(parts[7])
			fb.Key.ZoneID = parse.GetInt(parts[8])
			fb.Key.LinkID = parse.GetInt(parts[9])
			fb.Key.RoadTypeID = parse.GetInt(parts[14])
			fb.Key.SourceTypeID = parse.GetInt(parts[10])
			fb.Key.RegClassID = parse.GetInt(parts[11])
			fb.Key.FuelTypeID = parse.GetInt(parts[12])
			fb.Key.ModelYearID = parse.GetInt(parts[13])
			fb.Key.SCC = parse.GetString(parts[15])
			fb.Key.EngTechID = parse.GetInt(parts[16])
			fb.Key.SectorID = parse.GetInt(parts[17])
			fb.Key.HPID = parse.GetInt(parts[18])
			fb.Key.ActivityTypeID = parse.GetInt(parts[19])
			fb.Key.CalcIDs()
			fb.SetAsInput()

			activity := parse.GetFloat(parts[20])

			if fb.Key.SectorID > 0 { // if Nonroad, then split using Nonroad fuel supply...
				fsDetail := NRFuelSupply[FuelSupplyKey{fb.Key.CountyID, fb.Key.YearID, fb.Key.MonthID, fb.Key.FuelTypeID}]
				if fsDetail == nil {
					continue
				}
				for _, fsd := range fsDetail {
					fb.Add(fsd.FuelSubTypeID,fsd.FuelFormulationID,activity*fsd.MarketShare)
				}
			} else { // if onroad, split using onroad fuel supply...
				fsDetail := FuelSupply[FuelSupplyKey{fb.Key.CountyID, fb.Key.YearID, fb.Key.MonthID, fb.Key.FuelTypeID}]
				if fsDetail == nil {
					continue
				}
				for _, fsd := range fsDetail {
					fb.Add(fsd.FuelSubTypeID,fsd.FuelFormulationID,activity*fsd.MarketShare)
				}
			}
		}
		globalevents.MWOActivityBlockCreated()
		mwoActivityBlocks <- b // queue the newly parsed block
		linesReturn <- ls // recycle the line set
		parseTokens <- 1 // signal parsing is complete for one line set
	}
}
