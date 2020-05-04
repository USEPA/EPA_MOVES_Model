/**************************************************************************************************
 * @(#)CompareRuns.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.tools;

import java.io.*;
import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.common.*;

/**
 * Compare Classic and Rates First runs.
 *
 * @author		Wesley Faler
 * @version		2013-11-18
**/
public class CompareRuns {
	String databaseName = "test20131022";
	int rfInventory = -1, 				classicInventory = 133;
	int rfInventory2030 = -1, 			classicInventory2030 = 140;
	int rfRatesFullDetail = -1, 		classicRatesFullDetail = 134;
	int rfRatesNoFuel = -1, 			classicRatesNoFuel = 135;
	int rfRatesNoFuelNoModelYear = -1, 	classicRatesNoFuelNoModelYear = 136;
	int rfRatesNoModelYear = -1, 		classicRatesNoModelYear = 137;
	int rfRates2012FullDetail = -1, 	classicRates2012FullDetail = 138;
	int rfRates2030FullDetail = -1, 	classicRates2030FullDetail = 139;

	public static void main(String args[]) {
		CompareRuns r = new CompareRuns();
		r.gatherData();
		r.writeOutput();
	}

	DatabaseSelection outputDatabase;
	Connection db;
	TreeMap<String,String> detailHTML = new TreeMap<String,String>();
	TreeMap<String,String> summaryText = new TreeMap<String,String>();

	CompareRuns() {
		outputDatabase = new DatabaseSelection();
		outputDatabase.databaseName = databaseName;
		db = outputDatabase.openConnectionOrNull();
		setupIndexes();
	}

	void setupIndexes() {
		// Add indexes. They may already exist, so keep going if an error occurs.
		String[] statements = {
			"alter table ratePerDistance add key k1 (movesRunID,yearID, monthID, dayID, hourID, linkID, pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, avgSpeedBinID)",
			"alter table ratePerDistance add key k2 (movesRunID,yearID, monthID, dayID, hourID, linkID, pollutantID, processID, sourceTypeID, modelYearID, roadTypeID, avgSpeedBinID)",
			"alter table ratePerDistance add key k3 (movesRunID,yearID, monthID, dayID, hourID, pollutantID, processID, sourceTypeID, modelYearID, roadTypeID, avgSpeedBinID)",
			"alter table ratePerDistance add key k4 (movesRunID,yearID, monthID, dayID, hourID, linkID, pollutantID, processID, sourceTypeID, roadTypeID, avgSpeedBinID)",
			"alter table ratePerVehicle add key k1 (movesRunID,yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,modelYearID,fuelTypeID)",
			"alter table ratePerVehicle add key k2 (movesRunID,yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,fuelTypeID)",
			"alter table ratePerStart add key k1 (movesRunID,yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,modelYearID,fuelTypeID)",
			"alter table ratePerStart add key k2 (movesRunID,yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,fuelTypeID)",
			"alter table movesOutput add key k1 (movesRunID,yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,modelYearID,fuelTypeID)",
			"alter table movesOutput add key k2 (movesRunID,yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,fuelTypeID)",
			"alter table movesActivityOutput add key k1 (movesRunID,activityTypeID,yearID,monthID,dayID,hourID,sourceTypeID,modelYearID,fuelTypeID)",
			"alter table movesActivityOutput add key k2 (movesRunID,activityTypeID,yearID,monthID,dayID,hourID,sourceTypeID,fuelTypeID)"
		};
		for(int i=0;i<statements.length;i++) {
			try {
				SQLRunner.executeSQL(db,statements[i]);
			} catch(Exception e) {
				// Nothing to do here
			}
		}
	}

	void gatherData() {
		rfInventory = findRun("Inventory_FullDetail",rfInventory);
		rfInventory2030 = findRun("Inventory2030_FullDetail",rfInventory2030);
		rfRatesFullDetail = findRun("Rates_FullDetail",rfRatesFullDetail);
		rfRatesNoFuel = findRun("Rates_NoFuel",rfRatesNoFuel);
		rfRatesNoModelYear = findRun("Rates_NoModelYear",rfRatesNoModelYear);
		rfRatesNoFuelNoModelYear = findRun("Rates_NoFuelNoModelYear",rfRatesNoFuelNoModelYear);
		rfRates2012FullDetail = findRun("Rates2012_FullDetail",rfRates2012FullDetail);
		rfRates2030FullDetail = findRun("Rates2030_FullDetail",rfRates2030FullDetail);

		compareInventory("Inventory_FullDetail",classicInventory,rfInventory);
		compareActivity("Inventory_FullDetail",classicInventory,rfInventory,true,true,true);

		compareInventory("Inventory2030_FullDetail",classicInventory2030,rfInventory2030);
		compareActivity("Inventory2030_FullDetail",classicInventory2030,rfInventory2030,true,true,true);

		compareRates("Rates_FullDetail",classicRatesFullDetail,rfRatesFullDetail,true,true);
		compareRates("Rates2012_FullDetail",classicRates2012FullDetail,rfRates2012FullDetail,true,true);
		compareRates("Rates2030_FullDetail",classicRates2030FullDetail,rfRates2030FullDetail,true,true);
		compareRates("Rates_NoFuel",classicRatesNoFuel,rfRatesNoFuel,true,false);
		compareRates("Rates_NoModelYear",classicRatesNoModelYear,rfRatesNoModelYear,false,true);
		compareRates("Rates_NoFuelNoModelYear",classicRatesNoFuelNoModelYear,rfRatesNoFuelNoModelYear,false,false);
	}

	int findRun(String caseName, int newRunID) {
		if(newRunID > 0) {
			return newRunID;
		}
		String sql = "select max(movesRunID)"
				+ " from movesRun"
				+ " where runSpecFileName like '%" + caseName + ".mrs'";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			if(query.rs.next()) {
				return query.rs.getInt(1);
			}
		} catch(Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		} finally {
			query.onFinally();
		}
		return -1;
	}

	String getKey(String caseName, String dataSet) {
		return (caseName + "_" + dataSet).toLowerCase();
	}

	void dumpResults(SQLRunner.Query query, String caseName, String dataSet) {
		// Put the results into an HTML table.
		// Capture the greatest value of the maxPctDiff column.
		// Store the HTML and max(maxPctDiff) into a data structure keyed by caseName/dataSet.

		String key = getKey(caseName,dataSet);
		boolean hasData = false;
		double maxPercent = 1000;
		String html = "";
		try {
			ResultSetMetaData m = query.rs.getMetaData();
			int columnCount = m.getColumnCount();
			while(query.rs.next()) {
				double d = query.rs.getDouble("maxPctDiff");
				if(hasData) {
					maxPercent = Math.max(d,maxPercent);
				} else {
					maxPercent = d;
				}
				if(!hasData) {
					hasData = true;
					html = "<table border=\"1\"><tr>\n";
					// get all column names as <th> tags
					for(int i=1;i<=columnCount;i++) {
						html += "<th>" + m.getColumnLabel(i) + "</th>";
					}
				}
	
				html += "<tr>";
				for(int i=1;i<=columnCount;i++) {
					String s = query.rs.getString(i);
					html += "<td>" + s + "</td>";
				}
				html += "</tr>\n";
			}
			if(hasData) {
				html += "</tr></table>";
			}
			summaryText.put(key,"" + maxPercent + "%");
			detailHTML.put(key,html);
		} catch(Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}

	void compareInventory(String caseName, int classicRunID, int newRunID) {
		System.out.println("Comparing " + caseName + " MOVESOutput...");
		String[] statements = {
			"select rf.processID, rf.pollutantID, rf.fuelTypeID, "
			+ " 	max(case when c.emissionQuant > 0 then abs((rf.emissionQuant-c.emissionQuant)/c.emissionQuant*100)"
			+ " 		when rf.emissionQuant > 0 then 100.0 else 0.0 end) as maxPctDiff, "
			+ " 	min(case when c.emissionQuant > 0 then abs((rf.emissionQuant-c.emissionQuant)/c.emissionQuant*100)"
			+ " 		when rf.emissionQuant > 0 then 100.0 else 0.0 end) as minPctDiff, "
			+ " 	count(*) as c"
			+ " from movesOutput rf"
			+ " inner join movesOutput c using (yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,modelYearID,fuelTypeID,roadTypeID)"
			+ " where rf.movesRunID=" + newRunID + " and c.movesRunID=" + classicRunID
			+ " group by rf.processID, rf.pollutantID, rf.fuelTypeID"
			+ " order by rf.processID, rf.pollutantID, rf.fuelTypeID"
		};
		for(int i=0;i<statements.length;i++) {
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				query.open(db,statements[i]);
				dumpResults(query,caseName,"MOVESOutput");
			} catch(Exception e) {
				// Nothing to do here
			} finally {
				query.onFinally();
			}
		}
	}

	void compareRates(String caseName, int classicRunID, int newRunID,
			boolean useModelYear, boolean useFuelType) {
		String[] statements = {
			"ratePerDistance",
			"select rf.processID, rf.pollutantID," + (useFuelType? "rf.fuelTypeID,":"")
			+ " 	max(case when c.ratePerDistance > 0 then abs((rf.ratePerDistance-c.ratePerDistance)/c.ratePerDistance*100)"
			+ " 		when rf.ratePerDistance > 0 then 100.0"
			+ " 		else 0.0 end"
			+ " 		) as maxPctDiff, "
			+ " 	min(case when c.ratePerDistance > 0 then abs((rf.ratePerDistance-c.ratePerDistance)/c.ratePerDistance*100)"
			+ " 		when rf.ratePerDistance > 0 then 100.0"
			+ " 		else 0.0 end"
			+ " 		) as minPctDiff, "
			+ " 	count(*) as c"
			+ " from ratePerDistance rf"
			+ " inner join ratePerDistance c using (yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID,linkID,avgSpeedBinID" + (useModelYear?",modelYearID":"") + (useFuelType?",fuelTypeID":"") + ")"
			+ " where rf.movesRunID=" + newRunID + " and c.movesRunID=" + classicRunID
			+ " group by rf.processID, rf.pollutantID" + (useFuelType? ",rf.fuelTypeID":"")
			+ " order by rf.processID, rf.pollutantID" + (useFuelType? ",rf.fuelTypeID":""),

			"ratePerVehicle",
			"select rf.processID, rf.pollutantID, " + (useFuelType? "rf.fuelTypeID,":"")
			+ " 	max(case when c.ratePerVehicle > 0 then abs((rf.ratePerVehicle-c.ratePerVehicle)/c.ratePerVehicle*100)"
			+ " 		when rf.ratePerVehicle > 0 then 100.0"
			+ " 		else 0.0 end"
			+ " 		) as maxPctDiff, "
			+ " 	min(case when c.ratePerVehicle > 0 then abs((rf.ratePerVehicle-c.ratePerVehicle)/c.ratePerVehicle*100)"
			+ " 		when rf.ratePerVehicle > 0 then 100.0"
			+ " 		else 0.0 end"
			+ " 		) as minPctDiff, "
			+ " 	count(*) as c"
			+ " from ratePerVehicle rf"
			+ " inner join ratePerVehicle c using (yearID,monthID,dayID,hourID,pollutantID,processID,sourceTypeID" + (useModelYear?",modelYearID":"") + (useFuelType?",fuelTypeID":"") + ")"
			+ " where rf.movesRunID=" + newRunID + " and c.movesRunID=" + classicRunID
			+ " group by rf.processID, rf.pollutantID" + (useFuelType? ",rf.fuelTypeID":"")
			+ " order by rf.processID, rf.pollutantID" + (useFuelType? ",rf.fuelTypeID":"")
		};
		for(int i=0;i<statements.length;i+=2) {
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				System.out.println("Comparing " + caseName + " " + statements[i+0] + "...");
				query.open(db,statements[i+1]);
				dumpResults(query,caseName,statements[i+0]);
			} catch(Exception e) {
				// Nothing to do here
			} finally {
				query.onFinally();
			}
		}
		compareActivity(caseName,classicRunID,newRunID,useModelYear,useFuelType,false);
	}

	void compareActivity(String caseName, int classicRunID, int newRunID,
			boolean useModelYear, boolean useFuelType,
			boolean useDistance) {
		String coreSQL = "select rf.sourceTypeID," + (useFuelType? "rf.fuelTypeID,":"")
			+ " 	max(case when c.activity > 0 then abs((rf.activity-c.activity)/c.activity*100)"
			+ " 		when rf.activity > 0 then 100.0 else 0.0 end) as maxPctDiff, "
			+ " 	min(case when c.activity > 0 then abs((rf.activity-c.activity)/c.activity*100)"
			+ " 		when rf.activity > 0 then 100.0 else 0.0 end) as minPctDiff,"
			+ " 	count(*) as c"
			+ " from movesActivityOutput rf"
			+ " inner join movesActivityOutput c using (yearID,monthID,dayID,hourID,sourceTypeID" + (useModelYear?",modelYearID":"") + (useFuelType?",fuelTypeID":"") + ")"
			+ " where rf.movesRunID=" + newRunID + " and c.movesRunID=" + classicRunID
			+ " and rf.activityTypeID = ##activityTypeID## and c.activityTypeID=##activityTypeID##"
			+ " group by rf.sourceTypeID" + (useFuelType? ",rf.fuelTypeID":"")
			+ " order by rf.sourceTypeID" + (useFuelType? ",rf.fuelTypeID":"");

		String[] statements = {
			"Distance", "1",
			"ExtIdle", "3",
			"Population", "6",
			"Starts", "7",
			"Hotelling", "8"
		};
		for(int i=(useDistance?0:2);i<statements.length;i+=2) {
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				System.out.println("Comparing " + caseName + " " + statements[i+0] + "...");
				String sql = StringUtilities.replace(coreSQL,"##activityTypeID##",statements[i+1]);
				query.open(db,sql);
				dumpResults(query,caseName,statements[i+0]);
			} catch(Exception e) {
				// Nothing to do here
			} finally {
				query.onFinally();
			}
		}
	}
	
	void writeOutput() {
		String[] columnNames = {
			"MOVESOutput",
			"ratePerDistance",
			"ratePerVehicle",
			"Distance",
			"ExtIdle",
			"Population",
			"Starts",
			"Hotelling"
		};
		String[] rowNames = {
			"Inventory_FullDetail",
			"Inventory2030_FullDetail",
			"Rates_FullDetail",
			"Rates2012_FullDetail",
			"Rates2030_FullDetail",
			"Rates_NoFuel",
			"Rates_NoModelYear",
			"Rates_NoFuelNoModelYear"
		};
		int[] runIDs = {
			classicInventory, rfInventory,
			classicInventory2030, rfInventory2030,
			classicRatesFullDetail, rfRatesFullDetail,
			classicRates2012FullDetail, rfRates2012FullDetail,
			classicRates2030FullDetail, rfRates2030FullDetail,
			classicRatesNoFuel, rfRatesNoFuel,
			classicRatesNoModelYear, rfRatesNoModelYear,
			classicRatesNoFuelNoModelYear, rfRatesNoFuelNoModelYear
		};
		String summaryTable = "<table border=\"1\"><tr><th></th><th>IDs</th>";
		String details = "";

		// Make title row
		for(int i=0;i<columnNames.length;i++) {
			summaryTable += "<th>" + columnNames[i] + "</th>";
		}
		summaryTable += "</tr>\n";

		// Make each data row and details
		for(int r=0;r<rowNames.length;r++) {
			summaryTable += "<tr><td>" + rowNames[r] + "</td>";
			summaryTable += "<td>C: " + runIDs[r*2+0] + ", R: " + runIDs[r*2+1] + "</td>";
			for(int c=0;c<columnNames.length;c++) {
				String key = getKey(rowNames[r],columnNames[c]);
				String summary = summaryText.get(key);
				if(summary == null) {
					summaryTable += "<td></td>";
				} else {
					summaryTable += "<td><a href=\"#" + key + "\">" + summary + "</a></td>";
					// Add details and a link to the details
					String d = detailHTML.get(key);
					if(d != null) {
						details += "<a name=\"" + key + "\"></a><b>" + rowNames[r] + " / " + columnNames[c] + "</b><br>\n";
						details += d + "<br>\n";
					}
				}
			}
		}
		
		// Finish the table
		summaryTable += "</table>\n<br><br>";

		// Write the HTML file
		PrintWriter writer = null;
		try {
			File htmlFile = new File("ratesFirstComparisons.html");
			writer = new PrintWriter(new BufferedWriter(new FileWriter(htmlFile),128*1024));

			writer.println("<html>");
			writer.println("	<head>");
			writer.println("		<title>MOVES Rates First Comparisons</title>");
			writer.println("	</head>");
			writer.println("<b>MOVES Rates First Comparisons</b><br>\n");

			writer.println("<b>Max. Percent Difference</b><br>\n");
			writer.println(summaryTable);
			writer.println(details);

			// Done
			writer.println("</html>");
		} catch(Exception e) {
			/**
			 * @explain The output HTML file was removed or edited by a user or virus scanner
			 * before MOVES was finished with it.
			**/
			System.out.println("ERROR: Unable to write to HTML: " + e.toString());
		} finally {
			if(writer != null) {
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				writer = null;
			}
		}
	}
}
