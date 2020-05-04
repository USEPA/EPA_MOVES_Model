/**************************************************************************************************
 * @(#)MySQLSlowQueryAnalyzer.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.tools;

import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;

/**
 * Analyze and sort a MySQL log file of slow queries.
 *
 * @author		Wesley Faler
 * @version		2014-07-21
**/
public class MySQLSlowQueryAnalyzer {
	/** Test cases **/
	public static void main(String args[]) {
		String[] files = {
			//"testdata/SlowQueries/b1_slow_queries.txt", "testdata/SlowQueries/sorted_b1_slow_queries.txt",
			//"testdata/SlowQueries/b2_slow_queries.txt", "testdata/SlowQueries/sorted_b2_slow_queries.txt",
			//"testdata/SlowQueries/b2a_slow_queries.txt", "testdata/SlowQueries/sorted_b2a_slow_queries.txt",
			//"testdata/SlowQueries/b2b_slow_queries.txt", "testdata/SlowQueries/sorted_b2b_slow_queries.txt",
			//"testdata/SlowQueries/b2c_slow_queries.txt", "testdata/SlowQueries/sorted_b2c_slow_queries.txt",
			//"testdata/SlowQueries/b2d_slow_queries.txt", "testdata/SlowQueries/sorted_b2d_slow_queries.txt"
			//"testdata/SlowQueries/b2e_slow_queries.txt", "testdata/SlowQueries/sorted_b2e_slow_queries.txt"
			//"testdata/SlowQueries/b4_mesoscale_slow_queries.txt", "testdata/SlowQueries/sorted_b4_mesoscale_slow_queries.txt"
			//"testdata/SlowQueries/b4_slow_queries.txt", "testdata/SlowQueries/sorted_b4_slow_queries.txt"
			//"testdata/SlowQueries/b5a_slow_queries.txt", "testdata/SlowQueries/sorted_b5a_slow_queries.txt"
			//"C:\\EPA\\MOVES\\MOVESGHGSource\\T806 Air Toxics\\T806_mysql_slow_queries.txt", "C:\\EPA\\MOVES\\MOVESGHGSource\\T806 Air Toxics\\sorted_T806_mysql_slow_queries.txt"
			//"C:\\EPA\\MOVES\\MOVESGHGSource\\mysql_slow_queries.txt", "C:\\EPA\\MOVES\\MOVESGHGSource\\sorted_mysql_slow_queries.txt"
			//"C:\\EPA\\MOVES\\MOVESGHGSource\\Ref_mysql_slow_queries.txt", "C:\\EPA\\MOVES\\MOVESGHGSource\\sorted_mysql_slow_queries.txt"
			//"C:\\EPA\\MOVES\\MOVESGHGSource\\Speed2_Master_slowqueries.txt", "C:\\EPA\\MOVES\\MOVESGHGSource\\sorted_Speed2_Master_slowqueries.txt",
			//"C:\\EPA\\MOVES\\MOVESGHGSource\\Speed3_Worker_slowqueries.txt", "C:\\EPA\\MOVES\\MOVESGHGSource\\sorted_Speed3_Worker_slowqueries.txt"
			//"C:\\epa\\moves\\tasks\\Task1016 April Fixes\\diary\\SpeedTests_Baseline\\worker_slowqueries.txt", "C:\\epa\\moves\\tasks\\Task1016 April Fixes\\diary\\SpeedTests_Baseline\\sorted_worker_slowqueries.txt",
			//"C:\\epa\\moves\\tasks\\Task1016 April Fixes\\diary\\SpeedTests_Baseline\\master_slowqueries.txt", "C:\\epa\\moves\\tasks\\Task1016 April Fixes\\diary\\SpeedTests_Baseline\\sorted_master_slowqueries.txt",
			//"C:\\EPA\\MOVES\\Tasks\\Task1016 April Fixes\\diary\\SpeedTests_SmokeTests_20100517\\slowqueries.txt", "C:\\EPA\\MOVES\\Tasks\\Task1016 April Fixes\\diary\\SpeedTests_SmokeTests_20100517\\sorted_slowqueries.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1016 April Fixes\\diary\\SpeedTests_From_Dave\\nei_0519_slowqueries.txt", "C:\\EPA\\MOVES\\Tasks\\Task1016 April Fixes\\diary\\SpeedTests_From_Dave\\sorted_nei_0519_slowqueries.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1016 April Fixes\\diary\\SpeedTests_SmokeTests_20100517\\20100604_methane1_slowqueries.txt", "C:\\EPA\\MOVES\\Tasks\\Task1016 April Fixes\\diary\\SpeedTests_SmokeTests_20100517\\20100604_methane1_sorted_slowqueries.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Baseline\\MyISAM_as_Default\\CriteriaStart_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Baseline\\MyISAM_as_Default\\CriteriaStart_sorted_slowqueries.txt",
			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Baseline\\MyISAM_as_Default\\slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Baseline\\MyISAM_as_Default\\sorted_slowqueries.txt",
			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Baseline\\InnoDB_as_Default\\slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Baseline\\InnoDB_as_Default\\sorted_slowqueries.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Test1\\slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\ReduceWorkerTableSize\\Test1\\sorted_slowqueries.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\FromDave\\2010_StMo_NoPreAgg\\20101008slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\FromDave\\2010_StMo_NoPreAgg\\20101008sorted_slowqueries.txt",

			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\Macro\\BeforeMacroSpeedup\\slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\Macro\\BeforeMacroSpeedup\\sorted_slowqueries.txt",

			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\Macro\\MacroSpeedup1\\slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\Macro\\MacroSpeedup1\\sorted_slowqueries.txt"

			//"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\Macro\\ATHCSpeedup1\\slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1021 Add Additional Model Run-Time Performance Improvements to MOVES\\diary\\Macro\\ATHCSpeedup1\\sorted_slowqueries.txt"

			//"C:\\EPA\\MOVES\\Tasks\\Task1110 Update Evap Emissions\\diary\\Speed\\before_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1110 Update Evap Emissions\\diary\\Speed\\before_sorted_slowqueries.txt"

			//"C:\\EPA\\MOVES\\Tasks\\Task1110 Update Evap Emissions\\diary\\Speed\\after_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1110 Update Evap Emissions\\diary\\Speed\\after_sorted_slowqueries.txt"
			
			//"C:\\EPA\\MOVES\\Tasks\\Unfiled\\20120404_Performance\\slowqueries_20120404_AfterHCFuelSupply.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Unfiled\\20120404_Performance\\slowqueries_20120404_AfterHCFuelSupply_sorted.txt"

			//"C:\\EPA\\MOVES\\Tasks\\Unfiled\\20120404_Performance\\slowqueries_20120409_smalltest.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Unfiled\\20120404_Performance\\slowqueries_20120409_smalltest_sorted.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1206 Rates First Phase Two\\diary\\slowqueries20120910.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1206 Rates First Phase Two\\diary\\slowqueries20120910_sorted.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1201 Performance Indexing\\diary\\20121228a3_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1201 Performance Indexing\\diary\\20121228a3_sorted_slowqueries.txt"
			//"C:\\EPA\\MOVES\\Tasks\\Task1206 Rates First Phase Two\\diary\\20130103_Performance\\20130108_1hr_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1206 Rates First Phase Two\\diary\\20130103_Performance\\20130108_sorted_1hr_slowqueries.txt",
			//"C:\\EPA\\MOVES\\Tasks\\Task1206 Rates First Phase Two\\diary\\20130103_Performance\\20130108_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1206 Rates First Phase Two\\diary\\20130103_Performance\\20130108_sorted_slowqueries.txt",

			//"C:\\EPA\\MOVES\\Tasks\\Task1307-05 Adding Rates First for Chained Pollutants\\diary\\20131218_Performance\\20140112_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1307-05 Adding Rates First for Chained Pollutants\\diary\\20131218_Performance\\20140112_sorted_slowqueries.txt"

			//"C:\\EPA\\MOVES\\Tasks\\Task1307-05 Adding Rates First for Chained Pollutants\\diary\\20131218_Performance\\20140217_After_slowqueries.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1307-05 Adding Rates First for Chained Pollutants\\diary\\20131218_Performance\\20140217_sorted_After_slowqueries.txt",
			
			//"C:\\EPA\\MOVES\\Tasks\\Task1407-18 Performance and Bugs\\diary\\Performance_Tests\\slowqueries_Before_MOVES2014_OneCounty.txt",
			//	"C:\\EPA\\MOVES\\Tasks\\Task1407-18 Performance and Bugs\\diary\\Performance_Tests\\sorted_slowqueries_Before_MOVES2014_OneCounty.txt",

			"C:\\EPA\\MOVES\\Tasks\\Task1407-18 Performance and Bugs\\diary\\Performance_Tests\\slowqueries_MOVES2014_20140723_OneCounty.log",
				"C:\\EPA\\MOVES\\Tasks\\Task1407-18 Performance and Bugs\\diary\\Performance_Tests\\sorted_slowqueries_MOVES2014_20140723_OneCounty.txt"
		};
		for(int i=0;i<files.length;i+=2) {
			MySQLSlowQueryAnalyzer a = new MySQLSlowQueryAnalyzer();
			a.process(new File(files[i+0]),new File(files[i+1]));
		}
	}

	static class QueryDetails {
		public String sql;
		public String database;
		public double seconds = 0;
		public long recordsExamined = 0;
	}

	static class QuerySummary implements Comparable {
		public String genericSQL;
		public ArrayList<QueryDetails> queryDetails = new ArrayList<QueryDetails>();
		public double seconds = 0;
		public long recordsExamined = 0;

		public void add(QueryDetails q) {
			queryDetails.add(q);
			seconds += q.seconds;
			recordsExamined += q.recordsExamined;
		}

		public int compareTo(Object other) {
			if(other == null || !(other instanceof QuerySummary)) {
				return 1;
			}
			QuerySummary q = (QuerySummary)other;
			if(seconds != q.seconds) {
				if(seconds > q.seconds) {
					return +1;
				} else {
					return -1;
				}
			}
			if(queryDetails.size() != q.queryDetails.size()) {
				return queryDetails.size() - q.queryDetails.size();
			}
			return genericSQL.compareTo(q.genericSQL);
		}
	}

	static class QuerySummaryComparator implements Comparator<QuerySummary> {
		/**
		 * Compare two QuerySummary objects, in reverse order
		 * @param q1 the first object to be compared.
		 * @param q2 the second object to be compared.
		 * @return a negative integer, zero, or a positive integer as the first
		 * argument is less than, equal to, or greater than the second.
		**/
		public int compare(QuerySummary q1,QuerySummary q2) {
			return -q1.compareTo(q2);
		}
	}

	static class QuerySummaryTreeSet extends TreeSet<QuerySummary> {
		public QuerySummaryTreeSet() {
			super(new QuerySummaryComparator());
		}
	}

	/** Standard constructor. **/
	public MySQLSlowQueryAnalyzer() {
	}

	/**
	 * Store a query's information
	 * @param queries statistics
	 * @param statisticsLine line of the form:
	 * # Query_time: 20  Lock_time: 0  Rows_sent: 0  Rows_examined: 4390998
	 * @param database database the query was performed within
	 * @param query sql
	**/
	private void process(TreeMapIgnoreCase queries,String statisticsLine,
			String database,String query) {
		String genericQuery = SQLRunner.toGeneric(query);
		QuerySummary q = (QuerySummary)queries.get(genericQuery);
		if(q == null) {
			q = new QuerySummary();
			q.genericSQL = genericQuery;
			queries.put(genericQuery,q);
		}

		String[] parts = statisticsLine.split("\\s");
		if(parts == null || parts.length <= 0) {
			return;
		}

		QueryDetails d = new QueryDetails();
		d.sql = query;
		d.database = database;
		for(int i=0;i<parts.length-1;i++) {
			if(parts[i].equalsIgnoreCase("Query_time:")) {
				try {
					d.seconds += Double.parseDouble(parts[i+1]);
				} catch(Exception e) {
					Logger.logError(e,"Unable to parse query time: " + parts[i+1]);
				}
			} else if(parts[i].equalsIgnoreCase("Lock_time:")) {
				try {
					d.seconds += Double.parseDouble(parts[i+1]);
				} catch(Exception e) {
					Logger.logError(e,"Unable to parse lock time: " + parts[i+1]);
				}
			} else if(parts[i].equalsIgnoreCase("Rows_examined:")) {
				try {
					d.recordsExamined = Long.parseLong(parts[i+1]);
				} catch(Exception e) {
					Logger.logError(e,"Unable to parse rows examined: " + parts[i+1]);
				}
			}
		}
		q.add(d);
	}

	public void process(File inputFile, File outputFile) {
		PrintWriter writer = null;
		LineNumberReader reader = null;
		File csvOutputFile = new File(outputFile.getParentFile(),outputFile.getName()+".csv");
		// QuerySummary objects, keyed by generic query
		TreeMapIgnoreCase queries = new TreeMapIgnoreCase();
		try {
			// Load the input file
			reader = new LineNumberReader(new BufferedReader(new FileReader(inputFile),64*1024));
			String line;
			String database = "";
			boolean accumulating = false;
			String sql = "";
			String statisticsLine = "";
			while((line = reader.readLine()) != null) {
				if(line.startsWith("# Query_time: ")) {
					if(accumulating && sql.length() > 0) {
						process(queries,statisticsLine,database,sql);
						accumulating = false;
						sql = "";
					}
					statisticsLine = line;
					accumulating = true;
					continue;
				}
				if(!accumulating) {
					continue;
				}
				if(line.startsWith("use ")) {
					database = line.substring(4,line.length()-1);
					continue;
				}
				if(line.startsWith("#")) {
					if(accumulating && sql.length() > 0) {
						process(queries,statisticsLine,database,sql);
						accumulating = false;
						sql = "";
					}
					accumulating = false;
					continue;
				}
				sql += line + "\n";
			}
			if(accumulating && sql.length() > 0) {
				process(queries,statisticsLine,database,sql);
				accumulating = false;
				sql = "";
			}
			reader.close();
			reader = null;
			// Sort the summary
			QuerySummaryTreeSet sortedQueries = new QuerySummaryTreeSet();
			Set<String> keys = queries.keySet();
			for(Iterator<String> i=keys.iterator();i.hasNext();) {
				sql = (String)i.next();
				QuerySummary q = (QuerySummary)queries.get(sql);
				sortedQueries.add(q);
			}
			// Write the output file
			writer = new PrintWriter(new BufferedWriter(new FileWriter(outputFile),64*1024));
			for(Iterator<QuerySummary> i=sortedQueries.iterator();i.hasNext();) {
				QuerySummary q = (QuerySummary)i.next();
				writer.println("# ----------------------------------------------");
				writer.println("# Total_Seconds: " + q.seconds
						+ " Total_Calls: " + q.queryDetails.size()
						+ " Total_Records_Examined: " + q.recordsExamined);
				writer.println(q.genericSQL);
				writer.println("# ----------------------------------------------");
				for(int j=0;j<5 && j<q.queryDetails.size();j++) {
					QueryDetails d = (QueryDetails)q.queryDetails.get(j);
					writer.println("\t# Example_Seconds: " + d.seconds
						+ " Example_Records_Examined: " + d.recordsExamined);
					writer.println("\t" + d.sql);
				}
			}
			writer.close();
			writer = null;

			// Write the CSV output file
			writer = new PrintWriter(new BufferedWriter(new FileWriter(csvOutputFile),64*1024));
			writer.println("Total Seconds,Calls,Typical Records,Concurrency,Notes,Query");
			for(Iterator<QuerySummary> i=sortedQueries.iterator();i.hasNext();) {
				QuerySummary q = (QuerySummary)i.next();
				QueryDetails d = (QueryDetails)q.queryDetails.get(0);
				writer.println(q.seconds
						+ "," + q.queryDetails.size()
						+ "," + d.recordsExamined
						+ ","
						+ ","
						+ ",\"" + d.sql + "\""
						);
			}
			writer.close();
			writer = null;
		} catch(IOException e) {
			Logger.logError(e,"Unable to process slow query log");
		} finally {
			if(writer != null) {
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do
				}
				writer = null;
			}
			if(reader != null) {
				try {
					reader.close();
				} catch(Exception e) {
					// Nothing to do
				}
				reader = null;
			}
		}
	}
}
