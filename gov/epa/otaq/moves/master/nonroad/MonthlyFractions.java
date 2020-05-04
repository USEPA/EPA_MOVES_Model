/**************************************************************************************************
 * @(#)MonthlyFractions.java 
 *
 *
 *
 *************************************************************************************************/

package gov.epa.otaq.moves.master.nonroad;

import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.TreeMap;

/**
 * Extracts monthly fractions from NONROAD Season data file.
 * 
 * @author Cimulus, EPA
 * @version 2005-10-21 fix January index from 56 to 51
 **/
public class MonthlyFractions {
	private TreeMap fractions;

	/** Standard constructor. **/
	public MonthlyFractions() {
	}

	/**
	 * Load fractions for a given state and month from a seasons data file.
	 * 
	 * @param monthlyFractionFileName
	 *            File containing the monthly fractions.
	 * @param fipsStateID
	 *            FIPS state ID to retrieve monthly fractions for.
	 * @param monthID
	 *            month ID to retrieve fractions for.
	 * @return true if fractions were loaded.
	 **/
	public boolean load(String monthlyFractionFileName, String outputFileName,
			int fipsStateID, int monthID) throws IOException {
		if (monthID < 1 || monthID > 12) {
			System.out.println("Invalid monthID provided");
			return false;
		}
		fractions = new TreeMap();

		BufferedReader br;
		try {
			br = new BufferedReader(new FileReader(monthlyFractionFileName));
		} catch (IOException e) {
			System.out
					.println("Error opening file: " + monthlyFractionFileName);
			throw e;
		}

		String region = findRegion(br, fipsStateID);
		if (region.length() == 0) {
			System.out
					.println("Unable to find the region for the state provided");
		}
		writeSCCsToFile(br, outputFileName, region, monthID);

		br.close();
		return true;
	}

	/**
	 * Scans through the file looking for the /REGION/ tag has the side effect
	 * of advancing the BufferedReader through the regions section
	 * 
	 * @param br
	 *            a buffered file reader for the file to look through
	 * @param fipsStateID
	 *            the stateID to search for
	 * @return the region code for the given state or "" if it couldn't be found
	 **/
	private String findRegion(BufferedReader br, int fipsStateID)
			throws IOException {
		boolean foundRegionsTag = false;
		String fullStateID = fipsStateID + "";
		if (fipsStateID >= 0 && fipsStateID < 10) {
			fullStateID = "0" + fullStateID;
		}
		fullStateID = fullStateID + "000";

		while (br.ready()) {
			String line = br.readLine();
			if (!foundRegionsTag) {
				if (line.indexOf("/REGIONS/") < 0) { // loop until we find the
														// regions section
					continue;
				} else {
					foundRegionsTag = true;
				}
			} else {
				if (line.indexOf("/END/") >= 0) { // if we hit the end, return;
					return "";
				}
				if (line.indexOf(fullStateID) >= 0) {
					return line.substring(0, line.indexOf(' '));
				}
			}
		}
		return "";
	}

	/**
	 * Looks for the SCC Code for the specified region has the side effect of
	 * advancing the buffered reader through the monthly section
	 * 
	 * @param br
	 *            a buffered file reader for the file to look through
	 * @param outputFileName
	 *            the name of the output file
	 * @param region
	 *            the region to search for
	 * @param monthID
	 *            the month to get data from
	 **/
	private void writeSCCsToFile(BufferedReader br, String outputFileName,
			String region, int monthID) throws IOException {
		final int JANUARY_INDEX = 51;
		final int MONTH_FIELD_WIDTH = 10;
		final int SCC_CODE_BEGIN = 6;
		final int SCC_CODE_END = 16;

		FileOutputStream out = new FileOutputStream(outputFileName);
		PrintStream p = new PrintStream(out);

		int monthIndex = JANUARY_INDEX + ((monthID - 1) * MONTH_FIELD_WIDTH);
		boolean foundMonthlyTag = false;
		while (br.ready()) {
			String line = br.readLine();
			if (!foundMonthlyTag) {
				if (line.indexOf("/MONTHLY/") < 0) { // advance to the monthly
														// section
					continue;
				} else {
					foundMonthlyTag = true;
				}
			} else {
				if (line.indexOf("/END/") >= 0) { // if we hit the end, return;
					return;
				}
				if (line.startsWith(region)) {
					// parse the line, store the SCC and fraction for the month
					String sccCode = line.substring(SCC_CODE_BEGIN,
							SCC_CODE_END);
					String monthFraction = line.substring(monthIndex,
							monthIndex + MONTH_FIELD_WIDTH);
					// fractions.put(sccCode, monthFraction);
					// System.out.print(sccCode + "\t" + monthFraction + "\n");
					p.println(sccCode.trim() + "\t" + monthFraction.trim());
				}
			}
		}
		p.close();
		out.close();
	}
}
