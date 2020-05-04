package gov.epa.otaq.moves.master.nonroad;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

public class NonroadHelper {

	public Map<Integer, String> evapIdFilenameMap = new HashMap<Integer, String>();

	public NonroadHelper() {
		NonroadDataFilesObj nonroadDataFiles = new NonroadDataFilesObj();

		evapIdFilenameMap.put(130, nonroadDataFiles.emsEvDiuFile);
		evapIdFilenameMap.put(120, nonroadDataFiles.emsEvTANKFile); // "EVTANK.EMF");
		evapIdFilenameMap.put(121, nonroadDataFiles.emsEvHOSEFile); // "EVHOSE.EMF");
		evapIdFilenameMap.put(122, nonroadDataFiles.emsEvNECKFile); // "EVNECK.EMF");
		evapIdFilenameMap.put(123, nonroadDataFiles.emsEvSUPRETFile); // "EVSUPRET.EMF");
		evapIdFilenameMap.put(124, nonroadDataFiles.emsEvVENTFile); // "EVVENT.EMF");
		evapIdFilenameMap.put(131, nonroadDataFiles.emsEvHOTSKFile); // "EVHOTSK.EMF");
		evapIdFilenameMap.put(132, nonroadDataFiles.emsEvRUNLSFile); // "EVRUNLS.EMF");
	}

	public static String mapPollutantToNonroad(String onroadTerm) {
		String retStr = "";
		// pollutant
		if (onroadTerm == null) {
		} else if (onroadTerm.equalsIgnoreCase("HC")) {
			retStr = "THC";
		} else if (onroadTerm.toUpperCase().startsWith("PM")) {
			retStr = "PM";
		} else {
			retStr = onroadTerm;
		}
		return retStr;
	}

	public static String mapPollutantToOnroad(String nonroadTerm) {
		String retStr = "";
		// pollutant
		if (nonroadTerm == null) {
		} else if (nonroadTerm.equalsIgnoreCase("THC")) {
			retStr = "HC";
		} else if (nonroadTerm.equalsIgnoreCase("PM")) {
			retStr = "PM10-PRI";
		} else {
			retStr = nonroadTerm;
		}
		return retStr;
	}

	public static double delta = 0.0000001;

	/**
	 * Converts an array of Integer objects to an array of integer primitives
	 * 
	 * @param integerList
	 *            the integer list
	 * 
	 * @return an array of integer primitives
	 */
	public static int[] toIntArray(List<Integer> integerList) {
		Collections.sort(integerList);
		int[] intArray = new int[integerList.size()];
		for (int i = 0; i < integerList.size(); i++) {
			intArray[i] = integerList.get(i);
		}
		return intArray;
	}

	/**
	 * Converts an array of Integer objects to an array of integer primitives
	 * 
	 * @param integerList
	 *            the integer list
	 * 
	 * @return an array of integer primitives
	 */
	public static int[] toIntArray(TreeSet<Integer> integerList) {
		int[] intArray = new int[integerList.size()];
		int i = 0;
		for (Integer I : integerList) {
			intArray[i++] = I;
		}
		return intArray;
	}

	public static final String[] nonroadTableNeededAtWorkerSide = {
			"nrsourceusetype", "nrscc", "enginetech", "nrequipmenttype" };
}
