/**************************************************************************************************
 * @(#)SCC.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Represents one record in the SCC table.
 *
 * @author		Cimulus
 * @version		2003-01-27
**/
public class SCC implements Comparable {
	/** The SCC field in the SCC table **/
	public String scc;
	/** The Part5VClass field in the SCC table **/
	public int part5VClass;
	/** The HPMSRoadType field in the SCC table **/
	public int HPMSRoadType;
	/** The Segment field in the SCC table **/
	public int segment;
	/** The FuelType field in the SCC table **/
	public String fuelType;
	/** The Strokes field in the SCC table **/
	public int strokes;
	/** The Part5VClassDesc field in the SCC table **/
	public String part5VClassDesc;
	/** The RoadwayTypeDesc field in the SCC table **/
	public String roadwayTypeDesc;
	/** The SegmentDesc field in the SCC table **/
	public String segmentDesc;
	/** The SCCDesc field in the SCC table **/
	public String SCCDesc;
	/** The mgNH3perGallon field in the SCC table **/
	public double mgNH3perGallon;
	/** The BenzExhGas field in the SCC table **/
	public double benzExhGas;
	/** The BenzExhEth field in the SCC table **/
	public double benzExhEth;
	/** The BenzExhMTBE field in the SCC table **/
	public double benzExhMTBE;
	/** The BenzExhRFG field in the SCC table **/
	public double benzExhRFG;
	/** The BenzEvapGas field in the SCC table **/
	public double benzEvapGas;
	/** The BenzEvapEth field in the SCC table **/
	public double benzEvapEth;
	/** The BenzEvapMTBE field in the SCC table **/
	public double benzEvapMTBE;
	/** The BenzEvapRFG field in the SCC table **/
	public double benzEvapRFG;
	/** The ButaExhGas field in the SCC table **/
	public double butaExhGas;
	/** The ButaExhEth field in the SCC table **/
	public double butaExhEth;
	/** The ButaExhMTBE field in the SCC table **/
	public double butaExhMTBE;
	/** The ButaExhRFG field in the SCC table **/
	public double butaExhRFG;
	/** The FormExhGas field in the SCC table **/
	public double formExhGas;
	/** The FormExhEth field in the SCC table **/
	public double formExhEth;
	/** The FormExhMTBE field in the SCC table **/
	public double formExhMTBE;
	/** The FormExhRFG field in the SCC table **/
	public double formExhRFG;
	/** The AcetExhGas field in the SCC table **/
	public double acetExhGas;
	/** The AcetExhEth field in the SCC table **/
	public double acetExhEth;
	/** The AcetExhMTBE field in the SCC table **/
	public double acetExhMTBE;
	/** The AcetExhRFG field in the SCC table **/
	public double acetExhRFG;
	/** The AcroExhGas field in the SCC table **/
	public double acroExhGas;
	/** The AcroExhEth field in the SCC table **/
	public double acroExhEth;
	/** The AcroExhMTBE field in the SCC table **/
	public double acroExhMTBE;
	/** The AcroExhRFG field in the SCC table **/
	public double acroExhRFG;
	/** The MTBEExhGas field in the SCC table **/
	public double MTBEExhGas;
	/** The MTBEExhEth field in the SCC table **/
	public double MTBEExhEth;
	/** The MTBEExhMTBE field in the SCC table **/
	public double MTBEExhMTBE;
	/** The MTBEExhRFG field in the SCC table **/
	public double MTBEExhRFG;
	/** The MTBEEvapGas field in the SCC table **/
	public double MTBEEvapGas;
	/** The MTBEEvapEth field in the SCC table **/
	public double MTBEEvapEth;
	/** The MTBEEvapMTBE field in the SCC table **/
	public double MTBEEvapMTBE;
	/** The MTBEEvapRFG field in the SCC table **/
	public double MTBEEvapRFG;
	/** The BenzExhDies field in the SCC table **/
	public double benzExhDies;
	/** The ButaExhDies field in the SCC table **/
	public double butaExhDies;
	/** The FormExhDies field in the SCC table **/
	public double formExhDies;
	/** The AcetExhDies field in the SCC table **/
	public double acetExhDies;
	/** The AcroExhDies field in the SCC table **/
	public double acroExhDies;
	/** The TOGfac field in the SCC table **/
	public double togFac;
	/** The NMOGfac field in the SCC table **/
	public double nmogFac;
	/** The NMHCfac field in the SCC table **/
	public double nmhcFac;
	/** The VOCfac field in the SCC table **/
	public double vocFac;
	/** The PM25fac field in the SCC table **/
	public double pm25Fac;
	/** The SOAfac field in the SCC table **/
	public double soaFac;

	/**
	 * Constructor
	**/
	public SCC() {
		scc = "";
		part5VClass = 0;
		HPMSRoadType = 0;
		segment = 0;
		fuelType = "";
		strokes = 0;
		part5VClassDesc = "";
		roadwayTypeDesc = "";
		segmentDesc = "";
		SCCDesc = "";
		mgNH3perGallon = 0.0;
		benzExhGas = 0.0;
		benzExhEth = 0.0;
		benzExhMTBE = 0.0;
		benzExhRFG = 0.0;
		benzEvapGas = 0.0;
		benzEvapEth = 0.0;
		benzEvapMTBE = 0.0;
		benzEvapRFG = 0.0;
		butaExhGas = 0.0;
		butaExhEth = 0.0;
		butaExhMTBE = 0.0;
		butaExhRFG = 0.0;
		formExhGas = 0.0;
		formExhEth = 0.0;
		formExhMTBE = 0.0;
		formExhRFG = 0.0;
		acetExhGas = 0.0;
		acetExhEth = 0.0;
		acetExhMTBE = 0.0;
		acetExhRFG = 0.0;
		acroExhGas = 0.0;
		acroExhEth = 0.0;
		acroExhMTBE = 0.0;
		acroExhRFG = 0.0;
		MTBEExhGas = 0.0;
		MTBEExhEth = 0.0;
		MTBEExhMTBE = 0.0;
		MTBEExhRFG = 0.0;
		MTBEEvapGas = 0.0;
		MTBEEvapEth = 0.0;
		MTBEEvapMTBE = 0.0;
		MTBEEvapRFG = 0.0;
		benzExhDies = 0.0;
		butaExhDies = 0.0;
		formExhDies = 0.0;
		acetExhDies = 0.0;
		acroExhDies = 0.0;
		togFac = 0.0;
		nmogFac = 0.0;
		nmhcFac = 0.0;
		vocFac = 0.0;
		pm25Fac = 0.0;
		soaFac = 0.0;
	}

	/**
	 * Loads the SCC fields from the record on which the result set is currently positioned.
	 * Expects all columns to have been selected, but gets values by column name, rather
	 * than index, so the columns can be selected in any order.
	 * @param resultSet A valid result set positioned on the record to be loaded.
	 * @throws SQLException If there is an error getting a value.
	**/
	public void loadFromResultSet(ResultSet resultSet) throws SQLException {
		scc = resultSet.getString("SCC");
		part5VClass = resultSet.getInt("Part5VClass");
		HPMSRoadType = resultSet.getInt("HPMSRoadType");
		segment = resultSet.getInt("Segment");
		fuelType = resultSet.getString("FuelType");
		strokes = resultSet.getInt("Strokes");
		part5VClassDesc = resultSet.getString("Part5VClassDesc");
		roadwayTypeDesc = resultSet.getString("RoadwayTypeDesc");
		segmentDesc = resultSet.getString("SegmentDesc");
		SCCDesc = resultSet.getString("SCCDesc");
		mgNH3perGallon = resultSet.getDouble("mgNH3perGallon");
		benzExhGas = resultSet.getDouble("BenzExhGas");
		benzExhEth = resultSet.getDouble("BenzExhEth");
		benzExhMTBE = resultSet.getDouble("BenzExhMTBE");
		benzExhRFG = resultSet.getDouble("BenzExhRFG");
		benzEvapGas = resultSet.getDouble("BenzEvapGas");
		benzEvapEth = resultSet.getDouble("BenzEvapEth");
		benzEvapMTBE = resultSet.getDouble("BenzEvapMTBE");
		benzEvapRFG = resultSet.getDouble("BenzEvapRFG");
		butaExhGas = resultSet.getDouble("ButaExhGas");
		butaExhEth = resultSet.getDouble("ButaExhEth");
		butaExhMTBE = resultSet.getDouble("ButaExhMTBE");
		butaExhRFG = resultSet.getDouble("ButaExhRFG");
		formExhGas = resultSet.getDouble("FormExhGas");
		formExhEth = resultSet.getDouble("FormExhEth");
		formExhMTBE = resultSet.getDouble("FormExhMTBE");
		formExhRFG = resultSet.getDouble("FormExhRFG");
		acetExhGas = resultSet.getDouble("AcetExhGas");
		acetExhEth = resultSet.getDouble("AcetExhEth");
		acetExhMTBE = resultSet.getDouble("AcetExhMTBE");
		acetExhRFG = resultSet.getDouble("AcetExhRFG");
		acroExhGas = resultSet.getDouble("AcroExhGas");
		acroExhEth = resultSet.getDouble("AcroExhEth");
		acroExhMTBE = resultSet.getDouble("AcroExhMTBE");
		acroExhRFG = resultSet.getDouble("AcroExhRFG");
		MTBEExhGas = resultSet.getDouble("MTBEExhGas");
		MTBEExhEth = resultSet.getDouble("MTBEExhEth");
		MTBEExhMTBE = resultSet.getDouble("MTBEExhMTBE");
		MTBEExhRFG = resultSet.getDouble("MTBEExhRFG");
		MTBEEvapGas = resultSet.getDouble("MTBEEvapGas");
		MTBEEvapEth = resultSet.getDouble("MTBEEvapEth");
		MTBEEvapMTBE = resultSet.getDouble("MTBEEvapMTBE");
		MTBEEvapRFG = resultSet.getDouble("MTBEEvapRFG");
		benzExhDies = resultSet.getDouble("BenzExhDies");
		butaExhDies = resultSet.getDouble("ButaExhDies");
		formExhDies = resultSet.getDouble("FormExhDies");
		acetExhDies = resultSet.getDouble("AcetExhDies");
		acroExhDies = resultSet.getDouble("AcroExhDies");
		togFac = resultSet.getDouble("TOGfac");
		nmogFac = resultSet.getDouble("NMOGfac");
		nmhcFac = resultSet.getDouble("NMHCfac");
		vocFac = resultSet.getDouble("VOCfac");
		pm25Fac = resultSet.getDouble("PM25fac");
		soaFac = resultSet.getDouble("SOAfac");
	}

	/**
	 * Comparison routine used to sort these objects for display purposes.
	 * Compares the scc code only (case-insensitive).
	 * @param	other another SCC to compare to
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		if(other instanceof SCC) {
			SCC otherSelection = (SCC)other;
			return scc.compareToIgnoreCase(otherSelection.scc);
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * Comparison routine to test for equality.
	 * @param	other another SCC to compare to
	 * @return true if compareTo returns 0
	**/
	public boolean equals(Object other) {
		if(other instanceof SCC) {
			if(0 == compareTo(other)) {
				return true;
			}
		}
		return false;
	}
}
