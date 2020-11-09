/**************************************************************************************************
 * @(#)TankVaporVentingCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.RunSpec;
import java.util.*;
import java.sql.*;

/**
 * @algorithm
 * @owner Tank Vapor Venting Calculator
 * @calculator
**/

/**
 * Does processing for the Tank vapor venting process
 * @author		Wesley Faler
 * @author      EPA - Mitch C. Task 212A
 * @version		2011-01-02
**/
public class TankVaporVentingCalculator extends GenericCalculatorBase {
	/** Calendar years that have already been processed and data cached **/
	TreeSet<Integer> yearsSeen = new TreeSet<Integer>();

	static class EquationAbbreviation {
		public String equation;
		public String abbreviation;
	}
	/** tvv equations and their abbreviations **/
	ArrayList<EquationAbbreviation> tvvEquations = null;
	/** leaking equations and their abbreviations **/
	ArrayList<EquationAbbreviation> leakingEquations = null;
	/** CASE statement for the tvvEquation field **/
	String tvvCaseStatement = null;
	/** CASE statement for the leakEquation field **/
	String leakCaseStatement = null;
	/** True during the first bundle **/
	boolean isFirst = true;

	/**
	 * constructor
	**/
	public TankVaporVentingCalculator() {
		super(new String[] {
				"112"  // Total Gaseous Hydrocarbons Evap Fuel Vapor Venting
			},
			MasterLoopGranularity.MONTH, // Year instead of zone due to IM
										 // Month instead of year due to op modes
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR

			CompilationFlags.USE_MULTIDAY_DIURNALS? "database/MultidayTankVaporVentingCalculator.sql" : "database/TankVaporVentingCalculator.sql",

			ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MESOSCALE_LOOKUP?
				(new String[]{ "MesoscaleLookup" })
				:
				(new String[]{ "NonMesoscaleLookup" })
			//null // No special section names in the SQL
		);
	}

	/**
	 * Called just before readAndHandleScriptedCalculations(...) is called, this is the last
	 * point at which a derived class can alter the script replacement values and/or the
	 * set of section names to be enabled.  The derived class is allowed to remove entries
	 * from the passed parameters as well as add or modify entries.
	 * The default implementation is to do nothing.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @param replacements name/value pairs for replacement within the SQL script
	 * @param enabledSectionNames names of non-standard sections to used within the SQL script
	**/
	public void alterReplacementsAndSections(MasterLoopContext context,
			TreeMapIgnoreCase replacements,
			TreeSetIgnoreCase enabledSectionNames) {
		if(!CompilationFlags.USE_MULTIDAY_DIURNALS) {
			return;
		}
		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			// Expand equations from cumTVVCoeffs into SQL and place them back into replacements for use in the script.
			if(tvvEquations == null) {
				ArrayList<String> sqlsToExecute = new ArrayList<String>();
				tvvEquations = new ArrayList<EquationAbbreviation>();
				query.open(db,"select distinct tvvEquation from cumTVVCoeffs");
				tvvCaseStatement = "case tvvEquation";
				while(query.rs.next()) {
					String equation = StringUtilities.safeGetString(query.rs.getString(1));
					if(equation.length() <= 0) {
						equation = "0";
					}
					EquationAbbreviation a = new EquationAbbreviation();
					a.equation = equation;
					a.abbreviation = "T" + tvvEquations.size();
					tvvEquations.add(a);
					tvvCaseStatement += " when '" + a.abbreviation + "' then " + a.equation;
					sqlsToExecute.add("update cumTVVCoeffs set tvvEquation='" + a.abbreviation + "'"
							+ " where tvvEquation=" + DatabaseUtilities.escapeSQL(a.equation,true));
				}
				query.close();
				tvvCaseStatement += " else 0 end";

				leakingEquations = new ArrayList<EquationAbbreviation>();
				query.open(db,"select distinct leakEquation from cumTVVCoeffs");
				leakCaseStatement = "case leakEquation";
				while(query.rs.next()) {
					String equation = StringUtilities.safeGetString(query.rs.getString(1));
					if(equation.length() <= 0) {
						equation = "0";
					}
					EquationAbbreviation a = new EquationAbbreviation();
					a.equation = equation;
					a.abbreviation = "L" + leakingEquations.size();
					leakingEquations.add(a);
					leakCaseStatement += " when '" + a.abbreviation + "' then " + a.equation;
					sqlsToExecute.add("update cumTVVCoeffs set leakEquation='" + a.abbreviation + "'"
							+ " where leakEquation=" + DatabaseUtilities.escapeSQL(a.equation,true));
				}
				query.close();
				leakCaseStatement += " else 0 end";

				for(Iterator<String> i=sqlsToExecute.iterator();i.hasNext();) {
					SQLRunner.executeSQL(db,i.next());
				}
			}

			// Add replacements that use the large CASE statements built for each equation type
			replacements.put("##tvvEquations##",tvvCaseStatement);
			replacements.put("##leakEquations##",leakCaseStatement);

			// Cache year-specific data.  Note: This must be done after equations have been converted to identifiers
			// that are under 100 characters.
			Integer year = Integer.valueOf(context.year);
			if(!yearsSeen.contains(year)) {
				yearsSeen.add(year);
				enabledSectionNames.add("NewTVVYear");
			}

			// Clear precalculated tables if the user has changed the basis values used to populate them.
			if(isFirst) {
				SQLRunner.executeSQL(db,"insert ignore into sampleVehicleSoakingDayBasis (soakDayID, dayID, F) select soakDayID, dayID, F from sampleVehicleSoakingDayBasisUsed");
				SQLRunner.executeSQL(db,"insert ignore into sampleVehicleSoakingDay (soakDayID, sourceTypeID, dayID, F) select soakDayID, sourceTypeID, dayID, F from sampleVehicleSoakingDayUsed");

				if(!areTablesIdentical(db,"sampleVehicleSoakingDayBasis","sampleVehicleSoakingDayBasisUsed")) {
					SQLRunner.executeSQL(db,"truncate table sampleVehicleSoakingDay");
					SQLRunner.executeSQL(db,"truncate table sampleVehicleSoaking");
				}
				if(!areTablesIdentical(db,"sampleVehicleSoakingDay","sampleVehicleSoakingDayUsed")) {
					SQLRunner.executeSQL(db,"truncate table sampleVehicleSoaking");
				}

				int count = (int)SQLRunner.executeScalar(db,"select count(*) from sampleVehicleSoaking");
				if(count <= 0) {
					enabledSectionNames.add("FillSampleVehicleSoaking");
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to do multiday diurnal equation processing");
		} finally {
			query.onFinally();
			if(db != null) {
				try {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				} catch(Exception e) {
					// Nothing to do here
				}
				db = null;
			}
			isFirst = false;
		}
	}

	/**
	 * Check two tables for identical data.
	 * @param db database to use
	 * @param firstTable a table to be checked
	 * @param secondTable a table to be checked
	 * @return true if the tables have identical data
	 * @throws SQLException if anything goes wrong
	**/
	private static boolean areTablesIdentical(Connection db, String firstTable, String secondTable) throws SQLException {
		String sql = "checksum table " + firstTable + ", " + secondTable + " extended";
		String firstText = null;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				String t = StringUtilities.safeGetString(query.rs.getString("Checksum"));
				if(firstText == null) {
					firstText = t;
				} else if(!t.equals(firstText)) {
					return false;
				}
			}
			return true;
		} catch(SQLException e) {
			Logger.logError(e,"Unable to check table checksum using: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}
}
