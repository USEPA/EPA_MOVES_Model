/**************************************************************************************************
 * @(#)RefineryModel.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.general;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import java.util.*;
import java.sql.*;
import java.math.*;

/**
 * Update fuel formulation parameters in a manner consistent with refinery techniques.
 *
 * @author		Wesley Faler
 * @version		2015-06-03
**/
public class RefineryModel {
	/** true when being tested, causes extra logging **/
	public static boolean isTest = true; // false;

	/** true to allow extrapolation beyond correction factors in the database **/
	private static final boolean allowExtrapolation = false;

	/** Names, units, and DB name of properties that can be changed **/
	public static final String[] fuelPropertyNamesAndUnits = {
		"RVP", "psi", "RVP",
		"Sulfur", "ppm", "SULF",
		"Ethanol", "%", "ETOH",
		"T50", "F", "T50",
		"T90", "F", "T90",
		"Aromatics", "%", "AROM",
		"Olefins", "%", "OLEF",
		"Benzene", "%", "BENZ",
		//"E200", "%", "E200",
		//"E300", "%", "E300",
		//"BioDiesel Ester", "%", "BIO",
		//"CetaneIndex", "%", "CETANE",
		//"PAHContent", "%", "PAH",
		"MTBE", "%", "MTBE",
		"ETBE", "%", "ETBE",
		"TAME", "%", "TAME"
	};

	/** Represent a new or existing fuel formulation **/
	public static class FuelFormulation {
		public int regionID;
		public int fuelYearID;
		public int monthGroupID;

		/** Database identifier. Will be 0 when it is a new formulation **/
		public int fuelFormulationID = 0; // don't display
		public int fuelTypeID;
		public int fuelSubtypeID;

		/** Fuel properties **/
		public double RVP;
		public double sulfurLevel;
		public double ETOHVolume;
		public double MTBEVolume;
		public double ETBEVolume;
		public double TAMEVolume;
		public double aromaticContent;
		public double olefinContent;
		public double benzeneContent;
		public double e200;
		public double e300;
		public double volToWtPercentOxy;
		public double BioDieselEsterVolume;
		public double CetaneIndex;
		public double PAHContent;
		public double T50;
		public double T90;

		/** Flags used internally by the refinery model **/
		/*
		 	T50, T90, E200, and E300 are interrlated:
			T50 = 2.0408163 * (147.91 - e200) where e200 is not null and e200 > 0 and (T50 is null or T50 <= 0)
			T90 = 4.5454545 * (155.47 - e300) where e300 is not null and e300 > 0 and (T90 is null or T90 <= 0)
			e200 = 147.91-(T50/2.0408163) where T50 is not null and T50 > 0 and (e200 is null or e200 <= 0)
			e300 = 155.47-(T90/4.5454545) where T90 is not null and T90 > 0 and (e300 is null or e300 <= 0)
		*/
		private boolean needE200Calculation = false;
		private boolean needE300Calculation = false;
		private boolean needT50Calculation = false;
		private boolean needT90Calculation = false;
		private boolean needSubtypeCalculation = false;

		/**
		 * Calculate properties that depend upon other properties, such as T50, T90,
		 * E200, and E300.
		 * @throws IllegalArgumentException if there is insufficient data to do the calculations.
		**/
		public void calculateProperties() {
			if(e200 <= 0) {
				needE200Calculation = true;
			}
			if(e300 <= 0) {
				needE300Calculation = true;
			}
			if(T50 <= 0) {
				needT50Calculation = true;
			}
			if(T90 <= 0) {
				needT90Calculation = true;
			}
			if(needE200Calculation) {
				if(T50 > 0) {
					e200 = 147.91-(T50/2.0408163);
				} else {
					throw new IllegalArgumentException("Unable to calculate E200 from T50 because T50 is missing");
				}
				needE200Calculation = false;
				needT50Calculation = false;
			}
			if(needE300Calculation) {
				if(T90 > 0) {
					e300 = 155.47-(T90/4.5454545);
				} else {
					throw new IllegalArgumentException("Unable to calculate E300 from T90 because T90 is missing");
				}
				needE300Calculation = false;
				needT90Calculation = false;
			}
			if(needT50Calculation) {
				if(e200 > 0) {
					T50 = 2.0408163 * (147.91 - e200);
				} else {
					throw new IllegalArgumentException("Unable to calculate T50 from E200 because E200 is missing");
				}
				needE200Calculation = false;
				needT50Calculation = false;
			}
			if(needT90Calculation) {
				if(e300 > 0) {
					T90 = 4.5454545 * (155.47 - e300);
				} else {
					throw new IllegalArgumentException("Unable to calculate T90 from E300 because E300 is missing");
				}
				needE300Calculation = false;
				needT90Calculation = false;
			}
			if(needSubtypeCalculation) {
				needSubtypeCalculation = false;
				if(fuelSubtypeID <= 0 || fuelSubtypeID==10 || fuelSubtypeID==11 || fuelSubtypeID==12
						|| fuelSubtypeID==13 || fuelSubtypeID==14 || fuelSubtypeID==15
						|| fuelSubtypeID==51 || fuelSubtypeID==52 || fuelSubtypeID==18) {
					if(ETOHVolume < 0.10 && fuelSubtypeID != 11) {
						fuelSubtypeID = 10;
					}
					if(ETOHVolume >= 9 && ETOHVolume < 12.5) {
						fuelSubtypeID = 12;
					}
					if(ETOHVolume >= 6 && ETOHVolume < 9) {
						fuelSubtypeID = 13;
					}
					if(ETOHVolume >= 0.10 && ETOHVolume < 6) {
						fuelSubtypeID = 14;
					}
					if(ETOHVolume >= 12.5 && ETOHVolume < 17.5) {
						fuelSubtypeID = 15;
					}
					if(ETOHVolume >= 70.5 && ETOHVolume <= 100) {
						fuelSubtypeID = 51;
					}
					if(ETOHVolume >= 50.5 && ETOHVolume < 70.5) {
						fuelSubtypeID = 52;
					}
					if(ETOHVolume >= 17.5 && ETOHVolume < 50.5) {
						fuelSubtypeID = 18;
					}
				}
			}
		}

		/**
		 * Create a copy of the current formulation. All IDs are preserved.
		 * @return a copy of the current formulation, including the current formulation's ID.
		**/
		public FuelFormulation clone() {
			FuelFormulation result = new FuelFormulation();
			result.copyFrom(this);
			return result;
		}

		/**
		 * Set this instance's values to the values of another instance
		 * @param other instance to be read
		**/
		public void copyFrom(FuelFormulation other) {
			regionID = other.regionID;
			fuelYearID = other.fuelYearID;
			monthGroupID = other.monthGroupID;

			fuelFormulationID = other.fuelFormulationID;
			fuelTypeID = other.fuelTypeID;
			fuelSubtypeID = other.fuelSubtypeID;

			RVP = other.RVP;
			sulfurLevel = other.sulfurLevel;
			ETOHVolume = other.ETOHVolume;
			MTBEVolume = other.MTBEVolume;
			ETBEVolume = other.ETBEVolume;
			TAMEVolume = other.TAMEVolume;
			aromaticContent = other.aromaticContent;
			olefinContent = other.olefinContent;
			benzeneContent = other.benzeneContent;
			e200 = other.e200;
			e300 = other.e300;
			volToWtPercentOxy = other.volToWtPercentOxy;
			BioDieselEsterVolume = other.BioDieselEsterVolume;
			CetaneIndex = other.CetaneIndex;
			PAHContent = other.PAHContent;
			T50 = other.T50;
			T90 = other.T90;
		}

		/**
		 * Obtain the value of a fuel property.
		 * @param name name of a fuel property
		 * @return value of the fuel property
		 * @throws IllegalArgumentException if the name is unknown
		**/
		public double getProperty(String name) throws IllegalArgumentException {
			if(name.equalsIgnoreCase("RVP")) {
				return RVP;
			} else if(name.equalsIgnoreCase("Sulfur") || name.equalsIgnoreCase("SULF")) {
				return sulfurLevel;
			} else if(name.equalsIgnoreCase("Ethanol") || name.equalsIgnoreCase("ETOH")) {
				return ETOHVolume;
			} else if(name.equalsIgnoreCase("MTBE")) {
				return MTBEVolume;
			} else if(name.equalsIgnoreCase("ETBE")) {
				return ETBEVolume;
			} else if(name.equalsIgnoreCase("TAME")) {
				return TAMEVolume;
			} else if(name.equalsIgnoreCase("Aromatics") || name.equalsIgnoreCase("AROM")) {
				return aromaticContent;
			} else if(name.equalsIgnoreCase("Olefins") || name.equalsIgnoreCase("OLEF")) {
				return olefinContent;
			} else if(name.equalsIgnoreCase("Benzene") || name.equalsIgnoreCase("BENZ")) {
				return benzeneContent;
			} else if(name.equalsIgnoreCase("E200")) {
				return e200;
			} else if(name.equalsIgnoreCase("E300")) {
				return e300;
			} else if(name.equalsIgnoreCase("BioDiesel Ester") || name.equalsIgnoreCase("BIO")) {
				return BioDieselEsterVolume;
			} else if(name.equalsIgnoreCase("CetaneIndex") || name.equalsIgnoreCase("CETANE")) {
				return CetaneIndex;
			} else if(name.equalsIgnoreCase("PAHContent") || name.equalsIgnoreCase("PAH")) {
				return PAHContent;
			} else if(name.equalsIgnoreCase("T50")) {
				return T50;
			} else if(name.equalsIgnoreCase("T90")) {
				return T90;
			} else {
				throw new IllegalArgumentException("Unknown fuel property: " + name);
			}
		}

		/**
		 * Change the value of a fuel property.
		 * @param name name of a fuel property
		 * @param value new value for the property
		 * @throws IllegalArgumentException if the name is unknown or the value out of range.
		**/
		public void setProperty(String name, double value) throws IllegalArgumentException {
			if(name.equalsIgnoreCase("RVP")) {
				RVP = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("Sulfur") || name.equalsIgnoreCase("SULF")) {
				sulfurLevel = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("Ethanol") || name.equalsIgnoreCase("ETOH")) {
				ETOHVolume = value;
				needSubtypeCalculation = true;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("MTBE")) {
				MTBEVolume = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("ETBE")) {
				ETBEVolume = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("TAME")) {
				TAMEVolume = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("Aromatics") || name.equalsIgnoreCase("AROM")) {
				aromaticContent = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("Olefins") || name.equalsIgnoreCase("OLEF")) {
				olefinContent = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("Benzene") || name.equalsIgnoreCase("BENZ")) {
				benzeneContent = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("E200")) {
				e200 = value;
				needT50Calculation = true;
				needE200Calculation = false;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("E300")) {
				e300 = value;
				needT90Calculation = true;
				needE300Calculation = false;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("BioDiesel Ester") || name.equalsIgnoreCase("BIO")) {
				BioDieselEsterVolume = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("CetaneIndex") || name.equalsIgnoreCase("CETANE")) {
				CetaneIndex = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("PAHContent") || name.equalsIgnoreCase("PAH")) {
				PAHContent = value;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("T50")) {
				T50 = value;
				needT50Calculation = false;
				needE200Calculation = true;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else if(name.equalsIgnoreCase("T90")) {
				T90 = value;
				needT90Calculation = false;
				needE300Calculation = true;
				if(value < 0) {
					throw new IllegalArgumentException("Invalid " + name + " value: " + value);
				}
			} else {
				throw new IllegalArgumentException("Unknown fuel property: " + name);
			}
		}
	}

	/** A request to change a single property of one or more fuels **/
	public static class FuelChangeRequest {
		// Inputs
		public ArrayList<FuelFormulation> fuelsToChange = new ArrayList<FuelFormulation>();
		public String propertyNameToChange;
		public String units;
		public double targetValue;

		// Outputs
		public ArrayList<FuelFormulation> changedFuels = new ArrayList<FuelFormulation>();
		public ArrayList<String> messages = new ArrayList<String>();
		public boolean hasError = false;

		/** Clear all output variables **/
		public void resetOutput() {
			changedFuels.clear();
			messages.clear();
			hasError = false;
		}
	}

	/** A single factor from the fuelWizardFactors table **/
	static class FuelWizardFactor {
		public String propertyName;
		public double factor;
	}

	/** A row in the fuelWizardFactors table **/
	static class FuelWizardProperty {
		public String adjustedParameter;
		public String units;
		public double minLevel;
		public double maxLevel;
		public String functionType;
		public int monthGroupID;
		public int fuelTypeID;
		public ArrayList<FuelWizardFactor> factors = new ArrayList<FuelWizardFactor>();

		public void load(ResultSet rs) throws SQLException {
			// Example record:
			// adjustedParameter|minLevel|maxLevel|functionType|monthGroupID|fuelTypeID|RVP_factor|sulf_factor|ETOH_factor|arom_factor|olef_factor|benz_factor|e200_factor|e300_factor|T50_factor|T90_factor|units|dataSourceId
			// ETOH             |0       |10      |ADD         |7           |1         |1         |0          |0          |-2.58      |-0.87      |0          |3.46       |0.88       |-7.06     |-4        |%    |5101        
			adjustedParameter = rs.getString("adjustedParameter");
			minLevel = rs.getDouble("minLevel");
			maxLevel = rs.getDouble("maxLevel");
			functionType = rs.getString("functionType");
			monthGroupID = rs.getInt("monthGroupID");
			fuelTypeID = rs.getInt("fuelTypeID");
			String[] factorNames = {
				"RVP", "sulf", "ETOH", "arom", "olef", "benz", "T50", "T90" // "e200", "e300"
			};
			for(int i=0;i<factorNames.length;i++) {
				double v = rs.getDouble(factorNames[i] + "_factor");
				if(!rs.wasNull() && (v > 0 || v < 0)) {
					FuelWizardFactor f = new FuelWizardFactor();
					f.propertyName = factorNames[i];
					f.factor = v;
					factors.add(f);
				}
			}
			units = rs.getString("units");
			if(units.equalsIgnoreCase("C")) { // Convert C to F
				minLevel = (minLevel * 9.0/5.0) + 32.0;
				maxLevel = (maxLevel * 9.0/5.0) + 32.0;
			}
		}
	}

	/** Database that contains fuels and that should receive changed fuels. **/
	Connection db;
	/** True when using Nonroad fuel supply **/
	boolean isNonroad = false;
	/** Name of the table holding the fuel supply **/
	String fuelSupplyTable = "fuelSupply";

	/**
	 * Constructor
	 * @param dbToUse database that contains fuels and that should receive
	 * changed fuels.
	 * @param isNonroadToUse True when using Nonroad fuel supply
	**/
	public RefineryModel(Connection dbToUse, boolean isNonroadToUse) {
		db = dbToUse;
		isNonroad = isNonroadToUse;
		fuelSupplyTable = isNonroad? "nrFuelSupply" : "fuelSupply";
	}

	/**
	 * Load fuels and supply information from a database.
	 * @param fuels list to be populated
	**/
	public void loadFuels(ArrayList<FuelFormulation> fuels) {
		fuels.clear();
		
		String fuelSubtypeTable = isNonroad? "nrFuelSubtype" : "fuelSubtype";

		String defaultDatabaseName = SystemConfiguration.theSystemConfiguration.databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName ;
		String sql = "select fs.fuelRegionID, fs.fuelYearID, fs.monthGroupID,"
				+ " 	fs.fuelFormulationID, fst.fuelTypeID, ff.fuelSubtypeID,"
				+ " 	RVP,sulfurLevel,ETOHVolume,MTBEVolume,ETBEVolume,TAMEVolume,"
				+ " 	aromaticContent,olefinContent,benzeneContent,"
				+ " 	e200,e300,"
				+ " 	volToWtPercentOxy,BioDieselEsterVolume,"
				+ " 	CetaneIndex,PAHContent,"
				+ " 	T50,T90"
				+ " from " + fuelSupplyTable + " fs"
				+ " inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)"
				+ " inner join " + defaultDatabaseName + "." + fuelSubtypeTable + " fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)";
		//System.out.println(sql);
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				FuelFormulation f = new FuelFormulation();
				f.regionID = query.rs.getInt("fuelRegionID");
				f.fuelYearID = query.rs.getInt("fuelYearID");
				f.monthGroupID = query.rs.getInt("monthGroupID");
				f.fuelFormulationID = query.rs.getInt("fuelFormulationID");
				f.fuelTypeID = query.rs.getInt("fuelTypeID");
				f.fuelSubtypeID = query.rs.getInt("fuelSubtypeID");
				f.RVP = query.rs.getFloat("RVP");
				f.sulfurLevel = query.rs.getFloat("sulfurLevel");
				f.ETOHVolume = query.rs.getFloat("ETOHVolume");
				f.MTBEVolume = query.rs.getFloat("MTBEVolume");
				f.ETBEVolume = query.rs.getFloat("ETBEVolume");
				f.TAMEVolume = query.rs.getFloat("TAMEVolume");
				f.aromaticContent = query.rs.getFloat("aromaticContent");
				f.olefinContent = query.rs.getFloat("olefinContent");
				f.benzeneContent = query.rs.getFloat("benzeneContent");
				f.e200 = query.rs.getFloat("e200");
				f.e300 = query.rs.getFloat("e300");
				f.volToWtPercentOxy = query.rs.getFloat("volToWtPercentOxy");
				f.BioDieselEsterVolume = query.rs.getFloat("BioDieselEsterVolume");
				f.CetaneIndex = query.rs.getFloat("CetaneIndex");
				f.PAHContent = query.rs.getFloat("PAHContent");
				f.T50 = query.rs.getFloat("T50");
				f.T90 = query.rs.getFloat("T90");

				fuels.add(f);
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to read fuel supply and formulations");
		} finally {
			query.onFinally();
		}

		/*
		Random r = new Random();
		for(int i=0;i<150;i++) {
			FuelFormulation f = new FuelFormulation();
			f.regionID = 1 + i;
			f.aromaticContent = r.nextDouble();
			f.benzeneContent = r.nextDouble();
			f.BioDieselEsterVolume = r.nextDouble();
			f.CetaneIndex = r.nextDouble();
			f.e200 = 200;
			f.e300 = 300;
			f.ETBEVolume = 100 * r.nextDouble();
			f.fuelTypeID = 5;
			f.fuelYearID = 2013;
			f.monthGroupID = 9;
			f.RVP = r.nextDouble()*10;
			f.sulfurLevel = 30*r.nextDouble();
			fuels.add(f);
		}
		*/
	}

	/**
	 * Change a property of a set of fuels.
	 * @param request set of input fuels and property information. The request
	 * is updated to include the output fuels, warning/error messages, and overall
	 * status.
	**/
	public void changeFuels(FuelChangeRequest request) {
		// Clear output data
		request.resetOutput();
		// Make copies so the original fuels aren't changed, in case the user
		// does not want to commit the changes.
		for(FuelFormulation f : request.fuelsToChange) {
			request.changedFuels.add(f.clone());
		}
		// Find the name of the parameter to adjust.
		String adjustedParameter = "";
		for(int i=0;i<fuelPropertyNamesAndUnits.length;i+=3) {
			if(fuelPropertyNamesAndUnits[i+0].equalsIgnoreCase(request.propertyNameToChange)) {
				adjustedParameter = fuelPropertyNamesAndUnits[i+2];
				break;
			}
		}
		if(adjustedParameter == null || adjustedParameter.length() <= 0) {
			request.messages.add("ERROR: Unknown fuel property: " + request.propertyNameToChange);
			return;
		}
		// Standardize the units and value
		double targetValue = request.targetValue;
		if(request.units.equalsIgnoreCase("C")) { // Convert C to F
			targetValue = (targetValue * 9.0/5.0) + 32.0;
		}
		// Load fuel wizard properties for the adjusted parameter
		ArrayList<FuelWizardProperty> adjustments = loadFuelWizardProperties(
				DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT),
				adjustedParameter,request.messages);
		if(adjustments == null || adjustments.size() <= 0) {
			return;
		}
		// Modify the fuels in request.changedFuels.
		// Store any status messages into request.messages.
		for(FuelFormulation f : request.changedFuels) {
			try {
				updateFuel(f,adjustments,adjustedParameter,targetValue,request.messages);
				f.calculateProperties();
			} catch(IllegalArgumentException e) {
				request.messages.add(e.getMessage());
			}
		}
		if(request.messages.size() <= 0) {
			request.messages.add("Calculations complete.");
		}
	}

	/**
	 * Store fuel changes to the database.
	 * @param request results of a previous call to changeFuels()
	**/
	public void saveFuels(FuelChangeRequest request) {
		String sql = "";
		try {
			for(FuelFormulation f : request.changedFuels) {
				sql = "insert ignore into fuelFormulation (fuelFormulationID) values (" + f.fuelFormulationID + ")";
				SQLRunner.executeSQL(db,sql);
				sql = "update fuelFormulation set"
						+ " fuelSubtypeID=" + f.fuelSubtypeID
						+ " ,RVP= " + f.RVP
						+ " ,sulfurLevel= " + f.sulfurLevel
						+ " ,ETOHVolume= " + f.ETOHVolume
						+ " ,MTBEVolume= " + f.MTBEVolume
						+ " ,ETBEVolume= " + f.ETBEVolume
						+ " ,TAMEVolume= " + f.TAMEVolume
						+ " ,aromaticContent= " + f.aromaticContent
						+ " ,olefinContent= " + f.olefinContent
						+ " ,benzeneContent= " + f.benzeneContent
						+ " ,e200= " + f.e200
						+ " ,e300= " + f.e300
						+ " ,volToWtPercentOxy= " + f.volToWtPercentOxy
						+ " ,BioDieselEsterVolume= " + f.BioDieselEsterVolume
						+ " ,CetaneIndex= " + f.CetaneIndex
						+ " ,PAHContent= " + f.PAHContent
						+ " ,T50= " + f.T50
						+ " ,T90= " + f.T90
						+ " where fuelFormulationID=" + f.fuelFormulationID;
				//System.out.println(sql);
				SQLRunner.executeSQL(db,sql);
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to write new fuel using: " + sql);
		}
	}

	/**
	 * Read the fuelWizardFactors table.
	 * @param factorDB database holding the fuelWizardFactors table.
	 * @param adjustedParameter property to be changed.
	 * @param messages list of errors to show the user.
	 * @return non-empty list of properties. null upon any error or otherwise empty list.
	**/
	public ArrayList<FuelWizardProperty> loadFuelWizardProperties(
			Connection factorDB,
			String adjustedParameter, ArrayList<String> messages) {
		ArrayList<FuelWizardProperty> adjustments = new ArrayList<FuelWizardProperty>();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			sql = "select * from fuelWizardFactors where adjustedParameter=" + DatabaseUtilities.escapeSQL(adjustedParameter,true);
			query.open(factorDB,sql);
			while(query.rs.next()) {
				FuelWizardProperty p = new FuelWizardProperty();
				p.load(query.rs);
				adjustments.add(p);
			}
			if(adjustments.size() <= 0) {
				messages.add("ERROR: No fuel wizard adjustments available for " + adjustedParameter);
				return null;
			}
			return adjustments;
		} catch(SQLException e) {
			Logger.logError(e,"Unable to load fuel wizard properties using: " + sql);
			messages.add("ERROR: Unable to load fuel wizard properties.");
			return null;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Adjust the properties of a single fuel.
	 * @param f the fuel formulation to be altered
	 * @param adjustments all fuel wizard property records that apply to the adjusted parameter
	 * @param adjustedParameter the primary paramter being adjusted
	 * @param targetValue the desired final value of adjustedParameter
	 * @param messages error and information messages
	 * @throws IllegalArgumentException if any property or value is unacceptable
	**/
	public void updateFuel(FuelFormulation f, ArrayList<FuelWizardProperty> adjustments,
			String adjustedParameter, double targetValue, ArrayList<String> messages) throws IllegalArgumentException {
		boolean isDone = false;
		FuelFormulation backupF = f.clone();
		double currentValue = f.getProperty(adjustedParameter);
		double beginningValue = currentValue;
		boolean isIncreasing = targetValue >= currentValue;
		int jumpCount = 0;
		double extremeMinimum = 0, extremeMaximum = 0;
		boolean hasExtremes = false;
		for(FuelWizardProperty t : adjustments) {
			if(t.fuelTypeID == f.fuelTypeID && t.monthGroupID == f.monthGroupID) {
				if(!hasExtremes) {
					hasExtremes = true;
					extremeMaximum = t.maxLevel;
					extremeMinimum = t.minLevel;
				} else {
					extremeMaximum = Math.max(t.maxLevel,extremeMaximum);
					extremeMinimum = Math.min(t.minLevel,extremeMinimum);
				}
			}
		}
		if(!allowExtrapolation) {
			if(isIncreasing && targetValue > extremeMaximum) {
				//messages.add("ERROR: No fuel adjustment available changing " + adjustedParameter + " from " + beginningValue + " to " + targetValue + ", max. is " + extremeMaximum);
				messages.add("ERROR: The value you have specified (" + targetValue + ") is outside of the accepted range of " + extremeMinimum + " to " + extremeMaximum + ".");
				f.copyFrom(backupF);
				return;
			} else if(!isIncreasing && targetValue < extremeMinimum) {
				//messages.add("ERROR: No fuel adjustment available changing " + adjustedParameter + " from " + beginningValue + " to " + targetValue + ", min. is " + extremeMinimum);
				messages.add("ERROR: The value you have specified (" + targetValue + ") is outside of the accepted range of " + extremeMinimum + " to " + extremeMaximum + ".");
				f.copyFrom(backupF);
				return;
			}
		}
		while(!isDone) {
			currentValue = new BigDecimal(currentValue).round(new MathContext(6,RoundingMode.HALF_EVEN)).doubleValue();
			jumpCount++;
			if(jumpCount > 20) {
				messages.add("ERROR: Unable to converge upon target value");
				f.copyFrom(backupF);
				return;
			}
			// Find the applicable fuel wizard entry
			FuelWizardProperty p = null;
			FuelWizardProperty pAlt = null;
			for(FuelWizardProperty t : adjustments) {
				if(t.fuelTypeID == f.fuelTypeID && t.monthGroupID == f.monthGroupID) {
					//Logger.log(LogMessageCategory.DEBUG,"currentValue=" + currentValue);
					if(t.minLevel <= currentValue && t.maxLevel >= currentValue) {
						pAlt = t;
					}
					if(isIncreasing) {
						if(t.minLevel <= currentValue && (t.maxLevel > currentValue || (t.maxLevel == extremeMaximum && t.maxLevel >= currentValue))) {
							p = t;
							if(p.minLevel <= targetValue && (p.maxLevel > targetValue || (p.maxLevel == extremeMaximum && p.maxLevel <= targetValue))) {
								// The current property bounds the target value, so this is the last iteration.
								isDone = true;
								if(isTest) {
									Logger.log(LogMessageCategory.DEBUG,"End point found");
								}
							}
							if(isTest) {
								Logger.log(LogMessageCategory.DEBUG,"Using factors [" + p.minLevel + "," + p.maxLevel + ") for " + currentValue + " with target of " + targetValue);
							}
							break;
						}
					} else {
						if(t.maxLevel >= currentValue && (t.minLevel < currentValue || (t.minLevel == extremeMinimum && t.minLevel <= currentValue))) {
							p = t;
							if(p.maxLevel >= targetValue && (p.minLevel < targetValue || (p.minLevel == extremeMinimum && p.minLevel >= targetValue))) {
								// The current property bounds the target value, so this is the last iteration.
								isDone = true;
								if(isTest) {
									Logger.log(LogMessageCategory.DEBUG,"End point found");
								}
							}
							if(isTest) {
								Logger.log(LogMessageCategory.DEBUG,"Using factors (" + p.minLevel + "," + p.maxLevel + "] for " + currentValue + " with target of " + targetValue);
							}
							break;
						}
					}
				}
			}
			if(p == null && pAlt != null) {
				// There is no bin that strictly matches the current value.
				// This happens when the current value is at a bin edge for an extremal bin.
				// Use the extremal bin and declare the system done.
				if(allowExtrapolation) {
					p = pAlt;
					isDone = true;
					if(isTest) {
						Logger.log(LogMessageCategory.DEBUG,"Using extremal factors [" + p.minLevel + "," + p.maxLevel + "] for " + currentValue);
					}
				} else {
					//messages.add("ERROR: No fuel adjustment available changing " + adjustedParameter + " from " + beginningValue + " to " + targetValue);
					messages.add("ERROR: The value you have specified (" + targetValue + ") is outside of the accepted range of " + extremeMinimum + " to " + extremeMaximum + ".");
					f.copyFrom(backupF);
					return;
				}
			}
			if(p == null) {
				//messages.add("ERROR: No fuel adjustment available changing " + adjustedParameter + " from " + beginningValue + " to " + targetValue);
				messages.add("ERROR: The value you have specified (" + targetValue + ") is outside of the accepted range of " + extremeMinimum + " to " + extremeMaximum + ".");
				f.copyFrom(backupF);
				return;
			}
			// Determine the fraction of the bin that is used by the change.
			// When increasing, this is the fraction from the lowest bin edge to the target.
			// When decreasing, this is the fraction from the highest bin edge to the target.
			double fraction = 0;
			if(isIncreasing) {
				double low = Math.max(p.minLevel,currentValue);
				double high = Math.min(p.maxLevel,targetValue);
				fraction = (high - low) / (p.maxLevel - p.minLevel);
			} else {
				double high = Math.min(p.maxLevel,currentValue);
				double low = Math.max(p.minLevel,targetValue);
				fraction = (high - low) / (p.maxLevel - p.minLevel);
			}
			if(fraction > 0) {
				// Apply each factor
				for(FuelWizardFactor factor : p.factors) {
					double v = f.getProperty(factor.propertyName);
					if(p.functionType.equalsIgnoreCase("ADD")) {
						if(isIncreasing) {
							v += fraction * factor.factor;
							if(isTest) {
								Logger.log(LogMessageCategory.DEBUG,factor.propertyName + " = " + f.getProperty(factor.propertyName) + " + " + fraction + " * " + factor.factor + " = " + v);
							}
						} else {
							v -= fraction * factor.factor;
							if(isTest) {
								Logger.log(LogMessageCategory.DEBUG,factor.propertyName + " = " + f.getProperty(factor.propertyName) + " - " + fraction + " * " + factor.factor + " = " + v);
							}
						}
					} else if(p.functionType.equalsIgnoreCase("MULT")) {
						if(isIncreasing) {
							v *= 1 + (fraction * factor.factor);
							if(isTest) {
								Logger.log(LogMessageCategory.DEBUG,factor.propertyName + " = " + f.getProperty(factor.propertyName) + " * (1 + (" + fraction + " * " + factor.factor + ")) = " + v);
							}
						} else {
							v /= 1 + (fraction * factor.factor);
							if(isTest) {
								Logger.log(LogMessageCategory.DEBUG,factor.propertyName + " = " + f.getProperty(factor.propertyName) + " / (1 + (" + fraction + " * " + factor.factor + ")) = " + v);
							}
						}
					}
					f.setProperty(factor.propertyName,v);
				}
			}
			if(isDone) {
				currentValue = targetValue;
			} else if(isIncreasing) {
				currentValue = p.maxLevel;
			} else {
				currentValue = p.minLevel;
			}
			f.setProperty(adjustedParameter,currentValue);
			f.calculateProperties();
		}
	}
}
