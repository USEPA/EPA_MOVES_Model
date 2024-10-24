/**************************************************************************************************
 * @(#)RunSpec.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import java.sql.Connection;
import java.util.*;
import gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.*;

/**
 * A specification of all the parameters involved in a specific environmental effects test. This
 * includes a detailed metric specification (which vehicles, which pollutants, which locations) as
 * well as control strategies (incentives for certain vehicles, restrictions on others).
 *
 * @author		Wesley Faler
 * @version		2017-03-22
**/
public class RunSpec {
	/** Version of this RunSpec **/
	public String version = "";
	/** Textual description of this RunSpec **/
	public String description = "";
	/** The model is to be run. **/
	public Models models = new Models(Model.ONROAD);
	/** The scale that the model is to be run at. **/
	public ModelScale scale = ModelScale.MACROSCALE;
	/** The domain that the model is be run at. **/
	public ModelDomain domain = ModelDomain.NATIONAL_ALLOCATION;
	/** A generic county if the user elects to specify one for single county or project runs **/
	public GenericCounty genericCounty = null;
	/** GeographicSelection objects **/
	public TreeSet<GeographicSelection> geographicSelections = new TreeSet<GeographicSelection>();
	/** TimeSpan objects **/
	public TimeSpan timeSpan = new TimeSpan();
	/** Last calculated time span section status (-1=not calculated,0=not ready,1=Ok). **/
	public int timeSpanSectionStatus;
	/** OnRoadVehicleSelection objects **/
	public TreeSet<OnRoadVehicleSelection> onRoadVehicleSelections
			= new TreeSet<OnRoadVehicleSelection>();
	/** OffRoadVehicleSelection objects **/
	public TreeSet<OffRoadVehicleSelection> offRoadVehicleSelections
			= new TreeSet<OffRoadVehicleSelection>();
	/** List of SCC objects representing off road vehicle equipment. **/
	public TreeSet<SCC> offRoadVehicleSCCs = new TreeSet<SCC>();
	/** List of RoadType objects representing the selected road types. **/
	public TreeSet<RoadType> roadTypes = new TreeSet<RoadType>();
	/** PollutantProcessAssociation objects **/
	public TreeSet<PollutantProcessAssociation> pollutantProcessAssociations
			= new TreeSet<PollutantProcessAssociation>();
	/** DatabaseSelection objects **/
	public LinkedList<DatabaseSelection> databaseSelectionInputSets
			= new LinkedList<DatabaseSelection>();
	/**
	 * InternalControlStrategy objects, keyed by full package and class name,
	 * data is a LinkedList of instances.  The use of a LinkedList preserves the
	 * order in the GUI of the objects.
	**/
	public TreeMap<String,LinkedList<InternalControlStrategy> > internalControlStrategies
			= new TreeMap<String,LinkedList<InternalControlStrategy> >();
	/** Input Database **/
	public DatabaseSelection inputDatabase = new DatabaseSelection();
	/** Uncertainty parameter information **/
	public UncertaintyParameters uncertaintyParameters = new UncertaintyParameters();
	/** Specifies the desired geographic output detail. **/
	public GeographicOutputDetailLevel geographicOutputDetail = null;
	/** Data from the user's selections on the Output Emissions Breakdown GUI panel. **/
	public OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
			new OutputEmissionsBreakdownSelection();
	/** The database to output data to. **/
	public DatabaseSelection outputDatabase = null;
	/** The user's time step selection **/
	public OutputTimeStep outputTimeStep;
	/** Should CAFE data be output from the simulation. **/
	public boolean outputVMTData = false;
	/** SHO output **/
	public boolean outputSHO = false;
	/** SH output **/
	public boolean outputSH = false;
	/** SHP output **/
	public boolean outputSHP = false;
	/** SH output **/
	public boolean outputSHIdling = false;
	/** Starts output **/
	public boolean outputStarts = false;
	/** Population output **/
	public boolean outputPopulation = false;
	/** The desired hydrocarbon unit system. **/
//	public HydrocarbonUnitSystem hydrocarbonUnitSystem = null;
	/** The input database for mesoscale/microscale data. **/
	public DatabaseSelection scaleInputDatabase = new DatabaseSelection();
	/** PM (Particulate Matter) Size **/
	public int pmSize;
	/** Time, distance, and emission factors **/
	public OutputFactors outputFactors = new OutputFactors();
	/** Names of classes not to be executed **/
	public TreeSetIgnoreCase classesNotToExecute = new TreeSetIgnoreCase();
	/** Names of classes that should save their intermediate data **/
	public TreeSetIgnoreCase classesToSaveData = new TreeSetIgnoreCase();
	/** true if the Advanced Performance Features should preserve data **/
	public boolean shouldCopySavedGeneratorData = false;
	/** The data to save generator data to as part of the Advanced Performance Features **/
	public DatabaseSelection generatorDatabase = null;
	/** true when final aggregation should be skipped **/
	public boolean doNotPerformFinalAggregation = false;
	/** MOVESScenarioID used by emission rate ("Mesoscale") mode **/
	public String scenarioID = "";
	/**
	 * Used by emission rate ("Mesoscale") mode to control data removal from
	 * the MOVESOutput table.
	**/
	public boolean shouldTruncateMOVESOutput = true;
	/**
	 * Used by emission rate ("Mesoscale") mode to control data removal from
	 * the MOVESActivityOutput table.
	**/
	public boolean shouldTruncateMOVESActivityOutput = true;
	/**
	 * Used by emission rate ("Mesoscale") mode to control data removal from
	 * the BaseRateOutput table.
	**/
	public boolean shouldTruncateBaseRateOutput = true;
    /**
     * Used to bypass the feature that prevents users from running if CreateInputDatabase has a red X 
     */
    public boolean skipDomainDatabaseValidation = false;
	/**
	 * Used by emission rate mode to provide separate rates for ramps.
	 * Note: This feature has been removed, so if an old runspec is loaded with
	 * this option, notify the user that they need to re-save the runspec.
	**/
	//public boolean shouldSeparateRamps = false;
	public boolean hasDeprecatedShouldSeparateRampsTrue = false;
	/**
	 * Intercity Buses were changed to Other Buses in MOVES3. RunSpecXML converts
	 * the old name to the new name during load and sets this flag to warn the user
	 * when this happens.
	**/
	public boolean hadIntercityBuses = false;

	/** Constructor **/
	public RunSpec() {
		outputDatabase = new DatabaseSelection();
		outputDatabase.serverName = new String("");
		outputDatabase.databaseName = new String("");
		outputDatabase.userName = new String(
				SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].SERVER_USER_NAME);
		outputDatabase.password = new String(
				SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].SERVER_PASSWORD);

		inputDatabase = new DatabaseSelection();
		inputDatabase.serverName = new String("");
		inputDatabase.databaseName = new String("");
		inputDatabase.userName = new String(
				SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].SERVER_USER_NAME);
		inputDatabase.password = new String(
				SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].SERVER_PASSWORD);

		generatorDatabase = new DatabaseSelection();
		generatorDatabase.serverName = new String("");
		generatorDatabase.databaseName = new String("");
		generatorDatabase.userName = new String(
				SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].SERVER_USER_NAME);
		generatorDatabase.password = new String(
				SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].SERVER_PASSWORD);
	}

	/**
	 * Examine the scale and domain, determining if the custom domain within genericCounty
	 * should be used.
	 * @return true if genericCounty is a county that should be used
	**/
	public boolean isCustomDomain() {
		if(domain != null && domain != ModelDomain.NATIONAL_ALLOCATION && genericCounty != null
				&& geographicSelections.size() == 1) {
			Object firstElement = geographicSelections.iterator().next();
			GeographicSelection firstGS = (GeographicSelection) firstElement;
			if(firstGS.type == GeographicSelectionType.COUNTY
					&& firstGS.databaseKey == genericCounty.getCountyID()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Obtain the county ID from the first county-level geographic selection
	 * for county and project domains.
	 * @return county ID or 0 if the domain is not county or project, or if there
	 * is no county specified.
	**/
	public int getCountyID() {
		if(domain != null && domain != ModelDomain.NATIONAL_ALLOCATION
				&& geographicSelections.size() == 1) {
			Object firstElement = geographicSelections.iterator().next();
			GeographicSelection firstGS = (GeographicSelection) firstElement;
			if(firstGS.type == GeographicSelectionType.COUNTY) {
				return firstGS.databaseKey;
			}
		}
		return 0;
	}

	/**
	 * Check all InternalControlStrategy objects looking for deprecated objects.  These
	 * objects rely upon the importer mechanism for their data and are created automatically
	 * during runtime based upon data presence instead of being stored permanently within
	 * a RunSpec.
	 * @return true if any instantiated InternalControlStrategy implements the
	 * InternalControlStrategyUseImporterOnly interface.
	**/
	public boolean hasDeprecatedStrategies() {
		Set<String> keys = internalControlStrategies.keySet();
		for(Iterator<String> i=keys.iterator();i.hasNext();) {
			String className = i.next();
			LinkedList<InternalControlStrategy> s = internalControlStrategies.get(className);
			if(s != null && s.size() > 0) {
				InternalControlStrategy ics = s.get(0);
				if(ics instanceof InternalControlStrategyUseImporterOnly) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Checks for instantiated AVFT objects.
	 * @return true if the RunSpec holds an instantiated AVFT object.
	**/
	public boolean hasAVFT() {
		String className = "gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft.AVFTControlStrategy";
		return hasInternalControlStrategy(className);
	}

	/**
	 * Checks for instantiated Onroad Retrofit objects.
	 * @return true if the RunSpec holds an instantiated Onroad Retrofit object.
	**/
	public boolean hasOnRoadRetrofit() {
		String className = "gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy";
		return hasInternalControlStrategy(className);
	}

	/**
	 * Check for instantiated InternalControlStrategy.
	 * @param className full class name of the InternalControlStrategy to be checked.
	 * @return true if the RunSpec holds an instantiated InternalControlStrategy of the type requested.
	**/
	public boolean hasInternalControlStrategy(String className) {
		LinkedList<InternalControlStrategy> s = internalControlStrategies.get(className);
		return (s != null && s.size() > 0);
	}

	/**
	 * Checks for instantiated and enabled Rate of Progress
	 * @return true if the RunSpec requires rate of progress
	**/
	public boolean hasRateOfProgress() {
		String className = "gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.RateOfProgressStrategy";
		LinkedList<InternalControlStrategy> sl = internalControlStrategies.get(className);
		if(sl == null || sl.size() <= 0) {
			return false;
		}
		InternalControlStrategy s = sl.get(0);
		if(s != null && s instanceof RateOfProgressStrategy) {
			RateOfProgressStrategy r = (RateOfProgressStrategy)s;
			return r.useParameters;
		}
		return false;
	}

	/**
	 * get model combination - what models selected
	 * @return Models.ModelCombination
	**/
	public Models.ModelCombination getModelCombination() {
		return Models.evaluateModels( this.models);
	}
	
	/**
	 * filter road types based on model selected
	 * @return void
	**/
	public void filterRoadType() {
		Models.ModelCombination mc = getModelCombination();
		switch ( mc) {
		case M1:
			
			break;
		case M2:
			//
			break;
		default:
			break;
		}
	}

	/**
	 * Check for use of Evap processes (11,12,13) in rates mode.
	 * return true if any pol/process is for process 11, 12, or 13
	 * when in rates mode.
	**/
	public boolean usesEvapRates() {
		if(scale != ModelScale.MESOSCALE_LOOKUP) {
			return false;
		}
		for(PollutantProcessAssociation a : pollutantProcessAssociations) {
			if(a.emissionProcess != null) {
				if(a.emissionProcess.databaseKey == 11
						|| a.emissionProcess.databaseKey == 12
						|| a.emissionProcess.databaseKey == 13) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * internal function to parse MOVES version strings
	**/
	private String getMajorVersionString(String v) {
		try {
			// Assume version numbers are MOVESX.Y.Z-other
			String[] hypenSplit = v.split("-");            // first element contains the MOVESX.Y.Z component, second (et al) element(s) contains the "other" info
			String[] digits = hypenSplit[0].split("\\.");  // elements contain MOVESX, Y, Z, respectively
			return digits[0];
		} catch (Exception e){
			// default to full version string
			return v;
		}
	}
	
	public String getMajorVersionString() {
		return getMajorVersionString(version);
	}
	
	/**
	 * returns true if the other string has the same "Major" component as this RunSpec
	**/
	public boolean isSameMajorVersion(String other) {
		return getMajorVersionString(version).equalsIgnoreCase(getMajorVersionString(other));
	}
	
	/**
	 * returns true if the other string has a compatible "Major" component as this RunSpec
	**/
	public boolean isCompatibleVersion(String other) {
        // always compatible if we are the same major version
        if (isSameMajorVersion(other)) {
            return true;
        }

        // go through list of compatible major versions
        // Note: "version" is RunSpec version, "other" is current model version
        if (getMajorVersionString(version).equalsIgnoreCase("MOVES4") && getMajorVersionString(other).equalsIgnoreCase("MOVES5")) {
            return true;
        }
		return false;
	}
	
	/**
	 * internal function to parse MOVES version strings
	**/
	private int getMajorVersionInt(String v) {
		try {
			// Assume version numbers are MOVESX.Y.Z-other
			String[] hypenSplit = v.substring(5).split("-"); // first element contains the X.Y.Z component, second (et al) element(s) contains the "other" info
			String[] digits = hypenSplit[0].split("\\.");    // elements contain X, Y, Z, respectively
			return Integer.parseInt(digits[0]);
		} catch (Exception e){
			// default to 2014
			return 2014;
		}
	}
	
	public int getMajorVersionInt() {
		return getMajorVersionInt(version);
	}
	
	/**
	 * returns -1 if the passed version is older than the version of this runspec, 0 if they are the same, and 1 if the passed version is newer
	**/
	public int compareMajorVersion(String other) {
		// get integer representations of the major versions
		int otherInt = getMajorVersionInt(other);
		int thisInt = getMajorVersionInt();
		
		// for comparison purposes, treat 2014 like 2 (since it is older than MOVES3)
		if (otherInt == 2014) {
			otherInt = 2;
		}
		if (thisInt == 2014) {
			thisInt = 2;
		}
		
		// perform comparison
		if (otherInt == thisInt) {
			return 0;
		} else if (otherInt < thisInt) {
			return -1;
		} else {
			return 1;
		}
	}
}
