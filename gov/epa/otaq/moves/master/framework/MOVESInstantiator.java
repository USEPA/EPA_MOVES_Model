 /**************************************************************************************************
 * @(#)MOVESInstantiator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.RunSpec;
import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft.AVFTControlStrategy;
import gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy;

/**
 * This utility class examines ExecutionRunSpec and creates necessary MasterLoopable objects,
 * EmissionCalculator objects, and IntegratedPostProcessor objects.
 *
 * @author		Wesley Faler
 * @author		Gwo Shyu, EPA (task 216; 2009/10/16 EPA NH3 task)
 * @author		Mitch Cumberworth, EPA
 * @author		Ed Glover, EPA
 * @author		William Aikman EPA
 * @version		2017-07-04
**/
public class MOVESInstantiator {
	/** list of classes that support advanced performance features **/
	public static String[] advancedPerformanceClasses = {
		"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
		//"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.RatesOperatingModeDistributionGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
	    "gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
		//"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupTotalActivityGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator",
		"gov.epa.otaq.moves.master.framework.EmissionCalculator",
		"gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy",
		"gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG",
		"gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator",
		"gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.RateOfProgressStrategy"
	};

	/** list of classes that do their own handling of advanced performance features **/
	private static String[] specialCaseAdvancedPerformanceClasses = {
		"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator"
	};
	/** set of class name that do their own handling of advanced performance features **/
	private static TreeSetIgnoreCase specialCaseAdvancedPerformanceSet = null;

	/**
	 * list of class names and human-readable description pairs, containing at least the
	 * classes within advancedPerformanceClasses.
	**/
	private static String[] classNamesAndHumanNames = {
		"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			"Total Activity Generator (TAG)",
		"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator",
			"Operating Mode Distribution Generator (running OMDG)",
		"gov.epa.otaq.moves.master.implementation.ghg.RatesOperatingModeDistributionGenerator",
			"Rates Operating Mode Distribution Generator (running ROMDG)",
		"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Start Operating Mode Distribution Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
			"Evaporative Operating Mode Distribution Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator",
			"Tirewear Operating Mode Distribution Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			"Source Bin Distribution Generator (SBDG)",
	    "gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
	    	"Meteorology Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
			"Tank Temperature Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator",
			"Tank Fuel Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
			"Fuel Effects Generator",
		"gov.epa.otaq.moves.master.framework.EmissionCalculator",
			"Emission Calculators",
		//"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupTotalActivityGenerator",
		//	"Lookup Total Activity Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator",
			"Lookup Operating Mode Distribution Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy",
			"On-Road Retrofit",
		"gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG",
			"Project-Domain Total Activity Generator",
		"gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator",
			"Project-Domain Operating Mode Distribution Generator (running exhaust)",
		"gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.RateOfProgressStrategy",
			"Rate Of Progress Strategy"
	};

	/** cross reference of class names keyed by their human-readable names **/
	private static TreeMapIgnoreCase classNameByHumanName = null;
	/** cross reference of human-readable names keyed by their class names **/
	private static TreeMapIgnoreCase humanNameByClassName = null;

	/** Populates classNameByHumanName and humanNameByClassName **/
	private static void setupClassNameCrossReference() {
		if(classNameByHumanName != null && humanNameByClassName != null
				&& specialCaseAdvancedPerformanceSet != null) {
			return; // previously completed
		}
		classNameByHumanName = new TreeMapIgnoreCase();
		humanNameByClassName = new TreeMapIgnoreCase();
		for(int i=0;i<classNamesAndHumanNames.length-1;i+=2) {
			String c = classNamesAndHumanNames[i+0];
			String h = classNamesAndHumanNames[i+1];
			classNameByHumanName.put(h,c);
			humanNameByClassName.put(c,h);
		}

		specialCaseAdvancedPerformanceSet = new TreeSetIgnoreCase();
		for(int i=0;i<specialCaseAdvancedPerformanceClasses.length;i++) {
			specialCaseAdvancedPerformanceSet.add(specialCaseAdvancedPerformanceClasses[i]);
		}
	}

	/**
	 * Obtain the human-readable name of a class.
	 * @param className full name of a class, including its package
	 * @return the human-readable name of the class or the class name if the class is unknown
	**/
	public static String getHumanName(String className) {
		setupClassNameCrossReference();
		String result = (String)humanNameByClassName.get(className);
		if(result == null) {
			result = className;
		}
		return result;
	}

	/**
	 * Obtain the class name from a human-readable description.
	 * @param humanName human-readable description for a class
	 * @return the class name, including package name, or the human-name if the class is unknown
	**/
	public static String getClassName(String humanName) {
		setupClassNameCrossReference();
		String result = (String)classNameByHumanName.get(humanName);
		if(result == null) {
			result = humanName;
		}
		return result;
	}

	/**
	 * Check if an object is of a type that does its own cleanup for the advanced performance
	 * features.
	 * @param o object to be inspected
	 * @return true if the object's class, or any superclass, does it down special case cleanup.
	**/
	public static boolean isSpecialCaseForCleanup(Object o) {
		setupClassNameCrossReference();
		Class c = o.getClass();
		while(c != null) {
			if(specialCaseAdvancedPerformanceSet.contains(c.toString())) {
				return true;
			}
			c = c.getSuperclass();
		}
		return false;
	}

	/**
	 * Set of distinct classes that must be instantiated for the system to work as required
	 * by the runspec.  Each is a String with full package and class name.  The values herein
	 * are only useful during the execution of the performInstantiation(...) function, which
	 * calls MasterLoopable.subscribeToMe(...) which in turn may call alsoInstantiate(...)
	 * which affects this collection and performInstantiation(...)'s behavior.  This collection
	 * is created and destroyed by performInstantiation(...).
	**/
	private static TreeSet<String> neededClassNames = null;

	/**
	 * Set of MasterLoopable objects instantiated by performInstantiation(...).
	 * Each is keyed by its full package and class name.  This collection exists only after
	 * performInstantiation(...) has been called.
	**/
	private static TreeMap<String,MasterLoopable> createdObjects = null;

	/**
	 * This code builds all Generator objects and the EmissionCalculator. This code understands all
	 * dependencies and builds the objects differently based on the simulation scale (i.e. macro,
	 * meso, micro).
	 * @param targetRunSpec Specifies simulation parameters which indicate which objects should
	 * be instantiated.
	 * @param targetLoop The MasterLoop that the Generator's and EmissionCalculator's should be
	 * instantiated for.
	**/
	public static void performInstantiation(ExecutionRunSpec targetRunSpec,MasterLoop targetLoop) {
		boolean isMesoscaleLookup = targetRunSpec.getModelScale() == ModelScale.MESOSCALE_LOOKUP;
		boolean isProjectDomain = targetRunSpec.getModelDomain() == ModelDomain.PROJECT;
		RunSpec runSpec = ExecutionRunSpec.getRunSpec();
		Models.ModelCombination mc = runSpec.getModelCombination();

		if(isMesoscaleLookup) {
			OutputProcessor.getTheOutputProcessor().integratedPostProcessors.add(
					new MesoscaleLookupPostProcessor());
		}
		neededClassNames = new TreeSet<String>();
		EmissionCalculatorRegistration.reset();
		Pollutant equivalentCO2 = Pollutant.findByID(98);
		Pollutant sulfatePM10 = Pollutant.findByID(105);
		Pollutant sulfatePM25 = Pollutant.findByID(115);
		Pollutant PM25OrganicCarbon = Pollutant.findByID(111);
		Pollutant PM25ElementalCarbon = Pollutant.findByID(112);
		Pollutant so2 = Pollutant.findByID(31);
		Pollutant nO = Pollutant.findByID(32);
		Pollutant nO2 = Pollutant.findByID(33);
		Pollutant hono = Pollutant.findByID(34);
		Pollutant nH3 = Pollutant.findByID(30);

		String equivCO2PolName = ((equivalentCO2==null)?"NonExistent":equivalentCO2.pollutantName);
		String sulfatePM10Name = ((sulfatePM10==null)?"Non-Existent":sulfatePM10.pollutantName);
		String sulfatePM25Name = ((sulfatePM25==null)?"Non-Existent":sulfatePM25.pollutantName);
		String PM25OrganicCarbonName = ((PM25OrganicCarbon==null)?"NonExistant":PM25OrganicCarbon.pollutantName);
		String PM25ElementalCarbonName = ((PM25ElementalCarbon==null)?"NonExistant":PM25ElementalCarbon.pollutantName);
		String so2Name = ((so2==null)?"Non-Existent":so2.pollutantName);
		String nOName = ((nO==null)?"Non-Existent":nO.pollutantName);
		String nO2Name = ((nO2==null)?"Non-Existent":nO2.pollutantName);
		String nH3Name = ((nH3==null)?"Non-Existent":nH3.pollutantName);
		String honoName = ((hono==null)?"Non-Existent":hono.pollutantName);

		// Table of tuplets { pollutant name, emission process name, class name }
		// Pollutant name or emission process name may be null, but noth both.
		// Class name should never be null (though the system tolerates this, it is a
		// nonsensical case).
		String pollutantsProcessesAndNeeds[] = null;
		String pollutantsProcessesAndNeedsOnroad[] = {
			null, "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator",
			null, "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			null, "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
			null, "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
			"Total Energy Consumption","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.EnergyConsumptionCalculator",
			"Methane (CH4)", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Nitrous Oxide (N2O)", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CH4N2ORunningStartCalculator",
			"Atmospheric CO2","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			equivCO2PolName,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			"Total Gaseous Hydrocarbons","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaRunningCalculator",
			"Carbon Monoxide (CO)","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaRunningCalculator",
			"Oxides of Nitrogen (NOx)","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaRunningCalculator",
			"Composite - NonECPM","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.BasicRunningPMEmissionCalculator",
			"Elemental Carbon","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.BasicRunningPMEmissionCalculator",
			sulfatePM10Name,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			sulfatePM25Name,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Organic Carbon","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Elemental Carbon","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Composite - NonECPM","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary Exhaust PM2.5 - Total", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary PM10 - Elemental Carbon", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary PM10 - Organic Carbon", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM10  - Total", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			so2Name,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SO2Calculator",
			nOName,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			honoName,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			nO2Name,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NO2Calculator",
			nH3Name,"Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NH3RunningCalculator",
			null, "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator",
			null, "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			null, "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
			null, "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
			null, "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Total Energy Consumption","Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.EnergyConsumptionCalculator",
			"Methane (CH4)", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Nitrous Oxide (N2O)", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CH4N2ORunningStartCalculator",
			"Atmospheric CO2","Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			equivCO2PolName,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			"Total Gaseous Hydrocarbons", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaStartCalculator",
			"Carbon Monoxide (CO)", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaStartCalculator",
			"Oxides of Nitrogen (NOx)", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaStartCalculator",
			"Total Energy Consumption","Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Total Gaseous Hydrocarbons", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Carbon Monoxide (CO)", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Oxides of Nitrogen (NOx)", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Nitrous Oxide (N2O)", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Composite - NonECPM", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Composite - NonECPM", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.BasicStartPMEmissionCalculator",
			"Elemental Carbon", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
			"Elemental Carbon", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.BasicStartPMEmissionCalculator",
			sulfatePM10Name,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			sulfatePM25Name,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Organic Carbon", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Elemental Carbon", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Composite - NonECPM", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary Exhaust PM2.5 - Total", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary PM10 - Elemental Carbon", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary PM10 - Organic Carbon", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM10  - Total", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			so2Name,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SO2Calculator",
			nOName,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			honoName,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			nO2Name,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NO2Calculator",
			nH3Name, "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NH3StartCalculator",
			nH3Name,"Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
            null, "Extended Idle Exhaust",
                "gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
			null, "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator",
			null, "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			null, "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
		    "Total Gaseous Hydrocarbons","Extended Idle Exhaust",
                "gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMExtendedIdleEmissionCalculator",
            "Carbon Monoxide (CO)","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMExtendedIdleEmissionCalculator",
			"Oxides of Nitrogen (NOx)","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMExtendedIdleEmissionCalculator",
			"Total Energy Consumption","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.EnergyConsumptionCalculator",
			"Atmospheric CO2","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			equivCO2PolName,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			"Composite - NonECPM","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMExtendedIdleEmissionCalculator",
			PM25ElementalCarbonName,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMExtendedIdleEmissionCalculator",
			sulfatePM10Name,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			sulfatePM25Name,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			PM25OrganicCarbonName,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			PM25ElementalCarbonName,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Composite - NonECPM","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary Exhaust PM2.5 - Total", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary PM10 - Elemental Carbon", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary PM10 - Organic Carbon", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM10  - Total", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Methane (CH4)", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			so2Name,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SO2Calculator",
			nOName,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			honoName,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			nO2Name,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NO2Calculator",
			nH3Name,"Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NH3ExtendedIdleEmissionCalculator",
//			"Nitrous Oxide (N2O)", "Extended Idle Exhaust",
//TODO			"gov.epa.otaq.moves.master.implementation.ghg.CH4N2ORunningStartCalculator",

//			null, "Manufacture/Disposal", null

			/* !!! Gwo Shyu 12/03/2009 fix an error due to well-to-Pump does exists in MOVES for now: change starts here
			"Total Energy Consumption","Well-to-Pump",
				"gov.epa.otaq.moves.master.implementation.ghg.WellToPumpProcessor",
			"Methane (CH4)", "Well-to-Pump",
				"gov.epa.otaq.moves.master.implementation.ghg.CH4N2OWTPCalculator",
			"Nitrous Oxide (N2O)", "Well-to-Pump",
				"gov.epa.otaq.moves.master.implementation.ghg.CH4N2OWTPCalculator",
			"Atmospheric CO2","Well-to-Pump",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AtmosphericWTPCalculator",
			equivCO2PolName,"Well-to-Pump",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2EqivalentWTPCalculator",

			  !!! Change ends
			*/

			null, "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
			null, "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
			null, "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			null, "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.EvaporativePermeationCalculator",
			null, "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",

			null, "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
			null, "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator",
			null, "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
			null, "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			null, "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.TankVaporVentingCalculator",
			null, "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",

			null, "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
			null, "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator",
			null, "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
			null, "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			null, "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.LiquidLeakingCalculator",
			null, "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",

			null, "Evap Non-Fuel Vapors",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Evap Non-Fuel Vapors",
				"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
			null, "Evap Non-Fuel Vapors",
				"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
			null, "Evap Non-Fuel Vapors",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			null, "Evap Non-Fuel Vapors",
				"gov.epa.otaq.moves.master.implementation.ghg.DummyCalculator",

			null, "Brakewear",
				"gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
			null, "Brakewear",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Brakewear",
				"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator",
			null, "Brakewear",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			"Primary PM2.5 - Brakewear Particulate", "Brakewear",
				"gov.epa.otaq.moves.master.implementation.ghg.BasicBrakeWearPMEmissionCalculator",
			"Primary PM10 - Brakewear Particulate", "Brakewear",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10BrakeTireCalculator",

			null, "Tirewear",
				"gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
			null, "Tirewear",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Tirewear",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
			"Primary PM2.5 - Tirewear Particulate", "Tirewear",
				"gov.epa.otaq.moves.master.implementation.ghg.BasicTireWearPMEmissionCalculator",
			"Primary PM2.5 - Tirewear Particulate", "Tirewear",
				"gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator",
			"Primary PM10 - Tirewear Particulate", "Tirewear",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10BrakeTireCalculator",

			"Total Gaseous Hydrocarbons","Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.RefuelingLossCalculator",
			null, "Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
			"Total Gaseous Hydrocarbons","Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.RefuelingLossCalculator",
			null, "Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",

			"Primary PM10 - Elemental Carbon", "Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary PM10 - Organic Carbon", "Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM10  - Total", "Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM2.5 - Total", "Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"1","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"2","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"3","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"5","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"6","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"20","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"21","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"22","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"23","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"24","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"25","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"26","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"27","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"30","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"31","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"32","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"33","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"34","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"79","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"80","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"86","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"87","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"105","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"118","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"112","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"115","Crankcase Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",

			"Primary PM10 - Elemental Carbon", "Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary PM10 - Organic Carbon", "Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM10  - Total", "Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM2.5 - Total", "Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"1","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"2","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"3","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"5","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"6","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"20","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"21","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"22","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"23","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"24","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"25","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"26","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"27","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"30","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"31","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"32","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"33","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"34","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"79","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"80","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"86","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"87","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"105","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"118","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"112","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"115","Crankcase Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",

			"Primary PM10 - Elemental Carbon", "Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary PM10 - Organic Carbon", "Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM10  - Total", "Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM2.5 - Total", "Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"1","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"2","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"3","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"5","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"6","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"20","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"21","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"22","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"23","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"24","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"25","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"26","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"27","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"30","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"31","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"32","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"33","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"34","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"79","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"80","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"86","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"87","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"105","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"118","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"112","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"115","Crankcase Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",

			// Air Toxics, grouped here because it is known that many more will be added soon
			"Benzene", "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"1,3-Butadiene", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acetaldehyde", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acrolein", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Formaldehyde", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"1,3-Butadiene", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acetaldehyde", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acrolein", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Formaldehyde", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"1,3-Butadiene", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acetaldehyde", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acrolein", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Formaldehyde", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",

			// HC Speciation
			"Non-Methane Hydrocarbons","Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Evap Fuel Leaks",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Hydrocarbons","Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Evap Fuel Vapor Venting",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Hydrocarbons","Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Evap Permeation",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Hydrocarbons","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Extended Idle Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Hydrocarbons","Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Refueling Displacement Vapor Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Hydrocarbons","Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Refueling Spillage Loss",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Hydrocarbons","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Running Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Hydrocarbons","Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Start Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",

			// More air toxics
			"1,2,3,4,6,7,8-Heptachlorodibenzo-p-Dioxin","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,4,6,7,8-Heptachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,4,7,8,9-Heptachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,4,7,8-Hexachlorodibenzo-p-Dioxin","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,4,7,8-Hexachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,6,7,8-Hexachlorodibenzo-p-Dioxin","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,6,7,8-Hexachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,7,8,9-Hexachlorodibenzo-p-Dioxin","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,7,8,9-Hexachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,7,8-Pentachlorodibenzo-p-Dioxin","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"1,2,3,7,8-Pentachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"2,3,4,6,7,8-Hexachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"2,3,4,7,8-Pentachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"2,3,7,8-Tetrachlorodibenzo-p-Dioxin","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"2,3,7,8-Tetrachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Arsenic Compounds","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Chromium 3+","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Chromium 6+","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Manganese Compounds","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Mercury Divalent Gaseous","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Mercury Elemental Gaseous","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Mercury Particulate","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Nickel Compounds","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Octachlorodibenzo-p-dioxin","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",
			"Octachlorodibenzofuran","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsDistanceCalculator",

			"2,2,4-Trimethylpentane","Evap Fuel Leaks","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Evap Fuel Leaks","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Evap Fuel Leaks","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Evap Fuel Leaks","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Evap Fuel Leaks","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Evap Fuel Leaks","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"2,2,4-Trimethylpentane","Evap Fuel Vapor Venting","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Evap Fuel Vapor Venting","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Evap Fuel Vapor Venting","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Evap Fuel Vapor Venting","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Evap Fuel Vapor Venting","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Evap Fuel Vapor Venting","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"2,2,4-Trimethylpentane","Evap Permeation","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Evap Permeation","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Evap Permeation","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Evap Permeation","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Evap Permeation","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Evap Permeation","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"2,2,4-Trimethylpentane","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthylene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Anthracene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benz(a)anthracene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(a)pyrene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(b)fluoranthene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(g,h,i)perylene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(k)fluoranthene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Chrysene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Dibenzo(a,h)anthracene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluoranthene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluorene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Indeno(1,2,3,c,d)pyrene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Phenanthrene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Propionaldehyde","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Pyrene gas","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Styrene","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"2,2,4-Trimethylpentane","Refueling Displacement Vapor Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Refueling Displacement Vapor Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Refueling Displacement Vapor Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Refueling Displacement Vapor Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Refueling Displacement Vapor Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Refueling Displacement Vapor Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"2,2,4-Trimethylpentane","Refueling Spillage Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Refueling Spillage Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Refueling Spillage Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Refueling Spillage Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Refueling Spillage Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Refueling Spillage Loss","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"2,2,4-Trimethylpentane","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthylene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthylene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Anthracene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Anthracene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benz(a)anthracene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benz(a)anthracene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(a)pyrene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(a)pyrene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(b)fluoranthene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(b)fluoranthene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(g,h,i)perylene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(g,h,i)perylene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(k)fluoranthene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(k)fluoranthene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Chrysene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Chrysene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Dibenzo(a,h)anthracene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Dibenzo(a,h)anthracene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluoranthene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluoranthene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluorene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluorene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Indeno(1,2,3,c,d)pyrene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Indeno(1,2,3,c,d)pyrene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Phenanthrene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Phenanthrene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Propionaldehyde","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Pyrene gas","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Pyrene particle","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Styrene","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"2,2,4-Trimethylpentane","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthylene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthylene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Anthracene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Anthracene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benz(a)anthracene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benz(a)anthracene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(a)pyrene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(a)pyrene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(b)fluoranthene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(b)fluoranthene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(g,h,i)perylene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(g,h,i)perylene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(k)fluoranthene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(k)fluoranthene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Chrysene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Chrysene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Dibenzo(a,h)anthracene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Dibenzo(a,h)anthracene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluoranthene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluoranthene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluorene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluorene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Indeno(1,2,3,c,d)pyrene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Indeno(1,2,3,c,d)pyrene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Phenanthrene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Phenanthrene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Propionaldehyde","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Pyrene gas","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Pyrene particle","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Styrene","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",

			"2,2,4-Trimethylpentane","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"2,2,4-Trimethylpentane","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"2,2,4-Trimethylpentane","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Ethyl Benzene","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Ethyl Benzene","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Ethyl Benzene","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Hexane","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Hexane","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Hexane","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Propionaldehyde","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Propionaldehyde","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Propionaldehyde","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Styrene","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Styrene","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Styrene","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Toluene","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Toluene","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Toluene","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Xylene","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Xylene","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Xylene","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthylene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthylene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthylene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Anthracene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Anthracene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Anthracene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benz(a)anthracene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benz(a)anthracene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benz(a)anthracene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(a)pyrene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(a)pyrene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(a)pyrene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(b)fluoranthene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(b)fluoranthene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(b)fluoranthene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(g,h,i)perylene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(g,h,i)perylene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(g,h,i)perylene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(k)fluoranthene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(k)fluoranthene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(k)fluoranthene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Chrysene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Chrysene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Chrysene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluorene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluorene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluorene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Indeno(1,2,3,c,d)pyrene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Indeno(1,2,3,c,d)pyrene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Indeno(1,2,3,c,d)pyrene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Phenanthrene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Phenanthrene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Phenanthrene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Pyrene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Pyrene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Pyrene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthylene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthylene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Acenaphthylene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Anthracene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Anthracene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Anthracene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benz(a)anthracene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benz(a)anthracene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benz(a)anthracene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(a)pyrene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(a)pyrene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(a)pyrene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(b)fluoranthene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(b)fluoranthene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(b)fluoranthene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(g,h,i)perylene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(g,h,i)perylene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(g,h,i)perylene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(k)fluoranthene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(k)fluoranthene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Benzo(k)fluoranthene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Chrysene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Chrysene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Chrysene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluorene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluorene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluorene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Indeno(1,2,3,c,d)pyrene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Indeno(1,2,3,c,d)pyrene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Indeno(1,2,3,c,d)pyrene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Phenanthrene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Phenanthrene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Phenanthrene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Pyrene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Pyrene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Pyrene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Naphthalene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Naphthalene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Naphthalene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Dibenzo(a,h)anthracene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Dibenzo(a,h)anthracene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Dibenzo(a,h)anthracene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluoranthene particle","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluoranthene particle","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluoranthene particle","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Dibenzo(a,h)anthracene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Dibenzo(a,h)anthracene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Dibenzo(a,h)anthracene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluoranthene gas","Crankcase Running Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluoranthene gas","Crankcase Start Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
			"Fluoranthene gas","Crankcase Extended Idle Exhaust","gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",

			// Auxiliary Power Exhaust (91) process
            null, "Auxiliary Power Exhaust",
                "gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
			null, "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
			null, "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
		    "Total Gaseous Hydrocarbons","Auxiliary Power Exhaust",
                "gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMAuxiliaryPowerCalculator",
            "Carbon Monoxide (CO)","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMAuxiliaryPowerCalculator",
			"Oxides of Nitrogen (NOx)","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMAuxiliaryPowerCalculator",
			"Total Energy Consumption","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.EnergyConsumptionCalculator",
			"Atmospheric CO2","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			equivCO2PolName,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
			"Composite - NonECPM","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMAuxiliaryPowerCalculator",
			PM25ElementalCarbonName,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.CriteriaAndPMAuxiliaryPowerCalculator",
			sulfatePM10Name,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			sulfatePM25Name,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			PM25OrganicCarbonName,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			PM25ElementalCarbonName,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Composite - NonECPM","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary Exhaust PM2.5 - Total", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
			"Primary PM10 - Elemental Carbon", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary PM10 - Organic Carbon", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Primary Exhaust PM10  - Total", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
			"Methane (CH4)", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			so2Name,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.SO2Calculator",
			nOName,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			honoName,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
			nO2Name,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NO2Calculator",
			nH3Name,"Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.NH3AuxiliaryPowerCalculator",

			"1,3-Butadiene", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acetaldehyde", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acrolein", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzene", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethanol", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Formaldehyde", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"MTBE", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene", "Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",

			"Non-Methane Hydrocarbons","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Non-Methane Organic Gases","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Total Organic Gases","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
			"Volatile Organic Compounds","Auxiliary Power Exhaust",
				"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",

			"2,2,4-Trimethylpentane","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Acenaphthylene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Anthracene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benz(a)anthracene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(a)pyrene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(b)fluoranthene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(g,h,i)perylene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Benzo(k)fluoranthene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Chrysene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Dibenzo(a,h)anthracene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Ethyl Benzene","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluoranthene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Fluorene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Hexane","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Indeno(1,2,3,c,d)pyrene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Naphthalene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Phenanthrene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Propionaldehyde","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Pyrene gas","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Styrene","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Toluene","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			"Xylene","Auxiliary Power Exhaust","gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
			
			// TOG Speciation Mechanisms
			"CB05 Mechanism",null,"gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator",
			"CB6CMAQ Mechanism",null,"gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator",
			"SAPRC07T Mechanism",null,"gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator",
			"CB6AE7 Mechanism",null,"gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator",
			"NonHAPTOG Mechanism",null,"gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator"
		};

		String pollutantsProcessesAndNeedsNonroad[] = {
			/** NR_IMP: nonroad needed classes **/
			// the following is for NONROAD simulation
			// for all pollutants and processes, use
			// NonroadEmissionCalculator
			// - since it invokes nonroad.exe to do the simulation
			null,
			null,
			"gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",

			null, "1", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "15", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "18", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "19", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "20", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "21", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "30", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "31", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "32", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "90", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			null, "99", "gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",
			
			"5", null, "gov.epa.otaq.moves.master.implementation.ghg.NRHCSpeciationCalculator",
			"79", null, "gov.epa.otaq.moves.master.implementation.ghg.NRHCSpeciationCalculator",
			"80", null, "gov.epa.otaq.moves.master.implementation.ghg.NRHCSpeciationCalculator",
			"86", null, "gov.epa.otaq.moves.master.implementation.ghg.NRHCSpeciationCalculator",
			"87", null, "gov.epa.otaq.moves.master.implementation.ghg.NRHCSpeciationCalculator",

			"88", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",

			"20", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"21", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"22", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"23", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"24", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"25", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"26", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"27", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"40", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"41", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"42", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"43", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"44", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"45", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"46", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"60", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"61", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"62", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"63", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"65", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"66", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"67", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"68", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"69", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"70", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"71", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"72", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"73", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"74", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"75", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"76", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"77", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"78", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"81", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"82", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"83", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"84", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"130", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"131", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"132", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"133", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"134", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"135", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"136", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"137", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"138", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"139", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"140", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"141", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"142", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"143", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"144", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"145", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"146", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"168", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"169", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"170", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"171", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"172", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"173", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"174", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"175", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"176", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"177", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"178", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"181", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"182", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"183", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"184", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator",
			"185", null, "gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator"
		};

		switch (mc) {
			case M1:
				pollutantsProcessesAndNeeds = pollutantsProcessesAndNeedsOnroad;

				if(ExecutionRunSpec.theExecutionRunSpec.getOutputVMTData()){
					neededClassNames.add("gov.epa.otaq.moves.master.implementation.ghg.DistanceCalculator");
					neededClassNames.add("gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator");
					neededClassNames.add("gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator");
				}
		
				if(runSpec.outputSHO || runSpec.outputSH || runSpec.outputSHP || runSpec.outputSHIdling
						|| runSpec.outputStarts || runSpec.outputPopulation) {
					neededClassNames.add("gov.epa.otaq.moves.master.implementation.ghg.ActivityCalculator");
				}
				break;
			case M2:
				pollutantsProcessesAndNeeds = pollutantsProcessesAndNeedsNonroad;
				break;
			default:
				break;
		}

		for(int i=0;i<pollutantsProcessesAndNeeds.length;i+=3) {
			if(targetRunSpec.doesHavePollutantAndProcess(pollutantsProcessesAndNeeds[i],pollutantsProcessesAndNeeds[i+1])
					&& pollutantsProcessesAndNeeds[i+2] != null) {
				neededClassNames.add(pollutantsProcessesAndNeeds[i+2]);
			}
		}

		createdObjects = new TreeMap<String,MasterLoopable>();
		TreeSet<String> deferredClassNames = new TreeSet<String>();

		switch(mc) {
			case M1:
				if(isProjectDomain) {
					if(neededClassNames.contains(
							"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator")) {
						neededClassNames.remove(
								"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator");
						neededClassNames.add(
								"gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG");
					}
					if(neededClassNames.contains(
							"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupTotalActivityGenerator")) {
						neededClassNames.remove(
								"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupTotalActivityGenerator");
						neededClassNames.add(
								"gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG");
					}
					if(neededClassNames.contains(
							"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator")) {
						neededClassNames.remove(
								"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator");
						neededClassNames.add(
								"gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator");
					}
					if(!CompilationFlags.DO_RATES_FIRST) {
						neededClassNames.remove("gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator");
					}
					//neededClassNames.remove("gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator");
					//neededClassNames.remove("gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator");
					//neededClassNames.remove("gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator");
				}
				if(isMesoscaleLookup) {
					/*
					if(neededClassNames.contains(
							"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator")) {
						neededClassNames.remove(
								"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator");
						neededClassNames.add(
								"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupTotalActivityGenerator");
					}
					*/
					if(neededClassNames.contains(
							"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator")) {
						neededClassNames.remove(
								"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator");
						neededClassNames.add(
								"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator");
					}
				}
	
				if(CompilationFlags.DO_RATES_FIRST) {
					TreeSet<String> originalClassNames = new TreeSet<String>();
					originalClassNames.addAll(neededClassNames);
					neededClassNames.clear();
					String[] neededClasses = {
						"gov.epa.otaq.moves.master.implementation.ghg.BaseRateCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.BaseRateGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.RatesOperatingModeDistributionGenerator"
					};
					for(int i=0;i<neededClasses.length;i++) {
						neededClassNames.add(neededClasses[i]);
					}
					String[] whiteList = {
						"gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator",

					    "gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",

						"gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG",
						"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
						
						"gov.epa.otaq.moves.master.implementation.ghg.ActivityCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.AirToxicsCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.DistanceCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.CO2AERunningStartExtendedIdleCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.CrankcaseEmissionCalculatorNonPM",
						"gov.epa.otaq.moves.master.implementation.ghg.EvaporativePermeationCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.HCSpeciationCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.LiquidLeakingCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.NOCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.NO2Calculator",
						"gov.epa.otaq.moves.master.implementation.ghg.PM10BrakeTireCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.PM10EmissionCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.RefuelingLossCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.SO2Calculator",
						"gov.epa.otaq.moves.master.implementation.ghg.SulfatePMCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.TankVaporVentingCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator"
					};
					for(int i=0;i<whiteList.length;i++) {
						if(originalClassNames.contains(whiteList[i])) {
							neededClassNames.add(whiteList[i]);
						}
					}
				}
	
				// If distance is needed, but Running Exhaust is not selected, then we must
				// defer instantiating the TAG and distance calculator until after others
				// have signed up and we add Running Exhaust.
				if(!CompilationFlags.DO_RATES_FIRST && (ExecutionRunSpec.theExecutionRunSpec.getOutputVMTData()
						&& !targetRunSpec.doesHaveDistancePollutantAndProcess())) {
					String[] namesToDefer = {
						"gov.epa.otaq.moves.master.implementation.ghg.DistanceCalculator",
						"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
						"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator"
					};
					for(int i=0;i<namesToDefer.length;i++) {
						if(neededClassNames.contains(namesToDefer[i])) {
							neededClassNames.remove(namesToDefer[i]);
							deferredClassNames.add(namesToDefer[i]);
						}
					}
				}
				break;
			case M2:
				// Nothing to do here
				break;
		}

		try {
			// First, create the required objects, then in a separate pass, tell them
			// to subscribe with the MasterLoop.  This is done so that chained calculators
			// can find registered calculators.  Registration of pollutants produced by
			// calculators happens in their constructors but setup of chaining happens in
			// the subscribeToMe() method, so all calculators need to be instantiated and
			// therefore registered prior to calling subscribeToMe().
			InterconnectionTracker.startTracking();
			for(Iterator<String> i=neededClassNames.iterator();i.hasNext();) {
				String name = (String)i.next();
				if(!createdObjects.containsKey(name)) {
					try {
						Class<?> c = Class.forName(name);
						if(targetRunSpec.shouldExecute(c)) {
							try {
								Object object = c.getConstructor().newInstance();
								MasterLoopable loopable = (MasterLoopable)object;
								createdObjects.put(name,loopable);
								Logger.log(LogMessageCategory.INFO,"Class " + name + " has been instantiated.");
							} catch(Exception e) {
								/**
								 * @explain An internal code issue has occurred while setting up code
								 * required for a simulation.
								**/
								Logger.logError(e, "MOVESInstantiator is unable to instantiate " + name);
							}
						} else {
							Logger.log(LogMessageCategory.INFO,"Class " + name + " has been disabled by the user.");
						}
					} catch(ClassNotFoundException e) {
						/**
						 * @explain An internal code issue has occurred while setting up code
						 * required for a simulation.
						**/
						Logger.logError(e, "MOVESInstantiator could not find Java class " + name + " to instantiate");
					}
				}
			}
			// Now, call subscribeToMe() for each of the MasterLoopable objects
			Set<String> objectNames = createdObjects.keySet();
			for(Iterator<String> i=objectNames.iterator();i.hasNext();) {
				String name = (String)i.next();
				MasterLoopable loopable = (MasterLoopable)createdObjects.get(name);
				try {
					int eventRecordID =
							MOVESEngine.logEventStart("Subscribe " + loopable.getClass().getName());
					loopable.subscribeToMe(targetLoop);
					MOVESEngine.logEventStop(eventRecordID);
				} catch(Exception e) {
					/**
					 * @explain An internal code issue has occurred while setting up code
					 * required for a simulation.
					**/
					Logger.logError(e, "MOVESInstantiator is unable to subscribe " + name);
				}
			}
	
			// Call subscribeToMe() for each InternalControlStrategy as well
			TreeMap<String,LinkedList<InternalControlStrategy> > internalControlStrategies =
					targetRunSpec.getInternalControlStrategies();
			internalControlStrategies = addAutomaticInternalControlStrategies(internalControlStrategies);
			Set<String> strategyClassNames = internalControlStrategies.keySet();
			for(Iterator<String> i=strategyClassNames.iterator();i.hasNext();) {
				String name = (String)i.next();
				LinkedList<InternalControlStrategy> strategies = (LinkedList<InternalControlStrategy>)
						internalControlStrategies.get(name);
				if(strategies != null) {
					for(Iterator<InternalControlStrategy> j=strategies.iterator();j.hasNext();) {
						InternalControlStrategy strategy = (InternalControlStrategy)j.next();
						try {
							int eventRecordID =
									MOVESEngine.logEventStart(
									"Subscribe " + strategy.getClass().getName());
							strategy.subscribeToMe(targetLoop);
							MOVESEngine.logEventStop(eventRecordID);
						} catch(Exception e) {
							/**
							 * @explain An internal code issue has occurred while setting up code
							 * required for a simulation.
							**/
							Logger.logError(e, "MOVESInstantiator is unable to subscribe " + name);
						}
					}
				}
			}
	
			switch(mc) {
				case M1:
					// If Onroad distance is needed, but Running Exhaust is not selected, then we must
					// now add the TAG, the distance calculator, and the Running Exhaust process.
					// This must be done after other signups so that nothing else gets run during
					// Running Exhaust, which we're only adding because distance calculations
					// require it.
					if(!CompilationFlags.DO_RATES_FIRST && (ExecutionRunSpec.theExecutionRunSpec.getOutputVMTData()
							&& !targetRunSpec.doesHaveDistancePollutantAndProcess())) {
						// Add Running Exhaust to the target runspec
						EmissionProcess process = EmissionProcess.findByID(1);
						if(process == null) {
							Logger.log(LogMessageCategory.ERROR,"Cannot find Running Exhaust process needed for distance output.");
						}
						Pollutant pollutant = Pollutant.findByID(1);
						if(pollutant == null) {
							Logger.log(LogMessageCategory.ERROR,"Cannot find THC pollutant needed for distance output.");
						}
						PollutantProcessAssociation ppa = PollutantProcessAssociation.createByID(pollutant.databaseKey,process.databaseKey);
						if(ppa == null) {
							Logger.log(LogMessageCategory.ERROR,"Cannot find Running Exhaust/THC combination needed for distance output.");
						}
						if(process != null && pollutant != null && ppa != null) {
							// Add required pollutant/process so that SBDG and Distance Calculator will successfully register and execute.
							// Because they are added after all others though, no calculators will be instantiated for what we actually add here.
							targetRunSpec.targetProcesses.add(process);
							targetRunSpec.targetPollutants.add(pollutant);
							targetRunSpec.targetPollutantProcesses.add(Integer.valueOf(ppa.getDatabaseKey()));
							targetRunSpec.pollutantProcessAssociations.add(ppa);
			
							Connection executionDB = null;
							try {
								executionDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
								String sql = "insert into RunspecPollutantProcess (polProcessID) values (" + ppa.getDatabaseKey() + ")";
								SQLRunner.executeSQL(executionDB,sql);
							} catch(Exception e) {
								Logger.logError(e,"Unable to add Running Exhaust/THC to RunspecPollutantProcess table, as needed for distance calculation.");
							} finally {
								if(executionDB != null) {
									try {
										DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,executionDB);
									} catch(Exception e) {
										// Nothing to do here
									}
									executionDB = null;
								}
							}
			
							// Instantiate all deferred items
							TreeMap<String,MasterLoopable> deferredCreatedObjects = new TreeMap<String,MasterLoopable>();
							for(Iterator<String> i=deferredClassNames.iterator();i.hasNext();) {
								String name = (String)i.next();
								if(!createdObjects.containsKey(name)) {
									try {
										Class<?> c = Class.forName(name);
										if(targetRunSpec.shouldExecute(c)) {
											try {
												Object object = c.getConstructor().newInstance();
												MasterLoopable loopable = (MasterLoopable)object;
												createdObjects.put(name,loopable);
												deferredCreatedObjects.put(name,loopable);
											} catch(Exception e) {
												/**
												 * @explain An internal code issue has occurred while setting up code
												 * required for a simulation.
												**/
												Logger.logError(e, "MOVESInstantiator is unable to instantiate " + name);
											}
										}
									} catch(ClassNotFoundException e) {
										/**
										 * @explain An internal code issue has occurred while setting up code
										 * required for a simulation.
										**/
										Logger.logError(e, "MOVESInstantiator could not find Java class " + name +
												" to instantiate");
									}
								}
							}
			
							// Call subscribe on all deferred items
							Set<String> tempObjectNames = deferredCreatedObjects.keySet();
							for(Iterator<String> i=tempObjectNames.iterator();i.hasNext();) {
								String name = (String)i.next();
								MasterLoopable loopable = (MasterLoopable)deferredCreatedObjects.get(name);
								try {
									int eventRecordID =
											MOVESEngine.logEventStart("Subscribe " + loopable.getClass().getName());
									loopable.subscribeToMe(targetLoop);
									MOVESEngine.logEventStop(eventRecordID);
								} catch(Exception e) {
									/**
									 * @explain An internal code issue has occurred while setting up code
									 * required for a simulation.
									**/
									Logger.logError(e, "MOVESInstantiator is unable to subscribe " + name);
								}
							}
						}
					}
					break;
				case M2:
					// Nothing to do here
					break;
			}
		} finally {
			neededClassNames = null;
			InterconnectionTracker.finishTracking();
		}
	}

	/**
	 * Add any InternalControlStrategy objects that are implied by presence of table data.
	 * @param originals objects that exist in the RunSpec
	 * @return the collection of objects to be used at runtime.
	**/
	static TreeMap<String,LinkedList<InternalControlStrategy> > addAutomaticInternalControlStrategies(
			TreeMap<String,LinkedList<InternalControlStrategy> > originals) {
		String avftClassName = "gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft.AVFTControlStrategy";
		String retrofitClassName = "gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy";

		boolean hasAVFTData = false;
		boolean shouldCreateAVFT = false;
		boolean hasRetrofitData = false;
		boolean shouldCreateRetrofit = false;
		boolean hasAnyData = false;
		boolean shouldCreateSomething = false;

		LinkedList<InternalControlStrategy> s = originals.get(avftClassName);
		if(s != null && s.size() > 0) {
			hasAVFTData = true;
			hasAnyData = true;
		}
		s = originals.get(retrofitClassName);
		if(s != null && s.size() > 0) {
			hasRetrofitData = true;
			hasAnyData = true;
		}
		if(!hasAVFTData || !hasRetrofitData) {
			Connection executionDB = null;
			String sql = "";
			try {
				executionDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
				if(!hasAVFTData) {
					sql = "select count(*) from avft";
					if(SQLRunner.executeScalar(executionDB,sql) >= 1) {
						hasAVFTData = true;
						hasAnyData = true;
						shouldCreateAVFT = true;
						shouldCreateSomething = true;
					}
				}
				if(!hasRetrofitData) {
					sql = "select count(*) from onRoadRetrofit";
					if(SQLRunner.executeScalar(executionDB,sql) >= 1) {
						hasRetrofitData = true;
						hasAnyData = true;
						shouldCreateRetrofit = true;
						shouldCreateSomething = true;
					}
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to check for existing strategy data using: " + sql);
			} finally {
				if(executionDB != null) {
					try {
						DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,executionDB);
					} catch(Exception e) {
						// Nothing to do here
					}
					executionDB = null;
				}
			}
		}
		if(!shouldCreateSomething) {
			return originals;
		}
		// Copy the original collection
		TreeMap<String,LinkedList<InternalControlStrategy> > result = new TreeMap<String,LinkedList<InternalControlStrategy> >();
		Set<String> keys = originals.keySet();
		for(Iterator<String> ki=keys.iterator();ki.hasNext();) {
			String key = ki.next();
			LinkedList<InternalControlStrategy> newList = new LinkedList<InternalControlStrategy>();
			result.put(key,newList);
			s = originals.get(key);
			if(s != null) {
				newList.addAll(s);
			}
		}
		// Add AVFT data
		if(shouldCreateAVFT) {
			s = new LinkedList<InternalControlStrategy>();
			s.add(new AVFTControlStrategy());
			result.put(avftClassName,s);
		}
		// Add Onroad Retrofit data
		if(shouldCreateRetrofit) {
			s = new LinkedList<InternalControlStrategy>();
			s.add(new OnRoadRetrofitStrategy());
			result.put(retrofitClassName,s);
		}
		return result;
	}

	/**
	 * This is used to indicate that another class is also required to be instantiated.
	 * @param name The full package and class name of a MasterLoopable-based
	 * class to be instantiated.
	**/
	public static void alsoInstantiate(String name) {
		if(neededClassNames != null) {
			neededClassNames.add(name);
		} else {
			/**
			 * @explain An internal code issue has occurred while setting up code
			 * required for a simulation.
			**/
			Logger.log(LogMessageCategory.ERROR, "Invalid timing on request to instantiate "
					+ name);
		}
	}

	/**
	 * This is used to indicate that another class is also required to be instantiated.
	 * @param name The full package and class name of a MasterLoopable-based
	 * class to be checked for instantiation.
	**/
	public static MasterLoopable didInstantiate(String name) {
		if(createdObjects != null) {
			return (MasterLoopable)createdObjects.get(name);
		} else {
			/**
			 * @issue Invalid timing on request for instantiated object [name]
			 * @explain This internal error can occur if user-modified Java code calls
			 * MOVESInstantiator.didInstantiate() out of the proper sequence.
			**/
			Logger.log(LogMessageCategory.ERROR,
				"Invalid timing on request for instantiated object " + name);
			return null;
		}
	}

	/**
	 * Resets all internal collections and data. Useful when reloading.
	**/
	public static void reset() {
		neededClassNames = null;
		createdObjects = null;
		InterconnectionTracker.finishTracking();
	}
}
