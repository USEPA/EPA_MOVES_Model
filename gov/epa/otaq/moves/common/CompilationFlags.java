/**************************************************************************************************
 * @(#)CompilationFlags.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

/**
 * Control version-specific features.
 * Note:  If value(s) of these flags are changed, all classes
 *        which reference them must be recompiled
 *
 * @author  	EPA Ed Glover
 * @author		wgfaler
 * @author		EPA Mitch C.
 * @version		2015-03-17
**/
public class CompilationFlags {
	/** flag to control logging of timing events **/
	public static final boolean SHOULD_LOG_TIMING_EVENTS = false;

	/** flag to control export buttons on County and Project data managers from default data **/
	public static final boolean ENABLE_EXPORT_DEFAULT_DATA = false;

	/** flag controlling use of the NonRoad model **/
	public static final boolean USE_NONROAD = true;

	/** flag controlling use of multi-day diurnal calculations **/
	public static final boolean USE_MULTIDAY_DIURNALS = true;

	/** flag controlling use of the fuelUsageFraction table for mapping equipped fuel to used fuel **/
	public static final boolean USE_FUELUSAGEFRACTION = true;

	/** flag controlling calculation methodology of creating rates before inventory **/
	public static final boolean DO_RATES_FIRST = true;

	/** flag controlling use of the Auxiliary Power Exhaust process **/
	public static final boolean ENABLE_AUXILIARY_POWER_EXHAUST = true;

	/**
	 * false for USA, true for most international users.
	 * When true, distance data in external files is treated as kilometers and converted
	 * to miles when imported to the execution database.
	**/
	public static final boolean USE_KILOMETERS_IN_USER_DATA = false;

	/**
	 * true when no default emission rates (in EmissionRate and EmissionRateByAge
	 * tables) should be used. Enable this only when changing sourceBinIDs in
	 * custom data as mixing new sourceBinIDs and default sourceBinIDs in the
	 * emission rate tables would pose problems. Most users, including users of
	 * the MOVES International tools, can leave this as false.
	**/
	public static final boolean USE_ONLY_USER_SUPPLIED_EMISSION_RATES = false;

	/**
	 * false to stop runs when an average speed bin has no bounding drive cycle.
	 * When true, the nearest drive cycle is used which can result in aphysical
	 * emissions.
	**/
	public static final boolean ALLOW_DRIVE_CYCLE_EXTRAPOLATION = true;

	/**
	 * true to use 2010B's incorrect tirewear rate algorithm.
	 * false to use a corrected algorithm.
	 * This flag is only used when DO_RATES_FIRST = true
	**/
	public static final boolean USE_2010B_TIREWEAR_RATE_METHOD = false;

	/**
	 * true to generate documentation in the CalculatorInfo.txt file,
	 * rather than do any computation. All modules are instantiated, but
	 * no executeLoop or cleanup methods are called when true.
	 * Set to false for normal operations. Typically, only a software
	 * developer or technical writer would need to set this to true.
	**/
	public static final boolean GENERATE_CALCULATOR_INFO_DOCUMENTATION = false;

	/**
	 * true to use 2010B's algorithm for extended idle and APU hotelling.
	 * false to use MOVES2014's improved algorithm.
	 * This algorithm was improved in MOVES2014 to better handle
	 * hotelling hours in county-domain runs.
	**/
	public static final boolean USE_2010B_HOTELLING_ALGORITHM = false;
	
	/**
	 * true to use the emission rate adjustment factor for heavy-duty vehicles.
	 * false to use MOVES2014's default algorithm.
	 * This algorithm was written for the heavy-duty GHG2 rulemaking.
	**/
	public static final boolean USE_EMISSIONRATEADJUSTMENT_FACTOR = true;

	/**
	 * true to enable detailed timings for chained calculators.
	**/
	public static final boolean USE_WORKER_TIMING_DETAILS = true;

	/** true to enable MOVESOutput.fuelSubTypeID **/
	public static final boolean ALLOW_FUELSUBTYPE_OUTPUT = true;
}
