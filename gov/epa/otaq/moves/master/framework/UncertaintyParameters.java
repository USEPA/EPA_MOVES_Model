/**************************************************************************************************
 * @(#)UncertaintyParameters.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * Track data related to the uncertainty function of the software. In uncertainty mode, the 
 * entire simulation is run several times to calculate a variance between the different 
 * results.
 *
 * @author		Cimulus
 * @version		2003-02-04
**/
public class UncertaintyParameters {
	/** Default constructor **/
	public boolean uncertaintyModeEnabled;
	
	/**
	 * The number of runs of the system for a given simulation. 
	 * The term simulation is used to refer to a group of runs.
	**/
	public int numberOfRunsPerSimulation;
	
	/** The number of simulations that should be run in total. **/
	public int numberOfSimulations;
	
	/** Default constructor **/
	public UncertaintyParameters() {
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other UncertaintyParameters to check.
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other instanceof UncertaintyParameters) {
			UncertaintyParameters otherUncertaintyParameters =
					(UncertaintyParameters)other;
			return this.numberOfRunsPerSimulation ==
					otherUncertaintyParameters.numberOfRunsPerSimulation &&
					this.numberOfSimulations ==
					otherUncertaintyParameters.numberOfSimulations &&
					this.uncertaintyModeEnabled ==
					otherUncertaintyParameters.uncertaintyModeEnabled;
		} else {
			throw new ClassCastException();
		}
	}
}
