/**************************************************************************************************
 * @(#)OutputFactors.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.TreeMap;

/**
 * This class contains the selected output factors for time, distance, and mass (emission).
 *
 * @author		Cimulus
 * @version		2003-09-18
 * @author		Tim Hull
 * @version		2009-07-23
 * Small code modifications to accomodate new requirements of General Output (in particular, requiring distance units)
**/
public class OutputFactors {
	/** Indicates whether to use time factors. **/
	public boolean timeFactorsSelected = true;
	/** Indicates whether to use distance factors. **/
	public boolean distanceFactorsSelected = true;
	/** Indicates whether to use mass and energy factors. **/
	public boolean massFactorsSelected = true;
	/** The selected time measurement system. **/
	public TimeMeasurementSystem timeMeasurementSystem = null;
	/** The selected distance measurement system. **/
	public DistanceMeasurementSystem distanceMeasurementSystem = null;
	/** The selected mass measurement system. **/
	public MassMeasurementSystem massMeasurementSystem = null;
	/** The selected energy measurement system **/
	public EnergyMeasurementSystem energyMeasurementSystem = null;

	/** Default constructor. **/
	public OutputFactors() {
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other OutputFactors object to check.
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other instanceof OutputFactors) {
			OutputFactors otherOutputFactors = (OutputFactors)other;
			boolean result = false;
			result = this.timeFactorsSelected ==
					otherOutputFactors.timeFactorsSelected &&
					this.distanceFactorsSelected ==
					otherOutputFactors.distanceFactorsSelected &&
					this.massFactorsSelected ==
					otherOutputFactors.massFactorsSelected;
			if(this.timeMeasurementSystem != null ||
					otherOutputFactors.timeMeasurementSystem != null) {
				result &= (this.timeMeasurementSystem ==
						otherOutputFactors.timeMeasurementSystem);
			}
			if(this.distanceMeasurementSystem != null ||
					otherOutputFactors.distanceMeasurementSystem != null) {
				result &= (this.distanceMeasurementSystem ==
						otherOutputFactors.distanceMeasurementSystem);
			}
			if(this.massMeasurementSystem != null ||
					otherOutputFactors.massMeasurementSystem != null) {
				result &= (this.massMeasurementSystem ==
						otherOutputFactors.massMeasurementSystem);
			}
			if(this.energyMeasurementSystem != null ||
					otherOutputFactors.energyMeasurementSystem != null) {
				result &= (this.energyMeasurementSystem ==
						otherOutputFactors.energyMeasurementSystem);
			}
			return result;
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * Check to ensure that at least one set of factors and supporting measurement systems
	 * are selected.
	 * @return True if at least one set of factors is selected as well as the required
	 * measurement systems to support the selected factors.
	**/
	public boolean isValid() {
		if(!timeFactorsSelected && !distanceFactorsSelected && !massFactorsSelected) {
			return false;
		}

		boolean needMass = false;
		boolean needEnergy = false;
		boolean needTime = false;
		boolean needDistance = false;

		if(timeFactorsSelected) {
			needMass = true;
			needEnergy = true;
		}
		if(distanceFactorsSelected) {
			needMass = true;
			needEnergy = true;
			needDistance = true;
		}
		if(massFactorsSelected) {
			needMass = true;
			needEnergy = true;
		}
		if((needMass && massMeasurementSystem == null)
				|| (needEnergy && energyMeasurementSystem == null)
				|| (needTime && timeMeasurementSystem == null)
				|| (needDistance && distanceMeasurementSystem == null)) {
			return false;
		}
		return true;
	}
}
