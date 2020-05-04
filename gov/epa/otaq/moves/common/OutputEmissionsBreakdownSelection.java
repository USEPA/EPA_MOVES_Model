/**************************************************************************************************
 * @(#)OutputEmissionsBreakdownSelection.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

/**
 * This represents the checkboxes in the Output Emissions Breakdown GUI panel.
 *
 * @author		Wesley Faler
 * @version		2015-03-16
**/
public class OutputEmissionsBreakdownSelection {
	/** modelYear portion of output emissions. **/
	public boolean modelYear;
	
	/** fuelType portion of output emissions. **/
	public boolean fuelType;

	/** fuelSubType portion of output emissions. **/
	public boolean fuelSubType;
	
	/** emissionProcess portion of output emissions. **/
	public boolean emissionProcess;
	
	/** onRoadOffRoad portion of output emissions. **/
	public boolean onRoadOffRoad;
	
	/** roadType portion of output emissions. **/
	public boolean roadType;
	
	/** source use type portion of output emissions. **/
	public boolean sourceUseType;
	
	/** movesVehicleType portion of output emissions. **/
	public boolean movesVehicleType;
	
	/** on road scc portion of output emissions. **/
	public boolean onRoadSCC;
	
	/** sector portion of NonRoad output emissions. **/
	public boolean sector;

	/** engine tech ID portion of NonRoad output emissions **/
	public boolean engTechID;
	
	/** hpClass portion of output emissions. **/
	public boolean hpClass;
	
	/** regulatory class of onroad output emissions **/
	public boolean regClassID;

	/** estimateUncertainty portion of output emissions. **/
	public boolean estimateUncertainty;

	/** Number of iterations to include in uncertainty estimate. **/
	public int numberOfIterations;
	
	/** Keep sample data used in estimating uncertainty **/
	public boolean keepSampledData;

	/** Keep output of each iteration **/
	public boolean keepIterations;


	/** Default constructor **/
	public OutputEmissionsBreakdownSelection() {
	}
	
	/**
	 * Corrects invalid combinations of options. For example, if on/off road is 
	 * false, then all on road and off road booleans must be set to false.
	**/
	public void adjustForStrictlyOperativeValues() {
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other OutputEmissionsBreakdownSelection to check.
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other instanceof OutputEmissionsBreakdownSelection) {
			OutputEmissionsBreakdownSelection otherSelection =
					(OutputEmissionsBreakdownSelection)other;
			return this.modelYear == otherSelection.modelYear &&
					this.fuelType == otherSelection.fuelType &&
					this.fuelSubType == otherSelection.fuelSubType &&
					this.emissionProcess == otherSelection.emissionProcess &&
					this.onRoadOffRoad == otherSelection.onRoadOffRoad &&
					this.roadType == otherSelection.roadType &&
					this.sourceUseType == otherSelection.sourceUseType &&
					this.movesVehicleType == otherSelection.movesVehicleType &&
					this.onRoadSCC == otherSelection.onRoadSCC &&
					this.sector == otherSelection.sector &&
					this.engTechID == otherSelection.engTechID &&
					this.hpClass == otherSelection.hpClass &&
					this.estimateUncertainty == otherSelection.estimateUncertainty &&
					this.regClassID == otherSelection.regClassID;
		} else {
			throw new ClassCastException();
		}
	}
}
