/**
 * Project: EpaMOVES
 * Package: gov.epa.otaq.moves.master.nonroad.NonroadPMBasedSulfur.java
 * Version: Mar 8, 2012
 */
package gov.epa.otaq.moves.master.nonroad;

/**
 * @author Jizhen Zhao
 * 
 */
public class NonroadPMBasedSulfur implements Comparable<NonroadPMBasedSulfur> {
	String techType;
	double pmBaseSulfur;
	double sufatePNConversionFac;

	public NonroadPMBasedSulfur(String techType, double pmBaseSulfur,
			double sufatePNConversionFac) {
		super();
		this.techType = techType;
		this.pmBaseSulfur = pmBaseSulfur;
		this.sufatePNConversionFac = sufatePNConversionFac;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(NonroadPMBasedSulfur o) {
		// TODO Auto-generated method stub
		return techType.compareTo(o.techType);
	}

}
