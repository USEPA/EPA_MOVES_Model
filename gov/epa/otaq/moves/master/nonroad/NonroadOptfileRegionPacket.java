/**
 * Project: EpaMOVES
 * Package: gov.epa.otaq.moves.master.nonroad.NonroadOptfileRegionPacket.java
 * Version: Mar 8, 2012
 */
package gov.epa.otaq.moves.master.nonroad;

/**
 * @author  Administrator
 */
public class NonroadOptfileRegionPacket {
	/**
	 * 
	 */
	private String stateAbbr;
	/**
	 * 
	 */
	private String countyName;

	/**
	 * 
	 */
	public NonroadOptfileRegionPacket(String stateAbbr, String countyName) {
		this.stateAbbr = stateAbbr;
		this.countyName = countyName;
	}

	/**
	 * @return the stateAbbr
	 */
	public String getStateAbbr() {
		return stateAbbr;
	}

	/**
	 * @param stateAbbr the stateAbbr to set
	 */
	public void setStateAbbr(String stateAbbr) {
		this.stateAbbr = stateAbbr;
	}

	/**
	 * @return the countyName
	 */
	public String getCountyName() {
		return countyName;
	}

	/**
	 * @param countyName the countyName to set
	 */
	public void setCountyName(String countyName) {
		this.countyName = countyName;
	}
}