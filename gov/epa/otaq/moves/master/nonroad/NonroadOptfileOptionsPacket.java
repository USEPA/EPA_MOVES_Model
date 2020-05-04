/**
 * Project: EpaMOVES
 * Package: gov.epa.otaq.moves.master.nonroad.NonroadOptfileOptionsPacket.java
 * Version: Mar 8, 2012
 */
package gov.epa.otaq.moves.master.nonroad;

/**
 * @author Jizhen Zhao
 */
public class NonroadOptfileOptionsPacket {
	/**
	 * 
	 */
	private double rVPmkt;
	/**
	 * 
	 */
	private double oxygenWeightPercent;
	/**
	 * 
	 */
	private double gasSulfurPercent;
	/**
	 * 
	 */
	private double dieselSulfurPercent;
	/**
	 * 
	 */
	private double marineDieselSulfurPercent;
	/**
	 * 
	 */
	private double naturalGasSulfurPercent;
	/**
	 * 
	 */
	private double minimumTemperature;
	/**
	 * 
	 */
	private double maximumTemperature;
	/**
	 * 
	 */
	private double averageTemperature;
	/**
	 * 
	 */
	private String altitude;
	/**
	 * 
	 */
	private double eTOHMktShare;
	/**
	 * 
	 */
	private double eTOHVolume;

	/**
	 * 
	 */
	public NonroadOptfileOptionsPacket() {

	}

	/**
	 * 
	 */
	public NonroadOptfileOptionsPacket(double rVPmkt,
			double oxygenWeightPercent, double gasSulfurPercent,
			double dieselSulfurPercent, double marineDieselSulfurPercent,
			double naturalGasSulfurPercent, double minimumTemperature,
			double maximumTemperature, double averageTemperature,
			String altitude, double eTOHMktShare, double eTOHVolume) {
		this.rVPmkt = rVPmkt;
		this.oxygenWeightPercent = oxygenWeightPercent;
		this.gasSulfurPercent = gasSulfurPercent;
		this.dieselSulfurPercent = dieselSulfurPercent;
		this.marineDieselSulfurPercent = marineDieselSulfurPercent;
		this.naturalGasSulfurPercent = naturalGasSulfurPercent;
		this.minimumTemperature = minimumTemperature;
		this.maximumTemperature = maximumTemperature;
		this.averageTemperature = averageTemperature;
		this.altitude = altitude;
		this.eTOHMktShare = eTOHMktShare;
		this.eTOHVolume = eTOHVolume;
	}

	/**
	 * @return the rVPmkt
	 */
	public double getrVPmkt() {
		return rVPmkt;
	}

	/**
	 * @param rVPmkt
	 *            the rVPmkt to set
	 */
	public void setrVPmkt(double rVPmkt) {
		this.rVPmkt = rVPmkt;
	}

	/**
	 * @return the oxygenWeightPercent
	 */
	public double getOxygenWeightPercent() {
		return oxygenWeightPercent;
	}

	/**
	 * @param oxygenWeightPercent
	 *            the oxygenWeightPercent to set
	 */
	public void setOxygenWeightPercent(double oxygenWeightPercent) {
		this.oxygenWeightPercent = oxygenWeightPercent;
	}

	/**
	 * @return the gasSulfurPercent
	 */
	public double getGasSulfurPercent() {
		return gasSulfurPercent;
	}

	/**
	 * @param gasSulfurPercent
	 *            the gasSulfurPercent to set
	 */
	public void setGasSulfurPercent(double gasSulfurPercent) {
		this.gasSulfurPercent = gasSulfurPercent;
	}

	/**
	 * @return the dieselSulfurPercent
	 */
	public double getDieselSulfurPercent() {
		return dieselSulfurPercent;
	}

	/**
	 * @param dieselSulfurPercent
	 *            the dieselSulfurPercent to set
	 */
	public void setDieselSulfurPercent(double dieselSulfurPercent) {
		this.dieselSulfurPercent = dieselSulfurPercent;
	}

	/**
	 * @return the marineDieselSulfurPercent
	 */
	public double getMarineDieselSulfurPercent() {
		return marineDieselSulfurPercent;
	}

	/**
	 * @param marineDieselSulfurPercent
	 *            the marineDieselSulfurPercent to set
	 */
	public void setMarineDieselSulfurPercent(double marineDieselSulfurPercent) {
		this.marineDieselSulfurPercent = marineDieselSulfurPercent;
	}

	/**
	 * @return the naturalGasSulfurPercent
	 */
	public double getNaturalGasSulfurPercent() {
		return naturalGasSulfurPercent;
	}

	/**
	 * @param naturalGasSulfurPercent
	 *            the naturalGasSulfurPercent to set
	 */
	public void setNaturalGasSulfurPercent(double naturalGasSulfurPercent) {
		this.naturalGasSulfurPercent = naturalGasSulfurPercent;
	}

	/**
	 * @return the minimumTemperature
	 */
	public double getMinimumTemperature() {
		return minimumTemperature;
	}

	/**
	 * @param minimumTemperature
	 *            the minimumTemperature to set
	 */
	public void setMinimumTemperature(double minimumTemperature) {
		this.minimumTemperature = minimumTemperature;
	}

	/**
	 * @return the maximumTemperature
	 */
	public double getMaximumTemperature() {
		return maximumTemperature;
	}

	/**
	 * @param maximumTemperature
	 *            the maximumTemperature to set
	 */
	public void setMaximumTemperature(double maximumTemperature) {
		this.maximumTemperature = maximumTemperature;
	}

	/**
	 * @return the averageTemperature
	 */
	public double getAverageTemperature() {
		return averageTemperature;
	}

	/**
	 * @param averageTemperature
	 *            the averageTemperature to set
	 */
	public void setAverageTemperature(double averageTemperature) {
		this.averageTemperature = averageTemperature;
	}

	/**
	 * @return the altitude
	 */
	public String getAltitude() {
		return altitude;
	}

	/**
	 * @param altitude
	 *            the altitude to set
	 */
	public void setAltitude(String altitude) {
		this.altitude = altitude;
	}

	/**
	 * @return the eTOHMktShare
	 */
	public double geteTOHMktShare() {
		return eTOHMktShare;
	}

	/**
	 * @param eTOHMktShare
	 *            the eTOHMktShare to set
	 */
	public void seteTOHMktShare(double eTOHMktShare) {
		this.eTOHMktShare = eTOHMktShare;
	}

	/**
	 * @return the eTOHVolume
	 */
	public double geteTOHVolume() {
		return eTOHVolume;
	}

	/**
	 * @param eTOHVolume
	 *            the eTOHVolume to set
	 */
	public void seteTOHVolume(double eTOHVolume) {
		this.eTOHVolume = eTOHVolume;
	}
}