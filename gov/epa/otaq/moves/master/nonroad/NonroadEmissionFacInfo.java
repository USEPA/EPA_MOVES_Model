/**
 * Project: EpaMOVES
 * Package: gov.epa.otaq.moves.master.nonroad.NonroadEmissionFacInfo.java
 * Version: Sep 11, 2012
 */
package gov.epa.otaq.moves.master.nonroad;

/**
 * @author Administrator
 * 
 */
public class NonroadEmissionFacInfo {

	// detfac exist or not
	public boolean detExhTHCExist = false;
	public boolean detExhCOExist = false;
	public boolean detExhPMExist = false;
	public boolean detExhNOXExist = false;

	// emsfac exist or not
	public boolean emsExhTHCExist = false;
	public boolean emsExhCOExist = false;
	public boolean emsExhNOXExist = false;
	public boolean emsExhPMExist = false;
	public boolean emsBSFCExist = true; // always there
	public boolean emsCRANKExist = false;
	public boolean emsSPILLAGEExist = false;
	public boolean emsEvDiuExist = false;
	public boolean emsEvTANKExist = false;
	public boolean emsEvHOSEExist = false;
	public boolean emsEvNECKExist = false;
	public boolean emsEvSUPRETExist = false;
	public boolean emsEvVENTExist = false;
	public boolean emsEvHOTSKExist = false;
	public boolean emsEvRUNLSExist = false;

	public void reset() {
		// detfac exist or not
		detExhTHCExist = false;
		detExhCOExist = false;
		detExhPMExist = false;
		detExhNOXExist = false;

		// emsfac exist or not
		emsExhTHCExist = false;
		emsExhCOExist = false;
		emsExhNOXExist = false;
		emsExhPMExist = false;
		emsBSFCExist = true; // always there
		emsCRANKExist = false;
		emsSPILLAGEExist = false;
		emsEvDiuExist = false;
		emsEvTANKExist = false;
		emsEvHOSEExist = false;
		emsEvNECKExist = false;
		emsEvSUPRETExist = false;
		emsEvVENTExist = false;
		emsEvHOTSKExist = false;
		emsEvRUNLSExist = false;
	}
}
