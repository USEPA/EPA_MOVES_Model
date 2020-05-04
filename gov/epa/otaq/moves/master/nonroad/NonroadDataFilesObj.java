package gov.epa.otaq.moves.master.nonroad;

import java.util.TreeSet;

/**
 * @author Jizhen Zhao @ IE, UNC CH
 * 
 */
public class NonroadDataFilesObj {

	// with subfolders - don't use them

	// // activity DON'T CHANGE
	// public String activityFile = "DATA/ACTIVITY/ACTIVITY.DAT";
	//
	// // allocate DON'T CHANGE
	// public String allocateXrfFile = "DATA/ALLOCATE/ALLOCATE.XRF";
	// public String allAllocateFile = "DATA/ALLOCATE/ALL.ALO";
	// public String fipsFile = "DATA/ALLOCATE/FIPS.DAT";
	//
	// // detfac DON'T CHANGE
	// public String detExhTHCFile = "DATA/DETFAC/EXHTHC.DET";
	// public String detExhCOFile = "DATA/DETFAC/EXHCO.DET";
	// public String detExhPMFile = "DATA/DETFAC/EXHPM.DET";
	// public String detExhNOXFile = "DATA/DETFAC/EXHNOX.DET";
	//
	// // emsfac DON'T CHANGE
	// public String emsExhTHCFile = "DATA/EMSFAC/EXHTHC.EMF";
	// public String emsExhCOFile = "DATA/EMSFAC/EXHCO.EMF";
	// public String emsExhNOXFile = "DATA/EMSFAC/EXHNOX.EMF";
	// public String emsExhPMFile = "DATA/EMSFAC/EXHPM.EMF";
	// public String emsBSFCFile = "DATA/EMSFAC/BSFC.EMF";
	// public String emsCRANKFile = "DATA/EMSFAC/CRANK.EMF";
	// public String emsSPILLAGEFile = "DATA/EMSFAC/SPILLAGE.EMF";
	// public String emsEvDiuFile = "DATA/EMSFAC/EVDIU.EMF";
	// public String emsEvTANKFile = "DATA/EMSFAC/EVTANK.EMF";
	// public String emsEvHOSEFile = "DATA/EMSFAC/EVHOSE.EMF";
	// public String emsEvNECKFile = "DATA/EMSFAC/EVNECK.EMF";
	// public String emsEvSUPRETFile = "DATA/EMSFAC/EVSUPRET.EMF";
	// public String emsEvVENTFile = "DATA/EMSFAC/EVVENT.EMF";
	// public String emsEvHOTSKFile = "DATA/EMSFAC/EVHOTSK.EMF";
	// public String emsEvRUNLSFile = "DATA/EMSFAC/EVRUNLS.EMF";
	//
	// // growth DON'T CHANGE
	// public String growthFile = "DATA/GROWTH/NATION.GRW";
	//
	// // pop: need to be filled
	// public List<String> popFiles = new ArrayList<String>();
	//
	// // retrofit
	// public String retrofitFile = "DATA/RETROFIT/retrotst.dat";
	//
	// // season
	// public String seasonFile = "DATA/SEASON/SEASON.DAT";
	//
	// // tech
	// public String techExhFile = "DATA/TECH/TECH-EXH.DAT";
	// public String techEvpFile = "DATA/TECH/TECH-EVP.DAT";
	//
	// // output
	// public String outMsgFile = "OUTPUTS/NONROAD.MSG";
	// public String outDataFile = "OUTPUTS/NONROAD.OUT";
	// // model year output
	// public String outExhBMYFile = "OUTPUTS/NONROAD.BMY"; //
	// OUTPUTS/NRNMIM.BMX");
	// public String outEvBMYFile = "OUTPUTS/NONROAD.BMV"; //
	// OUTPUTS/NRNMIM.BMV");

	// without subfolders - use this since can use the bundler directly
	// activity DON'T CHANGE
	public String activityFile = "ACTIVITY.DAT";

	// allocate DON'T CHANGE
	public String allocateXrfFile = "ALLOCATE.XRF";
	public String allAllocateFile = "ALL.ALO";
	public String fipsFile = "FIPS.DAT";

	// detfac DON'T CHANGE
	public String detExhTHCFile = "EXHTHC.DET";
	public String detExhCOFile = "EXHCO.DET";
	public String detExhPMFile = "EXHPM.DET";
	public String detExhNOXFile = "EXHNOX.DET";

	// emsfac DON'T CHANGE
	public String emsExhTHCFile = "EXHTHC.EMF";
	public String emsExhCOFile = "EXHCO.EMF";
	public String emsExhNOXFile = "EXHNOX.EMF";
	public String emsExhPMFile = "EXHPM.EMF";
	public String emsBSFCFile = "BSFC.EMF";
	public String emsCRANKFile = "CRANK.EMF";
	public String emsSPILLAGEFile = "SPILLAGE.EMF";
	public String emsEvDiuFile = "EVDIU.EMF";
	public String emsEvTANKFile = "EVTANK.EMF";
	public String emsEvHOSEFile = "EVHOSE.EMF";
	public String emsEvNECKFile = "EVNECK.EMF";
	public String emsEvSUPRETFile = "EVSUPRET.EMF";
	public String emsEvVENTFile = "EVVENT.EMF";
	public String emsEvHOTSKFile = "EVHOTSK.EMF";
	public String emsEvRUNLSFile = "EVRUNLS.EMF";

	// growth DON'T CHANGE
	public String growthFile = "NATION.GRW";

	// pop: need to be filled
	public TreeSet<String> popFiles = new TreeSet<String>();

	// retrofit
	public String retrofitFile = "retrotst.dat";

	// season
	public String seasonFile = "SEASON.DAT";

	// tech
	public String techExhFile = "TECH-EXH.DAT";
	public String techEvpFile = "TECH-EVP.DAT";

	// output
	public String outMsgFile = "NONROAD.MSG";
	public String outDataFile = "NONROAD.OUT";
	// model year output
	public String outExhBMYFile = "NONROAD.BMY"; // OUTPUTS/NRNMIM.BMX");
	public String outEvBMYFile = "NONROAD.BMV"; // OUTPUTS/NRNMIM.BMV");
}
