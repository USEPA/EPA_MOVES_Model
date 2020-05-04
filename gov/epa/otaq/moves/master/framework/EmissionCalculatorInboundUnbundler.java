/**************************************************************************************************
 * @(#)EmissionCalculatorInboundUnbundler.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.LinkedList;

/**
 * Unbundles JAR files containing units of work that have been completed by a worker. This thread
 * is started by the MOVESEngine just after the Master Loop thread is started. It watches a shared
 * folder for distributed work files that have been marked with a status of done. When it finds a
 * file, it unbundles the file to a temporary folder and passes control to the OutputProcessor for
 * final processing.
 *
 * @author		Wesley Faler
 * @version		2010-10-11
**/
public class EmissionCalculatorInboundUnbundler extends MOVESThread {
	/** Information about the Calculator-level DONE files for a master **/
	public static class MasterDetails {
		public BundleManifest fullManifest = null;
		public int numberOfDONEFiles = 0;
	}

	/**
	 * Obtain information about the Calculator-level DONE files for a master.
	 * @param pd specifies the master to be used and the location of potential DONE files
	 * @return a MasterDetails object with the manifest from the first calculator-level DONE
	 * file found for the master and a count of the number of calculator-level DONE files
	 * available.
	 * @throws IOException if unable to find the file location or to extract details
	**/
	public static MasterDetails getDetails(PDSpec.PDSpecEntry pd) throws IOException {
		MasterDetails details = new MasterDetails();
		details.fullManifest = new BundleManifest();

		FilenameFilter doneFilter = DistributedWorkFileName.buildFileNameFilter(
				pd.masterID, "*", DistributedWorkFilePurpose.CALCULATOR, DistributedWorkFileState.DONE, "*");
		File[] doneFiles = null;
		File pickupFolder = new File(pd.pickupFolderName);
		doneFiles = pickupFolder.listFiles(doneFilter);
		if(doneFiles != null && doneFiles.length > 0) {
			File doneFile = doneFiles[0];
			File temporaryFolder = FileUtilities.createTemporaryFolder(null, "DONEProcessing");
			try {
				LinkedList<File> fileList = JARUtilities.unJarFileToFolder(temporaryFolder, doneFile);
				details.fullManifest.readFromFolder(temporaryFolder);
				details.numberOfDONEFiles = doneFiles.length;
			} catch (Exception exception) {
				/**
				 * @explain An error occurred while processing the results (a .DONE file) returned
				 * by a worker.
				**/
				Logger.logError(exception, "Failed to read manifest from DONE file");
				return null;
			} finally {
				FileUtilities.deleteTemporaryFolder(temporaryFolder);
			}
		}
		return details;
	}

	/**
	 * Obtain information about a Calculator-level DONE files for a master.
	 * @param pd specifies the master to be used and the location of potential DONE files
	 * @return a MasterDetails object with the manifest from the first calculator-level DONE
	 * file found for the master and a count of 1 calculator-level DONE files available.
	 * @throws IOException if unable to find the file location or to extract details
	**/
	public static MasterDetails getDetails(File doneFile) throws IOException {
		MasterDetails details = new MasterDetails();
		details.fullManifest = new BundleManifest();

		File temporaryFolder = FileUtilities.createTemporaryFolder(null, "DONEProcessing");
		try {
			LinkedList<File> fileList = JARUtilities.unJarFileToFolder(temporaryFolder, doneFile);
			details.fullManifest.readFromFolder(temporaryFolder);
			details.numberOfDONEFiles = 1;
		} catch (Exception exception) {
			/**
			 * @explain An error occurred while processing the results (a .DONE file) returned
			 * by a worker.
			**/
			Logger.logError(exception, "Failed to read manifest from DONE file");
			return null;
		} finally {
			FileUtilities.deleteTemporaryFolder(temporaryFolder);
		}

		return details;
	}

	/** The number of distributed work bundles that have been completely processed. **/
	public int processedWorkBundleCount = 0;
	/** The number of iterations that have been completely processed. **/
	public int processedIterationCount = 0;
	/** The number of bundles per iteration **/
	int bundlesPerIteration = -1;
	/** true when the system should act as if all bundles were present **/
	public boolean shouldFinishImmediately = false;
	/** alternate folder to use for locating DONE files **/
	public File pickupFolderOverride = null;
	/** alternate ID to use within the distributed processing system. **/
	public String distributedMasterIDOverride = null;

	/**
	 * Constructor.
	**/
	public EmissionCalculatorInboundUnbundler() {
		OutputProcessor.getTheOutputProcessor().resetPostProcessors();
	}

	/**
	 * This performs one-time startup activities prior to threaded operation.
	 * @return Should the thread continue to run
	**/
	protected boolean startupInThread() {
		OutputProcessor.getTheOutputProcessor().prepareForEstimateUncertainty();
		return true;
	}

	/** This performs one-time shutdown activities after threaded operation. **/
	protected void shutdownInThread() {

	}

	/**
	 * Watch for completed JAR files in a location specified by SystemConfiguration. When found,
	 * unbundle the results and pass the data on to OutputProcessor.
	 * @return Should the thread continue to run
	**/
	public boolean threadIterationGo() {
		Integer totalBundleCount =
				MOVESEngine.theInstance.getHowManyOutboundBundlesWillBeCreated();
		if(totalBundleCount != null) {
			if(bundlesPerIteration==-1) {
				bundlesPerIteration = totalBundleCount.intValue()/
						ExecutionRunSpec.theExecutionRunSpec.getHowManyIterationsWillBePerformed();
			}
			if(processedWorkBundleCount>0) {
				if(processedWorkBundleCount>=bundlesPerIteration*(processedIterationCount+1)) {
					int eventRecordID = MOVESEngine.logEventStart("Final Post Processing");
					OutputProcessor.getTheOutputProcessor().doFinalPostProcessingOnOutputDatabase();
					OutputProcessor.getTheOutputProcessor().performEstimateUncertainty();
					MOVESEngine.logEventStop(eventRecordID);
					processedIterationCount++;
				}
			}
			if(processedWorkBundleCount >= totalBundleCount.intValue()) {
				OutputProcessor.getTheOutputProcessor().cleanUpAfterEstimateUncertainty();
				OutputProcessor.getTheOutputProcessor().saveCMITs();
				// All work bundles have been processed. This thread is complete.
				Logger.log(LogMessageCategory.INFO,"Unbundler has detected completion conditions. Exiting.");
				if(distributedMasterIDOverride == null) {
					Logger.log(LogMessageCategory.INFO,"Generated bundles = " + totalBundleCount.intValue()
							+ " Retrieved bundles = " + processedWorkBundleCount);
				} else {
					Logger.log(LogMessageCategory.INFO," Retrieved bundles = " + processedWorkBundleCount
							+ " of " + totalBundleCount.intValue());
				}
				return false;
			}
		}

		if(shouldFinishImmediately) {
			MOVESEngine.theInstance.onReceivedBundle();
			processedWorkBundleCount++;
			MOVESEngine.theInstance.onProcessedBundle();
			return true;
		}

		// Scan for DONE files, only grabbing CALCULATOR DONE files if picking up DONE files for other masters.
		FilenameFilter doneFilter = DistributedWorkFileName.buildFileNameFilter(
				(distributedMasterIDOverride != null && distributedMasterIDOverride.length() > 0)? distributedMasterIDOverride : SystemConfiguration.getTheSystemConfiguration().distributedMasterID,
				"*", pickupFolderOverride==null? null : DistributedWorkFilePurpose.CALCULATOR, DistributedWorkFileState.DONE, "*");
		File[] doneFiles = null;
		if(pickupFolderOverride != null) {
			doneFiles = pickupFolderOverride.listFiles(doneFilter);
		} else {
			doneFiles = SystemConfiguration.getTheSystemConfiguration().sharedDistributedFolderPath.listFiles(doneFilter);
		}
		if(doneFiles != null) {
			for (int i = 0; i < doneFiles.length; i++) {
				File doneFile = doneFiles[i];
				File temporaryFolder = FileUtilities.createTemporaryFolder(null, "DONEProcessing");
				try {
					DistributedWorkFileName workFileName = DistributedWorkFileName.createFrom(doneFile.getName());
					boolean isCalculatorBundle = workFileName.purpose == DistributedWorkFilePurpose.CALCULATOR;

					if(isCalculatorBundle) {
						MOVESEngine.theInstance.onReceivedBundle();
					}
					LinkedList<File> fileList = JARUtilities.unJarFileToFolder(temporaryFolder, doneFile);
					int eventRecordID = MOVESEngine.logEventStart("Process File From Worker");
					OutputProcessor.getTheOutputProcessor().processWorkerFiles(temporaryFolder, fileList);
					if(isCalculatorBundle) {
						processedWorkBundleCount++;
					}
					doneFile.delete();
					MOVESEngine.logEventStop(eventRecordID);
					if(isCalculatorBundle) {
						MOVESEngine.theInstance.onProcessedBundle();
					} else if(pickupFolderOverride == null) {
						BundleManifest manifest = new BundleManifest();
						if(manifest.containsManifest(temporaryFolder) && manifest.readFromFolder(temporaryFolder)) {
							MasterLoopContext context = new MasterLoopContext();
							context.fromBundleManifestContext(manifest.context);
							MOVESEngine.theInstance.loop.nonCalculatorBundleReceived(context);
						}
					}
				} catch (Exception exception) {
					/**
					 * @explain An error occurred while processing the results (a .DONE file) returned
					 * by a worker.
					**/
					Logger.logError(exception, "Failed to process DONE file");
				} finally {
					MOVESEngine.theInstance.notifyListeners();
					FileUtilities.deleteTemporaryFolder(temporaryFolder);
				}
			}
		}

		return true;
	}

	/**
	 * Called directly after threadIterationGo. Subclasses typically implement a simple sleep
	 * operation here.
	 * @throws InterruptedException This is typically thrown when a sleep() call is interrupted.
	**/
	protected void threadIterationWait() throws InterruptedException {
		sleep(shouldFinishImmediately?100:2000);
	}
}
