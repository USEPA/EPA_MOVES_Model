/**************************************************************************************************
 * @(#)MasterLoopableThread.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.CompilationFlags;
import gov.epa.otaq.moves.common.MOVESThread;
import gov.epa.otaq.moves.common.BundleManifest;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import java.sql.*;

/**
 * A worker thread used to execute MasterLoopable objects. The MasterLoop starts a pool of these
 * threads to service the queue of MasterLoopables. Each thread pulls a MasterLoopable from the
 * queue, calls the MasterLoopable's cleanup or execution methods, and then notifies the Master
 * Loop when the method is done.
 *
 * @author		Wesley Faler
 * @version		2013-11-29
**/
public class MasterLoopableThread extends MOVESThread {
	/** The loop object that owns this thread. **/
	public MasterLoop owningLoop;

	/**
	 * Standard Constructor
	 * @param owningLoop The loop object that owns this thread.
	**/
	public MasterLoopableThread(MasterLoop owningLoop) {
		super(owningLoop, "MasterLoopableThread");

		this.owningLoop = owningLoop;
	}

	/**
	 * Performs main thread functionality. Waits for a MasterLoopable to be returned, calls the
	 * MasterLoopable's cleanup or execute method, and notifies the MasterLoop when the method
	 * is done.
	 * @return Returns true if this thread should continue running; false if this thread
	 * should exit.
	 * @throws InterruptedException This is typically thrown when a sleep() or wait() call
	 * is interrupted.
	**/
	protected boolean threadIterationGo() throws InterruptedException {
		MasterLoopable loopable = owningLoop.getNextLoopableForProcessing();
		if(loopable == null) {
			return true;
		}
		Connection outputDB = null;
		BundleManifest manifest = new BundleManifest();
		manifest.copyFrom(MOVESEngine.theInstance.masterFragment);
		try {
			if(owningLoop.context.isCleanUp) {
				if(!ExecutionRunSpec.shouldSaveData(loopable) ||
						MOVESInstantiator.isSpecialCaseForCleanup(loopable)) {
					int eventRecordID =
							MOVESEngine.logEventStart("Clean " + loopable.getClass().getName());
					MasterLoopContext context = owningLoop.context.copyForLoopable();
					context.owningLoop = owningLoop;
					context.loopable = loopable;
					manifest.context = context.toBundleManifestContext();
					long bundleStartMillis = System.currentTimeMillis();

					try {
						outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
						bundleStartMillis = System.currentTimeMillis();
					} catch(Exception e) {
						manifest = null;
					} finally {
						if(outputDB != null) {
							DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT,outputDB);
							outputDB = null;
						}
					}

					if(!CompilationFlags.GENERATE_CALCULATOR_INFO_DOCUMENTATION) {
						loopable.cleanDataLoop(context);
					}

					MOVESEngine.logEventStop(eventRecordID);
					if(manifest != null) {
						try {
							outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
							long bundleEndMillis = System.currentTimeMillis();
							manifest.recordEvent(outputDB,true,MOVESEngine.theInstance.getActiveRunID(),(float)((bundleEndMillis-bundleStartMillis)/1000.0),null);
						} catch(Exception e) {
							manifest = null;
						} finally {
							if(outputDB != null) {
								DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT,outputDB);
								outputDB = null;
							}
						}
					}
				}
			} else {
				int eventRecordID =
						MOVESEngine.logEventStart("Execute " + loopable.getClass().getName());
				MasterLoopContext context = owningLoop.context.copyForLoopable();
				context.owningLoop = owningLoop;
				context.loopable = loopable;

				manifest.context = context.toBundleManifestContext();
				long bundleStartMillis = System.currentTimeMillis();

				try {
					outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
					bundleStartMillis = System.currentTimeMillis();
				} catch(Exception e) {
					manifest = null;
				} finally {
					if(outputDB != null) {
						DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT,outputDB);
						outputDB = null;
					}
				}

				if(!CompilationFlags.GENERATE_CALCULATOR_INFO_DOCUMENTATION) {
					loopable.executeLoop(context);
				}

				MOVESEngine.logEventStop(eventRecordID);
				if(manifest != null) {
					try {
						outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
						long bundleEndMillis = System.currentTimeMillis();
						manifest.recordEvent(outputDB,true,MOVESEngine.theInstance.getActiveRunID(),(float)((bundleEndMillis-bundleStartMillis)/1000.0),null);
					} catch(Exception e) {
						manifest = null;
					} finally {
						if(outputDB != null) {
							DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT,outputDB);
							outputDB = null;
						}
					}
				}

				// If the loopable created bundles for asynchronous processing (such as is done
				// for some generators, but not with calculators), then clear loopable so it
				// doesn't get reported as complete.  Another thread instance for check for
				// completion.
				synchronized(context) {
					if(context.isDeferred) {
						loopable = null;
						context.isCreatingBundles = false;
					}
				}
				if(loopable == null) {
					// Check for bundle completion in case all results arrived before we got to here
					owningLoop.checkContextsWaitingForBundles();
				}
			}
		} finally {
			if(loopable != null) {
				owningLoop.loopableProcessingComplete(loopable);
			}
			if(outputDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT,outputDB);
				outputDB = null;
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
		// Doesn't need to sleep. go method will wait as long as necessary and detect interrupts.
	}

	/**
	 * This performs one-time startup activities prior to threaded operation.
	 * @return Should the thread continue to run
	**/
	protected boolean startupInThread() {
		return true;
	}

	/** This performs one-time shutdown activities after threaded operation. **/
	protected void shutdownInThread() {

	}
}
