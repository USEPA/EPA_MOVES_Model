/**************************************************************************************************
 * @(#)WorkerCommandLine.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker;

import gov.epa.otaq.moves.worker.gui.WorkerGUI;

/**
 * Class for Worker Command Line, contains main() which invokes Moves in a "batch" mode (no UI).
 *
 * @author		Wes Faler
 * @version		2010-07-05
**/
public class WorkerCommandLine {
	/** Invokes the worker application. **/
	public static void main(String[] args) {
		WorkerGUI.commandLineMain(args);
	}
}
