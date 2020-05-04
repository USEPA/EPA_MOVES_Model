/**************************************************************************************************
 * @(#)DistributedWorkFileName.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.File;
import java.io.FilenameFilter;
import java.util.*;

/**
 * Identifies a file containing a bundle of work for a distributed processing system. The MOVES
 * system bundles units of work into JAR files for processing by workers in a distributed work
 * environment. The Distributed Work File Name uniquely identifies the bundle of work, the 
 * Master process responsible for seeing the work done, the worker currently working on it, and
 * the current processing state of the work. The DistributedWorkFileName class contains methods for
 * generating a distributed work file's name and for returning filters that can be used to search
 * for a work file.
 *
 * @author		Wesley Faler
 * @version		2010-02-06
**/
public class DistributedWorkFileName {
	/**
	 * The Master ID (the process responsible for the bundle of work). 
	**/
	public String mid;

	/**
	 * The Queue ID (the bundle of work).
	**/
	public String qid;

	/** The purpose of the work unit **/
	public DistributedWorkFilePurpose purpose = DistributedWorkFilePurpose.CALCULATOR;

	/**
	 * The processing state of the work unit.
	**/
	public DistributedWorkFileState state;
	
	/**
	 * The ID of the worker that is processing this bundle of work. Only valid when state is
	 * DistributedWorkFileState.IN_PROGRESS
	**/
	public String wid;

	/** Utility class for the combination of purpose and state **/
	static class PurposeStateCombination {
		DistributedWorkFilePurpose purpose;
		DistributedWorkFileState state;
		String combinedText;
	}
	/** PurposeStateCombination objects keyed by their combined text **/
	static TreeMapIgnoreCase combinations = new TreeMapIgnoreCase();

	/**
	 * Creates a DistributedWorkFileName object from a source file name.
	 * @param sourceFileName The source file name to use
	 * @return The resulting parsed name object. This will be null if the source name was
	 * not correctly formatted.
	**/
	static public DistributedWorkFileName createFrom(String sourceFileName) {
		final int minStringSections = 3;
		final int maxStringSections = 4;
		
		String sections[] = new String[maxStringSections];
		int currentIndex = 0;
		for (int i = 0; i < maxStringSections; i++) {
			if(currentIndex >= 0) {
				int nextDelimiterIndex = sourceFileName.indexOf('_', currentIndex);

				if(nextDelimiterIndex < 0) {
					sections[i] = sourceFileName.substring(currentIndex);
					currentIndex = nextDelimiterIndex;
				} else {
					sections[i] = sourceFileName.substring(currentIndex, nextDelimiterIndex);
					currentIndex = nextDelimiterIndex + 1;
				}
			}
		}
		DistributedWorkFileName result = new DistributedWorkFileName();
		result.mid = sections[0];
		result.qid = sections[1];
		result.state = DistributedWorkFileState.getByDescription(sections[2]);
		if(result.state != null) {
			// Since the state matched exactly, this implies the calculator purpose since it is not
			// written into the file name.
			result.purpose = DistributedWorkFilePurpose.CALCULATOR;
		} else { // Decode the combined purpose and state
			synchronized(combinations) {
				if(combinations.size() == 0) { // Fill with all combinations if not already filled
					for(Iterator<DistributedWorkFilePurpose> i=DistributedWorkFilePurpose.allPurposes.iterator();i.hasNext();) {
						DistributedWorkFilePurpose p = i.next();
						for(Iterator<DistributedWorkFileState> j=DistributedWorkFileState.allStates.iterator();j.hasNext();) {
							PurposeStateCombination c = new PurposeStateCombination();
							c.state = j.next();
							c.purpose = p;
							if(c.purpose == DistributedWorkFilePurpose.CALCULATOR) {
								c.combinedText = c.state.toString();
							} else {
								c.combinedText = c.purpose.toString() + c.state.toString();
							}
							combinations.put(c.combinedText,c);
						}
					}
				}
				if(sections[2] != null && sections[2].length() > 0) {
					PurposeStateCombination c = (PurposeStateCombination)combinations.get(sections[2]);
					if(c != null) {
						result.purpose = c.purpose;
						result.state = c.state;
					}
				}
			}
		}
		result.wid = sections[3];
		
		if(result.isValid()) {
			return result;
		} else {
			return null;
		}
	}

	/**
	 * Builds a variant of the given file path using the specified distributed processing state.
	 * @param sourcePath The original file path.
	 * @param newState The new state to change the work item to.
	 * @return The modified file path.
	**/
	static public File alterFilePathState(File sourcePath, DistributedWorkFileState newState) {
		DistributedWorkFileName workFileName = createFrom(sourcePath.getName());
		if(workFileName == null) {
			return null;
		}
		workFileName.state = newState;
		return new File(sourcePath.getParent(), workFileName.toString());
	}

	/**
	 * Builds a FileNameFilter object that will find files with the specified elements.
	 * @param mid The Master ID query string. '*' are treated as wildcards.
	 * @param qid The work item queue ID query string. '*' are treated as wildcards.
	 * @param state The work item state. null is treated as a wild card.
	 * @param wid The worker ID query string. '*' are treated as wildcards.
	 * @return The FileNameFilter object.
	**/
	static public FilenameFilter buildFileNameFilter(String mid, String qid,
			DistributedWorkFileState state, String wid) {
		return buildFileNameFilter(mid,qid,null,state,wid);
	}

	/**
	 * Builds a FileNameFilter object that will find files with the specified elements.
	 * @param mid The Master ID query string. '*' are treated as wildcards.
	 * @param qid The work item queue ID query string. '*' are treated as wildcards.
	 * @param purpose The work item purpose.  null is treated as a wild card.
	 * @param state The work item state. null is treated as a wild card.
	 * @param wid The worker ID query string. '*' are treated as wildcards.
	 * @return The FileNameFilter object.
	**/
	static public FilenameFilter buildFileNameFilter(String mid, String qid,
			DistributedWorkFilePurpose purpose, DistributedWorkFileState state, String wid) {
		// Build the filter remembering that calculator purposes are never expressed in the file name, only assumed.
		String filterText = mid + "_" + qid + "_";
		if(purpose == null && state == null) {
			filterText += "*";
		} else if(purpose == null && state != null) { // purpose is null but state is not null
			filterText += "*" + state.toString();
		} else if(purpose != null && state == null) { // purpose is not null but state is
			if(purpose == DistributedWorkFilePurpose.CALCULATOR) {
				filterText += "*";
			} else {
				filterText += purpose.toString() + "*";
			}
		} else { // neither purpose nor state is null
			if(purpose == DistributedWorkFilePurpose.CALCULATOR) {
				filterText += state.toString();
			} else {
				filterText += purpose.toString() + state.toString();
			}
		}
		if(state == DistributedWorkFileState.IN_PROGRESS) {
			filterText += "_" + wid;
		}
		return new WildCardFileNameFilter(filterText);
	}

	/**
	 * Default Constructor. Constructs blank object.
	**/
	public DistributedWorkFileName() {
	}
	
	/**
	 * Determines if this is a valid work file name.
	 * @return Indicates if this is a valid file name.
	**/
	public boolean isValid() {
		if((mid == null) || (mid.length() < 1)) {
			return false;
		}
		if((qid == null) || (qid.length() < 1)) {
			return false;
		}
		if(purpose == null) {
			return false;
		}
		if(state == null) {
			return false;
		}
		// wid must be used in the state IN_PROGRESS and only in this state.
		if((state == DistributedWorkFileState.IN_PROGRESS) == (wid == null)) {
			return false;
		}
		return true;
	}
	
	/**
	 * Gets the complete textual file name
	 * @return The complete textual file name
	**/
	public String toString() {
		return mid + "_" + qid + "_"
			+ (purpose == DistributedWorkFilePurpose.CALCULATOR? "" : purpose.toString())
			+ state.toString()
			+ ((state == DistributedWorkFileState.IN_PROGRESS) ? ("_" + wid) : (""));
	}
}
