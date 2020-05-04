/**************************************************************************************************
 * @(#)GraphHolder.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph;

import java.util.*;
import java.io.*;

/**
 * Container for a directed graph of nodes and resources.
 * 
 * @author		Wesley Faler
 * @version		2010-01-16
**/
public class GraphHolder {
	/** First node in the linear sequence **/
	public INode linearHead;
	/** Last node in the linear sequence **/
	public INode linearTail;
	/** Set of nodes that have no dependencies upon other nodes **/
	public ArrayList<INode> roots = new ArrayList<INode>();
	/** Number of nodes in the graph **/
	public int nodeCount = 0;

	/**
	 * Add a node to the linear list and place its location into the
	 * predecessor and successor lists of the existing nodes.  The node
	 * should already be connected to its linear successor.
	 * @param n node to be added
	**/
	public void add(INode n) {
		nodeCount++;
		if(linearHead == null) {
			linearHead = n;
			linearTail = n;
			roots.add(n);
			return;
		}
		INode candidate = linearTail;
		linearTail = n;
		while(candidate != null) {
			if(GraphUtilities.areRelated(candidate,n)) { // If candidate produces what n needs...
				candidate.getSuccessors().add(n);
				n.getPredecessors().add(candidate);
			}
			candidate = candidate.getLinearPredecessor();
		}
		if(n.getPredecessors().size() == 0) {
			roots.add(n);
		}
	}

	/**
	 * Generate a file used to debug graphs.  The GraphViz DOT language is used.
	 * @param gfile file to be populated with DOT-language commands
	 * @param showLinearSequence true if the nodes should be connected via successor/predecessor as well
	 * as via their linear sequence
	 * @throws Exception if anything goes wrong
	**/
	public void generateGraphVizFile(File gfile, boolean showLinearSequence) throws Exception {
		PrintWriter writer = null;
		
		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(gfile),65536));
			writer.println("digraph G {");
			writer.println("\tnode [shape=record];");
			// Write all items in the graph
			for(INode cursor=linearTail;cursor != null;cursor = cursor.getLinearPredecessor()) {
				// Nodes are written as DOT records
				String inputsText = "";
				for(Iterator<IResource> i=cursor.getInputs();i != null && i.hasNext();) {
					IResource r = i.next();
					String path = getGraphVizText(r.getResourcePath());
					if(inputsText.length() > 0) {
						inputsText += "|";
					}
					inputsText += "i: " + path;
				}
				if(inputsText.length() == 0) {
					inputsText = "i: (none)";
				}

				String outputsText = "";
				for(Iterator<IResource> i=cursor.getOutputs();i != null && i.hasNext();) {
					IResource r = i.next();
					String path = getGraphVizText(r.getResourcePath());
					if(outputsText.length() > 0) {
						outputsText += "|";
					}
					outputsText += "o: " + path;
				}
				if(outputsText.length() == 0) {
					outputsText = "o: (none)";
				}

				// Write the node
				String nodeText = getGraphVizText(cursor.toString());
				writer.println("\tnode" + cursor.hashCode() + " [shape=record, label=\"{" + inputsText + "|" + nodeText + "|" + outputsText + "}\"];");
			}
			// Connect all items in the graph
			for(INode cursor=linearTail;cursor != null;cursor = cursor.getLinearPredecessor()) {
				String cursorName = "node" + cursor.hashCode();
				// Connect the cursor to each successor
				for(Iterator<INode> i=cursor.getSuccessors().iterator();i.hasNext();) {
					INode s = i.next();
					String sName = "node" + s.hashCode();
					writer.println("\t" + cursorName + " -> " + sName + ";");
				}
				if(showLinearSequence) {
					// Connect the cursor to its linear predecessor
					if(cursor.getLinearPredecessor() != null) {
						String pName = "node" + cursor.getLinearPredecessor().hashCode();
						writer.println("\t" + pName + " -> " + cursorName + " [style=dotted];");
					}
				}
			}
			writer.println("}");
		} finally {
			if(writer != null) {
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				writer = null;
			}
		}
	}

	/**
	 * Convert general text to that compatible with GraphViz DOT language label statement.
	 * Long text is truncated.
	 * @param text text to be converted
	 * @return text for use within a GraphViz DOT label statement
	**/
	private static String getGraphVizText(String text) {
		if(text == null) {
			return "";
		}
		text = text.trim();
		boolean wasTruncated = false;
		if(text.length() > 50) {
			text = text.substring(0,50);
			wasTruncated = true;
		}
		text = text.replaceAll("\\|","\\\\|");
		if(wasTruncated) {
			text += "...";
		}
		return text;
	}
}
