/**************************************************************************************************
 * @(#)INode.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph;

import java.util.*;

/**
 * Represent a node in a directed graph of nodes and resources.
 * Nodes consume and populate resources.  Nodes are also given
 * a linear sequence that represents a known single-threaded
 * execution sequence that would properly sequence the input/output
 * dependencies.
 * 
 * @author		Wesley Faler
 * @version		2010-01-16
**/
public interface INode {
	/**
	 * Obtain the resources that are required by the node.
	 * @return an Iterator to the set of IResource objects that are required
	 * by the node.  Will be null if there are no inputs.
	**/
	Iterator<IResource> getInputs();

	/**
	 * Obtain the resources that are written to by the node.
	 * @return an Iterator to the set of IResource objects that are updated
	 * by the node.  Will be null if there are no outputs.
	**/
	Iterator<IResource> getOutputs();

	/**
	 * Obtain the set of nodes that should be executed immediately prior
	 * to this node.
	 * @return a list of INode objects that generate a resource needed by
	 * the node.
	**/
	ArrayList<INode> getPredecessors();

	/**
	 * Obtain the set of nodes that should be executed immediately after
	 * to this node.
	 * @return a list of INode objects that consume a resource created by
	 * the node.
	**/
	ArrayList<INode> getSuccessors();

	/**
	 * Obtain the node that is considered the linear predecessor of this
	 * node.
	 * @return the INode that is the linear predecessor of this node
	**/
	INode getLinearPredecessor();
}
