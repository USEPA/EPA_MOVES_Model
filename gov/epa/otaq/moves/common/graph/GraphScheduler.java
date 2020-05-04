/**************************************************************************************************
 * @(#)GraphScheduler.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph;

import java.util.*;

/**
 * Container for a directed graph of nodes and resources.
 * 
 * @author		Wesley Faler
 * @version		2010-01-16
**/
public class GraphScheduler {
	/** The graph **/
	GraphHolder graph;

	/** Set of nodes that can be executed immediately **/
	ArrayList<INode> ready = new ArrayList<INode>();
	/**
	 * Set of nodes that are still be executed but do not yet have
	 * required predecessors completed.
	**/
	ArrayList<INode> todo = new ArrayList<INode>();
	/** Set of nodes that have been fully executed **/
	ArrayList<INode> done = new ArrayList<INode>();

	/**
	 * Constructor
	 * @param graphToUse the graph
	**/
	public GraphScheduler(GraphHolder graphToUse) {
		graph = graphToUse;
		// Place all root items into the ready queue
		for(Iterator<INode> i=graph.roots.iterator();i.hasNext();) {
			ready.add(i.next());
		}
		// Place all other nodes into the todo queue in reverse order
		for(INode cursor=graph.linearTail;cursor!=null;cursor=cursor.getLinearPredecessor()) {
			if(!ready.contains(cursor)) {
				todo.add(0,cursor);
			}
		}
	}

	/**
	 * Request a node to be executed and optionally mark a previous node as complete.
	 * @param contextID an optional identifier for the context within which the node will be
	 * executed, such as a thread ID.  May be blank or null.  Use of a context allows related
	 * nodes to be executed within related contexts, something intended to bring better cache
	 * usage and minimize resource swapping in underlying database and file systems.
	 * @param nodeJustCompleted a node that was completed by the execution context or null upon
	 * first calling this routine
	 * @return the next node to be executed, null if there are no more nodes
	**/
	public synchronized INode getNextNodeToDo(String contextID, INode nodeJustCompleted) {
		if(nodeJustCompleted != null) {
			done.add(nodeJustCompleted);
			// Check anything waiting for this completed node
			boolean madeChanges = false;
			for(Iterator<INode> i=nodeJustCompleted.getSuccessors().iterator();i.hasNext();) {
				INode s = i.next();
				if(todo.contains(s) && !done.contains(s)) {
					// If all of s's predecessors are done, put s into ready and remove from todo
					boolean areAllDone = true;
					for(Iterator<INode> j=s.getPredecessors().iterator();j.hasNext();) {
						INode p = j.next();
						if(!done.contains(p)) {
							areAllDone = false;
							break;
						}
					}
					if(areAllDone) {
						todo.remove(s);
						ready.add(s);
						madeChanges = true;
					}
				}
			}
			if(madeChanges) {
				notifyAll();
			}
			// Try to do one of the node's successors
			// This technique goes "depth-first" down a chain of related nodes in the hopes
			// of getting good disk and RAM cache efficiency.
			for(Iterator<INode> i=nodeJustCompleted.getSuccessors().iterator();i.hasNext();) {
				INode s = i.next();
				if(ready.contains(s)) {
					ready.remove(s);
					notifyAll();
					return s;
				}
			}
		}
		// While there are no nodes ready yet still nodes that could become ready...
		while(ready.size() <= 0 && todo.size() > 0) {
			// ... wait for another thread to indicate there have been changes.
			try {
				wait();
			} catch(InterruptedException e) {
				break;
			}
		}
		// If there are nodes ready to be processed, grab the lead one and return.
		if(ready.size() > 0) {
			return ready.remove(0);
		}
		// Nothing to do and no reason to wait for more to do later.
		return null;
	}
}
