/**************************************************************************************************
 * @(#)GraphTest.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph;

import junit.framework.*;
import java.util.*;
import java.io.*;

/**
 * Test Case for the graph classes
 *
 * @author		Wesley Faler
 * @version		2010-01-17
**/
public class GraphTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public GraphTest(String name) {
		super(name);
	}

	/**
	 * Tests GraphUtilities.areRelated()
	**/
	public void testAreRelated1() {
		BasicNodeImpl producer = new BasicNodeImpl();
		producer.addOutput(new BasicResourceImpl("sql|table1|subset1"));
		producer.addOutput(new BasicResourceImpl("sql|table2|"));

		BasicNodeImpl consumer = new BasicNodeImpl();
		consumer.addInput(new BasicResourceImpl("sql|table1"));

		assertTrue("areRelated failed, expected a relation",GraphUtilities.areRelated(producer,consumer));
	}

	/**
	 * Tests GraphUtilities.areRelated()
	**/
	public void testAreRelated2() {
		BasicNodeImpl producer = new BasicNodeImpl();
		producer.addOutput(new BasicResourceImpl("sql|table2|subset1"));
		producer.addOutput(new BasicResourceImpl("sql|table3|"));

		BasicNodeImpl consumer = new BasicNodeImpl();
		consumer.addInput(new BasicResourceImpl("sql|table1"));
		consumer.addInput(new BasicResourceImpl("sql|t"));
		consumer.addInput(new BasicResourceImpl("sql|table4"));
		consumer.addInput(new BasicResourceImpl("file|somefile.txt"));

		assertTrue("areRelated failed, no relation expected",!GraphUtilities.areRelated(producer,consumer));
	}

	/**
	 * Tests GraphUtilities.areRelated()
	**/
	public void testAreRelated3() {
		BasicNodeImpl producer = new BasicNodeImpl();
		producer.addOutput(new BasicResourceImpl("sql|table2|"));
		producer.addOutput(new BasicResourceImpl("sql|table1|"));

		BasicNodeImpl consumer = new BasicNodeImpl();
		consumer.addInput(new BasicResourceImpl("sql|TabLE1"));

		assertTrue("areRelated failed, expected a relation",GraphUtilities.areRelated(producer,consumer));
	}

	/**
	 * Tests GraphUtilities.areRelated()
	**/
	public void testAreRelated4() {
		BasicNodeImpl producer = new BasicNodeImpl();
		producer.addOutput(new BasicResourceImpl("sql|table1"));
		producer.addOutput(new BasicResourceImpl("sql|table2|"));

		BasicNodeImpl consumer = new BasicNodeImpl();
		consumer.addInput(new BasicResourceImpl("sql|table1|SUBSet1"));

		assertTrue("areRelated failed, expected a relation",GraphUtilities.areRelated(producer,consumer));
	}

	/**
	 * Tests GraphUtilities.areRelated()
	**/
	public void testAreRelated5() {
		BasicNodeImpl producer = new BasicNodeImpl();
		producer.addOutput(new BasicResourceImpl("sql|table1|subset1"));
		producer.addOutput(new BasicResourceImpl("sql|table2|"));

		BasicNodeImpl consumer = new BasicNodeImpl();
		consumer.addInput(new BasicResourceImpl("sql|table1|SUBSet2"));

		assertTrue("areRelated failed, no relation expected",!GraphUtilities.areRelated(producer,consumer));
	}

	/**
	 * Tests GraphHolder.add()
	**/
	public void testGraphHolder1() {
		assertNotNull("Did not get GraphHolder",buildAndTestGraph1());
	}

	/**
	 * Test graph visualization
	 * @throws Exception if anything goes wrong
	**/
	public void testGraphViz1() throws Exception {
		File folder = new File("testdata/graph");
		if(!folder.exists()) {
			folder.mkdirs();
		}
		assertTrue("Unable to create testdata/graph folder",folder.exists());

		File gfile = new File(folder,"graph1.dot");
		if(gfile.exists()) {
			gfile.delete();
		}
		GraphHolder graph = buildAndTestGraph1();
		graph.generateGraphVizFile(gfile,false);
		assertTrue("Did not create graph1.dot file",gfile.exists());

		File gfileLinear = new File(folder,"graph1linear.dot");
		if(gfileLinear.exists()) {
			gfileLinear.delete();
		}
		graph.generateGraphVizFile(gfileLinear,true);
		assertTrue("Did not create graph1linear.dot file",gfileLinear.exists());
	}

	/**
	 * Build a graph, testing for proper connectivity along the way.
	 * @return a graph which can be used for further testing
	**/
	private GraphHolder buildAndTestGraph1() {
		GraphHolder graph = new GraphHolder();

		BasicNodeImpl b = new BasicNodeImpl(null,"b");
		b.addOutput(new BasicResourceImpl("sql|table1|subset2"));
		graph.add(b);
		assertEquals("Wrong number of root items",1,graph.roots.size());

		BasicNodeImpl a = new BasicNodeImpl(b,"a");
		a.addOutput(new BasicResourceImpl("sql|table1|subset1"));
		graph.add(a);
		assertEquals("Wrong number of root items",2,graph.roots.size());

		BasicNodeImpl c = new BasicNodeImpl(a,"c");
		c.addInput(new BasicResourceImpl("sql|table1"));
		c.addOutput(new BasicResourceImpl("sql|table2"));
		graph.add(c);
		assertEquals("Wrong number of root items",2,graph.roots.size());
		assertEquals("c has wrong number of predecessors",2,c.getPredecessors().size());
		assertTrue("c does not use a as a predecessor",c.getPredecessors().contains(a));
		assertTrue("c does not use b as a predecessor",c.getPredecessors().contains(b));
		assertTrue("a does not use c as a successor",a.getSuccessors().contains(c));
		assertTrue("b does not use c as a successor",b.getSuccessors().contains(c));
		assertEquals("a has the wrong number of successors",1,a.getSuccessors().size());
		assertEquals("b has the wrong number of successors",1,b.getSuccessors().size());

		BasicNodeImpl e = new BasicNodeImpl(c,"e");
		e.addOutput(new BasicResourceImpl("sql|table4"));
		graph.add(e);
		assertEquals("Wrong number of root items",3,graph.roots.size());

		BasicNodeImpl d = new BasicNodeImpl(e,"d");
		d.addInput(new BasicResourceImpl("sql|table1|subset2"));
		d.addOutput(new BasicResourceImpl("sql|table3"));
		graph.add(d);
		assertEquals("Wrong number of root items",3,graph.roots.size());
		assertEquals("d has wrong number of predecessors",1,d.getPredecessors().size());
		assertTrue("d does not use b as a predecessor",d.getPredecessors().contains(b));
		assertTrue("b does not use d as a successor",b.getSuccessors().contains(d));
		assertEquals("b has the wrong number of successors",2,b.getSuccessors().size());

		assertEquals("graph has the wrong nodeCount",5,graph.nodeCount);

		return graph;
	}

	/**
	 * Tests GraphScheduler
	**/
	public void testGraphScheduler1() {
		GraphHolder graph = buildAndTestGraph1();
		assertNotNull("Did not get GraphHolder",graph);
		GraphScheduler scheduler = new GraphScheduler(graph);

		int howManyExecuted = 0;
		INode previousNode = null;
		TreeSet<Integer> seenNodes = new TreeSet<Integer>();
		ArrayList<INode> sequence = new ArrayList<INode>();
		while(true) {
			INode n = scheduler.getNextNodeToDo(null,previousNode);
			if(n == null) {
				break;
			}
			howManyExecuted++;
			assertFalse("Got too many nodes, expected exactly " + graph.nodeCount,howManyExecuted > graph.nodeCount);
			Integer hash = Integer.valueOf(n.hashCode());
			assertFalse("Duplicate node scheduled",seenNodes.contains(hash));
			seenNodes.add(hash);
			sequence.add(n);
			previousNode = n;
			System.out.println("testGraphScheduler1: execute node " + n.toString());
		}
		assertEquals("Executed wrong number of nodes",graph.nodeCount,howManyExecuted);

		int aIndex = findIndexMandatory(sequence,"a");
		int bIndex = findIndexMandatory(sequence,"b");
		int cIndex = findIndexMandatory(sequence,"c");
		int dIndex = findIndexMandatory(sequence,"d");
		int eIndex = findIndexMandatory(sequence,"e");

		assertTrue("a did not execute before c",aIndex < cIndex);
		assertTrue("b did not execute before c",bIndex < cIndex);
		assertTrue("b did not execute before d",bIndex < dIndex);
	}

	/**
	 * Tests GraphScheduler
	**/
	public void testGraphScheduler2() {
		GraphHolder graph = buildAndTestGraph1();
		assertNotNull("Did not get GraphHolder",graph);
		GraphScheduler scheduler = new GraphScheduler(graph);

		int howManyExecuted = 0;
		TreeSet<Integer> seenNodes = new TreeSet<Integer>();
		ArrayList<INode> sequence = new ArrayList<INode>();
		String[] contexts = { "context1", "context2" };
		INode[] previousNodes = new INode[contexts.length];
		boolean[] contextDone = new boolean[contexts.length];
		int contextDoneCount = 0;

		for(int loopCount=0;loopCount < graph.nodeCount*10 && contextDoneCount < contexts.length;loopCount++) {
			int contextIndex = loopCount % contexts.length;
			if(contextDone[contextIndex]) {
				continue;
			}
			INode n = scheduler.getNextNodeToDo(contexts[contextIndex],previousNodes[contextIndex]);
			if(n == null) {
				contextDone[contextIndex] = true;
				contextDoneCount++;
				continue;
			}
			howManyExecuted++;
			assertFalse("Got too many nodes, expected exactly " + graph.nodeCount,howManyExecuted > graph.nodeCount);
			Integer hash = Integer.valueOf(n.hashCode());
			assertFalse("Duplicate node scheduled",seenNodes.contains(hash));
			seenNodes.add(hash);
			sequence.add(n);
			System.out.println("testGraphScheduler2: context " + contextIndex + " execute node " + n.toString());
			previousNodes[contextIndex] = n;
		}
		assertEquals("Executed wrong number of nodes",graph.nodeCount,howManyExecuted);

		int aIndex = findIndexMandatory(sequence,"a");
		int bIndex = findIndexMandatory(sequence,"b");
		int cIndex = findIndexMandatory(sequence,"c");
		int dIndex = findIndexMandatory(sequence,"d");
		int eIndex = findIndexMandatory(sequence,"e");

		assertTrue("a did not execute before c",aIndex < cIndex);
		assertTrue("b did not execute before c",bIndex < cIndex);
		assertTrue("b did not execute before d",bIndex < dIndex);
	}

	/**
	 * Find a node within a list of nodes, asserting if the index is not found.
	 * @param nodes list of nodes to be searched
	 * @param nodeText text to be checked against INode.toString() on each node
	 * @return index within nodes or -1 if not found
	**/
	private int findIndexMandatory(ArrayList<INode> nodes, String nodeText) {
		int index = findIndex(nodes,nodeText);
		assertTrue("Did not find node \"" + nodeText + "\"",index >= 0);
		return index;
	}

	/**
	 * Find a node within a list of nodes
	 * @param nodes list of nodes to be searched
	 * @param nodeText text to be checked against INode.toString() on each node
	 * @return index within nodes or -1 if not found
	**/
	private int findIndex(ArrayList<INode> nodes, String nodeText) {
		for(int i=0;i<nodes.size();i++) {
			if(nodes.get(i).toString().equalsIgnoreCase(nodeText)) {
				return i;
			}
		}
		return -1;
	}
}
