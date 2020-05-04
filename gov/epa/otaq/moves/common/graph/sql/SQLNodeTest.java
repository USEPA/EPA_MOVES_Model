/**************************************************************************************************
 * @(#)SQLNodeTest.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph.sql;

import gov.epa.otaq.moves.common.graph.*;
import junit.framework.*;
import java.util.*;
import java.io.*;

/**
 * Test Case for the SQLNode class and its parsing
 *
 * @author		Wesley Faler
 * @version		2010-04-19
**/
public class SQLNodeTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public SQLNodeTest(String name) {
		super(name);
	}

	/**
	 * Tests SQL standardization
	**/
	public void testStandardization() {
		String inputSQL = "SeleCT A, B ,C,    'Hello' as D FROM testing where (A+(B*C))<3 or t>=7 and q='tes''ting, this (thing)'";
		String expectedSQL = "select a , b , c , 'hello' as d from testing where ( a + ( b * c ) ) < 3 or t > = 7 and q = 'tes''ting,\u00ffthis\u00ff(thing)'";
		String outputSQL = SQLNode.standardizeStatement(inputSQL);
		if(!expectedSQL.equals(outputSQL)) {
			System.out.println("Got standardized SQL: \"" + outputSQL + "\"");
			System.out.println("Expected SQL: \"" + expectedSQL + "\"");
		}
		assertEquals("Did not standardize SQL",expectedSQL,outputSQL);

		// Test splitting
		String[] parts = outputSQL.split("\\s");
		assertEquals("Last token is not the quoted text","'tes''ting,\u00ffthis\u00ff(thing)'",parts[parts.length-1]);
		parts = SQLNode.standardizeAfterSplit(parts);
		assertEquals("Did not restore spaces in quoted text","'tes''ting, this (thing)'",parts[parts.length-1]);
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode1() {
		String sql = "flush tables";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|" });
		hasOutputs(node,new String[] { "|sql|" });

		sql = "\t\t\tflush  \t \t\t  \t             TaBLes\t\n";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|" });
		hasOutputs(node,new String[] { "|sql|" });

		sql = "\r\r\n\t\tflush  \t \n\r  \t             TaBLes   ";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|" });
		hasOutputs(node,new String[] { "|sql|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode2() {
		String sql = "truncate table TestTableName";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|", "|sql|testtablename|" });
		hasOutputs(node,new String[] { "|sql|testtablename|" });

		sql = "truncate TestTableName";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|", "|sql|testtablename|" });
		hasOutputs(node,new String[] { "|sql|testtablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode3() {
		String sql = "drop table TestTableName";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|", "|sql|testtablename|" });
		hasOutputs(node,new String[] { "|sql|testtablename|" });

		sql = "drop table if exists TestTableName";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|", "|sql|testtablename|" });
		hasOutputs(node,new String[] { "|sql|testtablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode4() {
		String sql = "select * from ageCategory";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|agecategory|" });
		hasOutputs(node,null);
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode5() {
		String sql = "select * into outfile 'C:/tESt/DaTAFile.txt' from ageCategory";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|agecategory|" });
		hasOutputs(node,new String[] { "|file|c:/test/datafile.txt|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode6() {
		String sql = "create table theTableName ( someColumn int not null primary key )";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "create table if not exists theTableName ( someColumn int not null primary key )";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode7() {
		String sql = "create table theTableName select a, b, c from tableAlpha inner join beta on x=c";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|tablealpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "create table if not exists theTableName select a, b, c from tableAlpha inner join beta on x=c";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|tablealpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode8() {
		String sql = "create table theTableName like someOtherTable";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|someothertable|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "create table if not exists theTableName like someOtherTable";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|someothertable|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode9() {
		String sql = "insert into theTableName (A,B) values (1,2)";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert ignore into theTableName (A,B) values (1,2)";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert replace into theTableName (A,B) values (1,2)";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode10() {
		String sql = "insert into theTableName (A,B) select X, Y from alpha, beta";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert into theTableName (selectA, selectionB, Cselect ) select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert into theTableName select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert ignore into theTableName select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert ignore into theTableName (selectA, selectionB, Cselect ) select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert ignore into theTableName select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert replace into theTableName select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert replace into theTableName (selectA, selectionB, Cselect ) select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "insert replace into theTableName select X, Y from alpha, beta";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|sql|alpha|", "|sql|beta|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode11() {
		String sql = "analyze table theTableName";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode12() {
		String sql = "delete from table theTableName";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "delete from table theTableName where x>2";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode13() {
		String sql = "alter table theTableName add (\n\tkey (countyID),\n\tkey (yearID) )";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode14() {
		String sql = "create index someIndexName on theTableName (\ncountyID, yearID)";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "create unique index someIndexName on theTableName (\ncountyID, yearID)";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode15() {
		String sql = "LOAD DATA INFILE 'c:/testFile.txt' INTO TABLE theTableName FIELDS TERMINATED BY ',' IGNORE 1 LINES ";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|file|c:/testfile.txt|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "LOAD DATA INFILE 'c:/testFile.txt' IGNORE INTO TABLE theTableName FIELDS TERMINATED BY ',' IGNORE 1 LINES ";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|file|c:/testfile.txt|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });

		sql = "LOAD DATA INFILE 'c:/testFile.txt' REPLACE INTO TABLE theTableName FIELDS TERMINATED BY ',' IGNORE 1 LINES ";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|thetablename|", "|file|c:/testfile.txt|" });
		hasOutputs(node,new String[] { "|sql|thetablename|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode16() {
		String sql = "update someTable set A=3 where B<2";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|" });
		hasOutputs(node,new String[] { "|sql|sometable|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode17() {
		String sql = "update someTable, someOtherTable set A=3 where B<2 and C=D";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|" });
		hasOutputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|" });

		sql = "update someTable ,someOtherTable,gamma set A=3 where B<2 and C=D";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|", "|sql|gamma|" });
		hasOutputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|", "|sql|gamma|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode18() {
		String sql = "select * from someTable, someOtherTable where 4<Q";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|" });
		hasOutputs(node,null);

		sql = "select * from someTable, someOtherTable";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|" });
		hasOutputs(node,null);

		sql = "select * from someTable ,someOtherTable,gamma where B<2 and C=D";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|", "|sql|gamma|" });
		hasOutputs(node,null);

		sql = "select * from someTable ,someOtherTable,gamma";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|", "|sql|gamma|" });
		hasOutputs(node,null);
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode19() {
		String sql = "select * from someTable as T1, whereOtherwhere where 4<Q";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|whereotherwhere|" });
		hasOutputs(node,null);

		sql = "select * from someTable T1, whereOtherTable";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|whereothertable|" });
		hasOutputs(node,null);

		sql = "select aFrom, bfrom from someTable ,someOtherTable T2,gamma as T3 where B<2 and C=D";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|", "|sql|gamma|" });
		hasOutputs(node,null);

		sql = "select * from someTable ,someOtherTable as T2,gamma";
		node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|sometable|", "|sql|someothertable|", "|sql|gamma|" });
		hasOutputs(node,null);
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode20() {
		String sql = "select tableAlpha.*, whereBwhere , fromC , dfrom , TG.*"
				+ " from tableAlpha, t17 ,nextTable, tkat"
				+ " inner join tableBeta on (tableAlpha.a=b),"
				+ "tableGamma as TG"
				+ " left outer join tableDelta using (Q)"
				+ " inner join tableZeta as Z";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|tablealpha|", "|sql|t17|",
				"|sql|nexttable|", "|sql|tkat|",
				"|sql|tablebeta|", "|sql|tablegamma|",
				"|sql|tabledelta|", "|sql|tablezeta|" });
		hasOutputs(node,null);
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode21() {
		String sql = "drop procedure testproc";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|" });
		hasOutputs(node,new String[] { "|sql|" });
	}

	/**
	 * Tests SQLNode and its parsing
	**/
	public void testSQLNode22() {
		String sql = "create procedure testproc";
		SQLNode node = new SQLNode(null,sql);
		assertEquals("Did not store original SQL properly",sql,node.toString());
		hasInputs(node,new String[] { "|sql|" });
		hasOutputs(node,new String[] { "|sql|" });
	}

	/**
	 * Verify a node's input resources.
	 * @param node SQLNode to be examined
	 * @param inputNames full resource names that are to be found
	**/
	private void hasInputs(SQLNode node, String[] inputNames) {
		// Ensure everything in inputNames has been found in the node
		if(inputNames != null) {
			for(int i=0;i<inputNames.length;i++) {
				String text = inputNames[i];
				assertTrue("Missing input \"" + text + "\"",isInList(text,node.getInputs()));
			}
		}
		// Ensure enverything found in the node is in inputNames
		for(Iterator<IResource> i=node.getInputs();i!=null && i.hasNext();) {
			String candidateText = i.next().getResourcePath().toLowerCase();
			boolean found = false;
			if(inputNames != null) {
				for(int j=0;j<inputNames.length;j++) {
					if(inputNames[j].equals(candidateText)) {
						found = true;
						break;
					}
				}
			}
			assertTrue("Extra input \"" + candidateText + "\"",found);
		}
	}

	/**
	 * Verify a node's output resources.
	 * @param node SQLNode to be examined
	 * @param outputNames full resource names that are to be found
	**/
	private void hasOutputs(SQLNode node, String[] outputNames) {
		// Ensure everything in outputNames has been found in the node
		if(outputNames != null) {
			for(int i=0;i<outputNames.length;i++) {
				String text = outputNames[i];
				assertTrue("Missing output \"" + text + "\"",isInList(text,node.getOutputs()));
			}
		}
		// Ensure enverything found in the node is in outputNames
		for(Iterator<IResource> i=node.getOutputs();i!=null && i.hasNext();) {
			String candidateText = i.next().getResourcePath().toLowerCase();
			boolean found = false;
			if(outputNames != null) {
				for(int j=0;j<outputNames.length;j++) {
					if(outputNames[j].equals(candidateText)) {
						found = true;
						break;
					}
				}
			}
			assertTrue("Extra output \"" + candidateText + "\"",found);
		}
	}

	/**
	 * Utility to find a named resource within a list of resources.
	 * @param text full name of a resource
	 * @param resources the set of resources to be considered
	 * @return true if a resource with the exact name given by text is found
	**/
	private boolean isInList(String text, Iterator<IResource> resources) {
		if(resources != null && text != null && text.length() > 0) {
			for(;resources.hasNext();) {
				IResource candidate = resources.next();
				String candidatePath = candidate.getResourcePath().toLowerCase();
				if(candidatePath.equals(text)) { // be case-sensitive here, we want an exact match
					return true;
				}
			}
		}
		return false;
	}
}
