/**************************************************************************************************
 * @(#)ModelYearMapperTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.sql.SQLException;
import junit.framework.*;

/**
 * Test Case for the ModelYearMapper class
 *
 * @author		Wesley Faler
 * @version		2012-04-08
**/
public class ModelYearMapperTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public ModelYearMapperTest(String name) {
		super(name);
	}

	/**
	 * Implements the test case(s).
	**/
	public void testFindAndConvert() {
		ModelYearMapper mapper = new ModelYearMapper();
		mapper.addToMappings(1960,1989,1956,1985);
		mapper.addToMappings(1990,1999,1986,1995);

		//mapper.addToMappings(1960,1999,1956,1995);

		mapper.addToMappings(2000,2005,1997,2002);
		mapper.addToMappings(2006,2008,2005,2007);
		mapper.addToMappings(2009,2050,2009,2050);

		String input = "testing";
		String output = mapper.findAndConvert(input);
		assertEquals("Failed non-match test",input,output);
		
		input = "where mymap(mwo.modelYearID) >= startModelYearID and mymap(mwo.modelYearID) <= endModelYearID";
		output = mapper.findAndConvert(input);
		System.out.println(output);
		assertFalse("Failed match test",input.equals(output));

		input = "where mwo.modelYearID >= myrmap(startModelYearID) and mwo.modelYearID <= myrmap(endModelYearID)";
		output = mapper.findAndConvert(input);
		System.out.println(output);
		assertFalse("Failed reverse match test",input.equals(output));
		
		input = "insert ignore into PollutantProcessMappedModelYear (polProcessID,modelYearID,modelYearGroupID,fuelMYGroupID,IMModelYearGroupID)\n"
				+ " select\n"
				+ " 	polProcessID,\n"
				+ " 	MYRMAP(modelYearID) as modelYearID,\n"
				+ " 	modelYearGroupID,\n"
				+ " 	fuelMYGroupID,\n"
				+ " 	IMModelYearGroupID\n"
				+ " from pollutantProcessModelYear";
		output = mapper.findAndConvert(input);
		System.out.println(output);
	}
}
