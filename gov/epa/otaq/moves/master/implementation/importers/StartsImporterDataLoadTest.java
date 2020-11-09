/**************************************************************************************************
 * @(#)StartsImporterDataLoadTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import gov.epa.otaq.moves.common.DatabaseSelection;
import gov.epa.otaq.moves.common.DatabaseUtilities;
import gov.epa.otaq.moves.common.FileUtilities;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.worker.framework.WorkerConfiguration;
import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * Task1806 - Test Case for the StartsImporter class
 *
 * @author		John Covey
 * @version		2018-03-20
**/
public class StartsImporterDataLoadTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	
	private Connection db = null;
	public StartsImporterDataLoadTest(String name) {
		super(name);
	}

	@Override
    protected void setUp() throws Exception
    {
        super.setUp();
		try {
			WorkerConfiguration.theWorkerConfiguration.loadConfigurationData();
		} catch (IOException e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
		db = getTaskDB();
    }
	
	private Connection getTaskDB() {
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		dbSelection.databaseName = "task1806_county_default_loaded";
		db = dbSelection.databaseName.length() > 0? dbSelection.openConnectionOrNull() : null;
		if(null == db ) {
			// try again to get a connection, but specify an empty string for the database
			// name, in case the current one is invalid, this will at least allow us to get
			// a connection to the server to get the database list
			dbSelection.databaseName = "";
			db = dbSelection.openConnectionOrNull();
		}
		return db;
	}
	
	public void testStartsSourceTyeFractionDropped() {
		try {
			SQLRunner.executeSQL(db,"select count(*) from startsSourceTypeFraction");
		} catch (SQLException e) {
			e.printStackTrace();
			if (!e.getMessage().contains("doesn't exist")) {
				Assert.fail("Table startsSourceTypeFraction should not be found");
			}
			return;
		}
		Assert.fail("Table startsSourceTypeFraction should not be found");
	}

	public void testImportStartsOpModeDistributionDropped() {
		try {
			SQLRunner.executeSQL(db,"select count(*) from importstartsopmodedistribution");
		} catch (SQLException e) {
			e.printStackTrace();
			if (!e.getMessage().contains("doesn't exist")) {
				Assert.fail("Table importstartsopmodedistribution should not be found");
			}
			return;
		}
		Assert.fail("Table startsopmodedistribution should not be found");
	}

	public void testImportStartsPerDayData() throws IOException {
		importStartsTypeFile("startsperday");
	}
	
	public void testImportStartsHourFractionData() throws IOException {
		importStartsTypeFile("startshourfraction");
	}

	public void testImportStartsMonthAdjustData() throws IOException {
		importStartsTypeFile("startsmonthadjust");
	}

	public void testImportStartsOpModeDistributionData() throws IOException {
		importStartsTypeFile("startsopmodedistribution");
	}

	public void testImportStartsPerDayPerVehicle() throws IOException {
		importStartsTypeFile("startsperdaypervehicle");
	}

	public void importStartsTypeFile(String table) throws IOException {
		importStartsTypeFile(table, ""); 	
	}
	public void importStartsTypeFile(String table, String fileSuffix) throws IOException {
		File parentFolder = new File(".//testdata//startsimporter//");
		File srcFile = new File(parentFolder, table + fileSuffix + ".txt");
		File dstFile = new File(WorkerConfiguration.theWorkerConfiguration.workFolderPath, srcFile.getName());
		boolean copyOk = FileUtilities.copyFile(srcFile, dstFile, true);
		assertTrue("File copy failed for " + srcFile.getAbsolutePath(), copyOk);
		String loadFile = dstFile.getCanonicalPath().replace('\\', '/');
		String cmd = "LOAD DATA INFILE '" + loadFile + "' IGNORE INTO TABLE " + table + " FIELDS TERMINATED BY '\t' IGNORE 1 LINES";
		Path path = Paths.get(srcFile.getAbsolutePath());
		long lineCount = Files.lines(path).count() - 1;
		long insertedRowCount = 0;
		try {
			SQLRunner.executeSQL(db,"delete from " + table);
			SQLRunner.executeSQL(db,cmd);
			insertedRowCount = DatabaseUtilities.getRowCount(db,table);
		} catch (SQLException e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
		assertEquals("Expected inserted row count is not equal for " + table,insertedRowCount,lineCount);
		if(dstFile.exists()) {
			dstFile.delete();
		}
	}
	
	public void testInvalidStartsHourFractionRows() {
		testValidateImportSql("startshourfraction", "_bad", 14);
	}
	
	public void testInvalidStartsPerDayPerVehicleRows() {
		testValidateImportSql("startsPerDayPerVehicle", "_bad", 4);
	}
	
	public void testInvalidStartsMonthAdjustRows() {
		testValidateImportSql("startsMonthAdjust", "_bad", 3);
	}
	
	public void testInvalidStartsOpModeDistributionRows() {
		testValidateImportSql("startsOpModeDistribution", "_bad", 17);
	}
	
	public void testValidateImportSql(String tableName, String fileNameSuffix, int ExpectedErrorCount) {
		ImporterManager manager = new ImporterManager();
		manager.setMode(0);
		manager.setAsNonroad();
		try {
			SQLRunner.executeSQL(db,"delete from auditlog");
			importStartsTypeFile(tableName, fileNameSuffix);
		} catch (IOException | SQLException e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
		StartsImporter startImporter = new StartsImporter();
		RunSpecSectionStatus rsss = new RunSpecSectionStatus();
		try {
			startImporter.setImporterManager(manager);
			rsss = startImporter.getImporterDataStatusCore(db);
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}

		//check auditlog for new errors
		
		ResultSet rs;
		try {
			String sqlTest = "SELECT count(*) FROM auditlog where importerName = 'Starts' and briefDescription like concat('ERROR: ' ,'"
					+ tableName + "',' %')";
			rs = SQLRunner.executeQuery(db, sqlTest);
			rs.next();
			int rowCount = rs.getInt(1);
			rs.close();
			assertTrue("", rowCount == ExpectedErrorCount);

		} catch (SQLException e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}
}
