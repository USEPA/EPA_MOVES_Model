/**************************************************************************************************
 * @(#)AVFTControlStrategy.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.framework.importers.*;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.sql.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import java.awt.*;
import javax.swing.*;
import java.io.*;

/**
 * Implements a Control Strategy for AVFT (Alternative Vehicle Fuels & Technologies)
 *
 * @author		Wesley Faler
 * @version		2016-10-04
**/
public class AVFTControlStrategy extends InternalControlStrategy
		implements InternalControlStrategySingleInstanceOnly, InternalControlStrategyUseImporterOnly,
			IImportExportHandler, IExportDefaultHandler {
	/** GUI for this instance **/
	AVFTPanel gui = null;
	/** Data for the GUI for this instance **/
	AVFTPanelData guiData = null;
	/** EPATableModel to hold distinct Categories. **/
	public static EPATableModel tableCategoryList = new EPATableModel() ;
	/** EPATableModel to hold data from the Year table. **/
	public static EPATableModel tableYear = new EPATableModel() ;
	/** EPATableModel to hold data from the SourceUseType table. **/
	public static EPATableModel tableSourceUseType = new EPATableModel() ;
	/** EPATableModel to hold data from the FuelEngTechAssoc table. **/
	public static EPATableModel tableFuelEngTechAssoc = new EPATableModel() ;
	/** EPATableModel to hold data fuel/engine fraction. **/
	public static EPATableModel tableFuelEngFraction = new EPATableModel() ;
	/** EPATableModel to hold the Category data. **/
	public static EPATableModel tableCategory = new EPATableModel(
			"Category | Count | Expanded | AggregateColumn | AggregateColumnNumber | "
			+ " CategoryOrder | CategoryColumns | BackgroundColor | RowCategoryList |ColumnIndex");
	/** EPATableModel to hold the Category Columns data within a Category **/
	public static EPATableModel tableCategoryColumns = new EPATableModel(
			"TableCategory | ColumnName | ColumnDesc | ColumnNumber | RowFeta " ) ;

	/** Original data source file name with **/
	String dataSourceFileName = null;
	/** Type of the original data source file **/
	String dataSourceFileType = null;
	/** Worksheet, if applicable, within the original data source file **/
	String dataSourceWorksheetName = null;
	/** messages, warnings, and errors **/
	ArrayList<String> messages = new ArrayList<String>();

	/**
	 * Constructor
	**/
	public AVFTControlStrategy() {
		guiData = new AVFTPanelData(this);
		if(Configuration.allowGUI) {
			gui = new AVFTPanel(this,guiData);
			gui.setName( "AVFTPanel" );
		}
	}

	/**
	 * Gets the GUI detail panel object for the user to configure details specific to the
	 * derived subclass.
	 * @return The associated GUI JPanel for the derived subclass.
	**/
	public InternalControlStrategyPanel getDetailsPanel() {
		return gui;
	}

	/** Cancel all user edits made since last save or load from XML **/
	public void cancelEdits() {
		if(guiData != null) {
			guiData.cancel();
		}
		if(gui != null) {
			gui.revalidate();
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// Encourage garbage collection now since large data arrays are about to be created
		System.gc();

		// Update the database
		AVFTMasterLoopable processor = new AVFTMasterLoopable(this);
		processor.subscribeToMe(targetLoop);

		// Encourage garbage collection now since large data arrays have just been dropped
		processor = null;
		System.gc();
	}

	/**
	 * MasterLoopable override called during each relevant iteration of the MasterLoop.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void executeLoop(MasterLoopContext context) throws InterruptedException {
		// Nothing to do here, it was all done in subscribeToMe()
	}

	/**
	 * MasterLoopable override that cleans up all the data created within the
	 * executeLoop() method.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void cleanDataLoop(MasterLoopContext context) throws InterruptedException {
		// Nothing to do here, it was all done in subscribeToMe()
	}

	/**
	 * Creates XML representing this instance
	 * @return XML, ready to be inserted into a larger XML file under a suitable root
	**/
	public String getXML() {
		return null;
		//return gui.getXML() ;
	}

	/**
	 * Creates Tab-Separated-Value text representing this instance.
	 * @return text, ready to be inserted into a text file.  null if only XML is supported.
	**/
	public String getTSV() {
		if(guiData != null) {
			return guiData.getTSV();
		} else {
			return "";
		}
	}

	/**
	 * Read XML into this instance
	 * @param className name of the class used to instantiate this object
	 * @param root XML object under which the XML returned by getXML() has been stored
	 * @return true if the XML could be read successfully
	**/
	public boolean acceptXML(String className,Node root) {
		if(guiData != null) {
			return guiData.acceptXML( className , root ) ;
		} else {
			return false;
		}
	}

	/**
	 * Read Tab-Separated-Values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param text tab-separated-values as returned by getTSV()
	 * @return true if the text could be read successfully
	**/
	public boolean acceptTSV(String className,String text) {
		if(guiData != null) {
			return guiData.acceptTSV(className,text);
		} else {
			return false;
		}
	}

	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was imported.
	 * false if the import request should be handled by default logic instead.
	**/
	public boolean doImport(Frame ownerWindow) {
		if(gui != null) {
			return gui.doImport(ownerWindow);
		} else {
			return false;
		}
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	public boolean doExport(Frame ownerWindow) {
		if(gui != null) {
			return gui.doExport(ownerWindow,false);
		} else {
			return false;
		}
	}

	/**
	 * Process a user's request to initiate an export default operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	public boolean doExportDefault(Frame ownerWindow) {
		if(gui != null) {
			return gui.doExport(ownerWindow,true);
		} else {
			return false;
		}
	}

	/**
	 * Load a data file.  The entire reading process is encapsulated including error/warning
	 * generation.
	 * @param runspec active RunSpec
	 * @param fileName full name and path of the file to be loaded
	 * @param fileType type of the file to be loaded.  Either "XLS" or "Text".
	 * @param worksheetName for XLS files, the name of the worksheet within fileName
	 * that should loaded.  May be null in which case the first worksheet is used.
	 * @return true if the file was loaded
	**/
	boolean load(RunSpec runspec, String fileName, String fileType, String worksheetName) {
		messages.clear();
		if(guiData != null) {
			guiData.isAccurateSumsAnswer = null;
		}

		if(fileName == null || fileName.length() <= 0) {
			messages.add("Error: File does not exist: " + fileName);
			return false;
		}
		boolean result = false;
		File file = new File(fileName);
		CellFileReader reader = null;
		ImporterManager manager = null;
		SQLRunner.Query query = new SQLRunner.Query();
		Connection db = null;
		String sql = "";
		try {
			// Learn fuel/engine tech associations by sourceTypeID
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
			TreeMap<Integer,TreeSet<String> > fuelEngColumnNames = new TreeMap<Integer,TreeSet<String> >();
			sql = "select sourceTypeID, fuelTypeID, engtechID from fuelEngTechAssoc";
			query.open(db,sql);
			while(query.rs.next()) {
				Integer sourceTypeID = Integer.valueOf(query.rs.getInt(1));
				int fuelTypeID = query.rs.getInt(2);
				int engTechID = query.rs.getInt(3);
				TreeSet<String> s = fuelEngColumnNames.get(sourceTypeID);
				if(s == null) {
					s = new TreeSet<String>();
					fuelEngColumnNames.put(sourceTypeID,s);
				}
				s.add("Cat1Fuel" + fuelTypeID + "Engine" + engTechID);
			}
			query.close();

			manager = new ImporterManager(runspec);
			manager.instantiate(null); // load all filter values

			reader = new CellFileReader(new File(fileName),worksheetName);
			// Skip header row
			while(!reader.isEndOfData() && reader.shouldSkipLine()) {
				reader.endRow();
			}
			if(reader.isEndOfData()) {
				messages.add("ERROR: No header row found");
				return false;
			}
			reader.endRow(); // move past the header row

			// Read the rows, stopping when a blank row
			TreeSet<String> messageDuplicates = new TreeSet<String>();
			TreeSet<String> sourceModelYearsSeen = new TreeSet<String>();
			Integer ti;
			String ts;
			while(!reader.isEndOfData()) {
				if(reader.shouldSkipLine()) {
					reader.endRow();
					continue;
				}
				ti = reader.readIntegerCell();
				if(ti == null) {
					messages.add("Warning: Missing sourceTypeID on line " + reader.getLineNumber() + ", halting import.");
					return false;
				}
				int sourceTypeID = ti.intValue();
				if(sourceTypeID <= 0) {
					messages.add("Warning: Invalid sourceTypeID (" + sourceTypeID + ") on line " + reader.getLineNumber());
					result = false;
					reader.endRow();
					continue;
				} else if(!manager.doesInclude(ImporterManager.FILTER_SOURCE,ti)) {
					ts = "Warning: Source type " + sourceTypeID + " imported but not used in the RunSpec";
					if(!messageDuplicates.contains(ts)) {
						messageDuplicates.add(ts);
						messages.add(ts + " on line " + reader.getLineNumber());
					}
				}

				ti = reader.readIntegerCell();
				if(ti == null) {
					messages.add("Warning: Missing modelYearID on line " + reader.getLineNumber() + ", halting import.");
					return false;
				}
				int modelYearID = ti.intValue();
				if(modelYearID < 1960 || modelYearID > 2060) {
					result = false;
					ts = "Warning: Invalid modelYearID " + modelYearID;
					if(!messageDuplicates.contains(ts)) {
						messageDuplicates.add(ts);
						messages.add(ts + " on line " + reader.getLineNumber());
					}
				}

				String sourceModelYearKey = "" + sourceTypeID + "|" + modelYearID;
				if(!sourceModelYearsSeen.contains(sourceModelYearKey)) {
					sourceModelYearsSeen.add(sourceModelYearKey);
					// Zero all fraction information for this sourcetype/modelyear combination.
					// This prevents default data from conflicting with imported information.
					TreeSet<String> s = fuelEngColumnNames.get(Integer.valueOf(sourceTypeID));
					if(s != null) {
						if(guiData != null) {
							for(Iterator<String> i=s.iterator();i.hasNext();) {
								guiData.setMemory(""+sourceTypeID,""+modelYearID,i.next(),"0");
							}
						}
					}
				}

				ti = reader.readIntegerCell();
				if(ti == null) {
					messages.add("Warning: Missing fuelTypeID on line " + reader.getLineNumber() + ", halting import.");
					return false;
				}
				int fuelTypeID = ti.intValue();
				if(fuelTypeID <= 0) {
					messages.add("Warning: Invalid fuelTypeID (" + fuelTypeID + ") on line " + reader.getLineNumber());
					result = false;
					reader.endRow();
					continue;
				/*
				// Do not complain about fuel types not used by the RunSpec
				} else if(!manager.doesInclude(ImporterManager.FILTER_FUEL,ti)) {
					ts = "Warning: Fuel type " + fuelTypeID + " imported but not used in the RunSpec";
					if(!messageDuplicates.contains(ts)) {
						messageDuplicates.add(ts);
						messages.add(ts + " on line " + reader.getLineNumber());
					}
				*/
				}

				ti = reader.readIntegerCell();
				if(ti == null) {
					messages.add("Warning: Missing engTechID on line " + reader.getLineNumber() + ", halting import.");
					return false;
				}
				int engTechID = ti.intValue();
				if(engTechID <= 0) {
					messages.add("Warning: Invalid engTechID (" + engTechID + ") on line " + reader.getLineNumber());
					result = false;
					reader.endRow();
					continue;
				}

				// Check sourceTypeID/fuelTypeID/engTechID combination
				TreeSet<String> fe = fuelEngColumnNames.get(Integer.valueOf(sourceTypeID));
				if(fe != null) {
					String feKey = "Cat1Fuel" + fuelTypeID + "Engine" + engTechID;
					if(!fe.contains(feKey)) {
						messages.add("Warning: Invalid sourceTypeID/fuelTypeID/engTechID (" + sourceTypeID + "/" + fuelTypeID + "/" + engTechID + ") combination on line " + reader.getLineNumber());
						result = false;
						reader.endRow();
						continue;
					}
				}


				Double td = reader.readDoubleCell();
				if(ti == null) {
					messages.add("Warning: Missing fuelEngFraction on line " + reader.getLineNumber() + ", halting import.");
					return false;
				}
				double fuelEngFraction = td.doubleValue();
				if(fuelEngFraction < 0) {
					messages.add("Warning: Invalid fuelEngFraction (" + fuelEngFraction + ") on line " + reader.getLineNumber());
					result = false;
					reader.endRow();
					continue;
				}

				if(guiData != null) {
					guiData.setMemory(""+sourceTypeID,""+modelYearID,"Cat1Fuel" + fuelTypeID + "Engine" + engTechID,""+fuelEngFraction);
				}

				reader.endRow();
			}

			if(guiData != null) {
				guiData.setSums() ;
				guiData.resetFromDefaultTables = false ;
				guiData.isAccurateSums();
			}

			return result;
		} catch(Exception e) {
			messages.add("Error: Unable to load file: " + e.getMessage());
			e.printStackTrace();
			return false;
		} finally {
			if(reader != null) {
				try {
					reader.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				reader = null;
			}
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
			}
		}
	}

	/**
	 * Save the current information, whether valid or not, to a file.  The type of
	 * the file is inferred from the file extension.  ".XLS" extensions create
	 * an XLS file.  ".CSV" creates a comma-separated file.  All other extensions
	 * create a tab-separated file.
	 * @param runspec active RunSpec
	 * @param file the file to be created
	 * @param saveDefaultData true if the default data should be saved, false if
	 * user-supplied data should be saved
	 * @return true if the data was saved successfully
	**/
	public boolean save(RunSpec runspec, File file, boolean saveDefaultData) {
		CellFileWriter writer = null;
		ImporterManager manager = null;
		SQLRunner.Query query = new SQLRunner.Query();
		Connection db = null;
		try {
			manager = new ImporterManager(runspec);
			manager.instantiate(null); // load all filter values

			writer = new CellFileWriter(file,"AVFT");

			// Write header row
			writer.writeTextCell("sourceTypeID");
			writer.writeTextCell("modelYearID");
			writer.writeTextCell("fuelTypeID");
			writer.writeTextCell("engTechID");
			writer.writeTextCell("fuelEngFraction");
			writer.endRow();

			// Write core data
			if(saveDefaultData) {
				String sql = "";
				sql = "select sourceTypeModelYearID, fuelTypeID, engTechID,"
						+ " sum(stmyFraction) as fuelEngFraction, svp.sourceTypeID,"
						+ " svp.modelYearID as YearId"
						+ " from sampleVehiclePopulation svp"
						+ " group by svp.sourceTypeModelYearID, fuelTypeID, engTechID"
						+ " order by svp.sourceTypeModelYearID, fuelTypeID, engTechID";
				db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
				query.open(db,sql);
				Integer sourceTypeIDInt = null;
				Integer modelYearIDInt = null;
				Integer fuelTypeIDInt = null;
				while(query.rs.next()) {
					int sourceTypeID = query.rs.getInt("sourceTypeID");
					int modelYearID = query.rs.getInt("YearId");
					int fuelTypeID = query.rs.getInt("fuelTypeID");
					int engTechID = query.rs.getInt("engTechID");
					double fuelEngFraction = query.rs.getFloat("fuelEngFraction");
					if(fuelEngFraction <= 0) { // Don't export 0 fractions, it makes the output file too long
						continue;
					}

					if(sourceTypeIDInt == null || sourceTypeIDInt.intValue() != sourceTypeID) {
						sourceTypeIDInt = Integer.valueOf(sourceTypeID);
					}
					// Filter to use only source types in the current RunSpec
					if(!manager.doesInclude(ImporterManager.FILTER_SOURCE,sourceTypeIDInt)) {
						continue;
					}

					if(modelYearIDInt == null || modelYearIDInt.intValue() != modelYearID) {
						modelYearIDInt = Integer.valueOf(modelYearID);
					}
					// Filter to use only model years needed by the current RunSpec
					if(!manager.doesInclude(ImporterManager.FILTER_MODELYEARID,modelYearIDInt)) {
						continue;
					}

					// Do not filter by fuel type during export.
					/*
					if(fuelTypeIDInt == null || fuelTypeIDInt.intValue() != fuelTypeID) {
						fuelTypeIDInt = Integer.valueOf(fuelTypeID);
					}
					// Filter to use only fuel types in the current RunSpec
					if(!manager.doesInclude(ImporterManager.FILTER_FUEL,fuelTypeIDInt)) {
						continue;
					}
					*/
					writer.writeIntCell(sourceTypeID);
					writer.writeIntCell(modelYearID);
					writer.writeIntCell(fuelTypeID);
					writer.writeIntCell(engTechID);
					writer.writeDoubleCell(fuelEngFraction);
					writer.endRow();
				}
				query.close();
			} else {
				for(int rowSUT = 0;rowSUT < tableSourceUseType.getNumRows();rowSUT++) {
					int sourceTypeID = tableSourceUseType.getInt(rowSUT,"SourceTypeId"); // was .getString

					// Filter to use only source types in the current RunSpec
					if(!manager.doesInclude(ImporterManager.FILTER_SOURCE,Integer.valueOf(sourceTypeID))) {
						continue;
					}

					EPATableModel tbCat = (EPATableModel)tableSourceUseType.getObject(rowSUT,"TableCategory");
					if(tbCat == null) {
						continue;
					}

					EPATableModel tbData = (EPATableModel)tableSourceUseType.getObject(rowSUT,"TableFractionData");
					if(tbData == null) {
						continue;
					}

					for(int rowDATA = 0;rowDATA < tbData.getNumRows();rowDATA++) {
						int modelYearID = tbData.getInt(rowDATA,"YearId"); // was .getString

						// Filter to use only model years needed by the current RunSpec
						if(!manager.doesInclude(ImporterManager.FILTER_MODELYEARID,Integer.valueOf(modelYearID))) {
							continue;
						}

						for(int rowCAT = 0;rowCAT < tbCat.getNumRows();rowCAT++) {
							EPATableModel tbCatCol = (EPATableModel)tbCat.getObject(rowCAT,"CategoryColumns");
							if(tbCatCol == null) {
								continue;
							}

							for(int rowCATCOL = 0;rowCATCOL < tbCatCol.getNumRows();rowCATCOL++) {
								int column = tbCatCol.getInt(rowCATCOL,"ColumnNumber");
								double fuelEngFraction = tbData.getDouble(rowDATA,column);
								if(fuelEngFraction <= 0) { // Don't export 0 fractions, it makes the output file too long
									continue;
								}

								String columnName = tbCatCol.getString(rowCATCOL,"ColumnName");
								// Column name example: Cat5Fuel9Engine30
								int fuelIndex = columnName.indexOf("Fuel");
								int engineIndex = columnName.indexOf("Engine");
								int fuelTypeID = Integer.parseInt(columnName.substring(fuelIndex+4,engineIndex));
								int engTechID = Integer.parseInt(columnName.substring(engineIndex+6));

								// Do not filter by fuel type during export.
								/*
								// Filter to use only fuel types in the current RunSpec
								if(!manager.doesInclude(ImporterManager.FILTER_FUEL,Integer.valueOf(fuelTypeID))) {
									continue;
								}
								*/

								writer.writeIntCell(sourceTypeID);
								writer.writeIntCell(modelYearID);
								writer.writeIntCell(fuelTypeID);
								writer.writeIntCell(engTechID);
								writer.writeDoubleCell(fuelEngFraction); // was .getString
								writer.endRow();
							}
						}
					}
				}
			}

			// Write supporting lookup tabs if the output file supports worksheets
			if(CellFile.formatAllowsWorksheets(writer.getFileType())) {
				String[] decodeTableNames = { "SourceUseType", "FuelType", "EngineTech" };
				CommonNamesFilter filter = new CommonNamesFilter(manager,true);
				Connection defaultDb = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
				for(int di=0;di<decodeTableNames.length;di++) {
					String decodeTableName = decodeTableNames[di];
					for(int i=0;i<BasicDataHandler.commonDecodeTablesAndQueries.length;i+=2) {
						if(BasicDataHandler.commonDecodeTablesAndQueries[i].equalsIgnoreCase(decodeTableName)) {
							String sql = BasicDataHandler.commonDecodeTablesAndQueries[i+1];
							if(sql != null && sql.length() > 0) {
								writer.startTab(decodeTableName);
								writer.writeSQLResults(defaultDb,sql,filter);
							}
							break;
						}
					}
				}
			}

			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to save AVFT file");
			return false;
		} finally {
			if(writer != null) {
				writer.close();
				writer = null;
			}
			query.onFinally();
		}
	}
}
