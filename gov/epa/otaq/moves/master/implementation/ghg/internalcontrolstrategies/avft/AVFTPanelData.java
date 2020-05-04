/**************************************************************************************************
 * @(#)AVFTPanelData.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft;

import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.lang.*;
import java.sql.*;
import java.math.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import java.io.*;

/**
 * Implements a Panel for AVFT (Alternative Vehicle Fuels & Technologies) Control Strategy
 *
 * @author		Wesley Faler
 * @author		EPA - John K.
 * @author		EPA - Mitch C.
 * @author		Tim Hull
 * @version		2014-01-23
**/
class AVFTPanelData {
	/** Singleton for current AVFTPanel **/
	public static AVFTPanelData avftPanelCurrent = null ;

	/** A reference to the AVFTControlStrategy that is being used for this execution **/
	AVFTControlStrategy strategy;

	/** Most recent RunSpec **/
	RunSpec recentRunSpec = null;

	/** EPATableModel to hold distinct Categories. **/
	public static EPATableModel tableCategoryList = new EPATableModel() ;
	/** EPATableModel to hold data from the Year table. **/
	public static EPATableModel tableYear = new EPATableModel() ;
	/** EPATableModel to hold data from the SourceUseType table. **/
	public static EPATableModel tableSourceUseType = new EPATableModel() ;
	/** EPATableModel to hold data from the FuelEngTechAssoc table. **/
	public static EPATableModel tableFuelEngTechAssoc = new EPATableModel() ;
	/** EPATableModel to hold data from the FuelEngFraction table. **/
	public static EPATableModel tableFuelEngFraction = new EPATableModel() ;
	/** EPATableModel to hold the Fraction data displayed on the screen. **/
//	public static EPATableModel tableAvftActive ;
	/** EPATableModel to hold the Category data displayed on the screen. **/
//	public static EPATableModel tableCategoryActive ;
	/** EPATableModel to hold the Category data displayed on the screen. **/
	public static EPATableModel tableCategory = new EPATableModel(
			"Category | Count | Expanded | AggregateColumn | AggregateColumnNumber | "
			+ " CategoryOrder | CategoryColumns | BackgroundColor | RowCategoryList |ColumnIndex");
	/** EPATableModel to hold the Category Columns data displayed on the screen. **/
	public static EPATableModel tableCategoryColumns = new EPATableModel(
			"TableCategory | ColumnName | ColumnDesc | ColumnNumber | RowFeta " ) ;

	/** Stores the index value of the SourceTypeId column within tableFuelEngTechAssoc **/
	int indexFetaSourceTypeId = -1 ;
	/** Stores the index value of the CategoryDisplayOrder column within tableFuelEngTechAssoc **/
	int indexFetaCategoryDisplayOrder = -1 ;
	/** Stores the index value of the Category column within tableFuelEngTechAssoc **/
	int indexFetaCategory = -1 ;
	/** Stores the index value of the FuelTypeId column within tableFuelEngTechAssoc **/
	int indexFetaFuelTypeId = -1 ;
	/** Stores the index value of the EngTechId column within tableFuelEngTechAssoc **/
	int indexFetaEngTechId = -1 ;

	/** The stored value of the XML that will reset the screen values **/
	public Node resetXML = null ;
	/** The stored value of the TSV text that will reset the screen values **/
	public String resetTSV = null;

	/** Indicates whether the reset values are from the default tables **/
	public boolean resetFromDefaultTables = false ;

	/** Indicates whether the reset values are from the default tables **/
	public String dataDefaultDatabase = "" ;

	/** Cached answer from isAccurateSums() routine **/
	Boolean isAccurateSumsAnswer = null;

	/** Details explaining why isAccurateSums() returned false **/
	String inaccurateSumsExplanation = null;

	/**
	 * True when the GUI's values should be loaded from FuelEngFraction.
	 * When false, the SampleVehiclePopulation table is aggregated instead.
	**/
	boolean useFuelEngFractionTable = true;

	/**
	 * Constructor
	 * @param strategyToUse the AVFTControlStrategy that owns this panel
	**/
	public AVFTPanelData(AVFTControlStrategy strategyToUse) {
		strategy = strategyToUse;

		if ( strategy == null ) {
			return ;
		}

		tableCategoryList = strategy.tableCategoryList ;
		tableYear = strategy.tableYear ;
		tableSourceUseType = strategy.tableSourceUseType ;
		tableFuelEngTechAssoc = strategy.tableFuelEngTechAssoc ;
		tableFuelEngFraction = strategy.tableFuelEngFraction ;
		tableCategory = strategy.tableCategory ;
		tableCategoryColumns = strategy.tableCategoryColumns ;

		//System.out.println( "\n********\nAVFTPanel Constructor\n\n" ) ;

		clearAllData() ;

		avftPanelCurrent = this ;
		EPATableModel.setDbConnectionIfNull(
				DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT ) ) ;

		loadTables();
		loadTablesFromFuelEngTechAssoc() ;
	}

	/**
	 * Documents the memory structure. Primarily used for program debugging.
	 * and width of the JTable to fit in the screen.
	**/
	public void documentMemory() {
		StringBuffer sbData = new StringBuffer( 20000 ) ;
		String fileName = "avft.txt" ;
		int rowSUT , rowCAT , rowCATCOL , rowColor = 0 ;
		boolean b ;
		EPATableModel tbCat = null , tbCatCol = null , tbData ;

		sbData.append( "Avft Memory Structure\n\n" ) ;
		sbData.append( "Table Definition: tableCategoryList\n\n"
				+ tableCategoryList.getDefinition() ) ;
		sbData.append( "\n\n" ) ;
		sbData.append( "Table Definition: tableCategory\n\n" + tableCategory.getDefinition() ) ;
		sbData.append( "\n\n" ) ;
		sbData.append( "Table Definition: tableCategoryColumns\n\n" +
				tableCategoryColumns.getDefinition() ) ;
		sbData.append( "\n\n" ) ;

		for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
			sbData.append( "\n\n******************************************\n\n" ) ;
			sbData.append("Source Type: "+tableSourceUseType.getString(rowSUT,"SourceTypeId"));
			sbData.append( " - " + tableSourceUseType.getString(rowSUT,"SourceTypeName") + "\n");

			tbCat = ( EPATableModel ) tableSourceUseType.getObject( rowSUT , "TableCategory" ) ;
			if ( tbCat == null ) {
				sbData.append( "\tTableCategory is null - no columns attached.\n" ) ;
				continue ;
			}

			tbData = ( EPATableModel )
					tableSourceUseType.getObject( rowSUT , "TableFractionData" ) ;
			if ( tbData != null ) {
				sbData.append( tbData.getDefinition() ) ;
				sbData.append( "\n\n" ) ;
			}

			for ( rowCAT = 0 ; rowCAT < tbCat.getNumRows() ; rowCAT++ ) {
				sbData.append( "\t  Category: " + tbCat.getString(rowCAT,"Category") ) ;
				sbData.append( "  (" + tbCat.getString(rowCAT,"Count") + " columns)\n" ) ;

				sbData.append( "\t\t" + "Column: ") ;
				sbData.append( tbCat.getString( rowCAT , "AggregateColumnNumber" ) ) ;
				sbData.append( "  " + tbCat.getString( rowCAT , "AggregateColumn" ) ) ;
				sbData.append( " (Aggregate)\n" ) ;

				tbCatCol = ( EPATableModel ) tbCat.getObject( rowCAT , "CategoryColumns" ) ;
				if ( tbCatCol == null ) {
					sbData.append( "\t\t(CategoryColumns is null.\n" ) ;
					continue ;
				}

				for ( rowCATCOL = 0 ; rowCATCOL < tbCatCol.getNumRows() ; rowCATCOL++ ) {
					sbData.append( "\t\t" + "Column: ") ;
					sbData.append( tbCatCol.getString( rowCATCOL , "ColumnNumber" ) ) ;
					sbData.append( "  " + tbCatCol.getString( rowCATCOL , "ColumnName" ) ) ;
					sbData.append( "\n" ) ;
				}
				sbData.append( "\n" ) ;
			}

		}

		b = FileUtilities.writeFileContents( fileName , sbData.toString() ) ;
	}

	/**
	 * Returns a tab-separated-value definition for the data structure in memory
	 * @return Returns a string with text representing the object
	**/
	public String getTSV() {
		StringBuffer sbData = new StringBuffer( 2000000 ) ;
		String tab1 = "" , tab2 = "\t" , sourceType , dq = "\"" , yearId ;
		int rowSUT , rowCAT , rowCATCOL , rowColor = 0 , rowDATA , column ;
		boolean b ;
		EPATableModel tbCat = null , tbCatCol = null , tbData ;
		final String eol = System.getProperty("line.separator");

		TreeMapIgnoreCase categoryNameByID = new TreeMapIgnoreCase();

		for ( int row = 0 ; row < tableCategoryList.getNumRows() ; row++ ) {
			String category = tableCategoryList.getString( row , "Category" ) ;
			String id = tableCategoryList.getString( row , "categoryDisplayOrder" ) ;
			categoryNameByID.put(id,category);
		}

		sbData.append("dataSourceFileName\t" + StringUtilities.safeGetString(strategy.dataSourceFileName) + eol);
		sbData.append("dataSourceFileType\t" + StringUtilities.safeGetString(strategy.dataSourceFileType) + eol);
		sbData.append("dataSourceWorksheetName\t" + StringUtilities.safeGetString(strategy.dataSourceWorksheetName) + eol);

		sbData.append("description\t" + strategy.getDescription() + eol);

		sbData.append( "sourceTypeID"
				+ "\t" + "year"
				+ "\t" + "Category"
				+ "\t" + "fuelTypeID"
				+ "\t" + "engTechID"
				+ "\t" + "Amount"
				+ eol);

		for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
			sourceType = tableSourceUseType.getString( rowSUT , "SourceTypeId" ) ;

			tbCat = ( EPATableModel ) tableSourceUseType.getObject( rowSUT , "TableCategory" ) ;
			if ( tbCat == null ) {
				continue ;
			}

			tbData = ( EPATableModel )
					tableSourceUseType.getObject( rowSUT , "TableFractionData" ) ;
			if ( tbData == null ) {
				continue ;
			}

			for ( rowDATA = 0 ; rowDATA < tbData.getNumRows() ; rowDATA++ ) {
				yearId = tbData.getString( rowDATA , "YearId" ) ;

				for ( rowCAT = 0 ; rowCAT < tbCat.getNumRows() ; rowCAT++ ) {
					tbCatCol = ( EPATableModel ) tbCat.getObject( rowCAT , "CategoryColumns" ) ;
					if ( tbCatCol == null ) {
						continue ;
					}

					for ( rowCATCOL = 0 ; rowCATCOL < tbCatCol.getNumRows() ; rowCATCOL++ ) {
						column = tbCatCol.getInt( rowCATCOL , "ColumnNumber" ) ;
						String columnName = tbCatCol.getString( rowCATCOL , "ColumnName");
						// Column name example: Cat5Fuel9Engine30
						int catIndex = columnName.indexOf("Cat");
						int fuelIndex = columnName.indexOf("Fuel");
						int engineIndex = columnName.indexOf("Engine");

						String categoryID = columnName.substring(catIndex+3,fuelIndex);
						String categoryName = (String)categoryNameByID.get(categoryID);
						String fuelTypeID = columnName.substring(fuelIndex+4,engineIndex);
						String engTechID = columnName.substring(engineIndex+6);

						sbData.append( sourceType
								+ "\t" + yearId
								+ "\t" + categoryName
								+ "\t" + fuelTypeID
								+ "\t" + engTechID
								+ "\t" + tbData.getString( rowDATA , column )
								+ eol);
					}
				}
			}
		}

		return sbData.toString() ;
	}

	/**
	 * Read XML values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param root XML object under which the XML returned by getXML() has been stored
	 * @return true if the XML could be read successfully
	**/
	public boolean acceptXML(String className,Node root) {
		if(root == null) {
			return false;
		}

		isAccurateSumsAnswer = null;

		Node avft , record , attributeNode ;
		NamedNodeMap attributes ;
		String year , columnName , amount , s , name , value ;
		String srcTypeId , sourceType ;
		EPATableModel tbData ;
		int column , i , rowSUT , rowDATA ;

		try {
			boolean foundXML = false;

			for( avft = root.getFirstChild() ; avft != null ; avft = avft.getNextSibling() ) {
				if ( avft.getNodeName().equalsIgnoreCase( "avft" ) == false ) {
					continue;
				}
				if(!foundXML) {
					foundXML = true;
					clearAllData() ;
					resetXML = root ;
					resetTSV = null;
				}

				attributes = avft.getAttributes() ;
				for( i = 0; i < attributes.getLength(); i++) {
					attributeNode = attributes.item( i ) ;
					name = attributeNode.getNodeName() ;
					value = attributeNode.getNodeValue() ;
					if ( name.equalsIgnoreCase( "description" ) ) {
						strategy.setDescription( value ) ;
					}
				}

				for( record = avft.getFirstChild() ; record != null ;
						record = record.getNextSibling() ) {
					if ( record.getNodeName().equalsIgnoreCase( "avftdata" ) ) {
						attributes = record.getAttributes() ;

						year = "" ;
						srcTypeId = "" ;
						columnName = "" ;
						amount = "" ;

						for( i = 0; i < attributes.getLength(); i++) {
							attributeNode = attributes.item( i ) ;
							name = attributeNode.getNodeName() ;
							value = attributeNode.getNodeValue() ;

							if ( name.equalsIgnoreCase( "sourcetypeid" ) ) {
								srcTypeId = value ;
								continue ;
							}
							if ( name.equalsIgnoreCase( "yearid" ) ) {
								year = value ;
								continue ;
							}
							if ( name.equalsIgnoreCase( "column" ) ) {
								columnName = value ;
								continue ;
							}
							if ( name.equalsIgnoreCase( "amount" ) ) {
								amount = value ;
								continue ;
							}

							//System.out.println( "Set: " + srcTypeId + " , " + year + ":  "
							//	+ columnName + " = " + amount ) ;
						}

						// Now write the information to the AVFTPanel memory
						setMemory( srcTypeId , year , columnName , amount ) ;
					}
					break ;
				}
			}

			if(!foundXML) {
				return false;
			}

			setSums() ;
			resetFromDefaultTables = false ;

			return true;
		} catch( Exception ex ) {
			Logger.log(LogMessageCategory.WARNING, "\nAVFTPanel.acceptXML Exception: "
					+ ex.toString() + "\n" ) ;
		}

		return false ;
	}

	/**
	 * Read Tab-Separated-Values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param text tab-separated-values as returned by getTSV()
	 * @return true if the text could be read successfully
	**/
	public boolean acceptTSV(String className,String text) {
		isAccurateSumsAnswer = null;

		String srcTypeId, year, category, categoryID, engTechID, fuelTypeID, columnName, amount;
		TreeMapIgnoreCase categoryIDbyName = new TreeMapIgnoreCase();

		for ( int row = 0 ; row < tableCategoryList.getNumRows() ; row++ ) {
			category = tableCategoryList.getString( row , "Category" ) ;
			String id = tableCategoryList.getString( row , "categoryDisplayOrder" ) ;
			categoryIDbyName.put(category,id);
		}

		try {
			boolean foundText = false;
			boolean foundField = false;

			String[] csvLines = text.trim().split("\\n");
			if(csvLines == null || csvLines.length <= 0) {
				return false;
			}

			for(int line=0;line<csvLines.length;line++) {
				String[] fields = csvLines[line].trim().split("\\t");
				if(fields == null || fields.length < 2) {
					continue;
				}
				if(fields[0].equalsIgnoreCase("description")) {
					foundField = true;
					strategy.setDescription(fields[1].trim());
				} else if(fields[0].equalsIgnoreCase("dataSourceFileName")) {
					foundField = true;
					strategy.dataSourceFileName = fields[1].trim();
				} else if(fields[0].equalsIgnoreCase("dataSourceFileType")) {
					foundField = true;
					strategy.dataSourceFileType = fields[1].trim();
				} else if(fields[0].equalsIgnoreCase("dataSourceWorksheetName")) {
					foundField = true;
					strategy.dataSourceWorksheetName = fields[1].trim();
				}
				if(foundField) {
					foundField = false;
					if(!foundText) {
						foundText = true;
						//clearAllData() ;
						resetXML = null ;
						resetTSV = text;
					}
					continue;
				}

				if(fields.length < 6) {
					continue;
				}
				if(fields[0].equalsIgnoreCase("sourceTypeID")) {
					//System.out.println("Found title line");
					continue;
				}

				srcTypeId = fields[0].trim();
				year = fields[1].trim();
				category = fields[2].trim();
				fuelTypeID = fields[3].trim();
				engTechID = fields[4].trim();
				amount = fields[5].trim();

				categoryID = (String)categoryIDbyName.get(category);
				if(categoryID != null) {
					columnName = "Cat" + "1" // categoryID
							+ "Fuel" + fuelTypeID
							+ "Engine" + engTechID;

					if(srcTypeId.length() > 0 && year.length() > 0
							&& columnName.length() > 0 && amount.length() > 0) {
						if(!foundText) {
							foundText = true;
							//clearAllData() ;
							resetXML = null ;
							resetTSV = text;
						}
						setMemory( srcTypeId , year , columnName , amount ) ;
					}
				}
			}

			if(!foundText) {
				return false;
			}

			setSums() ;
			resetFromDefaultTables = false ;

			return true;
		} catch( Exception ex ) {
			Logger.log(LogMessageCategory.WARNING, "\nAVFTPanel.acceptTSV Exception: "
					+ ex.toString() + "\n" ) ;
		}

		return false ;
	}

	/**
	 * Finds the appropriate cell in the memory structures and updates the amount
	 * @param sourceTypeId The Source Type ID of the cell to be updated
	 * @param year The Year of the cell to be updated
	 * @param columnName The Column Name of the cell to be updated
	 * @param amount The Amount value of the cell that will be updated
	**/
	public void setMemory( String sourceTypeId , String year , String columnName ,
			String amount ) {
		if(!columnName.startsWith("Cat1F")) {
			// Ensure all column names conform to the new 1-category standard since
			// we no longer display categories.  Such column names start with "Cat1Fuel"
			columnName = "Cat1" + columnName.substring(columnName.indexOf('F'));
		}

		int rowSUT , rowDATA ;
		String sourceType , yearId ;
		EPATableModel tbCat = null , tbCatCol = null , tbData ;

		for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
			sourceType = tableSourceUseType.getString( rowSUT , "SourceTypeId" ) ;
			if ( sourceType.equals( sourceTypeId ) == false ) {
				continue ;
			}

			tbData = ( EPATableModel )
					tableSourceUseType.getObject( rowSUT , "TableFractionData" ) ;
			if ( tbData == null ) {
				continue ;
			}

			for ( rowDATA = 0 ; rowDATA < tbData.getNumRows() ; rowDATA++ ) {
				yearId = tbData.getString( rowDATA , "YearId" ) ;
				if ( yearId.equals( year ) == false ) {
					continue ;
				}

				tbData.setValue( amount , rowDATA , columnName ) ;
				return ;
			}
		}
		//System.out.println("AVFTPanel.setMemory(" + sourceTypeId + "," + year + "," + columnName + "," + amount + ") Failed");
	}

	/**
	 * Find a row in tableFuelEngTechAssoc
	 * @param sourceId source type to search for
	 * @param fuelTypeId fuel type to search for
	 * @param engTechId engine technology to search for
	 * @return 0-based index into tableFuelEngTechAssoc or -1 if now match was found
	**/
	int findFetaRow( int sourceId , int fuelTypeId , int engTechId ) {
		for ( int row = 0 ; row < tableFuelEngTechAssoc.getNumRowsActive() ; row++ ) {
			if ( sourceId == tableFuelEngTechAssoc.getInt( row , indexFetaSourceTypeId )
					&& fuelTypeId == tableFuelEngTechAssoc.getInt( row , indexFetaFuelTypeId )
					&& engTechId == tableFuelEngTechAssoc.getInt( row , indexFetaEngTechId ) ) {
				return row ;
			}
		}
		return -1 ;
	}

	/**
	 * Loads Data Tables with data from database
	 * This routine is intended to be called only once
	**/
	void loadTables() {
		int rc = 0 , row , id = 0 , idType , idYear , rowData , rowUse , sourceTypeId ,
				rowColor = 0 ;
		Integer idKey ;
		String sId , category , lastCategory ;
		int indexYearId = -1 , indexSourceTypeId = -1 , type , rCategory = 0 , countCategory = 0 ;
		int colIndex , indexCategory , idFuel , idEngine , categoryOrder , rCatColumns = 0 ;
		int indexCat , indexRow , rowCatList , indexCatListColor , srcTypeId ;
		String sql , columnName , columnNameAggregate , columnDesc ;
		EPATableModel tableCat , tableCatColumns = null , tableData ;

		EPATableModel.dbConnection =
				DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		Connection conn = EPATableModel.dbConnection ;
		String databaseName = "" ;

		if ( conn == null ) {
			return ;
		}

		try {
			databaseName = conn.getCatalog() ;
			if ( databaseName.equalsIgnoreCase( dataDefaultDatabase ) ) {
				return ;
			}
		} catch( Exception ex ) {
			databaseName = "" ;
			return ;
		}

		dataDefaultDatabase = databaseName ;

		//sql = "SELECT distinct Category, categoryDisplayOrder FROM FuelEngTechAssoc" ;
		sql = "SELECT 'ALL' as Category, 1 as categoryDisplayOrder" ;
		rc = tableCategoryList.sqlSelect( sql ) ;
		tableCategoryList.addColumns( " Abbr | BackgroundColor " ) ;

		for ( row = 0 ; row < tableCategoryList.getNumRows() ; row++ ) {
			category = tableCategoryList.getString( row , "Category" ) ;
			tableCategoryList.setValue( category , row , "Abbr" ) ;
			rowColor++ ;
		}

		//tableCategoryList.printDataRows( "catlist.txt" ) ;

		//sql = "SELECT * FROM year WHERE yearid >= 2001 ORDER BY yearid " ;
		sql = "SELECT modelYearID as yearID FROM modelYear ORDER BY modelYearID " ;
		rc = tableYear.sqlSelect( sql ) ;

		sql = "SELECT * FROM sourceusetype ORDER BY SourceTypeID" ;
		rc = tableSourceUseType.sqlSelect( sql ) ;

		tableSourceUseType.addColumns( "TableCategory | TableFractionData" ) ;
		sql = "SELECT a.sourceTypeID, a.fuelTypeID, a.engTechID, 'ALL' as category, 1 as categoryDisplayOrder, b.engtechname , c.fueltypedesc " +
				" FROM FuelEngTechAssoc a , enginetech b , fueltype c " +
				" WHERE a.fuelTypeID = c.fuelTypeID AND a.engTechID = b.engTechID " +
				"ORDER BY sourceTypeID , categoryDisplayOrder , a.fuelTypeID , a.engTechID " ;
		rc = tableFuelEngTechAssoc.sqlSelect( sql ) ;

		indexFetaSourceTypeId = tableFuelEngTechAssoc.getColumnIndex( "SourceTypeId" ) ;
		indexFetaCategoryDisplayOrder =
				tableFuelEngTechAssoc.getColumnIndex( "CategoryDisplayOrder" ) ;
		indexFetaCategory = tableFuelEngTechAssoc.getColumnIndex( "Category" ) ;
		indexFetaFuelTypeId = tableFuelEngTechAssoc.getColumnIndex( "FuelTypeId" ) ;
		indexFetaEngTechId = tableFuelEngTechAssoc.getColumnIndex( "EngTechId" ) ;
		tableFuelEngTechAssoc.addColumns( "RowCategoryList | ColAggregate | ColFraction" ) ;

		indexRow = tableFuelEngTechAssoc.getColumnIndex( "RowCategoryList" ) ;
		indexCat = tableCategoryList.getColumnIndex( "Category" ) ;
		indexCatListColor = tableCategoryList.getColumnIndex( "BackgroundColor" ) ;

		for ( row = 0 ; row < tableFuelEngTechAssoc.getNumRows() ; row++ ) {
			category = tableFuelEngTechAssoc.getString( row , "Category" ) ;
			rowCatList = tableCategoryList.find( category , indexCat ) ;
			tableFuelEngTechAssoc.setValue( rowCatList , row , indexRow ) ;
		}

		//tableFuelEngTechAssoc.printDataRows( "feta.txt" ) ;

		sql = "select sourceTypeModelYearID, fuelTypeID, engTechID,"
				+ " sum(stmyFraction) as fuelEngFraction, svp.sourceTypeID,"
				+ " svp.modelYearID as YearId"
				+ " from sampleVehiclePopulation svp"
				+ " group by svp.sourceTypeModelYearID, fuelTypeID, engTechID"
				+ " order by svp.sourceTypeModelYearID, fuelTypeID, engTechID";
		rc = tableFuelEngFraction.sqlSelect( sql ) ;

		//tableFuelEngFraction.printDataRows( "fef.txt" ) ;

		// Build a TableCategory for each SourceType
		for ( rowUse = 0 ; rowUse < tableSourceUseType.getNumRowsActive() ; rowUse++ ) {
			srcTypeId = tableSourceUseType.getInt( rowUse , 0 ) ;
			if ( srcTypeId == 0 ) {
				continue ;
			}

			indexSourceTypeId = tableFuelEngTechAssoc.getColumnIndex( "SourceTypeId" ) ;
			indexCategory = tableFuelEngTechAssoc.getColumnIndex( "Category" ) ;

			tableCat = tableCategory.copyStructure() ;
			rCatColumns = 0 ;
			rowColor = 0 ;

			tableSourceUseType.setValue( tableCat , rowUse , "TableCategory" ) ;
			tableData = new EPATableModel() ;
			tableSourceUseType.setValue( tableData , rowUse , "TableFractionData" ) ;

			// Load the YearId column in the Data table
			tableData.addColumns( "YearId" ) ;
			tableData.setNumRowsActive( tableYear.getNumRowsActive() ) ;
			for ( row = 0 ; row < tableYear.getNumRowsActive() ; row++ ) {
				tableData.setValue( tableYear.getInt( row , 0 ) , row , 0 ) ;
			}

			lastCategory = "" ;
			rCategory = -1 ;
			countCategory = 0 ;
			colIndex = 1 ;
			columnNameAggregate = "" ;

			// For the Active Source ID, determine the applicable columns
			for ( row = 0 ; row < tableFuelEngTechAssoc.getNumRowsActive() ; row++ ) {
				type = tableFuelEngTechAssoc.getInt( row , indexSourceTypeId ) ;
				if ( type != srcTypeId ) {
					continue ;
				}

				rowCatList = tableFuelEngTechAssoc.getInt( row , indexRow ) ;

				idFuel = tableFuelEngTechAssoc.getInt( row , "FuelTypeId" ) ;
				idEngine = tableFuelEngTechAssoc.getInt( row , "EngTechId" ) ;
				category = tableFuelEngTechAssoc.getString( row , indexCategory ) ;
				categoryOrder = tableFuelEngTechAssoc.getInt( row , "CategoryDisplayOrder" ) ;

				if ( category.equalsIgnoreCase( lastCategory ) == false ) {
					columnNameAggregate = "Cat" + categoryOrder + "Fuel" + idFuel ;

					rCategory++ ;
					tableCat.setValue( category , rCategory , "Category" ) ;
					tableCat.setValue( 0 , rCategory , "Count" ) ;
					tableCat.setValue( 0 , rCategory , "Expanded" ) ;
					tableCat.setValue( columnNameAggregate , rCategory , "AggregateColumn" ) ;
					tableCat.setValue( colIndex , rCategory , "AggregateColumnNumber" ) ;
					tableCat.setValue( categoryOrder , rCategory , "CategoryOrder" ) ;
					tableCat.setValue( rowCatList , rCategory , "RowCategoryList" ) ;
					tableCat.setValue(
							tableCategoryList.getObject( rowCatList , indexCatListColor ) ,
							rCategory , "BackgroundColor" ) ;

					tableCatColumns = tableCategoryColumns.copyStructure() ;
					rCatColumns = 0 ;

					tableCat.setNumRowsActive( rCategory + 1 ) ;
					tableCat.setValue( tableCatColumns , rCategory , "CategoryColumns" ) ;

					countCategory = 0 ;

					tableData.addColumns( columnNameAggregate ) ;
					tableData.setNumRowsActive( tableYear.getNumRowsActive() ) ;

					colIndex++ ;
				}

				lastCategory = category ;

				countCategory++ ;
				tableCat.setValue( countCategory , rCategory , "Count" ) ;

				columnName = "Cat" + categoryOrder + "Fuel" + idFuel + "Engine" + idEngine ;
				columnDesc = "";

				tableFuelEngTechAssoc.setValue( columnNameAggregate , row , "ColAggregate" ) ;
				tableFuelEngTechAssoc.setValue( columnName , row , "ColFraction" ) ;

				tableData.addColumns( columnName ) ;

				// Add CategoryColumn
				tableCatColumns.setNumRowsActive( rCatColumns + 1 ) ;
				tableCatColumns.setValue( tableCat , rCatColumns , "TableCategory" ) ;
				tableCatColumns.setValue( columnName , rCatColumns , "ColumnName" ) ;
				tableCatColumns.setValue( columnDesc , rCatColumns , "ColumnDesc" ) ;
				tableCatColumns.setValue( colIndex , rCatColumns , "ColumnNumber" ) ;
				tableCatColumns.setValue( row , rCatColumns , "RowFeta" ) ;
				rCatColumns++ ;

				colIndex++ ;
			}

			tableData.addColumns( "Sum" ) ;

			// Now for every column, default data to 0.0F for Fractions
			for ( colIndex = 1 ; colIndex < tableData.getColumnCount() ; colIndex++ ) {
				for ( rowData = 0 ; rowData < tableYear.getNumRowsActive() ; rowData++ ) {
					tableData.setValue( 0.0F , rowData , colIndex ) ;
				}
			}
			//System.out.println("AVFT srcTypeId=" + srcTypeId + ", tableData.getNumRowsActive()=" + tableData.getNumRowsActive());
		} // Next SourceUseCategory

		// Get default values
		sql = "select sourceTypeModelYearID, fuelTypeID, engTechID,"
				+ " sum(stmyFraction) as fuelEngFraction, svp.sourceTypeID,"
				+ " svp.modelYearID as YearId"
				+ " from sampleVehiclePopulation svp"
				+ " group by svp.sourceTypeModelYearID, fuelTypeID, engTechID"
				+ " order by svp.sourceTypeModelYearID, fuelTypeID, engTechID";
		SQLRunner.Query query = new SQLRunner.Query();
		//int howManyLoaded = 0;
		//double sum1960 = 0;
		try {
			query.open(conn,sql);
			while(query.rs.next()) {
				int sourceTypeID = query.rs.getInt("sourceTypeID");
				int modelYearID = query.rs.getInt("YearId");
				int fuelTypeID = query.rs.getInt("fuelTypeID");
				int engTechID = query.rs.getInt("engTechID");
				double fuelEngFraction = query.rs.getFloat("fuelEngFraction");
				if(fuelEngFraction > 0) {
					setMemory(""+sourceTypeID,""+modelYearID,"Cat1Fuel" + fuelTypeID + "Engine" + engTechID,""+fuelEngFraction);
					/*
					if(sourceTypeID == 11) {
						howManyLoaded++;
						if(modelYearID == 1960) {
							sum1960 += fuelEngFraction;
						}
					}
					*/
				}
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to get AVFT default motorcycle values",sql);
		} finally {
			query.onFinally();
		}
		//System.out.println("AVFT loaded " + howManyLoaded + " default values for source type 11.  1960 summed to " + sum1960);
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("\t\tAVFT:\r\n");

		if(strategy.dataSourceFileName == null || strategy.dataSourceFileName.length() <= 0) {
			// No data has been specified
			destination.append("\t\t\tNo AVFT parameters specified.\r\n");
			return;
		}

		// Data source information
		destination.append("\t\t\tData source file name:\r\n");
		destination.append("\t\t\t" + strategy.dataSourceFileName);

		if(strategy.dataSourceFileType != null && strategy.dataSourceFileType.length() > 0) {
			destination.append("\t\t\tData source file type:\r\n");
			destination.append("\t\t\t" + strategy.dataSourceFileType);
		}

		if(strategy.dataSourceWorksheetName != null
				&& strategy.dataSourceWorksheetName.length() > 0) {
			destination.append("\t\t\tData source worksheet name:\r\n");
			destination.append("\t\t\t" + strategy.dataSourceWorksheetName);
		}

		// Message information
		if(strategy.messages.size() <= 0) {
			destination.append("\t\t\tThere are no AVFT messages, warnings, or errors\r\n");
		} else {
			destination.append("\t\t\tMessages:\r\n");
			for(Iterator i=strategy.messages.iterator();i.hasNext();) {
				destination.append(i.next().toString() + "\r\n");
			}
		}
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		recentRunSpec = runspec;
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		recentRunSpec = runspec;
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,TreeMap sections) {
		recentRunSpec = runspec;
		if ( isAccurateSums() == true ) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK) ;
		} else {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
	}

	/**
	 * Obtain an explanation for a recent called to calculateRunSpecSectionStatus that
	 * yielded anything other than total success.
	 * @return human-readable text explaining the current state.
	**/
	public String explainRunSpecSectionStatus() {
		return inaccurateSumsExplanation;
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,TreeMap sections) {
		recentRunSpec = runspec;
		// Nothing needs to be done here since defaults are handled elsewhere for this
		// type of InternalControlStrategy.
		return null;
	}

	/**
	 * Update current selections to be consistent with a newly selected ModelScale.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus onScaleChange(RunSpec runspec,TreeMap sections) {
		recentRunSpec = runspec;
		// Nothing special to do here
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * Unloads the data from memory in preparation for a new set of data
	**/
	public void unload() {
		loadTablesFromFuelEngTechAssoc() ;
	}

	/**
	 * Clears all data from the TableFractionData memory tables for each SourceUseType
	**/
	void clearAllData() {
		int rowSUT , rowDATA , column ;
		String sourceType ;
		EPATableModel tableData ;

		for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
			sourceType = tableSourceUseType.getString( rowSUT , "SourceTypeId" ) ;

			tableData = ( EPATableModel )
					tableSourceUseType.getObject( rowSUT , "TableFractionData" ) ;
			if ( tableData == null ) {
				continue ;
			}

			for ( rowDATA = 0 ; rowDATA < tableData.getNumRows() ; rowDATA++ ) {
				for ( column = 1 ; column < tableData.getColumnCount() ; column++ ) {
					tableData.setValue( 0.0F , rowDATA , column ) ;
				}
			}
		}
	}

	/**
	 * Sets the sums for each Category
	**/
	void setSums() {
		int rowSUT , srcType ;

		for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
			srcType = tableSourceUseType.getInt( rowSUT , "SourceTypeId" ) ;
			calculateSourceTypeSums( srcType ) ;
		}
	}

	/**
	 * Loads the tableAvftActive from the data stored in tableFuelEngFraction
	 * for the SourceTypeId as set in the member variable 'sourceTypeId'.
	 * The 'sourceTypeId' must be set prior to calling this method.
	 * @return	Returns true if able to load the data successfully.
	**/
	boolean loadTablesFromFuelEngTechAssoc() {
		isAccurateSumsAnswer = null;

		int rSourceUseType , colIndex , rowData , row , year , rCategory ;
		int indexYearId , indexSourceTypeId , indexFuelEngFraction ;
		int type , fuelTypeId , engTechId , rowYear , srcType , rowSUT ;
		int indexAggregate , indexCell , indexSum , rowFeta ;
		float amount , amountSum , amountAggregate ;
		String columnNameAggregate , columnName , categoryOrder ;
		EPATableModel tableData , tableCategory ;
		boolean b ;

		//clearAllData() ;

		for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
			srcType = tableSourceUseType.getInt( rowSUT , "SourceTypeId" ) ;
			tableData = ( EPATableModel ) tableSourceUseType.getObject( rowSUT ,
					"TableFractionData" );
			tableCategory = ( EPATableModel ) tableSourceUseType.getObject( rowSUT ,
					"TableCategory" );

			if ( tableData == null || tableCategory == null ) {
				Logger.log( LogMessageCategory.WARNING ,
						"loadTablesFromFuelEngTechAssoc - table memory data is null." ) ;
				return false ;
			}

			rSourceUseType = srcType ;

			// To save time on text searches, find these index values in advance
			indexYearId = tableFuelEngFraction.getColumnIndex( "modelYearID" ) ;
			indexSourceTypeId = tableFuelEngFraction.getColumnIndex( "SourceTypeId" ) ;
			indexFuelEngFraction = tableFuelEngFraction.getColumnIndex( "FuelEngFraction" ) ;
			if ( indexYearId == -1 || indexSourceTypeId == -1 || indexFuelEngFraction == -1 ) {
				//System.out.println("loadTablesFromFuelEngTechAssoc indexYearId=" + indexYearId + ", indexSourceTypeId=" + indexSourceTypeId + ", indexFuelEngFraction=" + indexFuelEngFraction);
				//tableFuelEngFraction.printDataRows( "fef.txt" ) ;
				return false ;
			}

			indexSum = tableData.getColumnIndex( "Sum" ) ;
			if ( indexSum == -1 ) {
				//System.out.println("loadTablesFromFuelEngTechAssoc indexSum=" + indexSum);
				return false ;
			}

			// Now search tableFuelEngFraction rows and fill in data
			//int howManyFEF = 0;
			for ( row = 0 ; row < tableFuelEngFraction.getNumRows() ; row++ ) {
				type = tableFuelEngFraction.getInt( row , indexSourceTypeId ) ;

				if ( type != srcType ) {
					continue ;
				}

				year = tableFuelEngFraction.getInt( row , indexYearId ) ;
				rowYear = tableData.find( year , "YearId" ) ;

				if ( rowYear == -1 ) {
					continue ;
				}

				amount = tableFuelEngFraction.getFloat( row , indexFuelEngFraction ) ;
				amount = FloatConversion.toFloat( amount , 6 ) ;

				fuelTypeId = tableFuelEngFraction.getInt( row , "FuelTypeId" ) ;
				engTechId = tableFuelEngFraction.getInt( row , "EngTechId" ) ;
				categoryOrder = tableFuelEngTechAssoc.getString( row , "CategoryDisplayOrder" ) ;

				rowFeta = findFetaRow( srcType , fuelTypeId , engTechId ) ;
				if ( rowFeta == -1 ) {
					continue ;
				}

				columnNameAggregate = tableFuelEngTechAssoc.getString( rowFeta , "ColAggregate" ) ;
				columnName = tableFuelEngTechAssoc.getString( rowFeta , "ColFraction" ) ;

				indexAggregate = tableData.getColumnIndex( columnNameAggregate ) ;
				indexCell = tableData.getColumnIndex( columnName ) ;
				/*
				if ( indexAggregate == -1 || indexCell == -1 ) {
					if ( year == 2001 ) {
						System.out.println( "\tindex=-1 - continue " +
								"  srcType: " + srcType + "  row: " + row +
								"  amount: " + amount + "\n" +
								"  colAgg: " + columnNameAggregate + "(" + indexAggregate + ")\n" +
								"  colCell: " + columnName + "(" + indexCell + ")" ) ;
					}
				}
				*/
				tableData.setValue( amount , rowYear , indexCell ) ;

				amountAggregate = tableData.getFloat( rowYear , indexAggregate ) ;
				tableData.setValue( amount + amountAggregate , rowYear , indexAggregate ) ;

				amountSum = tableData.getFloat( rowYear , indexSum ) ;
				tableData.setValue( amount + amountSum , rowYear , indexSum ) ;
				//howManyFEF++;
			}

			calculateSourceTypeSums( srcType ) ;
			//tableData.printDataRows( "avft" + srcType + ".txt" ) ;
			//System.out.println("srcType=" + srcType + " howManyFEF=" + howManyFEF);
		}

		resetFromDefaultTables = true ;
		resetXML = null ;
		resetTSV = null ;

		return true ;
	} // end of loadTablesFromFuelEngTechAssoc()

	/**
	 * This method provides the functionality for the cancel button. It returns the memory
	 * in the tables back to the data as it was from the last Import or load.
	 * If the screen needs to be repainted, then it is the responsibility of the caller to
	 * execute this command
	**/
	public void cancel() {
		if ( resetXML != null ) {
			acceptXML( "AVFTPanel" , resetXML ) ;
		} else if ( resetTSV != null ) {
			acceptTSV( "AVFTPanel" , resetTSV ) ;
		} else if ( resetFromDefaultTables == true ) {
			loadTablesFromFuelEngTechAssoc() ;
		}
	}

	/**
	 * This method examines all SourceTypes and evaluates whether the Sums total 100%.
	 * @return Returns true if all sums equal 100%, false if they do not.
	**/
	public boolean isAccurateSums() {
		if(isAccurateSumsAnswer != null) {
			return isAccurateSumsAnswer.booleanValue();
		}

		boolean result = true;
		TreeSet<String> invalidModelYears = new TreeSet<String>();
		TreeSet<String> invalidSourceTypes = new TreeSet<String>();

		int rowSUT , rowDATA , srcTypeId ;
		float f ;
		EPATableModel tableData , tableCategory ;

		//System.out.println( "isAccurateSums() called..." ) ;

		for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
			String sourceType = tableSourceUseType.getString( rowSUT , "SourceTypeId" ) ;

			tableData = ( EPATableModel ) tableSourceUseType.getObject( rowSUT ,
					"TableFractionData" );
			tableCategory = ( EPATableModel ) tableSourceUseType.getObject( rowSUT ,
					"TableCategory" );
			if ( tableData == null || tableCategory == null ) {
				continue ;
			}

			srcTypeId = tableSourceUseType.getInt( rowSUT , 0 ) ;
			calculateSourceTypeSums( srcTypeId ) ;

			for ( rowDATA = 0 ; rowDATA < tableData.getNumRows() ; rowDATA++ ) {
				f = tableData.getFloat( rowDATA , "Sum" ) ;
				if ( f < 0.99999F || f > 1.00001F ) {
					result = false;
					String year = tableData.getString( rowDATA, "YearId" );
					invalidModelYears.add(year);
					invalidSourceTypes.add(sourceType);
				}
			}
		}

		if(result) {
			isAccurateSumsAnswer = Boolean.TRUE;
			inaccurateSumsExplanation = null;
		} else {
			isAccurateSumsAnswer = Boolean.FALSE;
			inaccurateSumsExplanation = "General issues";
			if(invalidModelYears.size() == 1 && invalidSourceTypes.size() == 1) {
				inaccurateSumsExplanation = "Model Year "
						+ invalidModelYears.first() + " does not sum to 100% for source type "
						+ invalidSourceTypes.first();
			} else {
				inaccurateSumsExplanation = "Rows for ";
				if(invalidModelYears.size() == 1) {
					inaccurateSumsExplanation += "model year " + invalidModelYears.first();
				} else {
					inaccurateSumsExplanation += "several model years (";
					int count = 0;
					for(Iterator i=invalidModelYears.iterator();i.hasNext();count++) {
						if(count >= 5) {
							inaccurateSumsExplanation += ", and others";
							break;
						}
						String year = (String)i.next();
						if(count > 0) {
							inaccurateSumsExplanation += ",";
						}
						inaccurateSumsExplanation += year;
					}
					inaccurateSumsExplanation += ")";
				}
				inaccurateSumsExplanation += " and for ";
				if(invalidSourceTypes.size() == 1) {
					inaccurateSumsExplanation += "source type " + invalidSourceTypes.first();
				} else {
					inaccurateSumsExplanation += "several source types (";
					int count = 0;
					for(Iterator i=invalidSourceTypes.iterator();i.hasNext();count++) {
						if(count >= 5) {
							inaccurateSumsExplanation += ", and others";
							break;
						}
						String sourceType = (String)i.next();
						if(count > 0) {
							inaccurateSumsExplanation += ",";
						}
						inaccurateSumsExplanation += sourceType;
					}
					inaccurateSumsExplanation += ")";
				}
				inaccurateSumsExplanation += " do not sum to 100%";
			}
		}
		return result;
	}

	/**
	 * This method examines a FractionTable connected to a selected SourceUseTypeId
	 * and updates the Summary columns (the Aggregates and the Total Sum)
	 * @param srcTypeId The Source Type ID for which to calculate the sums
	**/
	public void calculateSourceTypeSums( int srcTypeId ) {
		int rowSUT , rowDATA ;
		EPATableModel tableData , tableCategory ;

		rowSUT = tableSourceUseType.find( srcTypeId , "SourceTypeId" ) ;
		if ( rowSUT == -1 ) {
			return ;
		}

		tableData = ( EPATableModel ) tableSourceUseType.getObject( rowSUT , "TableFractionData" );
		tableCategory = ( EPATableModel ) tableSourceUseType.getObject( rowSUT , "TableCategory" );

		if ( tableData == null || tableCategory == null ) {
			return ;
		}

		for ( rowDATA = 0 ; rowDATA < tableData.getNumRows() ; rowDATA++ ) {
			calculateSums( tableData , tableCategory , rowDATA ) ;
		}
	}

	/**
	 * This method examines the selected row on the currently displayed panel
	 * and updates the Summary columns (the Aggregates and the Total Sum)
	 * @param row The row number for which to calculate the Sums
	**/
//	public void calculateSums( int row ) {
//		calculateSums( tableAvftActive , tableCategoryActive , row ) ;
//	}

	/**
	 * This method examines the selected row on the currently displayed panel
	 * and updates the Summary columns (the Aggregates and the Total Sum)
	 * @param tableData The EPATableModel that has the data for which to calculate sums
	 * @param tableCategory The EPATableModel that has the category definition for which
	 * to calculate sums
	 * @param row The row number for which to calculate the Sums
	**/
	public void calculateSums( EPATableModel tableData , EPATableModel tableCategory , int row ) {
		isAccurateSumsAnswer = null;

		float aggregate = 0.0F , sum = 0.0F , amount ;
		int rowCategory , rowColumn ;
		EPATableModel tableColumns ;
		String columnName ;

		if ( tableData == null || tableCategory == null ) {
			return ;
		}

		for ( rowCategory = 0 ; rowCategory < tableCategory.getNumRows() ; rowCategory++ ) {
			tableColumns = (EPATableModel) tableCategory.getObject(rowCategory,"CategoryColumns");
			if ( tableColumns == null ) {
				continue ;
			}

			aggregate = 0.0F ;
			for ( rowColumn = 0 ; rowColumn < tableColumns.getNumRows() ; rowColumn++ ) {
				columnName = tableColumns.getString( rowColumn , "ColumnName" ) ;
				amount = tableData.getFloat( row , columnName ) ;
				aggregate += amount ;
			}

			columnName = tableCategory.getString( rowCategory , "AggregateColumn" ) ;
			tableData.setValue( aggregate , row , columnName ) ;
			sum += aggregate ;
		}

		tableData.setValue( sum , row , "Sum" ) ;
	}

	/**
	 * This method examines the selected row. If the sums do not add up to 100%, then
	 * the method will update data values and increase each non-zero value proportionally
	 * so that the sum for the row is 100%.
	 * @param tableData The EPATableModel that has the data for which to calculate sums
	 * @param tableCategory The EPATableModel that has the category definition for which
	 * to calculate sums
	 * @param row The row number for which to normalize
	**/
	public void normalize( EPATableModel tableData , EPATableModel tableCategory , int row ) {
		float newAmount , sum , amount ;
		int rowCategory , rowColumn , column ;
		EPATableModel tableColumns ;
		String columnName ;
		int [] columnNumbers = new int[ 500 ] ;
		BigDecimal [] columnValues = new BigDecimal[ 500 ] ;
		int index = 0 , x ;
		float pctChange = 0.0F ;

		if ( tableData == null || tableCategory == null ) {
			return ;
		}

		if ( row < 0 || row >= tableData.getNumRows() ) {
			return ;
		}

		sum = tableData.getFloat( row , "Sum" ) ;
		if ( sum == 1.0F || sum == 0.0F ) {
			return ;
		}

		pctChange = 1.0F / sum ;

		for ( rowCategory = 0 ; rowCategory < tableCategory.getNumRows() ; rowCategory++ ) {
			tableColumns = (EPATableModel) tableCategory.getObject(rowCategory,"CategoryColumns");
			if ( tableColumns == null ) {
				continue ;
			}

			for ( rowColumn = 0 ; rowColumn < tableColumns.getNumRows() ; rowColumn++ ) {
				column = tableColumns.getInt( rowColumn , "ColumnNumber" ) ;
				columnName = tableColumns.getString( rowColumn , "ColumnName" ) ;
				amount = tableData.getFloat( row , column ) ;

				newAmount = amount * pctChange ;

				tableData.setValue( newAmount , row , column ) ;

				if ( amount > 0.0F ) {
					newAmount = amount * pctChange ;

					tableData.setValue( newAmount , row , column ) ;

					columnNumbers[ index ] = tableColumns.getInt( rowColumn , "ColumnNumber" ) ;
					columnValues[ index ] = FloatConversion.toBigDecimal( amount , 6 ) ;
					index++ ;
				}
			}
		}

		calculateSums( tableData , tableCategory , row ) ;
	}

	/**
	 * This method examines a selected SourceType. If the sums do not add up to 100%, then
	 * the method will update data values for each row and increase each non-zero value
	 * proportionally so that the sum for each row is 100%.
	 * @param srcTypeId The Source Type ID for which to calculate the sums
	**/
	public void normalizeSourceType( int srcTypeId ) {
		isAccurateSumsAnswer = null;

		int rowSUT , rowDATA ;
		EPATableModel tableData , tableCategory ;

		rowSUT = tableSourceUseType.find( srcTypeId , "SourceTypeId" ) ;
		if ( rowSUT == -1 ) {
			return ;
		}

		//System.out.println( "Normalize Source Type: " + srcTypeId + "  rowSUT: " + rowSUT ) ;

		tableData = ( EPATableModel ) tableSourceUseType.getObject( rowSUT , "TableFractionData" );
		tableCategory = ( EPATableModel ) tableSourceUseType.getObject( rowSUT , "TableCategory" );

		if ( tableData == null || tableCategory == null ) {
			return ;
		}

		for ( rowDATA = 0 ; rowDATA < tableData.getNumRows() ; rowDATA++ ) {
			normalize( tableData , tableCategory , rowDATA ) ;
		}
	}
}
