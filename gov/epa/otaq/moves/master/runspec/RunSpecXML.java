/**************************************************************************************************
 * @(#)RunSpecXML.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.lang.Integer;
import java.lang.StringBuffer;
import java.sql.Connection;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.gui.MOVESWindow;
import gov.epa.otaq.moves.master.framework.*;

/**
 * Class for Run Spec XML loading/saving. Loads from an XML file into RunSpec.
 * Saves a RunSpec into an XML file.
 *
 *<br>
 * Sample RunSpec XML file
 * <pre>
 * &lt;runspec&gt;
 *	&lt;description&gt;&lt;/description&gt;
 * 	&lt;modelscale value="MACROSCALE"/&gt;
 * 	&lt;geographicselections&gt;
 * 		&lt;geographicselection type="COUNTY" key="26161" description="MICHIGAN - Washtenaw County"/&gt;
 * 	&lt;/geographicselections&gt;
 * 	&lt;timespan&gt;
 * 		&lt;year key="2001"/&gt;
 * 		&lt;month key="6"/&gt;
 * 		&lt;day key="0"/&gt;
 * 		&lt;beginhour key="6"/&gt;
 * 		&lt;endhour key="6"/&gt;
 * 	&lt;/timespan&gt;
 * 	&lt;onroadvehicleselections&gt;
 * 		&lt;onroadvehicleselection fueltypeid="1" fueltypedesc="Gasoline" sourcetypeid="21" sourcetypename="Passenger Car"/&gt;
 * 	&lt;/onroadvehicleselections&gt;
 *	&lt;offroadvehicleselections&gt;
 * 	&lt;/offroadvehicleselections&gt;
 * 	&lt;offroadvehiclesccs&gt;
 * 	&lt;/offroadvehiclesccs&gt;
 * 	&lt;roadtypes&gt;
 * 		&lt;roadtype roadtypeid="23" roadtypename="Urban Interstate"/&gt;
 * 	&lt;/roadtypes&gt;
 * 	&lt;inputdatabase servername="" databasename="" description=""/&gt;
 * 	&lt;uncertaintyparameters uncertaintymodeenabled="false" numberofrunspersimulation="0" numberofsimulations="0"/&gt;
 * 	&lt;geographicoutputdetail description="COUNTY"/&gt;
 * 	&lt;outputdatabase servername="" databasename="JUnitTestOutput" description=""/&gt;
 * 	&lt;outputtimestep value="Hour"/&gt;
 * 	&lt;outputvmtdata value="false"/&gt;
 * 	&lt;scaleinputdatabase servername="" databasename="" description=""/&gt;
 * 	&lt;pmsize value="0"/&gt;
 * 	&lt;outputfactors&gt;
 * 		&lt;timefactors selected="true" units="Seconds"/&gt;
 * 		&lt;distancefactors selected="true" units="Miles"/&gt;
 * 		&lt;massfactors selected="true" units="Grams" energyunits="Million BTU"/&gt;
 * 	&lt;/outputfactors&gt;
 * &lt;/runspec&gt;
 * </pre>
 *
 * @author		Wes Faler
 * @author		Don Smith
 * @author 		Tim Hull
 * @author		Mike Kender	task 1903
 * @version		2019-10-18
**/
public class RunSpecXML {
	/**
	 * A shortcut to the parent RunSpec of this object, used to access the member variables of
	 * the parent RunSpec as a result of loading/saving.
	**/
	RunSpec runSpec;

	/** Constructor
	 * @param parent The RunSpec to load to and save from.
	**/
	public RunSpecXML(RunSpec parent) {
		runSpec = parent;
	}

	/**
	 * Loads from the XML file into the RunSpec.
	 * @param inFile The XML file to read from.
	 * @return True on success.
	**/
	public boolean load(File inFile) {
		if(!PollutantProcessLoader.isLoaded()) {
			PollutantProcessLoader.loadFromDatabase();
		}
		if(!TimeSpan.isLoaded()) {
			TimeSpan.loadTimeObjects();
		}
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			db.setErrorHandler(new XMLErrorHandler());
			Document doc = db.parse(inFile);
			// Verify that this is a runspec xml file
			Node runSpecNode = doc.getFirstChild();
			if(!(runSpecNode != null && runSpecNode.getNodeName().equalsIgnoreCase("runspec"))) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.ERROR, "Invalid RunSpec XML file.");
				return false;
			}
			// process Version from main node
			processVersion(runSpecNode);
			// Handle each "sub-node" under the main <runspec> tag, these are immediate child nodes
			for(Node subNode = runSpecNode.getFirstChild(); subNode != null;
					subNode = subNode.getNextSibling()) {
				processRunSpecSubNode(subNode);
			}
			if(runSpec.scale == ModelScale.MESOSCALE_LOOKUP) {
				runSpec.geographicOutputDetail = GeographicOutputDetailLevel.LINK;
			}
//			if(runSpec.domain == ModelDomain.NATIONAL_ALLOCATION) {
//				runSpec.scaleInputDatabase = new DatabaseSelection();
//			}
			// Enforce data consistency rules. Most rules are enforced by the GUI
			// and allowed to be flexible when directly loaded though.
			enforceConsistency();
		} catch(SAXException e) {
			// If we get this exception, then we don't have a chance to read any fields
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.logError(e,"Could not load runspec XML.");
		} catch(Exception e) {
			// This indicates an error during one field's parsing
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.logError(e,"Could not load runspec XML");
			return false;
		} finally {
			// Uncertainty is disabled in this version
			runSpec.outputEmissionsBreakdownSelection.estimateUncertainty =  false;
		}
		return true;
	}

	/**
	 * Handles a "sub-node" under the main <runspec> tag, this is an immediate child node
	 * @param node A Node object as an immediate child node
	**/
	public void processRunSpecSubNode(Node node) {
		String nodeName = node.getNodeName();
		if(nodeName.equalsIgnoreCase("description")) {
			Node childNode = node.getFirstChild();
			for(Node child = node.getFirstChild(); child != null;
					child = child.getNextSibling()) {
				if(child.getNodeType() == Node.TEXT_NODE
							|| child.getNodeType() == Node.CDATA_SECTION_NODE) {
					runSpec.description = StringUtilities.safeGetString(childNode.getNodeValue());
					break;
				}
			}
		} else if(nodeName.equalsIgnoreCase("models")) {
			processModels(node);
		} else if(nodeName.equalsIgnoreCase("modelscale")) {
			processModelScale(node);
		} else if(nodeName.equalsIgnoreCase("modeldomain")) {
			processModelDomain(node);
		} else if(nodeName.equalsIgnoreCase("genericcounty")) {
			processGenericCounty(node);
		} else if(nodeName.equalsIgnoreCase("geographicselections")){
			processGeographicSelections(node);
		} else if(nodeName.equalsIgnoreCase("timespan")) {
			processTimeSpan(node);
		} else if(nodeName.equalsIgnoreCase("onroadvehicleselections")) {
			processOnRoadVehicleSelections(node);
		} else if(nodeName.equalsIgnoreCase("offroadvehicleselections")) {
			processOffRoadVehicleSelections(node);
		} else if(nodeName.equalsIgnoreCase("offroadvehiclesccs")) {
			processOffRoadVehicleSCCs(node);
		} else if(nodeName.equalsIgnoreCase("roadtypes")) {
			processRoadTypes(node);
		} else if(nodeName.equalsIgnoreCase("pollutantprocessassociations")) {
			processPollutantProcessAssociations(node);
		} else if(nodeName.equalsIgnoreCase("databaseselections")) {
			processDatabaseSelections(node);
		} else if(nodeName.equalsIgnoreCase("internalcontrolstrategies")) {
			processInternalControlStrategies(node);
		} else if(nodeName.equalsIgnoreCase("inputdatabase")) {
			processInputDatabase(node);
		} else if(nodeName.equalsIgnoreCase("uncertaintyparameters")) {
			processUncertaintyParameters(node);
		} else if(nodeName.equalsIgnoreCase("geographicoutputdetail")) {
			processGeographicOutputDetail(node);
		} else if(nodeName.equalsIgnoreCase("outputemissionsbreakdownselection")) {
			processOutputEmissionsBreakdownSelection(node);
		} else if(nodeName.equalsIgnoreCase("outputdatabase")) {
			processOutputDatabase(node);
		} else if(nodeName.equalsIgnoreCase("outputtimestep")) {
			processOutputTimeStep(node);
		} else if(nodeName.equalsIgnoreCase("outputvmtdata")) {
			processOutputVMTData(node);
		} else if(nodeName.equalsIgnoreCase("outputsho")) {
			processOutputSHO(node);
		} else if(nodeName.equalsIgnoreCase("outputsh")) {
			processOutputSH(node);
		} else if(nodeName.equalsIgnoreCase("outputshp")) {
			processOutputSHP(node);
		} else if(nodeName.equalsIgnoreCase("outputshidling")) {
			processOutputSHIdling(node);
		} else if(nodeName.equalsIgnoreCase("outputstarts")) {
			processOutputStarts(node);
		} else if(nodeName.equalsIgnoreCase("outputpopulation")) {
			processOutputPopulation(node);
//		} else if(nodeName.equalsIgnoreCase("hydrocarbonunitsystem")) {
//			processHydrocarbonUnitSystem(node);
		} else if(nodeName.equalsIgnoreCase("scaleinputdatabase")) {
			processScaleInputDatabase(node);
		} else if(nodeName.equalsIgnoreCase("pmsize")) {
			processPMSize(node);
		} else if(nodeName.equalsIgnoreCase("outputfactors")) {
			processOutputFactors(node);
		} else if(nodeName.equalsIgnoreCase("savedata")) {
			processClassList(node,runSpec.classesToSaveData);
		} else if(nodeName.equalsIgnoreCase("donotexecute")) {
			processClassList(node,runSpec.classesNotToExecute);
		} else if(nodeName.equalsIgnoreCase("generatordatabase")) {
			processGeneratorDatabase(node);
		} else if(nodeName.equalsIgnoreCase("doNotPerformFinalAggregation")) {
			runSpec.doNotPerformFinalAggregation = getBooleanAttribute(node, "selected");
		} else if(nodeName.equalsIgnoreCase("lookuptableflags")) {
			processLookupTableFlags(node);
		} else if(nodeName.equalsIgnoreCase("skipdomaindatabasevalidation")) {
			runSpec.skipDomainDatabaseValidation = getBooleanAttribute(node, "selected");
        }
	}
	
	private void processVersion(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("version")) {
				runSpec.version = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
		}
	}

	private void processModels(Node node) {
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if ( subNode.getNodeName().equalsIgnoreCase("model")) 
				processModel(subNode);
		}
	}

	private void processModel(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		Model parsedModel = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedModel = Model.getByName(
						StringUtilities.safeGetString(attributeNode.getNodeValue()));
			}
		}
		if(parsedModel != null) {
			runSpec.models.clear();
			runSpec.models.add( parsedModel);
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid Model");
		}
	}

	/**
	 * Load a list of class names.
	 * @param node the Node object to read
	 * @param classNames set of class names to be populated
	**/
	void processClassList(Node node, TreeSetIgnoreCase classNames) {
		// This node can have 0 - n child nodes
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("class")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid class name entry");
			} else {
				NamedNodeMap attributes = subNode.getAttributes();
				boolean found = false;
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("name")) {
						classNames.add(attributeNode.getNodeValue());
						found = true;
					}
				}
				if(!found) {
					/** @explain The RunSpec file is corrupt, likely due to a typo. **/
					Logger.log(LogMessageCategory.WARNING, "Invalid class name entry");
				}
			}
		}
	}

	/**
	 * Handles the ModelScale node.
	 * @param node The Node object to handle.
	**/
	void processModelScale(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		ModelScale parsedScale = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedScale = ModelScale.getByName(
						StringUtilities.safeGetString(attributeNode.getNodeValue()));
			}
		}
		if(parsedScale != null) {
			runSpec.scale = parsedScale;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid ModelScale");
		}
	}

	/**
	 * Handles the ModelDomain node.
	 * @param node The Node object to handle.
	**/
	void processModelDomain(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		ModelDomain parsedDomain = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedDomain = ModelDomain.getByName(
						StringUtilities.safeGetString(attributeNode.getNodeValue()));
			}
		}
		if(parsedDomain != null) {
			runSpec.domain = parsedDomain;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid ModelDomain");
		}
	}

	/**
	 * Handles the GenericCounty node.
	 * @param node The Node object to handle.
	**/
	void processGenericCounty(Node node) {
		GenericCounty g = new GenericCounty();

		for(Node child = node.getFirstChild(); child != null; child = child.getNextSibling()) {
			String nodeName = child.getNodeName();
			if(nodeName.equalsIgnoreCase("shortid")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.shortCountyID = (Integer.valueOf(attributeNode.getNodeValue())).intValue();
					}
				}
			} else if(nodeName.equalsIgnoreCase("stateid")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.stateID = (Integer.valueOf(attributeNode.getNodeValue())).intValue();
					}
				}
			} else if(nodeName.equalsIgnoreCase("description")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.description = attributeNode.getNodeValue();
					}
				}
			} else if(nodeName.equalsIgnoreCase("altitude")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.setPressureFromAltitude(
								attributeNode.getNodeValue().equalsIgnoreCase("H"));
					}
				}
			} else if(nodeName.equalsIgnoreCase("gpafraction")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.gpaFraction = (Float.valueOf(attributeNode.getNodeValue())).floatValue();
					}
				}
			} else if(nodeName.equalsIgnoreCase("barometricpressure")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.barometricPressure = (Float.valueOf(attributeNode.getNodeValue())).floatValue();
					}
				}
			} else if(nodeName.equalsIgnoreCase("refuelvaporadjust")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.refuelingVaporProgramAdjust = (Float.valueOf(attributeNode.getNodeValue())).floatValue();
					}
				}
			} else if(nodeName.equalsIgnoreCase("refuelspilladjust")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
						g.refuelingSpillProgramAdjust = (Float.valueOf(attributeNode.getNodeValue())).floatValue();
					}
				}
			}
		}
		if(g.shortCountyID >= 1 && g.shortCountyID <= 999
				&& g.stateID >= 1 && g.stateID <= 99
				&& g.gpaFraction >= 0 && g.gpaFraction <= 1.0
				&& g.barometricPressure >= 0) {
			runSpec.genericCounty = g;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid Generic County.");
		}
	}

	/**
	 * Handles the GeographicSelections node.
	 * @param node The Node object to handle.
	**/
	void processGeographicSelections(Node node) {
		// This node can have 0 - n child nodes
		if ( !node.hasChildNodes()) { // 0 child nodes
			return;
		}
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("geographicselection")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid GeographicSelection.");
			}
			NamedNodeMap attributes = subNode.getAttributes();
			GeographicSelectionType parsedType = null;
			String parsedTextDescription = null;
			int parsedDatabaseKey = 0;
			for(int i = 0; i < attributes.getLength(); i++) {
				Node attributeNode = attributes.item(i);
				if(attributeNode.getNodeName().equalsIgnoreCase("type")) {
					parsedType = GeographicSelectionType.getByName(attributeNode.getNodeValue());
				} else if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
					parsedDatabaseKey = safeParseInt(attributeNode.getNodeValue());
				} else if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
					parsedTextDescription = new String(attributeNode.getNodeValue());
				}
			}
			if(parsedType != null && (parsedType == GeographicSelectionType.NATION)||
					(parsedDatabaseKey != 0 && parsedTextDescription != null)) {
				GeographicSelection parsedSelection = new GeographicSelection();
				parsedSelection.type = parsedType;
				parsedSelection.databaseKey = parsedDatabaseKey;
				parsedSelection.textDescription = parsedTextDescription;
				runSpec.geographicSelections.add(parsedSelection);
			} else {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid GeographicSelection.");
			}
		}
	}

	/**
	 * Handles the TimeSpan node.
	 * @param node The Node object to handle.
	**/
	void processTimeSpan(Node node) {
		TimeSpan timeSpan = new TimeSpan();
		for(Node child = node.getFirstChild(); child != null; child = child.getNextSibling()) {
			String nodeName = child.getNodeName();
			if(nodeName.equalsIgnoreCase("year")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
						timeSpan.years.add(Integer.valueOf(attributeNode.getNodeValue()));
					}
				}
			} else if(nodeName.equalsIgnoreCase("month")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
						TimeSpan.Month m = TimeSpan.getMonthByIndex(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(m != null) {
							timeSpan.months.add(m);
						}
					} else if(attributeNode.getNodeName().equalsIgnoreCase("id")) {
						TimeSpan.Month m = TimeSpan.getMonthByID(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(m != null) {
							timeSpan.months.add(m);
						}
					}
				}
			} else if(nodeName.equalsIgnoreCase("day")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
						TimeSpan.Day d = TimeSpan.getDayByIndex(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(d != null) {
							timeSpan.days.add(d);
						}
					} else if(attributeNode.getNodeName().equalsIgnoreCase("id")) {
						TimeSpan.Day d = TimeSpan.getDayByID(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(d != null) {
							timeSpan.days.add(d);
						}
					}
				}
			} else if(nodeName.equalsIgnoreCase("beginhour")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
						TimeSpan.Hour h = TimeSpan.getHourByIndex(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(h != null) {
							timeSpan.beginHourID = h.hourID;
						}
					} else if(attributeNode.getNodeName().equalsIgnoreCase("id")) {
						TimeSpan.Hour h = TimeSpan.getHourByID(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(h != null) {
							timeSpan.beginHourID = h.hourID;
						}
					}
				}
			} else if(nodeName.equalsIgnoreCase("endhour")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
						TimeSpan.Hour h = TimeSpan.getHourByIndex(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(h != null) {
							timeSpan.endHourID = h.hourID;
						}
					} else if(attributeNode.getNodeName().equalsIgnoreCase("id")) {
						TimeSpan.Hour h = TimeSpan.getHourByID(Integer.valueOf(attributeNode.getNodeValue()).intValue());
						if(h != null) {
							timeSpan.endHourID = h.hourID;
						}
					}
				}
			} else if(nodeName.equalsIgnoreCase("aggregateBy")) {
				NamedNodeMap attributes = child.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
						timeSpan.aggregateBy =
								OutputTimeStep.getByDescription(attributeNode.getNodeValue());
					}
				}
			}
		}
		runSpec.timeSpanSectionStatus = -1;
		runSpec.timeSpan = timeSpan;
	}

	/**
	 * Handles the OnRoadVehicleSelections node.
	 * @param node The Node object to handle.
	**/
	void processOnRoadVehicleSelections(Node node) {
		// This node can have 0 - n child nodes
		if ( !node.hasChildNodes()) { // 0 child nodes
			return;
		}
        
        // track source type and fuel type selections to ensure no fuel type combinations are missing
        TreeMap<Integer, TreeSet<Integer>> stftSelections = new TreeMap<Integer, TreeSet<Integer>>();

		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("onroadvehicleselection")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid OnRoadVehicleSelection");
			} else {
				NamedNodeMap attributes = subNode.getAttributes();
				OnRoadVehicleSelection parsedOnRoadVehicleSelection = new OnRoadVehicleSelection();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("fueltypeid")) {
						parsedOnRoadVehicleSelection.fuelTypeID =
								safeParseInt(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("fueltypedesc")) {
						parsedOnRoadVehicleSelection.fuelTypeDesc = attributeNode.getNodeValue();
					} else if(attributeNode.getNodeName().equalsIgnoreCase("sourcetypeid")) {
						parsedOnRoadVehicleSelection.sourceTypeID =
								safeParseInt(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("sourcetypename")) {
						parsedOnRoadVehicleSelection.sourceTypeName = attributeNode.getNodeValue();
					}
				}
				
				// transparently support old runspecs by converting "Intercity Bus" to "Other Buses" on load
				if (parsedOnRoadVehicleSelection.sourceTypeName.equalsIgnoreCase("Intercity Bus")) {
					parsedOnRoadVehicleSelection.sourceTypeName = "Other Buses";
					runSpec.hadIntercityBuses = true;
				}

                // only add if selection is valid
				if(parsedOnRoadVehicleSelection.isValid()) {
					runSpec.onRoadVehicleSelections.add(parsedOnRoadVehicleSelection);

                    // also, track source type and fuel type selections
                    Integer sourceTypeID = Integer.valueOf(parsedOnRoadVehicleSelection.sourceTypeID);
                    Integer fuelTypeID = Integer.valueOf(parsedOnRoadVehicleSelection.fuelTypeID);
                    TreeSet<Integer> newFuelTypesSet = new TreeSet<Integer>();
                    newFuelTypesSet.add(fuelTypeID);
                    TreeSet<Integer> existingFuelTypesSet = stftSelections.putIfAbsent(sourceTypeID, newFuelTypesSet);
                    if (existingFuelTypesSet != null) {
                        existingFuelTypesSet.add(fuelTypeID);
                        stftSelections.put(sourceTypeID, existingFuelTypesSet);
                    }
				} else {
					/** @explain The RunSpec file is corrupt, likely due to a typo. **/
					Logger.log(LogMessageCategory.WARNING, "Invalid OnRoadVehicleSelection");
				}
			}
		}

        // load valid source type / fuel type combinations (query is from loadValidFuelSourceCombinations() of OnRoadVehicleEquipment.java)
        TreeMap<Integer, ArrayList<OnRoadVehicleSelection>> validSelectionsByST = new TreeMap<Integer, ArrayList<OnRoadVehicleSelection>>();
		String sql = "SELECT DISTINCT ft.fuelTypeID, ft.fuelTypeDesc, sut.sourceTypeID, "
                    +"sut.sourceTypeName FROM FuelType ft, SourceUseType sut, FuelEngTechAssoc "
                    +"feta WHERE ft.fuelTypeID = feta.fuelTypeID AND sut.sourceTypeID = "
                    +"feta.sourceTypeID ORDER BY ft.fuelTypeDesc, sut.sourceTypeName";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
            Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
			query.open(db,sql);
			while(query.rs.next()) {
                OnRoadVehicleSelection vehicle = new OnRoadVehicleSelection();
				vehicle.fuelTypeID = Integer.valueOf(query.rs.getInt(1));
				vehicle.fuelTypeDesc = query.rs.getString(2);
				vehicle.sourceTypeID = Integer.valueOf(query.rs.getInt(3));
				vehicle.sourceTypeName = query.rs.getString(4);
                ArrayList<OnRoadVehicleSelection> newEntry = new ArrayList<OnRoadVehicleSelection>();
                newEntry.add(vehicle);
                ArrayList<OnRoadVehicleSelection> existingEntry = validSelectionsByST.putIfAbsent(vehicle.sourceTypeID, newEntry);
                if (existingEntry != null) {
                    existingEntry.add(vehicle);
                    validSelectionsByST.put(vehicle.sourceTypeID, existingEntry);
                }
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to load the list of source type and fuel type combinations.");
		} finally {
			query.onFinally();
		}

        // ensure all fuel types are selected for each source type. If something is missing, add it
        for (Map.Entry<Integer, TreeSet<Integer>> entry : stftSelections.entrySet()) {
            Integer sourceTypeID = entry.getKey();
            TreeSet<Integer> runspecFuels = entry.getValue();
            for (OnRoadVehicleSelection selection : validSelectionsByST.get(sourceTypeID)) {
                boolean foundFuel = false;
                for (Integer runspecFuel : runspecFuels) {
                    if (runspecFuel == Integer.valueOf(selection.fuelTypeID)) {
                        foundFuel = true;
                        break;
                    }
                }
                if (!foundFuel) {
                    runSpec.onRoadVehicleSelections.add(selection);
                    Logger.skipHandlers = true;
                    Logger.log(LogMessageCategory.WARNING, "Added missing vehicle selection: " + 
                                                            selection.sourceTypeName + " - " + selection.fuelTypeDesc);
                    Logger.skipHandlers = false;
                }
            }
        }
	}

	/**
	 * Handles the OffRoadVehicleSelections node.
	 * @param node The Node object to handle.
	**/
	void processOffRoadVehicleSelections(Node node) {
		// This node can have 0 - n child nodes
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("offroadvehicleselection")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid OffRoadVehicleSelection");
			} else {
				NamedNodeMap attributes = subNode.getAttributes();
				OffRoadVehicleSelection parsedOffRoadVehicleSelection =
						new OffRoadVehicleSelection();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("fueltypeid")) {
						parsedOffRoadVehicleSelection.fuelTypeID =
								safeParseInt(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("fueltypedesc")) {
						parsedOffRoadVehicleSelection.fuelTypeDesc = attributeNode.getNodeValue();
					} else if(attributeNode.getNodeName().equalsIgnoreCase("sectorid")) {
						parsedOffRoadVehicleSelection.sectorID =
								safeParseInt(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("sectorname")) {
						parsedOffRoadVehicleSelection.sectorName = attributeNode.getNodeValue();
					}
				}
				if(parsedOffRoadVehicleSelection.isValid()) {
					runSpec.offRoadVehicleSelections.add(parsedOffRoadVehicleSelection);
				} else {
					/** @explain The RunSpec file is corrupt, likely due to a typo. **/
					Logger.log(LogMessageCategory.WARNING, "Invalid OffRoadVehicleSelection");
				}
			}
		}
	}

	/**
	 * Handles the OffRoadVehicleSCCs node.
	 * @param node The Node object to handle.
	**/
	void processOffRoadVehicleSCCs(Node node) {
		// This node can have 0 - n child nodes
		if ( !node.hasChildNodes()) { // 0 child nodes
			return;
		}
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("scc")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid OffRoadVehicleSCC");
			} else {
				NamedNodeMap attributes = subNode.getAttributes();
				String parsedSCCCode = null;
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("code")) {
						parsedSCCCode = attributeNode.getNodeValue();
					}
				}
				if(parsedSCCCode != null && parsedSCCCode.length() > 0) {
					SCC parsedSCC = new SCC();
					parsedSCC.scc = parsedSCCCode;
					runSpec.offRoadVehicleSCCs.add(parsedSCC);
				} else {
					/** @explain The RunSpec file is corrupt, likely due to a typo. **/
					Logger.log(LogMessageCategory.WARNING, "Invalid OffRoadVehicleSCC");
				}
			}
		}
	}

	/**
	 * Handles the RoadTypes node.
	 * @param node The Node object to handle.
	**/
	void processRoadTypes(Node node) {
		// This node can have 0 - n child nodes
		if ( !node.hasChildNodes()) { // 0 child nodes
			return;
		}
		runSpec.hasDeprecatedShouldSeparateRampsTrue = getBooleanAttribute(node,"separateramps");
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("roadtype")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid RoadType");
			} else {
				NamedNodeMap attributes = subNode.getAttributes();
				RoadType parsedRoadType = null;
				int roadTypeID = 0;
				String roadTypeName = "";
				String modelCombination = "";
				Models.ModelCombination mc = Models.ModelCombination.M1;
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("roadtypeid")) {
						roadTypeID = safeParseInt(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("roadtypename")) {
						roadTypeName = attributeNode.getNodeValue();
					} else if (attributeNode.getNodeName().equalsIgnoreCase("modelCombination")) {
						modelCombination = attributeNode.getNodeValue();
						if (modelCombination.equals("M1")) {
							mc = Models.ModelCombination.M1;
						} else if (modelCombination.equals("M2")) {
							mc = Models.ModelCombination.M2;
						} else if (modelCombination.equals("M12")) {
							mc = Models.ModelCombination.M12;
						} else {
							mc = Models.ModelCombination.M0;
						} 
					}
				}
				if(roadTypeID > 0) {
					if(RoadType.isInDatabase(roadTypeID,roadTypeName)) {
						parsedRoadType = new RoadType(roadTypeID,roadTypeName, mc);
						runSpec.roadTypes.add(parsedRoadType);
					} else {
						/** @explain The RunSpec file is corrupt, likely due to a typo. **/
						Logger.log(LogMessageCategory.WARNING, "Road type ID, " + roadTypeID
								+ ", with name, '" + roadTypeName
								+ "', was not found in the default database.");
					}
				} else {
					/** @explain The RunSpec file is corrupt, likely due to a typo. **/
					Logger.log(LogMessageCategory.WARNING, "Invalid RoadType");
				}
			}
		}
	}

	/**
	 * Handles the PollutantProcessAssociations node.
	 * @param node The Node object to handle.
	**/
	void processPollutantProcessAssociations(Node node) {
		// This node can have 0 - n child nodes
		if ( !node.hasChildNodes()) { // 0 child nodes
			return;
		}
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("pollutantprocessassociation")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid PollutantProcessAssociation");
			} else {
				NamedNodeMap attributes = subNode.getAttributes();
				String parsedPollutantName = "";
				String parsedProcessName = "";
				int parsedPollutantKey = 0;
				int parsedProcessKey = 0;
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("pollutantname")) {
						parsedPollutantName = attributeNode.getNodeValue();
					} else if(attributeNode.getNodeName().equalsIgnoreCase("processname")) {
						parsedProcessName = attributeNode.getNodeValue();
					} else if(attributeNode.getNodeName().equalsIgnoreCase("pollutantkey")) {
						parsedPollutantKey = safeParseInt(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("processkey")) {
						parsedProcessKey = safeParseInt(attributeNode.getNodeValue());
					}
				}
				if(parsedPollutantName.length() > 0 && parsedPollutantKey != 0 &&
							parsedProcessName.length() > 0 && parsedProcessKey != 0) {
					PollutantProcessAssociation parsedPollutantProcessAssociation =
						PollutantProcessAssociation.createByID(parsedPollutantKey,parsedProcessKey);
					if(parsedPollutantProcessAssociation == null) {
						/** @explain The RunSpec file is corrupt, likely due to a typo. **/
						Logger.log(LogMessageCategory.WARNING,
								"Invalid PollutantProcessAssociation");
					} else {
						runSpec.pollutantProcessAssociations.add(parsedPollutantProcessAssociation);
					}
				} else {
					/** @explain The RunSpec file is corrupt, likely due to a typo. **/
					Logger.log(LogMessageCategory.WARNING, "Invalid PollutantProcessAssociation");
				}
			}
		}
	}

	/**
	 * Handles the DatabaseSelections node.
	 * @param node The Node object to handle.
	**/
	void processDatabaseSelections(Node node) {
		// This node can have 0 - n child nodes
		if ( !node.hasChildNodes()) { // 0 child nodes
			return;
		}
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("databaseselection")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid DatabaseSelection");
			} else {
				NamedNodeMap attributes = subNode.getAttributes();
				String parsedServerName = null;
				String parsedDatabaseName = null;
				String parsedDescription = null;
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("servername")) {
						parsedServerName = new String(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("databasename")) {
						parsedDatabaseName = new String(attributeNode.getNodeValue());
					} else if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
						parsedDescription = new String(attributeNode.getNodeValue());
					}
				}
				if(parsedServerName != null && parsedDatabaseName != null) {
					DatabaseSelection parsedDatabaseSelection = new DatabaseSelection();
					parsedDatabaseSelection.serverName = parsedServerName;
					parsedDatabaseSelection.databaseName = parsedDatabaseName;
					parsedDatabaseSelection.description =
							StringUtilities.safeGetString(parsedDescription);
					// Currently, these settings are hardcoded
					parsedDatabaseSelection.userName =
							DatabaseSelection.SERVER_USER_NAME;
					parsedDatabaseSelection.password =
							DatabaseSelection.SERVER_PASSWORD;
					runSpec.databaseSelectionInputSets.add(parsedDatabaseSelection);
				} else {
					/** @explain The RunSpec file is corrupt, likely due to a typo. **/
					Logger.log(LogMessageCategory.WARNING, "Invalid DatabaseSelection");
				}
			}
		}
	}

	/**
	 * Handles the InternalControlStrategies node.
	 * @param node The Node object to handle.
	**/
	void processInternalControlStrategies(Node node) {
		// This node can have 0 - n child nodes
		if ( !node.hasChildNodes()) { // 0 child nodes
			return;
		}
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(!subNode.getNodeName().equalsIgnoreCase("internalcontrolstrategy")) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid InternalControlStrategy");
			} else {
				InternalControlStrategy strategy = processInternalControlStrategy(subNode,null,null);
				if(strategy == null) {
					continue;
				}
				String className = strategy.getClass().getName();
				LinkedList<InternalControlStrategy> instances =
						runSpec.internalControlStrategies.get(className);
				if(instances == null) {
					instances = new LinkedList<InternalControlStrategy>();
					runSpec.internalControlStrategies.put(className,instances);
				}
				instances.add(strategy);
			}
		}
	}

	/**
	 * Handles an InternalControlStrategy node.
	 * @param node The Node object to handle, may be null
	 * @param defaultClassName default class name if one is not specified in the XML
	 * @param defaultText optional text to be read, may be null
	 * @return a non-null InternalControlStrategy object upon success, null upon failure
	**/
	InternalControlStrategy processInternalControlStrategy(Node node,String defaultClassName,
			String defaultText) {
		String className = null;
		if(node != null) {
			NamedNodeMap attributes = node.getAttributes();
			for(int i = 0; i < attributes.getLength(); i++) {
				Node attributeNode = attributes.item(i);
				if(attributeNode.getNodeName().equalsIgnoreCase("classname")) {
					className = new String(attributeNode.getNodeValue());
				}
			}
		}
		if(className == null && defaultClassName != null && defaultClassName.length() > 0) {
			className = defaultClassName;
		}
		if(className != null) {
			try {
				Class<?> c = Class.forName(className);
				InternalControlStrategy strategy = (InternalControlStrategy) c.getConstructor().newInstance();
				// Try loading from XML first
				if(strategy.acceptXML(className,node)) {
					//Logger.log(LogMessageCategory.INFO,"loaded via XML");
					return strategy;
				}
				// Try loading from the text within the node
				String text = defaultText;
				//Logger.log(LogMessageCategory.INFO,"defaultText=" + (defaultText==null?"null":"not null"));
				if(node != null) {
					Node childNode = node.getFirstChild();
					for(Node child = node.getFirstChild(); child != null;
							child = child.getNextSibling()) {
						if(child.getNodeType() == Node.TEXT_NODE
								|| child.getNodeType() == Node.CDATA_SECTION_NODE) {
							text = StringUtilities.safeGetString(childNode.getNodeValue());
							break;
						}
					}
					if(text != null) {
						text = text.trim();
						//Logger.log(LogMessageCategory.INFO,"node text="
						//		+ text.substring(0,Math.min(250,text.length())));
					} else {
						//Logger.log(LogMessageCategory.INFO,"node text=null");
					}
				}
				if(text != null) {
					text = text.trim();
					//Logger.log(LogMessageCategory.INFO,"text="
					//		+ text.substring(0,Math.min(250,text.length())));
					if(strategy.acceptTSV(className,text)) {
						//Logger.log(LogMessageCategory.INFO,"loaded via text");
						return strategy;
					}
				} else {
					//Logger.log(LogMessageCategory.INFO,"InternalControlStrategy text was null");
				}
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING,
						"InternalControlStrategy failed to load");
				return null;
			} catch(Exception e) {
				/** @explain The RunSpec file is corrupt, likely due to a typo. **/
				Logger.log(LogMessageCategory.WARNING, "Invalid InternalControlStrategy");
			}
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid InternalControlStrategy");
		}
		return null;
	}

	/**
	 * Handles the InputDatabase node.
	 * @param node The Node object to handle.
	**/
	void processInputDatabase(Node node) {
		// This node should have two attributes and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		String parsedServerName = null;
		String parsedDatabaseName = null;
		String parsedDescription = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("servername")) {
				parsedServerName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("databasename")) {
				parsedDatabaseName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
				parsedDescription = new String(attributeNode.getNodeValue());
			}
		}
		if(parsedServerName != null && parsedDatabaseName != null) {
			runSpec.inputDatabase = new DatabaseSelection();
			runSpec.inputDatabase.serverName = parsedServerName;
			runSpec.inputDatabase.databaseName = parsedDatabaseName;
			runSpec.inputDatabase.description = StringUtilities.safeGetString(parsedDescription);
			runSpec.inputDatabase.userName = "";
			runSpec.inputDatabase.password = "";
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid InputDatabase");
		}
	}

	/**
	 * Handles the UncertaintyParameters node.
	 * @param node The Node object to handle.
	**/
	void processUncertaintyParameters(Node node) {
		// This node should have three attributes and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		int attributeCount = 0;
		boolean hasUncertaintyModeEnabled = false;
		boolean hasNumberOfRunsPerSimulation = false;
		boolean hasNumberOfSimulations = false;
		boolean uncertaintyModeEnabled = false;
		int numberOfRunsPerSimulation = 0;
		int numberOfSimulations = 0;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("uncertaintymodeenabled")) {
				uncertaintyModeEnabled = safeParseBoolean(attributeNode.getNodeValue());
				hasUncertaintyModeEnabled = true;
			} else if(attributeNode.getNodeName().equalsIgnoreCase("numberofrunspersimulation")) {
				numberOfRunsPerSimulation = safeParseInt(attributeNode.getNodeValue());
				hasNumberOfRunsPerSimulation = true;
			} else if(attributeNode.getNodeName().equalsIgnoreCase("numberofsimulations")) {
				numberOfSimulations = safeParseInt(attributeNode.getNodeValue());
				hasNumberOfSimulations = true;
			}
		}
		if(hasUncertaintyModeEnabled && hasNumberOfRunsPerSimulation && hasNumberOfSimulations) {
			runSpec.uncertaintyParameters.uncertaintyModeEnabled = uncertaintyModeEnabled;
			runSpec.uncertaintyParameters.numberOfRunsPerSimulation = numberOfRunsPerSimulation;
			runSpec.uncertaintyParameters.numberOfSimulations = numberOfSimulations;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid UncertaintyParameter");
		}
	}

	/**
	 * Handles the GeographicOutputDetail node.
	 * @param node The Node object to handle.
	**/
	void processGeographicOutputDetail(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		GeographicOutputDetailLevel parsedGeographicOutputDetailLevel = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
				parsedGeographicOutputDetailLevel = GeographicOutputDetailLevel.getByName(
						StringUtilities.safeGetString(attributeNode.getNodeValue()));
			}
		}
		if(parsedGeographicOutputDetailLevel != null) {
			runSpec.geographicOutputDetail = parsedGeographicOutputDetailLevel;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid GeographicOutputDetail");
		}
	}

	/**
	 * Handles the OutputEmissionBreakdownsSelection node.
	 * @param node The Node object to handle.
	**/
	void processOutputEmissionsBreakdownSelection(Node node){
		// This node should have 11 subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean[] hasParam = new boolean[14];
		OutputEmissionsBreakdownSelection selection = new OutputEmissionsBreakdownSelection();
		int paramIndex = 0;
		boolean hasEngTech = false;
		boolean hasRegClass = false;
		boolean hasFuelSubType = false;
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(subNode.getNodeName().equalsIgnoreCase("modelyear")) {
				selection.modelYear = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("fueltype")) {
				selection.fuelType = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("fuelsubtype")) {
				selection.fuelSubType = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
				hasFuelSubType = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("emissionprocess")) {
				selection.emissionProcess = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("onroadoffroad")) {
				selection.onRoadOffRoad = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("roadtype")) {
				selection.roadType = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("sourceusetype")) {
				selection.sourceUseType = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("movesvehicletype")) {
				selection.movesVehicleType = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("onroadscc")) {
				selection.onRoadSCC = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("offroadscc")) {
				// Ignore this value. It is only supported for legacy reasons and was
				// never used in the rest of the system.
			} else if(subNode.getNodeName().equalsIgnoreCase("estimateuncertainty")) {
				selection.estimateUncertainty = getBooleanAttribute(subNode, "selected");
				selection.numberOfIterations = getIntegerAttribute(subNode, "numberOfIterations", 2);
				selection.keepSampledData = getBooleanAttribute(subNode, "keepSampledData");
				selection.keepIterations = getBooleanAttribute(subNode, "keepIterations");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("sector") || subNode.getNodeName().equalsIgnoreCase("segment")) {
				selection.sector = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("engtechid")) {
				selection.engTechID = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
				hasEngTech = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("hpclass")) {
				selection.hpClass = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
			} else if(subNode.getNodeName().equalsIgnoreCase("regclassid")) {
				selection.regClassID = getBooleanAttribute(subNode, "selected");
				hasParam[paramIndex++] = true;
				hasRegClass = true;
			}
			if(paramIndex >= hasParam.length) {
				break;
			}
		}
		boolean hasAllParams = true;
		int howManyParamsFound = 0;
		for(int i = 0; i < hasParam.length; i++) {
			if(!hasParam[i]) {
				hasAllParams = false;
			} else {
				howManyParamsFound++;
			}
		}
		// Support old runspecs
		int expectedLength = hasParam.length;
		if(!hasEngTech) {
			expectedLength--;
		}
		if(!hasRegClass) {
			expectedLength--;
		}
		if(!hasFuelSubType) {
			expectedLength--;
		}
		if(howManyParamsFound >= expectedLength) {
			hasAllParams = true;
		}
		// Complain if the RunSpec is not complete
		if(hasAllParams) {
			runSpec.outputEmissionsBreakdownSelection = selection;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid OutputEmissionsBreakdownSelection");
		}
	}

	/**
	 * Handles the OutputFactors node.
	 * @param node The Node object to handle.
	**/
	void processOutputFactors(Node node){
		// This node should have 3 subnodes, each with 2 attributes
		NamedNodeMap attributes = null;
		boolean[] hasParam = new boolean[3];
		OutputFactors selection = new OutputFactors();
		int paramIndex = 0;
		for(Node subNode = node.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("#text")) {
				continue;
			}
			if(subNode.getNodeName().equalsIgnoreCase("timefactors")) {
				int paramCount = 0;
				attributes = subNode.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("selected")) {
						selection.timeFactorsSelected =
								safeParseBoolean(attributeNode.getNodeValue());
						paramCount++;
					} else if(attributeNode.getNodeName().equalsIgnoreCase("units")) {
						selection.timeMeasurementSystem = TimeMeasurementSystem.getByDescription(
								attributeNode.getNodeValue());
						paramCount++;
					}
				}
				if(paramCount == 2) {
					hasParam[paramIndex++] = true;
				}
			} else if(subNode.getNodeName().equalsIgnoreCase("distancefactors")) {
				int paramCount = 0;
				attributes = subNode.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("selected")) {
						selection.distanceFactorsSelected =
								safeParseBoolean(attributeNode.getNodeValue());
						paramCount++;
					} else if(attributeNode.getNodeName().equalsIgnoreCase("units")) {
						selection.distanceMeasurementSystem =
								DistanceMeasurementSystem.getByDescription(
								attributeNode.getNodeValue());
						paramCount++;
					}
				}
				if(paramCount == 2) {
					hasParam[paramIndex++] = true;
				}
			} else if(subNode.getNodeName().equalsIgnoreCase("massfactors")) {
				int paramCount = 0;
				attributes = subNode.getAttributes();
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("selected")) {
						selection.massFactorsSelected =
								safeParseBoolean(attributeNode.getNodeValue());
						paramCount++;
					} else if(attributeNode.getNodeName().equalsIgnoreCase("units")) {
						selection.massMeasurementSystem = MassMeasurementSystem.getByDescription(
								attributeNode.getNodeValue());
						paramCount++;
					} else if(attributeNode.getNodeName().equalsIgnoreCase("energyunits")) {
						selection.energyMeasurementSystem =
								EnergyMeasurementSystem.getByDescription(
								attributeNode.getNodeValue());
						paramCount++;
					}
				}
				if(paramCount == 3) {
					hasParam[paramIndex++] = true;
				}
			}
			if(paramIndex >= 3) {
				break;
			}
		}
		boolean hasAllParams = true;
		for(int i = 0; i < hasParam.length; i++) {
			if(!hasParam[i]) {
				hasAllParams = false;
				break;
			}
		}
		if(hasAllParams) {
			runSpec.outputFactors = selection;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid OutputFactors");
		}
	}

	/**
	 * Parses a boolean attribute from the specified Node and name.
	 * @param node The Node object to get the attributes from.
	 * @param attributeName String indicating the boolean attribute's name.
	 * @return The parsed value, or false if the node doesn't have the specified attribute.
	**/
	boolean getBooleanAttribute(Node node, String attributeName) {
		return getBooleanAttribute(node,attributeName,false);
	}

	/**
	 * Parses a boolean attribute from the specified Node and name.
	 * @param node The Node object to get the attributes from.
	 * @param attributeName String indicating the boolean attribute's name.
	 * @param defaultValue value to use when the node lacks the attribute.
	 * @return The parsed value, or defaultValue if the node doesn't have the specified attribute.
	**/
	boolean getBooleanAttribute(Node node, String attributeName, boolean defaultValue) {
		boolean result = defaultValue;
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase(attributeName)) {
				result = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		return result;
	}

	/**
	 * Parses an integer attribute from the specified Node and name.
	 * @param node The Node object to get the attributes from.
	 * @param attributeName String indicating the boolean attribute's name.
	 * @return The parsed value, or false if the node doesn't have the specified attribute.
	**/
	int getIntegerAttribute(Node node, String attributeName, int defaultValue) {
		int result = defaultValue;
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase(attributeName)) {
				result = safeParseInt(attributeNode.getNodeValue());
			}
		}
		return result;
	}

	/**
	 * Handles the OutputDatabase node.
	 * @param node The Node object to handle.
	**/
	void processOutputDatabase(Node node) {
		// This node should have two attributes and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		String parsedServerName = null;
		String parsedDatabaseName = null;
		String parsedDescription = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("servername")) {
				parsedServerName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("databasename")) {
				parsedDatabaseName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
				parsedDescription = new String(attributeNode.getNodeValue());
			}
		}
		if(parsedServerName != null && parsedDatabaseName != null) {
			runSpec.outputDatabase = new DatabaseSelection();
			runSpec.outputDatabase.serverName = parsedServerName;
			runSpec.outputDatabase.databaseName = parsedDatabaseName;
			runSpec.outputDatabase.description = StringUtilities.safeGetString(parsedDescription);
			runSpec.outputDatabase.userName = "";
			runSpec.outputDatabase.password = "";
			DatabaseConnectionManager.setOutputDatabase(runSpec.outputDatabase.serverName,
				 	runSpec.outputDatabase.databaseName);
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid OutputDatabase");
		}
	}

	/**
	 * Handles the GeneratorDatabase node.
	 * @param node The Node object to handle.
	**/
	void processGeneratorDatabase(Node node) {
		// This node should have two attributes and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		String parsedServerName = null;
		String parsedDatabaseName = null;
		String parsedDescription = null;
		runSpec.shouldCopySavedGeneratorData = this.getBooleanAttribute(node,"shouldsave");
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("servername")) {
				parsedServerName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("databasename")) {
				parsedDatabaseName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
				parsedDescription = new String(attributeNode.getNodeValue());
			}
		}
		if(parsedServerName != null && parsedDatabaseName != null) {
			runSpec.generatorDatabase = new DatabaseSelection();
			runSpec.generatorDatabase.serverName = parsedServerName;
			runSpec.generatorDatabase.databaseName = parsedDatabaseName;
			runSpec.generatorDatabase.description = StringUtilities.safeGetString(parsedDescription);
			runSpec.generatorDatabase.userName = "";
			runSpec.generatorDatabase.password = "";
		}
	}

	/**
	 * Handles the lookuptableflags node.
	 * @param node The Node object to handle
	**/
	void processLookupTableFlags(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		runSpec.shouldTruncateMOVESOutput = this.getBooleanAttribute(node,"truncateoutput",true);
		runSpec.shouldTruncateMOVESActivityOutput = this.getBooleanAttribute(node,"truncateactivity",true);
		runSpec.shouldTruncateBaseRateOutput = this.getBooleanAttribute(node,"truncatebaserates",true);
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("scenarioid")) {
				runSpec.scenarioID = new String(attributeNode.getNodeValue());
			}
		}
	}

	/**
	 * Handles the OutputTimeStep node.
	 * @param node The Node object to handle.
	**/
	void processOutputTimeStep(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		String parsedValue = null;
		boolean hasValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = new String(attributeNode.getNodeValue());
				hasValue = true;
			}
		}
		if(hasValue == true) {
			runSpec.outputTimeStep = OutputTimeStep.getByDescription(parsedValue);
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid OutputTimeStep");
		}
	}

	/**
	 * Handles the OutputVMTData node.
	 * @param node The Node object to handle.
	**/
	void processOutputVMTData(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean parsedValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		runSpec.outputVMTData = parsedValue;
	}

	/**
	 * Handles the OutputSHO node.
	 * @param node The Node object to handle.
	**/
	void processOutputSHO(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean parsedValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		runSpec.outputSHO = parsedValue;
	}

	/**
	 * Handles the OutputSH node.
	 * @param node The Node object to handle.
	**/
	void processOutputSH(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean parsedValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		runSpec.outputSH = parsedValue;
	}

	/**
	 * Handles the OutputSHP node.
	 * @param node The Node object to handle.
	**/
	void processOutputSHP(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean parsedValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		runSpec.outputSHP = parsedValue;
	}

	/**
	 * Handles the OutputSHIdling node.
	 * @param node The Node object to handle.
	**/
	void processOutputSHIdling(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean parsedValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		runSpec.outputSHIdling = parsedValue;
	}

	/**
	 * Handles the OutputStarts node.
	 * @param node The Node object to handle.
	**/
	void processOutputStarts(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean parsedValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		runSpec.outputStarts = parsedValue;
	}

	/**
	 * Handles the OutputPopulation node.
	 * @param node The Node object to handle.
	**/
	void processOutputPopulation(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		boolean parsedValue = false;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = safeParseBoolean(attributeNode.getNodeValue());
			}
		}
		runSpec.outputPopulation = parsedValue;
	}

	/**
	 * Handles the HydrocarbonUnitSystem node.
	 * @param node The Node object to handle.
	**/
	void processHydrocarbonUnitSystem(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		String parsedValue = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedValue = new String(attributeNode.getNodeValue());
			}
		}
		HydrocarbonUnitSystem parsedHydrocarbonUnitSystem = null;
		if(parsedValue != null && parsedValue.length() > 0) {
			parsedHydrocarbonUnitSystem = HydrocarbonUnitSystem.getByName(parsedValue);
		}
		if(parsedHydrocarbonUnitSystem != null) {
//			runSpec.hydrocarbonUnitSystem = parsedHydrocarbonUnitSystem;
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid HydrocarbonUnitSystem");
		}
	}

	/**
	 * Handles the ScaleInputDatabase node.
	 * @param node The Node object to handle.
	**/
	void processScaleInputDatabase(Node node) {
		// This node should have two attributes and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		String parsedServerName = null;
		String parsedDatabaseName = null;
		String parsedDescription = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("servername")) {
				parsedServerName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("databasename")) {
				parsedDatabaseName = new String(attributeNode.getNodeValue());
			} else if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
				parsedDescription = new String(attributeNode.getNodeValue());
			}
		}
		if(parsedServerName != null && parsedDatabaseName != null) {
			runSpec.scaleInputDatabase = new DatabaseSelection();
			runSpec.scaleInputDatabase.serverName = parsedServerName;
			runSpec.scaleInputDatabase.databaseName = parsedDatabaseName;
			runSpec.scaleInputDatabase.description =
					StringUtilities.safeGetString(parsedDescription);
			runSpec.scaleInputDatabase.userName = "";
			runSpec.scaleInputDatabase.password = "";
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid ScaleInputDatabase");
		}
	}

	/**
	 * Handles the PMSize node.
	 * @param node The Node object to handle.
	**/
	void processPMSize(Node node){
		// This node should have one attribute and no subnodes
		NamedNodeMap attributes = node.getAttributes();
		Integer parsedPMSize = null;
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("value")) {
				parsedPMSize = Integer.valueOf(safeParseInt(attributeNode.getNodeValue()));
			}
		}
		if(parsedPMSize != null) {
			runSpec.pmSize = parsedPMSize.intValue();
		} else {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.log(LogMessageCategory.WARNING, "Invalid PMSize");
		}
	}

	/**
	 * Saves from the RunSpec to the XML file.
	 * @param outFile The XML file to write to.
	**/
	public void save(File outFile) {
		// This operation overwrites any existing file.
		if(outFile.isFile()) {
			outFile.delete();
		}
		try {
			PrintWriter printWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(outFile))));
			save(printWriter,false);
			printWriter.close();
		} catch(Exception e) {
			/**
			 * @explain The RunSpec could not be saved.  This often happens if the file is
			 * already open in a text editor.
			**/
			Logger.logError(e,"Unable to save XML.");
		}
	}

	/**
	 * Saves from the RunSpec to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	 * @param onlyWriteFilters true if only sections that represent data filters
	 * should be written.  When true, there are no top level nodes written.
	**/
	public void save(PrintWriter printWriter, boolean onlyWriteFilters) {
		try {
			if(!onlyWriteFilters) {
				printWriter.println("<runspec version=\""
						+ StringUtilities.safeGetString(gov.epa.otaq.moves.master.gui.MOVESWindow.MOVES_VERSION) + "\">");
				printWriter.println("\t<description><![CDATA["
						+ StringUtilities.safeGetString(runSpec.description) + "]]></description>");
				printWriter.println("\t<models>");
				for (Model model : runSpec.models.getModelList()) {
					printWriter.println("\t\t<model value=\""
							+ StringUtilities.safeGetString(model) + "\"/>");
				}
				printWriter.println("\t</models>");
				printWriter.println("\t<modelscale value=\""
						+ StringUtilities.safeGetString(runSpec.scale) + "\"/>");
				printWriter.println("\t<modeldomain value=\""
						+ StringUtilities.safeGetString(runSpec.domain) + "\"/>");
			}

			printGenericCounty(printWriter);
			printGeographicSelections(printWriter);
			printTimeSpan(printWriter);
			printOnRoadVehicleSelections(printWriter);
			printOffRoadVehicleSelections(printWriter);
			printSCCs(printWriter);
			printRoadTypes(printWriter);
			printPollutantProcessAssocations(printWriter);

			if(onlyWriteFilters) {
				return;
			}

			printDatabaseSelections(printWriter);
			printInternalControlStrategies(printWriter);
			String serverName = new String("");
			String databaseName = new String("");
			String databaseDescription = new String("");
			if(runSpec.inputDatabase != null) {
				serverName = StringUtilities.safeGetString(runSpec.inputDatabase.serverName);
				databaseName = StringUtilities.safeGetString(runSpec.inputDatabase.databaseName);
				databaseDescription =
						StringUtilities.safeGetEscapedString(runSpec.inputDatabase.description);
			}
			printWriter.println("\t<inputdatabase"
					+ " servername=\"" + serverName + "\""
					+ " databasename=\"" + databaseName + "\""
					+ " description=\"" + databaseDescription + "\"/>");
			printWriter.println("\t<uncertaintyparameters"
					+ " uncertaintymodeenabled=\""
					+ runSpec.uncertaintyParameters.uncertaintyModeEnabled + "\""
					+ " numberofrunspersimulation=\""
					+ runSpec.uncertaintyParameters.numberOfRunsPerSimulation + "\""
					+ " numberofsimulations=\""
					+ runSpec.uncertaintyParameters.numberOfSimulations + "\"/>");
			printWriter.println("\t<geographicoutputdetail"
					+ " description=\""
					+ StringUtilities.safeGetString(runSpec.geographicOutputDetail)
					+ "\"/>");
			printOutputEmissionsBreakdownSelections(printWriter);
			serverName = "";
			databaseName = "";
			databaseDescription = "";
			if(runSpec.outputDatabase != null) {
				serverName = StringUtilities.safeGetString(runSpec.outputDatabase.serverName);
				databaseName = StringUtilities.safeGetString(runSpec.outputDatabase.databaseName);
				databaseDescription = StringUtilities.safeGetEscapedString(runSpec.outputDatabase.description);
			}
			printWriter.println("\t<outputdatabase"
					+ " servername=\"" + serverName + "\""
					+ " databasename=\"" + databaseName + "\""
					+ " description=\"" + databaseDescription + "\"/>");
			printWriter.println("\t<outputtimestep value=\""
					+ StringUtilities.safeGetString(runSpec.outputTimeStep) + "\"/>");
			printWriter.println("\t<outputvmtdata value=\"" + runSpec.outputVMTData + "\"/>");
			printWriter.println("\t<outputsho value=\"" + runSpec.outputSHO + "\"/>");
			printWriter.println("\t<outputsh value=\"" + runSpec.outputSH + "\"/>");
			printWriter.println("\t<outputshp value=\"" + runSpec.outputSHP + "\"/>");
			printWriter.println("\t<outputshidling value=\"" + runSpec.outputSHIdling + "\"/>");
			printWriter.println("\t<outputstarts value=\"" + runSpec.outputStarts + "\"/>");
			printWriter.println("\t<outputpopulation value=\"" + runSpec.outputPopulation + "\"/>");
//			printWriter.println("\t<hydrocarbonunitsystem value=\""
//					+ StringUtilities.safeGetString(runSpec.hydrocarbonUnitSystem) + "\"/>");
			serverName = "";
			databaseName = "";
			databaseDescription = "";
			if(runSpec.scaleInputDatabase != null /*&& runSpec.domain != ModelDomain.NATIONAL_ALLOCATION*/) {
				serverName = StringUtilities.safeGetString(runSpec.scaleInputDatabase.serverName);
				databaseName = StringUtilities.safeGetString(runSpec.scaleInputDatabase.databaseName);
				databaseDescription = StringUtilities.safeGetEscapedString(runSpec.scaleInputDatabase.description);
			}
			printWriter.println("\t<scaleinputdatabase"
					+ " servername=\"" + serverName + "\""
					+ " databasename=\"" + databaseName + "\""
					+ " description=\"" + databaseDescription + "\"/>");
			printWriter.println("\t<pmsize value=\"" + runSpec.pmSize + "\"/>");
			printOutputFactors(printWriter);

			printWriter.println("\t<savedata>\n");
			printClasses(printWriter,runSpec.classesToSaveData);
			printWriter.println("\t</savedata>\n");

			printWriter.println("\t<donotexecute>\n");
			printClasses(printWriter,runSpec.classesNotToExecute);
			printWriter.println("\t</donotexecute>\n");

			serverName = "";
			databaseName = "";
			databaseDescription = "";
			if(runSpec.generatorDatabase != null) {
				serverName = StringUtilities.safeGetString(runSpec.generatorDatabase.serverName);
				databaseName = StringUtilities.safeGetString(runSpec.generatorDatabase.databaseName);
				databaseDescription =
						StringUtilities.safeGetEscapedString(runSpec.generatorDatabase.description);
			}
			printWriter.println("\t<generatordatabase"
					+ " shouldsave=\"" + runSpec.shouldCopySavedGeneratorData + "\""
					+ " servername=\"" + serverName + "\""
					+ " databasename=\"" + databaseName + "\""
					+ " description=\"" + databaseDescription + "\"/>");
			printWriter.println("\t<donotperformfinalaggregation selected=\""
					+ runSpec.doNotPerformFinalAggregation + "\"/>");

			printWriter.println("\t<lookuptableflags"
					+ " scenarioid=\"" + StringUtilities.safeGetString(runSpec.scenarioID) + "\""
					+ " truncateoutput=\"" + runSpec.shouldTruncateMOVESOutput + "\""
					+ " truncateactivity=\"" + runSpec.shouldTruncateMOVESActivityOutput + "\""
					+ " truncatebaserates=\"" + runSpec.shouldTruncateBaseRateOutput + "\""
					+ "/>");
            
            printWriter.println("\t<skipdomaindatabasevalidation selected=\""
                            + runSpec.skipDomainDatabaseValidation + "\"/>");

			printWriter.println("</runspec>");
		} catch(Exception e) {
			/** @nonissue **/
			Logger.logError(e,"Unable to save XML.");
		}
	}

	/**
	 * Internal class used to handle errors.
	**/
	public static class XMLErrorHandler implements ErrorHandler {
		/**
		 * Returns a string describing parse exception details
		 * @param spe The SAX Parse Exception that occurred.
		**/
		private String getParseExceptionInfo(SAXParseException spe) {
			String systemId = spe.getSystemId();
			if (systemId == null) {
				systemId = "null";
			}
			String info = "URI=" + systemId +
				" Line=" + spe.getLineNumber() +
				": " + spe.getMessage();
			return info;
		}

		/**
		 * Log SAX parser warning.
		 * @param spe The SAX Parse Exception that occurred.
		**/
		public void warning(SAXParseException spe) throws SAXException {
			/**
			 * @issue Warning: [SAX XML parser exception]
			 * @explain The RunSpec file is corrupt, likely due to a typo.
			**/
			Logger.log(LogMessageCategory.WARNING, "Warning: " + getParseExceptionInfo(spe));
		}

		/**
		 * Log SAX parser error.
		 * @param spe The SAX Parse Exception that occurred.
		**/
		public void error(SAXParseException spe) throws SAXException {
			String message = "Error: " + getParseExceptionInfo(spe);
			throw new SAXException(message);
		}

		/**
		 * Log SAX parser fatal error.
		 * @param spe The SAX Parse Exception that occurred.
		**/
		public void fatalError(SAXParseException spe) throws SAXException {
			String message = "Fatal Error: " + getParseExceptionInfo(spe);
			throw new SAXException(message);
		}
	}

	/**
	 * Helper function to convert a String to an int, returns 0 if it cannot be converted.
	 * @param inString The String to convert.
	 * @return The converted value, or zero, if the String cannot be converted.
	**/
	int safeParseInt(String inString) {
		int result = 0;
		try {
			result = Integer.parseInt(inString);
		} catch(NumberFormatException e) {
		}
		return result;
	}

	/**
	 * Helper function to convert a String to a boolean, returns false if it cannot be converted.
	 * @param inString The String to convert.
	 * @return The converted value, or false, if the String cannot be converted.
	**/
	boolean safeParseBoolean(String inString) {
		try {
			Boolean testBoolean = Boolean.valueOf(inString);
			return testBoolean.booleanValue();
		} catch(Exception e) {
		}
		return false;
	}

	/**
	 * Gets a "valid" version of a char.  If the char is a control char, then ASCII 0x20
	 * is returned, otherwise the original char is returned.
	 * @param c The char to check.
	 * @return The original char if valid, otherwise the space char is returned.
	**/
	char getValidChar(char c) {
		String result = String.valueOf(c);
		result = result.trim();
		if(result.length() == 1) {
			return result.charAt(0);
		}
		return ' ';
	}

	/**
	 * Writes the GenericCounty section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printGenericCounty(PrintWriter printWriter) {
		if(runSpec.genericCounty == null) {
			return;
		}
		printWriter.println("\t<genericcounty>");
		printWriter.println("\t\t<shortid value=\""
				+runSpec.genericCounty.shortCountyID+"\"/>");
		printWriter.println("\t\t<stateid value=\""
				+runSpec.genericCounty.stateID+"\"/>");
		printWriter.println("\t\t<description value=\""
				+StringUtilities.safeGetEscapedString(runSpec.genericCounty.description)+"\"/>");
		printWriter.println("\t\t<gpafraction value=\""
				+runSpec.genericCounty.gpaFraction+"\"/>");
		printWriter.println("\t\t<barometricpressure value=\""
				+runSpec.genericCounty.barometricPressure+"\"/>");
		printWriter.println("\t\t<refuelvaporadjust value=\""
				+runSpec.genericCounty.refuelingVaporProgramAdjust+"\"/>");
		printWriter.println("\t\t<refuelspilladjust value=\""
				+runSpec.genericCounty.refuelingSpillProgramAdjust+"\"/>");
		printWriter.println("\t</genericcounty>");
	}

	/**
	 * Writes the GeographicSelections section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printGeographicSelections(PrintWriter printWriter) {
		printWriter.println("\t<geographicselections>");
		for (Iterator geographyIterator =
				runSpec.geographicSelections.iterator(); geographyIterator.hasNext();) {
			GeographicSelection iterGeography = (GeographicSelection)geographyIterator.next();
			// Escape textDescription text.
			printWriter.println("\t\t<geographicselection"
					+ " type=\""
					+ StringUtilities.safeGetString(iterGeography.type.toString()) + "\""
					+ " key=\"" + iterGeography.databaseKey + "\""
					+ " description=\""
					+ StringUtilities.safeGetEscapedString(iterGeography.textDescription) + "\""
					+ "/>");
		}
		printWriter.println("\t</geographicselections>");
	}

	/**
	 * Writes the TimeSpan section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printTimeSpan(PrintWriter printWriter) {
		boolean useNonroadRules = runSpec.models.contains(Model.NONROAD);

		printWriter.println("\t<timespan>");
		for(Iterator yearIterator=runSpec.timeSpan.years.iterator(); yearIterator.hasNext();) {
			int year = ((Integer)yearIterator.next()).intValue();
			printWriter.println("\t\t<year key=\""+year+"\"/>");
		}

		for(Iterator i=runSpec.timeSpan.months.iterator();i.hasNext();) {
			TimeSpan.Month m = (TimeSpan.Month)i.next();
			printWriter.println("\t\t<month id=\""+m.monthID+"\"/>");
		}

		for(Iterator i=runSpec.timeSpan.days.iterator();i.hasNext();) {
			TimeSpan.Day d = (TimeSpan.Day)i.next();
			printWriter.println("\t\t<day id=\""+d.dayID+"\"/>");
		}

		printWriter.println("\t\t<beginhour id=\""+runSpec.timeSpan.beginHourID+"\"/>");
		printWriter.println("\t\t<endhour id=\""+runSpec.timeSpan.endHourID+"\"/>");

		if(useNonroadRules) {
			printWriter.println("\t\t<aggregateBy key=\""+OutputTimeStep.HOUR+"\"/>");
		} else {
			printWriter.println("\t\t<aggregateBy key=\""+runSpec.timeSpan.aggregateBy+"\"/>");
		}

		printWriter.println("\t</timespan>");
	}

	/**
	 * Writes the OnRoadVehicleSelections section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printOnRoadVehicleSelections(PrintWriter printWriter) {
		printWriter.println("\t<onroadvehicleselections>");
		for (Iterator onRoadIterator = runSpec.onRoadVehicleSelections.iterator();
				onRoadIterator.hasNext();) {
			OnRoadVehicleSelection iterSelection =
					(OnRoadVehicleSelection)onRoadIterator.next();
			printWriter.println("\t\t<onroadvehicleselection"
					+ " fueltypeid=\"" + iterSelection.fuelTypeID
					+ "\" fueltypedesc=\""
					+ StringUtilities.safeGetString(iterSelection.fuelTypeDesc)
					+ "\" sourcetypeid=\"" + iterSelection.sourceTypeID
					+ "\" sourcetypename=\""
					+ StringUtilities.safeGetString(iterSelection.sourceTypeName) + "\"/>");
		}
		printWriter.println("\t</onroadvehicleselections>");
	}

	/**
	 * Writes the OffRoadVehicleSelections section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printOffRoadVehicleSelections(PrintWriter printWriter) {
		printWriter.println("\t<offroadvehicleselections>");
		for (Iterator offRoadIterator = runSpec.offRoadVehicleSelections.iterator();
				offRoadIterator.hasNext();) {
			OffRoadVehicleSelection iterSelection =
					(OffRoadVehicleSelection)offRoadIterator.next();
			printWriter.println("\t\t<offroadvehicleselection"
					+ " fueltypeid=\"" + iterSelection.fuelTypeID
					+ "\" fueltypedesc=\""
					+ StringUtilities.safeGetString(iterSelection.fuelTypeDesc)
					+ "\" sectorid=\"" + iterSelection.sectorID
					+ "\" sectorname=\""
					+ StringUtilities.safeGetString(iterSelection.sectorName) + "\"/>");
		}
		printWriter.println("\t</offroadvehicleselections>");
	}

	/**
	 * Writes the OffRoadVehicleSCCs section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printSCCs(PrintWriter printWriter) {
		printWriter.println("\t<offroadvehiclesccs>");
		for (Iterator sccIterator = runSpec.offRoadVehicleSCCs.iterator();
				sccIterator.hasNext();) {
			SCC iterSCC = (SCC)sccIterator.next();
			printWriter.println("\t\t<scc code=\""
					+ StringUtilities.safeGetString(iterSCC.scc) + "\"/>");
		}
		printWriter.println("\t</offroadvehiclesccs>");
	}

	/**
	 * Writes the RoadTypes section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printRoadTypes(PrintWriter printWriter) {
		printWriter.println("\t<roadtypes>");
		for (Iterator roadTypeIterator = runSpec.roadTypes.iterator();
				roadTypeIterator.hasNext();) {
			RoadType iterRoadType = (RoadType)roadTypeIterator.next();
			String modelCombination = "";
			switch (iterRoadType.mc) {
			case M1:
				modelCombination = "M1";
				break;
			case M2:
				modelCombination = "M2";
				break;
			case M12:
				modelCombination = "M12";
				break;
			default: 
				modelCombination = "M0";
				break;
			}
			printWriter.println("\t\t<roadtype roadtypeid=\""
					+ iterRoadType.roadTypeID + "\" roadtypename=\""
					+ StringUtilities.safeGetString(iterRoadType.roadTypeName) 
					+ "\" modelCombination=\""
					+ modelCombination
					+ "\"/>");
		}
		printWriter.println("\t</roadtypes>");
	}

	/**
	 * Writes the PollutantProcessAssocations section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printPollutantProcessAssocations(PrintWriter printWriter) {
		printWriter.println("\t<pollutantprocessassociations>");
		for (Iterator associationIterator = runSpec.pollutantProcessAssociations.iterator();
				associationIterator.hasNext();) {
			PollutantProcessAssociation iterAssociation =
					(PollutantProcessAssociation)associationIterator.next();
			if(iterAssociation.pollutant != null && iterAssociation.emissionProcess != null) {
				printWriter.println("\t\t<pollutantprocessassociation"
						+ " pollutantkey=\"" + iterAssociation.pollutant.databaseKey
						+ "\" pollutantname=\""
						+ StringUtilities.safeGetString(iterAssociation.pollutant.pollutantName)
						+ "\" processkey=\"" + iterAssociation.emissionProcess.databaseKey
						+ "\" processname=\""
						+ StringUtilities.safeGetString(
						iterAssociation.emissionProcess.processName)
						+ "\"/>");
			}
		}
		printWriter.println("\t</pollutantprocessassociations>");
	}

	/**
	 * Writes the DatabaseSelections section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printDatabaseSelections(PrintWriter printWriter) {
		printWriter.println("\t<databaseselections>");
		for (ListIterator selectionIterator = runSpec.databaseSelectionInputSets.listIterator(0);
				selectionIterator.hasNext();) {
			DatabaseSelection iterAssociation =
					(DatabaseSelection)selectionIterator.next();
			printWriter.println("\t\t<databaseselection"
					+ " servername=\""
					+ StringUtilities.safeGetString(iterAssociation.serverName) + "\""
					+ " databasename=\""
					+ StringUtilities.safeGetString(iterAssociation.databaseName) + "\""
					+ " description=\""
					+ StringUtilities.safeGetEscapedString(iterAssociation.description) + "\"/>");
		}
		printWriter.println("\t</databaseselections>");
	}

	/**
	 * Writes the InternalControlStrategies section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printInternalControlStrategies(PrintWriter printWriter) {
		printWriter.println("\t<internalcontrolstrategies>");
		Set classNames = runSpec.internalControlStrategies.keySet();
		for(Iterator i=classNames.iterator();i.hasNext();) {
			String className = (String)i.next();
			LinkedList instances = (LinkedList)runSpec.internalControlStrategies.get(className);
			if(instances != null) {
				for(Iterator j=instances.iterator();j.hasNext();) {
					InternalControlStrategy strategy = (InternalControlStrategy)j.next();
					printInternalControlStrategy(strategy,printWriter,true);
				}
			}
		}
		printWriter.println("\t</internalcontrolstrategies>");
	}

	/**
	 * Writes a single InternalControlStrategy object to an XML file
	 * @param strategy the InternalControlStrategy object to write
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	 * @param useXMLAroundText When true and the strategy uses a pure text format
	 * rather than XML, a small XML wrapper will be added to the text.  This allows
	 * the text to be embedded within a larger XML document.
	**/
	void printInternalControlStrategy(InternalControlStrategy strategy,
			PrintWriter printWriter, boolean useXMLAroundText) {
		String xml = strategy.getXML();
		if(xml != null && xml.length() > 0) {
			printWriter.println("\t\t<internalcontrolstrategy"
					+ " classname=\"" + strategy.getClass().getName() + "\">");
			printWriter.println(xml);
			printWriter.println("\t\t</internalcontrolstrategy>");
		} else {
			String text = strategy.getTSV();
			if(text == null) {
				return;
			}
			if(useXMLAroundText) {
				printWriter.println("<internalcontrolstrategy"
						+ " classname=\"" + strategy.getClass().getName() + "\"><![CDATA[");
			}
			printWriter.println(text);
			if(useXMLAroundText) {
				printWriter.println("]]></internalcontrolstrategy>");
			}
		}
	}

	/**
	 * Writes the OutputEmissionsBreakdownSelections section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printOutputEmissionsBreakdownSelections(PrintWriter printWriter) {
		printWriter.println("\t<outputemissionsbreakdownselection>");
		printWriter.println("\t\t<modelyear selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.modelYear + "\"/>");
		printWriter.println("\t\t<fueltype selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.fuelType + "\"/>");
		if(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
			printWriter.println("\t\t<fuelsubtype selected=\""
					+ runSpec.outputEmissionsBreakdownSelection.fuelSubType + "\"/>");
		}
		printWriter.println("\t\t<emissionprocess selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.emissionProcess + "\"/>");
		printWriter.println("\t\t<onroadoffroad selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.onRoadOffRoad + "\"/>");
		printWriter.println("\t\t<roadtype selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.roadType + "\"/>");
		printWriter.println("\t\t<sourceusetype selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.sourceUseType + "\"/>");
		printWriter.println("\t\t<movesvehicletype selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.movesVehicleType + "\"/>");
		printWriter.println("\t\t<onroadscc selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.onRoadSCC + "\"/>");
		printWriter.println("\t\t<estimateuncertainty selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.estimateUncertainty
				+ "\" numberOfIterations=\""
				+ runSpec.outputEmissionsBreakdownSelection.numberOfIterations
				+ "\" keepSampledData=\""
				+ runSpec.outputEmissionsBreakdownSelection.keepSampledData
				+ "\" keepIterations=\""
				+ runSpec.outputEmissionsBreakdownSelection.keepIterations
				+ "\"/>");
		printWriter.println("\t\t<sector selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.sector + "\"/>");
		printWriter.println("\t\t<engtechid selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.engTechID + "\"/>");
		printWriter.println("\t\t<hpclass selected=\""
				+ runSpec.outputEmissionsBreakdownSelection.hpClass + "\"/>");
		if(CompilationFlags.DO_RATES_FIRST) {
			printWriter.println("\t\t<regclassid selected=\""
					+ runSpec.outputEmissionsBreakdownSelection.regClassID + "\"/>");
		}
		printWriter.println("\t</outputemissionsbreakdownselection>");
	}

	/**
	 * Writes the OutputFactors section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	void printOutputFactors(PrintWriter printWriter) {
		printWriter.println("\t<outputfactors>");
		printWriter.println("\t\t<timefactors selected=\""
				+ runSpec.outputFactors.timeFactorsSelected + "\""
				+ " units=\"" + (runSpec.outputFactors.timeMeasurementSystem != null ?
				runSpec.outputFactors.timeMeasurementSystem.toString() : "")
				+ "\"/>");
		printWriter.println("\t\t<distancefactors selected=\""
				+ runSpec.outputFactors.distanceFactorsSelected + "\""
				+ " units=\"" + (runSpec.outputFactors.distanceMeasurementSystem != null ?
				runSpec.outputFactors.distanceMeasurementSystem.toString() : "")
				+ "\"/>");
		printWriter.println("\t\t<massfactors selected=\""
				+ runSpec.outputFactors.massFactorsSelected + "\""
				+ " units=\"" + (runSpec.outputFactors.massMeasurementSystem != null ?
				runSpec.outputFactors.massMeasurementSystem.toString() : "")
				+ "\" energyunits=\"" + (runSpec.outputFactors.energyMeasurementSystem != null ?
				runSpec.outputFactors.energyMeasurementSystem.toString() : "")
				+ "\"/>");
		printWriter.println("\t</outputfactors>");
	}

	/**
	 * Write a set of class names to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file
	 * @param classNames a set of class names to be saved
	**/
	void printClasses(PrintWriter printWriter, TreeSetIgnoreCase classNames) {
		for(Iterator i=classNames.iterator();i.hasNext();) {
			String s = (String)i.next();
			printWriter.println("\t\t<class name=\"" + s + "\"/>");
		}
	}

	/**
	 * Saves from a single InternalControlStrategy to an XML file.
	 * @param outFile The XML file to write to.
	 * @param strategy The InternalControlStrategy object to export
	**/
	public void saveInternalControlStrategy(File outFile,InternalControlStrategy strategy) {
		// This operation overwrites any existing file.
		if(outFile.isFile()) {
			outFile.delete();
		}
		try {
			PrintWriter printWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(outFile))));
			printInternalControlStrategy(strategy,printWriter,false);
			printWriter.close();
		} catch(Exception e) {
			/** @explain The RunSpec file is corrupt, likely due to a typo. **/
			Logger.logError(e,"Unable to save XML for InternalControlStrategy");
		}
	}

	/**
	 * Loads from the XML file into a new instance of an InternalControlStrategy
	 * @param inFile The XML file to read from.
	 * @param defaultClassName class name to use if one is not specified within the file
	 * @return non-null instance of an InternalControlStrategy upon success,
	 * null upon failure
	**/
	public InternalControlStrategy loadInternalControlStrategy(File inFile,
			String defaultClassName) {
		InternalControlStrategy strategy = null;
		String errorMessage = null;
		Exception error = null;

		// Try loading from XML
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			db.setErrorHandler(new XMLErrorHandler());
			Document doc = db.parse(inFile);
			// Verify that this is an InternalControlStrategy xml file
			Node node = doc.getFirstChild();
			if(!(node != null && node.getNodeName().equalsIgnoreCase("internalcontrolstrategy"))) {
				//L ogger.log(LogMessageCategory.ERROR, "Invalid InternalControlStrategy XML file.");
				/**
				 * @issue Invalid InternalControlStrategy XML file.
				 * @explain The RunSpec file is corrupt, likely due to a typo.
				**/
				errorMessage = "Invalid InternalControlStrategy XML file.";
			} else {
				strategy = processInternalControlStrategy(node,defaultClassName,null);
			}
		} catch(SAXException e) {
			// If we get this exception, then we don't have a chance to read any fields
			//L ogger.logError(e,"Could not load InternalControlStrategy XML.");
			errorMessage = "Could not load InternalControlStrategy XML.";
			error = e;
		} catch(Exception e) {
			// This indicates an error during one field's parsing
			//L ogger.logError(e,"Could not load InternalControlStrategy XML");
			errorMessage = "Could not load InternalControlStrategy XML";
			error = e;
		}

		if(strategy != null) {
			return strategy;
		}

		// Try loading from plain text
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new FileReader(inFile),32768);
			StringBuffer b = new StringBuffer((int)(inFile.length()*1.10));
			String line;
			while((line=reader.readLine()) != null) {
				b.append(line);
				b.append('\n');
			}
			reader.close();
			reader = null;

			strategy = processInternalControlStrategy(null,defaultClassName,b.toString());
		} catch(Exception e) {
			errorMessage = "Could not load InternalControlStrategy text";
			error = e;
		} finally {
			if(reader != null) {
				try {
					reader.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				reader = null;
			}
		}

		if(strategy != null) {
			return strategy;
		}

		if(error != null) {
			/**
			 * @issue Could not load InternalControlStrategy XML.
			 * @explain The RunSpec file is corrupt, likely due to a typo.
			**/
			Logger.logError(error,errorMessage);
		} else if(errorMessage != null) {
			/**
			 * @issue Could not load InternalControlStrategy text
			 * @explain The RunSpec file is corrupt, likely due to a typo.
			**/
			Logger.log(LogMessageCategory.ERROR,errorMessage);
		}

		return null;
	}

	/**
	 * Enforce data consistency rules. Most rules are enforced by the GUI
	 * and allowed to be flexible when directly loaded though.
	**/
	void enforceConsistency() {
		boolean useNonroadRules = runSpec.models.contains(Model.NONROAD);
		if(useNonroadRules) {
			/*
			runSpec.outputEmissionsBreakdownSelection.onRoadSCC = true;
			runSpec.outputEmissionsBreakdownSelection.sector = true;
			runSpec.outputEmissionsBreakdownSelection.engTechID = true;
			runSpec.outputEmissionsBreakdownSelection.hpClass = true;
			runSpec.outputEmissionsBreakdownSelection.modelYear = true;
			runSpec.outputEmissionsBreakdownSelection.fuelType = true;
			runSpec.outputEmissionsBreakdownSelection.emissionProcess = true;
			*/
			if(!runSpec.outputEmissionsBreakdownSelection.fuelType) {
				runSpec.outputEmissionsBreakdownSelection.fuelSubType = false;
			}
			if(runSpec.outputEmissionsBreakdownSelection.fuelSubType) {
				runSpec.outputEmissionsBreakdownSelection.fuelType = true;
			}
			runSpec.timeSpan.aggregateBy = OutputTimeStep.HOUR;
		} else {
			// Don't do fuelSubTypeID detail for Onroad yet.
			runSpec.outputEmissionsBreakdownSelection.fuelSubType = false;

			// Enforce onroad SCC detail requirements. To create onroad SCC output,
			// MOVES requires several details to be enabled.
			if(runSpec.outputEmissionsBreakdownSelection.onRoadSCC) {
				runSpec.outputEmissionsBreakdownSelection.fuelType = true;
				runSpec.outputEmissionsBreakdownSelection.sourceUseType = true;
				runSpec.outputEmissionsBreakdownSelection.roadType = true;
				runSpec.outputEmissionsBreakdownSelection.emissionProcess = true;
			}
			if(CompilationFlags.DO_RATES_FIRST && runSpec.scale == ModelScale.MESOSCALE_LOOKUP) { // If making Rates outputs...
				runSpec.outputVMTData = runSpec.usesEvapRates();
				runSpec.outputPopulation = true;
				runSpec.outputSHO = false;
				runSpec.outputSH = false;
				runSpec.outputSHP = false;
				runSpec.outputSHIdling = true;
				runSpec.outputStarts = true;
			}
		}
	}
}
