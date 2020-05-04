/**************************************************************************************************
 * @(#)DocumentBuilder.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.tools.messages;

import java.io.*;
import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.SchemaInspector;
import gov.epa.otaq.moves.common.graph.*;
import gov.epa.otaq.moves.common.graph.sql.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * Find tag messages and documentation within Java and SQL Code.
 * Tags messages can be documented using Java-Doc style opening and closing comment markers
 * (slash-star-star and star-star-slash respectively) and new @-style tags.
 * SQL tags are wraped in comment line (--)
 * tags are:
 *	@algorithm
 *	@owner
 *	@input
 *	@output
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @version		2015-04-04
**/
public class DocumentBuilder {
	/**
	 * Entry point.  The first argument is the name of the HTML file to be generated.
	 * All remaining arguments are the names of folders to be scanned.
	 * No document is generated if a document is not named or if no folders are named.
	 * @param args The first argument is the name of the HTML file to be generated.
	 * All remaining arguments are the names of folders to be scanned.
	**/
	public static void main(String[] args) {
		if(args.length < 1) {
			System.out.println("ERROR: No output HTML file name given.");
			return;
		}
		if(args.length < 2) {
			System.out.println("ERROR: No folder to be scanned given.");
			return;
		}
		DocumentBuilder d = new DocumentBuilder(args);
		d.go();
	}

	public static class AlgoDescription  {
		public int startLineNumber0;
		public int endLineNumber0;
		public int lastValidLine;
		public int stepIndex=0;
		public File file;
		public String fileName = "";
		public String filePath = "";
		public String algorithm = "";
		public String step = "";
		public String call = "";
		public boolean callIsDone = false;
		public String owner = "";
		public String moduleName = "";
		public String signup = "";
		public String loopableType = "";
		public boolean isUsed = true;
		public ArrayList<String> conditions = new ArrayList<String>();
		public ArrayList<String> next100SQLLines = new ArrayList<String>();
		public ArrayList<String> lines = new ArrayList<String>();
		public TreeSetIgnoreCase inputs = new TreeSetIgnoreCase();
		public TreeSetIgnoreCase outputs = new TreeSetIgnoreCase();
	}

	public static class AlgorithmOwner {
		public String owner = "";
		public String moduleName = "";
		public String htmlTagName = "";
		public boolean isTopLevel = true;
		public boolean hasCall = false;
		public String signup = "";
		public String loopableType = "";
		public boolean isUsed = true;
		public TreeSetIgnoreCase inputs = new TreeSetIgnoreCase();
		public TreeSetIgnoreCase outputs = new TreeSetIgnoreCase();
		public boolean hasPrintableSteps = false;
		public TreeMap<String,AlgoDescription> steps = new TreeMap<String,AlgoDescription>();
		public TreeSetIgnoreCase fileNames = new TreeSetIgnoreCase();
	}

	public static class DatabaseTable {
		public String name = "";
		public TreeMap<String,AlgorithmOwner> users = new TreeMap<String,AlgorithmOwner>();
		public String htmlTagName = "";
		public String createTableStatement = "";

		public DatabaseTable(String nameToUse) {
			name = nameToUse;
		}
	}

	public static class Registration {
		public String outputPollutantName = "";
		public String outputPollutantID	= "";
		public String processName = "";
		public String processID	= "";
		public String moduleName = "";
	}

	public static class Subscribe {
		public String moduleName = "";
		public String processName = "";
		public String processID	= "";
		public String granularity = "";
		public String priority = "";
	}

	public static class Chain {
		public String outputModuleName = "";
		public String inputModuleName = "";
	}

	/** HTML file to be created **/
	File htmlFile = null;
	/** List of File objects that are the folders to be scanned **/
	ArrayList<File> folders = new ArrayList<File>();
	/** List of AlgoDescription objects in the order they were discovered **/
	ArrayList<AlgoDescription> algos = new ArrayList<AlgoDescription>();
	TreeMap<String,AlgorithmOwner> owners = new TreeMap<String,AlgorithmOwner>();
	TreeMap<String,DatabaseTable> defaultTables = new TreeMap<String,DatabaseTable>();
	ArrayList<Registration> registrations = new ArrayList<Registration>();
	ArrayList<Subscribe> subscribes = new ArrayList<Subscribe>();
	ArrayList<Chain> chains = new ArrayList<Chain>();

	String currentWorkingFile;
	String currentWorkingFilePath;
	String currentOwner = "";
	String currentStep = "";

	private DocumentBuilder(String[] args) {
		htmlFile = new File(args[0]);
		if(htmlFile.exists()) {
			FileUtilities.deleteFileWithRetry(htmlFile);
		}
		for(int i=1;i<args.length;i++) {
			File f = new File(args[i]);
			if(f.exists()) {
				folders.add(f);
			}
		}
	}

	/** Scan folders and generate the HTML file **/
	private void go() {
		if(folders.size() <= 0) {
			System.out.println("ERROR: No valid folder found to scan.");
			return;
		}
		// Load supporting information
		learnDefaultTables();
		loadCalulatorFacts(new File("CalculatorInfo.txt"));
		// Scan all required folders and subfolders
		for(Iterator<File> i=folders.iterator();i.hasNext();) {
			File f = (File)i.next();
			try {
				String parentPath = "";
				File parent = f.getCanonicalFile().getParentFile();
				if(parent != null) {
					parentPath = parent.getCanonicalPath().toLowerCase();
				} else {
					System.out.println("Folder " + f.getName() + " has no parent.");
				}
				scanFolder(f,parentPath);
			} catch(IOException e) {
				System.out.println("ERROR: Unable to scan folder " + f.getName());
				return;
			}
		}
		aggregateOwners();
		writeToHTML();
		try {
			System.out.println("Message documentation generated in: " + htmlFile.getCanonicalPath());
		} catch(Exception e) {
			// Nothing to be done here
		}
	}

	/**
	 * Scan a folder's files and subfolders.
	 * @param folder folder to be examined
	 * @param topLevelText text to be removed when storing file paths
	 * @throws IOException if anything goes wrong
	**/
	private void scanFolder(File folder, String topLevelText) throws IOException {
		File[] children = folder.listFiles();
		if(children == null || children.length <= 0) {
			return;
		}

		// Scan files first
		for(int i=0;i<children.length;i++) {
			if(!children[i].isDirectory()) {
				scanFile(children[i],topLevelText);
			}
		}

		// Scan child folders after files
		for(int i=0;i<children.length;i++) {
			if(children[i].isDirectory()) {
				scanFolder(children[i],topLevelText);
			}
		}
	}

	/**
	 * Scan a file for tags.
	 * @param file file to be examined
	 * @param topLevelText text to be removed when storing file paths
	 * @throws IOException if anything goes wrong
	**/
	private void scanFile(File file, String topLevelText) throws IOException {
		String canonicalPath = file.getCanonicalPath();
		String lowerCasePath = canonicalPath.toLowerCase();
		if(lowerCasePath.endsWith("documentbuilder.java")
			|| lowerCasePath.endsWith("messagedocumentor.java")
			|| lowerCasePath.endsWith("test.java")
			|| lowerCasePath.endsWith("crankcaseemissioncalculator.java")
			|| (lowerCasePath.endsWith("tankvaporventingcalculator.sql") && !lowerCasePath.endsWith("multidaytankvaporventingcalculator.sql"))
			|| !(lowerCasePath.endsWith(".java") || lowerCasePath.endsWith(".sql") || lowerCasePath.endsWith(".go"))
			){
			return;
		}
		if(lowerCasePath.startsWith(topLevelText)) {
			canonicalPath = canonicalPath.substring(topLevelText.length());
		}
		ArrayList<String> lines = readTrimmedLines(file);
		if(lines == null || lines.size() <= 0) {
			return;
		}
		for(int i=0;i<lines.size();i++) {
			String line = (String)lines.get(i);
			if(line.indexOf("@fileNotUsed") >= 0) {
				return;
			}
		}
		int nextAllowedSiteLine = 0;

		currentWorkingFile=file.getName();
		//currentWorkingFilePath = canonicalPath.replace("\\","\\\\");
		currentWorkingFilePath = canonicalPath.replace("\\","/");
		currentStep = "";
		currentOwner = "";

		boolean allowInfer = true;
		if(lowerCasePath.endsWith("aggregationsqlgenerator.java")
				|| file.getName().equalsIgnoreCase("generator.java")
				|| file.getName().equalsIgnoreCase("crankcaseemissioncalculator.java")
				|| file.getName().equalsIgnoreCase("emissioncalculator.java")
				|| file.getName().equalsIgnoreCase("dummycalculator.java")
				|| file.getName().equalsIgnoreCase("genericcalculatorbase.java")
				|| file.getName().equalsIgnoreCase("genericcalculator.sql")) {
			allowInfer = false;
		}
		AlgoDescription inferredAlgo = new AlgoDescription();
		inferredAlgo.file = file;
		inferredAlgo.startLineNumber0 = 0;
		inferredAlgo.endLineNumber0 = 0;
		inferredAlgo.owner = "";
		inferredAlgo.step = "";
		inferredAlgo.fileName=currentWorkingFile;
		inferredAlgo.filePath=currentWorkingFilePath;

		int firstAlgoIndex = algos.size();
		boolean didInfer = false;
		boolean didInferNext100 = false;
		boolean isSqlCalculator = lowerCasePath.endsWith("calculator.sql");
		if(allowInfer) {
			if(isSqlCalculator) {
				didInfer = inferFromSQL(file,lines,inferredAlgo);
			} else if(lowerCasePath.endsWith("generator.java") || lowerCasePath.endsWith("calculator.java")) {
				didInfer = inferFromJava(file,lines,inferredAlgo);
			}
		}

		for(int i=0;i<lines.size();i++) {
			String line = (String)lines.get(i);
			// Look for comment blocks to start
			if(line.startsWith("/**")) {
				nextAllowedSiteLine = i;
				int startLine = i;
				int endLine = i;
				for(;i<lines.size();i++) {
					line = (String)lines.get(i);
					if(line.endsWith("**/")) {
						endLine = i;
						break;
					}
				}
				parseCommentBlock(file,lines,startLine,endLine,false,isSqlCalculator);
				continue;
			}
			if(line.startsWith("/*")) {
				nextAllowedSiteLine = i;
				int startLine = i;
				int endLine = i;
				for(;i<lines.size();i++) {
					line = (String)lines.get(i);
					if(line.endsWith("*/")) {
						endLine = i;
						break;
					}
				}
				parseCommentBlock(file,lines,startLine,endLine,false,isSqlCalculator);
				continue;
			}
			if(line.startsWith("//")) {
				nextAllowedSiteLine = i;
				int startLine = i;
				int endLine = i;
				for(;i<lines.size();i++) {
					line = (String)lines.get(i);
					if(line.startsWith("//")) {
						endLine = i;
					} else {
						break;
					}
				}
				parseCommentBlock(file,lines,startLine,endLine,false,isSqlCalculator);
				continue;
			}
			if(line.startsWith("-- @")) {
				nextAllowedSiteLine = i;
				int startLine = i;
				int endLine = i;
				boolean almostDone = false;
				int extraLines=0;
				for(;i<lines.size();i++) {
					line = (String)lines.get(i);
					endLine = i;
					if( almostDone==false && (line.length()==0 || !line.startsWith("--")) ) {
						almostDone = true;
					}
					if( almostDone==true ) {
						// read next 200 lines or until a comment line
						extraLines++;
						if( extraLines>200 || line.startsWith("--") || line.length()==0 ) {
							if(line.startsWith("--")) {
								endLine = i-1;
							}
							break;
						}//if
					}
				}//for
				parseCommentBlock(file,lines,startLine,endLine,true,isSqlCalculator);
				continue;
			}
			if(i < nextAllowedSiteLine) {
				continue;
			}
		}

		// Stub some information for files that should be part of a calculator or generator even if no comments were found
		if(allowInfer && firstAlgoIndex == algos.size()) {
			if(isSqlCalculator) {
				//System.out.println("Adding calculator default for " + file.getName());
				AlgoDescription algo = new AlgoDescription();
				algo.file = file;
				algo.startLineNumber0 = 0;
				algo.endLineNumber0 = 0;
				algo.owner=currentOwner;
				algo.step = "";
				algo.fileName=currentWorkingFile;
				algo.filePath=currentWorkingFilePath;
				algos.add(algo);
				algo.stepIndex = algos.size();

				algo.loopableType = "CALCULATOR";
			} else if(lowerCasePath.endsWith("generator.java")) {
				//System.out.println("Adding generator default for " + file.getName());
				AlgoDescription algo = new AlgoDescription();
				algo.file = file;
				algo.startLineNumber0 = 0;
				algo.endLineNumber0 = 0;
				algo.owner=currentOwner;
				algo.step = "";
				algo.fileName=currentWorkingFile;
				algo.filePath=currentWorkingFilePath;
				algos.add(algo);
				algo.stepIndex = algos.size();

				algo.loopableType = "GENERATOR";
			}
		}

		// Apply inferred items if we found or made an algorithm description
		if(didInfer){
			//System.out.println("Adding inferrences for " + file.getName());
			inferredAlgo.owner=currentOwner;
			algos.add(inferredAlgo);
			inferredAlgo.stepIndex = algos.size();
		}

		// Make sure all blocks in this file have an owner,
		// defaulting to the first owner or the file name.
		String moduleName = FileUtilities.getBaseFileName(file);
		String fileOwner = moduleName;
		for(int i=firstAlgoIndex;i<algos.size();i++) {
			AlgoDescription a = algos.get(i);
			if(a.owner.length() > 0) {
				fileOwner = a.owner;
				break;
			}
		}
		for(int i=firstAlgoIndex;i<algos.size();i++) {
			AlgoDescription a = algos.get(i);
			a.moduleName = moduleName;
			if(a.owner.length() <= 0) {
				a.owner = fileOwner;
			}
		}
	}

	/**
	 * Reads an entire file into an ArrayList of String objects, one for each line.
	 * Lines are trimmed as they are read.
	 * @param file The file to read.
	 * @return The file contents as an ArrayList of String objects, one for each line.
	 * @throws IOException If there was an error reading the file.
	**/
	private static ArrayList<String> readTrimmedLines(File file) throws IOException {
		ArrayList<String> lines = new ArrayList<String>();
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new FileReader(file), 65536);
			String line = null;
			while((line = reader.readLine()) != null) {
				line = line.trim();
				line = StringUtilities.replace(line,"\\r","");
				line = StringUtilities.replace(line,"\\n","");
				line = StringUtilities.replace(line,"\\t","");
				lines.add(line.trim());
			}
		} finally {
			if(reader != null) {
				try {
					reader.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
		return lines;
	}

	/**
	 * Scan a Java-Doc style comment block for our tags, generating an AlgoDescription object as needed.
	 * @param file file being processed
	 * @param lines all lines within the file
	 * @param startLine 0-based index within lines to the opening line of the comment block
	 * @param endLine 0-based index within lines to the closing line of the comment block
	 * @param isSql test for is sql statments
	 * @param isSqlCalculator test for lowerCasePath.endsWith("calculator.sql")
	**/
	private void parseCommentBlock(File file, ArrayList<String> lines, int startLine, int endLine, boolean isSql, boolean isSqlCalculator) {
		boolean hasOurTags = false;
		for(int i=startLine;i<=endLine;i++) {
			String line = (String)lines.get(i);
			if( line.indexOf("@author") >= 0
				|| line.indexOf("@version") >= 0
				|| line.indexOf("@param") >= 0
				|| line.indexOf("@return") >= 0
				|| line.indexOf("@nonissue") >= 0
				|| line.indexOf("@issue") >= 0
				|| line.indexOf("@explain") >= 0 ) {
				return; // Is a standard JavaDoc block
			}
			if(!hasOurTags
				&& (line.indexOf("@algorithm") >= 0
					|| line.indexOf("@owner") >= 0
					|| line.indexOf("@step") >= 0
					|| line.indexOf("@call") >= 0
					|| line.indexOf("@condition") >= 0
					|| line.indexOf("@input") >= 0
					|| line.indexOf("@output") >= 0
					|| line.indexOf("@calculator") >= 0
					|| line.indexOf("@generator") >= 0
					|| line.indexOf("@notused") >= 0) ) {
				hasOurTags = true;
			}
		}
		if(!hasOurTags) {
			return; // Is a JavaDoc block without information for us
		}
		AlgoDescription algo = new AlgoDescription();
		algo.file = file;
		algo.startLineNumber0 = startLine;
		algo.endLineNumber0 = endLine;
		algo.owner="";
		algo.step = "";
		algo.fileName=currentWorkingFile;
		algo.filePath=currentWorkingFilePath;
		algos.add(algo);

		algo.stepIndex = algos.size();

		int lastValidLine=0;
		for(int i=startLine;i<=endLine;i++) {
			boolean shouldLog = false;
			nextCommandLine = i-1;
			String line = (String)lines.get(i);
			if(line.indexOf("@algorithm") >= 0) {
				int index = line.indexOf("@algorithm") + 10;
				algo.algorithm = parseToNextCommand(lines,i,endLine,index);
				lastValidLine=i;
			}
			/*
			if(algo.algorithm.toLowerCase().indexOf("do start temperature adjustments by opmodeid") >= 0) {
				shouldLog = true;
			}
			*/
			if(line.indexOf("@owner") >= 0) {
				int index = line.indexOf("@owner") + 6;
				algo.owner = parseToNextCommand(lines,i,endLine,index);
				currentOwner = algo.owner;
				lastValidLine=i;
			} else if(line.indexOf("@input") >= 0) {
				int index = line.indexOf("@input") + 6;
				algo.inputs.add( parseToNextCommand(lines,i,endLine,index));
				lastValidLine=i;
			} else if(line.indexOf("@output") >= 0) {
				int index = line.indexOf("@output") + 7;
				algo.outputs.add( parseToNextCommand(lines,i,endLine,index));
				lastValidLine=i;
			} else if(line.indexOf("@step") >= 0) {
				int index = line.indexOf("@step") + 6;
				algo.step = parseToNextCommand(lines,i,endLine,index);
				currentStep = algo.step;
				lastValidLine=i;
			} else if(line.indexOf("@call") >= 0) {
				int index = line.indexOf("@call") + 5;
				algo.call = parseToNextCommand(lines,i,endLine,index);
				lastValidLine=i;
			} else if(line.indexOf("@condition") >= 0) {
				int index = line.indexOf("@condition") + 10;
				String t = parseToNextCommand(lines,i,endLine,index);
				if(shouldLog) {
					System.out.println("parseToNextCommand(lines,"+i+","+endLine+","+index+")");
					System.out.println("algo.conditions.add(" + t + ")");
				}
				algo.conditions.add(t);
				lastValidLine=i;
			} else if(line.indexOf("@signup") >= 0) {
				int index = line.indexOf("@signup") + 7;
				algo.signup = parseToNextCommand(lines,i,endLine,index).toUpperCase();
				lastValidLine=i;
			} else if(line.indexOf("@calculator") >= 0) {
				int index = line.indexOf("@calculator") + 11;
				algo.loopableType = "CALCULATOR";
				lastValidLine=i;
			} else if(line.indexOf("@generator") >= 0) {
				int index = line.indexOf("@generator") + 10;
				algo.loopableType = "GENERATOR";
				lastValidLine=i;
			} else if(line.indexOf("@notused") >= 0) {
				algo.isUsed = false;
				lastValidLine=i;
			}
			
			if(shouldLog) {
				System.out.println("i="+i+",nextCommandLine="+nextCommandLine+",lastValidLine="+lastValidLine+",line="+line);
			}
			if(nextCommandLine > i) {
				i = nextCommandLine - 1; // subtract 1 to allow for loop increment above
			}
		}

		if(algo.owner.length() <= 0) {
			algo.owner = currentOwner;
		}
		if(algo.step.length() <= 0) {
			algo.step = currentStep;
		}

		algo.lastValidLine=lastValidLine;

		// stuff extra lines here
		if( isSql ){
			int addCount =0;
			for(int i=lastValidLine+1;i<=endLine;i++) {
				String line = (String)lines.get(i);
				if(addCount<1000 ){
					algo.next100SQLLines.add(line);
					addCount++;
					//if( line.endsWith(";") )break;
				}
			}
			if(isSqlCalculator ) {
				inferFromSQLNext100(file,algo);
			}
		}
	}

	int nextCommandLine = 0;

	/**
	 * Extract command text, upto the next command or end of comment block
	 * @param lines all lines within a file
	 * @param currentLine index within lines of the current line
	 * @param endLine last line to be examined
	 * @param startIndex starting location within the current line
	 * @return text of the command
	**/
	private String parseToNextCommand(ArrayList<String> lines, int currentLine, int endLine,
			int startIndex) {
		nextCommandLine = currentLine+1;
		String line = (String)lines.get(currentLine);
		String result = line.substring(startIndex).trim();
		if(result.endsWith("**/")) {
			result = result.substring(0,result.length()-3).trim();
			return result;
		}
		if(result.endsWith("*/")) {
			result = result.substring(0,result.length()-2).trim();
			return result;
		}
		for(int i=currentLine+1;i<=endLine;i++) {
			nextCommandLine = i;
			line = (String)lines.get(i);
			if(line.indexOf("@") >= 0) {
				return result;
			}
			if(line.startsWith("*") && !line.startsWith("**/") && !line.startsWith("*/")) {
				result += " " + line.substring(1).trim();
				continue;
			} else if(line.startsWith("--")) {
				result += " " + line.substring(2).trim();
				continue;
			} else if(line.length()==0) {
				return result;
			}
			if(result.endsWith("**/")) {
				result = result.substring(0,result.length()-3).trim();
				return result;
			}
			if(result.endsWith("*/")) {
				result = result.substring(0,result.length()-2).trim();
				return result;
			}
			break;
		}
		return result;
	}

	/**
	 * Get the list of all tables in the default database.
	**/
	private void learnDefaultTables() {
		File defaultScript = new File("database/CreateDefault.sql");
		if(!defaultScript.exists()) {
			/**
			 * @explain The algorithm documentation will be incomplete because
			 * the required script file (database/CreateDefault.sql) could not
			 * be found.
			**/
			System.out.println("ERROR: Unable to locate database/CreateDefault.sql");
			return;
		}
		Connection db = null;
		try {
			TreeMapIgnoreCase tableData = SchemaInspector.readScript(defaultScript);
			if(tableData == null) {
				return;
			}
			Set<String> tableNames = tableData.keySet();
			for(String name : tableNames) {
				DatabaseTable t = new DatabaseTable(name);
				defaultTables.put(t.name.toUpperCase(),t);
			}

			Configuration.allowGUI = false;

			if(!SystemConfiguration.getTheSystemConfiguration().didLoad) {
				return;
			}
			DatabaseConnectionManager.initialize(MOVESDatabaseType.DEFAULT);
			db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
			tableNames = defaultTables.keySet();
			for(String name : tableNames) {
				DatabaseTable table = defaultTables.get(name);
				String sql = "show create table " + table.name;
				SQLRunner.Query query = new SQLRunner.Query();
				query.open(db,sql);
				if(query.rs.next()) {
					table.createTableStatement = StringUtilities.replace(query.rs.getString(2),"`","");
				}
				query.onFinally();
			}
		} catch(Exception e) {
			/**
			 * @explain The algorithm documentation will be incomplete because
			 * the required script file (database/CreateDefault.sql) could not
			 * be parsed.
			**/
			System.out.println("ERROR: Unable to read database/CreateDefault.sql: " + e.toString());
		}
	}

	/**
	 * Merge all comment blocks into owner object.
	**/
	private void aggregateOwners() {
		// Make the list of owners and their steps
		for(int i=0;i<algos.size();i++) {
			AlgoDescription algo = algos.get(i);
			if(algo.owner.length() <= 0) {
				continue;
			}
			AlgorithmOwner owner = owners.get(algo.owner);
			if(owner == null) {
				owner = new AlgorithmOwner();
				owner.owner = algo.owner;
				owner.moduleName = algo.moduleName;
				owners.put(owner.owner,owner);
			}
			if(!algo.isUsed) {
				owner.isUsed = false;
			}
			owner.fileNames.add(algo.filePath);
			owner.steps.put(algo.step.toUpperCase() + "." + StringUtilities.getLeadingZeroPaddedString(algo.stepIndex,6),algo);
			if(algo.algorithm.length() > 0) {
				owner.hasPrintableSteps = true;
			}
			if(!owner.hasCall && algo.call.length() > 0) {
				owner.hasCall = true;
			}
			if(algo.signup.length() > 0 && owner.signup.length() <= 0) {
				owner.signup = algo.signup;
			}
			if(algo.loopableType.length() > 0 && owner.loopableType.length() <= 0) {
				owner.loopableType = algo.loopableType;
			}
		}
		Set<String> ownerNames = owners.keySet();
		// Resolve calls by using their steps, recursively
		for(String ownerName : ownerNames) {
			AlgorithmOwner owner = owners.get(ownerName);
			if(!owner.hasCall) { // Quit quickly if there are no steps that call other steps
				continue;
			}
			boolean done = false;
			while(!done) {
				done = true;
				Set<String> stepNames = owner.steps.keySet();
				for(String stepName : stepNames) {
					AlgoDescription algo = owner.steps.get(stepName);
					if(algo.call.length() > 0 && !algo.callIsDone) {
						algo.callIsDone = true;
						AlgorithmOwner calledOwner = owners.get(algo.call.toUpperCase());
						if(calledOwner != null) {
							calledOwner.isTopLevel = false;
							if(calledOwner.hasPrintableSteps) {
								owner.hasPrintableSteps = true;
							}
							Set<String> otherStepNames = calledOwner.steps.keySet();
							for(String otherStepName : otherStepNames) {
								AlgoDescription otherAlgo = calledOwner.steps.get(otherStepName);
								owner.steps.put(stepName + ":" + otherStepName,otherAlgo);
							}
							done = false;
							break;
						}
					}
				}
			}
		}
		// Now that all steps are known we can find all inputs and outputs
		for(String ownerName : ownerNames) {
			AlgorithmOwner owner = owners.get(ownerName);
			Set<String> stepNames = owner.steps.keySet();
			for(String stepName : stepNames) {
				AlgoDescription algo = owner.steps.get(stepName);
				// Get the list of inputs from the default database
				for(String t : algo.inputs) {
					DatabaseTable table = defaultTables.get(t.toUpperCase());
					if(table != null) {
						owner.inputs.add(table.name);
						table.users.put(owner.owner.toUpperCase(),owner);
					}
				}
				// Get the list of outputs from the default database
				for(String t : algo.outputs) {
					DatabaseTable table = defaultTables.get(t.toUpperCase());
					if(table != null) {
						owner.outputs.add(table.name);
						table.users.put(owner.owner.toUpperCase(),owner);
					}
				}
			}
		}
		// Match all chaining data to owners
		for(Chain c : chains) {
			AlgorithmOwner a = findOwnerExtensive(c.inputModuleName);
			if(a != null) {
				c.inputModuleName = a.owner;
			}
			a = findOwnerExtensive(c.outputModuleName);
			if(a != null) {
				c.outputModuleName = a.owner;
			}
		}
		// Match all registrations to owners
		for(Registration r : registrations) {
			AlgorithmOwner a = findOwnerExtensive(r.moduleName);
			if(a != null) {
				r.moduleName = a.owner;
			}
		}
	}

	private int inputLogCounter = 20;

	/**
	 * Infer data from a calculator SQL file.
	 * @param file file to be examined.
	 * @param lines trimmed text lines.
	 * @param algo object to hold inferred results.
	 * @return true if anything was inferred.
	**/
	private boolean inferFromSQLNext100(File file,AlgoDescription algo) {
		boolean foundAnything = false;
		boolean isInExtract = false;
		String currentStatement = "";
		for(int li=0;li<algo.next100SQLLines.size();li++) {
			String line = algo.next100SQLLines.get(li);
			if(line.startsWith("--")) {
				currentStatement = "";
				continue;
			}
			if(currentStatement.length() > 0) {
				currentStatement += " ";
			}
			currentStatement += line;
			boolean shouldDebug = false;
			/*
			if(currentStatement.toLowerCase().indexOf("update baserateoutputwithfuel, starttempadjustment, pollutantprocessmappedmodelyear") >= 0) {
				shouldDebug = true;
				System.out.println("algo.next100SQLLines.size()="+algo.next100SQLLines.size());
			}
			*/
			if(currentStatement.endsWith(";")) {
				SQLNode node = new SQLNode(null,currentStatement);
				if(shouldDebug) {
					System.out.println(currentStatement);
				}
				for(Iterator<IResource> outputs = node.getOutputs();outputs.hasNext();) {
					String tableName = outputs.next().getResourcePath();
					if(tableName.startsWith("|file|##") && tableName.endsWith("##|")) {
						tableName = tableName.substring(8,tableName.length()-3);
					}
					if(shouldDebug) {
						System.out.println("output: \"" + tableName + "\"");
					}
					// Format: |sql|mytablename|
					if(tableName.startsWith("|sql|") && tableName.endsWith("|")) {
						if(tableName.equals("|sql||") || tableName.equals("|sql|")) {
							tableName = "";
						} else {
							tableName = tableName.substring(5,tableName.length()-1);
						}
					}
					if(tableName.length() > 0) {
						algo.outputs.add(tableName);
						foundAnything = true;
					}
				}
				for(Iterator<IResource> inputs = node.getInputs();inputs.hasNext();) {
					String tableName = inputs.next().getResourcePath();
					if(shouldDebug) {
						System.out.println("input: \"" + tableName + "\"");
					}
					// Format: |sql|mytablename|
					if(tableName.startsWith("|sql|") && tableName.endsWith("|")) {
						if(tableName.equals("|sql||") || tableName.equals("|sql|")) {
							tableName = "";
						} else {
							tableName = tableName.substring(5,tableName.length()-1);
						}
					}
					if(tableName.length() > 0) {
						if(!algo.outputs.contains(tableName)) {
							algo.inputs.add(tableName);
							foundAnything = true;
						}
					}
				}
				currentStatement = "";
			}
		}
		return foundAnything;
	}

	/**
	 * Infer data from a calculator SQL file.
	 * @param file file to be examined.
	 * @param lines trimmed text lines.
	 * @param algo object to hold inferred results.
	 * @return true if anything was inferred.
	**/
	private boolean inferFromSQL(File file,ArrayList<String> lines,AlgoDescription algo) {
		boolean foundAnything = false;
		boolean isInExtract = false;
		String currentStatement = "";
		for(int li=0;li<lines.size();li++) {
			String line = lines.get(li);
			if(line.startsWith("--")) { // If it is a comment line
				if(line.startsWith("-- Section Extract Data")) {
					isInExtract = true;
				} else if(line.startsWith("-- End Section Extract Data")) {
					isInExtract = false;
				}
			} else {
				if(currentStatement.length() > 0) {
					currentStatement += " ";
				}
				currentStatement += line;
				if(currentStatement.endsWith(";")) {
					if(isInExtract) {
						SQLNode node = new SQLNode(null,currentStatement);
						for(Iterator<IResource> inputs = node.getInputs();inputs.hasNext();) {
							String tableName = inputs.next().getResourcePath();
							// Format: |sql|mytablename|
							if(tableName.startsWith("|sql|") && tableName.endsWith("|")) {
								if(tableName.equals("|sql||") || tableName.equals("|sql|")) {
									tableName = "";
								} else {
									tableName = tableName.substring(5,tableName.length()-1);
								}
							}
							if(tableName.length() > 0) {
								algo.inputs.add(tableName);
								foundAnything = true;
							}
						}
					}
					currentStatement = "";
				}
			}
		}
		return foundAnything;
	}

	/**
	 * Infer data from a calculator or generator Java file.
	 * @param file file to be examined.
	 * @param lines trimmed text lines.
	 * @param algo object to hold inferred results.
	 * @return true if anything was inferred.
	**/
	private boolean inferFromJava(File file,ArrayList<String> lines,AlgoDescription algo) {
		boolean foundAnything = false;
		String[] granularities = {
			"MasterLoopGranularity.HOUR", "HOUR",
			"MasterLoopGranularity.DAY", "DAY",
			"MasterLoopGranularity.MONTH", "MONTH",
			"MasterLoopGranularity.YEAR", "YEAR",
			"MasterLoopGranularity.LINK", "LINK",
			"MasterLoopGranularity.ZONE", "ZONE",
			"MasterLoopGranularity.COUNTY", "COUNTY",
			"MasterLoopGranularity.STATE", "STATE",
			"MasterLoopGranularity.PROCESS", "PROCESS"
		};
		for(int li=0;li<lines.size();li++) {
			String line = lines.get(li);
			if(algo.signup.length() <= 0 && line.indexOf("MasterLoopGranularity.") >= 0) {
				for(int i=0;i<granularities.length;i+=2) {
					if(line.indexOf(granularities[i]) >= 0) {
						algo.signup = granularities[i+1];
						foundAnything = true;
						//System.out.println(file.getName() + " @ " + algo.signup);
						break;
					}
				}
			}
			// TODO
		}
		return foundAnything;
	}

	/**
	 * Generate the output document.
	**/
	private void oldWriteToRTF() {
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(htmlFile),128*1024));
			writer.println("{\\rtf1\\ansi Messages\\par\\par");

			ArrayList<String> usedOwner = new ArrayList<String>();

			for(Iterator<AlgoDescription> ii=algos.iterator();ii.hasNext();) {
				TreeMapIgnoreCase sortedAlgos = new TreeMapIgnoreCase();
				AlgoDescription algoPrime = (AlgoDescription)ii.next();
				// find first owner
			    boolean hasBeenUsed=false;
				for (String curVal : usedOwner){
					if (curVal.equalsIgnoreCase(algoPrime.owner)==true){
						hasBeenUsed=true;
					}
				}
				if( hasBeenUsed==false ){
					usedOwner.add( algoPrime.owner );
					// copy all matching owners - indexed by step.stepIndex
					for(Iterator<AlgoDescription> jj=algos.iterator();jj.hasNext();) {
						AlgoDescription nextAlgo = (AlgoDescription)jj.next();
						if( algoPrime.owner.equalsIgnoreCase(nextAlgo.owner)==true ){
							int stepIndex=nextAlgo.stepIndex;
							String stepValue = Integer.toString(stepIndex);
							sortedAlgos.put(stepValue,nextAlgo);
						}
					}

					// owner
					writer.println("\\pard\\li720\\b0\\i0 owner: " + algoPrime.owner + "\\par");

					Set<String> keys = sortedAlgos.keySet();
					// algo descriptions
					writer.println("\\i Algo: \\par");
					for(Iterator<String> jj=keys.iterator();jj.hasNext();) {
						String messageText = (String)jj.next();
						AlgoDescription nextAlgo = (AlgoDescription)sortedAlgos.get(messageText);
						writer.println("\\pard\\li720\\b0\\i0 \\par");
						writer.println("\\pard\\li720\\b0\\i0 " + nextAlgo.algorithm + "\\par");
					}
					// inputs
					writer.println("\\i Inputs: \\par");
					for(Iterator<String> jj=keys.iterator();jj.hasNext();) {
						String messageText = (String)jj.next();
						AlgoDescription nextAlgo = (AlgoDescription)sortedAlgos.get(messageText);
						for(Iterator<String> kk=nextAlgo.inputs.iterator();kk.hasNext();) {
							String text = (String)kk.next();
							if( text.length()>0 ){
								writer.println("\\pard\\li720\\b0\\i0" + text + "\\par");
							}
						}
					}
					// outputs
					writer.println("\\i Outputs: \\par");
					for(Iterator<String> jj=keys.iterator();jj.hasNext();) {
						String messageText = (String)jj.next();
						AlgoDescription nextAlgo = (AlgoDescription)sortedAlgos.get(messageText);
						//if( nextAlgo.output.length()>0 ){
						//	writer.println("\\pard\\li720\\b0\\i0 output: " + nextAlgo.output + "\\par");
						//}
					}

					// files
					writer.println("\\i Files: \\par");
					ArrayList<String> usedFileName = new ArrayList<String>();
					for(Iterator<String> jj=keys.iterator();jj.hasNext();) {
						String messageText = (String)jj.next();
						AlgoDescription nextAlgo = (AlgoDescription)sortedAlgos.get(messageText);
						if( nextAlgo.filePath.length()>0 ){
						    boolean hasUsed=false;
							for (String curVal : usedFileName){ if (curVal.equalsIgnoreCase(nextAlgo.filePath)==true){ hasUsed=true; } }
							if( hasUsed==false ){
								writer.println("\\ "+nextAlgo.filePath+" \\par");
								usedFileName.add( nextAlgo.filePath );
								}
							}
						}
					writer.println("\\pard\\par");
				}
			}
			writer.println("\\pard\\par");
			writer.println("}");
		} catch(Exception e) {
			/**
			 * @explain The output RTF file was removed or edited by a user or virus scanner
			 * before MOVES was finished with it.
			**/
			System.out.println("ERROR: Unable to write to RTF: " + e.toString());
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
	 * Write a title then a comma-separated list of text, using a hanging indent.
	 * @param writer file to be populated.
	 * @param title text to put at the head of the list. Does not include a colon or separator space.
	 * @param list ordered list of textual items
	 * @throws IOException if something goes wrong.
	**/
	private void writeOneLineList(PrintWriter writer, String title, ArrayList<String> list) throws IOException {
		if(list.size() > 0) {
			writer.println("" + title + ": ");
			boolean isFirst = true;
			for(String t : list) {
				if(!isFirst) {
					writer.print(", ");
				}
				DatabaseTable table = defaultTables.get(t.toUpperCase());
				if(table != null) {
					writer.print("<a href=\"#" + table.htmlTagName + "\">" + t + "</a>");
				} else {
					writer.print(t);
				}
				isFirst = false;
			}
			writer.println("<br>");
		}
	}

	/**
	 * Write a title then a comma-separated list of text, using a hanging indent.
	 * @param writer file to be populated.
	 * @param title text to put at the head of the list. Does not include a colon or separator space.
	 * @param set set of textual items
	 * @throws IOException if something goes wrong.
	**/
	private void writeOneLineList(PrintWriter writer, String title, TreeSetIgnoreCase set) throws IOException {
		if(set.size() > 0) {
			writer.println("" + title + ": ");
			boolean isFirst = true;
			for(String t : set) {
				if(!isFirst) {
					writer.print(", ");
				}
				DatabaseTable table = defaultTables.get(t.toUpperCase());
				if(table != null) {
					writer.print("<a href=\"#" + table.htmlTagName + "\">" + t + "</a>");
				} else {
					writer.print(t);
				}
				isFirst = false;
			}
			writer.println("<br>");
		}
	}

	/**
	 * Create documentation for an owner.
	 * @param writer file to be populated.
	 * @param owner AlgorithmOwner to be documented.
	 * @throws IOException if something goes wrong.
	**/
	private void writeOwner(PrintWriter writer, AlgorithmOwner owner) throws IOException {
		/*
		<OWNER> title page

		WidgetCalculator is a CALCULATOR that executes at the YEAR level.

		The WidgetCalculator is responsible for:
		- Running Exhaust (1), Total Gaseous Hydrocarbons (1)
		- Start Exhaust (2), Total Gaseous Hydrocarbons (1)
		- Running Exhaust (1), Volatile Organic Compounds (87)

		Effects
		WidgetCalculator applies the following effects:
		| Process | Pollutant | Effect |
		| Running Exhaust (1) | Total Gaseous Hydrocarbons(1) | Humidity Effects |
		| Running Exhaust (1) | Total Gaseous Hydrocarbons(1) | Air Conditioning |
		...

		Inputs
		WidgetCalculator uses the following tables:
		- AvgSpeedBin
		- RoadTypeOpModeDistribution
		- ZoneMonthHour
		(these are from the @input entries limited to only the tables found in the default database)

		Files
		WidgetCalculator's algorithms are expressed in these source code files:
		- database/abc.sql
		- database/def.sql
		- gov/blah/blah/WidgetCalculator.java

		Steps
		The WidgetCalculator algorithm is as follows.

		Step: Summarize the widget allocation table.
		   Inputs: abc, def, ghi
		   Outputs: xyz
		   Conditions: Only for passenger cars.  All others get a fraction of 1.0.

		Step: ......
		*/

		writer.println("<hr><h1><a name=\"" + owner.htmlTagName + "\"></a>" + owner.owner + "</h1>");
		if(owner.signup.length() <= 0 && owner.loopableType.toLowerCase().indexOf("calculator") >= 0) {
			writer.println("" + owner.owner + " is a CHAINED " + owner.loopableType + ".<br>");
		} else {
			writer.println("" + owner.owner + " is a " + owner.loopableType + " that executes at the " + owner.signup + " level.<br>");
		}

		// write out registrations
		if( registrations.size()>0 ){
			boolean foundFirst = false;
			for( Registration r:registrations ){
				if(r.moduleName.equalsIgnoreCase(owner.owner)){
					if(foundFirst == false){
						// write title
						writer.println("<br>The "+owner.owner +" is responsible for:<br><ul>");
						foundFirst = true;
					}
					if( foundFirst == true ){
						writer.println("<li>"+r.processName+" ("+r.processID+") "+r.outputPollutantName+" ("+r.outputPollutantID+")</li>");
					}
				}
			}
			if( foundFirst == true ){
				writer.println("</ul>");
			}
		}

		// ignore Subscribe for now
		//

		// chains: write all outputModuleName	then inputModuleName
		if( chains.size()>0 ){
			boolean foundFirst = false;
			for( Chain c:chains ){
				if(c.outputModuleName.equalsIgnoreCase(owner.owner)
						|| c.outputModuleName.equalsIgnoreCase(owner.moduleName)){
					if(foundFirst == false){
						// write title
						writer.println("<br>The "+owner.owner +" input comes from:<br><ul>");
						foundFirst = true;
					}
					if( foundFirst == true ){
						AlgorithmOwner algoOwner = findOwner(c.inputModuleName);
						if(algoOwner!=null) {
							writer.println("<li><a href=\"#" + algoOwner.htmlTagName + "\">" + algoOwner.owner + "</a></li>");
						} else {
							writer.println("<li>" + c.inputModuleName + "</li>");
						}
					}
				}
			}
			if( foundFirst == true ){
				writer.println("</ul>");
			}
			foundFirst = false;
			for( Chain c:chains ){
				if(c.inputModuleName.equalsIgnoreCase(owner.owner)
						|| c.inputModuleName.equalsIgnoreCase(owner.moduleName)){
					if(foundFirst == false){
						// write title
						writer.println("<br>The "+owner.owner +" output is used by:<br><ul>");
						foundFirst = true;
					}
					if( foundFirst == true ){
						AlgorithmOwner algoOwner = findOwner(c.outputModuleName);
						if(algoOwner!=null) {
							writer.println("<li><a href=\"#" + algoOwner.htmlTagName + "\">" + algoOwner.owner + "</a></li>");
						} else {
							writer.println("<li>" + c.outputModuleName + "</li>");
						}
					}
				}
			}
			if( foundFirst == true ){
				writer.println("</ul>");
			}
		}

		// Inputs
		if(owner.inputs.size() > 0) {
			writer.println("<br>" + owner.owner + " uses the following tables:");
			writer.println("<ul>");
			for(String tableName : owner.inputs) {
				DatabaseTable table = defaultTables.get(tableName.toUpperCase());
				if(table != null) {
					writer.println("<li><a href=\"#" + table.htmlTagName + "\">" + tableName + "</a></li>");
				} else {
					writer.println("<li>" + tableName + "</li>");
				}
			}
			writer.println("</ul>");
		}
		// Outputs (for Generators)
		if(owner.loopableType.toLowerCase().indexOf("generator") >= 0 && owner.outputs.size() > 0) {
			writer.println("<br>" + owner.owner + " populates the following tables:");
			writer.println("<ul>");
			for(String tableName : owner.outputs) {
				DatabaseTable table = defaultTables.get(tableName.toUpperCase());
				if(table != null) {
					writer.println("<li><a href=\"#" + table.htmlTagName + "\">" + tableName + "</a></li>");
				} else {
					writer.println("<li>" + tableName + "</li>");
				}
			}
			writer.println("</ul>");
		}
		// Files
		if(owner.fileNames.size() > 0) {
			writer.println("<br>" + owner.owner + " is expressed in these source code files:");
			writer.println("<ul>");
			for(String fileName : owner.fileNames) {
				writer.println("<li>" + fileName + "</li>");
			}
			writer.println("</ul>");
		}
		// Steps
		if(owner.hasPrintableSteps && owner.steps.size() > 0) {
			writer.println("<h2>Steps</h2>");
			writer.println("The " + owner.owner + " algorithm is as follows.<br>");
			Set<String> stepNames = owner.steps.keySet();
			for(String stepName : stepNames) {
				AlgoDescription step = owner.steps.get(stepName);
				if(step == null || step.call.length() > 0 || step.algorithm.length() <= 0) {
					// Don't print steps that just call other steps.
					// Don't print steps that have no description.
					continue;
				}
				// Print the step description.
				writer.println("<br>Step: " + step.algorithm + "<br>");
				// Print the list of conditions.
				writeOneLineList(writer,"Conditions",step.conditions);
				// Print the list of input tables in the step
				writeOneLineList(writer,"Inputs",step.inputs);
				// Print the list of output tables in the step
				writeOneLineList(writer,"Outputs",step.outputs);
			}
		}
	}

	/**
	 * Write chapters for each owner.
	 * @param writer file to be populated.
	 * @throws IOException if anything goes wrong.
	**/
	private void writeOwners(PrintWriter writer) throws IOException {
		Set<String> ownerNames = owners.keySet();
		for(String ownerName : ownerNames) {
			AlgorithmOwner owner = owners.get(ownerName);
			if(!owner.isTopLevel || !owner.isUsed) {
				continue;
			}
			writeOwner(writer,owner);
		}
	}

	/**
	 * Write a chapter with the default database cross reference.
	 * @param writer file to be populated.
	 * @throws IOException if anything goes wrong.
	**/
	private void writeDefaultDatabase(PrintWriter writer) throws IOException {
		Set<String> tableNames = defaultTables.keySet();
		for(String tableName : tableNames) {
			DatabaseTable table = defaultTables.get(tableName);
			writer.println("<hr><h1><a name=\"" + table.htmlTagName + "\"></a>" + table.name + " table</h1>");
			if(table.createTableStatement.length() > 0) {
				writer.println("<pre>");
				writer.println(table.createTableStatement);
				writer.println("</pre><br>");
			}
			if(table.users.size() > 0) {
				writer.println("The " + table.name + " is used by these modules:");
				writer.println("<ul>");
				Set<String> ownerNames = table.users.keySet();
				for(String ownerName : ownerNames) {
					AlgorithmOwner owner = table.users.get(ownerName);
					if(!owner.isUsed) {
						continue;
					}
					writer.println("<li><a href=\"#" + owner.htmlTagName + "\">" + owner.owner + "</a></li>");
				}
				writer.println("</ul>");
			}
		}
	}

	/**
	 * Create a table of contents.
	 * @param writer file to be populated.
	 * @throws IOException if anything goes wrong.
	**/
	private void writeContents(PrintWriter writer) throws IOException {
		writer.println("<h1>Contents</h1>");
		writer.println("<table><tr><th>Modules</th><th>Database tables</th></tr><tr><td valign=\"top\"><ul>");

		Set<String> ownerNames = owners.keySet();
		for(String ownerName : ownerNames) {
			AlgorithmOwner owner = owners.get(ownerName);
			if(!owner.isTopLevel || !owner.isUsed) {
				continue;
			}
			writer.println("<li><a href=\"#" + owner.htmlTagName + "\">" + owner.owner + "</a></li>");
		}

		writer.println("</ul></td><td valign=\"top\"><ul>");

		Set<String> tableNames = defaultTables.keySet();
		for(String tableName : tableNames) {
			DatabaseTable table = defaultTables.get(tableName);
			writer.println("<li><a href=\"#" + table.htmlTagName + "\">" + table.name + "</a></li>");
		}

		writer.println("</ul></td></tr></table>");
	}

	/** Assign HTML tags to all owners and tables. **/
	private void generateHTMLTags() {
		Set<String> tableNames = defaultTables.keySet();
		for(String tableName : tableNames) {
			DatabaseTable table = defaultTables.get(tableName);
			table.htmlTagName = "table" + tableName.toLowerCase().replace(' ','-');
		}

		Set<String> ownerNames = owners.keySet();
		for(String ownerName : ownerNames) {
			AlgorithmOwner owner = owners.get(ownerName);
			owner.htmlTagName = "module" + ownerName.toLowerCase().replace(' ','-');
		}
	}

	/**
	 * Generate the output document.
	**/
	private void writeToHTML() {
		generateHTMLTags();

		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(htmlFile),128*1024));

			writer.println("<html>");
			writer.println("	<head>");
			writer.println("		<title>MOVES Algorithms</title>");
			writer.println("	</head>");
			writer.println("<b>MOVES Algorithms Reference</b><br>");

			// Make the table of contents
			writeContents(writer);

			// Write all owners
			writeOwners(writer);

			// Write the default database cross reference
			writeDefaultDatabase(writer);

			// Write Registrations
			writeRegistrations(writer);

			//writeDebug(writer);

			// Done
			writer.println("</html>");
		} catch(Exception e) {
			/**
			 * @explain The output HTML file was removed or edited by a user or virus scanner
			 * before MOVES was finished with it.
			**/
			System.out.println("ERROR: Unable to write to HTML: " + e.toString());
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
	 * print out debugging information.
	 * @param writer file to be populated.
	 * @throws IOException if anything goes wrong.
	**/
	private void writeDebug(PrintWriter writer) throws IOException {
		// write title
		writer.println("<hr><h1>Debug Info</h1>");
		writer.println("<table><tr><th>htmlTagName</th><th>owner</th><th>moduleName</th><th>Files</th></tr>");
		Set<String> ownerNames = owners.keySet();
		for(String ownerName : ownerNames) {
			AlgorithmOwner owner = owners.get(ownerName);
			writer.println("<tr><td>" + owner.htmlTagName + "</td><td>" + owner.owner + "</td><td>" + owner.moduleName + "</td><td>");
			boolean isFirst = true;
			for(String fileName : owner.fileNames) {
				if(!isFirst) {
					writer.println("<br>");
				}
				isFirst = false;
				writer.println(fileName);
			}
			writer.println("</td></tr>");
		}
		writer.println("</table><br>");
	}

	/**
	 * print out all Registrations in a big table
	 * @param writer file to be populated.
	 * @throws IOException if anything goes wrong.
	**/
	private void writeRegistrations(PrintWriter writer) throws IOException {
		if( registrations.size() <= 0 ){
			return;
		}
		// write title
		writer.println("<hr><h1>Pollutants and Modules Cross Reference</h1>");
		writer.println("<table><tr><th>Pollutant</th><th>Process</th><th>Module</th></tr>");
		// write out registrations
		TreeSet<String> registrationsSeen = new TreeSet<String>();
		int keyCounter = 0;
		for( Registration r:registrations ){
			keyCounter++;
			String key = r.outputPollutantName + "|" + r.outputPollutantID
					+ "|" + r.processName + "|" + r.processID;
			if(registrationsSeen.contains(key)) {
				continue;
			}
			registrationsSeen.add(key);

			writer.println("<tr><td>"+r.outputPollutantName+" (" + r.outputPollutantID + ")</td>"
					+ "<td>"+r.processName+" (" + r.processID + ")</td><td>");

			String links = "";
			TreeSet<String> inputOwnerNames = new TreeSet<String>();

			for(Registration r2 : registrations) {
				String key2 = r2.outputPollutantName + "|" + r2.outputPollutantID
						+ "|" + r2.processName + "|" + r2.processID;
				if(!key2.equalsIgnoreCase(key)) {
					continue;
				}

				AlgorithmOwner algoOwner = findOwner(r2.moduleName);
				String link = "";
				if(algoOwner!=null) {
					findAllInputs(inputOwnerNames,algoOwner);

					link = "<a href=\"#" + algoOwner.htmlTagName + "\">" + algoOwner.owner + "</a>";
				} else {
					link = r2.moduleName;
				}
				if(links.length() > 0) {
					links += " ";
				}
				links += link;
			}

			writer.println(links + "</td>");
			/*
			writer.println("<td>");

			// Find all input tables
			for(String t : inputOwnerNames) {
				System.out.println("Input to " + key + " [" + keyCounter + "] from " + t);
			}
			TreeSet<String> inputTables = new TreeSet<String>();
			for(String ownerName : inputOwnerNames) {
				AlgorithmOwner algoOwner = findOwner(ownerName);
				if(algoOwner != null && algoOwner.inputs.size() > 0) {
					for(String tableName : algoOwner.inputs) {
						inputTables.add(tableName.toUpperCase());
					}
				}
			}
			String tableNames = "";
			for(String tableName : inputTables) {
				DatabaseTable table = defaultTables.get(tableName.toUpperCase());
				String link = "";
				if(table != null) {
					link = "<a href=\"#" + table.htmlTagName + "\">" + tableName + "</a>";
				} else {
					link = tableName;
				}
				if(tableNames.length() > 0) {
					tableNames += ", ";
				}
				tableNames += link;
			}

			writer.println(tableNames + "</td>");
			*/
			writer.println("</tr>");
		}
		writer.println("</table><br>");
	}

	/**
	 * Locate an AlgorithmOwner by its human-readable name or by its module name.
	 * @param nameOrModule human-readable name or module name
	 * @return an AlgorithmOwner object or null if no match is found
	**/
	AlgorithmOwner findOwner(String nameOrModule) {
		AlgorithmOwner algoOwner = owners.get(nameOrModule);
		if(algoOwner==null) {
			Set<String> ownerNames = owners.keySet();
			for(String ownerName : ownerNames) {
				AlgorithmOwner owner = owners.get(ownerName);
				if(owner.moduleName.equalsIgnoreCase(nameOrModule)) {
					algoOwner = owner;
					break;
				}
			}
		}
		return algoOwner;
	}

	/**
	 * Locate an AlgorithmOwner by its human-readable name, by its module name, or by one of its file names.
	 * @param nameOrModule human-readable name or module name
	 * @return an AlgorithmOwner object or null if no match is found
	**/
	AlgorithmOwner findOwnerExtensive(String nameOrModule) {
		AlgorithmOwner algoOwner = owners.get(nameOrModule);
		if(algoOwner==null) {
			Set<String> ownerNames = owners.keySet();
			for(String ownerName : ownerNames) {
				AlgorithmOwner owner = owners.get(ownerName);
				if(owner.moduleName.equalsIgnoreCase(nameOrModule)) {
					algoOwner = owner;
					break;
				}
			}
		}
		if(algoOwner == null) {
			String lcNameOrModule = nameOrModule.toLowerCase();
			Set<String> ownerNames = owners.keySet();
			for(String ownerName : ownerNames) {
				AlgorithmOwner owner = owners.get(ownerName);
				for(String fileName : owner.fileNames) {
					fileName = fileName.toLowerCase();
					if(fileName.endsWith("/" + lcNameOrModule + ".java")
							|| fileName.endsWith("/" + lcNameOrModule + ".sql")) {
						algoOwner = owner;
						break;
					}
				}
			}
		}
		return algoOwner;
	}

	/**
	 * Obtain a set of all modules that provide input in any way to a given module.
	 * @param inputOwnerNames result set of names. Some may not resolve to AlgorithmOwner objects.
	 * @param owner target module
	**/
	void findAllInputs(TreeSet<String> inputOwnerNames, AlgorithmOwner owner) {
		inputOwnerNames.add(owner.owner);
		for( Chain c:chains ){
			if(c.outputModuleName.equalsIgnoreCase(owner.owner)
					|| c.outputModuleName.equalsIgnoreCase(owner.moduleName)){
				AlgorithmOwner algoOwner = findOwner(c.inputModuleName);
				if(algoOwner!=null) {
					if(!inputOwnerNames.contains(algoOwner.owner)) {
						findAllInputs(inputOwnerNames,algoOwner);
					}
				} else {
					inputOwnerNames.add(c.inputModuleName);
				}
			}
		}
	}

	/**
	 * populate information from "CalulatorInfo.txt" file into arrays
	 * @param file file to be examined
	**/
	private void loadCalulatorFacts(File file) {
		try{
			ArrayList<String> lines = readTrimmedLines(file);
			if(lines == null || lines.size() <= 0) {
				return;
			}
			for(int i=0;i<lines.size();i++) {
				String line = (String)lines.get(i);
				// look for Registration as the first column
				if( line.startsWith("Registration")) {
					ArrayList<String> parts = StringUtilities.splitCSVremovingQuotes(line,'\t');
					if(parts == null || parts.size() < 6) {
						break;
					}
					Registration reg = new Registration();
					reg.outputPollutantName = (String)parts.get(1);
					reg.outputPollutantID	= (String)parts.get(2);
					reg.processName = (String)parts.get(3);
					reg.processID	= (String)parts.get(4);
					reg.moduleName = (String)parts.get(5);

					registrations.add(reg);
				}
				if( line.startsWith("Subscribe")) {
					ArrayList<String> parts = StringUtilities.splitCSVremovingQuotes(line,'\t');
					if(parts == null || parts.size() < 6) {
						break;
					}
					Subscribe sub = new Subscribe();
					sub.moduleName = (String)parts.get(1);
					sub.processID	= (String)parts.get(2);
					sub.processName = (String)parts.get(3);
					sub.granularity = (String)parts.get(4);
					sub.priority = (String)parts.get(5);

					subscribes.add(sub);
				}
				if( line.startsWith("Chain")) {
					ArrayList<String> parts = StringUtilities.splitCSVremovingQuotes(line,'\t');
					if(parts == null || parts.size() < 3) {
						break;
					}
					Chain cha = new Chain();
					cha.outputModuleName = (String)parts.get(1);
					cha.inputModuleName = (String)parts.get(2);

					chains.add(cha);
				}
			}
		}catch(Exception e){
			System.out.println("ERROR: Unable to load calculator details from " + file.getName());
			System.out.println(e.toString());
		}
	}
}
