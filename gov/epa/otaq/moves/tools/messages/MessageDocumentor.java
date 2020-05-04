/**************************************************************************************************
 * @(#)MesageDocumentor.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.tools.messages;

import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;

/**
 * Find error messages and documentation within Java Code.
 * Error messages can be documented using Java-Doc style opening and closing comment markers
 * (slash-star-star and star-star-slash respectively) and new @-style tags.  These comments should
 * be placed within a few lines preceding Logger, JDialog, or System.out calls.  The available
 * tags are:<br>
 * &#64;nonissue - used to denote the next message (Logger, JDialog, System.out) is not a
 * documentable issue.<br>
 * &#64;issue <summary line> - used to explicitly give the summary line of an issue.  This is the
 * text a user will likely search for within the documentation.  Messages without an @issue tag
 * are parsed to infer the summary line.<br>
 * &#64;explain <lines of text> - gives the explanation and possible mitigation steps for the issue.
 * This line is mandatory for each issue.<br>
 * &#64;recurs - denotes the explanation as being applicable to all highly similar messages in the
 * system even if those messages don't have explicit explanation blocks.<br>
 *
 * @author		Wesley Faler
 * @version		2009-04-20
**/
public class MessageDocumentor {
	/**
	 * Entry point.  The first argument is the name of the RTF file to be generated.
	 * All remaining arguments are the names of folders to be scanned.
	 * No document is generated if a document is not named or if no folders are named.
	 * @param args The first argument is the name of the RTF file to be generated.
	 * All remaining arguments are the names of folders to be scanned.
	**/
	public static void main(String[] args) {
		if(args.length < 1) {
			System.out.println("ERROR: No output RTF file name given.");
			return;
		}
		if(args.length < 2) {
			System.out.println("ERROR: No folder to be scanned given.");
			return;
		}
		MessageDocumentor d = new MessageDocumentor(args);
		d.go();
	}

	public static class SiteCore {
		public File file;
		public int startLineNumber0;
		public int endLineNumber0;
	}

	public static class Site extends SiteCore {
		public String signature = "";
		public ArrayList<String> lines = new ArrayList<String>();
		public Issue issue = null;
		public boolean isSingleStatement = true;

		public void calculateSignature() {
			signature = "";
			for(int i=0;i<lines.size();i++) {
				String line = (String)lines.get(i);
				signature += line.replaceAll("\\s","");
			}
		}

		/**
		 * Get the whole text from the first quotation mark to the last one.
		 * Example: Logger.blah(abc,"This is a " + t + " test.");
		 * We'll change the above to:  "This is a [*] test." and use it
		 * as the messageText for the issue.
		 * @return generic text found between quotation marks, or null if no quoted text was found.
		**/
		public String makeGenericQuotedData() {
			// Merge all the lines
			String fullText = "";
			for(Iterator i=lines.iterator();i.hasNext();) {
				fullText += (String)i.next();
			}
			int index = fullText.indexOf("\"");
			if(index < 0) {
				return null;
			}
			// Remove the following sequences, as these are only used for line continuation:
			// 		"+ "
			// 		" +"
			// 		" + "
			// 		"+"
			fullText = StringUtilities.replace(fullText,"\"+ \"","");
			fullText = StringUtilities.replace(fullText,"\" +\"","");
			fullText = StringUtilities.replace(fullText,"\" + \"","");
			fullText = StringUtilities.replace(fullText,"\"+\"","");

			index = fullText.indexOf("\"");
			int endIndex = fullText.lastIndexOf("\"");
			char[] chars = fullText.substring(index+1,endIndex).toCharArray();
			String result = "";
			boolean inQuotes = true;
			for(int i=0;i<chars.length;i++) {
				if(chars[i] == '\"' && chars[i-1] != '\\') {
					if(inQuotes) {
						result += "[*]";
					}
					inQuotes = !inQuotes;
				} else if(inQuotes) {
					result += chars[i];
				}
			}
			return result;
		}
	}

	public static class Issue extends SiteCore {
		public boolean isIssue = true;
		public boolean isRecurring = false;
		public String messageText = "";
		public String explanation = "";
		public Site firstSite = null;

		public boolean isOutput;
		public boolean isDuplicate;

		public void addSite(Site s) {
			if(firstSite == null) {
				firstSite = s;
			}
		}
	}

	/** RTF file to be created **/
	File rtfFile = null;
	/** List of File objects that are the folders to be scanned **/
	ArrayList<File> folders = new ArrayList<File>();
	/** List of Site objects in the order they were discovered **/
	ArrayList<Site> sites = new ArrayList<Site>();
	/** List of Issue objects in the order they were discovered **/
	ArrayList<Issue> issues = new ArrayList<Issue>();

	private MessageDocumentor(String[] args) {
		rtfFile = new File(args[0]);
		if(rtfFile.exists()) {
			FileUtilities.deleteFileWithRetry(rtfFile);
		}
		for(int i=1;i<args.length;i++) {
			File f = new File(args[i]);
			if(f.exists()) {
				folders.add(f);
			}
		}
	}

	/** Scan folders and generate the RTF file **/
	private void go() {
		if(folders.size() <= 0) {
			System.out.println("ERROR: No valid folder found to scan.");
			return;
		}
		// Scan all required folders and subfolders
		for(Iterator<File> i=folders.iterator();i.hasNext();) {
			File f = (File)i.next();
			try {
				scanFolder(f);
			} catch(IOException e) {
				System.out.println("ERROR: Unable to scan folder " + f.getName());
				return;
			}
		}
		connectIssuesAndSites();
		fillIssueMessageText();
		sortAndWriteIssuesToRTF();
		complain();
		try {
			System.out.println("Message documentation generated in: " + rtfFile.getCanonicalPath());
		} catch(Exception e) {
			// Nothing to be done here
		}
	}

	/**
	 * Scan a folder's files and subfolders.
	 * @param folder folder to be examined
	 * @throws IOException if anything goes wrong
	**/
	private void scanFolder(File folder) throws IOException {
		File[] children = folder.listFiles();
		if(children == null || children.length <= 0) {
			return;
		}

		// Scan files first
		for(int i=0;i<children.length;i++) {
			if(!children[i].isDirectory()) {
				scanFile(children[i]);
			}
		}
		
		// Scan child folders after files
		for(int i=0;i<children.length;i++) {
			if(children[i].isDirectory()) {
				scanFolder(children[i]);
			}
		}
	}

	/**
	 * Scan a file for message sites and explanations.
	 * @param file file to be examined
	 * @throws IOException if anything goes wrong
	**/
	private void scanFile(File file) throws IOException {
		String canonicalPath = file.getCanonicalPath();
		String lowerCasePath = canonicalPath.toLowerCase();
		if(lowerCasePath.endsWith("messagedocumentor.java")
				|| lowerCasePath.endsWith("test.java")
				|| !lowerCasePath.endsWith(".java")) {
			return;
		}
		ArrayList<String> lines = readTrimmedLines(file);
		if(lines == null || lines.size() <= 0) {
			return;
		}
		int nextAllowedSiteLine = 0;
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
				parseCommentBlock(file,lines,startLine,endLine);
				continue;
			}
			if(i < nextAllowedSiteLine) {
				continue;
			}
			// Skip false-positives for message sites
			if(line.startsWith("Logger.log(LogMessageCategory.INFO")) {
				continue;
			}
			// Look for message sites
			if(line.startsWith("Logger.") || line.startsWith("System.out.println")
					|| line.startsWith("JOptionPane.showMessageDialog")) {
				i = parseSiteToSemiColon(file,lines,i);
				nextAllowedSiteLine = i + 4;
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
	 * Scan a Java-Doc style comment block for our tags, generating an Issue object as needed.
	 * @param file file being processed
	 * @param lines all lines within the file
	 * @param startLine 0-based index within lines to the opening line of the comment block
	 * @param endLine 0-based index within lines to the closing line of the comment block
	**/
	private void parseCommentBlock(File file, ArrayList<String> lines, int startLine, int endLine) {
		boolean hasOurTags = false;
		for(int i=startLine;i<=endLine;i++) {
			String line = (String)lines.get(i);
			if(line.indexOf("@author") >= 0 || line.indexOf("@version") >= 0 
					|| line.indexOf("@param") >= 0 || line.indexOf("@return") >= 0) {
				return; // Is a standard JavaDoc block
			}
			if(!hasOurTags && (line.indexOf("@nonissue") >= 0 || line.indexOf("@issue") >= 0
					|| line.indexOf("@explain") >= 0)) {
				hasOurTags = true;
			}
		}
		if(!hasOurTags) {
			return; // Is a JavaDoc block without information for us
		}
		Issue issue = new Issue();
		issue.file = file;
		issue.startLineNumber0 = startLine;
		issue.endLineNumber0 = endLine;
		issues.add(issue);

		for(int i=startLine;i<=endLine;i++) {
			String line = (String)lines.get(i);
			if(line.indexOf("@recur") >= 0) {
				issue.isRecurring = true;
				continue;
			} else if(line.indexOf("@nonissue") >= 0) {
				issue.isIssue = false;
				return;
			} else if(line.indexOf("@issue") >= 0) {
				int index = line.indexOf("@issue") + 6 + 1;
				issue.messageText = parseToNextCommand(lines,i,endLine,index);
			} else if(line.indexOf("@explain") >= 0) {
				int index = line.indexOf("@explain") + 8 + 1;
				issue.explanation = parseToNextCommand(lines,i,endLine,index);
			}
		}
		if(issue.explanation == null) {
			issue.explanation = "";
		}
		if(issue.explanation.length() <= 0) {
			try {
				/** @explain Each issue requires the explain tag to be used. **/
				System.out.println("ERROR: Issue without @explanation.  File "
						+ file.getCanonicalPath() + ", line " + (startLine+1));
			} catch(Exception e) {
				// Nothing to do here
			}
		}
	}

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
		String line = (String)lines.get(currentLine);
		String result = line.substring(startIndex).trim();
		if(result.endsWith("**/")) {
			result = result.substring(0,result.length()-3).trim();
			return result;
		}
		for(int i=currentLine+1;i<=endLine;i++) {
			line = (String)lines.get(i);
			if(line.indexOf("@") >= 0) {
				return result;
			}
			if(line.startsWith("*") && !line.startsWith("**/")) {
				result += " " + line.substring(1).trim();
			} else {
				result += " " + line;
			}
			if(result.endsWith("**/")) {
				result = result.substring(0,result.length()-3).trim();
				return result;
			}
		}
		return result;
	}

	/**
	 * Scan a Java statement looking for the line with the trailing semicolon.
	 * @param file file being processed
	 * @param lines all lines within the file
	 * @param startLine 0-based index within lines to the opening line of the comment block
	 * @return index of the line containing the final semicolon on the statement.
	**/
	private int parseSiteToSemiColon(File file, ArrayList<String> lines, int startLine) {
		Site site = new Site();
		site.file = file;
		site.startLineNumber0 = startLine;
		site.isSingleStatement = true;
		sites.add(site);
		for(int i=startLine;i<lines.size();i++) {
			String line = (String)lines.get(i);
			site.lines.add(line);
			if(line.indexOf(";") >= 0) {
				site.endLineNumber0 = i;
				site.calculateSignature();
				return i;
			}
		}
		return lines.size();
	}

	/** Connect, if possible, Issue and Site objects **/
	private void connectIssuesAndSites() {
		/*
			For each site,
				Look for a nearby issue, assigning as needed.
		*/
		for(Iterator<Site> si=sites.iterator();si.hasNext();) {
			Site site = (Site)si.next();
			for(Iterator<Issue> ii=issues.iterator();ii.hasNext();) {
				Issue issue = (Issue)ii.next();
				if(site.file == issue.file
						&& site.startLineNumber0 > issue.endLineNumber0
						&& site.startLineNumber0 <= issue.endLineNumber0 + 5) {
					site.issue = issue;
					issue.addSite(site);
					break;
				}
			}
		}
		/*
			For each site still without an issue
				Look for a recurring issue with a first site that has the same signature.
				If found, add the site to the issue and vice versa.
		*/
		for(Iterator<Site> si=sites.iterator();si.hasNext();) {
			Site site = (Site)si.next();
			if(site.issue != null) {
				continue;
			}
			for(Iterator<Issue> ii=issues.iterator();ii.hasNext();) {
				Issue issue = (Issue)ii.next();
				if(issue.isRecurring && issue.firstSite != null
						&& issue.firstSite.signature.equals(site.signature)) {
					site.issue = issue;
					issue.addSite(site);
					break;
				}
			}
		}
		// Create issues for database-centric sites without explicit issues
		for(Iterator<Site> si=sites.iterator();si.hasNext();) {
			Site site = (Site)si.next();
			if(site.issue == null && site.isSingleStatement
					&& site.signature.endsWith("sql);")) {
				Issue issue = new Issue();
				issue.startLineNumber0 = site.startLineNumber0;
				issue.endLineNumber0 = site.endLineNumber0;
				issue.explanation = "An error occurred while working with a database.  "
						+ "The database could have become unavailable or could have been "
						+ "modified externally from MOVES.";
				site.issue = issue;
				issue.addSite(site);
				issues.add(issue);
			}
		}
	}

	/** Attempt to generate messageText for issues that don't have it explicitly stated **/
	private void fillIssueMessageText() {
		for(Iterator<Issue> ii=issues.iterator();ii.hasNext();) {
			Issue issue = (Issue)ii.next();
			if((issue.messageText == null || issue.messageText.length() <= 0)
					&& issue.firstSite != null) {
				if(issue.firstSite.isSingleStatement) {
					String t = issue.firstSite.makeGenericQuotedData();
					if(t != null && t.length() > 0) {
						issue.messageText = t;
					}
				}
			}
		}
	}

	/** Sort issues alphabetically and write them to the RTF file **/
	private void sortAndWriteIssuesToRTF() {
		TreeMapIgnoreCase sortedIssues = new TreeMapIgnoreCase();
		for(Iterator<Issue> ii=issues.iterator();ii.hasNext();) {
			Issue issue = (Issue)ii.next();
			if(issue.isIssue) {
				if(issue.messageText != null && issue.messageText.length() > 0) {
					if(issue.messageText.length() < 5) {
						issue.isIssue = false;
					}
				}
			}
			if(issue.isIssue) {
				if(issue.messageText != null && issue.messageText.length() > 0) {
					if(sortedIssues.containsKey(issue.messageText)) {
						issue.isDuplicate = true;
					} else {
						issue.isOutput = true;
						sortedIssues.put(issue.messageText,issue);
					}
				}
			}
		}

		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(rtfFile),128*1024));
			writer.println("{\\rtf1\\ansi Messages\\par\\par");

			Set<String> keys = sortedIssues.keySet();
			for(Iterator<String> ii=keys.iterator();ii.hasNext();) {
				String messageText = (String)ii.next();
				Issue issue = (Issue)sortedIssues.get(messageText);
				writer.println("\\b\\i " + messageText + "\\par");
				writer.println("\\pard\\li720\\b0\\i0 " + issue.explanation + "\\par");
				writer.println("\\pard\\par");
			}

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

	/** Print any compalints about sites without issues and issues that weren't used. **/	
	private void complain() {
		int errorCount = 0;
		try {
			for(Iterator<Site> si=sites.iterator();si.hasNext();) {
				Site site = (Site)si.next();
				if(site.issue != null &&
						(!site.issue.isIssue || site.issue.isOutput || site.issue.isDuplicate)) {
					continue;
				}
				String filePath = site.file.getCanonicalPath();
				System.out.println(filePath + ":" + (site.startLineNumber0+1)
						+ ": Missing explanation");
				errorCount++;
			}
		} catch(Exception e) {
			/**
			 * @explain This internal error can happen if a file is removed while MOVES
			 * is processing it.
			**/
			System.out.println("ERROR: Unable to print missing site and issue information.");
		}
		/** @nonissue **/
		System.out.println("******************************************");
		System.out.println("" + errorCount + " message documentation errors were found.");
		System.out.println("******************************************");
	}
}
