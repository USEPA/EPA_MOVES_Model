/**************************************************************************************************
 * @(#)ExtractedDataCache.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.sql.*;
import java.util.*;
import java.io.*;

/**
 * Cache data files extracted from the master to be sent to workers.
 *
 * @author		Wesley Faler
 * @version		2014-04-08
**/
public class ExtractedDataCache {
	/** Mutex for guarding all shared data **/
	private static Integer mutex = new Integer(101110);
	/** Folder holding all cached files **/
	private static File cacheFolder = null;

	static class CacheEntry {
		public File file;
		public String name;
	}

	/** cached CacheEntry objects, keyed by lowercase SQL-based key **/
	private static TreeMap<String,CacheEntry> cachedFiles = new TreeMap<String,CacheEntry>();

	/** Establish the folder to hold cache files and do any other required setup. **/
	public static void setup() {
		synchronized(mutex) {
			if(cacheFolder != null) {
				return;
			}
			cacheFolder = FileUtilities.createTemporaryFolder(null,"ExtDataCache");
		}
	}

	/** Remove all cached data **/
	public static void clear() {
		synchronized(mutex) {
			setup();
			Set<String> keys = cachedFiles.keySet();
			for(Iterator<String> i=keys.iterator();i.hasNext();) {
				String key = i.next();
				CacheEntry ce = cachedFiles.get(key);
				if(ce != null && ce.file != null) {
					FileUtilities.deleteFileWithRetry(ce.file);
				}
			}
			cachedFiles.clear();
		}
	}

	/**
	 * Process a SQL statement that may contain cache directives.  Example statements are:
	 * 		cache select * from SomeTable into outfile '##SomeTable##';
	 *		cache(year=##context.yearID##) select * from SomeTable into outfile '##SomeTable##';
	 * The latter example actually has the context year changed before it gets to this point, making it
	 * something such as:
	 *		cache(year=2010) select * from SomeTable into outfile '##SomeTable##';
	 * The cache-clause, including its parameters, are part of the file cache key.
	 * @param db database to be used
	 * @param sql statement that may or may not contain cache directives
	 * @param temporaryFolder directory to hold any files copied from the cache
	 * @return a file in the temporary folder, copied from the cache.  If there are no cache directives,
	 * null is returned.
	 * @throws SQLException if the SQL syntax is invalid
	 * @throws IOException if any issues arise copying files
	**/
	public static File handle(Connection db,String sql,File temporaryFolder) throws SQLException, IOException {
		if(!sql.startsWith("cache") && !sql.startsWith("CACHE") && !sql.startsWith("Cache")) {
			return null;
		}
		// Convert the SQL statement into the cache key.
		String cacheKey = sql.toLowerCase();
		CacheEntry ce = null;
		synchronized(mutex) {
			ce = cachedFiles.get(cacheKey);
		}
		// If the key is not found or we don't have a file...
		if(ce == null) {
			// Get the real SQL statement that is after the "cache[(....)]" clause.
			sql = sql.substring(5); // get everything past the word "cache"
			if(sql.startsWith("(")) { // If we find (, then skip to the matching ) and remove it all.
				int index = sql.indexOf(')');
				if(index > 0) {
					sql = sql.substring(index+1).trim();
				}
			}
			// Get the table name in the real SQL and replace it with the cacheFolder path and unique file name
			int firstEscapeTextIndex = sql.indexOf("##");
			if(firstEscapeTextIndex < 0) {
				return null;
			}
			int secondEscapeTextIndex = sql.indexOf("##",firstEscapeTextIndex + 2);
			if(secondEscapeTextIndex < 0) {
				return null;
			}
			ce = new CacheEntry();
			ce.name = sql.substring(firstEscapeTextIndex + 2, secondEscapeTextIndex).toLowerCase(); // standardize on lower-case names
			while(true) {
				ce.file = new File(cacheFolder,ce.name + "." + System.currentTimeMillis());
				if(!ce.file.exists()) {
					break;
				}
				try {
					Thread.sleep(3);
				} catch(InterruptedException e) {
					throw new IOException("Thread interrupted while creating unique name for cached file");
				}
			}
			// Replace backslashes with forward slashes for mysql
			String fileName = ce.file.getCanonicalPath().replace('\\', '/');
			sql = sql.substring(0, firstEscapeTextIndex) + fileName + sql.substring(secondEscapeTextIndex + 2);

			// Execute the statement, generating the cached file
			SQLRunner.executeSQL(db,sql);

			// Store the cache information
			ce.file.deleteOnExit();
			synchronized(mutex) {
				cachedFiles.put(cacheKey,ce);
			}
		}
		if(ce == null || ce.file == null) {
			return null;
		}
		File finalFile = new File(temporaryFolder,ce.name);
		if(finalFile.exists()) {
			FileUtilities.deleteFileWithRetry(finalFile);
		}
		if(!FileUtilities.copyFile(ce.file,finalFile,true)) {
			throw new IOException("Unable to copy " + ce.file.getName() + " to " + finalFile.getName());
		}
		return finalFile;
	}
}
