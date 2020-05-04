/**************************************************************************************************
 * @(#)S3.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.tools.amazon;

import java.io.*;
import java.util.*;
import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.s3.*;
import com.amazonaws.services.s3.model.*;

/**
 * Script utilities for accessing Amazon S3 file storage.
 * Command lines available are:<br>
 * <ul>
 * 	<li>login accesskey <keyvalue> secretkey <keyvalue>
 *	<li>logout
 * 	<li>put bucket <bucketname> nameinbucket <nameinbucket> file <filenameandpath><br>
 *   Note: put creates the bucket if it does not exist and does not transfer if <nameinbucket> already exists.
 * 	<li>get bucket <bucketname> nameinbucket <nameinbucket> file <filenameandpath><br>
 *   Note: get does not transfer data if <filenameandpath> already exists.
 * 	<li>delete bucket <bucketname> nameinbucket <nameinbucket><br>
 *	 Note: delete reports warnings (not errors) if the bucket or the named object do not exist.
 * 	<li>list bucket <bucketname>
 * </ul>
 *
 * @author		Wesley Faler
 * @version		2013-06-03
**/
public class S3 {
	static final String AUTH_FILE = "AwsCredentials.properties";

	static TreeMap<String,TreeSet<String> > bucketCache = new TreeMap<String,TreeSet<String> >();

    /**
     * Entry point for scripts.
     * @param args the command line arguments
    **/
    public static void main(String[] args) {
    	if(args == null || args.length < 1) {
    		// Print usage and quit
    		printUsage();
    		return;
    	}
    	if(args[0].equalsIgnoreCase("login") && args.length >= 5) {
    		login(args);
    	} else if(args[0].equalsIgnoreCase("logout") && args.length >= 1) {
    		logout(args);
    	} else if(args[0].equalsIgnoreCase("put") && args.length >= 7) {
    		put(args);
    	} else if(args[0].equalsIgnoreCase("putmany") && args.length >= 7) {
    		putMany(args);
    	} else if(args[0].equalsIgnoreCase("get") && args.length >= 7) {
    		get(args);
    	} else if(args[0].equalsIgnoreCase("delete") && args.length >= 5) {
    		delete(args);
    	} else if(args[0].equalsIgnoreCase("list") && args.length >= 3) { // prefix is optional
    		list(args);
    	} else {
    		S3.log("Unknown command: " + args[0]);
    		printUsage();
    	}
    	writeCaches();
    }

	/** Display command line options **/
	static void printUsage() {
		System.out.println("Usage:");
		System.out.println("login accesskey <keyvalue> secretkey <keyvalue>");
		System.out.println("logout");
		System.out.println("put bucket <bucketname> nameinbucket <nameinbucket> file <filenameandpath>");
		System.out.println("    Note: put creates the bucket if it does not exist and does not transfer");
		System.out.println("          if <nameinbucket> already exists.");
		System.out.println("putmany bucket <bucketname> prefix <prefix> directory <fulldirectorypath>");
		System.out.println("    Do a put on all files with the matching prefix.  * is used to indicate all files.");
		System.out.println("get bucket <bucketname> nameinbucket <nameinbucket> file <filenameandpath>");
		System.out.println("    Note: get does not transfer data if <filenameandpath> already exists.");
		System.out.println("delete bucket <bucketname> nameinbucket <nameinbucket>");
		System.out.println("    Note: delete reports warnings if the bucket or the named object do not exist.");
		System.out.println("list bucket <bucketname> [prefix <nameprefix>]");
	}

	/**
	 * Parse a command line for name/value parameters.
	 * @param args command line arguments to main()
	 * @param parameterNames name of the required parameters
	 * @return a map of parameter name and values.  Values are never null, blank, or enclosed in quotation marks.
	 * Only parameters actually present on the command line are used.
	**/
	static TreeMap<String,String> getParameters(String[] args,String[] parameterNames) {
		TreeMap<String,String> results = new TreeMap<String,String>();
		for(int ai=0;ai<args.length-1;ai++) {
			for(int pi=0;pi<parameterNames.length;pi++) {
				if(parameterNames[pi].equalsIgnoreCase(args[ai])) {
					String value = args[++ai];
					if(value != null) {
						value = value.trim();
						if(value.equals("\"\"")) {
							value = "";
						} else {
							if(value.startsWith("\"")) {
								value = value.substring(1);
							}
							if(value.endsWith("\"")) {
								value = value.substring(0,value.length()-1);
							}
							value = value.trim();
						}
						if(value.length() > 0) {
							results.put(parameterNames[pi],value);
						}
					}
				}
			}
		}
		return results;
	}

	/**
	 * Create an authentication credential file.
	 * @param args command line arguments to main()
	**/
	static void login(String[] args) {
		String[] parameterNames = { "accesskey", "secretkey" };
		TreeMap<String,String> parameters = getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid LOGIN command.");
			printUsage();
			return;
		}
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new File(AUTH_FILE));
			writer.println("accessKey = " + parameters.get("accesskey"));
			writer.println("secretKey = " + parameters.get("secretkey"));
		} catch(Exception e) {
			S3.log(e.toString());
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
		clearCaches();
	}

	/**
	 * Destroy any authentication credential file.
	 * @param args command line arguments to main()
	**/
	static void logout(String[] args) {
		File f = new File(AUTH_FILE);
		if(f.exists()) {
			f.delete();
		}
		clearCaches();
	}

	/**
	 * Do a put operation on all files with a given prefix found in a directory.
	 * @param args command line arguments to main()
	**/
	static void putMany(String[] args) {
		String[] parameterNames = { "bucket", "prefix", "directory" };
		TreeMap<String,String> parameters = getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid PUTMANY command.");
			printUsage();
			return;
		}
		try {
			String prefix = parameters.get("prefix").toLowerCase();
			String directoryName = parameters.get("directory");
			File directory = new File(directoryName);
			if(!directory.exists()) {
				S3.log("Directory \"" + directoryName + "\" does not exist.");
				return;
			}
			File[] files = directory.listFiles();
			if(files != null) {
				for(int i=0;i<files.length;i++) {
					String name = files[i].getName().toLowerCase();
					if(prefix.equals("*") || name.startsWith(prefix)) {
						String[] putParameters = {
							"bucket", parameters.get("bucket"),
							"nameinbucket", files[i].getName(),
							"file", files[i].getCanonicalPath()
						};
						put(putParameters);
					}
				}
			}
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon S3, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with S3, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Put a file into a bucket.
	 * @param args command line arguments to main()
	**/
	static void put(String[] args) {
		String[] parameterNames = { "bucket", "nameinbucket", "file" };
		TreeMap<String,String> parameters = getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid PUT command.");
			printUsage();
			return;
		}
		while(true) {
			AmazonS3 s3 = null;
			try {
				s3 = new AmazonS3Client(new PropertiesCredentials(new File(AUTH_FILE)));
	
				String bucketName = parameters.get("bucket");
				String nameInBucket = parameters.get("nameinbucket");
				String fileName = parameters.get("file");
	
				File f = new File(fileName);
				if(!f.exists()) {
					S3.log("File \"" + fileName + "\" does not exist.");
					return;
				}
				if(!doesBucketExist(s3,bucketName)) {
					S3.log("Bucket \"" + bucketName + "\" does not exist.");
					return;
				}
				if(list(s3,bucketName,nameInBucket).contains(nameInBucket)) {
					System.out.println("Bucket \"" + bucketName + "\" already contains \"" + nameInBucket + "\"");
					return;
				}
				while(true) {
					try {
			            s3.putObject(new PutObjectRequest(bucketName, nameInBucket, f));
			            break;
			        } catch (AmazonClientException ace) {
			        	String aceMessage = ace.getMessage().toLowerCase();
			        	if(aceMessage.indexOf(" hash ") >= 0 || aceMessage.indexOf(" hash:") >= 0 || aceMessage.indexOf(" integrity ") >= 0
			        			|| aceMessage.indexOf("unable to upload part") >= 0) {
			        		String errorMessage = "Retrying upload of " + bucketName + "/" + nameInBucket + " after error: " + ace.getMessage();
			        		S3.log(errorMessage);
			        		log(errorMessage);
			        		// Delete the file in S3
			        		try {
	    			            s3.deleteObject(bucketName, nameInBucket);
			        		} catch(Exception e) {
			        			// Nothing to do here, the file may not even exist in S3
			        		}
			        		// Loop around to retry the upload
			        		waitRandomSeconds();
			        		continue;
			        	}
			        } catch(Exception e) {
		        		// Delete the file in S3
		        		try {
    			            s3.deleteObject(bucketName, nameInBucket);
		        		} catch(Exception e2) {
		        			// Nothing to do here, the file may not even exist in S3
		        		}
		        		// Loop around to retry the upload
		        		waitRandomSeconds();
		        		continue;
			        }
				}
				S3.log("Stored \"" + nameInBucket + "\"");
				addToBucketCache(bucketName, nameInBucket);
				return;
	        } catch (AmazonServiceException ase) {
				S3.log("Caught an AmazonServiceException, which means your request made it "
						+ "to Amazon S3, but was rejected with an error response for some reason.");
				S3.log("Error Message:    " + ase.getMessage());
				S3.log("HTTP Status Code: " + ase.getStatusCode());
				S3.log("AWS Error Code:   " + ase.getErrorCode());
				S3.log("Error Type:       " + ase.getErrorType());
				S3.log("Request ID:       " + ase.getRequestId());
	        } catch (AmazonClientException ace) {
				S3.log("Caught an AmazonClientException, which means the client encountered "
						+ "a serious internal problem while trying to communicate with S3, "
						+ "such as not being able to access the network.");
				S3.log("Error Message: " + ace.getMessage());
	        } catch(Exception e) {
	        	S3.log(e.toString());
	        }
       		waitRandomSeconds();
		}
	}

	/**
	 * Get a file from a bucket.
	 * @param args command line arguments to main()
	**/
	static void get(String[] args) {
		String[] parameterNames = { "bucket", "nameinbucket", "file" };
		TreeMap<String,String> parameters = getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid GET command.");
			printUsage();
			return;
		}
		while(true) {
			AmazonS3 s3 = null;
			try {
				s3 = new AmazonS3Client(new PropertiesCredentials(new File(AUTH_FILE)));
	
				String bucketName = parameters.get("bucket");
				String nameInBucket = parameters.get("nameinbucket");
				String fileName = parameters.get("file");
	
				File f = new File(fileName);
				if(f.exists()) {
					System.out.println("File \"" + fileName + "\" already exists.");
					return;
				}
				if(!doesBucketExist(s3,bucketName)) {
					S3.log("Bucket \"" + bucketName + "\" does not exist.");
					return;
				}
				if(!list(s3,bucketName,nameInBucket).contains(nameInBucket)) {
					S3.log("Bucket \"" + bucketName + "\" does not contain \"" + nameInBucket + "\"");
					return;
				}
				while(true) {
					try {
			            s3.getObject(new GetObjectRequest(bucketName, nameInBucket), f);
			            break;
			        } catch (AmazonClientException ace) {
			        	String aceMessage = ace.getMessage().toLowerCase();
			        	if(aceMessage.indexOf(" hash ") >= 0 || aceMessage.indexOf(" hash:") >= 0 || aceMessage.indexOf(" integrity ") >= 0) {
			        		String errorMessage = "Retrying download of " + bucketName + "/" + nameInBucket + " after error: " + ace.getMessage();
			        		S3.log(errorMessage);
			        		log(errorMessage);
			        		// Delete the local file
			        		try {
	    			            f.delete();
			        		} catch(Exception e) {
			        			// Nothing to do here, the file may not even exist on the file system
			        		}
			        		// Loop around to retry the download
			        		waitRandomSeconds();
			        		continue;
			        	}
			        } catch(Exception e) {
		        		String errorMessage = "Retrying download of " + bucketName + "/" + nameInBucket + " after error: " + e.getMessage();
		        		S3.log(errorMessage);
		        		log(errorMessage);
		        		// Delete the local file
		        		try {
				            f.delete();
		        		} catch(Exception e2) {
		        			// Nothing to do here, the file may not even exist on the file system
		        		}
		        		// Loop around to retry the download
		        		waitRandomSeconds();
		        		continue;
			        }
				}
				S3.log("Retrieved \"" + nameInBucket + "\"");
				return;
	        } catch (AmazonServiceException ase) {
				S3.log("Caught an AmazonServiceException, which means your request made it "
						+ "to Amazon S3, but was rejected with an error response for some reason.");
				S3.log("Error Message:    " + ase.getMessage());
				S3.log("HTTP Status Code: " + ase.getStatusCode());
				S3.log("AWS Error Code:   " + ase.getErrorCode());
				S3.log("Error Type:       " + ase.getErrorType());
				S3.log("Request ID:       " + ase.getRequestId());
	        } catch (AmazonClientException ace) {
				S3.log("Caught an AmazonClientException, which means the client encountered "
						+ "a serious internal problem while trying to communicate with S3, "
						+ "such as not being able to access the network.");
				S3.log("Error Message: " + ace.getMessage());
	        } catch(Exception e) {
	        	S3.log(e.toString());
	        }
      		waitRandomSeconds();
		}
	}

	/**
	 * Remove a file from a bucket.
	 * @param args command line arguments to main()
	**/
	static void delete(String[] args) {
		delete(args,false);
	}

	/**
	 * Remove a file from a bucket.
	 * @param args command line arguments to main()
	 * @param isSilent false to print messages about file existence
	**/
	static void delete(String[] args, boolean isSilent) {
		String[] parameterNames = { "bucket", "nameinbucket" };
		TreeMap<String,String> parameters = getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DELETE command.");
			printUsage();
			return;
		}
		while(true) {
			AmazonS3 s3 = null;
			try {
				s3 = new AmazonS3Client(new PropertiesCredentials(new File(AUTH_FILE)));
	
				String bucketName = parameters.get("bucket");
				String nameInBucket = parameters.get("nameinbucket");
	
				if(!doesBucketExist(s3,bucketName)) {
					S3.log("Bucket \"" + bucketName + "\" does not exist.");
					return;
				}
				if(nameInBucket.endsWith("*")) {
					String shortNameInBucket = nameInBucket.substring(0,nameInBucket.length()-1); // remove the asterisk
					TreeSet<String> files = list(s3,bucketName,shortNameInBucket); // get all files with the desired prefix
					if(files != null && files.size() > 0) {
						// Delete each matching file
						for(Iterator<String> i=files.iterator();i.hasNext();) {
							String t = i.next();
				            s3.deleteObject(bucketName, t);
							S3.log("Deleted \"" + t + "\"");
							removeFromBucketCache(bucketName,t);
						}
					} else {
						if(!isSilent) {
							System.out.println("Bucket \"" + bucketName + "\" does not contain \"" + nameInBucket + "*\"");
						}
						return;
					}
				} else {
					if(!list(s3,bucketName,nameInBucket).contains(nameInBucket)) {
						if(!isSilent) {
							System.out.println("Bucket \"" + bucketName + "\" does not contain \"" + nameInBucket + "\"");
						}
						return;
					}
		            s3.deleteObject(bucketName, nameInBucket);
					S3.log("Deleted \"" + nameInBucket + "\"");
					removeFromBucketCache(bucketName,nameInBucket);
				}
				return;
	        } catch (AmazonServiceException ase) {
				S3.log("Caught an AmazonServiceException, which means your request made it "
						+ "to Amazon S3, but was rejected with an error response for some reason.");
				S3.log("Error Message:    " + ase.getMessage());
				S3.log("HTTP Status Code: " + ase.getStatusCode());
				S3.log("AWS Error Code:   " + ase.getErrorCode());
				S3.log("Error Type:       " + ase.getErrorType());
				S3.log("Request ID:       " + ase.getRequestId());
	        } catch (AmazonClientException ace) {
				S3.log("Caught an AmazonClientException, which means the client encountered "
						+ "a serious internal problem while trying to communicate with S3, "
						+ "such as not being able to access the network.");
				S3.log("Error Message: " + ace.getMessage());
	        } catch(Exception e) {
	        	S3.log(e.toString());
	        }
       		waitRandomSeconds();
		}
	}

	/**
	 * Enumerate all files within a bucket.
	 * @param args command line arguments to main()
	**/
	static void list(String[] args) {
		String[] parameterNames = { "bucket", "prefix" };
		TreeMap<String,String> parameters = getParameters(args,parameterNames);
		if(!parameters.containsKey("prefix")) {
			parameters.put("prefix","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid LIST command.");
			printUsage();
			return;
		}
		AmazonS3 s3 = null;
		try {
			s3 = new AmazonS3Client(new PropertiesCredentials(new File(AUTH_FILE)));

			String bucketName = parameters.get("bucket");
			String prefix = parameters.get("prefix");

			if(!doesBucketExist(s3,bucketName)) {
				S3.log("Bucket \"" + bucketName + "\" does not exist.");
				return;
			}
			System.out.println("Contents of \"" + bucketName + "\":");
			TreeSet<String> names = list(s3,bucketName,prefix);
			if(names != null && names.size() > 0) {
				for(Iterator<String> i=names.iterator();i.hasNext();) {
					String name = i.next();
					System.out.println(name);
				}
			}
			System.out.println("End of Contents of \"" + bucketName + "\"");
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon S3, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with S3, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Check the existence of a bucket.
	 * @param s3 interface to Amazon services
	 * @param bucketName name of S3 bucket
	**/
	static boolean doesBucketExist(AmazonS3 s3, String bucketName)
			throws AmazonClientException, AmazonServiceException, Exception {
		synchronized(bucketCache) {
			TreeSet<String> listing = bucketCache.get(bucketName);
			if(listing == null) {
				File cacheFile = new File("cache." + bucketName);
				if(cacheFile.exists() || s3.doesBucketExist(bucketName)) {
					listing = listFromCacheFile(bucketName);
					if(listing == null) {
						listing = listCore(s3,bucketName);
					}
				} else {
					return false;
				}
				if(listing != null) {
					bucketCache.put(bucketName,listing);
				}
			}
			return true;
		}
	}

	/**
	 * List all objects in a bucket.
	 * @param s3 interface to Amazon services
	 * @param bucketName name of S3 bucket
	 * @param prefix prefix to use, blank for all objects
	 * @throws AmazonClientException if anything goes wrong
	 * @throws AmazonServiceException if anything goes wrong
	 * @throws Exception if anything goes wrong
	 * @returns set of names (keys) in the bucket with the prefix
	**/
	static TreeSet<String> list(AmazonS3 s3, String bucketName, String prefix)
			throws AmazonClientException, AmazonServiceException, Exception {
		synchronized(bucketCache) {
			TreeSet<String> listing = bucketCache.get(bucketName);
			if(listing == null) {
				listing = listFromCacheFile(bucketName);
				if(listing == null) {
					listing = listCore(s3,bucketName);
				}
				if(listing != null) {
					bucketCache.put(bucketName,listing);
				}
			}
			if(listing != null && prefix != null && prefix.length() > 0) {
				TreeSet<String> filteredListing = new TreeSet<String>();
				for(Iterator<String> i=listing.iterator();i.hasNext();) {
					String t = i.next();
					if(t.startsWith(prefix)) {
						filteredListing.add(t);
					}
				}
				return filteredListing;
			}
			return listing;
		}
	}

	/**
	 * List all objects in a bucket.
	 * @param s3 interface to Amazon services
	 * @param bucketName name of S3 bucket
	 * @throws AmazonClientException if anything goes wrong
	 * @throws AmazonServiceException if anything goes wrong
	 * @throws Exception if anything goes wrong
	 * @returns set of names (keys) in the bucket with the prefix
	**/
	static TreeSet<String> listCore(AmazonS3 s3, String bucketName)
			throws AmazonClientException, AmazonServiceException, Exception {
		TreeSet<String> names = new TreeSet<String>();
		ObjectListing listing = s3.listObjects(bucketName,"");
		while(listing != null) {
			for(Iterator<S3ObjectSummary> i=listing.getObjectSummaries().iterator();i.hasNext();) {
				S3ObjectSummary s = i.next();
				names.add(s.getKey());
			}
			if(!listing.isTruncated()) {
				break;
			}
			listing = s3.listNextBatchOfObjects(listing);
		}
		return names;
	}

	/**
	 * Read a locally cached bucket listing.
	 * @param bucketName name of S3 bucket
	 * @return set of names (keys) in the bucket, or null if no local cache file exists or if the local cache is empty.
	**/
	static TreeSet<String> listFromCacheFile(String bucketName) {
		File cacheFile = new File("cache." + bucketName);
		if(!cacheFile.exists()) {
			System.out.println("No local cache for bucket " + bucketName);
			return null;
		}
		System.out.println("Reading local cache for bucket " + bucketName);
		LineNumberReader reader = null;
		try {
			reader = new LineNumberReader(new FileReader(cacheFile),128*1024);
			TreeSet<String> listing = new TreeSet<String>();
			String line = "";
			while((line = reader.readLine()) != null) {
				line = line.trim();
				if(line.length() > 0) {
					listing.add(line);
				}
			}
			if(listing.size() > 0) {
				System.out.println("Finished reading local cache for bucket " + bucketName);
				return listing;
			} else {
				System.out.println("Local cache is empty for bucket " + bucketName);
				return null;
			}
		} catch(Exception e) {
			S3.log(e.toString());
			return null;
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
	}

	/**
	 * Cache data about a new transfer into a bucket.
	 * @param bucketName name of S3 bucket
	 * @param nameInBucket name of the file placed into the bucket
	**/
	static void addToBucketCache(String bucketName, String nameInBucket) {
		synchronized(bucketCache) {
			TreeSet<String> listing = bucketCache.get(bucketName);
			if(listing != null) {
				listing.add(nameInBucket);
			}
		}
	}

	/**
	 * Cache data about a file removed from a bucket.
	 * @param bucketName name of S3 bucket
	 * @param nameInBucket name of the file removed from the bucket
	**/
	static void removeFromBucketCache(String bucketName, String nameInBucket) {
		synchronized(bucketCache) {
			TreeSet<String> listing = bucketCache.get(bucketName);
			if(listing != null) {
				listing.remove(nameInBucket);
			}
		}
	}

	/**
	 * Clear all local cache files for any bucket in the bucketCache, then clear the bucketCache itself.
	**/
	static void clearCaches() {
		try {
			File[] files = new File(".").listFiles();
			if(files != null && files.length > 0) {
				for(int i=0;i<files.length;i++) {
					String name = files[i].getName().toLowerCase();
					if(name.startsWith("cache.")) {
						files[i].delete();
					}
				}
			}
		} catch(Exception e) {
			S3.log(e.toString());
		} finally {
			bucketCache.clear();
		}
	}

	/**
	 * Write bucketCache to local cache files.
	**/
	static void writeCaches() {
		Set<String> cacheNames = bucketCache.keySet();
		for(Iterator<String> ki=cacheNames.iterator();ki.hasNext();) {
			String bucketName = ki.next();
			TreeSet<String> listing = bucketCache.get(bucketName);
			if(listing != null && listing.size() > 0) {
				PrintWriter writer = null;
				try {
					File cacheFile = new File("cache." + bucketName);
					writer = new PrintWriter(new BufferedWriter(new FileWriter(cacheFile),128*1024));
					for(Iterator<String> i=listing.iterator();i.hasNext();) {
						writer.println(i.next());
					}
				} catch(Exception e) {
					S3.log(e.toString());
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
		}
	}

	/**
	 * Write a message to the movesamazon.log file.
	 * @param text message to be written
	**/
	public static void log(String text) {
		System.out.println(text);

		File logFile = new File("movesamazon.log");
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new FileWriter(logFile,true));
			writer.println(text);
		} catch(Exception e) {
			// Nothing to do here
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

	/** Random number generator **/
	private static Random randomGenerator = null;

	/** Wait idle a short, but random, period.  Useful when attempting retries for file or network failures **/
	public static void waitRandomSeconds() {
		if(randomGenerator == null) {
			randomGenerator = new Random(System.currentTimeMillis());
		}
		int delaySeconds = 5 + randomGenerator.nextInt(30);
		long endTime = (1000L * delaySeconds) + System.currentTimeMillis();
		while(endTime >= System.currentTimeMillis()) {
			try {
				Thread.sleep(1000);
			} catch(Exception e) {
				// Nothing to do here
			}
		}
	}
}
