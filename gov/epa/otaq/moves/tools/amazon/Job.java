/**************************************************************************************************
 * @(#)Job.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.tools.amazon;

import java.io.*;
import java.util.*;
import java.util.jar.*;
import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.sqs.*;
import com.amazonaws.services.sqs.model.*;
import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;
import com.amazonaws.services.s3.*;
import com.amazonaws.services.s3.model.*;
import java.util.concurrent.*;

/**
 * Script utilities for accessing Amazon SQS job queues.
 *
 * @author		Wesley Faler, Chiu Foong
 * @version		2018-02-06
**/
public class Job {
	static final String STANDARD_OUTPUTDB = "standardoutput";

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
    		S3.login(args);
    	} else if(args[0].equalsIgnoreCase("logout") && args.length >= 1) {
    		S3.logout(args);
    	} else if(args[0].equalsIgnoreCase("createqueue") && args.length >= 5) {
    		createQueue(args);
    	} else if(args[0].equalsIgnoreCase("deletequeue") && args.length >= 3) {
    		deleteQueue(args);
    	} else if(args[0].equalsIgnoreCase("flushqueue") && args.length >= 3) {
    		flushQueue(args);
    	} else if(args[0].equalsIgnoreCase("listqueues") && args.length >= 1) {
    		listQueues(args);
    	} else if(args[0].equalsIgnoreCase("addstatus") && args.length >= 5) {
    		addStatus(args);
    	} else if(args[0].equalsIgnoreCase("getstatus") && args.length >= 5) {
    		getStatus(args);
    	} else if(args[0].equalsIgnoreCase("waitforjob") && args.length >= 5) {
    		waitForJob(args);
    	} else if(args[0].equalsIgnoreCase("getjob") && args.length >= 5) {
    		getJob(args);
    	} else if(args[0].equalsIgnoreCase("completejob") && args.length >= 5) {
    		completeJob(args);
    	} else if(args[0].equalsIgnoreCase("addjob") && args.length >= 17) {
    		addJob(args);
    	} else if(args[0].equalsIgnoreCase("addjobs") && args.length >= 17) {
    		addJobs(args);
    	} else if(args[0].equalsIgnoreCase("readdjobs") && args.length >= 17) {
    		reAddJobs(args);
    	} else if(args[0].equalsIgnoreCase("jarjob") && args.length >= 3) {
    		jarJob(args);
    	} else if(args[0].equalsIgnoreCase("uploadjob") && args.length >= 5) {
    		uploadJob(args);
    	} else if(args[0].equalsIgnoreCase("jarjobs") && args.length >= 3) {
    		jarJobs(args);
    	} else if(args[0].equalsIgnoreCase("uploadjobs") && args.length >= 5) {
    		uploadJobs(args);
    	} else if(args[0].equalsIgnoreCase("jarjobresults") && args.length >= 3) {
    		jarJobResults(args);
    	} else if(args[0].equalsIgnoreCase("uploadjobresults") && args.length >= 5) {
    		uploadJobResults(args);
    	} else if(args[0].equalsIgnoreCase("batchstatus") && args.length >= 3) {
    		batchStatus(args);
    	} else if(args[0].equalsIgnoreCase("removejobjars") && args.length >= 5) {
    		removeJobJars(args);
    	} else if(args[0].equalsIgnoreCase("downloadjobresults") && args.length >= 5) {
    		downloadJobResults(args);
    	} else if(args[0].equalsIgnoreCase("downloadresults") && args.length >= 5) {
    		downloadResults(args);
    	} else if(args[0].equalsIgnoreCase("downloadjobdbresults") && args.length >= 5) {
    		downloadJobDBResults(args);
    	} else if(args[0].equalsIgnoreCase("downloaddbresults") && args.length >= 5) {
    		downloadDBResults(args);
    	} else if(args[0].equalsIgnoreCase("terminate") && args.length >= 3) {
    		terminate(args);
    	} else if(args[0].equalsIgnoreCase("addpostprocess") && args.length >= 13) {
    		addPostProcess(args);
    	} else if(args[0].equalsIgnoreCase("readdpostprocess") && args.length >= 13) {
    		reAddPostProcess(args);
    	} else if(args[0].equalsIgnoreCase("getpostprocess") && args.length >= 7) {
			getPostProcess(args);
    	} else if(args[0].equalsIgnoreCase("downloadpostresults") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadPostResults(args);
    	} else if(args[0].equalsIgnoreCase("downloadallpostresults") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadAllPostResults(args);
    	} else if(args[0].equalsIgnoreCase("splitresults") && args.length >= 5) {
    		splitResults(args);
    	} else if(args[0].equalsIgnoreCase("scanlogs") && args.length >= 3) {
    		scanLogs(args);
    	} else if(args[0].equalsIgnoreCase("downloaddatabasejar") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadDatabaseJar(args);
    	} else if(args[0].equalsIgnoreCase("downloadBucketNameJar") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadBucketNameJar(args);
    	} else if(args[0].equalsIgnoreCase("downloadDbResultProfileJar") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadDbResultProfileJar(args);
	    } else if(args[0].equalsIgnoreCase("downloadMovesDbJar") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadMovesDbJar(args);
		} else if(args[0].equalsIgnoreCase("downloadPostProcessJar") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadPostProcessJar(args);
		} else if(args[0].equalsIgnoreCase("downloadCodeJar") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadCodeJar(args);
		} else if(args[0].equalsIgnoreCase("downloadDbResultsJar") && args.length >= 5) { // "code" is optional, so keep limit of 5
    		downloadDbResultsJar(args);		
    	} else {
    		S3.log("Unknown command: " + args[0]);
    		printUsage();
    	}
    	S3.writeCaches();
    }

	/** Display command line options **/
	static void printUsage() {
		System.out.println("Usage:");
		System.out.println("login accesskey <keyvalue> secretkey <keyvalue>");

		System.out.println("logout");

		System.out.println("createqueue queue <queuename> timeoutminutes <defaulttimeoutminutes>");
		System.out.println("deletequeue queue <queuename>");
		System.out.println("flushqueue queue <queuename>");

		System.out.println("listqueues");

		System.out.println("addstatus queue <queuename> status <statusmessage>");

		System.out.println("getstatus queue <queuename> file <statuslogfile>");

		System.out.println("waitforjob queue <queuename> command <commandtorun>");

		System.out.println("getjob queue <queuename> command <commandtorun>");
		System.out.println("    Execute <commandtorun> with environment variables:");
		System.out.println("    MOVES_JOBID");
		System.out.println("    MOVES_DATABASEBUCKET");
		System.out.println("    MOVES_DATABASENAME");
		System.out.println("    MOVES_CODEBUCKET");
		System.out.println("    MOVES_CODENAME");
		System.out.println("    MOVES_JOBBUCKET");
		System.out.println("    MOVES_JOBNAME");
		System.out.println("	MOVES_STATUSQUEUE");
		System.out.println("    MOVES_EXTRADBS1");
		System.out.println("    MOVES_EXTRADBS2");
		System.out.println("    MOVES_OUTPUTDB");

		System.out.println("completejob queue <queuename> jobid <jobid>");

		System.out.println("addjob jobqueue <jobqueuename> statusqueue <statusqueuename>");
		System.out.println("    databasebucket <dbbucket> database <dbname>");
		System.out.println("    codebucket <codebucket> code <codename>");
		System.out.println("    jobbucket <jobbucket> jobdir <jobdirectory>");
		System.out.println("    Does nothing if the job folder contains status.queued file.");
		System.out.println("    Does nothing if the job's folder already contains results*.jar, *.log, *.txt files, and/or an output/ folder. (like downloadjobresults)");
		System.out.println("    Complains if the job folder is missing status.uploaded file.");
		System.out.println("    Before enqueuing the job, deletes results_<batch>_<job>.jar if it exists from the bucket.");
		System.out.println("    After enqueuing the job, creates status.queued file.");

		System.out.println("addjobs jobqueue <jobqueuename> statusqueue <statusqueuename>");
		System.out.println("    databasebucket <dbbucket> database <dbname>");
		System.out.println("    codebucket <codebucket> code <codename>");
		System.out.println("    jobbucket <jobbucket> batchdir <batchdirectory>");
		System.out.println("    Adds all jobs in a batch using the addjob command, but only for jobs not already queued.");

		System.out.println("readdjobs jobqueue <jobqueuename> statusqueue <statusqueuename>");
		System.out.println("    databasebucket <dbbucket> database <dbname>");
		System.out.println("    codebucket <codebucket> code <codename>");
		System.out.println("    jobbucket <jobbucket> batchdir <batchdirectory>");
		System.out.println("    Adds all jobs in a batch using the addjob command, clearing any prior status.queued file for jobs not done.");

		System.out.println("jarjob jobdir <jobdirectory>");
			//Jars the job's databases/ folder
			//Does not jar the batch's databases/ folder

		System.out.println("uploadjob jobdir <jobdirectory> jobbucket <jobbucket>");
		System.out.println("    creates status.uploaded file in the job folder.");
		System.out.println("jarjobs batchdir <batchdirectory>");
			//Jars each job in a batch.
			//Creates a jar holding the batch's databases/ folder, unless that jar already exists, called databases_<batch>.jar in the batch's databases/
			//Creates a jar holding the shared databases_<scenario>/ folder, unless the jar already exists, called databases_<scenario>.jar in the shared databases_<scenario>/ folder

		System.out.println("uploadjobs batchdir <batchdirectory> jobbucket <jobbucket>");
			//Uploads each job's jar
			//Uploads the batch's database jar
			//Uploads the scenario's shared database jar

		System.out.println("jarjobresults jobdir <jobdirectory>");
			//	Creates results_<jobdirectory>.jar expecting <jobdirectory> to be <batch>_<job>
			//	Includes *.txt, *.log files and output/ folder.

		System.out.println("uploadjobresults jobdir <jobdirectory> jobbucket <jobbucket>");

		System.out.println("removejobjars jobdir <jobdirectory> jobbucket <jobbucket>");
		System.out.println("    Removes <batch>_<job>.jar and results_<batch>_<job>.jar files if they exist.");
		System.out.println("    Creates status.cleaned file in the job folder.");

		System.out.println("downloadjobresults jobdir <jobdirectory> jobbucket <jobbucket>");
		System.out.println("    Does nothing if the job's folder already contains results*.jar, *.log, *.txt files, and/or an output/ folder.");
		System.out.println("    Checks <jobbucket> for results_<batch>_<job>.jar file, downloading it if available.");
		System.out.println("    After the download, removes the job's jar files from the bucket using removejobjars.");
		System.out.println("    Creates status.done file in the job folder.");

		System.out.println("downloadresults batchdir <batchdirectory> jobbucket <jobbucket>");
		System.out.println("    Does downloadjobresults on each job folder in a batch.");

		System.out.println("downloadjobdbresults jobdir <jobdirectory> jobbucket <jobbucket>");
		System.out.println("    Does nothing if the job's folder already contains db_results*.jar and/or an output/ folder.");
		System.out.println("    Checks <jobbucket> for db_results_<batch>_<job>.jar file, downloading it if available.");
		System.out.println("    After the download, does not delete files from the bucket.");

		System.out.println("downloaddbresults batchdir <batchdirectory> jobbucket <jobbucket>");
		System.out.println("    Does downloadjobdbresults on each job folder in a batch.");

		System.out.println("batchstatus batchdir <batchdirectory>");
		System.out.println("    Create [a new] <batch>_status.csv file in <batchdirectory>");
		System.out.println("    Checks each job directory for status.* files [.uploaded, .queued, .done, .cleaned]");

		System.out.println("terminate instanceid <instanceid>");
		System.out.println("    Terminate an EC2 instance");

		System.out.println("addpostprocess jobqueue <jobqueuename> statusqueue <statusqueuename>");
		System.out.println("    codebucket <codebucket> code <codename>");
		System.out.println("    jobbucket <jobbucket> batchdir <batchdirectory>");
		System.out.println("    Post process all jobs in a batch.");

		System.out.println("getpostprocess queue <queuename> command <commandtorun> todir <downloaddirectory>");
		System.out.println("    Execute <commandtorun> with environment variables:");
		System.out.println("    MOVES_BATCHID");
		System.out.println("    MOVES_JOBID");
		System.out.println("    MOVES_CODEBUCKET");
		System.out.println("    MOVES_CODENAME");
		System.out.println("    MOVES_JOBBUCKET");
		System.out.println("    MOVES_BATCH");
		System.out.println("    MOVES_JOBNAME");
		System.out.println("    MOVES_STATUSQUEUE");

		System.out.println("downloadpostresults batchdir <batchdirectory> jobbucket <jobbucket> [code <postversion>]");
		System.out.println("    Downloads a batch's post processing result files or just one file if <postversion> is supplied.");

		System.out.println("downloadallpostresults todir <downloaddirectory> jobbucket <jobbucket> [code <postversion>]");
		System.out.println("    Download post processing results for all batches.");
		System.out.println("    All results are downloaded unless <postversion> is supplied.");

		System.out.println("splitresults fromdir <downloaddirectory> todir <processeddirectory>");
		System.out.println("    Split a results*.jar file into one with only logs and db_results*.jar with the output databases.");

		System.out.println("scanlogs logqueuedir <logqueuedirectory>");
		System.out.println("    Read all log files for errors, renaming them as OK.* or Failed.*");
	}

	/**
	 * Create a queue.
	 * @param args command line arguments to main()
	**/
	static void createQueue(String[] args) {
		String[] parameterNames = { "queue", "timeoutminutes" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid CREATEQUEUE command.");
			printUsage();
			return;
		}
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL != null) {
				S3.log("Queue \"" + queueName + "\" already exists.");
				return;
			}

			String timeoutMinutesText = parameters.get("timeoutminutes");
			int timeoutMinutes = 60*24;

			try {
				timeoutMinutes = Integer.parseInt(timeoutMinutesText);
				if(timeoutMinutes <= 0) {
					timeoutMinutes = 60*24;
					System.out.println("Using default timeout of " + timeoutMinutes + " minutes");
				}
			} catch(Exception e) {
				S3.log("Invalid timeout provided \"" + timeoutMinutesText + "\"");
				return;
			}
			Integer visibilityTimeout = new Integer(timeoutMinutes*60);
			queueURL = sqs.createQueue(new CreateQueueRequest(queueName,visibilityTimeout)).getQueueUrl();
			S3.log("Created queue \"" + queueName + "\" with timeout of " + visibilityTimeout + " seconds");
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * List queues.
	 * @param args command line arguments to main()
	**/
	static void listQueues(String[] args) {
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String[] attributeNames = {
				"All"
			};
			TreeSet<String> names = list(sqs);
			ArrayList<Map<String,String> > attributes = new ArrayList<Map<String,String> >();
			for(String url : names) {
				attributes.add(sqs.getQueueAttributes(new GetQueueAttributesRequest(url).withAttributeNames(attributeNames)).getAttributes());
			}

			System.out.println("Queues:");
			int index = 0;
			for(String url : names) {
				System.out.println(url);
                for (Map.Entry<String, String> entry : attributes.get(index++).entrySet()) {
                    System.out.println("\t" + entry.getKey() + " = " + entry.getValue());
                }
			}
			System.out.println("End of Queues.");
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Delete a queue.
	 * @param args command line arguments to main()
	**/
	static void deleteQueue(String[] args) {
		String[] parameterNames = { "queue" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DELETEQUEUE command.");
			printUsage();
			return;
		}
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null || queueURL.length() <= 0) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}

			sqs.deleteQueue(new DeleteQueueRequest(queueURL));
			S3.log("Deleted queue \"" + queueName + "\"");
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Add a status message to a text-only queue.
	 * @param args command line arguments to main()
	**/
	static void addStatus(String[] args) {
		String[] parameterNames = { "queue", "status" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid ADDSTATUS command.");
			printUsage();
			return;
		}
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}

			String instanceID = System.getenv("INSTANCE_ID");
			if(instanceID == null) {
				instanceID = "";
			}
			String now = datetime();
			//String status = "" + System.currentTimeMillis() + ": " + parameters.get("status");
			String status = now + ": " + instanceID + ": " + parameters.get("status");
            sqs.sendMessage(new SendMessageRequest(queueURL, status));
            System.out.println("Status enqueued.");
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Get a status message from a text-only queue and append it to a file.
	 * @param args command line arguments to main()
	**/
	static void getStatus(String[] args) {
		String[] parameterNames = { "queue", "file" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid GETSTATUS command.");
			printUsage();
			return;
		}
		File logFile = new File(parameters.get("file"));
		PrintWriter writer = null;
		AmazonSQSClient sqs = null;
		AmazonSQSAsyncClient sqsAsync = null;

		TreeSet<String> messagesToWrite = new TreeSet<String>();
		ArrayList<Future<Void> > futures = new ArrayList<Future<Void> >();

		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			sqsAsync = new AmazonSQSAsyncClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}

			boolean done = false;
			int totalCount = 0;
			while(!done && totalCount < 10000) {
				done = true;

				String statusText = null;
	            ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL).withMaxNumberOfMessages(new Integer(10));
	            List<Message> messages = sqs.receiveMessage(receiveMessageRequest).getMessages();
	            for (Message message : messages) {
	            	/*
	                System.out.println("  Message");
	                System.out.println("    MessageId:     " + message.getMessageId());
	                System.out.println("    ReceiptHandle: " + message.getReceiptHandle());
	                System.out.println("    MD5OfBody:     " + message.getMD5OfBody());
	                System.out.println("    Body:          " + message.getBody());
	                */
	                statusText = message.getBody();
	                System.out.println("    " + statusText);
	                /*
	                for (Map.Entry<String, String> entry : message.getAttributes().entrySet()) {
	                    System.out.println("  Attribute");
	                    System.out.println("    Name:  " + entry.getKey());
	                    System.out.println("    Value: " + entry.getValue());
	                }
	                */
	                // Delete the message from the queue
		            String messageRecieptHandle = message.getReceiptHandle();
		            //sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageRecieptHandle));
		            Future<Void> f = sqsAsync.deleteMessageAsync(new DeleteMessageRequest(queueURL, messageRecieptHandle));
		            if(f != null) {
		            	futures.add(f);
		            }

					if(writer == null) {
						writer = new PrintWriter(new BufferedWriter(new FileWriter(logFile,true),64*1024));
					}
					messagesToWrite.add(statusText);
					//writer.println(statusText);
					done = false;
					totalCount++;
	            }
	            if(messagesToWrite.size() >= 200) {
        			for(Iterator<String> i=messagesToWrite.iterator();i.hasNext();) {
        				writer.println(i.next());
        			}
        			messagesToWrite.clear();
        			writer.flush();
	            }
	            if(futures.size() >= 10) {
	            	//System.out.println("Waiting for " + futures.size() + " transactions...");
	            	for(Iterator<Future<Void> > i=futures.iterator();i.hasNext();) {
	            		Future<Void> f = i.next();
	            		try {
		            		f.get();
	            		} catch(Exception e) {
	            			// Nothing to do here
	            		}
	            	}
	            	futures.clear();
	            	//System.out.println("Done waiting for transactions.");
	            }
			}
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        } finally {
        	if(writer != null) {
        		try {
        			for(Iterator<String> i=messagesToWrite.iterator();i.hasNext();) {
        				writer.println(i.next());
        			}
        		} catch(Exception e) {
        			// Nothing to do
        		}
        		try {
        			writer.close();
        		} catch(Exception e) {
        			// Nothing to do
        		}
        		writer = null;
        	}
            if(futures.size() > 0) {
            	//System.out.println("Waiting for final " + futures.size() + " transactions...");
            	for(Iterator<Future<Void> > i=futures.iterator();i.hasNext();) {
            		Future<Void> f = i.next();
            		try {
	            		f.get();
            		} catch(Exception e) {
            			// Nothing to do here
            		}
            	}
            	futures.clear();
            	//System.out.println("Done waiting for transactions.");
            }
            if(sqsAsync != null) {
            	ExecutorService e = sqsAsync.getExecutorService();
            	if(e != null) {
	            	try {
	            		//System.out.println("Shutting down async executor...");
	            		e.shutdown();
	            		//System.out.println("Completed shutdown of async executor.");
	            	} catch(Exception ex) {
	            		// Nothing to do
	            	}
            	}
            }
        }
	}

	/**
	 * Flush all messages from a queue.
	 * @param args command line arguments to main()
	**/
	static void flushQueue(String[] args) {
		String[] parameterNames = { "queue" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid FLUSHQUEUE command.");
			printUsage();
			return;
		}
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}

			// Try several times to get all messages from a queue.  Queues have been known to return no messages
			// for a while then to produce data long held.  Here, we must go several passes with time between
			// without messages before concluding the queue has been flushed.
			for(int emptyPasses=0;emptyPasses<3;emptyPasses++) {
				if(emptyPasses > 0) {
					// Sleep for a bit between loops.
					Thread.sleep(30L*1000L);
				}
				boolean done = false;
				while(!done) {
					done = true;

					String statusText = null;
		            ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL).withMaxNumberOfMessages(new Integer(10));
		            List<Message> messages = sqs.receiveMessage(receiveMessageRequest).getMessages();
		            for (Message message : messages) {
		                System.out.println("  Message");
		                System.out.println("    MessageId:     " + message.getMessageId());
		                System.out.println("    ReceiptHandle: " + message.getReceiptHandle());
		                System.out.println("    MD5OfBody:     " + message.getMD5OfBody());
		                System.out.println("    Body:          " + message.getBody());
		                statusText = message.getBody();
		                for (Map.Entry<String, String> entry : message.getAttributes().entrySet()) {
		                    System.out.println("  Attribute");
		                    System.out.println("    Name:  " + entry.getKey());
		                    System.out.println("    Value: " + entry.getValue());
		                }
		                // Delete the message from the queue
			            String messageRecieptHandle = message.getReceiptHandle();
			            sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageRecieptHandle));
						done = false;
						emptyPasses = 0; // start over again if we found anything.
		            }
				}
			}
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Get a job and execute an external program to process the job.  The external program
	 * must use the completejob command to finish the job.
	 * @param args command line arguments to main()
	**/
	static void getJob(String[] args) {
		getJobCore(args,false);
	}

	/**
	 * Get a job and execute an external program to process the job.  The external program
	 * must use the completejob command to finish the job.
	 * Waits for a job or until 45 minutes have elapsed.
	 * @param args command line arguments to main()
	**/
	static void waitForJob(String[] args) {
		getJobCore(args,true);
	}

	/**
	 * Get a job and execute an external program to process the job.  The external program
	 * must use the completejob command to finish the job.
	 * @param args command line arguments to main()
	 * @param waitForQueueEntry true if the routine should wait for a job or until 45 minutes has passed.
	**/
	static void getJobCore(String[] args, boolean waitForQueueEntry) {
		String[] parameterNames = { "queue", "command" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid GETJOB command.");
			printUsage();
			return;
		}
		long expirationTime = System.currentTimeMillis() + (waitForQueueEntry?(45L*60L*1000L): -1);
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}

			TreeMap<String,String> env = new TreeMap<String,String>();
			ArrayList<String> keys = new ArrayList<String>();
			boolean hasJob = false;
            String messageReceiptHandle = "";

			while(true) {
	            ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL).withMaxNumberOfMessages(new Integer(1));
	            List<Message> messages = sqs.receiveMessage(receiveMessageRequest).getMessages();
	            for (Message message : messages) {
	                System.out.println("  Message");
	                System.out.println("    MessageId:     " + message.getMessageId());
	                System.out.println("    ReceiptHandle: " + message.getReceiptHandle());
	                System.out.println("    MD5OfBody:     " + message.getMD5OfBody());
	                System.out.println("    Body:          " + message.getBody());
	                for (Map.Entry<String, String> entry : message.getAttributes().entrySet()) {
	                    System.out.println("  Attribute");
	                    System.out.println("    Name:  " + entry.getKey());
	                    System.out.println("    Value: " + entry.getValue());
	                }
	                String body= message.getBody();
	                String[] parts = body.split("\\t");
	                if(parts.length % 2 == 0) {
	                	for(int i=0;i<parts.length;i+=2) {
	                		env.put(parts[i+0],parts[i+1]);
	                		keys.add(parts[i+0]);
	                	}
	                }
	                // Delete the message from the queue
		            messageReceiptHandle = messages.get(0).getReceiptHandle();
		            env.put("MOVES_JOBID",messageReceiptHandle);
		            hasJob = true;
		            break;
	            }

				if(hasJob) {
					break;
				} else {
					if(waitForQueueEntry && System.currentTimeMillis() <= expirationTime) {
						Thread.sleep(2L*60L*1000L);
						continue;
					}
					return; // no message found
				}
			}

			// Loop over all found job jars in the bucket that match the prefix MOVES_JOBNAME (w/o the * of course)
			ArrayList<String> jobNames = new ArrayList<String>();
			String baseJobName = env.get("MOVES_JOBNAME");
			if(baseJobName == null || baseJobName.length() <= 0) {
	            sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageReceiptHandle));
				return;
			}
			if(baseJobName.endsWith("*")) {
				baseJobName = baseJobName.substring(0,baseJobName.length()-1);
				AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
				TreeSet<String> fileNames = S3.list(s3,env.get("MOVES_JOBBUCKET"),baseJobName);
				if(fileNames != null && fileNames.size() > 0) {
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						String name = i.next();
						//name = name.toLowerCase();
						if(name.startsWith(baseJobName) && name.toLowerCase().endsWith(".jar")) {
							jobNames.add(name.substring(0,name.length()-4));
						}
					}
				}
			} else {
				jobNames.add(baseJobName);
			}
			if(jobNames.size() <= 0) {
	            sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageReceiptHandle));
				return;
			}
			for(int ji=0;ji<jobNames.size();ji++) {
				ProcessBuilder builder = new ProcessBuilder(parameters.get("command"));
				Map<String,String> builderEnv = builder.environment();
				for(int i=0;i<keys.size();i++) {
					String key = keys.get(i);
					builderEnv.put(key,env.get(key));
				}
				builderEnv.put("MOVES_JOBNAME",jobNames.get(ji));
				builderEnv.put("MOVES_JOBID",ji!=0?"IGNORE":messageReceiptHandle);
				System.out.println("Executing job " + jobNames.get(ji));
				Process process = builder.start();
				process.waitFor();
			}
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Get a status message to a text-only queue and pass it to an external program.
	 * If no staus message is found, the external program is not launched.
	 * If a message is found, it is available to the external program via the MOVES_STATUS
	 * environment variable.
	 * @param args command line arguments to main()
	**/
	static void completeJob(String[] args) {
		String[] parameterNames = { "queue", "jobid" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid COMPLETEJOB command.");
			printUsage();
			return;
		}
		if(parameters.get("jobid").equalsIgnoreCase("IGNORE")) {
			return;
		}
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}

            String messageRecieptHandle = parameters.get("jobid");
            sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageRecieptHandle));
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Add a job to a queue.  All files for the job must already have been uploaded
	 * to buckets.
	 * @param args command line arguments to main()
	**/
	static void addJob(String[] args) {
		addJobCore(1,args,false);
	}

	/**
	 * Add a job to a queue.  All files for the job must already have been uploaded
	 * to buckets.
	 * @param mode 0: do not enqueue a command, 1: enqueue a command for the job, -1: enqueue a command for the batch
	 * @param args command line arguments to main()
	 * @return true if the job is ready to be processed
	**/
	static boolean addJobCore(int mode, String[] args, boolean forceEnqueue) {
		String[] parameterNames = {
			"jobqueue", "statusqueue", "databasebucket", "database",
			"codebucket", "code", "jobbucket", "jobdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid ADDJOB command.");
			printUsage();
			return false;
		}
		// Does nothing if the job folder contains status.queued file.
		// Does nothing if the job's folder already contains results*.jar, *.log, *.txt files, and/or an output/ folder. (like downloadjobresults)
		// Complains if the job folder is missing status.uploaded file.
		// Before enqueuing the job, deletes results_<batch>_<job>.jar if it exists from the bucket.
		// After enqueuing the job, creates status.queued file.
		AmazonSQSClient sqs = null;
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return false;
			}
			String jobName = jobDirectory.getName();
			File uploadedFile = new File(jobDirectory,"status.uploaded");
			if(!uploadedFile.exists()) {
				S3.log("ERROR: Job has not been uploaded: " + jobName);
				return false;
			}
			File queuedFile = new File(jobDirectory,"status.queued");
			if(!forceEnqueue && queuedFile.exists()) {
				return false;
			}
			if(!forceEnqueue) {
				File[] files = jobDirectory.listFiles();
				if(files != null && files.length > 0) {
					String outputDatabase = getOutputDatabase(jobDirectory);
					for(int i=0;i<files.length;i++) {
						String name = files[i].getName().toLowerCase();
						if(name.equals(outputDatabase)
								|| name.endsWith(".log")
								|| name.endsWith(".txt")
								|| (name.startsWith("results_") && name.endsWith(".jar"))
								|| (name.startsWith("db_results_") && name.endsWith(".jar"))
									) {
							return false;
						}
					}
				}
			}
			File batchDirectory = jobDirectory.getParentFile();
			String batchName = batchDirectory.getName();
			File jobJar = new File(jobDirectory,batchName+"_"+jobName+".jar");
			String batchDatabasesName = "";
			String scenarioDatabasesName = "";

			File batchDatabases = new File(batchDirectory,"databases");
			if(batchDatabases.exists()) {
				File batchDatabaseJar = new File(batchDirectory,"databases_"+batchDirectory.getName()+".jar");
				if(batchDatabaseJar.exists()) {
					batchDatabasesName = batchDatabaseJar.getName();
				}
			}

			File scenarioDatabases = findScenarioDatabases(batchDirectory);
			if(scenarioDatabases != null) {
				File scenarioDatabaseJar = new File(scenarioDatabases.getParentFile(),scenarioDatabases.getName()+".jar");
				if(scenarioDatabaseJar.exists()) {
					scenarioDatabasesName = scenarioDatabaseJar.getName();
				}
			}
/*
			String[] deleteParams = {
				"bucket", parameters.get("jobbucket"),
				"nameinbucket", "results_"+batchName+"_"+jobName+".jar"
			};
			S3.delete(deleteParams,true);

			String[] dbDeleteParams = {
				"bucket", parameters.get("jobbucket"),
				"nameinbucket", "db_results_"+batchName+"_"+jobName+".jar"
			};
			S3.delete(dbDeleteParams,true);
*/
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("jobqueue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return false;
			}
			if(mode != 0) {
				String body = "MOVES_DATABASEBUCKET\t" + parameters.get("databasebucket")
						+ "\tMOVES_DATABASENAME\t" + parameters.get("database")
						+ "\tMOVES_CODEBUCKET\t" + parameters.get("codebucket")
						+ "\tMOVES_CODENAME\t" + parameters.get("code")
						+ "\tMOVES_JOBBUCKET\t" + parameters.get("jobbucket")
						+ "\tMOVES_JOBNAME\t" + (mode==1?(batchName+"_"+jobName):(batchName+"_*"))
						+ "\tMOVES_STATUSQUEUE\t" + parameters.get("statusqueue")
						+ "\tMOVES_EXTRADBS1\t" + batchDatabasesName
						+ "\tMOVES_EXTRADBS2\t" + scenarioDatabasesName
						+ "\tMOVES_OUTPUTDB\t" + STANDARD_OUTPUTDB; // getOutputDatabase(jobDirectory);
	            sqs.sendMessage(new SendMessageRequest(queueURL, body));
	            System.out.println(body);
	            S3.log("Command added to the queue.");
			}
            createFile(queuedFile,"queued");
            return true;
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
        return false;
	}

	/** Queue URL keyed by queue name **/
	static TreeMap<String,String> queueURLCache = new TreeMap<String,String>();

	/**
	 * Obtain the URL of an existing queue without creating the queue which would
	 * result in an error if the timeout in the creation did not match the original
	 * creation timeout.
	 * @param sqs interface to Amazon services
	 * @param queueName queue to be checked
	 * @throws AmazonClientException if anything goes wrong
	 * @throws AmazonServiceException if anything goes wrong
	 * @throws Exception if anything goes wrong
	 * @returns the queue's URL if it already exists, else null.
	**/
	static String getQueueURL(AmazonSQS sqs, String queueName)
			throws AmazonClientException, AmazonServiceException, Exception {
		String cachedURL = queueURLCache.get(queueName);
		if(cachedURL != null) {
			return cachedURL;
		}

		String queueSuffix = "/" + queueName;
		String queueMid = "/" + queueName + "/";
		TreeSet<String> names = list(sqs);
		for(String url : names) {
			if(url.endsWith(queueSuffix) || url.indexOf(queueMid) >= 0) {
				queueURLCache.put(queueName,url);
				return url;
			}
		}
		return null;
	}

	/**
	 * List all queues URLs
	 * @param sqs interface to Amazon services
	 * @throws AmazonClientException if anything goes wrong
	 * @throws AmazonServiceException if anything goes wrong
	 * @throws Exception if anything goes wrong
	 * @returns set of queue names
	**/
	static TreeSet<String> list(AmazonSQS sqs)
			throws AmazonClientException, AmazonServiceException, Exception {
		TreeSet<String> names = new TreeSet<String>();
        for (String queueUrl : sqs.listQueues().getQueueUrls()) {
        	names.add(queueUrl);
        }
		return names;
	}

	/**
	 * Create a JAR file for a job using standard naming and folder conventions.
	 * @param args command line arguments to main()
	**/
	static void jarJob(String[] args) {
		String[] parameterNames = {
			"jobdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid JARJOB command.");
			printUsage();
			return;
		}
		File runspec = null;
		File originalRunSpec = null;
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return;
			}
			String jobName = jobDirectory.getName();
			runspec = new File(jobDirectory,jobName + ".mrs");
			if(!runspec.exists()) {
				S3.log("ERROR: Job RunSpec does not exist: " + runspec.getCanonicalPath());
				runspec = null;
				return;
			}
			originalRunSpec = new File(jobDirectory,"originalRunSpec.mrs");
			if(originalRunSpec.exists()) {
				runspec.delete();
				originalRunSpec.renameTo(runspec);
				originalRunSpec = new File(jobDirectory,"originalRunSpec.mrs"); // done to clear any existence caching
			}
			makeStandardRunSpec(runspec,originalRunSpec);

			File batchDirectory = jobDirectory.getParentFile();
			String batchName = batchDirectory.getName();
			File jobJar = new File(jobDirectory,batchName+"_"+jobName+".jar");
			if(jobJar.exists()) {
				System.out.println("job jar already exists: " + jobJar.getCanonicalPath());
				return;
				//System.out.println("Deleting existing JAR file: " + jobJar.getCanonicalPath());
				//jobJar.delete();
			}
			ArrayList<String> fileNames = new ArrayList<String>();
			fileNames.add(runspec.getName());
			fileNames.add("databases/");
			File databases = new File(jobDirectory,"databases");
			File batchDatabases = new File(jobDirectory,"../databases");
			ArrayList<File> roots = new ArrayList<File>();
			if(databases.exists()) {
				roots.add(databases);
			}
			/*
			if(batchDatabases.exists()) {
				roots.add(batchDatabases);
			}
			*/
			if(roots.size() > 0) {
				TreeSet<String> namesSeen = new TreeSet<String>();
				for(Iterator<File> ri=roots.iterator();ri.hasNext();) {
					File root = ri.next();
					File[] databaseNames = root.listFiles();
					if(databaseNames != null && databaseNames.length > 0) {
						for(int i=0;i<databaseNames.length;i++) {
							File db = databaseNames[i];
							String dbName = db.getName().toLowerCase();
							if(namesSeen.contains(dbName.toLowerCase())) {
								continue;
							}
							namesSeen.add(dbName.toLowerCase());
							dbName += "/";
							if(root == databases) {
								dbName = "databases/" + dbName;
							} else {
								dbName = "../databases/" + dbName;
							}
							fileNames.add(dbName);
							File[] dataFiles = db.listFiles();
							if(dataFiles != null && dataFiles.length > 0) {
								for(int j=0;j<dataFiles.length;j++) {
									String fileName = dbName + dataFiles[j].getName();
									fileNames.add(fileName);
								}
							}
						}
					}
				}
			}
			jarFilesCore(jobJar, jobDirectory, fileNames);

            S3.log("Job JAR created: " + jobJar.getCanonicalPath());
        } catch(Exception e) {
        	S3.log(e.toString());
        } finally {
        	if(runspec != null && originalRunSpec != null) {
        		try {
	        		if(originalRunSpec.exists()) {
	        			if(runspec.exists()) {
	        				runspec.delete();
	        			}
	        			originalRunSpec.renameTo(runspec);
	        		}
        		} catch(Exception e) {
        			S3.log("Unable to recover original RunSpec file: " + e.toString());
        		}
        	}
        }
	}

	/**
	 * Upload a job's JAR file, if it exists, to a bucket.
	 * @param args command line arguments to main()
	**/
	static void uploadJob(String[] args) {
		String[] parameterNames = {
			"jobdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid UPLOADJOB command.");
			printUsage();
			return;
		}
		/* Use S3's put routine:
		 * 	<li>put bucket <bucketname> nameinbucket <nameinbucket> file <filenameandpath><br>
		 *   Note: put creates the bucket if it does not exist and does not transfer if <nameinbucket> already exists.
		*/
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return;
			}
			String jobName = jobDirectory.getName();
			File runspec = new File(jobDirectory,jobName + ".mrs");
			if(!runspec.exists()) {
				S3.log("ERROR: Job RunSpec does not exist: " + runspec.getCanonicalPath());
				return;
			}
			File batchDirectory = jobDirectory.getParentFile();
			String batchName = batchDirectory.getName();
			File jobJar = new File(jobDirectory,batchName+"_"+jobName+".jar");
			if(!jobJar.exists()) {
				S3.log("Job JAR file does not exist: " + jobJar.getCanonicalPath());
				return;
			}
			File uploadedFile = new File(jobDirectory,"status.uploaded");
			if(uploadedFile.exists()) {
				System.out.println("Job already uploaded: " + jobName);
				return;
			}

			String[] putArgs = {
				"bucket", parameters.get("jobbucket"),
				"nameinbucket", jobJar.getName(),
				"file", jobJar.getCanonicalPath()
			};
			S3.put(putArgs);

			createFile(uploadedFile,"uploaded");
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Create a JAR file for each job in a batch.
	 * @param args command line arguments to main()
	**/
	static void jarJobs(String[] args) {
		String[] parameterNames = {
			"batchdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid JARJOBS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File[] folders = batchDirectory.listFiles();
			if(folders != null && folders.length > 0) {
				for(int i=0;i<folders.length;i++) {
					if(!folders[i].isDirectory()) {
						continue;
					}
					String jobName = folders[i].getName();
					if(jobName.equalsIgnoreCase("databases") || jobName.equalsIgnoreCase("output")) {
						continue;
					}
					String[] jobParameters = {
						"jobdir", folders[i].getCanonicalPath()
					};
					jarJob(jobParameters);
				}
			}
			// Create the batch's database jar
			File batchDatabases = new File(batchDirectory,"databases");
			if(batchDatabases.exists()) {
				File batchDatabaseJar = new File(batchDirectory,"databases_"+batchDirectory.getName()+".jar");
				if(!batchDatabaseJar.exists()) {
					ArrayList<String> fileNames = new ArrayList<String>();

					ArrayList<File> roots = new ArrayList<File>();
					roots.add(batchDatabases);
					TreeSet<String> namesSeen = new TreeSet<String>();
					for(Iterator<File> ri=roots.iterator();ri.hasNext();) {
						File root = ri.next();
						File[] databaseNames = root.listFiles();
						if(databaseNames != null && databaseNames.length > 0) {
							for(int i=0;i<databaseNames.length;i++) {
								File db = databaseNames[i];
								String dbName = db.getName().toLowerCase();
								if(namesSeen.contains(dbName.toLowerCase())) {
									continue;
								}
								namesSeen.add(dbName.toLowerCase());
								dbName += "/";
								fileNames.add(dbName);
								File[] dataFiles = db.listFiles();
								if(dataFiles != null && dataFiles.length > 0) {
									for(int j=0;j<dataFiles.length;j++) {
										String fileName = dbName + dataFiles[j].getName();
										fileNames.add(fileName);
									}
								}
							}
						}
					}
					jarFilesCore(batchDatabaseJar, batchDatabases, fileNames);
				}
			}
			// Create the scenario's database jar
			File scenarioDatabases = findScenarioDatabases(batchDirectory);
			if(scenarioDatabases != null) {
				File scenarioDatabaseJar = new File(scenarioDatabases.getParentFile(),scenarioDatabases.getName()+".jar");
				if(!scenarioDatabaseJar.exists()) {
					ArrayList<String> fileNames = new ArrayList<String>();

					ArrayList<File> roots = new ArrayList<File>();
					roots.add(scenarioDatabases);
					TreeSet<String> namesSeen = new TreeSet<String>();
					for(Iterator<File> ri=roots.iterator();ri.hasNext();) {
						File root = ri.next();
						File[] databaseNames = root.listFiles();
						if(databaseNames != null && databaseNames.length > 0) {
							for(int i=0;i<databaseNames.length;i++) {
								File db = databaseNames[i];
								String dbName = db.getName().toLowerCase();
								if(namesSeen.contains(dbName.toLowerCase())) {
									continue;
								}
								namesSeen.add(dbName.toLowerCase());
								dbName += "/";
								fileNames.add(dbName);
								File[] dataFiles = db.listFiles();
								if(dataFiles != null && dataFiles.length > 0) {
									for(int j=0;j<dataFiles.length;j++) {
										String fileName = dbName + dataFiles[j].getName();
										fileNames.add(fileName);
									}
								}
							}
						}
					}
					jarFilesCore(scenarioDatabaseJar, scenarioDatabases, fileNames);
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Upload all job JARs for a batch.
	 * @param args command line arguments to main()
	**/
	static void uploadJobs(String[] args) {
		String[] parameterNames = {
			"batchdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid UPLOADJOBS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File[] folders = batchDirectory.listFiles();
			if(folders != null && folders.length > 0) {
				for(int i=0;i<folders.length;i++) {
					if(!folders[i].isDirectory()) {
						continue;
					}
					String jobName = folders[i].getName();
					if(jobName.equalsIgnoreCase("databases") || jobName.equalsIgnoreCase("output")) {
						continue;
					}
					String[] jobParameters = {
						"jobdir", folders[i].getCanonicalPath(),
						"jobbucket", parameters.get("jobbucket")
					};
					uploadJob(jobParameters);
				}
			}
			// Upload the batch's database jar
			File batchDatabases = new File(batchDirectory,"databases");
			if(batchDatabases.exists()) {
				File batchDatabaseJar = new File(batchDirectory,"databases_"+batchDirectory.getName()+".jar");
				if(batchDatabaseJar.exists()) {
					String[] putArgs = {
						"bucket", parameters.get("jobbucket"),
						"nameinbucket", batchDatabaseJar.getName(),
						"file", batchDatabaseJar.getCanonicalPath()
					};
					S3.put(putArgs);
				}
			}

			// Upload the scenario's database jar
			File scenarioDatabases = findScenarioDatabases(batchDirectory);
			if(scenarioDatabases != null) {
				File scenarioDatabaseJar = new File(scenarioDatabases.getParentFile(),scenarioDatabases.getName()+".jar");
				if(scenarioDatabaseJar.exists()) {
					String[] putArgs = {
						"bucket", parameters.get("jobbucket"),
						"nameinbucket", scenarioDatabaseJar.getName(),
						"file", scenarioDatabaseJar.getCanonicalPath()
					};
					S3.put(putArgs);
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Locate a directory holding databases shared by all batches in the same logical scenario.
	 * @param batchDirectory directory for a batch
	 * @return existing directory of the form "databases_<scenario>" or null if the directory does not exist.
	**/
	static File findScenarioDatabases(File batchDirectory) {
		File[] folders = batchDirectory.getParentFile().listFiles();
		if(folders != null && folders.length > 0) {
			File scenarioDatabases = null;
			String scenarioName = "";
			String lowercaseBatchName = batchDirectory.getName().toLowerCase();
			for(int i=0;i<folders.length;i++) {
				String name = folders[i].getName().toLowerCase();
				if(name.startsWith("databases_") && name.length() > 10) {
					scenarioName = name.substring(10);
					if(lowercaseBatchName.startsWith(scenarioName)) {
						return folders[i];
					}
				}
			}
		}
		return null;
	}

	/**
	 * Create a JAR file for a job using standard naming and folder conventions.
	 * @param args command line arguments to main()
	**/
	static void jarJobResults(String[] args) {
		String[] parameterNames = {
			"jobdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid JARJOBRESULTS command.");
			printUsage();
			return;
		}
		boolean shouldSplitResults = false;
		String envFlag = System.getenv("SEPARATERESULTS");
		if(envFlag != null) {
			envFlag = envFlag.trim().toLowerCase();
		} else {
			envFlag = "";
		}
		if(envFlag != null &&
				(envFlag.startsWith("1") || envFlag.startsWith("y") || envFlag.startsWith("t"))
				) {
			System.out.println("Creating split output files");
			shouldSplitResults = true;
		}
		// Obtain *.log, *.txt, *.out, output/*
		// Do not obtain TODO/* or DONE/*
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return;
			}
			File resultsJar = new File(jobDirectory,"results_" + jobDirectory.getName() + ".jar");
			if(resultsJar.exists()) {
				resultsJar.delete();
			}
			File splitResultsJar = new File(jobDirectory,"db_results_" + jobDirectory.getName() + ".jar");
			if(splitResultsJar.exists()) {
				splitResultsJar.delete();
			}
			ArrayList<String> fileNames = new ArrayList<String>();
			ArrayList<String> splitFileNames = new ArrayList<String>();

			String outputDatabase = getOutputDatabase(jobDirectory);
			File outputDirectory = new File(jobDirectory,outputDatabase);

			if(outputDirectory.exists()) {
				if(shouldSplitResults) {
					// Add output database files relative to the output directory, i.e. without a path prefix.
					File[] outputFiles = outputDirectory.listFiles();
					if(outputFiles != null && outputFiles.length > 0) {
						for(int i=0;i<outputFiles.length;i++) {
							splitFileNames.add(outputFiles[i].getName());
						}
					}
				} else {
					// Add output database files relative to the job directory
					fileNames.add(outputDatabase + "/");
					File[] outputFiles = outputDirectory.listFiles();
					if(outputFiles != null && outputFiles.length > 0) {
						for(int i=0;i<outputFiles.length;i++) {
							fileNames.add(outputDatabase + "/" + outputFiles[i].getName());
						}
					}
				}
			}
			File[] jobFiles = jobDirectory.listFiles();
			if(jobFiles != null && jobFiles.length > 0) {
				long longestLength = -1;
				String longestFileName = "";

				for(int i=0;i<jobFiles.length;i++) {
					String t = jobFiles[i].getName().toLowerCase();
					if(t.endsWith(".log") || t.endsWith(".txt") || t.endsWith(".out")) {
						fileNames.add(jobFiles[i].getName());
						long len = jobFiles[i].length();
						if(longestLength <= 0 || len > longestLength) {
							longestLength = len;
							longestFileName = jobFiles[i].getName();
						}
					}
				}
				if(shouldSplitResults && fileNames.size() > 1) {
					// When splitting results, only keep the longest log file, which will be the O/S-level log
					// that includes the MOVES log information.
					fileNames.clear();
					fileNames.add(longestFileName);
				}
			}
			jarFilesCore(resultsJar,jobDirectory,fileNames);
			System.out.println("Created job results file: " + resultsJar.getCanonicalPath());

			if(splitFileNames.size() > 0) {
				jarFilesCore(splitResultsJar,outputDirectory,splitFileNames);
				System.out.println("Created split job results file: " + splitResultsJar.getCanonicalPath());
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Upload a job's results JAR file, if it exists, to a bucket.
	 * @param args command line arguments to main()
	**/
	static void uploadJobResults(String[] args) {
		String[] parameterNames = {
			"jobdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid UPLOADJOBRESULTS command.");
			printUsage();
			return;
		}
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return;
			}
			String jobName = jobDirectory.getName();
			File resultsJar = new File(jobDirectory,"results_"+jobName+".jar");
			if(!resultsJar.exists()) {
				S3.log("ERROR: Job results Jar does not exist: " + resultsJar.getCanonicalPath());
				return;
			}

			String[] putArgs = {
				"bucket", parameters.get("jobbucket"),
				"nameinbucket", resultsJar.getName(),
				"file", resultsJar.getCanonicalPath()
			};
			S3.put(putArgs);

			File dbResultsJar = new File(jobDirectory,"db_results_"+jobName+".jar");
			if(dbResultsJar.exists()) {
				String[] dbPutArgs = {
					"bucket", parameters.get("jobbucket"),
					"nameinbucket", dbResultsJar.getName(),
					"file", dbResultsJar.getCanonicalPath()
				};
				S3.put(dbPutArgs);
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Add all jobs in a batch.
	 * @param args command line arguments to main()
	**/
	static void reAddJobs(String[] args) {
		String[] parameterNames = {
			"jobqueue", "statusqueue", "databasebucket", "database",
			"codebucket", "code", "jobbucket", "batchdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid READDJOBS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File[] folders = batchDirectory.listFiles();
			if(folders != null && folders.length > 0) {
				boolean hasAnyJobsToRun = false;
				for(int i=0;i<folders.length;i++) {
					if(!folders[i].isDirectory()) {
						continue;
					}
					String jobName = folders[i].getName();
					if(jobName.equalsIgnoreCase("databases") || jobName.equalsIgnoreCase("output")) {
						continue;
					}
					File jobDirectory = folders[i];
					File queuedFile = new File(jobDirectory,"status.queued");
					if(!queuedFile.exists()) {
						hasAnyJobsToRun = true;
						continue;
					}
					File[] files = jobDirectory.listFiles();
					boolean hasResults = false;
					if(files != null && files.length > 0) {
						String outputDatabase = getOutputDatabase(jobDirectory);
						for(int j=0;j<files.length;j++) {
							String name = files[j].getName().toLowerCase();
							if(name.equals(outputDatabase)
									|| name.endsWith(".log")
									|| name.endsWith(".txt")
									|| (name.startsWith("results_") && name.endsWith(".jar"))
									|| (name.startsWith("db_results_") && name.endsWith(".jar"))
										) {
								hasResults = true;
								break;
							}
						}
					}
					if(!hasResults) {
						// No results exist but the job was queued.
						// Remove the queued file so the job will
						// be resubmitted for processing.
						queuedFile.delete();
						File cleanedFile = new File(jobDirectory,"status.cleaned");
						if(cleanedFile.exists()) {
							cleanedFile.delete();
						}
						File doneFile = new File(jobDirectory,"status.done");
						if(doneFile.exists()) {
							doneFile.delete();
						}
						hasAnyJobsToRun = true;
					}
				}
				if(hasAnyJobsToRun) {
					addJobs(args);
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Add all jobs in a batch.
	 * @param args command line arguments to main()
	**/
	static void addJobs(String[] args) {
		String[] parameterNames = {
			"jobqueue", "statusqueue", "databasebucket", "database",
			"codebucket", "code", "jobbucket", "batchdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid ADDJOBS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File[] folders = batchDirectory.listFiles();
			if(folders != null && folders.length > 0) {
				int jobMode = 0; // -1: add a command for the whole batch, 0: add no command
				ArrayList<File> jobFolders = new ArrayList<File>();
				for(int i=0;i<folders.length;i++) {
					if(!folders[i].isDirectory()) {
						continue;
					}
					String jobName = folders[i].getName();
					if(jobName.equalsIgnoreCase("databases") || jobName.equalsIgnoreCase("output")) {
						continue;
					}
					jobFolders.add(folders[i]);
				}
				// Add jobs, only enqueing a command for the last job since all data must exist before the command is enqueued.
				boolean anyJobsReady = false;
				for(int i=jobFolders.size()-1;i>=0;i--) {
					File jobFolder = jobFolders.get(i);
					String[] addParameters = {
						"jobqueue", parameters.get("jobqueue"),
						"statusqueue", parameters.get("statusqueue"),
						"databasebucket", parameters.get("databasebucket"),
						"database", parameters.get("database"),
						"codebucket", parameters.get("codebucket"),
						"code", parameters.get("code"),
						"jobbucket", parameters.get("jobbucket"),
						"jobdir", jobFolder.getCanonicalPath()
					};
					if(addJobCore(0, addParameters, false)) {
						anyJobsReady = true;
					}
				}
				if(anyJobsReady) {
					File jobFolder = jobFolders.get(0);
					String[] addParameters = {
						"jobqueue", parameters.get("jobqueue"),
						"statusqueue", parameters.get("statusqueue"),
						"databasebucket", parameters.get("databasebucket"),
						"database", parameters.get("database"),
						"codebucket", parameters.get("codebucket"),
						"code", parameters.get("code"),
						"jobbucket", parameters.get("jobbucket"),
						"jobdir", jobFolder.getCanonicalPath()
					};
					addJobCore(-1, addParameters, true);
					System.out.println("Batch enqueued: " + batchDirectory.getName());
				} else {
					System.out.println("Batch was *NOT* enqueued because no job needed to be or was ready to be run: " + batchDirectory.getName());
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Create a new status CSV <batch>_status.csv
	 * @param args command line arguments to main()
	**/
	static void batchStatus(String[] args) {
		String[] parameterNames = {
			"batchdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid BATCHSTATUS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			// Checks each job directory for status.* files [.uploaded, .queued, .done, .cleaned]
			String text = "Job,Uploaded,Queued,Done,Cleaned\n";
			String[] extensions = {
				"uploaded", "queued", "done", "cleaned"
			};
			File[] folders = batchDirectory.listFiles();
			if(folders != null && folders.length > 0) {
				for(int i=0;i<folders.length;i++) {
					if(!folders[i].isDirectory()) {
						continue;
					}
					String jobName = folders[i].getName();
					if(jobName.equalsIgnoreCase("databases") || jobName.equalsIgnoreCase("output")) {
						continue;
					}
					text += jobName;
					for(int j=0;j<extensions.length;j++) {
						text += ",";
						String t = "status." + extensions[j];
						File f = new File(folders[i],t);
						if(f.exists()) {
							text += extensions[j];
						}
					}
					text += "\n";
				}
			}
			File csv = new File(batchDirectory,batchDirectory.getName()+"_status.csv");
			if(csv.exists()) {
				csv.delete();
			}
			createFile(csv,text);
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Removes <batch>_<job>.jar and results_<batch>_<job>.jar files if they exist.
	 * Creates status.cleaned file in the job folder.
	 * @param args command line arguments to main()
	**/
	static void removeJobJars(String[] args) {
		String[] parameterNames = {
			"jobdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid REMOVEJOBJARS command.");
			printUsage();
			return;
		}
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return;
			}
			String jobName = jobDirectory.getName();
			File batchDirectory = jobDirectory.getParentFile();
			String batchName = batchDirectory.getName();
			File jobJar = new File(jobDirectory,batchName+"_"+jobName+".jar");

			String[] deleteNames = {
				batchName+"_"+jobName+".jar",
				"results_" + batchName+"_"+jobName+".jar"
				// Do not deleted db_results_*.jar
			};

			for(int i=0;i<deleteNames.length;i++) {
				String[] deleteArgs = {
					"bucket", parameters.get("jobbucket"),
					"nameinbucket", deleteNames[i]
				};
				S3.delete(deleteArgs);
			}

			File cleanedFile = new File(jobDirectory,"status.cleaned");
			createFile(cleanedFile,"cleaned");
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Download results from a bucket.
	 * downloadjobresults jobdir <jobdirectory> jobbucket <jobbucket>
	 * Does nothing if the job's folder already contains results*.jar, *.log, *.txt files, and/or an output/ folder.
	 * Checks <jobbucket> for results_<batch>_<job>.jar file, downloading it if available.
	 * After the download, removes the job's jar files from the bucket using removejobjars.
	 * Creates status.done file in the job folder.
	 * @param args command line arguments to main()
	**/
	static void downloadJobResults(String[] args) {
		String[] parameterNames = {
			"jobdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DOWNLOADJOBRESULTS command.");
			printUsage();
			return;
		}
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return;
			}
			String jobName = jobDirectory.getName();
			File batchDirectory = jobDirectory.getParentFile();
			String batchName = batchDirectory.getName();
			String outputDatabase = getOutputDatabase(jobDirectory);

			// If the results are already present, then don't download or extract anything
			boolean hasOutputDatabase = false;
			boolean hasLogFile = false;
			boolean hasDBResultsJar = false;

			File[] files = jobDirectory.listFiles();
			if(files != null && files.length > 0) {
				for(int i=0;i<files.length;i++) {
					String name = files[i].getName().toLowerCase();
					if(name.equalsIgnoreCase(outputDatabase)) {
						hasOutputDatabase = true;
					} else if(name.endsWith(".log") || name.endsWith(".txt")) {
						hasLogFile = true;
					} else if(name.startsWith("db_results_") && name.endsWith(".jar")) {
						hasDBResultsJar = true;
					}
				}
			}

			// If there is no sign of splitting, quit if there is a log file or an output database.
			if(!hasDBResultsJar && (hasLogFile || hasOutputDatabase)) {
				return;
			}
			// If there is sign of splitting, quit if there is a log file.
			if(hasDBResultsJar && hasLogFile) {
				return;
			}

			File resultsJar = new File(jobDirectory,"results_"+batchName+"_"+jobName+".jar");
			if(!resultsJar.exists()) {
				String[] getArgs = {
					"bucket", parameters.get("jobbucket"),
					"nameinbucket", resultsJar.getName(),
					"file", resultsJar.getCanonicalPath()
				};
				S3.get(getArgs);
			}
			if(resultsJar.exists()) {
				File doneFile = new File(jobDirectory,"status.done");
				if(!doneFile.exists()) {
					createFile(doneFile,"done");
					removeJobJars(args);
				}

				// Unjar the results file
				System.out.println("Unpacking: " + resultsJar.getCanonicalPath());
				unpackJar(jobDirectory,resultsJar);
				// Part a: only keep Download
				// Copy the oslog.txt file to the scenario's logqueue directory
				File logqueue = new File(batchDirectory.getParentFile(),"logqueue");
				if(!logqueue.exists()) {
					logqueue.mkdirs();
				}
				File osLog = new File(jobDirectory,"oslog.txt");
				if(osLog.exists()) {
					File logQueueFile = new File(logqueue,batchDirectory.getName() + "_" + jobDirectory.getName() + "_oslog.txt");
					if(!logQueueFile.exists()) {
						copyFile(osLog,logQueueFile,true);
					}
				}
				// Rename the STANDARD_OUTPUTDB folder to outputDatabase
				File standardOutput = new File(jobDirectory,STANDARD_OUTPUTDB);
				File outputDB = new File(jobDirectory,outputDatabase);
				if(standardOutput.exists() && !outputDB.exists()) {
					standardOutput.renameTo(outputDB);
				}
			} else {
				S3.log("Unable to unpack jar because it doesn't exist: " + resultsJar.getCanonicalPath());
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        	e.printStackTrace();
        }
	}

	/**
	 * Download results for each job in a batch
	 * @param args command line arguments to main()
	**/
	static void downloadResults(String[] args) {
		String[] parameterNames = {
			"batchdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DOWNLOADRESULTS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File[] folders = batchDirectory.listFiles();
			if(folders != null && folders.length > 0) {
				for(int i=0;i<folders.length;i++) {
					if(!folders[i].isDirectory()) {
						continue;
					}
					String jobName = folders[i].getName();
					if(jobName.equalsIgnoreCase("databases") || jobName.equalsIgnoreCase("output")) {
						continue;
					}
					String[] jobArgs = {
						"jobdir", folders[i].getCanonicalPath(),
						"jobbucket", parameters.get("jobbucket")
					};
					downloadJobResults(jobArgs);
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Download results from a bucket.
	 * downloadjobdbresults jobdir <jobdirectory> jobbucket <jobbucket>
	 * Does nothing if the job's folder already contains db_results*.jar and/or an output/ folder.
	 * Checks <jobbucket> for db_results_<batch>_<job>.jar file, downloading it if available.
	 * After the download, does not remove the job's jar files from the bucket.
	 * @param args command line arguments to main()
	**/
	static void downloadJobDBResults(String[] args) {
		String[] parameterNames = {
			"jobdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DOWNLOADJOBDBRESULTS command.");
			printUsage();
			return;
		}
		try {
			File jobDirectory = new File(parameters.get("jobdir"));
			if(!jobDirectory.exists()) {
				S3.log("ERROR: Job directory does not exist: " + jobDirectory.getCanonicalPath());
				return;
			}
			String jobName = jobDirectory.getName();
			File batchDirectory = jobDirectory.getParentFile();
			String batchName = batchDirectory.getName();
			String outputDatabase = getOutputDatabase(jobDirectory);

			// If the results are already present, then don't download or extract anything
			File[] files = jobDirectory.listFiles();
			if(files != null && files.length > 0) {
				for(int i=0;i<files.length;i++) {
					String name = files[i].getName().toLowerCase();
					if(name.equals(outputDatabase)) {
							//|| name.endsWith(".log")
							//|| name.endsWith(".txt")) {
							//|| (name.startsWith("results_") && name.endsWith(".jar"))) {
						return;
					}
				}
			}

			File resultsJar = new File(jobDirectory,"db_results_"+batchName+"_"+jobName+".jar");
			if(!resultsJar.exists()) {
				String[] getArgs = {
					"bucket", parameters.get("jobbucket"),
					"nameinbucket", resultsJar.getName(),
					"file", resultsJar.getCanonicalPath()
				};
				S3.get(getArgs);
			}
			if(resultsJar.exists()) {
				// Don't make a status.done file
				// Don't remove jars from the bucket

				// Unjar the results file
				File outputDirectory = new File(jobDirectory,outputDatabase);
				if(!outputDirectory.exists()) {
					outputDirectory.mkdir();
				}
				System.out.println("Unpacking: " + resultsJar.getCanonicalPath());
				unpackJar(outputDirectory,resultsJar);
				/*
				// Rename the STANDARD_OUTPUTDB folder to outputDatabase
				File standardOutput = new File(jobDirectory,STANDARD_OUTPUTDB);
				File outputDB = new File(jobDirectory,outputDatabase);
				if(standardOutput.exists() && !outputDB.exists()) {
					standardOutput.renameTo(outputDB);
				}
				*/
			} else {
				System.out.println("Unable to unpack jar because it doesn't exist: " + resultsJar.getCanonicalPath());
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        	e.printStackTrace();
        }
	}

	/**
	 * Download DB results for each job in a batch
	 * @param args command line arguments to main()
	**/
	static void downloadDBResults(String[] args) {
		String[] parameterNames = {
			"batchdir", "jobbucket"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DOWNLOADDBRESULTS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File[] folders = batchDirectory.listFiles();
			if(folders != null && folders.length > 0) {
				for(int i=0;i<folders.length;i++) {
					if(!folders[i].isDirectory()) {
						continue;
					}
					String jobName = folders[i].getName();
					if(jobName.equalsIgnoreCase("databases") || jobName.equalsIgnoreCase("output")) {
						continue;
					}
					String[] jobArgs = {
						"jobdir", folders[i].getCanonicalPath(),
						"jobbucket", parameters.get("jobbucket")
					};
					downloadJobDBResults(jobArgs);
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Terminate an instance
	 * terminate instanceid <instanceid>
	 * @param args command line arguments to main()
	**/
	static void terminate(String[] args) {
		String[] parameterNames = {
			"instanceid"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid TERMINATE command.");
			printUsage();
			return;
		}
		AmazonEC2Client ec2 = null;
		try {
			ec2 = new AmazonEC2Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			TerminateInstancesRequest request = new TerminateInstancesRequest();
			request.withInstanceIds(new String[] { parameters.get("instanceid") });
			ec2.terminateInstances(request);
			System.out.println("Instance termination requested for " + parameters.get("instanceid"));
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon EC2, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with EC2, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Internal function used to take several temporary files, JAR them into one single
	 * JAR file.  Path information relative to the passed base folder is stored.
	 * @param targetJarPath The file path to store the target JAR file at
	 * @param baseDirectory Base directory that contains all source files.
	 * @param sourceFilePaths An collection of file names, all relative to the base directory.
	 * @throws FileNotFoundException From some java.io operations.
	 * @throws IOException From some java.io operations.
	**/
	static void jarFilesCore(File targetJarPath, File baseDirectory, ArrayList<String> sourceFilePaths)
			throws FileNotFoundException, IOException {
		JarOutputStream jarStream = null;
		boolean isInEntry = false;
		BufferedInputStream inputStream = null;
		byte[] xferBuffer = new byte[1024*128];
		try {
			jarStream = new JarOutputStream(new FileOutputStream(targetJarPath.getCanonicalPath()));

			for(Iterator<String> i=sourceFilePaths.iterator();i.hasNext();) {
				String iterFilePath = i.next();
				//System.out.println("adding \"" + iterFilePath + "\"");
				File f = new File(baseDirectory,iterFilePath);
				if(iterFilePath.startsWith("../")) {
					// Remove the prefix since it was only used to find the file
					iterFilePath = iterFilePath.substring(3);
				}
				if(!f.exists()) {
					continue;
				}
				if(f.isDirectory()) {
					if(!iterFilePath.endsWith("/")) {
						iterFilePath += "/";
					}
					JarEntry jarEntry = new JarEntry(iterFilePath);
					jarEntry.setTime(f.lastModified());
					jarEntry.setMethod(JarOutputStream.STORED);
					jarEntry.setSize(0L);
					jarEntry.setCrc(0L);
					jarStream.putNextEntry(jarEntry);
					isInEntry = true;
					jarStream.closeEntry();
					isInEntry = false;
				} else {
					JarEntry jarEntry = new JarEntry(iterFilePath);
					inputStream = new BufferedInputStream(new FileInputStream(f));

					jarStream.putNextEntry(jarEntry);
					isInEntry = true;

					int iterByteCount;
					while ((iterByteCount = inputStream.read(xferBuffer)) > 0) {
						jarStream.write(xferBuffer, 0, iterByteCount);
					}

					inputStream.close();
					inputStream = null;

					jarStream.closeEntry();
					isInEntry = false;
				}
			}
		} finally {
			if(jarStream != null) {
				if(isInEntry) {
					try {
						jarStream.closeEntry();
					} catch(Exception e) {
						// Nothing to do here
					}
					isInEntry = false;
				}
				jarStream.close();
				jarStream = null;
			}
			if(inputStream != null) {
				try {
					inputStream.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				inputStream = null;
			}
		}
	}

	/**
	 * Create a small marker file filled with default text.
	 * @param f file to be created if it does not already exist
	 * @param text text to add
	**/
	static void createFile(File f, String text) {
		PrintWriter writer = null;
		try {
			if(f.exists()) {
				return;
			}
			writer = new PrintWriter(f);
			writer.println(text);
		} catch(Exception e) {
			S3.log("Unable to create file " + f.getName());
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
	 * Unjars a JAR file to the designated target folder. Doesn't handle path information.
	 * @param targetFolderPath The folder path to un jar the files to.
	 * @param srcJarFilePath The jar file to open.
	 * @throws FileNotFoundException From some java.io operations.
	 * @throws IOException From some java.io operations.
	**/
	static public void unpackJar(File targetFolderPath, File srcJarFilePath)
			throws FileNotFoundException, IOException {
		for(int i=2;i>=0;i--) {
			try {
				unpackJarCore(targetFolderPath,srcJarFilePath);
			} catch(FileNotFoundException e) {
				if(i == 0) {
					throw e;
				}
			} catch(IOException e) {
				if(i == 0) {
					throw e;
				}
			}
		}
	}

	/**
	 * Unjars a JAR file to the designated target folder. Doesn't handle path information.
	 * @param targetFolderPath The folder path to un jar the files to.
	 * @param srcJarFilePath The jar file to open.
	 * @throws FileNotFoundException From some java.io operations.
	 * @throws IOException From some java.io operations.
	**/
	static void unpackJarCore(File targetFolderPath, File srcJarFilePath)
			throws FileNotFoundException, IOException {
		byte[] xferBuffer = new byte[1024*128];
		FileInputStream fileInputStream = null;
		BufferedInputStream bufferedInputStream = null;
		JarInputStream jarInputStream = null;
		FileOutputStream targetFileStream = null;
		try {
			fileInputStream = new FileInputStream(srcJarFilePath);
			bufferedInputStream = new BufferedInputStream(fileInputStream);
			jarInputStream = new JarInputStream(bufferedInputStream);

			JarEntry jarEntry;

			while((jarEntry = jarInputStream.getNextJarEntry()) != null) {
				File targetFilePath = new File(targetFolderPath, jarEntry.getName());
				if(jarEntry.isDirectory()) {
					if(!targetFilePath.exists()) {
						targetFilePath.mkdirs();
					}
				} else {
					targetFileStream = new FileOutputStream(targetFilePath);

					int iterByteCount;
					while ((iterByteCount = jarInputStream.read(xferBuffer)) > 0) {
						targetFileStream.write(xferBuffer, 0, iterByteCount);
					}
					targetFileStream.close();
					targetFileStream = null;
				}
			}
		} finally {
			if(targetFileStream != null) {
				try {
					targetFileStream.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				targetFileStream = null;
			}
			if(jarInputStream != null) {
				try {
					jarInputStream.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				jarInputStream = null;
			}
			if(bufferedInputStream != null) {
				try {
					bufferedInputStream.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				bufferedInputStream = null;
			}
			if(fileInputStream != null) {
				try {
					fileInputStream.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fileInputStream = null;
			}
		}
	}

	/**
	 * Helper function to copy a file to destination file.
	 * @param sourceFilePath The file to copy.
	 * @param destinationFilePath The destination File to copy the file to.
	 * @param overWrite Indicates if the destination should be overwritten.
	 * @throws Exception if anything goes wrong
	**/
	public static void copyFile(File sourceFilePath, File destinationFilePath, boolean overWrite)
			throws Exception {
		BufferedInputStream fileIn = null;
		BufferedOutputStream fileOut = null;
		try {
			if(destinationFilePath.exists() && !overWrite) {
				return;
			}

			int bufferSize = 256*1024;
			byte[] buffer = new byte[bufferSize];

			fileIn = new BufferedInputStream(new FileInputStream(sourceFilePath),bufferSize);
			destinationFilePath.delete();
			// destinationFilePath.createNewFile();
			fileOut = new BufferedOutputStream(new FileOutputStream(destinationFilePath),bufferSize);
			int length;
			while((length = fileIn.read(buffer)) >= 0) {
				if(length > 0) {
					fileOut.write(buffer,0,length);
				}
			}
		} catch(Exception e) {
			throw e;
		} finally {
			if(fileIn != null) {
				try {
					fileIn.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if(fileOut != null) {
				try {
					fileOut.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/** Cache of output databases, keyed by full canonical path of the job directory **/
	static TreeMap<String,String> outputDatabases = new TreeMap<String,String>();

	/**
	 * Locate the output database name within a job's runspec.
	 * @param jobDirectory folder holding a job runspec.
	 * @return The output database name.
	 * @throws FileNotFoundException If the job's runspec file was not found.
	 * @throws IOException If there was an error reading the runspec file.
	**/
	static String getOutputDatabase(File jobDirectory) throws FileNotFoundException, IOException {
		String jobKey = jobDirectory.getCanonicalPath();
		String databaseName = outputDatabases.get(jobKey);
		if(databaseName != null) {
			return databaseName;
		}

		String jobName = jobDirectory.getName();
		File runspec = new File(jobDirectory,jobName + ".mrs");
		if(!runspec.exists()) {
			runspec = null;
			File[] files = jobDirectory.listFiles();
			if(files != null) {
				for(int i=0;i<files.length;i++) {
					if(files[i].getName().toLowerCase().endsWith(".mrs")) {
						runspec = files[i];
						break;
					}
				}
			}
			if(runspec == null) {
				throw new FileNotFoundException("ERROR: Job RunSpec does not exist: " + runspec.getCanonicalPath());
			}
		}
		char[] characterBuffer = new char[(int) runspec.length()];
		FileReader fileReader = null;
		try {
			fileReader = new FileReader(runspec);
			fileReader.read(characterBuffer);
		} finally {
			if(fileReader != null) {
				try {
					fileReader.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fileReader = null;
			}
		}
		String contents = new String(characterBuffer);
		// <outputdatabase servername="localhost" databasename="04021_2005_OZONEPMTOXICSGHG" description=""/>
		int startIndex = contents.indexOf("<outputdatabase");
		if(startIndex < 0) {
			throw new IOException("ERROR: No outputdatabase tag in runspec file");
		}
		int endIndex = contents.indexOf("/>",startIndex+1);
		if(endIndex < 0) {
			throw new IOException("ERROR: No end to outputdatabase tag in runspec file");
		}
		int databaseIndex = contents.indexOf("databasename",startIndex+1);
		if(databaseIndex < 0 || databaseIndex >= endIndex) {
			throw new IOException("ERROR: No database attribute in outputdatabase tag");
		}
		int startQuote = contents.indexOf("\"",databaseIndex+1);
		if(startQuote < 0 || startQuote >= endIndex) {
			throw new IOException("ERROR: Missing start quotation mark on database attribute in outputdatabase tag");
		}
		int endQuote = contents.indexOf("\"",startQuote+1);
		if(endQuote < 0 || endQuote >= endIndex) {
			throw new IOException("ERROR: Missing closing quotation mark on database attribute in outputdatabase tag");
		}
		databaseName = contents.substring(startQuote+1,endQuote).trim().toLowerCase();
		outputDatabases.put(jobKey,databaseName);
		//System.out.println("Job " + jobName + " outputs to " + databaseName);
		return databaseName;
	}

	/**
	 * Create a copy of a runspec and ensure it uses the STANDARD_OUTPUTDB database.
	 * @param runspec input file to be read and to be updated.
	 * @param originalRunSpec copy of the original input file.  Will be created by this routine.
	 * @throws FileNotFoundException if the input runspec could not be found.
	 * @throws IOException if anything goes wrong.
	**/
	static void makeStandardRunSpec(File runspec, File originalRunSpec) throws FileNotFoundException, IOException {
		char[] characterBuffer = new char[(int) runspec.length()];
		FileReader fileReader = null;
		FileWriter fileWriter = null;
		try {
			// Read the input version
			fileReader = new FileReader(runspec);
			fileReader.read(characterBuffer);

			// Make a copy into originalRunSpec
			fileWriter = new FileWriter(originalRunSpec);
			fileWriter.write(characterBuffer);
		} finally {
			if(fileReader != null) {
				try {
					fileReader.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fileReader = null;
			}
			if(fileWriter != null) {
				try {
					fileWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fileWriter = null;
			}
		}
		// Replace the output database
		String contents = new String(characterBuffer);
		// <outputdatabase servername="localhost" databasename="04021_2005_OZONEPMTOXICSGHG" description=""/>
		int startIndex = contents.indexOf("<outputdatabase");
		if(startIndex < 0) {
			throw new IOException("ERROR: No outputdatabase tag in runspec file");
		}
		int endIndex = contents.indexOf("/>",startIndex+1);
		if(endIndex < 0) {
			throw new IOException("ERROR: No end to outputdatabase tag in runspec file");
		}
		int databaseIndex = contents.indexOf("databasename",startIndex+1);
		if(databaseIndex < 0 || databaseIndex >= endIndex) {
			throw new IOException("ERROR: No database attribute in outputdatabase tag");
		}
		int startQuote = contents.indexOf("\"",databaseIndex+1);
		if(startQuote < 0 || startQuote >= endIndex) {
			throw new IOException("ERROR: Missing start quotation mark on database attribute in outputdatabase tag");
		}
		int endQuote = contents.indexOf("\"",startQuote+1);
		if(endQuote < 0 || endQuote >= endIndex) {
			throw new IOException("ERROR: Missing closing quotation mark on database attribute in outputdatabase tag");
		}
		String leftSide = contents.substring(0,startQuote+1);
		String rightSide = contents.substring(endQuote);
		String newContents = leftSide + STANDARD_OUTPUTDB + rightSide;
		try {
			// Make a copy into runspec
			fileWriter = new FileWriter(runspec);
			fileWriter.write(newContents);
		} finally {
			if(fileWriter != null) {
				try {
					fileWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fileWriter = null;
			}
		}
	}

	/** Utility string used for numeric padding **/
	static final String zeroPadding = "00000000000000000000"; // 20 zeros
						    		 //12345678901234567890
				    				 //         1         2

	/**
	 * Place leading zeros on a number, making a specific field width
	 * @param number value to be converted and left padded with zeros
	 * @param length length to extend or clip the number and its zeros to
	 * @return text padded with zeros or clipped to be exactly length characters
	**/
	public static String leftZeroPad(int number, int length) {
		String text = "" + number;
		if(text.length() == length) {
			return text;
		} else if(text.length() < length) {
			String result = text;
			while(result.length() < length) {
				int lengthToAdd = length - result.length();
				if(lengthToAdd >= zeroPadding.length()) {
					result = zeroPadding + result;
				} else {
					result = zeroPadding.substring(0,lengthToAdd) + result;
					break;
				}
			}
			return result;
		} else {
			return text.substring(0,length);
		}
	}

	/**
	 * Get the current date and time.
	 * @return current date and time in format: yyyymmdd hhMMss
	**/
	public static String datetime() {
		int nowMillis = (int)(System.currentTimeMillis() % 1000);
		Calendar c = Calendar.getInstance();
		return leftZeroPad(c.get(Calendar.YEAR),4)
				+ leftZeroPad(c.get(Calendar.MONTH)+1,2)
				+ leftZeroPad(c.get(Calendar.DAY_OF_MONTH),2)
				+ " "
				+ leftZeroPad(c.get(Calendar.HOUR_OF_DAY),2)
				+ leftZeroPad(c.get(Calendar.MINUTE),2)
				+ leftZeroPad(c.get(Calendar.SECOND),2)
				+ "." + leftZeroPad(nowMillis,3);
	}

	/**
	 * Add a batch to post processing.
	 * @param args command line arguments to main()
	**/
	static void reAddPostProcess(String[] args) {
		String[] parameterNames = {
			"jobqueue", "statusqueue", "codebucket", "code", "jobbucket", "batchdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid READDPOSTPROCESS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File postQueuedFile = new File(batchDirectory,"status.postqueued");
			boolean hasOutputDirectory = false;
			boolean hasPostResults = false;
			File[] files = batchDirectory.listFiles();
			if(files != null && files.length > 0) {
				for(int i=0;i<files.length;i++) {
					String name = files[i].getName().toLowerCase();
					if(name.equalsIgnoreCase("output")) {
						hasOutputDirectory = true;
					} else if(name.startsWith("post_results_") && name.endsWith(".jar")) {
						hasPostResults = true;
					}
				}
				if(!hasOutputDirectory || !hasPostResults) {
					// No results exist but the job was queued.
					// Remove the queued file so the job will
					// be resubmitted for processing.
					postQueuedFile.delete();
					addPostProcess(args);
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Post process all jobs in a batch.
	 * @param args command line arguments to main()
	**/
	static void addPostProcess(String[] args) {
		String[] parameterNames = {
			"jobqueue", "statusqueue", "codebucket", "code", "jobbucket", "batchdir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid ADDPOSTPROCESS command.");
			printUsage();
			return;
		}
		AmazonSQSClient sqs = null;
		AmazonS3 s3 = null;
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			File postQueuedFile = new File(batchDirectory,"status.postqueued");
			if(postQueuedFile.exists()) {
				System.out.println("Batch already queued for post processing: " + batchDirectory.getCanonicalPath());
				return;
			}
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			String jobBucketName = parameters.get("jobbucket");
			// Ensure jobbucket contains at least one db_results_<batch>*.jar file
			TreeSet<String> foundFiles = S3.list(s3,jobBucketName,"db_results_" + batchDirectory.getName());
			if(foundFiles == null || foundFiles.size() <= 0) {
				S3.log("Not queued because the batch has no db_results_*.jar files in the job bucket: " + batchDirectory.getCanonicalPath());
				return;
			}
			// Add the queue entry
			String queueName = parameters.get("jobqueue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}
			String body = "MOVES_BATCH\t" + batchDirectory.getName()
					+ "\tMOVES_CODEBUCKET\t" + parameters.get("codebucket")
					+ "\tMOVES_CODENAME\t" + parameters.get("code")
					+ "\tMOVES_JOBBUCKET\t" + parameters.get("jobbucket")
					+ "\tMOVES_JOBNAME\t" + batchDirectory.getName()
					+ "\tMOVES_STATUSQUEUE\t" + parameters.get("statusqueue");
            sqs.sendMessage(new SendMessageRequest(queueURL, body));
            System.out.println(body);
            System.out.println("Post processing command added to the queue.");

            createFile(postQueuedFile,"postqueued");
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Get a post process batch command and execute an external program to process it.
	 * @param args command line arguments to main()
	**/
	static void getPostProcess(String[] args) {
		boolean waitForQueueEntry = true;
		String[] parameterNames = { "queue", "command", "todir" };
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid GETPOSTPROCESS command.");
			printUsage();
			return;
		}
		long expirationTime = System.currentTimeMillis() + (waitForQueueEntry?(45L*60L*1000L): -1);
		AmazonSQSClient sqs = null;
		try {
			sqs = new AmazonSQSClient(new PropertiesCredentials(new File(S3.AUTH_FILE)));

			String queueName = parameters.get("queue");
			String queueURL = getQueueURL(sqs,queueName);
			if(queueURL == null) {
				S3.log("Queue \"" + queueName + "\" does not exist.");
				return;
			}

			TreeMap<String,String> env = new TreeMap<String,String>();
			ArrayList<String> keys = new ArrayList<String>();
			boolean hasJob = false;
            String messageReceiptHandle = "";

			while(true) {
	            ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL).withMaxNumberOfMessages(new Integer(1));
	            List<Message> messages = sqs.receiveMessage(receiveMessageRequest).getMessages();
	            for (Message message : messages) {
	                System.out.println("  Message");
	                System.out.println("    MessageId:     " + message.getMessageId());
	                System.out.println("    ReceiptHandle: " + message.getReceiptHandle());
	                System.out.println("    MD5OfBody:     " + message.getMD5OfBody());
	                System.out.println("    Body:          " + message.getBody());
	                for (Map.Entry<String, String> entry : message.getAttributes().entrySet()) {
	                    System.out.println("  Attribute");
	                    System.out.println("    Name:  " + entry.getKey());
	                    System.out.println("    Value: " + entry.getValue());
	                }
	                String body= message.getBody();
	                String[] parts = body.split("\\t");
	                if(parts.length % 2 == 0) {
	                	for(int i=0;i<parts.length;i+=2) {
	                		env.put(parts[i+0],parts[i+1]);
	                		keys.add(parts[i+0]);
	                	}
	                }
	                // Delete the message from the queue
		            messageReceiptHandle = messages.get(0).getReceiptHandle();
	                sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageReceiptHandle));
		            hasJob = true;
		            break;
	            }

				if(hasJob) {
					break;
				} else {
					if(waitForQueueEntry && System.currentTimeMillis() <= expirationTime) {
						Thread.sleep(2L*60L*1000L);
						continue;
					}
					return; // no message found
				}
			}

			if(!hasJob) {
				return;
			}
			String batchName = env.get("MOVES_BATCH");
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			TreeSet<String> dbResultFiles = new TreeSet<String>();
			TreeSet<String> fileNames = S3.list(s3,env.get("MOVES_JOBBUCKET"),"db_results_" + batchName);
			if(fileNames != null && fileNames.size() > 0) {
				for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
					String name = i.next();
					if(name.startsWith("db_results_") && name.toLowerCase().endsWith(".jar")) {
						dbResultFiles.add(name);
					}
				}
			}
			if(dbResultFiles.size() <= 0) {
				// No db_results_<batch>*.jar files found
				return;
			}
			File toDir = new File(parameters.get("todir"));
			for(Iterator<String> i=dbResultFiles.iterator();i.hasNext();) {
				String name = i.next();
				File destinationFile = new File(toDir,name);
				String destinationFileText = destinationFile.getCanonicalPath();
				String[] getParameters = {
					"bucket", env.get("MOVES_JOBBUCKET"),
					"nameinbucket", name,
					"file", destinationFileText
				};
				S3.get(getParameters);
				if(destinationFile.exists()) {
					String destinationDirName = destinationFileText.substring(0,destinationFileText.length()-4);
					File destinationDir = new File(destinationDirName);
					if(!destinationDir.exists()) {
						destinationDir.mkdir();
						unpackJar(destinationDir,destinationFile);
					}
				}
			}

			ProcessBuilder builder = new ProcessBuilder(parameters.get("command"));
			Map<String,String> builderEnv = builder.environment();
			for(int i=0;i<keys.size();i++) {
				String key = keys.get(i);
				builderEnv.put(key,env.get(key));
			}
			builderEnv.put("MOVES_JOBID",messageReceiptHandle);
			builderEnv.put("MOVES_BATCHID",messageReceiptHandle);
			System.out.println("Executing post processor job " + batchName);
			Process process = builder.start();
			process.waitFor();
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Download post processing results for a batch
	 * @param args command line arguments to main()
	**/
	static void downloadPostResults(String[] args) {
		String[] parameterNames = {
			"batchdir", "jobbucket", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DOWNLOADPOSTRESULTS command.");
			printUsage();
			return;
		}
		try {
			File batchDirectory = new File(parameters.get("batchdir"));
			if(!batchDirectory.exists()) {
				S3.log("ERROR: Batch directory does not exist: " + batchDirectory.getCanonicalPath());
				return;
			}
			String postVersion = parameters.get("code").trim();
			/*
			File resultsFile = new File(batchDirectory,"post_results_" + batchDirectory.getName() + ".jar");
			if(!resultsFile.exists()) {
				String[] getParameters = {
					"bucket", parameters.get("jobbucket"),
					"nameinbucket", resultsFile.getName(),
					"file", resultsFile.getCanonicalPath()
				};
				S3.get(getParameters);
			}
			File outputDirectory = new File(batchDirectory,"output");
			if(!outputDirectory.exists() && resultsFile.exists()) {
				outputDirectory.mkdir();
				unpackJar(outputDirectory,resultsFile);
			}
			// Copy the oslog.txt file to the scenario's logqueue directory
			File logqueue = new File(batchDirectory.getParentFile(),"logqueue");
			if(!logqueue.exists()) {
				logqueue.mkdirs();
			}
			File osLog = new File(outputDirectory,"oslog.txt");
			if(osLog.exists()) {
				File logQueueFile = new File(logqueue,"post_" + batchDirectory.getName() + "_oslog.txt");
				if(!logQueueFile.exists()) {
					copyFile(osLog,logQueueFile,true);
				}
			}
			*/
			String batchName = batchDirectory.getName();
			String suffixLowerCase = "_" + batchName.toLowerCase() + ".jar";
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			TreeSet<String> fileNames = S3.list(s3,parameters.get("jobbucket"),"post_results_" + postVersion);
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
			if(fileNames != null && fileNames.size() > 0) {
				for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
					String name = i.next();
					String nameLowerCase = name.toLowerCase();
					if(!nameLowerCase.endsWith(suffixLowerCase)) {
						continue;
					}
					// Find the postVersion that is given in the name.  It may not exist in older files.
					// post_results_POSTVERSION[suffixLowerCase]
					// post_results[suffixLowerCase]
					String filePostVersion = ""; // no leading or trailing underscore
					String filePostVersionInsert = ""; // no leading underscore, has trailing underscore when filePostVersion is not blank
					if(name.length() > suffixLowerCase.length() + 13) { // 13 == "post_results_"
						filePostVersion = name.substring(13,name.length()-suffixLowerCase.length());
						filePostVersionInsert = filePostVersion + "_";
					}
					File resultsFile = new File(batchDirectory,"post_results_" + filePostVersionInsert + batchDirectory.getName() + ".jar");
					if(!resultsFile.exists()) {
						String[] getParameters = {
							"bucket", parameters.get("jobbucket"),
							"nameinbucket", resultsFile.getName(),
							"file", resultsFile.getCanonicalPath()
						};
						S3.get(getParameters);
					}
					File outputDirectory = new File(batchDirectory,filePostVersion.length() > 0? ("output_" + filePostVersion) : "output");
					if(!outputDirectory.exists() && resultsFile.exists()) {
						outputDirectory.mkdir();
						unpackJar(outputDirectory,resultsFile);
					}
					// Copy the oslog.txt file to the scenario's logqueue directory
					File logqueue = new File(batchDirectory.getParentFile(),"logqueue");
					if(!logqueue.exists()) {
						logqueue.mkdirs();
					}
					File osLog = new File(outputDirectory,"oslog.txt");
					if(osLog.exists()) {
						File logQueueFile = new File(logqueue,"post_" + filePostVersionInsert + batchDirectory.getName() + "_oslog.txt");
						if(!logQueueFile.exists()) {
							copyFile(osLog,logQueueFile,true);
						}
					}
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Download post processing results for all batches
	 * @param args command line arguments to main()
	**/
	static void downloadAllPostResults(String[] args) {
		String[] parameterNames = {
			"todir", "jobbucket", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid DOWNLOADALLPOSTRESULTS command.");
			printUsage();
			return;
		}
		try {
			File toDirectory = new File(parameters.get("todir"));
			if(!toDirectory.exists()) {
				S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
				return;
			}
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			TreeSet<String> fileNames = S3.list(s3,parameters.get("jobbucket"),"post_results_" + postVersion);
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
			if(fileNames != null && fileNames.size() > 0) {
				for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
					String name = i.next();
					File localFile = new File(toDirectory,name);
					if(!localFile.exists()) {
						String[] getParameters = {
							"bucket", parameters.get("jobbucket"),
							"nameinbucket", name,
							"file", localFile.getCanonicalPath()
						};
						S3.get(getParameters);
						if(localFile.exists()) {
							// Find the postVersion that is given in the name.  It may not exist in older files.
							// post_results_POSTVERSION_POSTDATE_CASE_CASEVERSION_COUNTY_MONTH.jar
							// post_results_CASE_CASEVERSION_COUNTY_MONTH.jar
							String filePostVersion = "";
							int previousIndex = name.length();
							for(int j=0;j<4;j++) {
								previousIndex = name.lastIndexOf('_',previousIndex);
								if(previousIndex < 0) {
									// The file does not conform to even our minimum naming conventions.
									// Skip it.
									break;
								}
							}
							if(previousIndex < 0) {
								continue; // skip files that don't meet even the minimum naming convention
							}
							if(previousIndex > 12) { // 12 == "post_results"
								filePostVersion = name.substring(13,previousIndex-13);
							}
							File outputDirectory = new File(toDirectory,filePostVersion.length() > 0? ("output_" + filePostVersion) : "output");
							if(!outputDirectory.exists()) {
								outputDirectory.mkdir();
							}
							unpackJar(outputDirectory,localFile);
						}
					}
				}
			}
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Recursively delete all children of a directory, but not the directory itsef.
	 * @param f directory to be emptied
	**/
	static void deleteDirectoryContents(File f) {
		try {
			File[] children = f.listFiles();
			if(children != null && children.length > 0) {
				for(int i=0;i<children.length;i++) {
					if(children[i].isDirectory()) {
						deleteDirectoryContents(children[i]);
					}
					children[i].delete();
				}
			}
		} catch(Exception e) {
			// Nothing to do here
		}
	}

	/**
	 * Convert a results_*.jar file that holds both an output database and logs into
	 * a results_*.jar for the logs and a db_results_*.jar for the output database.
	 * @param args command line arguments to main()
	**/
	static void splitResults(String[] args) {
		String[] parameterNames = {
			"fromdir", "todir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid SPLITRESULTS command.");
			printUsage();
			return;
		}
		try {
			File fromDirectory = new File(parameters.get("fromdir"));
			if(!fromDirectory.exists()) {
				S3.log("ERROR: fromDirectory does not exist: " + fromDirectory.getCanonicalPath());
				return;
			}
			File toDirectory = new File(parameters.get("todir"));
			if(!toDirectory.exists()) {
				S3.log("ERROR: toDirectory does not exist: " + toDirectory.getCanonicalPath());
				return;
			}
			File tempDirectory = new File(fromDirectory,"splittemp");
			if(tempDirectory.exists()) {
				deleteDirectoryContents(tempDirectory);
			} else {
				tempDirectory.mkdir();
			}
			File[] fromFiles = fromDirectory.listFiles();
			if(fromFiles != null) {
				for(int fi=0;fi<fromFiles.length;fi++) {
					String fname = fromFiles[fi].getName().toLowerCase();
					if(fname.startsWith("results_") && fname.endsWith(".jar")) {
						deleteDirectoryContents(tempDirectory);
						unpackJar(tempDirectory,fromFiles[fi]);
						// jarFilesCore(File targetJarPath, File baseDirectory, ArrayList<String> sourceFilePaths)
						File[] extractedFiles = tempDirectory.listFiles();
						if(extractedFiles != null && extractedFiles.length > 0) {
							// Make the new results_*.jar
							ArrayList<String> logFiles = new ArrayList<String>();
							File osLog = new File(tempDirectory,"oslog.txt");
							if(osLog.exists()) {
								logFiles.add(osLog.getName());
							} else {
								for(int ei=0;ei<extractedFiles.length;ei++) {
									if(extractedFiles[ei].isFile()) {
										logFiles.add(extractedFiles[ei].getName());
									}
								}
							}
							if(logFiles.size() > 0) {
								File resultsJar = new File(toDirectory,fromFiles[fi].getName());
								if(resultsJar.exists()) {
									resultsJar.delete();
								}
								jarFilesCore(resultsJar,tempDirectory,logFiles);
							}
							// Make the new db_results_*.jar
							File dbDirectory = null;
							ArrayList<String> dbFiles = new ArrayList<String>();
							for(int ei=0;ei<extractedFiles.length;ei++) {
								if(extractedFiles[ei].isDirectory()) {
									dbDirectory = extractedFiles[ei];
									File[] dbf = dbDirectory.listFiles();
									if(dbf != null && dbf.length > 0) {
										for(int di=0;di<dbf.length;di++) {
											dbFiles.add(dbf[di].getName());
										}
									}
									break;
								}
							}
							if(dbFiles.size() > 0) {
								File dbResultsJar = new File(toDirectory,"db_"+fromFiles[fi].getName());
								if(dbResultsJar.exists()) {
									dbResultsJar.delete();
								}
								jarFilesCore(dbResultsJar,dbDirectory,dbFiles);
							}
							// Rename the initial file
							File usedFile = new File(fromDirectory,"Processed." + fromFiles[fi].getName());
							if(usedFile.exists()) {
								usedFile.delete();
							}
							fromFiles[fi].renameTo(usedFile);
						}
					}
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	/**
	 * Check each log file in a logqueue directoy.  Files without error are renamed as "OK.*".
	 * Files with an error or mismatch get renamed to "Failed.*"
	 * @param args command line arguments to main()
	**/
	static void scanLogs(String[] args) {
		String[] parameterNames = {
			"logqueuedir"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid SCANLOGS command.");
			printUsage();
			return;
		}
		try {
			File logDirectory = new File(parameters.get("logqueuedir"));
			if(!logDirectory.exists()) {
				S3.log("ERROR: logQueueDir does not exist: " + logDirectory.getCanonicalPath());
				return;
			}
			File[] logFiles = logDirectory.listFiles();
			if(logFiles != null) {
				for(int fi=0;fi<logFiles.length;fi++) {
					String fname = logFiles[fi].getName().toLowerCase();
					if(!fname.startsWith("ok.") && !fname.startsWith("failed.") && !fname.startsWith("post_")) {
						File newFile;
						if(isLogOK(logFiles[fi])) {
							newFile = new File(logDirectory,"OK." + logFiles[fi].getName());
						} else {
							newFile = new File(logDirectory,"Failed." + logFiles[fi].getName());
						}
						if(newFile.exists()) {
							newFile.delete();
						}
						logFiles[fi].renameTo(newFile);
					}
				}
			}
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}

	static boolean isLogOK(File logFile) throws Exception {
		// [java] 3/26/11 6:06 AM INFO: 3 bundles will be used.
		//	" bundles will be used."
		// [java] 3/26/11 6:13 AM INFO: Generated bundles = 3 Retrieved bundles = 3
		//	"Generated bundles = "
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new FileReader(logFile),128*1024);
			String line = "";
			String countText = "";
			int state = 0;
			int index;
			// Looking for " bundles will be used."
			while((line = reader.readLine()) != null) {
				index = line.indexOf(" bundles will be used.");
				if(index > 0) {
					state = 10;
					int prefixIndex = line.indexOf(" INFO: ");
					if(prefixIndex < 0) {
						return false;
					}
					countText = line.substring(prefixIndex+7,index).trim();
					if(countText.length() <= 0) {
						return false;
					}
					countText = "Generated bundles = " + countText + " Retrieved bundles = " + countText;
					break;
				}
			}
			if(state == 0) {
				// Quit if we didn't find the line
				return false;
			}
			// Looking for "Generated bundles = "
			while((line = reader.readLine()) != null) {
				index = line.indexOf("Generated bundles =");
				if(index > 0) {
					return line.indexOf(countText) >= 0;
				}
			}
			// No "Generated bundles = " text found.
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
		}
	}
	/**
	 * Download databases*.jar
	 * @param args command line arguments to main()
	**/
	static void downloadDatabaseJar(String[] args) {
		String[] parameterNames = {
			"todir", "bucketfile", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid downloadDatabaseJar command.");
			printUsage();
			return;
		}
		try {
			
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			File f = new File(parameters.get("bucketfile"));
			BufferedReader b = new BufferedReader(new FileReader(f));
			List<Bucket> buckets = s3.listBuckets();
            String bucketName = "";
            File toDirectory;
            while ((bucketName = b.readLine()) != null) {
            	toDirectory = new File(parameters.get("todir") + "\\"+bucketName);
    			if(!toDirectory.exists()) {
    				toDirectory.mkdirs();
    				//S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
    				//return;
    			}
	        	S3.log("Bucket name:" + bucketName);
	        	TreeSet<String> fileNames = S3.list(s3,bucketName,"databases" );
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
				
				
				if(fileNames != null && fileNames.size() > 0) {
					
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						String name = i.next();
						S3.log(name);
						File localFile = new File(toDirectory,name);
						
						if(!localFile.exists()) {
							String[] getParameters = {
								"bucket", bucketName,
								"nameinbucket", name,
								"file", localFile.getCanonicalPath()
							};
							S3.get(getParameters);
							
						}
					}
				}
	        }
	        
			
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}
	/**
	 * Download <bucketname>*.jar
	 * @param args command line arguments to main()
	**/
	static void downloadBucketNameJar(String[] args) {
		String[] parameterNames = {
			"todir", "bucketfile", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid downloadBucketNameJar command.");
			printUsage();
			return;
		}
		try {
			
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			File f = new File(parameters.get("bucketfile"));
			BufferedReader b = new BufferedReader(new FileReader(f));

            String bucketName = "";
            File toDirectory;
            while ((bucketName = b.readLine()) != null) {
            	toDirectory = new File(parameters.get("todir") + "\\"+bucketName);
    			if(!toDirectory.exists()) {
    				toDirectory.mkdirs();
    				//S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
    				//return;
    			}

	        	S3.log("Bucket name:" + bucketName);
	        	TreeSet<String> fileNames = S3.list(s3,bucketName,bucketName);
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
				
				if(fileNames != null && fileNames.size() > 0) {
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						String name = i.next();
						S3.log(name);
						File localFile = new File(toDirectory,name);
						if(!localFile.exists()) {
							String[] getParameters = {
								"bucket", bucketName,
								"nameinbucket", name,
								"file", localFile.getCanonicalPath()
							};
							S3.get(getParameters);
						}
					}
				}
	        }
	        
			
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}
	/**
	 * Download db_results*_prof*.jar
	 * @param args command line arguments to main()
	**/
	static void downloadDbResultProfileJar(String[] args) {
		String[] parameterNames = {
			"todir", "bucketfile", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid db_results*_prof*.jar command.");
			printUsage();
			return;
		}
		try {
			
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			File f = new File(parameters.get("bucketfile"));
			BufferedReader b = new BufferedReader(new FileReader(f));

            String bucketName = "";
            File toDirectory;
            while ((bucketName = b.readLine()) != null) {
            	toDirectory = new File(parameters.get("todir") + "\\"+bucketName);
    			if(!toDirectory.exists()) {
    				toDirectory.mkdirs();
    				//S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
    				//return;
    			}
	        	S3.log("Bucket name:" + bucketName);
	        	TreeSet<String> fileNames = S3.list(s3,bucketName,"db_results");
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
				if(fileNames != null && fileNames.size() > 0) {
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						String name = i.next();
						S3.log(name);
						// Check name ith pattern  db_results*_prof*
						String expression = "^db_result.*_prof.*.jar"; 
						if (!name.matches(expression)) {
							continue;
						}
						File localFile = new File(toDirectory,name);
						if(!localFile.exists()) {
							String[] getParameters = {
								"bucket", bucketName,
								"nameinbucket", name,
								"file", localFile.getCanonicalPath()
							};
							S3.get(getParameters);
							break;
//							
						}
					}
				}
	        }
	        
			
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}
	/**
	 * Download Moves*db*.jar
	 * @param args command line arguments to main()
	**/
	static void downloadMovesDbJar(String[] args) {
		String[] parameterNames = {
			"todir", "bucketfile", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid downloadMovesDbJar command.");
			printUsage();
			return;
		}
		try {
			
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			File f = new File(parameters.get("bucketfile"));
			BufferedReader b = new BufferedReader(new FileReader(f));

            String bucketName = "";
            File toDirectory;
            while ((bucketName = b.readLine()) != null) {
            	toDirectory = new File(parameters.get("todir") + "\\"+bucketName);
    			if(!toDirectory.exists()) {
    				toDirectory.mkdirs();
    				//S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
    				//return;
    			}
	        	S3.log("Bucket name:" + bucketName);
	        	TreeSet<String> fileNames = S3.list(s3,bucketName,"moves");
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
				if(fileNames != null && fileNames.size() > 0) {
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						try {
							String name = i.next();
							S3.log(name);
							// Check name ith pattern  Moves*db*.jar
							String expression = "^moves.*db.*.jar"; 
							if (!name.matches(expression)) {
								continue;
							}
							File localFile = new File(toDirectory,name);
							if(!localFile.exists()) {
								String[] getParameters = {
									"bucket", bucketName,
									"nameinbucket", name,
									"file", localFile.getCanonicalPath()
								};
								S3.get(getParameters);
							}
						} catch (AmazonServiceException ase) {
							S3.log("Caught an AmazonServiceException, which means your request made it "
									+ "to Amazon SQS, but was rejected with an error response for some reason.");
							S3.log("Error Message:    " + ase.getMessage());
							S3.log("HTTP Status Code: " + ase.getStatusCode());
							S3.log("AWS Error Code:   " + ase.getErrorCode());
							S3.log("Error Type:       " + ase.getErrorType());
							S3.log("Request ID:       " + ase.getRequestId());
				        } catch (AmazonClientException ace) {
							S3.log("Caught an AmazonClientException, which means the client encountered "
									+ "a serious internal problem while trying to communicate with SQS, "
									+ "such as not being able to access the network.");
							S3.log("Error Message: " + ace.getMessage());
				        } catch(Exception e) {
				        	S3.log(e.toString());
				        }
					}
				}
	        }
	        
			
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}
	/**
	 * Download Postprocess_*.jar
	 * @param args command line arguments to main()
	**/
	static void downloadPostProcessJar(String[] args) {
		String[] parameterNames = {
			"todir", "bucketfile", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid downloadPostProcessJar command.");
			printUsage();
			return;
		}
		try {
			
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			File f = new File(parameters.get("bucketfile"));
			BufferedReader b = new BufferedReader(new FileReader(f));

            String bucketName = "";
            File toDirectory;
            while ((bucketName = b.readLine()) != null) {
            	toDirectory = new File(parameters.get("todir") + "\\"+bucketName);
    			if(!toDirectory.exists()) {
    				toDirectory.mkdirs();
    				//S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
    				//return;
    			}

	        	S3.log("Bucket name:" + bucketName);
	        	TreeSet<String> fileNames = S3.list(s3,bucketName,"postprocess");
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
				if(fileNames != null && fileNames.size() > 0) {
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						try {
							String name = i.next();
							S3.log(name);
							File localFile = new File(toDirectory,name);
							if(!localFile.exists()) {
								String[] getParameters = {
									"bucket", bucketName,
									"nameinbucket", name,
									"file", localFile.getCanonicalPath()
								};
								S3.get(getParameters);
							}
						} catch (AmazonServiceException ase) {
							S3.log("Caught an AmazonServiceException, which means your request made it "
									+ "to Amazon SQS, but was rejected with an error response for some reason.");
							S3.log("Error Message:    " + ase.getMessage());
							S3.log("HTTP Status Code: " + ase.getStatusCode());
							S3.log("AWS Error Code:   " + ase.getErrorCode());
							S3.log("Error Type:       " + ase.getErrorType());
							S3.log("Request ID:       " + ase.getRequestId());
				        } catch (AmazonClientException ace) {
							S3.log("Caught an AmazonClientException, which means the client encountered "
									+ "a serious internal problem while trying to communicate with SQS, "
									+ "such as not being able to access the network.");
							S3.log("Error Message: " + ace.getMessage());
				        } catch(Exception e) {
				        	S3.log(e.toString());
				        }
					}
				}
	        }
	        
			
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}
	/**
	 * Download Code_*.jar
	 * @param args command line arguments to main()
	**/
	static void downloadCodeJar(String[] args) {
		String[] parameterNames = {
			"todir", "bucketfile", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid downloadCodeJar command.");
			printUsage();
			return;
		}
		try {
			
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			File f = new File(parameters.get("bucketfile"));
			BufferedReader b = new BufferedReader(new FileReader(f));
			List<Bucket> buckets = s3.listBuckets();
            String bucketName = "";
            File toDirectory;
            while ((bucketName = b.readLine()) != null) {
            	toDirectory = new File(parameters.get("todir") + "\\"+bucketName);
    			if(!toDirectory.exists()) {
    				toDirectory.mkdirs();
    				//S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
    				//return;
    			}
	        	S3.log("Bucket name:" + bucketName);
	        	TreeSet<String> fileNames = S3.list(s3,bucketName,"code" );
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
				
				
				if(fileNames != null && fileNames.size() > 0) {
					
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						try {
							String name = i.next();
							S3.log(name);
							File localFile = new File(toDirectory,name);
							
							if(!localFile.exists()) {
								String[] getParameters = {
									"bucket", bucketName,
									"nameinbucket", name,
									"file", localFile.getCanonicalPath()
								};
								S3.get(getParameters);
								
							}
						} catch (AmazonServiceException ase) {
							S3.log("Caught an AmazonServiceException, which means your request made it "
									+ "to Amazon SQS, but was rejected with an error response for some reason.");
							S3.log("Error Message:    " + ase.getMessage());
							S3.log("HTTP Status Code: " + ase.getStatusCode());
							S3.log("AWS Error Code:   " + ase.getErrorCode());
							S3.log("Error Type:       " + ase.getErrorType());
							S3.log("Request ID:       " + ase.getRequestId());
				        } catch (AmazonClientException ace) {
							S3.log("Caught an AmazonClientException, which means the client encountered "
									+ "a serious internal problem while trying to communicate with SQS, "
									+ "such as not being able to access the network.");
							S3.log("Error Message: " + ace.getMessage());
				        } catch(Exception e) {
				        	S3.log(e.toString());
				        }
					}
				}
	        }
	        
			
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}
	/**
	 * Download db_results*.jar
	 * @param args command line arguments to main()
	**/
	static void downloadDbResultsJar(String[] args) {
		String[] parameterNames = {
			"todir", "bucketfile", "code"
		};
		TreeMap<String,String> parameters = S3.getParameters(args,parameterNames);
		if(!parameters.containsKey("code")) {
			parameters.put("code","");
		}
		if(parameters.size() != parameterNames.length) {
			S3.log("Invalid downloadDbResultsJar command.");
			printUsage();
			return;
		}
		try {
			
			String postVersion = parameters.get("code").trim();
			AmazonS3 s3 = new AmazonS3Client(new PropertiesCredentials(new File(S3.AUTH_FILE)));
			File f = new File(parameters.get("bucketfile"));
			BufferedReader b = new BufferedReader(new FileReader(f));
			List<Bucket> buckets = s3.listBuckets();
            String bucketName = "";
            File toDirectory;
            while ((bucketName = b.readLine()) != null) {
            	toDirectory = new File(parameters.get("todir") + "\\"+bucketName);
    			if(!toDirectory.exists()) {
    				toDirectory.mkdirs();
    				//S3.log("ERROR: Destinaton directory does not exist: " + toDirectory.getCanonicalPath());
    				//return;
    			}
	        	S3.log("Bucket name:" + bucketName);
	        	TreeSet<String> fileNames = S3.list(s3,bucketName,"db_results" );
				// if "postVersion" is "", this acts as an effective wildcard since the following code
				// references the file suffix.
				
				
				if(fileNames != null && fileNames.size() > 0) {
					
					for(Iterator<String> i=fileNames.iterator();i.hasNext();) {
						try {
							String name = i.next();
							S3.log(name);
							File localFile = new File(toDirectory,name);
							
							if(!localFile.exists()) {
								String[] getParameters = {
									"bucket", bucketName,
									"nameinbucket", name,
									"file", localFile.getCanonicalPath()
								};
								S3.get(getParameters);
								break;
							}
						} catch (AmazonServiceException ase) {
							S3.log("Caught an AmazonServiceException, which means your request made it "
									+ "to Amazon SQS, but was rejected with an error response for some reason.");
							S3.log("Error Message:    " + ase.getMessage());
							S3.log("HTTP Status Code: " + ase.getStatusCode());
							S3.log("AWS Error Code:   " + ase.getErrorCode());
							S3.log("Error Type:       " + ase.getErrorType());
							S3.log("Request ID:       " + ase.getRequestId());
				        } catch (AmazonClientException ace) {
							S3.log("Caught an AmazonClientException, which means the client encountered "
									+ "a serious internal problem while trying to communicate with SQS, "
									+ "such as not being able to access the network.");
							S3.log("Error Message: " + ace.getMessage());
				        } catch(Exception e) {
				        	S3.log(e.toString());
				        }
					}
				}
	        }
	        
			
        } catch (AmazonServiceException ase) {
			S3.log("Caught an AmazonServiceException, which means your request made it "
					+ "to Amazon SQS, but was rejected with an error response for some reason.");
			S3.log("Error Message:    " + ase.getMessage());
			S3.log("HTTP Status Code: " + ase.getStatusCode());
			S3.log("AWS Error Code:   " + ase.getErrorCode());
			S3.log("Error Type:       " + ase.getErrorType());
			S3.log("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
			S3.log("Caught an AmazonClientException, which means the client encountered "
					+ "a serious internal problem while trying to communicate with SQS, "
					+ "such as not being able to access the network.");
			S3.log("Error Message: " + ace.getMessage());
        } catch(Exception e) {
        	S3.log(e.toString());
        }
	}
}
