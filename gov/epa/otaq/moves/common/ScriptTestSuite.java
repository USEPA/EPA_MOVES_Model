/**************************************************************************************************
 * @(#)ScriptTestSuite.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.File;
import java.lang.reflect.Constructor;
import java.util.*;

import org.junit.runner.*;
import org.junit.runner.notification.*;
import junit.framework.*;
import abbot.Log;
import abbot.script.Script;
import junit.extensions.abbot.ScriptFixture;

import static org.junit.Assert.*;

/**
 * Similar to Abbot's ScriptTestSuite, itself similar to TestSuite, except that it 
 * auto-generates a suite based on test scripts matching certain criteria.
 * The directory given in the system property "abbot.testsuite.path" is scanned for
 * XML files, each of which is expected to be an Abbot test script.
**/
public class ScriptTestSuite extends TestSuite {
	/** List of tests to be performed **/
	ArrayList<Test> tests = new ArrayList<Test>();
	/** List of file names (*.xml) corresponding to each of the tests to be performed **/
	ArrayList<String> fileNames = new ArrayList<String>();
	/** Directory to be scanned for test XML files **/
    File primaryDirectory;

    /** 
     * Constructs a suite of tests from all the scripts found in the
     * directory specified by the system property "abbot.testsuite.path".
     * The most common use for this constructor would be from an Ant 'junit'
     * task, where the system property is defined for a given run.
     * The suite will recurse directories if "abbot.testsuite.path.recurse" is
     * set to true.
    **/
    public ScriptTestSuite() {
        super(ScriptFixture.class.getName());
        setup(System.getProperty("abbot.testsuite.path",
                                System.getProperty("user.dir")),
             Boolean.getBoolean("abbot.testsuite.path.recurse"));
    }

    /** 
     * Constructs a suite of tests from all the scripts found in the current
     * directory.  Does not recurse to subdirectories.  The Class argument
     * must be a subclass of junit.extensions.abbot.ScriptFixture.
    **/
    public void setup() {
        setup(System.getProperty("user.dir"), false);
    }

    /**
     * Constructs a suite of tests from all the scripts found in the given
     * directory.  Does not recurse to subdirectories. The Class argument
     * must be a subclass of junit.extensions.abbot.ScriptFixture.
     * @param dirname directory holding XML test scripts
    **/
    public void setup(String dirname) {
        setup(dirname, false);
    }

    /**
     * Constructs an ScriptTestSuite from all the scripts in the given
     * directory, recursing if recurse is true. The Class argument
     * must be a class derived from junit.extensions.abbot.ScriptFixture.
     * @param dirname directory holding XML test scripts
     * @param recurse true if subfolders under directory should be searched
    **/
    public void setup(String dirname, boolean recurse) {
        setup(findFilenames(dirname, recurse));
        primaryDirectory = new File(dirname);
        if (!primaryDirectory.exists() || !primaryDirectory.isDirectory()) {
            String msg = "Directory '" + dirname + "' did not exist"
                + " when scanning for test scripts";
            tests.add(warningTest(msg));
            fileNames.add(msg);
        }
    }

    /** 
     * Constructs a suite of tests for each script given in the argument
     * list, using the given class derived from ScriptFixture to wrap each
     * script. 
     * @param filenames array of possible XML test files
    **/
    public void setup(String[] filenames) {
        primaryDirectory = new File(System.getProperty("user.dir"));
        Log.debug("Loading with " + filenames.length + " files");
        for(int i=0;i < filenames.length;i++) {
            File f = new File(filenames[i]);
            // Filter for desired files only
            if(!accept(f)) {
                continue;
            }
            try {
                ScriptFixture test = new ScriptFixture(f.getAbsolutePath());
                tests.add(test);
                fileNames.add(f.getAbsolutePath());
            } catch(Throwable thr) {
                Log.warn(thr);
                String t = "Could not construct an instance of ScriptFixture";
                tests.add(warningTest(t));
                fileNames.add(t);
                break;
            }
        }
        if(tests.size() == 0) {
        	String t = "No scripts found";
            tests.add(warningTest(t));
            fileNames.add(t);
        }
    }

    /**
     * Return whether to accept the given file.   The default implementation
     * omits common backup files.
     * @param file possible XML test file
     * @return true if the file can be used as a test script
    **/
    public boolean accept(File file) {
        String name = file.getName();
        boolean isXML = FileUtilities.getFileExtension(file,true).equalsIgnoreCase(".xml");
        return isXML && !name.startsWith(".#")
            && !name.endsWith("~")
            && !name.endsWith(".bak");
    }

    /**
     * Returns a test which will fail and log a warning message.
     * @param message message to be shown when the resulting Test is executed
     * @return a Test object that will fail after showing the given message
    **/
    private Test warningTest(final String message) {
        return new TestCase("warning") {
            protected void runTest() {
                fail(message);
            }
        };		
    }

    /**
     * Add all test scripts in the given directory, optionally recursing to
     * subdirectories.  Returns a list of absolute paths.
     * @param dir folder to be searched
     * @param files list to be filled with file paths to be executed
     * @return list of file paths to be executed.  Is the same as the populated files
     * parameter.
    **/
    protected static ArrayList<String> findTestScripts(File dir, ArrayList<String> files,
                                          boolean recurse) {
        File[] flist = dir.listFiles();
        for(int i=0;flist != null && i < flist.length;i++) {
            //Log.debug("Examining " + flist[i]);
            if(flist[i].isDirectory()) {
                if(recurse) {
                    findTestScripts(flist[i], files, recurse);
                }
            } else if(Script.isScript(flist[i])) {
                String filename = flist[i].getAbsolutePath();
                if(!files.contains(filename)) {
                    Log.debug("Adding " + filename);
                    files.add(filename);
                }
            }
        }
        return files;
    }

    /** 
     * Scan for test scripts and return an array of filenames for all scripts
     * found.
     * @param dirname directory to be scanned
     * @param recurse true if subfolders under the directory should be searched
     * @return array of file paths to be used
    **/
    static String[] findFilenames(String dirname, boolean recurse) {
        File dir = new File(dirname);
        ArrayList<String> list = new ArrayList<String>();
        if(dir.exists() && dir.isDirectory()) {
            findTestScripts(dir, list, recurse);
        }
        return (String[])list.toArray(new String[list.size()]);
    }

	/**
	 * Execute all test cases
	**/
	@org.junit.Test
	public void runTests() {
		System.out.println("Preparing to run " + tests.size() + " GUI tests");
		JUnitCore core = new JUnitCore();
		Iterator<String> fi=fileNames.iterator();
		for(Iterator<Test> i=tests.iterator();i.hasNext() && fi.hasNext();) {
			String fileName = fi.next();
			Result result = core.run(i.next());
			assertNotNull("No result object from test " + fileName,result);
			List<Failure> failures = result.getFailures();
			if(failures != null) {
				for(Iterator<Failure> j=failures.iterator();j.hasNext();) {
					fail(j.next().toString());
				}
			}
			assertTrue("Got failure in GUI script " + fileName,result.wasSuccessful());
		}
	}
}
