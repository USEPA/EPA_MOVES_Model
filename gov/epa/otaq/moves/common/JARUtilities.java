/**************************************************************************************************
 * @(#)JARUtilities.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;

/**
 * This static class provides convenient JAR related functionality through a small layer
 * of code on top of the Java API.
 *
 * @author		Wesley Faler
 * @version		2010-10-11
**/
public class JARUtilities {
	/**
	 * Internal function used to take several temporary files, JAR them into one single
	 * JAR file.  Path information is not stored in the JAR file.
	 * @param targetJarPath The file path to store the target JAR file at
	 * @param sourceFilePaths An collection of File objects indicating which files to JAR.
	 * @throws FileNotFoundException From some java.io operations.
	 * @throws IOException From some java.io operations.
	**/
	static public void jarFiles(File targetJarPath, LinkedList<File> sourceFilePaths)
			throws FileNotFoundException, IOException {
		if(targetJarPath.exists()) {
			FileUtilities.deleteFileWithRetry(targetJarPath);
		}
		for(int i=10;i>=0;i--) {
			try {
				jarFilesCore(targetJarPath,sourceFilePaths);
				if(targetJarPath.exists()) {
					break;
				}
			} catch(FileNotFoundException e) {
				throw e;
			} catch(IOException e) {
				if(i == 0) {
					throw e;
				} else {
					try {
						Thread.sleep(15*1000);
					} catch(Exception ex) {
						// Nothing to do here
					}
				}
			}
		}
	}

	/**
	 * Internal function used to take several temporary files, JAR them into one single
	 * JAR file.  Path information is not stored in the JAR file.
	 * @param targetJarPath The file path to store the target JAR file at
	 * @param sourceFilePaths An collection of File objects indicating which files to JAR.
	 * @throws FileNotFoundException From some java.io operations.
	 * @throws IOException From some java.io operations.
	**/
	static void jarFilesCore(File targetJarPath, LinkedList<File> sourceFilePaths)
			throws FileNotFoundException, IOException {
		JarOutputStream jarStream = null;
		boolean isInEntry = false;
		BufferedInputStream inputStream = null;
		byte[] xferBuffer = new byte[1024*128];
		try {
			jarStream = new JarOutputStream(new FileOutputStream(targetJarPath.getCanonicalPath()));

			ListIterator<File> sourceFilePathIterator = sourceFilePaths.listIterator();
			while (sourceFilePathIterator.hasNext()) {
				File iterFilePath = (File)sourceFilePathIterator.next();

				JarEntry jarEntry = new JarEntry(iterFilePath.getName());
				inputStream = new BufferedInputStream(new FileInputStream(iterFilePath));

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
	 * Unjars a JAR file to the designated target folder. Doesn't handle path information.
	 * @param targetFolderPath The folder path to un jar the files to.
	 * @param srcJarFilePath The jar file to open.
	 * @return A list of all the paths that were extracted. All elements are File objects.
	 * @throws FileNotFoundException From some java.io operations.
	 * @throws IOException From some java.io operations.
	**/
	static public LinkedList<File> unJarFileToFolder(File targetFolderPath, File srcJarFilePath)
			throws FileNotFoundException, IOException {
		LinkedList<File> fileList = new LinkedList<File>();

		byte[] xferBuffer = new byte[1024*128];
		FileInputStream fileInputStream = null;
		BufferedInputStream bufferedInputStream = null;
		JarInputStream jarInputStream = null;
		FileOutputStream targetFileStream = null;
		try {
			fileInputStream = FileUtilities.openFileInputStreamWithRetry(srcJarFilePath);
			bufferedInputStream = new BufferedInputStream(fileInputStream);
			jarInputStream = new JarInputStream(bufferedInputStream);

			JarEntry jarEntry;

			while((jarEntry = jarInputStream.getNextJarEntry()) != null) {
				File targetFilePath = new File(targetFolderPath, jarEntry.getName());
				fileList.addLast(targetFilePath);
				targetFileStream = new FileOutputStream(targetFilePath);

				int iterByteCount;
				while ((iterByteCount = jarInputStream.read(xferBuffer)) > 0) {
					targetFileStream.write(xferBuffer, 0, iterByteCount);
				}
				targetFileStream.close();
				targetFileStream = null;
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

		return fileList;
	}
}
