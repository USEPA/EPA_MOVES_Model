/**************************************************************************************************
 * @(#)OpenFile.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.awt.*;

public class OpenFile {
	public static boolean open(File file) {
		try {
			if (OSDetector.isWindows()) {
				Runtime.getRuntime().exec(new String[] {"rundll32", "url.dll,FileProtocolHandler", file.getAbsolutePath()});
				return true;
			} else if (OSDetector.isLinux() || OSDetector.isMac()) {
				Runtime.getRuntime().exec(new String[]{"/usr/bin/open", file.getAbsolutePath()});
				return true;
			} else {
				if (Desktop.isDesktopSupported()) {
					Desktop.getDesktop().open(file);
					return true;
				} else {
					return false;
				}
			}
		} catch (Exception e) {
			Logger.log(LogMessageCategory.ERROR, "Could not open " + file.getAbsolutePath());
			Logger.log(LogMessageCategory.ERROR, "Encountered the following error: " + e);
			return false;
		}
	}
}
