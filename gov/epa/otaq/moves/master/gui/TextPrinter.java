/**************************************************************************************************
 * @(#)TextPrinter.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import gov.epa.otaq.moves.common.*;
import java.awt.Component;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Font;
import java.awt.print.*;
import javax.print.*;
import javax.print.attribute.*;
import javax.print.attribute.standard.*;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import javax.swing.JOptionPane;

/**
 * Class that implements text printing. This handles multiple paging and line wrapping.
 *
 * @author		Cimulus
 * @author		EPA Mitch C.  (changed to specifiy monospaced font)
 * @version		2006-12-27
**/
public class TextPrinter implements Printable {
	/** The text that an instance of this class prints.**/
	String text;
	/** true if a smaller font should be used **/
	boolean smallFont = false;
	
	/**
	 * A static method to print specified text.  Printing is done in portrait mode
	 * using a regular (not small) font.
	 *
	 * @param parentComponent The parent GUI component to put the print dialog under.
	 * @param srcText The text to print
	**/
	public static void printText(Component parentComponent, String srcText) {
		printText(parentComponent, srcText, false, false);
	}

	/**
	 * A static method to print specified text.
	 *
	 * @param parentComponent The parent GUI component to put the print dialog under.
	 * @param srcText The text to print
	 * @param landscape true if landscape orientation should be used
	 * @param smallFont true if a smaller font should be used
	**/
	public static void printText(Component parentComponent, String srcText, 
			boolean landscape, boolean smallFont) {
		TextPrinter printObj = new TextPrinter();
		printObj.text = srcText;
		printObj.smallFont = smallFont;
		
		PrinterJob printJob = PrinterJob.getPrinterJob();

		PrintRequestAttributeSet aset = new HashPrintRequestAttributeSet();
		PageFormat format = printJob.defaultPage();
		format.setOrientation(landscape?PageFormat.LANDSCAPE:PageFormat.PORTRAIT);
		printJob.defaultPage(format);

		printJob.setPrintable(printObj,format);

		if(printJob.printDialog()) {
			if(landscape) {
				aset.add(OrientationRequested.LANDSCAPE);
			} else {
				aset.add(OrientationRequested.PORTRAIT);
			}
			try {
				printJob.print(aset);
			} catch(PrinterException exception) {
				Logger.log(LogMessageCategory.ERROR,
						"PrinterException " + exception.getMessage());
			}
		}
	}
	
	/**
	 * Printable interface method.
	 *
	 * @param graphics The Graphics object to draw to.
	 * @param pageFormat Page formatting parameters
	 * @param pageIndex The index of the page to print.
	 * @return True if a page was printed.
	**/
	public int print(Graphics graphics, PageFormat pageFormat, int pageIndex) {
		graphics.setFont(new Font("monospaced", Font.PLAIN, smallFont?8:12));
		FontMetrics fontMetrics = graphics.getFontMetrics();
		double imageableWidth = pageFormat.getImageableWidth();
		int linesPerPage = (int)(pageFormat.getImageableHeight() / fontMetrics.getHeight());
		int startLineIndex = pageIndex * linesPerPage;
		int stopLineIndex = startLineIndex + linesPerPage;

		double x = pageFormat.getImageableX();
		double y = pageFormat.getImageableY() + fontMetrics.getAscent();

		StringCharacterIterator characterIterator = new StringCharacterIterator(text);

		int lineIndex = 0;
		String currentLine = new String();
		// Attempt to keep all of stringSection on the same line.
		String stringSection = new String();
		char lastChar = '\0';
		for (char iterChar = characterIterator.first(); iterChar != CharacterIterator.DONE;
				iterChar = characterIterator.next()) {
			if ((lastChar == '\r') && (iterChar =='\n')) {
				lastChar = iterChar;
				continue;
			}
			
			lastChar = iterChar;

			if ((iterChar == '\r') || (iterChar == '\n')) {
				if (lineIndex >= startLineIndex) {
					graphics.drawString(currentLine, (int)x, (int)y);
					y += fontMetrics.getHeight();
				}

				currentLine = "";
				lineIndex++;
				if (lineIndex >= stopLineIndex) {
					break;
				}
				continue;
			}

			// simple workaround so that the tab chars can be used in the string buffer
			if(iterChar != '\t') {
				currentLine += iterChar;
			} else {
				currentLine += "    ";
			}

			if (fontMetrics.stringWidth(currentLine) > imageableWidth) {
				// Wrapping. Iterate backwards and find the best character to wrap at.
				int wrapIndex = currentLine.length() - 1;
				for (; wrapIndex > 0; wrapIndex--) {
					if ((currentLine.charAt(wrapIndex) == ' ') || 
							(currentLine.charAt(wrapIndex) == '\t')) {
						break;
					}
				}
				
				if (lineIndex >= startLineIndex) {
					graphics.drawString(currentLine.substring(0, wrapIndex), (int)x, (int)y);
					y += fontMetrics.getHeight();
				}

				currentLine = currentLine.substring(wrapIndex + 1);
				lineIndex++;
				if (lineIndex >= stopLineIndex) {
					break;
				}
				continue;
			}
		}

		if (lineIndex >= startLineIndex) {
			graphics.drawString(currentLine, (int)x, (int)y);
			y += fontMetrics.getHeight();
		}

		return (lineIndex > startLineIndex) ? PAGE_EXISTS : NO_SUCH_PAGE;
	}
}
