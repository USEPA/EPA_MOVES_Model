/**************************************************************************************************
 * @(#)ImageUtility.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.gis.api;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

import javax.imageio.ImageIO;
//import javax.units.SI;

import java.awt.print.*;
import javax.print.*;
import javax.print.attribute.*;
import javax.print.attribute.standard.*;

/**
 * Utilities for images, including saving to files, printing, and display.
 *
 * @author		Wesley Faler
 * @version		2009-04-15
**/
public class ImageUtility {
	/**
	 * Create a JPG format image file
	 * @param image map image from the render() routine
	 * @param fileName name and path of the image to create, including the ".jpg" extension
	 * @throws IOException if unable to write the image
	**/
	public static void saveJPG(BufferedImage image, String fileName) throws IOException {
		ImageIO.write(image, "jpg", new File(fileName));
	}

	/**
	 * Create a PNG format image file
	 * @param image map image from the render() routine
	 * @param fileName name and path of the image to create, including the ".png" extension
	 * @throws IOException if unable to write the image
	**/
	public static void savePNG(BufferedImage image, String fileName) throws IOException {
		ImageIO.write(image, "png", new File(fileName));
	}

	/** Utility class for image print jobs **/
	static class ImagePrinter implements Printable {
		BufferedImage image;

		public ImagePrinter(BufferedImage imageToUse) {
			image = imageToUse;
		}

		public int print(Graphics g, PageFormat pageFormat, int pageIndex) {
			if(pageIndex > 0) {
				return Printable.NO_SUCH_PAGE;
			}
			Graphics2D g2d = (Graphics2D)g;

			//Set us to the upper left corner
			g2d.translate(pageFormat.getImageableX(), pageFormat.getImageableY());
			AffineTransform at = new AffineTransform();
			at.translate(0,0);

			//We need to scale the image properly so that it fits on one page.
			double xScale = pageFormat.getImageableWidth() / image.getWidth();
			double yScale = pageFormat.getImageableHeight() / image.getHeight();
			// Maintain the aspect ratio by taking the min of those 2 factors and
			// using it to scale both dimensions.
			double aspectScale = Math.min(xScale,yScale);
			at.scale(aspectScale,aspectScale);

			g2d.drawRenderedImage(image,at);
			return Printable.PAGE_EXISTS;
 		}
	}

	/**
	 * Print an image
	 * @param image image to be printed
	**/
	public static void print(BufferedImage image) throws Exception {
		ImagePrinter printable = new ImagePrinter(image);
		PrinterJob printJob = PrinterJob.getPrinterJob();
		printJob.setPrintable(printable);
		if(printJob.printDialog()) {
			printJob.print();
		}
	}

	/** Utility class for displaying an image **/
	public static class ImagePanel extends JPanel {
		public BufferedImage image;
		public Dimension preferredSize;
		public Dimension imageSize;

		public ImagePanel(BufferedImage imageToUse) {
			setup(imageToUse,null);
			setupMouseScrolling();
		}

		public ImagePanel(BufferedImage imageToUse, Dimension preferredSizeToUse) {
			setup(imageToUse,preferredSizeToUse);
			setupMouseScrolling();
		}

		void setupMouseScrolling() {
			MouseInputAdapter mouse = new MouseInputAdapter() {
				int dx, dy;

				public void mouseDragged(MouseEvent e) {
					Container c = getParent();
					if(c instanceof JViewport) {
						JViewport jv = (JViewport)c;
						Point p = jv.getViewPosition();
						int nx = p.x - (e.getX() - dx);
						int ny = p.y - (e.getY() - dy);
						int maxX = getWidth() - jv.getWidth();
						int maxY = getHeight() - jv.getHeight();
						if(nx < 0) {
							nx = 0;
						} else if(nx > maxX) {
							nx = maxX;
						}
						if(ny < 0) {
							ny = 0;
						} else if(ny > maxY) {
							ny = maxY;
						}
						jv.setViewPosition(new Point(nx,ny));
					}
				}

				public void mousePressed(MouseEvent e) {
					setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
					dx = e.getX();
					dy = e.getY();
				}

				public void mouseReleased(MouseEvent e) {
					setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				}
			};

			addMouseMotionListener(mouse);
			addMouseListener(mouse);
		}

		public void setup(BufferedImage imageToUse, Dimension preferredSizeToUse) {
			image = imageToUse;
			if(image != null) {
				if(preferredSizeToUse != null) {
					preferredSize = preferredSizeToUse;
				} else {
					preferredSize = new Dimension(image.getWidth(),image.getHeight());
				}
				setPreferredSize(preferredSize);
				imageSize = new Dimension(image.getWidth(),image.getHeight());
			}
		}

		public void paintComponent(Graphics graphics) {
			super.paintComponent(graphics);
			if(image != null) {
				if(imageSize == null) {
					imageSize = new Dimension(image.getWidth(),image.getHeight());
				}
				graphics.drawImage(image,0,0,(int)imageSize.getWidth(),(int)imageSize.getHeight(),null);
			}
		}
	}
}
