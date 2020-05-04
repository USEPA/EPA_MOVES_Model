/**************************************************************************************************
 * @(#)Scale.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.gis.api;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.FontMetrics;
import java.awt.geom.Rectangle2D;
import java.text.*;

/**
 * Hold the coloration bins for a rendered image.  Each bin, or range of data values,
 * is represented by a center point, a color, an optional text label, and (except
 * for the highest bin) a maximum value.<br>
 * Two constructors are provided.  A convenience constructor creates a linear gradient
 * while a simpler constructor does only basic initialization.  When using the basic
 * constructor, the user must:<br>
 * <ul>
 * <li>fill binCenters with the displayable value for each bin.  binCenters is not
 * used when correlating colors and bins but is used when generating display text.</li>
 * <li>fill binMaximums with the maximum, inclusive, value of each bin.  The highest
 * bin does not have a binMaximums entry, thus there are only binCount-1 of these entries.</li>
 * <li>fill binColors with Color objects for each bin.</li>
 * <li>optionally create and fill binLabels with a String for each bin.  If binLabels is
 * left null then labels will be created automatically upon the first lookup of value to
 * Color.  If not null, then entries will be displayed as set with null entries shown as
 * blank strings.<li>
 * </ul>
 *
 * @author		Wesley Faler
 * @version		2008-05-03
**/
public class Scale {
	/** true if the user's Min/Max should be used as centers of the lower and upper bins **/
	private static final boolean binsShowMinMax = false;
	/** enumeration for the popular color palette known as "Fire" **/
	public static final int PALETTE_FIRE = 1;
	/** enumeration for a gray-scale palette **/
	public static final int PALETTE_GRAY = 2;

	/** number of bins **/
	public int binCount = 0;
	/** center point of each bin, binCount of these exist **/
	public double[] binCenters = null;
	/**
	 * maximum bounds for each bin, binCount-1 of these exist.
	 * binMaximums[0] is the maximum value, inclusive, of the lowest bin.
	**/
	public double[] binMaximums = null;
	/** Color of each bin, binCount of these exist **/
	public Color[] binColors = null;
	/**
	 * Text for each bin, binCount of these exist.  If left null, labels will
	 * created automatically.  If not null but entries are null then blank ("")
	 * text will be used.
	**/
	public String[] binLabels = null;

	/** hex-notation of each bin color, binCount of these exist **/
	String[] binColorsHex = null;

	/**
	 * Basic constructor.  The user must completely fill in binCenters,
	 * binMaximums, binColors, and optionally binLabels before using this Scale
	 * object to render a map.
	 * @param binCountToUse number of bins
	**/
	public Scale(int binCountToUse) {
		binCount = binCountToUse;
		binCenters = new double[binCount];
		if(binCount == 1) {
			binMaximums = new double[1];
		} else {
			binMaximums = new double[binCount-1];
		}
		binColors = new Color[binCount];
		binLabels = null;
	}

	/**
	 * Convenience Constructor that generates a linear gradient
	 * @param binCountToUse number of bins
	 * @param minValue minimum data value to be represented
	 * @param maxValue maximum data value to be represented
	 * @param whichPalette one of the Scale.PALETTE_xxxxx enumerations
	 * @param useLabels true if label text should be displayed
	**/
	public Scale(int binCountToUse,double minValue,double maxValue,int whichPalette,
			boolean useLabels) {
		if(minValue > maxValue) {
			double t = minValue;
			minValue = maxValue;
			maxValue = t;
		}
		binCount = binCountToUse;
		switch(whichPalette) {
			default:
			case PALETTE_FIRE:
				binColors = generateFireSpectrum(binCount);
				break;
			case PALETTE_GRAY:
				binColors = generateGraySpectrum(binCount);
				break;
		}
		// Create binCenters and binMaximums
		binCenters = new double[binCount];
		if(binCount == 1) {
			binMaximums = new double[1];
			binMaximums[0] = maxValue;
			binCenters[0] = (maxValue + minValue) / 2.0;
		} else {
			binMaximums = new double[binCount-1];
			if(binsShowMinMax) {
				double step = (maxValue - minValue) / (double)(binCount-1);
				double halfStep = step/2.0;
				for(int i=0;i<binCount;i++) {
					binCenters[i] = minValue + i*step;
					if(i<binCount-1) {
						binMaximums[i] = binCenters[i] + halfStep;
					}
				}
				binCenters[0] = minValue; // ensure no unusual rounding issues with the end points
				binCenters[binCount-1] = maxValue;
			} else {
				double step = (maxValue - minValue) / (double)(binCount);
				double halfStep = step/2.0;
				for(int i=0;i<binCount;i++) {
					binCenters[i] = minValue + i*step + halfStep;
					//System.out.println("binCenters[" + i + "]=" + binCenters[i]);
					if(i<binCount-1) {
						binMaximums[i] = binCenters[i] + halfStep;
						//System.out.println("binMaximums[" + i + "]=" + binMaximums[i]);
					}
				}
			}
		}
		// Create binLabels
		if(useLabels) {
			createBinLabels();
		} else {
			binLabels = new String[binCount];
			// binLabels[...] intentionally left null
		}
	}

	/** Create and fill binLabels **/
	private void createBinLabels() {
		double logCount = Math.log(binCount) / Math.log(10);
		int significantDigits = (int)logCount;
		if(logCount > significantDigits) {
			significantDigits++;
		}
		significantDigits += 2;
		String pattern = "0.";
		for(int i=1;i<significantDigits;i++) {
			pattern += "0";
		}
		pattern += "E0";
		DecimalFormat formatter = new DecimalFormat(pattern);
		binLabels = new String[binCount];
		for(int i=0;i<binCount;i++) {
			binLabels[i] = formatter.format(binCenters[i]);
		}
	}

	/** Fillout any remaining data required before doing lookups **/
	private void finishSetup() {
		if(binColorsHex == null) {
			binColorsHex = new String[binColors.length];
			for(int i=0;i<binColors.length;i++) {
				if(binColors[i] == null) {
					binColors[i] = Color.white;
				}
				binColorsHex[i] = toHex(binColors[i]);
			}
		}
		if(binLabels == null) {
			createBinLabels();
		}
	}

	/**
	 * Obtain the label text for a bin.
	 * @param binIndex 0-based index of the bin being examined
	 * @return text to display for the bin or "" if none, but never null.
	**/
	public String getLabel(int binIndex) {
		if(binLabels != null && binLabels[binIndex] != null) {
			return binLabels[binIndex];
		} else {
			return "";
		}
	}

	/**
	 * Obtain the color for a bin.
	 * @param binIndex 0-based index of the bin being examined
	 * @return Color for the bin
	**/
	public Color getColor(int binIndex) {
		if(binColors != null && binColors[binIndex] != null) {
			return binColors[binIndex];
		} else {
			return Color.white;
		}
	}

	/**
	 * Obtain the color text for a bin.
	 * @param binIndex 0-based index of the bin being examined
	 * @return Color text for the bin
	**/
	public String getColorHex(int binIndex) {
		if(binColorsHex != null && binColorsHex[binIndex] != null) {
			return binColorsHex[binIndex];
		} else {
			return "ffffff";
		}
	}

	/**
	 * Determine which bin a value falls within.
	 * @param value value to use
	 * @return 0-based index of the bin containing value.  Always &gt;= 0.
	 * Always &lt; binCount.
	**/
	public int getIndex(double value) {
		finishSetup();
		return binarySearch(value,binMaximums);
	}

	/**
	 * Display a spectrum vertically
	 * @param g canvas to draw upon
	 * @param top topmost row to draw on the canvas, these are the last entries in the spectrum
	 * @param left leftmost column of the spectrum display
	 * @param width number of columns on the canvas
	 * @param height number of rows on the canvas for the whole spectrum.
	**/
	public void drawVerticalSpectrum(Graphics2D g, 
			int top, int left, int width, int height) {
		FontMetrics metrics = g.getFontMetrics();
		double dy = (double)height / (double)(binColors.length);
		double y = top;
		double nextLabelY = y-1;
		for(int i=binColors.length-1;i>=0;i--,y+=dy) {
			g.setColor(binColors[i]);
			g.fillRect(left,(int)y,width,1+(int)dy);
			if(y>=nextLabelY) {
				Rectangle2D bounds = metrics.getStringBounds(binLabels[i],g);
				nextLabelY = y + bounds.getHeight()*1.5;
				g.setColor(Color.black);
				g.drawString(binLabels[i],left-(int)(bounds.getWidth()+0.5)-2,(int)(y+bounds.getHeight()));
			}
		}
	}

	/**
	 * Determine which bin a value falls within.
	 * @param value value to use
	 * @param binMaximums upper bounds of each bin
	 * @return 0-based index of the bin containing value.  Always &gt;= 0.
	 * Will be binMaximum.length if outside the upper value of binMaximums.
	**/
	public static int binarySearch(double value,double[] binMaximums) {
		if(binMaximums.length == 1) {
			if(value <= binMaximums[0]) {
				return 0;
			}
			return 1;
		} else if(binMaximums.length == 2) {
			if(value <= binMaximums[0]) {
				return 0;
			} else if(value <= binMaximums[1]) {
				return 1;
			}
			return 2;
		}
		if(value > binMaximums[binMaximums.length-1]) {
			return binMaximums.length;
		}
		int lowerIndex = 0, upperIndex = binMaximums.length-1;
		int centerIndex;
		while(true) {
			centerIndex = (int)((upperIndex + lowerIndex) / 2);
			if(centerIndex == lowerIndex) {
				return upperIndex;
			} else if(centerIndex == upperIndex) {
				return lowerIndex;
			} else if(value <= binMaximums[lowerIndex]) {
				return lowerIndex;
			} else if(value > binMaximums[upperIndex]) {
				return upperIndex+1;
			} else if(value <= binMaximums[centerIndex]) {
				upperIndex = centerIndex;
			} else {
				lowerIndex = centerIndex;
			}
		}
	}

	/**
	 * Create a popular color palette known as the "fire palette".  Taken from
	 * http://vis.iu.edu/Publications/Storm.pdf, figure 5, page 7.  The lower
	 * 12.5% (1/8th) of the spectrum is dropped for aesthetic reasons.
	 * @param howManyBins the number of different colors desired.
	 * @return an array of Color objects spaced evenly through the spectrum.  The
	 * lowest entry has the "coolest" color.
	**/
	public static Color[] generateFireSpectrum(int howManyBins) {
		Color[] spectrum = new Color[howManyBins];
		float[] hsv = new float[3];
		float[] rgb = new float[3];
		final double skippedSection = 0.125;
		final double usedSection = 1.0-skippedSection;
		for(int i=0;i<howManyBins;i++) {
			double fraction = ((double)i)/((double)(howManyBins-1.0));
			fraction = skippedSection + (fraction*usedSection);
			hsv[0] = (float)(1-Math.sqrt(fraction));
			if(fraction < 1.0/3.0) {
				hsv[1] = (float)(1.0-((Math.pow((1.0/3.0)-fraction,2))/(1.0/9.0)));
			} else {
				hsv[1] = (float)1.0;
			}
			hsv[2] = (float)0.98;
			convertHSVtoRGB(hsv,rgb);
			spectrum[i] = new Color(rgb[0],rgb[1],rgb[2],(float)1.0);
		}
		return spectrum;
	}

	/**
	 * Generate a gray-scale color palette, omitting white and black.  Black is considered
	 * the "hottest" color since it will likely be used for the most pollution.
	 * @param howManyBins the number of different colors desired.
	 * @return an array of Color objects spaced evenly through the spectrum.  The
	 * lowest entry has the "coolest" color.
	**/
	public static Color[] generateGraySpectrum(int howManyBins) {
		Color[] spectrum = new Color[howManyBins];
		float[] rgb = new float[3];
		final double blackSkippedSection = 0.30;
		final double whiteSkippedSection = 0.10;
		final double usedSection = 1.0 - blackSkippedSection - whiteSkippedSection;
		for(int i=0;i<howManyBins;i++) {
			double fraction = ((double)i)/((double)(howManyBins-1.0));
			fraction = whiteSkippedSection + (fraction*usedSection);
			fraction = 1.0-fraction; // make black the hottest color
			spectrum[i] = new Color((float)fraction,(float)fraction,(float)fraction,(float)1.0);
		}
		return spectrum;
	}

	/**
	 * Generate the Red/Green/Blue values for a color given a Hue/Saturation/Value.
	 * From http://en.wikipedia.org/wiki/HSV_color_space#From_HSV_to_RGB
	 * @param hsv input HSV values, [0] is H, [1] S, and [2] V.
	 * @param rgb output RGB values, [0] is R, [1] G, and [2] B.
	**/
	public static void convertHSVtoRGB(float[] hsv,float[] rgb) {
		while(hsv[0] >= 1) {
			hsv[0] -= 1;
		}
		while(hsv[0] < 0) {
			hsv[0] += 1;
		}
		int hi = ((int)(hsv[0]*360/60)) % 6;
		float f = (float)((hsv[0]*360/60) - hi);
		float p = (float)(hsv[2]*(1.0-hsv[1]));
		float q = (float)(hsv[2]*(1.0-f*hsv[1]));
		float t = (float)(hsv[2]*(1.0-(1.0-f)*hsv[1]));

		switch(hi) {
			case 0:
				rgb[0] = hsv[2]; rgb[1] = t; rgb[2] = p;
				break;
			case 1:
				rgb[0] = q; rgb[1] = hsv[2]; rgb[2] = p;
				break;
			case 2:
				rgb[0] = p; rgb[1] = hsv[2]; rgb[2] = t;
				break;
			case 3:
				rgb[0] = p; rgb[1] = q; rgb[2] = hsv[2];
				break;
			case 4:
				rgb[0] = t; rgb[1] = p; rgb[2] = hsv[2];
				break;
			case 5:
				rgb[0] = hsv[2]; rgb[1] = p; rgb[2] = q;
				break;
		}
	}

	/**
	 * Generate the hexadecimal text format for a Color
	 * @param c Color to be examined
	 * @return hexadecimal string in the format ABCDEF
	**/
	public static String toHex(Color c) {
		String r = "0" + Integer.toHexString(c.getRed());
		String g = "0" + Integer.toHexString(c.getGreen());
		String b = "0" + Integer.toHexString(c.getBlue());

		return r.substring(r.length()-2)
				+ g.substring(g.length()-2)
				+ b.substring(b.length()-2);
	}
}
