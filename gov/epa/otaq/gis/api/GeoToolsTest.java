/**************************************************************************************************
 * @(#)GeoToolsTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.gis.api;

import junit.framework.*;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.RenderingHints;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;

import javax.imageio.ImageIO;
//import javax.units.SI;

//import org.geotools.catalog.*;
//import org.geotools.catalog.defaults.*;
import org.opengis.feature.simple.*;
import org.opengis.feature.type.*;

import org.geotools.factory.*;
import org.geotools.data.*;
import org.geotools.data.shapefile.*;
import org.geotools.feature.*;
import org.geotools.geometry.jts.*;
import org.geotools.map.*;
import org.geotools.renderer.GTRenderer;
import org.geotools.renderer.lite.StreamingRenderer;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.operation.*;
import org.geotools.referencing.factory.*;
import org.geotools.filter.*;
import org.opengis.referencing.*;
import org.opengis.referencing.crs.*;
import org.opengis.referencing.cs.*;
import org.opengis.referencing.operation.*;
import org.opengis.parameter.*;
import org.geotools.styling.*;
import com.vividsolutions.jts.geom.*;

/*
import org.geotools.catalog.GeoResource;
import org.geotools.catalog.Service;
import org.geotools.catalog.defaults.DefaultServiceFinder;
import org.geotools.data.FeatureSource;
import org.geotools.data.postgis.PostgisDataStoreFactory;
import org.geotools.data.shapefile.ShapefileDataStoreFactory;
import org.geotools.data.wfs.WFSDataStoreFactory;
import org.geotools.demo.mappane.MapViewer;
import org.geotools.feature.AttributeType;
import org.geotools.feature.AttributeTypeFactory;
import org.geotools.feature.SimpleFeature;
import org.geotools.feature.FeatureCollection;
import org.geotools.feature.FeatureCollections;
import org.geotools.feature.FeatureType;
import org.geotools.feature.FeatureTypes;
import org.geotools.feature.GeometryAttributeType;
import org.geotools.feature.IllegalAttributeException;
import org.geotools.feature.SchemaException;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.map.DefaultMapLayer;
import org.geotools.map.MapLayer;
import org.geotools.referencing.FactoryFinder;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.factory.FactoryGroup;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.DefiningConversion;
import org.geotools.styling.Graphic;
import org.geotools.styling.Mark;
import org.geotools.styling.SLDParser;
import org.geotools.styling.Style;
import org.geotools.styling.StyleBuilder;
import org.geotools.styling.StyleFactory;
import org.geotools.styling.StyleFactoryFinder;
import org.geotools.styling.Symbolizer;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.IdentifiedObject;
import org.opengis.referencing.NoSuchIdentifierException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.cs.AxisDirection;
import org.opengis.referencing.cs.CSFactory;
import org.opengis.referencing.cs.CartesianCS;
import org.opengis.referencing.cs.CoordinateSystemAxis;
import org.opengis.referencing.operation.Conversion;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

import java.awt.Color;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.geotools.catalog.Catalog;
import org.geotools.catalog.GeoResource;
import org.geotools.catalog.Service;
import org.geotools.catalog.defaults.DefaultCatalog;
import org.geotools.catalog.defaults.DefaultServiceFinder;
import org.geotools.data.FeatureSource;
import org.geotools.data.memory.MemoryDataStore;
import org.geotools.data.postgis.PostgisDataStoreFactory;
import org.geotools.data.shapefile.ShapefileDataStoreFactory;
import org.geotools.data.wfs.WFSDataStoreFactory;
import org.geotools.demo.mappane.MapViewer;
import org.geotools.feature.AttributeType;
import org.geotools.feature.AttributeTypeFactory;
import org.geotools.feature.SimpleFeature;
import org.geotools.feature.FeatureType;
import org.geotools.feature.FeatureTypes;
import org.geotools.feature.GeometryAttributeType;
import org.geotools.feature.IllegalAttributeException;
import org.geotools.feature.SchemaException;
import org.geotools.styling.Graphic;
import org.geotools.styling.Mark;
import org.geotools.styling.SLDParser;
import org.geotools.styling.Style;
import org.geotools.styling.StyleBuilder;
import org.geotools.styling.StyleFactory;
import org.geotools.styling.StyleFactoryFinder;
import org.geotools.styling.Symbolizer;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
*/

/**
 * This class tests the interface to GeoTools
 *
 * @author		Wesley Faler
 * @version		2009-04-15
**/
public class GeoToolsTest extends TestCase {
	/**
	 * Filter a set of shapes/features selecting only the states or counties desired.
	**/
	static class StateCountyFilter {
		protected int stateAttributeIndex = -1;
		protected int countyAttributeIndex = -1;

		protected TreeSet<String> acceptableStateCountyValues = new TreeSet<String>();
		protected FeatureSource<SimpleFeatureType,SimpleFeature> fs = null;
		protected boolean filterByCounty = true;

		public StateCountyFilter(FeatureSource<SimpleFeatureType,SimpleFeature> fsToUse,
				boolean filterByCountyToUse) {
			fs = fsToUse;
			filterByCounty = filterByCountyToUse;
			try {
				SimpleFeatureType schema = null;
				List<AttributeType> attributeTypes = null;
				FeatureCollection<SimpleFeatureType,SimpleFeature> features = fs.getFeatures();
				Iterator<SimpleFeature> fi = features.iterator();
				try {
					if(fi.hasNext()) {
						SimpleFeature feature = (SimpleFeature) fi.next();
						schema = feature.getFeatureType();
						attributeTypes = schema.getTypes();
					}
				} finally {
					features.close(fi);
				}
				int index = 0;
				for(Iterator<AttributeType> i=attributeTypes.iterator();i.hasNext();index++) {
					String name = i.next().getName().toString();
					if(name.equalsIgnoreCase("STATE")) {
						stateAttributeIndex = index;
					} else if(filterByCounty && name.equalsIgnoreCase("COUNTY")) {
						countyAttributeIndex = index;
					}
				}
			} catch(Exception e) {
				System.out.println(e.toString());
				e.printStackTrace();
			}
		}

		public void allow(String value) {
			if(!filterByCounty && value.length() > 2) {
				value = value.substring(0,2);
			}
			acceptableStateCountyValues.add(value);
		}

		public FeatureCollection<SimpleFeatureType,SimpleFeature> filter() {
			FeatureCollection<SimpleFeatureType,SimpleFeature> results = 
					FeatureCollections.newCollection();
			try {
				FeatureCollection<SimpleFeatureType,SimpleFeature> features = fs.getFeatures();
				Iterator<SimpleFeature> fi = features.iterator();
				try {
					while(fi.hasNext()) {
						SimpleFeature feature = (SimpleFeature) fi.next();
						if(contains(feature)) {
							results.add(feature);
						}
					}
				} finally {
					features.close(fi);
				}
			} catch(Exception e) {
				/**
				 * @recur
				 * @nonissue
				**/
				System.out.println(e.toString());
				e.printStackTrace();
			}
			return results;
		}

		public boolean contains(SimpleFeature feature) {
			String state = "", county = "";
			if(stateAttributeIndex >= 0) {
				state = feature.getAttribute(stateAttributeIndex).toString().trim();
			}
			if(countyAttributeIndex >= 0) {
				county = feature.getAttribute(countyAttributeIndex).toString().trim();
			}
			return acceptableStateCountyValues.contains(state+county);
		}
	}

	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public GeoToolsTest(String name) {
		super(name);
	}

	/**
	 * Create a URL file a file name
	 * @param target name of a file
	 * @return URL to the passed file or null if the file doesn't exist
	**/
    public static URL acquireURL(String target) {
    	File targetFile = new File(target);
    	if(targetFile.exists()) {
        	try {
				return targetFile.toURI().toURL();
			} catch (MalformedURLException e) {
				// Nothing to do here
			}
        }
        return null;
        /*
    	try {
			return new URL(target);
		} catch (MalformedURLException e) {
        	return null;
        }
        */
    }

	/**
	 * Helper function to copy a file to destination file.
	 * @param sourceFilePath The file to copy.
	 * @param destinationFilePath The destination File to copy the file to.
	 * @param overWrite Indicates if the destination should be overwritten.
	 * @return True if copied.
	**/
	public static boolean copyFile(File sourceFilePath, File destinationFilePath,
			boolean overWrite) {
		FileInputStream fileIn = null;
		FileOutputStream fileOut = null;
		byte[] buffer = new byte[4096];
		try {
			if(destinationFilePath.exists() && !overWrite) {
				return false;
			}
			fileIn = new FileInputStream(sourceFilePath);
			destinationFilePath.delete();
			destinationFilePath.createNewFile();
			fileOut =  new FileOutputStream(destinationFilePath);
			int howManyRead = 0;
			while((howManyRead = fileIn.read(buffer)) > 0) {
				fileOut.write(buffer,0,howManyRead);
			}
		} catch(Exception e) {
			return false;
		} finally {
			if(fileOut != null) {
				try {
					fileOut.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fileOut = null;
			}
			if(fileIn != null) {
				try {
					fileIn.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fileIn = null;
			}
		}
		return true;
	}

	/**
	 * Copy a file of the form "test_basename.ext" to "basename.ext", deleting
	 * any destination file that may already exist.
	 * @param pathName path and file name of the base file.
	**/
	boolean copyTestFile(String pathName) {
		File destinationFile = new File(pathName);
		File testFile = new File(destinationFile.getParentFile(),"test_" + destinationFile.getName());
		if(destinationFile.exists()) {
			destinationFile.delete();
		}

		return copyFile(testFile,destinationFile,true);
	}

	/**
	 * Generate an image file with state outlines, some county outlines, and a colored county.
	**/
	public void testStatesWithCounties() throws Exception {
	    final String countyShapeFileName = "libs/geotools/co99_d00.shp";
	    final String stateShapeFileName = "libs/geotools/st99_d00.shp";

		String pathName = countyShapeFileName;
        URL countyShape = acquireURL(pathName);
        assertNotNull("could not find county shapefile " + pathName,countyShape);
        String filepart = pathName.substring(0, pathName.lastIndexOf("."));
        pathName = filepart + ".sld";
        assertTrue("Unable to copy test SLD " + pathName,copyTestFile(pathName));
        URL countySLD = acquireURL( pathName );
        assertNotNull("could not find county sld " + pathName);

		pathName = stateShapeFileName;
        URL stateShape = acquireURL(pathName);
        assertNotNull("could not find state shapefile " + pathName,stateShape);
        filepart = pathName.substring(0, pathName.lastIndexOf("."));
        pathName = filepart + ".sld";
        assertTrue("Unable to copy test SLD " + pathName,copyTestFile(pathName));
        URL stateSLD = acquireURL(pathName);
        assertNotNull("could not find state sld " + pathName);

    	try {
	        MapContext context = new DefaultMapContext(DefaultGeographicCRS.WGS84);

	        ShapefileDataStore countyDataSource = new ShapefileDataStore(countyShape);
	        FeatureSource<SimpleFeatureType,SimpleFeature> countyFeatureSource = 
	        		countyDataSource.getFeatureSource();

	        StyleFactory styleFactory = CommonFactoryFinder.getStyleFactory(null);
	        SLDParser styleReader = new SLDParser(styleFactory,countySLD);
	        org.geotools.styling.Style[] countyStyles = styleReader.readXML();

	        ShapefileDataStore stateDataSource = new ShapefileDataStore(stateShape);
	        FeatureSource<SimpleFeatureType,SimpleFeature> stateFeatureSource = 
	        		stateDataSource.getFeatureSource();

	        styleReader = new SLDParser(styleFactory,stateSLD);
	        org.geotools.styling.Style[] stateStyles = styleReader.readXML();

			StateCountyFilter stateFilter = new StateCountyFilter(stateFeatureSource,false);
			StateCountyFilter countyFilter = new StateCountyFilter(countyFeatureSource,true);

			stateFilter.allow("26161");
			countyFilter.allow("26161");
			countyFilter.allow("26065");
			countyFilter.allow("26093");
			countyFilter.allow("26125");
			countyFilter.allow("26163");
			/*
			stateFilter.allow("17");
			stateFilter.allow("18161");
			countyFilter.allow("18161");
			*/

	        context.addLayer(stateFilter.filter(),stateStyles[0]);
	        context.addLayer(countyFilter.filter(),countyStyles[0]);

	        ReferencedEnvelope layerBounds = context.getLayerBounds();
	        //System.out.println("layerBounds=" + layerBounds);
		    ReferencedEnvelope usaEnvelope = new ReferencedEnvelope(-179.0,-60,17,72, DefaultGeographicCRS.WGS84);
	        ReferencedEnvelope displayEnvelope = layerBounds;
			// Clip displayEnvelope so it is always displayable within the USA boundaries
			displayEnvelope = new ReferencedEnvelope(displayEnvelope.intersection(usaEnvelope),DefaultGeographicCRS.WGS84);
			// Ensure there is a border so that labels can always be drawn
			displayEnvelope.expandBy(displayEnvelope.getWidth()*0.05,displayEnvelope.getHeight()*0.025);

	        GTRenderer renderer = new StreamingRenderer();
			//renderImage(context,renderer,displayEnvelope,"png","testdata/gisBasics.png");
			renderImage(context,renderer,displayEnvelope,"jpg","testdata/gisStatesAndCounties.jpg");
			//GIF doesn't work, so use JPG  renderImage(context,renderer,displayEnvelope,"gif","testdata/gisBasics.gif");
		} catch(Exception e) {
			System.out.println(e.toString());
			e.printStackTrace();
			throw e;
			//fail("Unable to test");
		}
	}

	/**
	 * Generate an image with just counties.
	**/
	public void testBasics() throws Exception {
	    final String SHAPEFILENAME = "libs/geotools/co99_d00.shp";

		String pathname = SHAPEFILENAME;
        URL shape = acquireURL( pathname );
        assertNotNull("could not find shapefile " + pathname,shape);
        String filepart = pathname.substring(0, pathname.lastIndexOf("."));
        URL sld = acquireURL( filepart+".sld" );        
        assertNotNull("could not find sld " + filepart + ".sld");
    	try {
	        ShapefileDataStore ds = new ShapefileDataStore(shape);

	        FeatureSource<SimpleFeatureType,SimpleFeature> fs = ds.getFeatureSource();
	        com.vividsolutions.jts.geom.Envelope env = fs.getBounds();
	        assertNotNull("env was null",env);
	        /** @nonissue **/
	        System.out.println("loadAndTest.env=" + env.toString());

			/*
			FeatureType schema = null;
			AttributeType[] attributeTypes = null;
			FeatureCollection features = fs.getFeatures();
			assertNotNull("features was null",features);
			Iterator fi = features.iterator();
			try {
				for(int count=0;count<5 && fi.hasNext();count++) {
					SimpleFeature feature = (SimpleFeature) fi.next();
					if(schema == null) {
						schema = feature.getFeatureType();
						//System.out.println("schema.getTypeName()=\"" + schema.getTypeName() + "\"");
						attributeTypes = schema.getAttributeTypes();
					}
					// Print features and their attributes, used for debugging
					S ystem.out.println("SimpleFeature: " + feature.getID());
					int attributeCount = feature.getNumberOfAttributes();
					for(int ai=1;ai<attributeCount;ai++) { // skip attribute 0, it is the geometry
						Object a = feature.getAttribute(ai);
						S ystem.out.println("Attribute[" + ai + "] is \"" + attributeTypes[ai].getName() + "\" = \"" + a.toString() + "\"");
					}
				}
			} finally {
				features.close(fi);
			}
			*/

	        //StyleFactory factory = StyleFactoryFinder.createStyleFactory();
	        StyleFactory factory = CommonFactoryFinder.getStyleFactory(null);
	        
	        SLDParser stylereader = new SLDParser(factory,sld);
	        org.geotools.styling.Style[] style = stylereader.readXML();
	        
	        MapContext context = new DefaultMapContext(DefaultGeographicCRS.WGS84);
			//System.out.println("style[0]=" + style[0]);
			StateCountyFilter filter = new StateCountyFilter(fs,false);
			/*
			filter.allow("26161");
			filter.allow("26065");
			filter.allow("26093");
			filter.allow("26163");
			*/

			filter.allow("17");
			filter.allow("18");
			//filter.allow("18161");

			//boolean useWholeUSA = false;
		    ReferencedEnvelope usaEnvelope = new ReferencedEnvelope(-179.0,-60,17,72, DefaultGeographicCRS.WGS84);
			FeatureCollection<SimpleFeatureType,SimpleFeature> filteredFeatures = filter.filter();
	        context.addLayer(filteredFeatures,style[0]);
	        ReferencedEnvelope layerBounds = context.getLayerBounds();
	        ReferencedEnvelope displayEnvelope = layerBounds;
			// Clip displayEnvelope so it is always displayable within the USA boundaries
			if(displayEnvelope != usaEnvelope) {
				displayEnvelope = new ReferencedEnvelope(displayEnvelope.intersection(usaEnvelope),DefaultGeographicCRS.WGS84);
				// Ensure there is a border so that labels can always be drawn
				displayEnvelope.expandBy(displayEnvelope.getWidth()*0.05,displayEnvelope.getHeight()*0.025);
			}
	        GTRenderer renderer = new StreamingRenderer();
			//renderImage(context,renderer,displayEnvelope,"png","testdata/gisBasics.png");
			renderImage(context,renderer,displayEnvelope,"jpg","testdata/gisBasics.jpg");
			//GIF doesn't work, so use JPG  renderImage(context,renderer,displayEnvelope,"gif","testdata/gisBasics.gif");
		} catch(Exception e) {
			System.out.println(e.toString());
			e.printStackTrace();
			throw e;
			//fail("Unable to test");
		}
	}

	/**
	 * Create an image from a GIS map and store it to disk.
	 * @param context a GIS map of state, counties, or both.  context has already been filtered and styled.
	 * @param renderer the Renderer that will do the actual display
	 * @param envelope the area to be displayed
	 * @param imageType "png" or "jpg"
	 * @param imageFileName name of the image file to be created, including the file extension
	**/
    public void renderImage(MapContext context, GTRenderer renderer, Envelope envelope,
    		String imageType, String imageFileName) {
        final int w = 1024;
        final int h = 768;
        BufferedImage image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = image.createGraphics();
        try{
	        g.setColor(Color.white);
	        g.fillRect(0,0,w,h);

			renderer.setContext(context);

			/* Do not antialias as this would complicate manual recoloring of generated images
			RenderingHints hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
			renderer.setJava2DHints(hints);
			*/
			
			Map<Object,Object> rendererParams = new HashMap<Object,Object>();
			rendererParams.put("optimizedDataLoadingEnabled",new Boolean(true));
			renderer.setRendererHints(rendererParams);

	        renderer.paint(g, new Rectangle(0,0,w,h), envelope);

			//Color[] spectrum = generateFireSpectrum(256);
			Color[] spectrum = generateGraySpectrum(256);
			int spectrumWidth = 30;
			drawVerticalSpectrum(g,spectrum,50,w-spectrumWidth-2,spectrumWidth,256*2);
            //ImageIO.write(image, "png", new File("testdata/gtdemo-image.png"));
            ImageIO.write(image, imageType, new File(imageFileName));
        } catch (IOException ioex) {
            System.err.println("IO Exception on image file write: "+ ioex);
			ioex.printStackTrace();
		} catch(Exception e) {
			System.err.println("Exception: " + e);
			e.printStackTrace();
		} catch(Error e) {
			System.err.println("Error: " + e);
			e.printStackTrace();
		} finally {
			g.dispose();
		}
	}

	/**
	 * Display a spectrum vertically
	 * @param g canvas to draw upon
	 * @param spectrum array of Color objects to be drawn
	 * @param top topmost row to draw on the canvas, these are the last entries in the spectrum
	 * @param left leftmost column of the spectrum display
	 * @param width number of columns on the canvas
	 * @param height number of rows on the canvas for the whole spectrum.
	**/
	public static void drawVerticalSpectrum(Graphics2D g, Color[] spectrum,
			int top, int left, int width, int height) {
		double dy = (double)height / (double)(spectrum.length);
		double y = top;
		for(int i=spectrum.length-1;i>=0;i--,y+=dy) {
			g.setColor(spectrum[i]);
			g.fillRect(left,(int)y,width,1+(int)dy);
		}
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
	 * Create a popular color palette known as the "fire palette".  Taken from
	 * http://vis.iu.edu/Publications/Storm.pdf, figure 5, page 7.  The lower
	 * portion of the spectrum is dropped for aesthetic reasons.
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

	/** Test Scale.binarySearch **/
	public void testBinarySearch() {
		double[] binMaximums = {
			2, 5, 7, 11, 12, 19, 23, 27, 31
		};
		int index = Scale.binarySearch(8,binMaximums);
		assertEquals("Binary Search for 8 failed",3,index);

		index = Scale.binarySearch(1,binMaximums);
		assertEquals("Binary Search for 1 failed",0,index);

		index = Scale.binarySearch(100,binMaximums);
		assertEquals("Binary Search for 100 failed",9,index);

		index = Scale.binarySearch(13,binMaximums);
		assertEquals("Binary Search for 13 failed",5,index);

		index = Scale.binarySearch(31,binMaximums);
		assertEquals("Binary Search for 31 failed",8,index);
	}

	/** Test Scale.binarySearch **/
	public void testBinarySearch2() {
		double[] binMaximums = {
			1.3, 1.5
		};
		int index = Scale.binarySearch(2,binMaximums);
		assertEquals("Binary Search for 2 failed",2,index);

		index = Scale.binarySearch(1,binMaximums);
		assertEquals("Binary Search for 1 failed",0,index);
	}

	/** Test Scale.toHex **/
	public void testToHex() {
		Color c = new Color((float)0.5,(float)0,(float)1.0);
		String hex = Scale.toHex(c);
		assertEquals("Color did not convert to hex correctly","8000ff",hex);
	}

	/** Test map generation **/
	public void testMapGeneration() throws Exception {
		String fileName = "testdata/gisMap1.jpg";
		File imageFile = new File(fileName);
		if(imageFile.exists()) {
			imageFile.delete();
		}
		StateCountyMapRenderer map = new StateCountyMapRenderer();
		map.add("26161",314);
		map.add("26065",200);
		map.add("26093",22);
		map.addOutlineOnly("26163");
		map.addOutlineOnly("26125");
		Scale legend = new Scale(32,map.getMinimumValue(),map.getMaximumValue(),
				Scale.PALETTE_FIRE,true);
		BufferedImage image = map.render(legend,true,true);
		ImageUtility.saveJPG(image,fileName);
		assertTrue("Image does not exist",imageFile.exists());
		assertTrue("Image file is zero bytes",imageFile.length() > 0);
		//ImageUtility.print(image);
	}
}
