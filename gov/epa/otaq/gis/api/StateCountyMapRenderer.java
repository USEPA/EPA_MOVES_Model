/**************************************************************************************************
 * @(#)StateCountyMapRenderer.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.gis.api;

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
import java.sql.*;

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

/**
 * Generate an image for a state and/or county map of the USA.
 * The calling pattern is:<br>
 * <ul>
 * <li>instanitate a StateCountyMapRenderer</li>
 * <li>call add(fipsCode,value) or addOutlineOnly(fipsCode) for each
 * state and/or county to be shown.  State outlines are automatically
 * shown for each county added.</li>
 * <li>Optionally, the addFromSQL(...) function can be used to retrieve data
 * from a table and call add(...) and addOutlineOnly(...) with the information.</li>
 * <li>create and fillin a Scale object.  The getMinimumValue() and
 * getMaximumValue() routines can be used to get the range to apply
 * to the Scale.  The Scale object provides the mapping between data value
 * and color.</li>
 * <li>call the render(...) function to create an image of the map.
 * When calling render(...), the Scale must always be provided but it doesn't
 * have to be drawn or its text displayed.</li>
 * <li>Handle any exceptions, especially GISException objects, that are thrown
 * by the called functions.  GISException's toString() function is especially
 * suited to giving a human-readable message.</li>
 * </ul>
 *
 * @author		Wesley Faler
 * @version		2009-04-15
**/
public class StateCountyMapRenderer {
	/** Shape files **/
	static final String[] shapeFiles = {
		"libs/geotools/st99_d00.shp", // states not including Virgin Islands
		"libs/geotools/co78_d00.shp", // counties not including Virgin Islands
		"libs/geotools/co99_d00.shp"  // counties in Virgin Islands
	};
	/** FeatureSource objects derived from the shape files **/
	static ArrayList<FeatureSource<SimpleFeatureType,SimpleFeature> > featureSources = null;
	/** GeoTools renderer **/
	static GTRenderer renderer = null;
	/** StyleFactory shared across all instances **/
	static StyleFactory styleFactory = null;

	/** County shape file **/
	static final String countyShapeFileName = "libs/geotools/co99_d00.shp";
    /** State shape file **/
	static final String stateShapeFileName = "libs/geotools/st99_d00.shp";
    /**
     * Display boundary for the USA.  The extreme western islands of Alaska
     * are clipped by this boundary but this makes the display much easier.
    **/
    static final ReferencedEnvelope usaEnvelope = 
    		new ReferencedEnvelope(-179.0,0,-10,72, DefaultGeographicCRS.WGS84);
//    		new ReferencedEnvelope(-179.0,-60,17,72, DefaultGeographicCRS.WGS84);

	/** GIS map **/
	MapContext context = null;
    /** Filters to extract states/counties from the feature sources **/
    ArrayList<StateCountyFilter> filters = new ArrayList<StateCountyFilter>();
	/** flag indicating if there is a minimum or maximum value yet **/
	boolean hasMinimumAndMaximum = false;
	/** minimum value **/
	double minimumValue = 0;
	/** maximum value **/
	double maximumValue = 0;
	/** values of states and/or counties.  keyed by FIPS code String, data is Double **/
	TreeMap<String,Double> values = new TreeMap<String,Double>();

	/**
	 * Constructor
	 * @throws Exception upon any sort of missing file
	**/
	public StateCountyMapRenderer() throws Exception {
		// Force reclaimation of memory immediately so that the application we launch has as
		// much memory to do its work as possible
		Runtime.getRuntime().runFinalization();
		Runtime.getRuntime().gc();
		Runtime.getRuntime().runFinalization();
		Runtime.getRuntime().gc();

		if(featureSources == null) {
			// Load all feature sources
			featureSources = new ArrayList<FeatureSource<SimpleFeatureType,SimpleFeature> >();
			for(int i=0;i<shapeFiles.length;i++) {
				URL shape = acquireURL(shapeFiles[i]);
				ShapefileDataStore dataStore = new ShapefileDataStore(shape);
				FeatureSource<SimpleFeatureType,SimpleFeature> source = dataStore.getFeatureSource();
				featureSources.add(source);
			}
		}

        context = new DefaultMapContext(DefaultGeographicCRS.WGS84);
	
		int index = 0;
		for(Iterator<FeatureSource<SimpleFeatureType,SimpleFeature> > i=featureSources.iterator();i.hasNext();index++) {
			FeatureSource<SimpleFeatureType,SimpleFeature> source = (FeatureSource<SimpleFeatureType,SimpleFeature>)i.next();
			filters.add(new StateCountyFilter(source,false,shapeFiles[index]));
			filters.add(new StateCountyFilter(source,true,shapeFiles[index]));
		}
	}

	/**
	 * Obtain the minimum value in the data
	 * @return the minimum value successfully passed to the add() routine
	**/
	public double getMinimumValue() {
		return minimumValue;
	}

	/**
	 * Obtain the maximum value in the data
	 * @return the maximum value successfully passed to the add() routine
	**/
	public double getMaximumValue() {
		return maximumValue;
	}

	/**
	 * Add a state or county's data to the display
	 * @param stateOrCountyFIPS state FIPS code (2 digits) or a county FIPS code (5 digits
	 * which includes the 2 digit state FIPS code)
	 * @param value data value for the state or county
	 * @throws GISException if the FIPS code is not properly formatted or if a duplicate entry
	 * is attempted
	**/
	public void add(String stateOrCountyFIPS, double value) throws GISException {
		if(stateOrCountyFIPS.length() == 4 || stateOrCountyFIPS.length() == 1) {
			// 1 or 4 characters is most likely in need of a leading zero.
			// The Shape files store FIPS with leading zeros but databases store
			// them as integers and drop the leading zeros.
			stateOrCountyFIPS = "0" + stateOrCountyFIPS;
		}
		if(values.containsKey(stateOrCountyFIPS)) {
			throw new GISException("Duplicate entry: " + stateOrCountyFIPS);
		}

		for(Iterator<StateCountyFilter> i=filters.iterator();i.hasNext();) {
			StateCountyFilter filter = (StateCountyFilter)i.next();
			filter.allow(stateOrCountyFIPS);
		}
		//stateFilter.allow(stateOrCountyFIPS);
		//countyFilter.allow(stateOrCountyFIPS);

		values.put(stateOrCountyFIPS,new Double(value));

		if(!hasMinimumAndMaximum) {
			hasMinimumAndMaximum = true;
			minimumValue = value;
			maximumValue = value;
		} else {
			if(value < minimumValue) {
				minimumValue = value;
			}
			if(value > maximumValue) {
				maximumValue = value;
			}
		}
	}

	/**
	 * Add a state or county's outline to the display
	 * @param stateOrCountyFIPS state FIPS code (2 digits) or a county FIPS code (5 digits
	 * which includes the 2 digit state FIPS code)
	 * @throws GISException if the FIPS code is not properly formatted
	**/
	public void addOutlineOnly(String stateOrCountyFIPS) throws GISException {
		if(stateOrCountyFIPS.length() == 4 || stateOrCountyFIPS.length() == 1) {
			// 1 or 4 characters is most likely in need of a leading zero.
			// The Shape files store FIPS with leading zeros but databases store
			// them as integers and drop the leading zeros.
			stateOrCountyFIPS = "0" + stateOrCountyFIPS;
		}
		for(Iterator<StateCountyFilter> i=filters.iterator();i.hasNext();) {
			StateCountyFilter filter = (StateCountyFilter)i.next();
			filter.allow(stateOrCountyFIPS);
		}
		//stateFilter.allow(stateOrCountyFIPS);
		//countyFilter.allow(stateOrCountyFIPS);
	}

	/**
	 * Add values from a database query.  null values are added as outline only.
	 * @param db database connection to use
	 * @param sql SQL query containing at least a column with a FIPS code (state or county)
	 * and a numeric value column.
	 * @param fipsColumn name of the column with a state or county FIPS code
	 * @param valueColumn name of the column with a nullable numeric value
	 * @throws SQLException upon a connection or query error
	 * @throws GISException if any of the FIPS codes are not properly formatted
	**/
	public void addFromSQL(Connection db, String sql, String fipsColumn, String valueColumn)
			throws SQLException, GISException {
		PreparedStatement statement = null;
		ResultSet rs = null;
		try {
			statement = db.prepareStatement(sql);
			rs = statement.executeQuery();
			while(rs.next()) {
				String fips = rs.getString(fipsColumn);
				double value = rs.getDouble(valueColumn);
				if(rs.wasNull()) {
					addOutlineOnly(fips);
				} else {
					add(fips,value);
				}
			}
		} catch(SQLException e) {
			throw e;
		} catch(GISException e) {
			throw e;
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
	}

	/**
	 * Generate an image of the data
	 * @param legend Coloration and text to display explaining the map.
	 * When filled with blank bin labels, no text will be generated.
	 * @param shouldDrawLegend true if the legend should be displayed
	 * @param shouldDrawLabels true if labels on states or counties should be displayed
	 * @return an image of the map
	**/
	public BufferedImage render(Scale legend, boolean shouldDrawLegend,
			boolean shouldDrawLabels) throws Exception {
		if(styleFactory == null) {
	        //styleFactory = StyleFactoryFinder.createStyleFactory();
	        styleFactory = CommonFactoryFinder.getStyleFactory(null);
		}

		for(Iterator<StateCountyFilter> i=filters.iterator();i.hasNext();) {
			StateCountyFilter filter = (StateCountyFilter)i.next();
			FeatureCollection<SimpleFeatureType,SimpleFeature> features = filter.filter();
			if(features == null) {
				continue;
			}
			URL sld = acquireURL(generateSLD(filter.isStateLevel(), filter, legend,
					filter.shapeFileName, shouldDrawLabels));
	        SLDParser styleReader = new SLDParser(styleFactory,sld);
    	    org.geotools.styling.Style[] styles = styleReader.readXML();
    	    context.addLayer(features,styles[0]);
		}

        ReferencedEnvelope layerBounds = context.getLayerBounds();
        //System.out.println("layerBounds=" + layerBounds);
	    ReferencedEnvelope usaEnvelope = new ReferencedEnvelope(-179.0,-60,17,72, DefaultGeographicCRS.WGS84);
        ReferencedEnvelope displayEnvelope = layerBounds;
		// Clip displayEnvelope so it is always displayable within the USA boundaries
		displayEnvelope = new ReferencedEnvelope(displayEnvelope.intersection(usaEnvelope),DefaultGeographicCRS.WGS84);

        final int imageWidth = 1024;
        final int imageHeight = 768;
		final int spectrumWidth = 25;
		final int legendWidth = spectrumWidth+2+60;

		// Ensure there is a border so that labels can always be drawn
		Rectangle mapViewport = new Rectangle(5,5,imageWidth-5-legendWidth-5,imageHeight-5-5);
		double xScale = (double)mapViewport.getWidth() / (double)displayEnvelope.getWidth();
		double yScale = (double)mapViewport.getHeight() / (double)displayEnvelope.getHeight();
		double minScale = Math.min(xScale,yScale);
		int newWidth = (int)(mapViewport.getWidth() * minScale / xScale);
		int newHeight = (int)(mapViewport.getHeight() * minScale / yScale);
		mapViewport.setBounds((int)mapViewport.getMinX(),(int)mapViewport.getMinY(),newWidth,newHeight);

		if(renderer == null) {
        	renderer = new StreamingRenderer();
		}

        BufferedImage image = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = image.createGraphics();
        try{
	        g.setColor(Color.white);
	        g.fillRect(0,0,imageWidth,imageHeight);

			renderer.setContext(context);

			/* Do not antialias as this would complicate manual recoloring of generated images
			RenderingHints hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, 
					RenderingHints.VALUE_ANTIALIAS_ON );
			renderer.setJava2DHints(hints);
			*/
			
			Map<Object,Object> rendererParams = new HashMap<Object,Object>();
			rendererParams.put("optimizedDataLoadingEnabled",new Boolean(true));
			renderer.setRendererHints(rendererParams);

	        renderer.paint(g, mapViewport/*new Rectangle(0,0,imageWidth,imageHeight)*/, displayEnvelope);

			if(shouldDrawLegend) {
				legend.drawVerticalSpectrum(g,50,imageWidth-spectrumWidth-2,spectrumWidth,256*2);
			}
		} finally {
			g.dispose();
		}

		return image;
	}

	/**
	 * Filter a set of shapes/features selecting only the states or counties desired.
	**/
	static class StateCountyFilter {
		protected int stateAttributeIndex = -1;
		protected int countyAttributeIndex = -1;

		protected TreeSet<String> acceptableStateCountyValues = new TreeSet<String>();
		protected FeatureSource<SimpleFeatureType,SimpleFeature> fs = null;
		protected boolean filterByCounty = true;

		public String shapeFileName;

		public StateCountyFilter(FeatureSource<SimpleFeatureType,SimpleFeature> fsToUse,
				boolean filterByCountyToUse, String shapeFileNameToUse) {
			fs = fsToUse;
			/*
			if(shapeFileNameToUse.indexOf("78") > 0) {
				FeatureType schema = null;
				AttributeType[] attributeTypes = null;
				FeatureCollection features = null;
				Iterator fi = null;
				try {
					features = fs.getFeatures();
					fi = features.iterator();
					for(int count=0;count<5 && fi.hasNext();count++) {
						SimpleFeature feature = (SimpleFeature) fi.next();
						if(schema == null) {
							schema = feature.getFeatureType();
							S ystem.out.println("schema.getTypeName()=\"" + schema.getTypeName() + "\"");
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
				} catch(Exception e) {
					e.printStackTrace();
				} finally {
					features.close(fi);
				}
			}
			*/

			filterByCounty = filterByCountyToUse;
			shapeFileName = shapeFileNameToUse;
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
				/**
				 * @issue Unable to setup state/county filter: [message]
				 * @explain MOVES is unable to process the state/county map definition files.
				 * Check for an incomplete install and that all files in your MOVES installation
				 * are not read-only.
				**/
				System.out.println("Unable to setup state/county filter: " + e.toString());
				e.printStackTrace();
			}
		}

		public boolean isStateLevel() {
			return !filterByCounty;
		}

		public boolean isCountyLevel() {
			return filterByCounty;
		}

		public TreeSet<String> getAcceptedEntries() {
			return acceptableStateCountyValues;
		}

		public void allow(String value) throws GISException {
			if(!filterByCounty && value.length() > 2) {
				value = value.substring(0,2);
			}
			// Check for general validity
			try {
				Integer.parseInt(value);
			} catch(Exception e) {
				throw new GISException("Invalid FIPS code format: " + value);
			}
			if(filterByCounty) {
				// Check for County validity
				if(value.length() != 2 && value.length() != 5) {
					throw new GISException("Invalid county FIPS code format: " + value);
				}
				if(value.length() == 5) {
					acceptableStateCountyValues.add(value);
				}
			} else {
				// Check for State validity
				if(value.length() != 2) {
					throw new GISException("Invalid state FIPS code format: " + value);
				}
				acceptableStateCountyValues.add(value);
			}
		}

		public FeatureCollection<SimpleFeatureType,SimpleFeature> filter() {
			if(stateAttributeIndex < 0
					|| (filterByCounty && countyAttributeIndex < 0)) {
				return null;
			}
			FeatureCollection<SimpleFeatureType,SimpleFeature> results = 
					FeatureCollections.newCollection();
			boolean foundAny = false;
			try {
				FeatureCollection<SimpleFeatureType,SimpleFeature> features = fs.getFeatures();
				Iterator<SimpleFeature> fi = features.iterator();
				try {
					while(fi.hasNext()) {
						SimpleFeature feature = (SimpleFeature) fi.next();
						if(contains(feature)) {
							results.add(feature);
							foundAny = true;
						}
					}
				} finally {
					features.close(fi);
				}
			} catch(Exception e) {
				/**
				 * @issue Unable to filter feature collection: [message]
				 * @explain MOVES is unable to process the state/county map definition files.
				 * Check for an incomplete install and that all files in your MOVES installation
				 * are not read-only.
				**/
				System.out.println("Unable to filter feature collection: " + e.toString());
				e.printStackTrace();
			}
			if(foundAny) {
				return results;
			} else {
				return null;
			}
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
	 * Create a URL file a file name
	 * @param target name of a file
	 * @return URL to the passed file, throws an exception if the file doesn't exist
	**/
    public static URL acquireURL(String target) throws GISException {
    	File targetFile = new File(target);
    	if(targetFile.exists()) {
        	try {
				return targetFile.toURI().toURL();
			} catch (MalformedURLException e) {
				throw new GISException("Malformated URL exception finding file " + target);
			}
        }
        throw new GISException("Critical file missing: " + target);
    }

	/**
	 * Generate a SLD file describing the colors and text for each rendered state or county.
	 * @param isStateLevel true if state-level styles are being generated
	 * @param filter state or county filter
	 * @param legend Coloration and text to display explaining the map
	 * @param shapeFileName name and path of the shape file being used.  The SLD file is
	 * created with the same name but .SLD extension in the same folder.
	 * @param shouldDrawLabels true if labels on states or counties should be displayed
	 * @return the name of the SLD file including path
	 * @throws IOException upon file operation errors
	**/
	String generateSLD(boolean isStateLevel, StateCountyFilter filter, Scale legend, 
			String shapeFileName, boolean shouldDrawLabels) throws IOException {
        String sldFileName = shapeFileName.substring(0, shapeFileName.lastIndexOf(".")) + ".sld";
        File sldFile = new File(sldFileName);
        PrintWriter writer = null;
        TreeSet<String> displayedItems = filter.getAcceptedEntries();
        //String name = isStateLevel?"st99_d00":"co99_d00";
        int index = shapeFileName.lastIndexOf(".");
        String name = shapeFileName.substring(0,index);
        index = name.lastIndexOf("/");
        name = name.substring(index+1);
        try {
        	writer = new PrintWriter(new BufferedWriter(new java.io.FileWriter(sldFile),64*1024));
			writer.print(
					"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n" +
					"<StyledLayerDescriptor version=\"1.0.0\" xmlns=\"http://www.opengis.net/sld\"\n" +
					"  xmlns:sld=\"http://www.opengis.net/sld\" xmlns:ogc=\"http://www.opengis.net/ogc\"\n" +
					"  xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n" +
					"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" +
					"  xsi:schemaLocation=\"http://www.opengis.net/sld http://schemas.cubewerx.com/schemas/sld/1.0.0-cw/StyledLayerDescriptor.xsd\">\n" +
					"  <NamedLayer>\n" +
					"    <Name>" + name + "</Name>\n" +
					"    <UserStyle>\n" +
					"      <Name>" + name + "</Name>\n" +
					"      <IsDefault>1</IsDefault>\n" +
					"      <FeatureTypeStyle>\n" +
					"        <FeatureTypeName>" + name + "</FeatureTypeName>\n" +
					"        <Rule>\n" +
					"          <PolygonSymbolizer>\n" +
					"            <Stroke>\n" +
					"              <CssParameter name=\"stroke\">#000000</CssParameter>\n" +
					"              <CssParameter name=\"stroke-width\">1</CssParameter>\n" +
					"            </Stroke>\n" +
					"						<Fill>\n" +
					"							<CssParameter name=\"fill\">#FFFFFF</CssParameter>\n" +
					"						</Fill>\n" +
					"          </PolygonSymbolizer>\n" +
					"        </Rule>\n");

			// Iterate over each displayedItem, generating a rule for it
			// The rule will be skipped if there is no data for the item as the default rule
			// will ensure an empty outline is presented.
			// The rule will always include STATE and will include COUNTY if !isStateLevel
			for(Iterator<String> i=displayedItems.iterator();i.hasNext();) {
				String itemFIPS = (String)i.next();
				if(itemFIPS.length() <= 0) {
					continue;
				}
				if(!values.containsKey(itemFIPS)) {
					// Skip entries that will show but with an outline only since the default
					// <Rule> governs their display.
					continue;
				}

				String stateFIPS = itemFIPS;
				if(stateFIPS.length() > 2) {
					stateFIPS = stateFIPS.substring(0,2);
				}
				String countyFIPS = itemFIPS;
				if(countyFIPS.length() >= 5) {
					countyFIPS = countyFIPS.substring(2); // skip 2-digit state FIPS
				} else {
					countyFIPS = "";
				}
				if(!isStateLevel && countyFIPS.length() <= 0) {
					continue;
				}

				writer.print("<Rule>\n" +
						"			<Filter>\n");

				if(isStateLevel) {
					// State-only filter
					writer.print(
							"					<ogc:PropertyIsEqualTo>\n" +
							"						<ogc:PropertyName>STATE</ogc:PropertyName>\n" +
							"						<ogc:Literal>" + stateFIPS + "</ogc:Literal>\n" +
							"					</ogc:PropertyIsEqualTo>\n");
				} else {
					// State and County filter
					writer.print("				<ogc:And>\n" +
							"					<ogc:PropertyIsEqualTo>\n" +
							"						<ogc:PropertyName>STATE</ogc:PropertyName>\n" +
							"						<ogc:Literal>" + stateFIPS + "</ogc:Literal>\n" +
							"					</ogc:PropertyIsEqualTo>\n" +
							"					<ogc:PropertyIsEqualTo>\n" +
							"						<ogc:PropertyName>COUNTY</ogc:PropertyName>\n" +
							"						<ogc:Literal>" + countyFIPS + "</ogc:Literal>\n" +
							"					</ogc:PropertyIsEqualTo>\n" +
							"				</ogc:And>\n");
				}

				Double value = (Double)values.get(itemFIPS);
				int binIndex = legend.getIndex(value.doubleValue());
				String colorHex = legend.getColorHex(binIndex);

				writer.print("			</Filter>\n" +
						"  <PolygonSymbolizer>\n" +
						"    <Stroke>\n" +
						"      <CssParameter name=\"stroke\">#000000</CssParameter>\n" +
						"      <CssParameter name=\"stroke-width\">1</CssParameter>\n" +
						"    </Stroke>\n" +
						"				<Fill>\n" +
						"					<CssParameter name=\"fill\">#" + colorHex + "</CssParameter>\n" +
						"				</Fill>\n" +
						"  </PolygonSymbolizer>\n");
				writer.print("			<TextSymbolizer>\n" +
						"				<Label>" + (shouldDrawLabels?itemFIPS:"&#160;") + "</Label>\n" +
						"				<Font>\n" +
						"					<CssParameter name=\"font-style\">normal</CssParameter>\n" +
						"				</Font>\n" +
						"				<VendorOption name=\"group\">yes</VendorOption>\n" +
						"			</TextSymbolizer>\n");
				writer.print("</Rule>\n");
			}

			writer.print(
				"      </FeatureTypeStyle>\n" +
				"    </UserStyle>\n" +
				"  </NamedLayer>\n" +
				"</StyledLayerDescriptor>\n");

	        return sldFileName;
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
