/**************************************************************************************************
 * @(#)BasicResourceImpl.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph;

/**
 * Provides a basic String-based implementation of IResource.
 * 
 * @author		Wesley Faler
 * @version		2010-01-16
**/
public class BasicResourceImpl implements IResource {
	/** Full path of the resource including end-delimiters **/
	private String resourcePath = "||";

	/**
	 * Constructor that sets an empty resource.
	**/
	public BasicResourceImpl() {
	}

	/**
	 * Constructor that sets the resource path.
	 * @param r the pipe ('|') delimited resource path to be used
	**/
	public BasicResourceImpl(String r) {
		setResourcePath(r);
	}

	/**
	 * Obtain the standardized URI-esque name of the resource.
	 * For example, if a table is written to by multiple nodes
	 * but their data does not overlap, they would use the table as their
	 * resource name but each add a separator and a description of the
	 * dataset they contribute.
	 * @return name of the resource with the pipe character ('|')
	 * at the beginning, end, and between each segment of the name.
	**/
	public String getResourcePath() {
		return resourcePath;
	}

	/**
	 * Set the resource path.
	 * @param r the pipe ('|') delimited resource path to be used
	**/
	public void setResourcePath(String r) {
		if(r == null) {
			r = "";
		}
		r = r.trim();
		if(!r.startsWith("|")) {
			r = "|" + r;
		}
		if(!r.endsWith("|")) {
			r += "|";
		}
		resourcePath = r;
	}
}
