/**************************************************************************************************
 * @(#)IResource.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph;

/**
 * Describe a resource that is read or written to by an INode.
 * Resources have a naming convention that allows fragmented/partial
 * usage.  For instance, if a table is written to by multiple nodes
 * but their data does not overlap, they would use the table as their
 * resource name but each add a separator and a description of the
 * dataset they contribute.
 * 
 * @author		Wesley Faler
 * @version		2010-01-16
**/
public interface IResource {
	/**
	 * Obtain the standardized URI-esque name of the resource.
	 * For example, if a table is written to by multiple nodes
	 * but their data does not overlap, they would use the table as their
	 * resource name but each add a separator and a description of the
	 * dataset they contribute.
	 * @return name of the resource with the pipe character ('|')
	 * at the beginning, end, and between each segment of the name.
	**/
	String getResourcePath();
}
