/**************************************************************************************************
 * @(#)EmissionCalculatorRegistration.java
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.*;

/**
 * An instance of this class represents a single registration of an EmissionCalculator object
 * for an EmissionProcess/Pollutant combination. This class has static functionality that provides
 * registration and find functionality. This is designed to facilitate EmissionCalculator chaining
 * (see Overview for more details).
 *
 * @author		Wesley Faler
 * @author		M. Cumberworth (bug fix in register method)
 * @version		2013-11-27
**/
public class EmissionCalculatorRegistration implements Comparable {
	/** The set of EmissionCalculatorRegistration objects. **/
	static TreeSet<EmissionCalculatorRegistration> registeredCalculators
			= new TreeSet<EmissionCalculatorRegistration>();

	/** The Pollutant that the registration is for **/
	public Pollutant pollutant;
	/** The EmissionProcess that the registration is for **/
	public EmissionProcess process;
	/** The EmissionCalculator that the registration is for **/
	public EmissionCalculator emissionCalculator;

	/**
	 * Clear the calculator registrations.  This should be done before instantiating
	 * any EmissionCalculator-derived objects.
	**/
	public static void reset() {
		registeredCalculators.clear();
	}

	/**
	 * This will typically be called by concrete EmissionCalculator sub-classes in their
	 * constructors.
	 * @param targetPollutant The pollutant being registered for.
	 * @param targetProcess The EmissionProcess being registered for.
	 * @param targetCalculator The EmissionCalculator being registered.
	**/
	public static void register(Pollutant targetPollutant, EmissionProcess targetProcess,
			EmissionCalculator targetCalculator) {
		if((targetPollutant != null) && (targetProcess != null)) {
			EmissionCalculatorRegistration r = new EmissionCalculatorRegistration();
			r.pollutant = targetPollutant;
			r.process = targetProcess;
			r.emissionCalculator = targetCalculator;
			registeredCalculators.add(r);
			InterconnectionTracker.recordRegistration(r);
		}
	}

	/**
	 * Finds all EmissionCalculators that are registered for the specific Polluant/EmissionProcess
	 * combination. Returns a LinkedList of EmissionCalculator objects. This is typically called
	 * by other EmissionCalculator objects in their subscribeToMe methods for chaining purposes.
	 * @param targetPollutant The pollutant to search for EmissionCalculator's for.
	 * May not be null.
	 * @param targetProcess The emission process to search for EmissionCalculator's for.
	 * May not be null.
	 * @return A LinkedList of EmissionCalculator objects and is never null even if no 
	 * matching calculators were found.
	**/
	public static LinkedList<EmissionCalculator> find(Pollutant targetPollutant, 
			EmissionProcess targetProcess) {
		LinkedList<EmissionCalculator> results = new LinkedList<EmissionCalculator>();
		if(targetProcess == null) {
			return results;
		}
		for(Iterator<EmissionCalculatorRegistration> i=registeredCalculators.iterator();
				i.hasNext();) {
			EmissionCalculatorRegistration r = i.next();
			if(targetPollutant == r.pollutant
					&& (targetProcess == null || targetProcess == r.process)) {
				results.add(r.emissionCalculator);
			}
		}
		return results;
	}

	/**
	 * Finds all EmissionCalculators that are registered for a specific Polluant
	 * Returns a LinkedList of EmissionCalculator objects. This is typically called
	 * by other EmissionCalculator objects in their subscribeToMe methods for chaining purposes.
	 * @param targetPollutant The pollutant to search for EmissionCalculator's for.
	 * May not be null.
	 * @return A LinkedList of EmissionCalculator objects and is never null even if no 
	 * matching calculators were found.
	**/
	public static LinkedList<EmissionCalculator> findPollutant(Pollutant targetPollutant) {
		LinkedList<EmissionCalculator> results = new LinkedList<EmissionCalculator>();
		for(Iterator<EmissionCalculatorRegistration> i=registeredCalculators.iterator();
				i.hasNext();) {
			EmissionCalculatorRegistration r = i.next();
			if(targetPollutant == r.pollutant) {
				results.add(r.emissionCalculator);
			}
		}
		return results;
	}

	/**
	 * Finds all EmissionCalculators that are registered for the specific Polluant but
	 * not for the specified EmissionProcess.  This is useful when finding all calculators
	 * except those that produce Well-To-Pump emissions.
	 * Returns a LinkedList of EmissionCalculator objects. This is typically called
	 * by other EmissionCalculator objects in their subscribeToMe methods for chaining purposes.
	 * @param targetPollutant The pollutant to search for EmissionCalculator's for.
	 * May not be null.
	 * @param excludedProcess The emission process to search for EmissionCalculator's for.
	 * May be null, implying all processes match.
	 * @return A LinkedList of EmissionCalculator objects and is never null even if no 
	 * matching calculators were found.
	**/
	public static LinkedList<EmissionCalculator> findNotInProcess(Pollutant targetPollutant, 
			EmissionProcess excludedProcess) {
		LinkedList<EmissionCalculator> results = new LinkedList<EmissionCalculator>();
		if(targetPollutant == null) {
			return results;
		}

		for(Iterator<EmissionCalculatorRegistration> i=registeredCalculators.iterator();
				i.hasNext();) {
			EmissionCalculatorRegistration r = i.next();
			if(targetPollutant == r.pollutant && (excludedProcess == null || excludedProcess != r.process)) {
				results.add(r.emissionCalculator);
			}
		}
		return results;
	}

	/**
	 * Add a list of calculators to an existing set, taking care not to add duplicates.
	 * @param destinationSet list to be filled without duplicates
	 * @param sourceCalculators list of calculators, typically returned by one of the find routines
	**/
	public static void merge(LinkedList<EmissionCalculator> destinationSet, 
			LinkedList<EmissionCalculator> sourceCalculators) {
		if(destinationSet == null || sourceCalculators == null) {
			return;
		}
		for(Iterator<EmissionCalculator> i=sourceCalculators.iterator();i.hasNext();) {
			EmissionCalculator c = i.next();
			if(!destinationSet.contains(c)) {
				destinationSet.add(c);
			}
		}
	}

	/**
	 * Default constructor
	**/
	public EmissionCalculatorRegistration() {
	}

	/**
	 * This compares two EmissionCalculatorRegistration objects. This provides a consistent
	 * arbitrary ordering.
	 *
	 * @param o The Object to be compared.
	 * @return a negative integer, zero, or a positive integer as this object is less than,
	 * equal to, or greater than the specified object.
	**/
	public int compareTo(Object o) {
		EmissionCalculatorRegistration other = (EmissionCalculatorRegistration)o;
		if(pollutant != other.pollutant) {
			return pollutant.compareTo(other.pollutant);
		}
		if(process != other.process) {
			return process.compareTo(other.process);
		}
		if(emissionCalculator != other.emissionCalculator) {
			return emissionCalculator.hashCode() - other.emissionCalculator.hashCode();
		}

		return 0;
	}
}
