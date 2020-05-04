package gov.epa.otaq.moves.master.nonroad;

import gov.epa.otaq.moves.common.LogMessageCategory;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import gov.epa.otaq.moves.common.Logger;

/**
 * @author Jizhen Zhao @ IE, UNC CH
 * @version 2012-03-06
 */
public class SccTech_HpminmaxYear {
	/**
	 * @author Administrator
	 *
	 */
	static class TechRecord {
		/**
		 * 
		 */
		String scc = null;
		/**
		 * 
		 */
		int hpmin = -1;
		/**
		 * 
		 */
		int hpmax = -1;
		/**
		 * 
		 */
		int year = -1;
		/**
		 * 
		 */
		String techName = null;
		
		/**
		 * Constructor
		 * @param scc
		 * @param hpmin
		 * @param hpmax
		 * @param year
		 * @param techName
		 */
		public TechRecord(String scc, int hpmin, int hpmax, int year,
				String techName) {
			super();
			this.scc = scc;
			this.hpmin = hpmin;
			this.hpmax = hpmax;
			this.year = year;
			this.techName = techName;
		}

	}
	
	/**
	 * @param scc
	 * @param hpmin
	 * @param hpmax
	 * @param year
	 * @param techName
	 * @return
	 */
	static TechRecord createTechRecord(String scc, int hpmin, int hpmax, int year,
				String techName) {
		return new TechRecord(scc, hpmin, hpmax, year, techName);
	}
	
	/**
	 * @author Administrator
	 *
	 */
	class Hpminmax {
		/**
		 * 
		 */
		/**
		 * 
		 */
		int min, max;
		/**
		 * @param min
		 * @param max
		 */
		Hpminmax(int min, int max){
			this.min = min;
			this.max = max;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return "Hpminmax [min=" + min + ", max=" + max + "]";
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getOuterType().hashCode();
			result = prime * result + max;
			result = prime * result + min;
			return result;
		}
		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Hpminmax other = (Hpminmax) obj;
			if (!getOuterType().equals(other.getOuterType()))
				return false;
			if (max != other.max)
				return false;
			if (min != other.min)
				return false;
			return true;
		}
		/**
		 * @return
		 */
		private SccTech_HpminmaxYear getOuterType() {
			return SccTech_HpminmaxYear.this;
		}
		
	}
	
	/**
	 * @author Administrator
	 *
	 */
	class YearTech {
		/**
		 * @author Administrator
		 *
		 */
		class TechFraction {
			/**
			 * 
			 */
			Map<String,Double>techFrac = new TreeMap<String,Double>();
			/**
			 * 
			 */
			double totalFrac = 0;
			/**
			 * 
			 */
			TechFraction(){
			}
			/**
			 * @param techName
			 * @param frac
			 */
			void add(String techName, double frac) {
				techFrac.put(techName, frac);
				totalFrac += frac;
			}
			/**
			 * 
			 */
			void clear() {
				techFrac.clear();
			}
			/* (non-Javadoc)
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				String retStr = "TechFraction [techFrac=\n";
				for ( Map.Entry<String, Double>entry : techFrac.entrySet()) {
					retStr += entry.getKey() + ": " + entry.getValue() + "\n";
				}
				retStr += "]";
				return retStr;
			}
			
		}
		/**
		 * 
		 */
		Map<Integer,TechFraction> yearTech = new TreeMap<Integer,TechFraction>();
		
		/**
		 * 
		 */
		YearTech() {}
		/**
		 * @param year
		 * @param techName
		 * @param frac
		 */
		void add(int year, String techName, double frac) {
			TechFraction techFrac = yearTech.get(year);
			if (techFrac == null){
				techFrac = new TechFraction();
				yearTech.put(year, techFrac);
			}
			techFrac.add(techName, frac);
			
		}
		/**
		 * @param year
		 * @return
		 */
		TechFraction get(int year) {
			return yearTech==null ? null : yearTech.get(new Integer(year));
		}
		/**
		 * 
		 */
		void clear() {
			yearTech.clear();
		}
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			String retStr = "YearTech [yearTech=\n===";
			for ( Map.Entry<Integer, TechFraction> entry : yearTech.entrySet()) {
				retStr += "---\n";
				retStr += entry.getKey() + ":\n";
				retStr += entry.getValue() + "\n";
				retStr += "---\n";
			}
			
			retStr +="===\n]";
			return retStr;
		}
		
	}
	/**
	 * 
	 */
	Map<Hpminmax,YearTech> minmaxYear = new HashMap<Hpminmax,YearTech>();
	/**
	 * 
	 */
	String scc = null;
	/**
	 * 
	 */
	Hpminmax minmax = null; // use only one for now
	/**
	 * 
	 */
	SccTech_HpminmaxYear() {}
	/**
	 * @param scc
	 * @param hpmin
	 * @param hpmax
	 * @param year
	 * @param techName
	 * @param frac
	 * @return
	 */
	boolean add(String scc, int hpmin, int hpmax, int year, String techName, double frac) {
		if (this.scc == null) {
			this.scc = scc;
		} else {
			if (!this.scc.equalsIgnoreCase(scc)) {
				Logger.log(LogMessageCategory.INFO, "SCC does not match: should be " + this.scc + ", but is " + scc);
				return false;
			}
		}
		Hpminmax minmax = new Hpminmax(hpmin,hpmax);
		if ( this.minmax == null) {
			this.minmax = minmax;
		} else {
			if (!this.minmax.equals(minmax)) {
				Logger.log(LogMessageCategory.INFO, "Minmax does not match: shouldbe " + this.minmax + ", but is " + minmax);
				return false;
			}
		}
		
		YearTech yearTech = minmaxYear.get(minmax);
		if (yearTech == null){
			yearTech = new YearTech();
			minmaxYear.put(minmax, yearTech);
		}
		yearTech.add(year, techName, frac);
		
		return true;
	}
	/**
	 * 
	 */
	void clear() {
		minmaxYear.clear();
	}
	/**
	 * @param hpmin
	 * @param hpmax
	 * @return
	 */
	YearTech getYearTech(int hpmin, int hpmax) {
		return minmaxYear.get(new Hpminmax(hpmin, hpmax));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String retStr = "SccTech_HpminmaxYear [\nscc="
				+ scc + ", \nminmax=" + minmax + "\nminmaxYear=\n" + minmaxYear + "]\n";
		return retStr;
	}
	
	public static void main(String[] args) {
		
		SccTech_HpminmaxYear minmaxYear = new SccTech_HpminmaxYear();
		
		String [] SCCs = {"01000", "02000"};
		int [] MINs = {1,5,10};
		int [] years = {2001, 2002};
		String [] techs = {"tech1", "tech2", "tech3"};
		double [] fractions = {0.1, 0.3, 0.6};
		
		boolean isOK = true;
		for (String scc : SCCs) {
			for ( int hpmin : MINs) {
				for (int year : years) {
					int i = 0;
					for (String techName : techs) {
							isOK = minmaxYear.add(scc, hpmin, hpmin+5, year, techName, fractions[i]);
							String msg = "Insert [" + scc + ", " + hpmin + ", " +
							             (hpmin+5) + ", " + year + ", " + techName + 
							             ", " + fractions[i] + "]: ";
							msg += isOK ? "succeeded." : "failed";
							System.out.println(msg);
							i++;
					}
				}
			}
		}
		System.out.println(minmaxYear);
	}
	
}

