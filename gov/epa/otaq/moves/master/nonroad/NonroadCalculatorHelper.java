package gov.epa.otaq.moves.master.nonroad;

import gov.epa.otaq.moves.master.framework.Pollutant;

public class NonroadCalculatorHelper {

	public static enum NonroadProc {
		RunningExhaust("Running Exhaust"), EvapPermeation("Evap Permeation"), EvapFuelVaporVenting(
				"Evap Fuel Vapor Venting"), CrankcaseRunningExhaust(
				"Crankcase Running Exhaust"), RefuelingDisplacementVaporLoss(
				"Refueling Displacement Vapor Loss"), RefuelingSpillageLoss(
				"Refueling Spillage Loss");

		private final String name;

		private NonroadProc(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}

		@Override
		public String toString() {
			return name;
		}
	};

	public static String NonroadProcessNames[] = { "Nonroad"
	// "Running Exhaust",
	// "Evap Permeation",
	// "Evap Fuel Vapor Venting",
	// "Crankcase Running Exhaust",
	// "Refueling Displacement Vapor Loss",
	// "Refueling Spillage Loss"
	};

	public static Pollutant[] getAllPollutants() {
		/** NR_IMP: **/
		// need to filter out the pollutants that does not belong to Nonroad
		/** NR_IMP **/
		//
		Pollutant[] allPollutants = Pollutant.allPollutants
				.toArray(new Pollutant[0]); // getAllPollutants();
		// Pollutant[] allPollutants = pollutants;
		return allPollutants;
	}
}
