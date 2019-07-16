/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

/**
 * @author Abhishek Rudra
 *
 */
public class ShowData {

	private Observation observation;
	private List<FactValuePair> factValuePair;


	/**
	 * @param observation
	 * @param factValuePair
	 */
	public ShowData(Observation observation, List<FactValuePair> factValuePair) {
		this.observation = observation;
		this.factValuePair = factValuePair;
	}

	public Observation getObservation() {
		return observation;
	}

	public void setObservation(Observation observation) {
		this.observation = observation;
	}

	public List<FactValuePair> getFactValuePair() {
		return factValuePair;
	}

	public void setFactValuePair(List<FactValuePair> factValuePair) {
		this.factValuePair = factValuePair;
	}
}
