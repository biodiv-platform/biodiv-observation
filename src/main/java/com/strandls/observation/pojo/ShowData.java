/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.traitsModule.pojo.FactValuePair;
import com.strandls.userGroup.pojo.UserGroupIbp;

/**
 * @author Abhishek Rudra
 *
 */

public class ShowData {

	private Observation observation;
	private List<FactValuePair> factValuePair;
	private List<ObservationResourceUser> observationResoruce;
	private List<UserGroupIbp> userGroups;

	/**
	 * @param observation
	 * @param factValuePair
	 * @param observationResoruce
	 * @param userGroups
	 */
	public ShowData(Observation observation, List<FactValuePair> factValuePair,
			List<ObservationResourceUser> observationResoruce, List<UserGroupIbp> userGroups) {
		super();
		this.observation = observation;
		this.factValuePair = factValuePair;
		this.observationResoruce = observationResoruce;
		this.userGroups = userGroups;
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

	public List<ObservationResourceUser> getObservationResoruce() {
		return observationResoruce;
	}

	public void setObservationResoruce(List<ObservationResourceUser> observationResoruce) {
		this.observationResoruce = observationResoruce;
	}

	public List<UserGroupIbp> getUserGroups() {
		return userGroups;
	}

	public void setUserGroups(List<UserGroupIbp> userGroups) {
		this.userGroups = userGroups;
	}

}
