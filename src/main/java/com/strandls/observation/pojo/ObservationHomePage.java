package com.strandls.observation.pojo;

import com.strandls.observation.es.util.ObservationListMinimalData;

public class ObservationHomePage {

	private String resourceUrl;
	private ObservationListMinimalData observation;

	/**
	 * 
	 */
	public ObservationHomePage() {
		super();
	}

	/**
	 * @param resourceUrl
	 * @param observation
	 */
	public ObservationHomePage(String resourceUrl, ObservationListMinimalData observation) {
		super();
		this.resourceUrl = resourceUrl;
		this.observation = observation;
	}

	public String getResourceUrl() {
		return resourceUrl;
	}

	public void setResourceUrl(String resourceUrl) {
		this.resourceUrl = resourceUrl;
	}

	public ObservationListMinimalData getObservation() {
		return observation;
	}

	public void setObservation(ObservationListMinimalData observation) {
		this.observation = observation;
	}

}
