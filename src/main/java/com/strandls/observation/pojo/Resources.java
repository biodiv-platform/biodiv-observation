package com.strandls.observation.pojo;

import java.util.List;

public class Resources {
	Long id; // Observation id
	String curationStatus;
	List<ObservatioImageResourceCropinfo> observationResource;

	public Resources() {
		super();
	}

	public Resources(Long id, String curationStatus, List<ObservatioImageResourceCropinfo> observationResource) {
		super();
		this.id = id;
		this.curationStatus = curationStatus;
		this.observationResource = observationResource;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getCurationStatus() {
		return curationStatus;
	}

	public void setCurationStatus(String curationStatus) {
		this.curationStatus = curationStatus;
	}

	public List<ObservatioImageResourceCropinfo> getObservationResource() {
		return observationResource;
	}

	public void setObservationResource(List<ObservatioImageResourceCropinfo> observationResource) {
		this.observationResource = observationResource;
	}

}
