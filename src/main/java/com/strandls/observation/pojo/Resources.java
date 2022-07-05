package com.strandls.observation.pojo;

import java.util.ArrayList;
import java.util.List;

public class Resources {
	Long id; // Observation id
	String cropStatus;
	List<ObservatioImageResourceCropinfo> observationResource;

	public Resources() {
		super();
		// TODO Auto-generated constructor stub
	}

	public Resources(Long id, String cropStatus, List<ObservatioImageResourceCropinfo> observationResource) {
		super();
		this.id = id;
		this.cropStatus = cropStatus;
		this.observationResource = observationResource;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getCropStatus() {
		return cropStatus;
	}

	public void setCropStatus(String cropStatus) {
		this.cropStatus = cropStatus;
	}

	public List<ObservatioImageResourceCropinfo> getObservationResource() {
		return observationResource;
	}

	public void setObservationResource(List<ObservatioImageResourceCropinfo> observationResource) {
		this.observationResource = observationResource;
	}

}
