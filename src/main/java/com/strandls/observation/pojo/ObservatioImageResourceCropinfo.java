package com.strandls.observation.pojo;

import com.strandls.resource.pojo.ResourceData;

public class ObservatioImageResourceCropinfo {
	private String cropStatus;
	private Long[] bbox;
	private ResourceData resource;

	public ObservatioImageResourceCropinfo() {
		super();
		// TODO Auto-generated constructor stub
	}

	public ObservatioImageResourceCropinfo(String cropStatus, Long[] bbox, ResourceData resource) {
		super();
		this.cropStatus = cropStatus;
		this.bbox = bbox;
		this.resource = resource;
	}

	public String getCropStatus() {
		return cropStatus;
	}

	public void setCropStatus(String cropStatus) {
		this.cropStatus = cropStatus;
	}

	public Long[] getBbox() {
		return bbox;
	}

	public void setBbox(Long[] bbox) {
		this.bbox = bbox;
	}

	public ResourceData getResource() {
		return resource;
	}

	public void setResource(ResourceData resource) {
		this.resource = resource;
	}

}
