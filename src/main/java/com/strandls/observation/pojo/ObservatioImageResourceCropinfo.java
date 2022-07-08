package com.strandls.observation.pojo;

import com.strandls.resource.pojo.License;
import com.strandls.resource.pojo.Resource;

public class ObservatioImageResourceCropinfo {
	private String cropStatus;
	private Long[] bbox;
	private Resource resource;
	private com.strandls.resource.pojo.UserIbp userIbp;
	private License license;

	public ObservatioImageResourceCropinfo() {
		super();
		// TODO Auto-generated constructor stub
	}

	public ObservatioImageResourceCropinfo(String cropStatus, Long[] bbox, Resource resource,
			com.strandls.resource.pojo.UserIbp userIbp, License license) {
		super();
		this.cropStatus = cropStatus;
		this.bbox = bbox;
		this.resource = resource;
		this.userIbp = userIbp;
		this.license = license;
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

	public Resource getResource() {
		return resource;
	}

	public void setResource(Resource resource) {
		this.resource = resource;
	}

	public com.strandls.resource.pojo.UserIbp getUserIbp() {
		return userIbp;
	}

	public void setUserIbp(com.strandls.resource.pojo.UserIbp userIbp) {
		this.userIbp = userIbp;
	}

	public License getLicense() {
		return license;
	}

	public void setLicense(License license) {
		this.license = license;
	}

}
