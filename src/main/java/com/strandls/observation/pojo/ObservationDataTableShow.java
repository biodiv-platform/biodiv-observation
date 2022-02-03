package com.strandls.observation.pojo;

import java.util.Map;

import com.strandls.user.pojo.UserIbp;

public class ObservationDataTableShow {

	private Long id;

	private String scientificName;

	private String commonName;

	private Long sGroup;

	private String fromDate;
	
	private String toDate;

	private String observedAt;

	private String locationScale;

	private Double longitude;

	private Double latitude;

	private String dateAccuracy;

	private String notes;

	private Boolean geoPrivacy;

	private Map<String, Object> checklistAnnotation;
	
	private UserIbp userInfo;

	public ObservationDataTableShow() {
		super();
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getScientificName() {
		return scientificName;
	}

	public void setScientificName(String scientificName) {
		this.scientificName = scientificName;
	}

	public String getCommonName() {
		return commonName;
	}

	public void setCommonName(String commonName) {
		this.commonName = commonName;
	}

	public Long getsGroup() {
		return sGroup;
	}

	public void setsGroup(Long sGroup) {
		this.sGroup = sGroup;
	}

	public String getFromDate() {
		return fromDate;
	}

	public void setFromDate(String fromDate) {
		this.fromDate = fromDate;
	}

	public String getObservedAt() {
		return observedAt;
	}

	public void setObservedAt(String observedAt) {
		this.observedAt = observedAt;
	}

	public String getLocationScale() {
		return locationScale;
	}

	public void setLocationScale(String locationScale) {
		this.locationScale = locationScale;
	}

	public Double getLongitude() {
		return longitude;
	}

	public void setLongitude(Double longitude) {
		this.longitude = longitude;
	}

	public Double getLatitude() {
		return latitude;
	}

	public void setLatitude(Double latitude) {
		this.latitude = latitude;
	}

	public String getDateAccuracy() {
		return dateAccuracy;
	}

	public void setDateAccuracy(String dateAccuracy) {
		this.dateAccuracy = dateAccuracy;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public Boolean getGeoPrivacy() {
		return geoPrivacy;
	}

	public void setGeoPrivacy(Boolean geoPrivacy) {
		this.geoPrivacy = geoPrivacy;
	}

	public Map<String, Object> getChecklistAnnotation() {
		return checklistAnnotation;
	}

	public void setChecklistAnnotation(Map<String, Object> checklistAnnotation) {
		this.checklistAnnotation = checklistAnnotation;
	}

	public UserIbp getUserInfo() {
		return userInfo;
	}

	public void setUserInfo(UserIbp userInfo) {
		this.userInfo = userInfo;
	}

	public String getToDate() {
		return toDate;
	}

	public void setToDate(String toDate) {
		this.toDate = toDate;
	}


}
