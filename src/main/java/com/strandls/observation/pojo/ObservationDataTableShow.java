package com.strandls.observation.pojo;

import java.util.Date;
import java.util.Map;

public class ObservationDataTableShow {

	private Long id;

	private String scientificName;

	private String commonName;

	private Long user;

	private Long sGroup;

	private Long userGroups;

	private String fromDate;

	private String observedAt;

	private String locationScale;

	private Double longitude;

	private Double latitude;

	private String dateAccuracy;

	private String notes;

	private Boolean geoPrivacy;

	private Map<String, Object> checklistAnnotation;

	public ObservationDataTableShow(Long id, String scientificName, String commonName, Long user, Long sGroup,
			Long userGroups, String fromDate, String observedAt, String locationScale, Double longitude, Double latitude,
			String dateAccuracy, String notes, Boolean geoPrivacy, Map<String, Object> checklistAnnotation) {
		super();
		this.id = id;
		this.scientificName = scientificName;
		this.commonName = commonName;
		this.user = user;
		this.sGroup = sGroup;
		this.userGroups = userGroups;
		this.fromDate = fromDate;
		this.observedAt = observedAt;
		this.locationScale = locationScale;
		this.longitude = longitude;
		this.latitude = latitude;
		this.dateAccuracy = dateAccuracy;
		this.notes = notes;
		this.geoPrivacy = geoPrivacy;
		this.checklistAnnotation = checklistAnnotation;
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

	public Long getUser() {
		return user;
	}

	public void setUser(Long user) {
		this.user = user;
	}

	public Long getsGroup() {
		return sGroup;
	}

	public void setsGroup(Long sGroup) {
		this.sGroup = sGroup;
	}

	public Long getUserGroups() {
		return userGroups;
	}

	public void setUserGroups(Long userGroups) {
		this.userGroups = userGroups;
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

}
