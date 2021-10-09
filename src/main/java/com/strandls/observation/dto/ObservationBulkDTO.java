package com.strandls.observation.dto;

import java.util.Date;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationBulkDTO {
    // title
    private String title;
    private String summary;
    private String description;
    private Date createdOn;

    // usage rights
    private Long licenseId;

    // party
    private Long contributors;
    private String attribution;

    // taxonomic coverage
    private String sGroup;
    
    private String userGroup;

    // temporal coverage
    private String dateAccuracy;
    private Date observedFromDate;
    private Date observedToDate;

    // geographical coverage
    private String locationScale;
    private String locationAccuracy;
    private String observedAt;
    private String reverseGeocoded;
    private String wktString;
    private Double latitude;
    private Double longitude;
    private Boolean useDegMinSec;
    private String degMinSec;
    private Boolean hidePreciseLocation;

    // others
    private String project;
    private String methods;
    private String basisOfData;
    private String basisOfRecord;

    // other fields related to bulk upload
    private Boolean isVerified;
    private Long dataset;
    private String filename;
    private Long languageId;
    private Map<String, Integer> columns;
    private Map<String, Integer> checklistAnnotation;

    public String getTitle() {
        return title;
    }
    public void setTitle(String title) {
        this.title = title;
    }
    public String getSummary() {
        return summary;
    }
    public void setSummary(String summary) {
        this.summary = summary;
    }
    public String getDescription() {
        return description;
    }
    public void setDescription(String description) {
        this.description = description;
    }
    public Date getCreatedOn() {
        return createdOn;
    }
    public void setCreatedOn(Date createdOn) {
        this.createdOn = createdOn;
    }
    public Long getLicenseId() {
        return licenseId;
    }
    public void setLicenseId(Long licenseId) {
        this.licenseId = licenseId;
    }
    public Long getContributors() {
        return contributors;
    }
    public void setContributors(Long contributors) {
        this.contributors = contributors;
    }
    public String getAttribution() {
        return attribution;
    }
    public void setAttribution(String attribution) {
        this.attribution = attribution;
    }
    public String getSGroup() {
        return sGroup;
    }
    public void setSGroup(String sGroup) {
        this.sGroup = sGroup;
    }
    public String getDateAccuracy() {
        return dateAccuracy;
    }
    public void setDateAccuracy(String dateAccuracy) {
        this.dateAccuracy = dateAccuracy;
    }
    public Date getObservedFromDate() {
        return observedFromDate;
    }
    public void setObservedFromDate(Date observedFromDate) {
        this.observedFromDate = observedFromDate;
    }
    public Date getObservedToDate() {
        return observedToDate;
    }
    public void setObservedToDate(Date observedToDate) {
        this.observedToDate = observedToDate;
    }
    public String getLocationScale() {
        return locationScale;
    }
    public void setLocationScale(String locationScale) {
        this.locationScale = locationScale;
    }
    public String getLocationAccuracy() {
        return locationAccuracy;
    }
    public void setLocationAccuracy(String locationAccuracy) {
        this.locationAccuracy = locationAccuracy;
    }
    public String getObservedAt() {
        return observedAt;
    }
    public void setObservedAt(String observedAt) {
        this.observedAt = observedAt;
    }
    public String getReverseGeocoded() {
        return reverseGeocoded;
    }
    public void setReverseGeocoded(String reverseGeocoded) {
        this.reverseGeocoded = reverseGeocoded;
    }
    public Double getLatitude() {
        return latitude;
    }
    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }
    public Double getLongitude() {
        return longitude;
    }
    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }
    public Boolean getUseDegMinSec() {
        return useDegMinSec;
    }
    public void setUseDegMinSec(Boolean useDegMinSec) {
        this.useDegMinSec = useDegMinSec;
    }
    public String getDegMinSec() {
        return degMinSec;
    }
    public void setDegMinSec(String degMinSec) {
        this.degMinSec = degMinSec;
    }
    public Boolean getHidePreciseLocation() {
        return hidePreciseLocation;
    }
    public void setHidePreciseLocation(Boolean hidePreciseLocation) {
        this.hidePreciseLocation = hidePreciseLocation;
    }
    public String getProject() {
        return project;
    }
    public void setProject(String project) {
        this.project = project;
    }
    public String getMethods() {
        return methods;
    }
    public void setMethods(String methods) {
        this.methods = methods;
    }
    public String getBasisOfData() {
        return basisOfData;
    }
    public void setBasisOfData(String basisOfData) {
        this.basisOfData = basisOfData;
    }
    public Long getDataset() {
        return dataset;
    }
    public void setDataset(Long dataset) {
        this.dataset = dataset;
    }
    public String getFilename() {
        return filename;
    }
    public void setFilename(String filename) {
        this.filename = filename;
    }
    public Long getLanguageId() {
        return languageId;
    }
    public void setLanguageId(Long languageId) {
        this.languageId = languageId;
    }
    public Map<String, Integer> getColumns() {
        return columns;
    }
    public void setColumns(Map<String, Integer> columns) {
        this.columns = columns;
    }
	public Boolean getIsVerified() {
		return isVerified;
	}
	public void setIsVerified(Boolean isVerified) {
		this.isVerified = isVerified;
	}
	public Map<String, Integer> getChecklistAnnotation() {
		return checklistAnnotation;
	}
	public void setChecklistAnnotation(Map<String, Integer> checklistAnnotation) {
		this.checklistAnnotation = checklistAnnotation;
	}
	public String getWktString() {
		return wktString;
	}
	public void setWktString(String wktString) {
		this.wktString = wktString;
	}
	public String getBasisOfRecord() {
		return basisOfRecord;
	}
	public void setBasisOfRecord(String basisOfRecord) {
		this.basisOfRecord = basisOfRecord;
	}
	public String getUserGroup() {
		return userGroup;
	}
	public void setUserGroup(String userGroup) {
		this.userGroup = userGroup;
	}
}
