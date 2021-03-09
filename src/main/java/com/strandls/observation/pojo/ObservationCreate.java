/**
 * 
 */
package com.strandls.observation.pojo;

import java.sql.Date;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.strandls.utility.pojo.Tags;

/**
 * @author Abhishek Rudra
 *
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationCreate {

//	----Core Observation Data-------
	private Long sGroup;
	private Boolean helpIdentify;
	private Date createdOn;
	private Date fromDate;
	private Date toDate;
	private RecoData recoData;
	private String dateAccuracy;
	private Date observedOn;
	private String protocol;
	private String basisOfRecords;
	private Long obsvLanguageId;

//	-----Location Data--------
	private String observedAt;
	private String reverseGeocoded;
	private String locationScale;
	private Double latitude;
	private Double longitude;
	private Boolean useDegMinSec;
	private String degMinSec;
	private Boolean hidePreciseLocation;

//	-----Traits Data---------
	private Map<String, List<Long>> facts;
//	traitId:[valueId list] patter

//	-----Notes and tags-------
	private String notes;
	private List<Tags> tags;

//	-----User Group Data-----
	private List<Long> userGroupId;

//	-----Resource Data--------
	private List<ObservationResourceData> resources;

	/**
	 * 
	 */
	public ObservationCreate() {
		super();
	}

	/**
	 * @param sGroup
	 * @param helpIdentify
	 * @param createdOn
	 * @param fromDate
	 * @param toDate
	 * @param recoData
	 * @param dateAccuracy
	 * @param observedOn
	 * @param protocol
	 * @param basisOfRecords
	 * @param obsvLanguageId
	 * @param observedAt
	 * @param reverseGeocoded
	 * @param locationScale
	 * @param latitude
	 * @param longitude
	 * @param useDegMinSec
	 * @param degMinSec
	 * @param hidePreciseLocation
	 * @param facts
	 * @param notes
	 * @param tags
	 * @param userGroupId
	 * @param resources
	 */
	public ObservationCreate(Long sGroup, Boolean helpIdentify, Date createdOn, Date fromDate, Date toDate,
			RecoData recoData, String dateAccuracy, Date observedOn, String protocol, String basisOfRecords,
			Long obsvLanguageId, String observedAt, String reverseGeocoded, String locationScale, Double latitude,
			Double longitude, Boolean useDegMinSec, String degMinSec, Boolean hidePreciseLocation,
			Map<String, List<Long>> facts, String notes, List<Tags> tags, List<Long> userGroupId,
			List<com.strandls.observation.pojo.ObservationResourceData> resources) {
		super();
		this.sGroup = sGroup;
		this.helpIdentify = helpIdentify;
		this.createdOn = createdOn;
		this.fromDate = fromDate;
		this.toDate = toDate;
		this.recoData = recoData;
		this.dateAccuracy = dateAccuracy;
		this.observedOn = observedOn;
		this.protocol = protocol;
		this.basisOfRecords = basisOfRecords;
		this.obsvLanguageId = obsvLanguageId;
		this.observedAt = observedAt;
		this.reverseGeocoded = reverseGeocoded;
		this.locationScale = locationScale;
		this.latitude = latitude;
		this.longitude = longitude;
		this.useDegMinSec = useDegMinSec;
		this.degMinSec = degMinSec;
		this.hidePreciseLocation = hidePreciseLocation;
		this.facts = facts;
		this.notes = notes;
		this.tags = tags;
		this.userGroupId = userGroupId;
		this.resources = resources;
	}

//	-----GETTERS AND SETTERS----

	public Long getsGroup() {
		return sGroup;
	}

	public void setsGroup(Long sGroup) {
		this.sGroup = sGroup;
	}

	public Boolean getHelpIdentify() {
		return helpIdentify;
	}

	public void setHelpIdentify(Boolean helpIdentify) {
		this.helpIdentify = helpIdentify;
	}

	public Date getCreatedOn() {
		return createdOn;
	}

	public void setCreatedOn(Date createdOn) {
		this.createdOn = createdOn;
	}

	public Date getFromDate() {
		return fromDate;
	}

	public void setFromDate(Date fromDate) {
		this.fromDate = fromDate;
	}

	public Date getToDate() {
		return toDate;
	}

	public void setToDate(Date toDate) {
		this.toDate = toDate;
	}

	public RecoData getRecoData() {
		return recoData;
	}

	public void setRecoData(RecoData recoData) {
		this.recoData = recoData;
	}

	public String getDateAccuracy() {
		return dateAccuracy;
	}

	public void setDateAccuracy(String dateAccuracy) {
		this.dateAccuracy = dateAccuracy;
	}

	public Date getObservedOn() {
		return observedOn;
	}

	public void setObservedOn(Date observedOn) {
		this.observedOn = observedOn;
	}

	public String getProtocol() {
		return protocol;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	public String getBasisOfRecords() {
		return basisOfRecords;
	}

	public void setBasisOfRecords(String basisOfRecords) {
		this.basisOfRecords = basisOfRecords;
	}

	public Long getObsvLanguageId() {
		return obsvLanguageId;
	}

	public void setObsvLanguageId(Long obsvLanguageId) {
		this.obsvLanguageId = obsvLanguageId;
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

	public String getLocationScale() {
		return locationScale;
	}

	public void setLocationScale(String locationScale) {
		this.locationScale = locationScale;
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

	public Map<String, List<Long>> getFacts() {
		return facts;
	}

	public void setFacts(Map<String, List<Long>> facts) {
		this.facts = facts;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public List<Tags> getTags() {
		return tags;
	}

	public void setTags(List<Tags> tags) {
		this.tags = tags;
	}

	public List<Long> getUserGroupId() {
		return userGroupId;
	}

	public void setUserGroupId(List<Long> userGroupId) {
		this.userGroupId = userGroupId;
	}

	public List<ObservationResourceData> getResources() {
		return resources;
	}

	public void setResources(List<ObservationResourceData> resources) {
		this.resources = resources;
	}
}