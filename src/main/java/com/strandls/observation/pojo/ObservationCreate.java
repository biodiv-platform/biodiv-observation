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
	private Map<String, List<Long>> factValuePairs;
//	traitId:[valueId list] pattern
	private Map<String, List<String>> factValueStringPairs;

//	-----Notes and tags-------
	private String notes;
	private List<Tags> tags;

//	-----User Group Data-----
	private List<Long> userGroupId;

//	-----Resource Data--------
	private List<ResourceDataObs> resources;
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

	public Map<String, List<Long>> getFactValuePairs() {
		return factValuePairs;
	}

	public void setFactValuePairs(Map<String, List<Long>> factValuePairs) {
		this.factValuePairs = factValuePairs;
	}

	public Map<String, List<String>> getFactValueStringPairs() {
		return factValueStringPairs;
	}

	public void setFactValueStringPairs(Map<String, List<String>> factValueStringPairs) {
		this.factValueStringPairs = factValueStringPairs;
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

	public List<ResourceDataObs> getResources() {
		return resources;
	}

	public void setResources(List<ResourceDataObs> resources) {
		this.resources = resources;
	}
}