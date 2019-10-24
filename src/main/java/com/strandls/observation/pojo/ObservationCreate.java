/**
 * 
 */
package com.strandls.observation.pojo;

import java.sql.Date;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.utility.pojo.TagsMapping;

/**
 * @author Abhishek Rudra
 *
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationCreate {

//	----Core Observation Data-------
	private Long sGroup;
	private Boolean helpIdentified;
	private Date createdOn;
	private Date fromDate;
	private Date toDate;
	private Long commonNameId;
	private String commonName;
	private Long scientificNameId;
	private String scientificName;
	private String recoComment;
	private String confidence;
	private String dateAccuracy;
	private Date observedOn;
	private Long languageId;
	private String protocol;
	private String basisOfRecords;

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
	private List<FactValuePair> facts;

//	-----Notes and tags-------
	private String notes;
	private TagsMapping tags;

//	-----User Group Data-----
	private List<Long> userGroupId;

//	-----Resource Data--------
	private Map<String, String> resources;

//	-----GETTERS AND SETTERS----

	public Long getsGroup() {
		return sGroup;
	}

	public void setsGroup(Long sGroup) {
		this.sGroup = sGroup;
	}

	public Boolean getHelpIdentified() {
		return helpIdentified;
	}

	public void setHelpIdentified(Boolean helpIdentified) {
		this.helpIdentified = helpIdentified;
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

	public Long getCommonNameId() {
		return commonNameId;
	}

	public void setCommonNameId(Long commonNameId) {
		this.commonNameId = commonNameId;
	}

	public String getCommonName() {
		return commonName;
	}

	public void setCommonName(String commonName) {
		this.commonName = commonName;
	}

	public Long getScientificNameId() {
		return scientificNameId;
	}

	public void setScientificNameId(Long scientificNameId) {
		this.scientificNameId = scientificNameId;
	}

	public String getScientificName() {
		return scientificName;
	}

	public void setScientificName(String scientificName) {
		this.scientificName = scientificName;
	}

	public String getRecoComment() {
		return recoComment;
	}

	public void setRecoComment(String recoComment) {
		this.recoComment = recoComment;
	}

	public String getConfidence() {
		return confidence;
	}

	public void setConfidence(String confidence) {
		this.confidence = confidence;
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

	public Long getLanguageId() {
		return languageId;
	}

	public void setLanguageId(Long languageId) {
		this.languageId = languageId;
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

	public List<FactValuePair> getFacts() {
		return facts;
	}

	public void setFacts(List<FactValuePair> facts) {
		this.facts = facts;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public TagsMapping getTags() {
		return tags;
	}

	public void setTags(TagsMapping tags) {
		this.tags = tags;
	}

	public List<Long> getUserGroupId() {
		return userGroupId;
	}

	public void setUserGroupId(List<Long> userGroupId) {
		this.userGroupId = userGroupId;
	}

	public Map<String, String> getResources() {
		return resources;
	}

	public void setResources(Map<String, String> resources) {
		this.resources = resources;
	}

}
