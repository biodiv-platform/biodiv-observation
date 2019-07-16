package com.strandls.observation.pojo;
// default package
// Generated 5 Jul, 2019 1:18:15 PM by Hibernate Tools 5.2.12.Final

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * ObservationId generated by hbm2java
 */
@Entity
@Table(name = "observation", schema = "public")
@JsonIgnoreProperties(ignoreUnknown = true)
public class Observation implements java.io.Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8040652447273785394L;
	private long id;
	private long version;
	private long authorId;
	private Date createdOn;
	private long groupId;
	private double latitude;
	private double longitude;
	private String notes;
	private Date fromDate;
	private String placeName;
	private int rating;
	private String reverseGeocodedName;
	private Integer flagCount;
	private Boolean geoPrivacy;
	private Long habitatId;
	private Boolean isDeleted;
	private Date lastRevised;
	private String locationAccuracy;
	private Long visitCount;
	private String searchText;
	private Long maxVotedRecoId;
	private Boolean agreeTerms;
	private boolean isChecklist;
	private boolean isShowable;
	private Long sourceId;
	private Date toDate;
    private String topology;
	private String checklistAnnotations;
	private Integer featureCount;
	private Boolean isLocked;
	private long licenseId;
	private long languageId;
	private String locationScale;
	private String accessRights;
	private String catalogNumber;
	private Long datasetId;
	private String externalDatasetKey;
	private String externalId;
	private String externalUrl;
	private String informationWithheld;
	private Date lastCrawled;
	private Date lastInterpreted;
	private String originalAuthor;
	private String publishingCountry;
	private Long reprImageId;
	private String viaCode;
	private String viaId;
	private String protocol;
	private String basisOfRecord;
	private int noOfImages;
	private int noOfVideos;
	private int noOfAudio;
	private int noOfIdentifications;
	private Long dataTableId;
	private String dateAccuracy;
	private Serializable traitsJson;

	
	@Id
	@Column(name = "id", nullable = false)
	public long getId() {
		return this.id;
	}

	public void setId(long id) {
		this.id = id;
	}

	@Column(name = "version", nullable = false)
	public long getVersion() {
		return this.version;
	}

	public void setVersion(long version) {
		this.version = version;
	}

	@Column(name = "author_id", nullable = false)
	public long getAuthorId() {
		return this.authorId;
	}

	public void setAuthorId(long authorId) {
		this.authorId = authorId;
	}

	@Column(name = "created_on", nullable = false, length = 29)
	public Date getCreatedOn() {
		return this.createdOn;
	}

	public void setCreatedOn(Date createdOn) {
		this.createdOn = createdOn;
	}

	@Column(name = "group_id", nullable = false)
	public long getGroupId() {
		return this.groupId;
	}

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	@Column(name = "latitude", nullable = false, precision = 17, scale = 17)
	public double getLatitude() {
		return this.latitude;
	}

	public void setLatitude(double latitude) {
		this.latitude = latitude;
	}

	@Column(name = "longitude", nullable = false, precision = 17, scale = 17)
	public double getLongitude() {
		return this.longitude;
	}

	public void setLongitude(double longitude) {
		this.longitude = longitude;
	}

	@Column(name = "notes")
	public String getNotes() {
		return this.notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	@Column(name = "from_date", length = 29)
	public Date getFromDate() {
		return this.fromDate;
	}

	public void setFromDate(Date fromDate) {
		this.fromDate = fromDate;
	}

	@Column(name = "place_name")
	public String getPlaceName() {
		return this.placeName;
	}

	public void setPlaceName(String placeName) {
		this.placeName = placeName;
	}

	@Column(name = "rating", nullable = false)
	public int getRating() {
		return this.rating;
	}

	public void setRating(int rating) {
		this.rating = rating;
	}

	@Column(name = "reverse_geocoded_name")
	public String getReverseGeocodedName() {
		return this.reverseGeocodedName;
	}

	public void setReverseGeocodedName(String reverseGeocodedName) {
		this.reverseGeocodedName = reverseGeocodedName;
	}

	@Column(name = "flag_count")
	public Integer getFlagCount() {
		return this.flagCount;
	}

	public void setFlagCount(Integer flagCount) {
		this.flagCount = flagCount;
	}

	@Column(name = "geo_privacy")
	public Boolean getGeoPrivacy() {
		return this.geoPrivacy;
	}

	public void setGeoPrivacy(Boolean geoPrivacy) {
		this.geoPrivacy = geoPrivacy;
	}

	@Column(name = "habitat_id")
	public Long getHabitatId() {
		return this.habitatId;
	}

	public void setHabitatId(Long habitatId) {
		this.habitatId = habitatId;
	}

	@Column(name = "is_deleted")
	public Boolean getIsDeleted() {
		return this.isDeleted;
	}

	public void setIsDeleted(Boolean isDeleted) {
		this.isDeleted = isDeleted;
	}

	@Column(name = "last_revised", length = 29)
	public Date getLastRevised() {
		return this.lastRevised;
	}

	public void setLastRevised(Date lastRevised) {
		this.lastRevised = lastRevised;
	}

	@Column(name = "location_accuracy")
	public String getLocationAccuracy() {
		return this.locationAccuracy;
	}

	public void setLocationAccuracy(String locationAccuracy) {
		this.locationAccuracy = locationAccuracy;
	}

	@Column(name = "visit_count")
	public Long getVisitCount() {
		return this.visitCount;
	}

	public void setVisitCount(Long visitCount) {
		this.visitCount = visitCount;
	}

	@Column(name = "search_text")
	public String getSearchText() {
		return this.searchText;
	}

	public void setSearchText(String searchText) {
		this.searchText = searchText;
	}

	@Column(name = "max_voted_reco_id")
	public Long getMaxVotedRecoId() {
		return this.maxVotedRecoId;
	}

	public void setMaxVotedRecoId(Long maxVotedRecoId) {
		this.maxVotedRecoId = maxVotedRecoId;
	}

	@Column(name = "agree_terms")
	public Boolean getAgreeTerms() {
		return this.agreeTerms;
	}

	public void setAgreeTerms(Boolean agreeTerms) {
		this.agreeTerms = agreeTerms;
	}

	@Column(name = "is_checklist", nullable = false)
	public boolean isIsChecklist() {
		return this.isChecklist;
	}

	public void setIsChecklist(boolean isChecklist) {
		this.isChecklist = isChecklist;
	}

	@Column(name = "is_showable", nullable = false)
	public boolean isIsShowable() {
		return this.isShowable;
	}

	public void setIsShowable(boolean isShowable) {
		this.isShowable = isShowable;
	}

	@Column(name = "source_id")
	public Long getSourceId() {
		return this.sourceId;
	}

	public void setSourceId(Long sourceId) {
		this.sourceId = sourceId;
	}

	@Column(name = "to_date", length = 29)
	public Date getToDate() {
		return this.toDate;
	}

	public void setToDate(Date toDate) {
		this.toDate = toDate;
	}

	//@Type(type = "org.hibernate.spatial.GeometryType")
	@Column(name = "topology", nullable = false)
	public String getTopology() {
		return this.topology;
	}

	public void setTopology(String topology) {
		this.topology = topology;
	}

	@Column(name = "checklist_annotations")
	public String getChecklistAnnotations() {
		return this.checklistAnnotations;
	}

	public void setChecklistAnnotations(String checklistAnnotations) {
		this.checklistAnnotations = checklistAnnotations;
	}

	@Column(name = "feature_count")
	public Integer getFeatureCount() {
		return this.featureCount;
	}

	public void setFeatureCount(Integer featureCount) {
		this.featureCount = featureCount;
	}

	@Column(name = "is_locked")
	public Boolean getIsLocked() {
		return this.isLocked;
	}

	public void setIsLocked(Boolean isLocked) {
		this.isLocked = isLocked;
	}

	@Column(name = "license_id", nullable = false)
	public long getLicenseId() {
		return this.licenseId;
	}

	public void setLicenseId(long licenseId) {
		this.licenseId = licenseId;
	}

	@Column(name = "language_id", nullable = false)
	public long getLanguageId() {
		return this.languageId;
	}

	public void setLanguageId(long languageId) {
		this.languageId = languageId;
	}

	@Column(name = "location_scale", nullable = false)
	public String getLocationScale() {
		return this.locationScale;
	}

	public void setLocationScale(String locationScale) {
		this.locationScale = locationScale;
	}

	@Column(name = "access_rights")
	public String getAccessRights() {
		return this.accessRights;
	}

	public void setAccessRights(String accessRights) {
		this.accessRights = accessRights;
	}

	@Column(name = "catalog_number")
	public String getCatalogNumber() {
		return this.catalogNumber;
	}

	public void setCatalogNumber(String catalogNumber) {
		this.catalogNumber = catalogNumber;
	}

	@Column(name = "dataset_id")
	public Long getDatasetId() {
		return this.datasetId;
	}

	public void setDatasetId(Long datasetId) {
		this.datasetId = datasetId;
	}

	@Column(name = "external_dataset_key")
	public String getExternalDatasetKey() {
		return this.externalDatasetKey;
	}

	public void setExternalDatasetKey(String externalDatasetKey) {
		this.externalDatasetKey = externalDatasetKey;
	}

	@Column(name = "external_id")
	public String getExternalId() {
		return this.externalId;
	}

	public void setExternalId(String externalId) {
		this.externalId = externalId;
	}

	@Column(name = "external_url")
	public String getExternalUrl() {
		return this.externalUrl;
	}

	public void setExternalUrl(String externalUrl) {
		this.externalUrl = externalUrl;
	}

	@Column(name = "information_withheld")
	public String getInformationWithheld() {
		return this.informationWithheld;
	}

	public void setInformationWithheld(String informationWithheld) {
		this.informationWithheld = informationWithheld;
	}

	@Column(name = "last_crawled", length = 29)
	public Date getLastCrawled() {
		return this.lastCrawled;
	}

	public void setLastCrawled(Date lastCrawled) {
		this.lastCrawled = lastCrawled;
	}

	@Column(name = "last_interpreted", length = 29)
	public Date getLastInterpreted() {
		return this.lastInterpreted;
	}

	public void setLastInterpreted(Date lastInterpreted) {
		this.lastInterpreted = lastInterpreted;
	}

	@Column(name = "original_author")
	public String getOriginalAuthor() {
		return this.originalAuthor;
	}

	public void setOriginalAuthor(String originalAuthor) {
		this.originalAuthor = originalAuthor;
	}

	@Column(name = "publishing_country")
	public String getPublishingCountry() {
		return this.publishingCountry;
	}

	public void setPublishingCountry(String publishingCountry) {
		this.publishingCountry = publishingCountry;
	}

	@Column(name = "repr_image_id")
	public Long getReprImageId() {
		return this.reprImageId;
	}

	public void setReprImageId(Long reprImageId) {
		this.reprImageId = reprImageId;
	}

	@Column(name = "via_code")
	public String getViaCode() {
		return this.viaCode;
	}

	public void setViaCode(String viaCode) {
		this.viaCode = viaCode;
	}

	@Column(name = "via_id")
	public String getViaId() {
		return this.viaId;
	}

	public void setViaId(String viaId) {
		this.viaId = viaId;
	}

	@Column(name = "protocol", nullable = false)
	public String getProtocol() {
		return this.protocol;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	@Column(name = "basis_of_record", nullable = false)
	public String getBasisOfRecord() {
		return this.basisOfRecord;
	}

	public void setBasisOfRecord(String basisOfRecord) {
		this.basisOfRecord = basisOfRecord;
	}

	@Column(name = "no_of_images", nullable = false)
	public int getNoOfImages() {
		return this.noOfImages;
	}

	public void setNoOfImages(int noOfImages) {
		this.noOfImages = noOfImages;
	}

	@Column(name = "no_of_videos", nullable = false)
	public int getNoOfVideos() {
		return this.noOfVideos;
	}

	public void setNoOfVideos(int noOfVideos) {
		this.noOfVideos = noOfVideos;
	}

	@Column(name = "no_of_audio", nullable = false)
	public int getNoOfAudio() {
		return this.noOfAudio;
	}

	public void setNoOfAudio(int noOfAudio) {
		this.noOfAudio = noOfAudio;
	}

	@Column(name = "no_of_identifications", nullable = false)
	public int getNoOfIdentifications() {
		return this.noOfIdentifications;
	}

	public void setNoOfIdentifications(int noOfIdentifications) {
		this.noOfIdentifications = noOfIdentifications;
	}

	@Column(name = "data_table_id")
	public Long getDataTableId() {
		return this.dataTableId;
	}

	public void setDataTableId(Long dataTableId) {
		this.dataTableId = dataTableId;
	}

	@Column(name = "date_accuracy", length = 100)
	public String getDateAccuracy() {
		return this.dateAccuracy;
	}

	public void setDateAccuracy(String dateAccuracy) {
		this.dateAccuracy = dateAccuracy;
	}

	@Column(name = "traits_json")
	public Serializable getTraitsJson() {
		return this.traitsJson;
	}

	public void setTraitsJson(Serializable traitsJson) {
		this.traitsJson = traitsJson;
	}



}
