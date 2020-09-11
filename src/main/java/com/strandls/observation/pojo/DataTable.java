package com.strandls.observation.pojo;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import com.vividsolutions.jts.geom.Geometry;

@Entity
@Table(name = "data_table")
public class DataTable implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -3167028587223452212L;
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "id", nullable = false)
	private Long id;
	
	@Column(name = "version", nullable = false)
	private Long version;
	
	@Column(name = "access_license_id", nullable = false)
	private Long accessLicenseId;
	
	@Column(name = "access_rights")
	private String accessRights;
	
	@Column(name = "agree_terms", nullable = false)
	private Boolean agreeTerms;
	
	@Column(name = "checklist_id")
	private Long checklistId;
	
	@Column(name = "columns", nullable = false)
	private String columns;
	
	@Column(name = "created_on", nullable = false)
	private Date createdOn;
	
	@Column(name = "custom_fields")
	private String customFields;
	
	@Column(name = "data_table_type", nullable = false, length = 255)
	private String dataTableType;
	
	@Column(name = "dataset_id")
	private Long datasetid;
	
	@Column(name = "description")
	private String description;
	
	@Column(name = "external_id", length = 255)
	private String externalId;
	
	@Column(name = "external_url", length = 255)
	private String externalUrl;
	
	@Column(name = "feature_count", nullable = false)
	private Integer featureCount;
	
	@Column(name = "flag_count", nullable = false)
	private Integer flagCount;
	
	@Column(name = "geographical_coverage_geo_privacy", nullable = false)
	private Boolean geographicalCoverageGeoPrivacy;

	@Column(name = "geographical_coverage_latitude", nullable = false)
	private Double geographicalCoverageLatitude;
	
	@Column(name = "geographical_coverage_location_accuracy", nullable = false, length = 255)
	private String geographicalCoverageLocationAccuracy;
	
	@Column(name = "geographical_coverage_location_scale", nullable = false, length = 255)
	private String geographicalCoverageLocationScale;
	
	@Column(name = "geographical_coverage_longitude", nullable = false)
	private Double geographicalCoverageLongitude;
	
	@Column(name = "geographical_coverage_place_name")
	private String geographicalCoveragePlaceName;
	
	@Column(name = "geographical_coverage_topology")
	private Geometry geographicalCoverageTopology;
	
	@Column(name = "is_deleted", nullable = false)
	private Boolean deleted;
	
	@Column(name = "language_id", nullable = false)
	private Long languageId;

	@Column(name = "last_revised", nullable = false)
	private Date lastRevised;
	
	@Column(name = "methods")
	private String methods;
	
	@Column(name = "party_attributions")
	private String partyAttributions;
	
	@Column(name = "party_contributor_id", nullable = false)
	private Long partyContributorId;
	
	@Column(name = "party_uploader_id", nullable = false)
	private Long partyUploaderId;
	
	@Column(name = "project")
	private String project;
	
	@Column(name = "rating", nullable = false)
	private Integer rating;
	
	@Column(name = "taxonomic_coverage_group_ids", nullable = false, length = 255)
	private String taxonomicCoverageGroupIds;
	
	@Column(name = "temporal_coverage_from_date", nullable = false)
	private Date temporalCoverageFromDate;
	
	@Column(name = "temporal_coverage_to_date")
	private Date temporalCoverageToDate;
	
	@Column(name = "title", nullable = false)
	private String title;
	
	@Column(name = "u_file_id", nullable = false)
	private Long uFileId;
	
	@Column(name = "uploader_id", nullable = false)
	private Long uploaderId;
	
	@Column(name = "via_code", length = 255)
	private String viaCode;
	
	@Column(name = "via_id", length = 255)
	private String viaId;
	
	@Column(name = "temporal_coverage_date_accuracy", length = 100)
	private String temporalCoverageDateAccuracy;
	
	@Column(name = "upload_log_id")
	private Long uploadLogId;
	
	@Column(name = "trait_value_file_id")
	private Long traitValueFileId;
	
	@Column(name = "summary", nullable = false, length =  10000)
	private String summary;

	@Column(name = "images_file_id")
	private Long imagesFileId;
	
	@Column(name = "basis_of_data", nullable = false, columnDefinition = "varchar(255) default 'PRIMARY_OBSERVATION'")
	private String basisOfData;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Long getVersion() {
		return version;
	}

	public void setVersion(Long version) {
		this.version = version;
	}

	public Long getAccessLicenseId() {
		return accessLicenseId;
	}

	public void setAccessLicenseId(Long accessLicenseId) {
		this.accessLicenseId = accessLicenseId;
	}

	public String getAccessRights() {
		return accessRights;
	}

	public void setAccessRights(String accessRights) {
		this.accessRights = accessRights;
	}

	public Boolean getAgreeTerms() {
		return agreeTerms;
	}

	public void setAgreeTerms(Boolean agreeTerms) {
		this.agreeTerms = agreeTerms;
	}

	public Long getChecklistId() {
		return checklistId;
	}

	public void setChecklistId(Long checklistId) {
		this.checklistId = checklistId;
	}

	public String getColumns() {
		return columns;
	}

	public void setColumns(String columns) {
		this.columns = columns;
	}

	public Date getCreatedOn() {
		return createdOn;
	}

	public void setCreatedOn(Date createdOn) {
		this.createdOn = createdOn;
	}

	public String getCustomFields() {
		return customFields;
	}

	public void setCustomFields(String customFields) {
		this.customFields = customFields;
	}

	public String getDataTableType() {
		return dataTableType;
	}

	public void setDataTableType(String dataTableType) {
		this.dataTableType = dataTableType;
	}

	public Long getDatasetid() {
		return datasetid;
	}

	public void setDatasetid(Long datasetid) {
		this.datasetid = datasetid;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getExternalId() {
		return externalId;
	}

	public void setExternalId(String externalId) {
		this.externalId = externalId;
	}

	public String getExternalUrl() {
		return externalUrl;
	}

	public void setExternalUrl(String externalUrl) {
		this.externalUrl = externalUrl;
	}

	public Integer getFeatureCount() {
		return featureCount;
	}

	public void setFeatureCount(Integer featureCount) {
		this.featureCount = featureCount;
	}

	public Integer getFlagCount() {
		return flagCount;
	}

	public void setFlagCount(Integer flagCount) {
		this.flagCount = flagCount;
	}

	public Boolean getGeographicalCoverageGeoPrivacy() {
		return geographicalCoverageGeoPrivacy;
	}

	public void setGeographicalCoverageGeoPrivacy(Boolean geographicalCoverageGeoPrivacy) {
		this.geographicalCoverageGeoPrivacy = geographicalCoverageGeoPrivacy;
	}

	public Double getGeographicalCoverageLatitude() {
		return geographicalCoverageLatitude;
	}

	public void setGeographicalCoverageLatitude(Double geographicalCoverageLatitude) {
		this.geographicalCoverageLatitude = geographicalCoverageLatitude;
	}

	public String getGeographicalCoverageLocationAccuracy() {
		return geographicalCoverageLocationAccuracy;
	}

	public void setGeographicalCoverageLocationAccuracy(String geographicalCoverageLocationAccuracy) {
		this.geographicalCoverageLocationAccuracy = geographicalCoverageLocationAccuracy;
	}

	public String getGeographicalCoverageLocationScale() {
		return geographicalCoverageLocationScale;
	}

	public void setGeographicalCoverageLocationScale(String geographicalCoverageLocationScale) {
		this.geographicalCoverageLocationScale = geographicalCoverageLocationScale;
	}

	public Double getGeographicalCoverageLongitude() {
		return geographicalCoverageLongitude;
	}

	public void setGeographicalCoverageLongitude(Double geographicalCoverageLongitude) {
		this.geographicalCoverageLongitude = geographicalCoverageLongitude;
	}

	public String getGeographicalCoveragePlaceName() {
		return geographicalCoveragePlaceName;
	}

	public void setGeographicalCoveragePlaceName(String geographicalCoveragePlaceName) {
		this.geographicalCoveragePlaceName = geographicalCoveragePlaceName;
	}

	public Geometry getGeographicalCoverageTopology() {
		return geographicalCoverageTopology;
	}

	public void setGeographicalCoverageTopology(Geometry geographicalCoverageTopology) {
		this.geographicalCoverageTopology = geographicalCoverageTopology;
	}

	public Boolean getDeleted() {
		return deleted;
	}

	public void setDeleted(Boolean deleted) {
		this.deleted = deleted;
	}

	public Long getLanguageId() {
		return languageId;
	}

	public void setLanguageId(Long languageId) {
		this.languageId = languageId;
	}

	public Date getLastRevised() {
		return lastRevised;
	}

	public void setLastRevised(Date lastRevised) {
		this.lastRevised = lastRevised;
	}

	public String getMethods() {
		return methods;
	}

	public void setMethods(String methods) {
		this.methods = methods;
	}

	public String getPartyAttributions() {
		return partyAttributions;
	}

	public void setPartyAttributions(String partyAttributions) {
		this.partyAttributions = partyAttributions;
	}

	public Long getPartyContributorId() {
		return partyContributorId;
	}

	public void setPartyContributorId(Long partyContributorId) {
		this.partyContributorId = partyContributorId;
	}

	public Long getPartyUploaderId() {
		return partyUploaderId;
	}

	public void setPartyUploaderId(Long partyUploaderId) {
		this.partyUploaderId = partyUploaderId;
	}

	public String getProject() {
		return project;
	}

	public void setProject(String project) {
		this.project = project;
	}

	public Integer getRating() {
		return rating;
	}

	public void setRating(Integer rating) {
		this.rating = rating;
	}

	public String getTaxonomicCoverageGroupIds() {
		return taxonomicCoverageGroupIds;
	}

	public void setTaxonomicCoverageGroupIds(String taxonomicCoverageGroupIds) {
		this.taxonomicCoverageGroupIds = taxonomicCoverageGroupIds;
	}

	public Date getTemporalCoverageFromDate() {
		return temporalCoverageFromDate;
	}

	public void setTemporalCoverageFromDate(Date temporalCoverageFromDate) {
		this.temporalCoverageFromDate = temporalCoverageFromDate;
	}

	public Date getTemporalCoverageToDate() {
		return temporalCoverageToDate;
	}

	public void setTemporalCoverageToDate(Date temporalCoverageToDate) {
		this.temporalCoverageToDate = temporalCoverageToDate;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Long getuFileId() {
		return uFileId;
	}

	public void setuFileId(Long uFileId) {
		this.uFileId = uFileId;
	}

	public Long getUploaderId() {
		return uploaderId;
	}

	public void setUploaderId(Long uploaderId) {
		this.uploaderId = uploaderId;
	}

	public String getViaCode() {
		return viaCode;
	}

	public void setViaCode(String viaCode) {
		this.viaCode = viaCode;
	}

	public String getViaId() {
		return viaId;
	}

	public void setViaId(String viaId) {
		this.viaId = viaId;
	}

	public String getTemporalCoverageDateAccuracy() {
		return temporalCoverageDateAccuracy;
	}

	public void setTemporalCoverageDateAccuracy(String temporalCoverageDateAccuracy) {
		this.temporalCoverageDateAccuracy = temporalCoverageDateAccuracy;
	}

	public Long getUploadLogId() {
		return uploadLogId;
	}

	public void setUploadLogId(Long uploadLogId) {
		this.uploadLogId = uploadLogId;
	}

	public Long getTraitValueFileId() {
		return traitValueFileId;
	}

	public void setTraitValueFileId(Long traitValueFileId) {
		this.traitValueFileId = traitValueFileId;
	}

	public String getSummary() {
		return summary;
	}

	public void setSummary(String summary) {
		this.summary = summary;
	}

	public Long getImagesFileId() {
		return imagesFileId;
	}

	public void setImagesFileId(Long imagesFileId) {
		this.imagesFileId = imagesFileId;
	}

	public String getBasisOfData() {
		return basisOfData;
	}

	public void setBasisOfData(String basisOfData) {
		this.basisOfData = basisOfData;
	}

	@Override
	public String toString() {
		return "DataTable [id=" + id + ", version=" + version + ", accessLicenseId=" + accessLicenseId
				+ ", accessRights=" + accessRights + ", agreeTerms=" + agreeTerms + ", checklistId=" + checklistId
				+ ", columns=" + columns + ", createdOn=" + createdOn + ", customFields=" + customFields
				+ ", dataTableType=" + dataTableType + ", datasetid=" + datasetid + ", description=" + description
				+ ", externalId=" + externalId + ", externalUrl=" + externalUrl + ", featureCount=" + featureCount
				+ ", flagCount=" + flagCount + ", geographicalCoverageGeoPrivacy=" + geographicalCoverageGeoPrivacy
				+ ", geographicalCoverageLatitude=" + geographicalCoverageLatitude
				+ ", geographicalCoverageLocationAccuracy=" + geographicalCoverageLocationAccuracy
				+ ", geographicalCoverageLocationScale=" + geographicalCoverageLocationScale
				+ ", geographicalCoverageLongitude=" + geographicalCoverageLongitude
				+ ", geographicalCoveragePlaceName=" + geographicalCoveragePlaceName + ", geographicalCoverageTopology="
				+ geographicalCoverageTopology + ", deleted=" + deleted + ", languageId=" + languageId
				+ ", lastRevised=" + lastRevised + ", methods=" + methods + ", partyAttributions=" + partyAttributions
				+ ", partyContributorId=" + partyContributorId + ", partyUploaderId=" + partyUploaderId + ", project="
				+ project + ", rating=" + rating + ", taxonomicCoverageGroupIds=" + taxonomicCoverageGroupIds
				+ ", temporalCoverageFromDate=" + temporalCoverageFromDate + ", temporalCoverageToDate="
				+ temporalCoverageToDate + ", title=" + title + ", uFileId=" + uFileId + ", uploaderId=" + uploaderId
				+ ", viaCode=" + viaCode + ", viaId=" + viaId + ", temporalCoverageDateAccuracy="
				+ temporalCoverageDateAccuracy + ", uploadLogId=" + uploadLogId + ", traitValueFileId="
				+ traitValueFileId + ", summary=" + summary + ", imagesFileId=" + imagesFileId + ", basisOfData=" + basisOfData + "]";
	}
}
