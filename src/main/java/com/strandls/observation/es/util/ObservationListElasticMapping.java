/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.strandls.user.pojo.UserIbp;

/**
 * @author ashish
 *
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListElasticMapping {
	private Long observationId;
	private String placeName;
	private String noOfIdentification;
	private String createdOn; // pg
	private String lastRevised;
	private String reverseGeocodedName;
	private Long speciesGroupId;
	private String speciesGroup;
	private Long noOfImages;
	private Long noOfAudios;
	private Long noOfVideos;
	private String reprImageUrl;
	private Boolean isLocked; //pg
	private String locationScale;
	private Double latitude; //pg
	private Double longitude; //pg
	private String dateAccuracy;
	private String fromDate;
	private String toDate;
	private String observedInMonth;
	private Boolean geoPrivacy;
	private String datasetTitle;
	private LocationInformation locationInformation;
	private Integer recoVoteCount;
	private UserIbp user;
	private List<Facts> facts;
	private List<Flags> flags;
	
	private Max_voted_reco maxVotedReco;
	private List<All_reco_vote>allRecoVotes; //pg
	//private RecoShow recoShow;
	
	private List<User_group_observations> userGroup;
	
	private List<Custom_fields> customFields;
	
	private List<com.strandls.observation.es.util.Tags> tags;
	private Boolean containsMedia;
	private String uploadProtocol;
	private Integer flagCount;
	private String organismRemarks;
	private String annotations;

	@JsonProperty("observation_id")
	private void unpackObservationId(Long observation_id) {
		observationId = observation_id;
	}
	
	@JsonProperty("place_name")
	private void unpackPlaceName(String name) {
		placeName = name;
	}
	@JsonProperty("no_of_identifications")
	private void unpackNumberOfIndetification(String number) {
		noOfIdentification = number;
	}


	@JsonProperty("created_on")
	private void unpackDate(String created_on) {
		createdOn = created_on;
	}

	@JsonProperty(value = "last_revised")
	private void unpackLastRevised(String lastModified) {
		lastRevised = lastModified;
	}
	
	@JsonProperty(value = "reverse_geocoded_name")
	private void unpackAddress(String reverse_geocoded_name) {
		reverseGeocodedName = reverse_geocoded_name;
	}

	@JsonProperty(value = "group_id")
	private void unpackSGroupId(Long group_id) {
		speciesGroupId = group_id;
	}

	@JsonProperty(value = "group_name")
	private void unpacksGroup(String group_name) {
		speciesGroup = group_name;
	}

	@JsonProperty(value = "no_of_images")
	private void unpackImages(Long images) {
		noOfImages = images;
	}

	@JsonProperty(value = "no_of_audio")
	private void unpackAudio(Long audio) {
		noOfAudios = audio;
	}

	@JsonProperty(value = "no_of_videos")
	private void unpackVideo(Long videos) {
		noOfVideos = videos;
	}

	@JsonProperty(value = "repr_image_url")
	private void unpackReprImage(String reprImage) {
		reprImageUrl = reprImage;
	}
	
	@JsonProperty(value = "is_locked")
	private void unpackisLocked(Boolean value) {
		isLocked = value;
	}
	
	@JsonProperty(value = "max_voted_reco")
	private void unpackMaxVotedReco(Max_voted_reco reco) {
		maxVotedReco = reco;
	}
	
	@JsonProperty(value = "all_reco_vote")
	private void unpackAllRecoVotes(List<All_reco_vote> recos) {
		allRecoVotes = recos;
	}
	
	@JsonProperty(value = "location_scale")
	private void unpackLocationScale(String scale) {
		locationScale = scale;
	}
	
	@JsonProperty(value = "location")
	private void unpackLocation(Location location) {
		latitude = location.getLat();
		longitude = location.getLon();
	}
	@JsonProperty(value = "date_accuracy")
	private void unpackDateAccuracy(String accuracy) {
		dateAccuracy = accuracy;
	}
	@JsonProperty(value = "from_date")
	private void unpackFromDate(String date) {
		fromDate = date;
	}
	@JsonProperty(value = "to_date")
	private void unpackToDate(String date) {
		toDate = date;
	}
	
	@JsonProperty(value = "observed_in_month")
	private void unpackObservedMonth(String month) {
		observedInMonth = month;
	}
	@JsonProperty(value = "geo_privacy")
	private void unpackGeoPrivacy(Boolean value) {
		geoPrivacy = value;
	}
	
	@JsonProperty(value = "no_media")
	private void unpackNoMedia(Integer value) {
		if(value == 1) { 
			containsMedia = false;}
		else if(value == 0) {
			containsMedia = true;}
	}
	
	@JsonProperty(value = "dataset_title")
	private void unpackDatasetTitle(String title) {
		datasetTitle = title;
	}
	
	@JsonProperty(value = "protocol")
	private void unpackProtocol(String protocol) {
		uploadProtocol = protocol;
	}
	
	@JsonProperty(value = "flag_count")
	private void unpackFlagCount(Integer count) {
		flagCount = count;
	}
	
	@JsonProperty(value = "notes")
	private void unpackNotes(String notes) {
		organismRemarks = notes;
	}
	
	@JsonProperty(value = "checklist_annotations")
	private void unpackChecklist(String notes) {
		annotations = notes;
	}
	
	@JsonProperty(value = "location_information")
	private void unpackLocationInformation(LocationInformation location) {
		locationInformation = location;
	}
	
	@JsonProperty(value = "reco_vote_count")
	private void unpackRecoVoteCount(Integer count) {
		recoVoteCount = count;
	}


//	---------USER IBP------------

	@JsonProperty(value = "author_id")
	private void unpackAuthorId(Long author_id) {
		if (user == null)
			user = new UserIbp();
		user.setId(author_id);
	}

	@JsonProperty(value = "created_by")
	private void unpackAuthorName(String created_by) {
		if (user == null)
			user = new UserIbp();
		user.setName(created_by);
	}

	@JsonProperty(value = "profile_pic")
	private void unpackAuthorPic(String profile_pic) {
		if (user == null)
			user = new UserIbp();
		user.setProfilePic(profile_pic);
	}

//	---------FACT VALUE PAIR-----------

	@JsonProperty(value = "facts")
	private void unpackFacts(List<Facts> allFacts) {
		facts = allFacts;
//		factValuePair = new ArrayList<FactValuePair>();
//		if (facts != null) {
//			for (Facts fact : facts) {
//				FactValuePair fvp = new FactValuePair();
//				fvp.setNameId(fact.getTrait_id());
//				fvp.setName(fact.getName());
//				fvp.setType(fact.getTrait_types());
//				fvp.setIsParticipatry(fact.getIs_participatory());
//				if (fact.getTrait_value() != null) {
//					for (Trait_value value : fact.getTrait_value()) {
//						fvp.setValue(value.getValue());
//						fvp.setValueId(value.getTrait_value_id());
//						factValuePair.add(fvp);
//					}
//				}
//
//			}
		}

//	------------FLAG SHOW--------------

	@JsonProperty(value = "flags")
	private void unpackFlags(List<Flags> allFlag) {
		flags = allFlag;
	}
@JsonProperty(value = "user_group_observations")
private void unpackUserGroup(List<User_group_observations> ugObservation) {
		userGroup = ugObservation;
	}

//	--------------------CUSTOM FIELDS--------------
	@JsonProperty(value = "custom_fields")
	private void unpackCustomField(List<Custom_fields> custom_fields) {
		customFields = custom_fields;
	}

//	---------------TAGS-------------------------

	@JsonProperty(value = "tags")
	private void unpacktags(List<com.strandls.observation.es.util.Tags> tagsES) {
		tags = tagsES;
	}

	/**
	 * 
	 */
	public ObservationListElasticMapping() {
		super();
	}

	public ObservationListElasticMapping(Long observationId, String placeName, String noOfIdentification,
			String createdOn, String lastRevised, String reverseGeocodedName, Long speciesGroupId, String speciesGroup,
			Long noOfImages, Long noOfAudios, Long noOfVideos, String reprImageUrl, Boolean isLocked,
			String locationScale, Double latitude, Double longitude, String dateAccuracy, String fromDate,
			String toDate, String observedInMonth, Boolean geoPrivacy, String datasetTitle,
			LocationInformation locationInformation, Integer recoVoteCount, UserIbp user, List<Facts> facts, List<Flags> flags,
			Max_voted_reco maxVotedReco, List<All_reco_vote> allRecoVotes, List<User_group_observations> userGroup,
			List<Custom_fields> customFields, List<com.strandls.observation.es.util.Tags> tags, Boolean containsMedia,
			String uploadProtocol, Integer flagCount, String organismRemarks, String annotations) {
		super();
		this.observationId = observationId;
		this.placeName = placeName;
		this.noOfIdentification = noOfIdentification;
		this.createdOn = createdOn;
		this.lastRevised = lastRevised;
		this.reverseGeocodedName = reverseGeocodedName;
		this.speciesGroupId = speciesGroupId;
		this.speciesGroup = speciesGroup;
		this.noOfImages = noOfImages;
		this.noOfAudios = noOfAudios;
		this.noOfVideos = noOfVideos;
		this.reprImageUrl = reprImageUrl;
		this.isLocked = isLocked;
		this.locationScale = locationScale;
		this.latitude = latitude;
		this.longitude = longitude;
		this.dateAccuracy = dateAccuracy;
		this.fromDate = fromDate;
		this.toDate = toDate;
		this.observedInMonth = observedInMonth;
		this.geoPrivacy = geoPrivacy;
		this.datasetTitle = datasetTitle;
		this.locationInformation = locationInformation;
		this.recoVoteCount = recoVoteCount;
		this.user = user;
		this.facts = facts;
		this.flags = flags;
		this.maxVotedReco = maxVotedReco;
		this.allRecoVotes = allRecoVotes;
		this.userGroup = userGroup;
		this.customFields = customFields;
		this.tags = tags;
		this.containsMedia = containsMedia;
		this.uploadProtocol = uploadProtocol;
		this.flagCount = flagCount;
		this.organismRemarks = organismRemarks;
		this.annotations = annotations;
	}

	public Long getObservationId() {
		return observationId;
	}

	public String getPlaceName() {
		return placeName;
	}

	public String getNoOfIdentification() {
		return noOfIdentification;
	}

	public String getCreatedOn() {
		return createdOn;
	}

	public String getLastRevised() {
		return lastRevised;
	}

	public String getReverseGeocodedName() {
		return reverseGeocodedName;
	}

	public Long getSpeciesGroupId() {
		return speciesGroupId;
	}

	public String getSpeciesGroup() {
		return speciesGroup;
	}

	public Long getNoOfImages() {
		return noOfImages;
	}

	public Long getNoOfAudios() {
		return noOfAudios;
	}

	public Long getNoOfVideos() {
		return noOfVideos;
	}

	public String getReprImageUrl() {
		return reprImageUrl;
	}

	public Boolean getIsLocked() {
		return isLocked;
	}

	public String getLocationScale() {
		return locationScale;
	}

	public Double getLatitude() {
		return latitude;
	}

	public Double getLongitude() {
		return longitude;
	}

	public String getDateAccuracy() {
		return dateAccuracy;
	}

	public String getFromDate() {
		return fromDate;
	}

	public String getToDate() {
		return toDate;
	}

	public String getObservedInMonth() {
		return observedInMonth;
	}

	public Boolean getGeoPrivacy() {
		return geoPrivacy;
	}

	public String getDatasetTitle() {
		return datasetTitle;
	}

	public LocationInformation getLocationInformation() {
		return locationInformation;
	}

	public UserIbp getUser() {
		return user;
	}

	public List<Facts> getFacts() {
		return facts;
	}

	public List<Flags> getFlags() {
		return flags;
	}

	public Max_voted_reco getMaxVotedReco() {
		return maxVotedReco;
	}

	public List<All_reco_vote> getAllRecoVotes() {
		return allRecoVotes;
	}

	public List<User_group_observations> getUserGroup() {
		return userGroup;
	}

	public List<Custom_fields> getCustomFields() {
		return customFields;
	}

	public List<com.strandls.observation.es.util.Tags> getTags() {
		return tags;
	}

	public Boolean getContainsMedia() {
		return containsMedia;
	}

	public String getUploadProtocol() {
		return uploadProtocol;
	}

	public Integer getFlagCount() {
		return flagCount;
	}

	public String getOrganismRemarks() {
		return organismRemarks;
	}
	public String getAnnotations() {
		return annotations;
	}

	public Integer getRecoVoteCount() {
		return recoVoteCount;
	}


	


	
}
