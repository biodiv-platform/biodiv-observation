/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.strandls.utility.pojo.Tags;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoShow;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.pojo.CustomFieldData;
import com.strandls.userGroup.pojo.CustomFieldObservationData;
import com.strandls.userGroup.pojo.CustomFieldValues;
import com.strandls.userGroup.pojo.CustomFieldValuesData;
import com.strandls.utility.pojo.Flag;
import com.strandls.utility.pojo.FlagShow;

/**
 * @author Abhishek Rudra
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListPageMapper {

	private Long observationId;
	private Date createdOn;
	private Date lastRevised;
	private String reverseGeocodedName;
	private Long sGroupId;
	private String sGroup;
	private Long noOfImages;
	private Long noOfAudios;
	private Long noOfVideos;
	private String reprImageUrl;

	private UserIbp user;
	private List<FactValuePair> factValuePair;
	private List<FlagShow> flagShow;
	private RecoShow recoShow;
	private List<Long> userGroup;
	private List<CustomFieldObservationData> customField;
	private List<Tags> tags;

	@JsonProperty("observation_id")
	private void unpackObservationId(Long observation_id) {
		observationId = observation_id;
	}

	@JsonProperty("created_on")
	private void unpackDate(Date created_on) {
		createdOn = created_on;
	}

	@JsonProperty(value = "last_revised")
	private void unpackLastRevised(Date lastModified) {
		lastRevised = lastModified;
	}

	@JsonProperty(value = "reverse_geocoded_name")
	private void unpackAddress(String reverse_geocoded_name) {
		reverseGeocodedName = reverse_geocoded_name;
	}

	@JsonProperty(value = "group_id")
	private void unpackSGroupId(Long group_id) {
		sGroupId = group_id;
	}

	@JsonProperty(value = "group_name")
	private void unpacksGroup(String group_name) {
		sGroup = group_name;
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

//	---------USER IBP------------

	@JsonProperty(value = "author_id")
	private void unpackAuthorId(Long author_id) {
		user = new UserIbp();
		user.setId(author_id);
	}

	@JsonProperty(value = "created_by")
	private void unpackAuthorName(String created_by) {
		user.setName(created_by);
	}

	@JsonProperty(value = "profile_pic")
	private void unpackAuthorPic(String profile_pic) {
		user.setProfilePic(profile_pic);
	}

//	---------FACT VALUE PAIR-----------

	@JsonProperty(value = "facts")
	private void unpackFacts(List<Facts> facts) {
		factValuePair = new ArrayList<FactValuePair>();
		FactValuePair fvp = new FactValuePair();
		if(facts!=null) {
			for (Facts fact : facts) {
				fvp.setNameId(fact.getTrait_id());
				fvp.setName(fact.getName());
				fvp.setType(fact.getTrait_types());
				fvp.setIsParticipatry(fact.getIs_participatory());
				if(fact.getTrait_value()!=null) {
					for (Trait_value value : fact.getTrait_value()) {
						fvp.setValue(value.getValue());
						fvp.setValueId(value.getTrait_value_id());
						factValuePair.add(fvp);
					}
				}
				
			}
		}
		
	}

//	------------FLAG SHOW--------------

	@JsonProperty(value = "flags")
	private void unpackFlags(List<Flags> flags) {
		flagShow = new ArrayList<FlagShow>();
		FlagShow fs = new FlagShow();
		Flag flagIbp = new Flag();
		com.strandls.utility.pojo.UserIbp useribp = new com.strandls.utility.pojo.UserIbp();
		if (flags != null) {
			for (Flags flag : flags) {

				useribp.setId(flag.getAuthor_id());
				useribp.setName(flag.getAuthor_name());
				useribp.setProfilePic(flag.getProfile_pic());

				flagIbp.setId(flag.getId());
				flagIbp.setCreatedOn(flag.getCreated_on());
				flagIbp.setNotes(flag.getNotes());
				flagIbp.setFlag(flag.getFlag());

				fs.setFlag(flagIbp);
				fs.setUser(useribp);
				flagShow.add(fs);

			}
		}

	}

//	---------RECO IBP----ALL RECO VOTE ----IS LOCKED-------

//	---------------IS LOCKED------------------
	@JsonProperty(value = "is_locked")
	private void unpackIsLocked(Boolean isLocked) {
		recoShow = new RecoShow();
		recoShow.setIsLocked(isLocked);
	}

//	----------------RECO IBP------------------
	@JsonProperty(value = "max_voted_reco")
	private void unpackMaxName(Max_voted_reco maxVoted) {
		if (maxVoted != null) {
			String commonName = "";
			for (Common_names cn : maxVoted.getCommon_names()) {
				commonName = commonName + cn.getCommon_name() + "||";
			}
			commonName = commonName.substring(0, commonName.length() - 2);
			RecoIbp recoIbp = new RecoIbp(commonName, maxVoted.getScientific_name(), null, null, null, null,
					maxVoted.getTaxonstatus(), null);
			Long taxonId = null;
			if (maxVoted.getHierarchy() != null) {
				for (Hierarchy hierarchy : maxVoted.getHierarchy()) {
					taxonId = hierarchy.getTaxon_id();
				}
			}

			recoIbp.setTaxonId(taxonId);
			recoShow.setRecoIbp(recoIbp);
		}

	}

//	------------ALL RECO VOTE SUGGESTIONS--------------------
	@JsonProperty(value = "all_reco_vote")
	private void unpackAllReco(List<All_reco_vote> allRecoVote) {

		List<AllRecoSugguestions> allRecoList = new ArrayList<AllRecoSugguestions>();
		AllRecoSugguestions allRecoSuggeSugguestions = new AllRecoSugguestions();
		if (allRecoVote != null) {
			for (All_reco_vote allreco : allRecoVote) {
				String commonName = "";
				for (Common_names cn : allreco.getCommon_names()) {
					commonName = commonName + "||" + cn.getCommon_name();
				}
				List<UserIbp> userList = new ArrayList<UserIbp>();
				UserIbp useribp = new UserIbp();
				for (Authors_voted author : allreco.getAuthors_voted()) {
					useribp.setId(author.getId());
					useribp.setName(author.getName());
					useribp.setProfilePic(author.getProfile_pic());
				}
				allRecoSuggeSugguestions = new AllRecoSugguestions(commonName, allreco.getScientific_name().getName(),
						allreco.getScientific_name().getTaxon_detail().getId(),
						Long.parseLong(allreco.getScientific_name().getTaxon_detail().getSpecies_id()), userList);
				allRecoList.add(allRecoSuggeSugguestions);
			}
		}

		recoShow.setAllRecoVotes(allRecoList);
	}

//	---------------USER GROUP FOR OBSERVATION----------------

	@JsonProperty(value = "user_group_observations")
	private void unpackUserGroup(List<User_group_observations> ugObservation) {
		userGroup = new ArrayList<Long>();
		if (ugObservation != null) {
			for (User_group_observations ug : ugObservation) {
				userGroup.add(ug.getId());
			}
		}

	}

//	--------------------CUSTOM FIELDS--------------
	@JsonProperty(value = "custom_fields")
	private void unpackCustomField(List<Custom_fields> custom_fields) {
		customField = new ArrayList<CustomFieldObservationData>();
		CustomFieldObservationData cfObservationData = new CustomFieldObservationData();
		if (custom_fields != null) {
			for (Custom_fields cfs : custom_fields) {
				cfObservationData.setUserGroupId(cfs.getUser_group_id());

				List<CustomFieldData> customFieldlist = new ArrayList<CustomFieldData>();
				CustomFieldData cfData = new CustomFieldData();
				CustomFieldValuesData cfValueData = new CustomFieldValuesData();
				if (cfs.getCustom_field() != null) {
					for (Custom_field cf : cfs.getCustom_field()) {
						cfData.setAllowedParticipation(cf.getAllowed_participation());
						cfData.setCfIconUrl(cf.getCf_icon_url());
						cfData.setCfId(cf.getCustom_field_id());
						cfData.setCfName(cf.getCf_name());
						cfData.setCfNotes(cf.getCf_notes());

						Custom_field_values cfValue = cf.getCustom_field_values();
						cfValueData.setFieldTextData(cfValue.getField_text_data());
						cfValueData.setMaxRange(cfValue.getMax_range());
						cfValueData.setMinRange(cfValue.getMin_range());
						CustomFieldValues singleCategorical = new CustomFieldValues();
						singleCategorical.setValues(cfValue.getSingle_categorical_data());
						cfValueData.setSingleCategoricalData(singleCategorical);
						List<CustomFieldValues> multipleCategorical = new ArrayList<CustomFieldValues>();
						if (cfValue.getMultiple_categorical_data() != null) {
							for (String multi : cfValue.getMultiple_categorical_data()) {
								singleCategorical.setValues(multi);
								multipleCategorical.add(singleCategorical);
							}
						}

						cfValueData.setMultipleCategoricalData(multipleCategorical);

						cfData.setCustomFieldValues(cfValueData);
						cfData.setDataType(cf.getData_type());
						cfData.setDefaultValue(cf.getDefault_value());
						cfData.setDisplayOrder(Integer.parseInt(cf.getDisplay_order().toString()));
						cfData.setFieldType(cf.getField_type());
						cfData.setUnits(cf.getUnits());

						customFieldlist.add(cfData);
					}
				}

				cfObservationData.setCustomField(customFieldlist);

				customField.add(cfObservationData);
			}
		}

	}

//	---------------TAGS-------------------------

	@JsonProperty(value = "tags")
	private void unpacktags(List<com.strandls.observation.es.util.Tags> tagsES) {
		tags = new ArrayList<Tags>();
		Tags tagIbp = new Tags();
		if (tagsES != null) {
			for (com.strandls.observation.es.util.Tags tag : tagsES) {
				tagIbp.setName(tag.getName());
				tagIbp.setId(tag.getId());
				tags.add(tagIbp);
			}
		}

	}

	/**
	 * 
	 */
	public ObservationListPageMapper() {
		super();
	}

	/**
	 * @param observationId
	 * @param createdOn
	 * @param lastRevised
	 * @param reverseGeocodedName
	 * @param sGroupId
	 * @param sGroup
	 * @param noOfImages
	 * @param noOfAudios
	 * @param noOfVideos
	 * @param reprImageUrl
	 * @param user
	 * @param factValuePair
	 * @param flagShow
	 * @param recoShow
	 * @param userGroup
	 * @param customField
	 * @param tags
	 */
	public ObservationListPageMapper(Long observationId, Date createdOn, Date lastRevised, String reverseGeocodedName,
			Long sGroupId, String sGroup, Long noOfImages, Long noOfAudios, Long noOfVideos, String reprImageUrl,
			UserIbp user, List<FactValuePair> factValuePair, List<FlagShow> flagShow, RecoShow recoShow,
			List<Long> userGroup, List<CustomFieldObservationData> customField, List<Tags> tags) {
		super();
		this.observationId = observationId;
		this.createdOn = createdOn;
		this.lastRevised = lastRevised;
		this.reverseGeocodedName = reverseGeocodedName;
		this.sGroupId = sGroupId;
		this.sGroup = sGroup;
		this.noOfImages = noOfImages;
		this.noOfAudios = noOfAudios;
		this.noOfVideos = noOfVideos;
		this.reprImageUrl = reprImageUrl;
		this.user = user;
		this.factValuePair = factValuePair;
		this.flagShow = flagShow;
		this.recoShow = recoShow;
		this.userGroup = userGroup;
		this.customField = customField;
		this.tags = tags;
	}

	public Long getObservationId() {
		return observationId;
	}

	public void setObservationId(Long observationId) {
		this.observationId = observationId;
	}

	public Date getCreatedOn() {
		return createdOn;
	}

	public void setCreatedOn(Date createdOn) {
		this.createdOn = createdOn;
	}

	public Date getLastRevised() {
		return lastRevised;
	}

	public void setLastRevised(Date lastRevised) {
		this.lastRevised = lastRevised;
	}

	public String getReverseGeocodedName() {
		return reverseGeocodedName;
	}

	public void setReverseGeocodedName(String reverseGeocodedName) {
		this.reverseGeocodedName = reverseGeocodedName;
	}

	public Long getsGroupId() {
		return sGroupId;
	}

	public void setsGroupId(Long sGroupId) {
		this.sGroupId = sGroupId;
	}

	public String getsGroup() {
		return sGroup;
	}

	public void setsGroup(String sGroup) {
		this.sGroup = sGroup;
	}

	public Long getNoOfImages() {
		return noOfImages;
	}

	public void setNoOfImages(Long noOfImages) {
		this.noOfImages = noOfImages;
	}

	public Long getNoOfAudios() {
		return noOfAudios;
	}

	public void setNoOfAudios(Long noOfAudios) {
		this.noOfAudios = noOfAudios;
	}

	public Long getNoOfVideos() {
		return noOfVideos;
	}

	public void setNoOfVideos(Long noOfVideos) {
		this.noOfVideos = noOfVideos;
	}

	public String getReprImageUrl() {
		return reprImageUrl;
	}

	public void setReprImageUrl(String reprImageUrl) {
		this.reprImageUrl = reprImageUrl;
	}

	public UserIbp getUser() {
		return user;
	}

	public void setUser(UserIbp user) {
		this.user = user;
	}

	public List<FactValuePair> getFactValuePair() {
		return factValuePair;
	}

	public void setFactValuePair(List<FactValuePair> factValuePair) {
		this.factValuePair = factValuePair;
	}

	public List<FlagShow> getFlagShow() {
		return flagShow;
	}

	public void setFlagShow(List<FlagShow> flagShow) {
		this.flagShow = flagShow;
	}

	public RecoShow getRecoShow() {
		return recoShow;
	}

	public void setRecoShow(RecoShow recoShow) {
		this.recoShow = recoShow;
	}

	public List<Long> getUserGroup() {
		return userGroup;
	}

	public void setUserGroup(List<Long> userGroup) {
		this.userGroup = userGroup;
	}

	public List<CustomFieldObservationData> getCustomField() {
		return customField;
	}

	public void setCustomField(List<CustomFieldObservationData> customField) {
		this.customField = customField;
	}

	public List<Tags> getTags() {
		return tags;
	}

	public void setTags(List<Tags> tags) {
		this.tags = tags;
	}

}
