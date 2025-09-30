/**
 *
 */
package com.strandls.observation.es.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonAlias;
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
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.pojo.FlagShow;
import com.strandls.utility.pojo.Tags;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListPageMapper {

	@JsonAlias("observation_id")
	private Long observationId;

	@JsonAlias("created_on")
	private Date createdOn;

	@JsonAlias("last_revised")
	private Date lastRevised;

	@JsonAlias("from_date")
	private Date observedOn;

	@JsonAlias("place_name")
	private String placeName;

	@JsonAlias("reverse_geocoded_name")
	private String reverseGeocodedName;

	@JsonAlias("group_id")
	private Long speciesGroupId;

	@JsonAlias("group_name")
	private String speciesGroup;

	@JsonAlias("no_of_images")
	private Long noOfImages;

	@JsonAlias("no_of_audio")
	private Long noOfAudios;

	@JsonAlias("no_of_videos")
	private Long noOfVideos;

	@JsonAlias("repr_image_url")
	private String reprImageUrl;

	private UserIbp user;
	private List<FactValuePair> factValuePair;
	private List<FlagShow> flagShow;
	private RecoShow recoShow;
	private List<UserGroupIbp> userGroup;
	private List<CustomFieldObservationData> customField;
	private List<Tags> tags;
	private String observationNotes;

	// custom unpacking logic without @JsonProperty on methods

	@JsonProperty("author_id")
	private void unpackAuthorId(Long author_id) {
		if (user == null)
			user = new UserIbp();
		user.setId(author_id);
	}

	@JsonProperty("created_by")
	private void unpackAuthorName(String created_by) {
		if (user == null)
			user = new UserIbp();
		user.setName(created_by);
	}

	@JsonProperty("profile_pic")
	private void unpackAuthorPic(String profile_pic) {
		if (user == null)
			user = new UserIbp();
		user.setProfilePic(profile_pic);
	}

	@JsonProperty("facts")
	private void unpackFacts(List<Facts> facts) throws ParseException {
		factValuePair = new ArrayList<>();
		SimpleDateFormat sdf = new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy");
		if (facts != null) {
			for (Facts fact : facts) {
				FactValuePair fvp = new FactValuePair();
				fvp.setNameId(fact.getTrait_id());
				fvp.setName(fact.getName());
				fvp.setType(fact.getTrait_types());
				fvp.setIsParticipatry(fact.getIs_participatory());
				if (fact.getTrait_value() != null) {
					for (Trait_value v : fact.getTrait_value()) {
						if (v.getTrait_value_id() != null) {
							fvp.setValue(toTitleCase(v.getValue()));
							fvp.setValueId(v.getTrait_value_id());
						} else if (v.getFrom_value() != null) {
							fvp.setValue(v.getFrom_value() + (v.getTo_value() != null ? " : " + v.getTo_value() : ""));
						} else {
							if (v.getFrom_date() != null)
								fvp.setFromDate(sdf.parse(v.getFrom_date().toString()));
							if (v.getTo_date() != null)
								fvp.setFromDate(sdf.parse(v.getTo_date().toString()));
						}
						factValuePair.add(fvp);
						fvp = new FactValuePair();
						fvp.setNameId(fact.getTrait_id());
						fvp.setName(fact.getName());
						fvp.setType(fact.getTrait_types());
						fvp.setIsParticipatry(fact.getIs_participatory());
					}
				}
			}
		}
	}

	private String toTitleCase(String input) {
		StringBuilder sb = new StringBuilder(input.length());
		boolean next = true;
		for (char c : input.toCharArray()) {
			if (Character.isSpaceChar(c)) {
				next = true;
			} else if (next) {
				c = Character.toTitleCase(c);
				next = false;
			}
			sb.append(c);
		}
		return sb.toString();
	}

	@JsonProperty("flags")
	private void unpackFlags(List<Flags> flags) {
		flagShow = new ArrayList<>();
		if (flags != null) {
			for (Flags flag : flags) {
				FlagShow fs = new FlagShow();
				com.strandls.utility.pojo.Flag fIbp = new com.strandls.utility.pojo.Flag();
				com.strandls.utility.pojo.UserIbp u = new com.strandls.utility.pojo.UserIbp();
				u.setId(flag.getAuthor_id());
				u.setName(flag.getAuthor_name());
				u.setProfilePic(flag.getProfile_pic());
				fIbp.setId(flag.getId());
				fIbp.setCreatedOn(flag.getCreated_on());
				fIbp.setNotes(flag.getNotes());
				fIbp.setFlag(flag.getFlag());
				fs.setFlag(fIbp);
				fs.setUser(u);
				flagShow.add(fs);
			}
		}
	}

	@JsonProperty("is_locked")
	private void unpackIsLocked(Boolean isLocked) {
		if (recoShow == null)
			recoShow = new RecoShow();
		recoShow.setIsLocked(isLocked);
	}

	@JsonProperty("max_voted_reco")
	private void unpackMaxVoted(Max_voted_reco maxVoted) {
		if (recoShow == null)
			recoShow = new RecoShow();
		if (maxVoted != null) {
			StringBuilder cn = new StringBuilder();
			if (maxVoted.getCommon_names() != null) {
				for (Common_names c : maxVoted.getCommon_names()) {
					cn.append(c.getCommon_name()).append("||");
				}
				if (cn.length() > 2)
					cn.setLength(cn.length() - 2);
			}
			RecoIbp ri = new RecoIbp(cn.toString(),
					maxVoted.getItalicised_form() != null ? maxVoted.getItalicised_form()
							: maxVoted.getScientific_name(),
					null, null, null, null, maxVoted.getTaxonstatus(), null);
			Long taxonId = null;
			if (maxVoted.getHierarchy() != null) {
				for (Hierarchy h : maxVoted.getHierarchy()) {
					taxonId = h.getTaxon_id();
				}
			}
			// handle synonym logic
			if ("SYNONYM".equalsIgnoreCase(maxVoted.getTaxonstatus()) && recoShow.getAllRecoVotes() != null) {
				for (AllRecoSugguestions ar : recoShow.getAllRecoVotes()) {
					if (ar.getScientificName() != null
							&& ar.getScientificName().equalsIgnoreCase(maxVoted.getScientific_name())) {
						taxonId = ar.getTaxonId();
					}
				}
			}
			ri.setTaxonId(taxonId);
			recoShow.setRecoIbp(ri);
		}
	}

	@JsonProperty("all_reco_vote")
	private void unpackAllReco(List<All_reco_vote> allRecoVote) {
		List<AllRecoSugguestions> list = new ArrayList<>();
		if (allRecoVote != null) {
			for (All_reco_vote ar : allRecoVote) {
				StringBuilder cn = new StringBuilder();
				if (ar.getCommon_names() != null) {
					for (Common_names c : ar.getCommon_names()) {
						cn.append(c.getCommon_name()).append("||");
					}
					if (cn.length() > 2)
						cn.setLength(cn.length() - 2);
				}
				List<UserIbp> ul = new ArrayList<>();
				if (ar.getAuthors_voted() != null) {
					for (Authors_voted av : ar.getAuthors_voted()) {
						UserIbp u = new UserIbp();
						u.setId(av.getId());
						u.setName(av.getName());
						u.setProfilePic(av.getProfile_pic());
						ul.add(u);
					}
				}
				Long taxonId = null;
				String sciName = null;
				if (ar.getScientific_name() != null && ar.getScientific_name().getTaxon_detail() != null) {
					sciName = ar.getScientific_name().getTaxon_detail().getItalicised_form() != null
							? ar.getScientific_name().getTaxon_detail().getItalicised_form()
							: ar.getScientific_name().getTaxon_detail().getScientific_name();
					taxonId = ar.getScientific_name().getTaxon_detail().getId();
				}
				AllRecoSugguestions sug = new AllRecoSugguestions(cn.toString(), sciName, taxonId, null, ul);
				list.add(sug);
				if (recoShow.getRecoIbp() != null && "SYNONYM".equalsIgnoreCase(recoShow.getRecoIbp().getStatus())) {
					recoShow.getRecoIbp().setTaxonId(taxonId);
				}
			}
		}
		if (recoShow == null)
			recoShow = new RecoShow();
		recoShow.setAllRecoVotes(list);
	}

	@JsonProperty("user_group_observations")
	private void unpackUserGroup(List<User_group_observations> ugObs) {
		userGroup = new ArrayList<>();
		if (ugObs != null) {
			for (User_group_observations ug : ugObs) {
				UserGroupIbp ugI = new UserGroupIbp();
				ugI.setId(ug.getId());
				ugI.setName(ug.getName());
				ugI.setIcon(ug.getIcon());
				ugI.setWebAddress(ug.getDomain_name() != null ? ug.getDomain_name() : ("/group/" + ug.getWebaddress()));
				userGroup.add(ugI);
			}
		}
	}

	@JsonProperty("custom_fields")
	private void unpackCustomField(List<Custom_fields> cfList) {
		customField = new ArrayList<>();
		if (cfList != null) {
			for (Custom_fields c : cfList) {
				CustomFieldObservationData cfod = new CustomFieldObservationData();
				cfod.setUserGroupId(c.getUser_group_id());
				List<CustomFieldData> cfdList = new ArrayList<>();
				if (c.getCustom_field() != null) {
					for (Custom_field cf : c.getCustom_field()) {
						CustomFieldData cd = new CustomFieldData();
						cd.setAllowedParticipation(cf.getAllowed_participation());
						cd.setCfIconUrl(cf.getCf_icon_url());
						cd.setCfId(cf.getCustom_field_id());
						cd.setCfName(cf.getCf_name());
						cd.setCfNotes(cf.getCf_notes());
						Custom_field_values v = cf.getCustom_field_values();
						CustomFieldValuesData vd = new CustomFieldValuesData();
						vd.setFieldTextData(v.getField_text_data());
						vd.setMaxRange(v.getMax_range());
						vd.setMinRange(v.getMin_range());
						CustomFieldValues sc = new CustomFieldValues();
						sc.setValues(v.getSingle_categorical_data());
						vd.setSingleCategoricalData(sc);
						List<CustomFieldValues> mc = new ArrayList<>();
						if (v.getMultiple_categorical_data() != null) {
							for (String m : v.getMultiple_categorical_data()) {
								CustomFieldValues mcv = new CustomFieldValues();
								mcv.setValues(m);
								mc.add(mcv);
							}
						}
						vd.setMultipleCategoricalData(mc);
						cd.setCustomFieldValues(vd);
						cd.setDataType(cf.getData_type());
						cd.setDefaultValue(cf.getDefault_value());
						if (cf.getDisplay_order() != null) {
							cd.setDisplayOrder(Integer.parseInt(cf.getDisplay_order().toString()));
						}
						cd.setFieldType(cf.getField_type());
						cd.setUnits(cf.getUnits());

						cfdList.add(cd);
					}
				}
				cfod.setCustomField(cfdList);
				customField.add(cfod);
			}
		}
	}

	@JsonProperty("tags")
	private void unpackTags(List<com.strandls.observation.es.util.Tags> tagsES) {
		tags = new ArrayList<>();
		if (tagsES != null) {
			for (com.strandls.observation.es.util.Tags t : tagsES) {
				Tags ti = new Tags();
				ti.setId(t.getId());
				ti.setName(t.getName());
				tags.add(ti);
			}
		}
	}

	@JsonProperty("notes")
	private void unpackNotes(String notes) {
		observationNotes = notes;
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
	 * @param observedOn
	 * @param placeName
	 * @param reverseGeocodedName
	 * @param speciesGroupId
	 * @param speciesGroup
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
	public ObservationListPageMapper(Long observationId, Date createdOn, Date lastRevised, Date observedOn,
			String placeName, String reverseGeocodedName, Long speciesGroupId, String speciesGroup, Long noOfImages,
			Long noOfAudios, Long noOfVideos, String reprImageUrl, UserIbp user, List<FactValuePair> factValuePair,
			List<FlagShow> flagShow, RecoShow recoShow, List<UserGroupIbp> userGroup,
			List<CustomFieldObservationData> customField, List<Tags> tags, String observationNotes) {
		super();
		this.observationId = observationId;
		this.createdOn = createdOn;
		this.lastRevised = lastRevised;
		this.observedOn = observedOn;
		this.placeName = placeName;
		this.reverseGeocodedName = reverseGeocodedName;
		this.speciesGroupId = speciesGroupId;
		this.speciesGroup = speciesGroup;
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
		this.observationNotes = observationNotes;
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

	public Date getObservedOn() {
		return observedOn;
	}

	public void setObservedOn(Date observedOn) {
		this.observedOn = observedOn;
	}

	public String getPlaceName() {
		return placeName;
	}

	public void setPlaceName(String placeName) {
		this.placeName = placeName;
	}

	public String getReverseGeocodedName() {
		return reverseGeocodedName;
	}

	public void setReverseGeocodedName(String reverseGeocodedName) {
		this.reverseGeocodedName = reverseGeocodedName;
	}

	public Long getSpeciesGroupId() {
		return speciesGroupId;
	}

	public void setSpeciesGroupId(Long speciesGroupId) {
		this.speciesGroupId = speciesGroupId;
	}

	public String getSpeciesGroup() {
		return speciesGroup;
	}

	public void setSpeciesGroup(String speciesGroup) {
		this.speciesGroup = speciesGroup;
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

	public List<UserGroupIbp> getUserGroup() {
		return userGroup;
	}

	public void setUserGroup(List<UserGroupIbp> userGroup) {
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

	public String getObservationNotes() {
		return observationNotes;
	}

	public void setObservationNotes(String observationNotes) {
		this.observationNotes = observationNotes;
	}

}