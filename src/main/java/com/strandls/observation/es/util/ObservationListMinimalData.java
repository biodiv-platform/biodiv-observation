/**
 * 
 */
package com.strandls.observation.es.util;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.user.pojo.UserIbp;

/**
 * @author Abhishek Rudra
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListMinimalData {

	private Long observationId;
	private Long speciesGroupId;
	private String speciesGroup;
	private String thumbnail;
	private RecoIbp recoIbp;
	private UserIbp user;

	@JsonProperty("observation_id")
	private void unpackName(Long observation_id) {
		observationId = observation_id;
	}

	@JsonProperty(value = "group_id")
	private void unpackSGroupId(Long group_id) {
		speciesGroupId = group_id;
	}

	@JsonProperty(value = "group_name")
	private void unpacksGroup(String group_name) {
		speciesGroup = group_name;
	}

	@JsonProperty(value = "repr_image_url")
	private void unpackReprImage(String reprImage) {
		thumbnail = reprImage;
	}

	@JsonProperty(value = "max_voted_reco")
	private void unpackMaxName(Max_voted_reco maxVoted) {
		if (maxVoted != null) {
			String commonName = "";
			if (maxVoted.getCommon_names() != null) {
				for (Common_names cn : maxVoted.getCommon_names()) {
					commonName = commonName + cn.getCommon_name() + "||";
				}
				commonName = commonName.substring(0, commonName.length() - 2);
			}

			RecoIbp recoIbp = new RecoIbp(commonName, maxVoted.getItalicised_form(), null, null, null, null,
					maxVoted.getTaxonstatus(), null);
			Long taxonId = null;
			if (maxVoted.getHierarchy() != null)
				for (Hierarchy hierarchy : maxVoted.getHierarchy()) {
					taxonId = hierarchy.getTaxon_id();
				}
			recoIbp.setTaxonId(taxonId);
			this.recoIbp = recoIbp;

		}

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

	/**
	 * 
	 */
	public ObservationListMinimalData() {
		super();
	}

	/**
	 * @param observationId
	 * @param speciesGroupId
	 * @param speciesGroup
	 * @param thumbnail
	 * @param recoIbp
	 * @param user
	 */
	public ObservationListMinimalData(Long observationId, Long speciesGroupId, String speciesGroup, String thumbnail,
			RecoIbp recoIbp, UserIbp user) {
		super();
		this.observationId = observationId;
		this.speciesGroupId = speciesGroupId;
		this.speciesGroup = speciesGroup;
		this.thumbnail = thumbnail;
		this.recoIbp = recoIbp;
		this.user = user;
	}

	public Long getObservationId() {
		return observationId;
	}

	public void setObservationId(Long observationId) {
		this.observationId = observationId;
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

	public String getThumbnail() {
		return thumbnail;
	}

	public void setThumbnail(String thumbnail) {
		this.thumbnail = thumbnail;
	}

	public RecoIbp getRecoIbp() {
		return recoIbp;
	}

	public void setRecoIbp(RecoIbp recoIbp) {
		this.recoIbp = recoIbp;
	}

	public UserIbp getUser() {
		return user;
	}

	public void setUser(UserIbp user) {
		this.user = user;
	}

}
