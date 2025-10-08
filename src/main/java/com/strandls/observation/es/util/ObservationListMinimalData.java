package com.strandls.observation.es.util;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.user.pojo.UserIbp;

/**
 * @author Abhishek Rudra
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListMinimalData {

	@JsonAlias("observation_id")
	private Long observationId;

	@JsonAlias("group_id")
	private Long speciesGroupId;

	@JsonAlias("group_name")
	private String speciesGroup;

	@JsonAlias("repr_image_url")
	private String thumbnail;

	private RecoIbp recoIbp;

	private UserIbp user;

	private Double latitude;
	private Double longitude;

	@JsonAlias("created_on")
	private Date createdOn;

	@JsonAlias("from_date")
	private Date observedOn;

	@JsonProperty("location")
	private void unpackLocation(Location location) {
		if (location != null) {
			this.latitude = location.getLat();
			this.longitude = location.getLon();
		}
	}

	@JsonProperty("max_voted_reco")
	private void unpackMaxVoted(Max_voted_reco maxVoted) {
		if (maxVoted != null) {
			StringBuilder commonNameBuilder = new StringBuilder();
			if (maxVoted.getCommon_names() != null) {
				for (Common_names cn : maxVoted.getCommon_names()) {
					commonNameBuilder.append(cn.getCommon_name()).append("||");
				}
				if (commonNameBuilder.length() > 0)
					commonNameBuilder.setLength(commonNameBuilder.length() - 2); // Remove last ||
			}

			RecoIbp reco = new RecoIbp(commonNameBuilder.toString(),
					maxVoted.getItalicised_form() != null ? maxVoted.getItalicised_form()
							: maxVoted.getScientific_name(),
					null, null, null, null, maxVoted.getTaxonstatus(), null);

			Long taxonId = null;
			if (maxVoted.getHierarchy() != null) {
				for (Hierarchy h : maxVoted.getHierarchy()) {
					taxonId = h.getTaxon_id(); // Last one wins
				}
			}
			reco.setTaxonId(taxonId);
			this.recoIbp = reco;
		}
	}

	@JsonProperty("author_id")
	private void setAuthorId(Long authorId) {
		if (user == null)
			user = new UserIbp();
		user.setId(authorId);
	}

	@JsonProperty("created_by")
	private void setAuthorName(String createdBy) {
		if (user == null)
			user = new UserIbp();
		user.setName(createdBy);
	}

	@JsonProperty("profile_pic")
	private void setProfilePic(String profilePic) {
		if (user == null)
			user = new UserIbp();
		user.setProfilePic(profilePic);
	}

	public ObservationListMinimalData() {
	}

	public ObservationListMinimalData(Long observationId, Long speciesGroupId, String speciesGroup, String thumbnail,
			RecoIbp recoIbp, UserIbp user, Double latitude, Double longitude, Date createdOn, Date observedOn) {
		this.observationId = observationId;
		this.speciesGroupId = speciesGroupId;
		this.speciesGroup = speciesGroup;
		this.thumbnail = thumbnail;
		this.recoIbp = recoIbp;
		this.user = user;
		this.latitude = latitude;
		this.longitude = longitude;
		this.createdOn = createdOn;
		this.observedOn = observedOn;
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

	public Date getObservedOn() {
		return observedOn;
	}

	public void setObservedOn(Date observedOn) {
		this.observedOn = observedOn;
	}

	public Date getCreatedOn() {
		return createdOn;
	}

	public void setCreatedOn(Date createdOn) {
		this.createdOn = createdOn;
	}
}
