/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Abhishek Rudra
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListPageMapper {

	@JsonProperty("observation_id")
	private Long observationId;
	@JsonProperty("created_on")
	private Date createdOn;
	@JsonProperty("reverse_geocoded_name")
	private String geocodedPlaceName;
	@JsonProperty("group_name")
	private String groupName;
	@JsonProperty("flags")
	private List<Flags> flags;
	@JsonProperty("observation_resource")
	private List<Observation_resource> resources;
	@JsonProperty("no_of_images")
	private Long noOfImages;
	@JsonProperty("no_of_audio")
	private Long noOfAudios;
	@JsonProperty("no_of_videos")
	private Long noOfVideos;
	@JsonProperty("max_voted_reco")
	private Max_voted_reco maxVotedReco;
	@JsonProperty("author_id")
	private Long authorId;
	@JsonProperty("created_by")
	private String authorName;
	@JsonProperty("profile_pic")
	private String authorProfilePic;
	@JsonProperty("repr_image_url")
	private String reprImageUrl;
	@JsonProperty("all_reco_vote")
	private List<All_reco_vote> allRecoVote;
	@JsonProperty("facts")
	private List<Facts> facts;
	@JsonProperty("user_group_observations")
	private List<User_group_observations> userGroup;

	/**
	 * 
	 */
	public ObservationListPageMapper() {
		super();
	}

	/**
	 * @param observationId
	 * @param createdOn
	 * @param geocodedPlaceName
	 * @param groupName
	 * @param flags
	 * @param resources
	 * @param noOfImages
	 * @param noOfAudios
	 * @param noOfVideos
	 * @param maxVotedReco
	 * @param authorId
	 * @param authorName
	 * @param authorProfilePic
	 * @param reprImageUrl
	 * @param allRecoVote
	 * @param facts
	 * @param userGroup
	 */
	public ObservationListPageMapper(Long observationId, Date createdOn, String geocodedPlaceName, String groupName,
			List<Flags> flags, List<Observation_resource> resources, Long noOfImages, Long noOfAudios, Long noOfVideos,
			Max_voted_reco maxVotedReco, Long authorId, String authorName, String authorProfilePic, String reprImageUrl,
			List<All_reco_vote> allRecoVote, List<Facts> facts, List<User_group_observations> userGroup) {
		super();
		this.observationId = observationId;
		this.createdOn = createdOn;
		this.geocodedPlaceName = geocodedPlaceName;
		this.groupName = groupName;
		this.flags = flags;
		this.resources = resources;
		this.noOfImages = noOfImages;
		this.noOfAudios = noOfAudios;
		this.noOfVideos = noOfVideos;
		this.maxVotedReco = maxVotedReco;
		this.authorId = authorId;
		this.authorName = authorName;
		this.authorProfilePic = authorProfilePic;
		this.reprImageUrl = reprImageUrl;
		this.allRecoVote = allRecoVote;
		this.facts = facts;
		this.userGroup = userGroup;
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

	public String getGeocodedPlaceName() {
		return geocodedPlaceName;
	}

	public void setGeocodedPlaceName(String geocodedPlaceName) {
		this.geocodedPlaceName = geocodedPlaceName;
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	public List<Flags> getFlags() {
		return flags;
	}

	public void setFlags(List<Flags> flags) {
		this.flags = flags;
	}

	public List<Observation_resource> getResources() {
		return resources;
	}

	public void setResources(List<Observation_resource> resources) {
		this.resources = resources;
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

	public Max_voted_reco getMaxVotedReco() {
		return maxVotedReco;
	}

	public void setMaxVotedReco(Max_voted_reco maxVotedReco) {
		this.maxVotedReco = maxVotedReco;
	}

	public Long getAuthorId() {
		return authorId;
	}

	public void setAuthorId(Long authorId) {
		this.authorId = authorId;
	}

	public String getAuthorName() {
		return authorName;
	}

	public void setAuthorName(String authorName) {
		this.authorName = authorName;
	}

	public String getAuthorProfilePic() {
		return authorProfilePic;
	}

	public void setAuthorProfilePic(String authorProfilePic) {
		this.authorProfilePic = authorProfilePic;
	}

	public String getReprImageUrl() {
		return reprImageUrl;
	}

	public void setReprImageUrl(String reprImageUrl) {
		this.reprImageUrl = reprImageUrl;
	}

	public List<All_reco_vote> getAllRecoVote() {
		return allRecoVote;
	}

	public void setAllRecoVote(List<All_reco_vote> allRecoVote) {
		this.allRecoVote = allRecoVote;
	}

	public List<Facts> getFacts() {
		return facts;
	}

	public void setFacts(List<Facts> facts) {
		this.facts = facts;
	}

	public List<User_group_observations> getUserGroup() {
		return userGroup;
	}

	public void setUserGroup(List<User_group_observations> userGroup) {
		this.userGroup = userGroup;
	}

}
