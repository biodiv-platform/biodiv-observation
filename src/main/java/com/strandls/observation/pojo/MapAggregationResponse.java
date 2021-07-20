package com.strandls.observation.pojo;

import java.util.Map;

public class MapAggregationResponse {

	private Map<String, Long> groupSpeciesName;
	private Map<String, Long> groupStatus;
	private Map<String, Long> groupTaxonIDExists;
	private Map<String, Long> groupUserGroupName;
	private Map<String, Long> groupIdentificationNameExists;
	private Map<String, Long> groupFlag;
	private Map<String, Long> groupValidate;
	private Long groupAudio;
	private Long groupVideo;
	private Long groupImages;
	private Long groupNoMedia;
	private Map<String, Long> groupMonth;
	private Map<String, Map<String, Long>> groupTraits;
	private Map<String, Long> groupState;
	private Map<String, Long> groupRank;
	private Map<String, Long> geoEnity;
	private Map<String, Map<String, Long>> groupCustomField;

	public Map<String, Long> getGroupSpeciesName() {
		return groupSpeciesName;
	}

	public void setGroupSpeciesName(Map<String, Long> groupSpeciesName) {
		this.groupSpeciesName = groupSpeciesName;
	}

	public Map<String, Long> getGroupStatus() {
		return groupStatus;
	}

	public void setGroupStatus(Map<String, Long> groupStatus) {
		this.groupStatus = groupStatus;
	}

	public Map<String, Long> getGroupTaxonIDExists() {
		return groupTaxonIDExists;
	}

	public void setGroupTaxonIDExists(Map<String, Long> groupTaxonIDExists) {
		this.groupTaxonIDExists = groupTaxonIDExists;
	}

	public Map<String, Long> getGroupUserGroupName() {
		return groupUserGroupName;
	}

	public void setGroupUserGroupName(Map<String, Long> groupUserGroupName) {
		this.groupUserGroupName = groupUserGroupName;
	}

	public Map<String, Long> getGroupIdentificationNameExists() {
		return groupIdentificationNameExists;
	}

	public void setGroupIdentificationNameExists(Map<String, Long> groupIdentificationNameExists) {
		this.groupIdentificationNameExists = groupIdentificationNameExists;
	}

	public Map<String, Long> getGroupFlag() {
		return groupFlag;
	}

	public void setGroupFlag(Map<String, Long> groupFlag) {
		this.groupFlag = groupFlag;
	}

	public Map<String, Long> getGroupValidate() {
		return groupValidate;
	}

	public void setGroupValidate(Map<String, Long> groupValidate) {
		this.groupValidate = groupValidate;
	}

	public Long getGroupAudio() {
		return groupAudio;
	}

	public void setGroupAudio(Long groupAudio) {
		this.groupAudio = groupAudio;
	}

	public Long getGroupVideo() {
		return groupVideo;
	}

	public void setGroupVideo(Long groupVideo) {
		this.groupVideo = groupVideo;
	}

	public Long getGroupImages() {
		return groupImages;
	}

	public void setGroupImages(Long groupImages) {
		this.groupImages = groupImages;
	}

	public Long getGroupNoMedia() {
		return groupNoMedia;
	}

	public void setGroupNoMedia(Long groupNoMedia) {
		this.groupNoMedia = groupNoMedia;
	}

	public Map<String, Long> getGroupMonth() {
		return groupMonth;
	}

	public void setGroupMonth(Map<String, Long> groupMonth) {
		this.groupMonth = groupMonth;
	}

	public Map<String, Map<String, Long>> getGroupTraits() {
		return groupTraits;
	}

	public void setGroupTraits(Map<String, Map<String, Long>> groupTraits) {
		this.groupTraits = groupTraits;
	}

	public Map<String, Long> getGroupState() {
		return groupState;
	}

	public void setGroupState(Map<String, Long> groupState) {
		this.groupState = groupState;
	}

	public Map<String, Long> getGroupRank() {
		return groupRank;
	}

	public void setGroupRank(Map<String, Long> groupRank) {
		this.groupRank = groupRank;
	}

	public Map<String, Map<String, Long>> getGroupCustomField() {
		return groupCustomField;
	}

	public void setGroupCustomField(Map<String, Map<String, Long>> groupCustomField) {
		this.groupCustomField = groupCustomField;
	}

	public Map<String, Long> getGeoEnity() {
		return geoEnity;
	}

	public void setGeoEnity(Map<String, Long> geoEnity) {
		this.geoEnity = geoEnity;
	}

}
