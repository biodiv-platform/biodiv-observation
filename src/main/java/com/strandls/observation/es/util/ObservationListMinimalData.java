/**
 * 
 */
package com.strandls.observation.es.util;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Abhishek Rudra
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListMinimalData {

	@JsonProperty("observation_id")
	private Long observationId;
	@JsonProperty("group_name")
	private String groupName;
	@JsonProperty("repr_image_url")
	private String thumbnail;
	private String name;

	@JsonProperty("max_voted_reco")
	private void unpackName(Max_voted_reco maxVotedReco) {
		if (maxVotedReco != null)
			name = maxVotedReco.getScientific_name();
		else
			name = "unknown";
	}

	/**
	 * 
	 */
	public ObservationListMinimalData() {
		super();
	}

	/**
	 * @param observationId
	 * @param groupName
	 * @param thumbnail
	 * @param name
	 */
	public ObservationListMinimalData(Long observationId, String groupName, String thumbnail, String name) {
		super();
		this.observationId = observationId;
		this.groupName = groupName;
		this.thumbnail = thumbnail;
		this.name = name;
	}

	public Long getObservationId() {
		return observationId;
	}

	public void setObservationId(Long observationId) {
		this.observationId = observationId;
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	public String getThumbnail() {
		return thumbnail;
	}

	public void setThumbnail(String thumbnail) {
		this.thumbnail = thumbnail;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}
