/**
 * 
 */
package com.strandls.observation.es.util;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.strandls.observation.pojo.RecoIbp;

/**
 * @author Abhishek Rudra
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListMinimalData {

	private Long observationId;
	private Long sGroupId;
	private String sGroup;
	private String thumbnail;
	private RecoIbp recoIbp;

	@JsonProperty("observation_id")
	private void unpackName(Long observation_id) {
		observationId = observation_id;
	}

	@JsonProperty(value = "group_id")
	private void unpackSGroupId(Long group_id) {
		sGroupId = group_id;
	}

	@JsonProperty(value = "group_name")
	private void unpacksGroup(String group_name) {
		sGroup = group_name;
	}

	@JsonProperty(value = "repr_image_url")
	private void unpackReprImage(String reprImage) {
		thumbnail = reprImage;
	}

	@JsonProperty(value = "max_voted_reco")
	private void unpackMaxName(Max_voted_reco maxVoted) {
		if (maxVoted != null) {
			String commonName = "";
			if(maxVoted.getCommon_names()!=null) {
				for (Common_names cn : maxVoted.getCommon_names()) {
					commonName = commonName + cn.getCommon_name() + "||";
				}
				commonName = commonName.substring(0, commonName.length() - 2);
			}
			
			RecoIbp recoIbp = new RecoIbp(commonName, maxVoted.getScientific_name(), null, null, null, null,
					maxVoted.getTaxonstatus(), null);
			Long taxonId = null;
			for (Hierarchy hierarchy : maxVoted.getHierarchy()) {
				taxonId = hierarchy.getTaxon_id();
			}
			recoIbp.setTaxonId(taxonId);
			this.recoIbp = recoIbp;

		}

	}

	/**
	 * 
	 */
	public ObservationListMinimalData() {
		super();
	}

	/**
	 * @param observationId
	 * @param sGroupId
	 * @param sGroup
	 * @param thumbnail
	 * @param recoIbp
	 */
	public ObservationListMinimalData(Long observationId, Long sGroupId, String sGroup, String thumbnail,
			RecoIbp recoIbp) {
		super();
		this.observationId = observationId;
		this.sGroupId = sGroupId;
		this.sGroup = sGroup;
		this.thumbnail = thumbnail;
		this.recoIbp = recoIbp;
	}

	public Long getObservationId() {
		return observationId;
	}

	public void setObservationId(Long observationId) {
		this.observationId = observationId;
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

}
