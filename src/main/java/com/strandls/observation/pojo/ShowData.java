/**
 * 
 */
package com.strandls.observation.pojo;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import com.strandls.esmodule.pojo.ObservationInfo;
import com.strandls.esmodule.pojo.ObservationNearBy;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.pojo.CustomFieldObservationData;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.pojo.FlagShow;
import com.strandls.utility.pojo.Tags;

/**
 * @author Abhishek Rudra
 *
 */

public class ShowData implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3361865148967809245L;
	/**
	 * 
	 */
	private Observation observation;
	private List<FactValuePair> factValuePair;
	private List<ObservationResourceUser> observationResource;
	private List<UserGroupIbp> userGroups;
	private List<CustomFieldObservationData> customField;
	private ObservationLocationInfo layerInfo;
	private ObservationInfo esLayerInfo;
	private RecoIbp recoIbp;
	private List<FlagShow> flag;
	private List<Tags> tags;
	private List<Featured> fetaured;
	private UserIbp authorInfo;
	private Map<String, String> authorScore;
	private List<AllRecoSugguestions> allRecoVotes;
	private List<ObservationNearBy> observationNearBy;
	private Integer activityCount;

	/**
	 * 
	 */
	public ShowData() {
		super();
	}

	/**
	 * @param observation
	 * @param factValuePair
	 * @param observationResource
	 * @param userGroups
	 * @param customField
	 * @param layerInfo
	 * @param esLayerInfo
	 * @param recoIbp
	 * @param flag
	 * @param tags
	 * @param fetaured
	 * @param authorInfo
	 * @param authorScore
	 * @param allRecoVotes
	 * @param observationNearBy
	 * @param activityCount
	 */
	public ShowData(Observation observation, List<FactValuePair> factValuePair,
			List<ObservationResourceUser> observationResource, List<UserGroupIbp> userGroups,
			List<CustomFieldObservationData> customField, ObservationLocationInfo layerInfo,
			ObservationInfo esLayerInfo, RecoIbp recoIbp, List<FlagShow> flag, List<Tags> tags, List<Featured> fetaured,
			UserIbp authorInfo, Map<String, String> authorScore, List<AllRecoSugguestions> allRecoVotes,
			List<ObservationNearBy> observationNearBy, Integer activityCount) {
		super();
		this.observation = observation;
		this.factValuePair = factValuePair;
		this.observationResource = observationResource;
		this.userGroups = userGroups;
		this.customField = customField;
		this.layerInfo = layerInfo;
		this.esLayerInfo = esLayerInfo;
		this.recoIbp = recoIbp;
		this.flag = flag;
		this.tags = tags;
		this.fetaured = fetaured;
		this.authorInfo = authorInfo;
		this.authorScore = authorScore;
		this.allRecoVotes = allRecoVotes;
		this.observationNearBy = observationNearBy;
		this.activityCount = activityCount;
	}

	public Observation getObservation() {
		return observation;
	}

	public void setObservation(Observation observation) {
		this.observation = observation;
	}

	public List<FactValuePair> getFactValuePair() {
		return factValuePair;
	}

	public void setFactValuePair(List<FactValuePair> factValuePair) {
		this.factValuePair = factValuePair;
	}

	public List<ObservationResourceUser> getObservationResource() {
		return observationResource;
	}

	public void setObservationResource(List<ObservationResourceUser> observationResource) {
		this.observationResource = observationResource;
	}

	public List<UserGroupIbp> getUserGroups() {
		return userGroups;
	}

	public void setUserGroups(List<UserGroupIbp> userGroups) {
		this.userGroups = userGroups;
	}

	public List<CustomFieldObservationData> getCustomField() {
		return customField;
	}

	public void setCustomField(List<CustomFieldObservationData> customField) {
		this.customField = customField;
	}

	public ObservationLocationInfo getLayerInfo() {
		return layerInfo;
	}

	public void setLayerInfo(ObservationLocationInfo layerInfo) {
		this.layerInfo = layerInfo;
	}

	public ObservationInfo getEsLayerInfo() {
		return esLayerInfo;
	}

	public void setEsLayerInfo(ObservationInfo esLayerInfo) {
		this.esLayerInfo = esLayerInfo;
	}

	public RecoIbp getRecoIbp() {
		return recoIbp;
	}

	public void setRecoIbp(RecoIbp recoIbp) {
		this.recoIbp = recoIbp;
	}

	public List<FlagShow> getFlag() {
		return flag;
	}

	public void setFlag(List<FlagShow> flag) {
		this.flag = flag;
	}

	public List<Tags> getTags() {
		return tags;
	}

	public void setTags(List<Tags> tags) {
		this.tags = tags;
	}

	public List<Featured> getFetaured() {
		return fetaured;
	}

	public void setFetaured(List<Featured> fetaured) {
		this.fetaured = fetaured;
	}

	public UserIbp getAuthorInfo() {
		return authorInfo;
	}

	public void setAuthorInfo(UserIbp authorInfo) {
		this.authorInfo = authorInfo;
	}

	public Map<String, String> getAuthorScore() {
		return authorScore;
	}

	public void setAuthorScore(Map<String, String> authorScore) {
		this.authorScore = authorScore;
	}

	public List<AllRecoSugguestions> getAllRecoVotes() {
		return allRecoVotes;
	}

	public void setAllRecoVotes(List<AllRecoSugguestions> allRecoVotes) {
		this.allRecoVotes = allRecoVotes;
	}

	public List<ObservationNearBy> getObservationNearBy() {
		return observationNearBy;
	}

	public void setObservationNearBy(List<ObservationNearBy> observationNearBy) {
		this.observationNearBy = observationNearBy;
	}

	public Integer getActivityCount() {
		return activityCount;
	}

	public void setActivityCount(Integer activityCount) {
		this.activityCount = activityCount;
	}

}
