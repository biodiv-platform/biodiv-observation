/**
 * 
 */
package com.strandls.observation.pojo;

import java.io.Serializable;
import java.util.List;

import com.strandls.esmodule.pojo.ObservationInfo;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.pojo.Flag;
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
	private ObservationLocationInfo layerInfo;
	private ObservationInfo esLayerInfo;
	private RecoIbp recoIbp;
	private Flag flag;
	private List<Tags> tags;
	private List<Featured> fetaured;
	private UserIbp authorInfo;
	private List<AllRecoSugguestions> allRecoVotes;

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
	 * @param layerInfo
	 * @param esLayerInfo
	 * @param recoIbp
	 * @param flag
	 * @param tags
	 * @param fetaured
	 * @param authorInfo
	 * @param allRecoVotes
	 */
	public ShowData(Observation observation, List<FactValuePair> factValuePair,
			List<ObservationResourceUser> observationResource, List<UserGroupIbp> userGroups,
			ObservationLocationInfo layerInfo, ObservationInfo esLayerInfo, RecoIbp recoIbp, Flag flag, List<Tags> tags,
			List<Featured> fetaured, UserIbp authorInfo, List<AllRecoSugguestions> allRecoVotes) {
		super();
		this.observation = observation;
		this.factValuePair = factValuePair;
		this.observationResource = observationResource;
		this.userGroups = userGroups;
		this.layerInfo = layerInfo;
		this.esLayerInfo = esLayerInfo;
		this.recoIbp = recoIbp;
		this.flag = flag;
		this.tags = tags;
		this.fetaured = fetaured;
		this.authorInfo = authorInfo;
		this.allRecoVotes = allRecoVotes;
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

	public Flag getFlag() {
		return flag;
	}

	public void setFlag(Flag flag) {
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

	public List<AllRecoSugguestions> getAllRecoVotes() {
		return allRecoVotes;
	}

	public void setAllRecoVotes(List<AllRecoSugguestions> allRecoVotes) {
		this.allRecoVotes = allRecoVotes;
	}

}
