package com.strandls.observation.pojo;

import java.util.List;
import java.util.Map;

import com.strandls.dataTable.pojo.DataTableWkt;
import com.strandls.esmodule.pojo.ObservationInfo;
import com.strandls.esmodule.pojo.ObservationNearBy;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.resource.pojo.ResourceData;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.pojo.CustomFieldObservationData;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.pojo.FlagShow;
import com.strandls.utility.pojo.Tags;

public class ExternalShowData extends ShowData {
	private String dataSource;
	private String externalGbifReferenceLink;
	private String externalOriginalReferenceLink;
	private String annotations;

	public ExternalShowData() {
		super();
	}

	/*
	 * public ExternalShowData(Observation observation, List<FactValuePair>
	 * factValuePair, List<ResourceData> observationResource, List<UserGroupIbp>
	 * userGroups, List<CustomFieldObservationData> customField,
	 * ObservationLocationInfo layerInfo, ObservationInfo esLayerInfo, RecoIbp
	 * recoIbp, List<FlagShow> flag, List<Tags> tags, List<Featured> fetaured,
	 * UserIbp authorInfo, Map<String, String> authorScore,
	 * List<AllRecoSugguestions> allRecoVotes, List<ObservationNearBy>
	 * observationNearBy, Integer activityCount) { super(observation, factValuePair,
	 * observationResource, userGroups, customField, layerInfo, esLayerInfo,
	 * recoIbp, flag, tags, fetaured, authorInfo, authorScore, allRecoVotes,
	 * observationNearBy, dataTable, checkListAnnotation, activityCount); }
	 */

	public ExternalShowData(Observation observation, List<FactValuePair> factValuePair,
			List<ResourceData> observationResource, List<UserGroupIbp> userGroups,
			List<CustomFieldObservationData> customField, ObservationLocationInfo layerInfo,
			ObservationInfo esLayerInfo, RecoIbp recoIbp, List<FlagShow> flag, List<Tags> tags, List<Featured> fetaured,
			UserIbp authorInfo, Map<String, String> authorScore, List<AllRecoSugguestions> allRecoVotes,
			List<ObservationNearBy> observationNearBy, DataTableWkt dataTable, Map<String, Object> checkListAnnotation,
			Integer activityCount) {
		super(observation, factValuePair, observationResource, userGroups, customField, layerInfo, esLayerInfo, recoIbp,
				flag, tags, fetaured, authorInfo, authorScore, allRecoVotes, observationNearBy, dataTable,
				checkListAnnotation, activityCount);
	}

	public ExternalShowData(String dataSource, String externalGbifReferenceLink, String externalOriginalReferenceLink,
			String annotations) {
		super();
		this.dataSource = dataSource;
		this.externalGbifReferenceLink = externalGbifReferenceLink;
		this.externalOriginalReferenceLink = externalOriginalReferenceLink;
		this.annotations = annotations;
	}

	public String getDataSource() {
		return dataSource;
	}

	public void setDataSource(String dataSource) {
		this.dataSource = dataSource;
	}

	public String getExternalGbifReferenceLink() {
		return externalGbifReferenceLink;
	}

	public void setExternalGbifReferenceLink(String externalGbifReferenceLink) {
		this.externalGbifReferenceLink = externalGbifReferenceLink;
	}

	public String getExternalOriginalReferenceLink() {
		return externalOriginalReferenceLink;
	}

	public void setExternalOriginalReferenceLink(String externalOriginalReferenceLink) {
		this.externalOriginalReferenceLink = externalOriginalReferenceLink;
	}

	public String getAnnotations() {
		return annotations;
	}

	public void setAnnotations(String annotations) {
		this.annotations = annotations;
	}

}
