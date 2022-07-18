package com.strandls.observation.es.util;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.strandls.user.pojo.UserIbp;

public class ObservationListElasticResourceMapping extends ObservationListElasticMapping {
	private List<Observation_resource> observationResource;

	@JsonProperty(value = "observation_resource")
	private void unpackObservationResource(List<Observation_resource> observationResources) {
		observationResource = observationResources;
	}

	public ObservationListElasticResourceMapping() {
		super();

	}

	public ObservationListElasticResourceMapping(Long observationId, String placeName, String noOfIdentification,
			String createdOn, String lastRevised, String reverseGeocodedName, Long speciesGroupId, String speciesGroup,
			Long noOfImages, Long noOfAudios, Long noOfVideos, String reprImageUrl, Boolean isLocked,
			String locationScale, Double latitude, Double longitude, String dateAccuracy, String fromDate,
			String toDate, String observedInMonth, Boolean geoPrivacy, String datasetTitle, String dataTableTitle,
			LocationInformation locationInformation, Integer recoVoteCount, UserIbp user, List<Facts> facts,
			List<Flags> flags, Max_voted_reco maxVotedReco, List<All_reco_vote> allRecoVotes,
			List<User_group_observations> userGroup, List<Custom_fields> customFields, List<Tags> tags,
			Boolean containsMedia, String uploadProtocol, Integer flagCount, String organismRemarks, String annotations,
			String basisOfRecord, String basisOfData) {
		super(observationId, placeName, noOfIdentification, createdOn, lastRevised, reverseGeocodedName, speciesGroupId,
				speciesGroup, noOfImages, noOfAudios, noOfVideos, reprImageUrl, isLocked, locationScale, latitude,
				longitude, dateAccuracy, fromDate, toDate, observedInMonth, geoPrivacy, datasetTitle, dataTableTitle,
				locationInformation, recoVoteCount, user, facts, flags, maxVotedReco, allRecoVotes, userGroup,
				customFields, tags, containsMedia, uploadProtocol, flagCount, organismRemarks, annotations,
				basisOfRecord, basisOfData);

	}

	public ObservationListElasticResourceMapping(List<Observation_resource> observationResource) {
		super();
		this.observationResource = observationResource;
	}

	public List<Observation_resource> getObservationResource() {
		return observationResource;
	}

}
