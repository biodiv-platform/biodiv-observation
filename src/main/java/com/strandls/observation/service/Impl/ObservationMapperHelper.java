/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;

import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.resource.pojo.Resource;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationMapperHelper {

	public Observation createObservationMapping(ObservationCreate observationData) {

		Observation observation = new Observation();
		observation.setAuthorId(null);// author id remaining
		observation.setCreatedOn(observationData.getCreatedOn());
		observation.setGroupId(observationData.getsGroup());
		observation.setLatitude(observationData.getLatitude());
		observation.setLongitude(observationData.getLongitude());
		observation.setNotes(observationData.getNotes());
		observation.setFromDate(observationData.getFromDate());
		observation.setPlaceName(observationData.getObservedAt()); // place name given by user
		observation.setRating(null);// what to insert
		observation.setReverseGeocodedName(observationData.getReverseGeocoded()); // google reversed name for the lat
																					// and long
		observation.setFlagCount(0);// during creation it should be 0
		observation.setGeoPrivacy(observationData.getHidePreciseLocation());
		observation.setHabitatId(null);// has to Depricate , default to all
		observation.setIsDeleted(false);
		observation.setLastRevised(observationData.getCreatedOn());// initially same as date of creation of object later
																	// when updated
		observation.setLocationAccuracy(null); // what to insert
		observation.setVisitCount(0L); // updateble field
		observation.setSearchText(null); // it is not used as of now , maybe in future

//		needs to be calculated at runtime
//		if (observationData.getRecoVoteId() != null)
//			observation.setMaxVotedRecoId(null);
//		else
//			observation.setMaxVotedRecoId(null);// needs to calculate using recoid and name, 

		observation.setAgreeTerms(true);
		observation.setIsChecklist(false);// false for nrml case only used in DATATABLE
		observation.setIsShowable(true);
		observation.setSourceId(null);// observation id in nrml case, used only in GBIF
		observation.setToDate(observationData.getToDate());
		observation.setTopology(null);// conversion formula postgis
		observation.setChecklistAnnotations(null);// from data set
		observation.setFeatureCount(0);// update field initially 0, used only after its attached and featured to a
										// group
		observation.setIsLocked(false);// update field , initially false
		observation.setLicenseId(822L);// default 822
		observation.setLanguageId(observationData.getLanguageId());
		observation.setLocationScale(observationData.getLocationScale()); // 5 options

		observation.setAccessRights(null);// null for nrml case only used in GBIF
		observation.setCatalogNumber(null);// null for nrml case only used in GBIF
		observation.setDatasetId(null);// null for nrml case only used in GBIF
		observation.setExternalDatasetKey(null);// null for nrml case only used in GBIF
		observation.setExternalId(null);// null for nrml case only used in GBIF
		observation.setExternalUrl(null);// null for nrml case only used in GBIF
		observation.setInformationWithheld(null);// null for nrml case only used in GBIF
		observation.setLastCrawled(null);// null for nrml case only used in GBIF
		observation.setLastInterpreted(null);// null for nrml case only used in GBIF
		observation.setOriginalAuthor(null);// null for nrml case only used in GBIF
		observation.setPublishingCountry(null);// from IP address
		observation.setViaCode(null);// null for nrml case only used in GBIF
		observation.setViaId(null);// null for nrml case only used in GBIF

		observation.setReprImageId(null);
		observation.setProtocol(observationData.getProtocol());
		observation.setBasisOfRecord(observationData.getBasisOfRecords());
		observation.setNoOfImages(null);
		observation.setNoOfAudio(null);
		observation.setNoOfVideos(null);

		if (observationData.getHelpIdentified() == true)
			observation.setNoOfIdentifications(0);// initailly 0-1 but can increase with the no of reco vote
		else
			observation.setNoOfIdentifications(1);

		observation.setDataTableId(null);//
		observation.setDateAccuracy(observationData.getDateAccuracy());

		return observation;

	}

	public RecoCreate createRecoMapping(ObservationCreate observationData) {

		RecoCreate recoCreate = new RecoCreate();
		recoCreate.setConfidence(observationData.getConfidence());
		recoCreate.setRecoComment(observationData.getRecoComment());
		recoCreate.setCommonName(observationData.getCommonName());
		recoCreate.setCommonNameId(observationData.getCommonNameId());
		recoCreate.setScientificName(observationData.getScientificName());
		recoCreate.setScientificNameId(observationData.getScientificNameId());

		return recoCreate;

	}

	public List<Resource> createResourceMapping(ObservationCreate observationData) {
		List<Resource> resources = new ArrayList<Resource>();
		for (Entry<String, String> entry : observationData.getResources().entrySet()) {
			Resource resource = new Resource();
			resource.setVersion(0L);
			resource.setDescription(null);
			resource.setFileName(entry.getKey());
			resource.setMimeType(null);
			if (entry.getValue().startsWith("image"))
				resource.setType("IMAGE");
			else if (entry.getValue().startsWith("audio"))
				resource.setType("AUDIO");
			else if (entry.getValue().startsWith("video"))
				resource.setType("VIDEO");
			resource.setUrl(null);
			resource.setRating(null);
			resource.setUploadTime(new Date());
			resource.setId(1426L);
			resource.setContext(null);
			resource.setLanguageId(205L);
			resource.setAccessRights(null);
			resource.setAnnotations(null);
			resource.setGbifId(null);
			resource.setLicenseId(822L);

			resources.add(resource);
		}
		return resources;
	}

}
