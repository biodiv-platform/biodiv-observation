/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.ObservationInfo;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.resource.pojo.Resource;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.Featured;
import com.strandls.utility.pojo.Flag;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServiceImpl implements ObservationService {

	private static final Logger logger = LoggerFactory.getLogger(ObservationServiceImpl.class);

	@Inject
	private ObservationDAO observationDao;

	@Inject
	private TraitsServiceApi traitService;

	@Inject
	private ResourceServicesApi resourceService;

	@Inject
	private UserGroupSerivceApi userGroupService;

	@Inject
	private LayerServiceApi layerService;

	@Inject
	private EsServicesApi esService;

	@Inject
	private RecommendationServiceImpl recoService;

	@Inject
	private UtilityServiceApi utilityServices;

	@Inject
	private ObservationMapperHelper observationHelper;

	@Override
	public ShowData findById(Long id) {

		List<FactValuePair> facts;
		List<ObservationResourceUser> observationResource;
		List<UserGroupIbp> userGroups;
		ObservationLocationInfo layerInfo;
		ObservationInfo esLayerInfo = null;
		RecoIbp reco = null;
		Flag flag;
		List<String> tags;
		List<Featured> fetaured;
		Observation observation = observationDao.findById(id);
		if (observation != null) {
			try {
				facts = traitService.getFacts("species.participation.Observation", id.toString());
				observationResource = resourceService.getImageResource(id.toString());
				userGroups = userGroupService.getObservationUserGroup(id.toString());
				layerInfo = layerService.getLayerInfo(String.valueOf(observation.getLatitude()),
						String.valueOf(observation.getLongitude()));
				flag = utilityServices.getFlagByObservation("species.participation.Observation", id.toString());
				tags = utilityServices.getTags("observation", id.toString());
				fetaured = utilityServices.getAllFeatured("species.participation.Observation", id.toString());
				if (observation.getMaxVotedRecoId() != null) {
					reco = recoService.fetchRecoName(id, observation.getMaxVotedRecoId());
					esLayerInfo = esService.getObservationInfo("observation", "observation",
							observation.getMaxVotedRecoId().toString());
				}
				ShowData data = new ShowData(observation, facts, observationResource, userGroups, layerInfo,
						esLayerInfo, reco, flag, tags, fetaured);
				return data;
			} catch (Exception e) {
				logger.error(e.getMessage());
			}
		}
		return null;
	}

	@Override
	public void createObservation(ObservationCreate observationData) {

		try {
			Observation observation = observationHelper.createObservationMapping(observationData);
			observation = observationDao.save(observation);
			RecoCreate recoCreate = observationHelper.createRecoMapping(observationData);
			Long maxVotedReco = recoService.createReco(observation.getId(), recoCreate);

			List<Resource> resources = observationHelper.createResourceMapping(observationData);
			resourceService.createResource("OBSERVATION", String.valueOf(observation.getId()), resources);

			traitService.createFacts("species.participation.Observation", String.valueOf(observation.getId()),
					observationData.getFacts());

			userGroupService.createObservationUserGroupMapping(String.valueOf(observation.getId()),
					observationData.getUserGroupId());
			utilityServices.createTags("observation", observationData.getTags());

//			update observaiton object
			
			
			observation.setMaxVotedRecoId(maxVotedReco);
			Integer noOfImages = 0;
			Integer noOfAudio = 0;
			Integer noOfVideo = 0;

			for (Resource res : resources) {
				if (res.getType().equals("AUDIO"))
					noOfAudio++;
				else if (res.getType().equals("IMAGE"))
					noOfImages++;
				else if (res.getType().equals("VIDEO"))
					noOfVideo++;
			}
			observation.setNoOfAudio(noOfAudio);
			observation.setNoOfImages(noOfImages);
			observation.setNoOfVideos(noOfVideo);

			observationDao.update(observation);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
