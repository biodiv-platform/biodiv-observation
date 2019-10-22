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
import com.strandls.esmodule.pojo.ObservationNearBy;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationShowService;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.User;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.Featured;
import com.strandls.utility.pojo.Flag;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationShowServiceImpl implements ObservationShowService {

	private static final Logger logger = LoggerFactory.getLogger(ObservationShowServiceImpl.class);

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
	private UserServiceApi userService;

	@Override
	public ShowData findById(Long id) {

		List<FactValuePair> facts;
		List<ObservationResourceUser> observationResource;
		List<UserGroupIbp> userGroups;
		ObservationLocationInfo layerInfo;
		ObservationInfo esLayerInfo = null;
		RecoIbp reco = null;
		User authorUser;
		Flag flag;
		List<String> tags;
		List<Featured> fetaured;
		List<ObservationNearBy> observationNearBy;
		Observation observation = observationDao.findById(id);

		try {
			facts = traitService.getFacts(id.toString());
			observationResource = resourceService.getImageResource(id.toString());
			userGroups = userGroupService.getObservationUserGroup(id.toString());
			layerInfo = layerService.getLayerInfo(String.valueOf(observation.getLatitude()),
					String.valueOf(observation.getLongitude()));
			flag = utilityServices.getFlagByObservation("species.participation.Observation", id.toString());
			tags = utilityServices.getTags("observation", id.toString());
			fetaured = utilityServices.getAllFeatured("species.participation.Observation", id.toString());
			observationNearBy = esService.getNearByObservation("observation", "observation",
					String.valueOf(observation.getLatitude()), String.valueOf(observation.getLongitude()));
			authorUser = userService.getUser(String.valueOf(observation.getAuthorId()));
			if (observation.getMaxVotedRecoId() != null) {
				reco = recoService.fetchRecoName(id, observation.getMaxVotedRecoId());
				esLayerInfo = esService.getObservationInfo("observation", "observation",
						observation.getMaxVotedRecoId().toString());
			}
			ShowData data = new ShowData(observation, facts, observationResource, userGroups, layerInfo, esLayerInfo,
					reco, flag, tags, fetaured, authorUser, observationNearBy);
			return data;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

}
