package com.strandls.observation.service.Impl;

import java.util.List;

import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.net.HttpHeaders;
import com.strandls.activity.controller.ActivityServiceApi;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.integrator.controllers.IntegratorServicesApi;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.service.ObservationCreateService;
import com.strandls.observation.util.ObservationCreateThread;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.Resource;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.userGroup.controller.UserGroupServiceApi;
import com.strandls.utility.controller.UtilityServiceApi;

import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;

public class ObservationCreateServiceImpl implements ObservationCreateService {

	private final Logger logger = LoggerFactory.getLogger(ObservationCreateServiceImpl.class);

	@Inject
	private LogActivities logActivity;

	@Inject
	private ObservationDAO observationDao;

	@Inject
	private TraitsServiceApi traitService;

	@Inject
	private ResourceServicesApi resourceService;

	@Inject
	private UserGroupServiceApi userGroupService;

	@Inject
	private RecommendationServiceImpl recoService;

	@Inject
	private UtilityServiceApi utilityServices;

	@Inject
	private ObservationMapperHelper observationHelper;

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private ActivityServiceApi activityService;

	@Inject
	private Headers headers;

	@Inject
	private ObservationServiceImpl observationImpl;

	@Inject
	private IntegratorServicesApi integratorService;

	@Override
	public Long createObservation(HttpServletRequest request, ObservationCreate observationData, Boolean updateEs) {
		try {
			System.out.println("\n\n\n***** Observation Create Data: " + observationData.getResources().toString()
					+ " ***** \n\n\n");
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long maxVotedReco = null;
			Observation observation = observationHelper.createObservationMapping(userId, observationData);
			observation = observationDao.save(observation);

			if (Boolean.FALSE.equals(observationData.getHelpIdentify())) {
				RecoCreate recoCreate = observationHelper.createRecoMapping(observationData.getRecoData());
				maxVotedReco = recoService.createRecoVote(request, userId, observation.getId(),
						observationData.getRecoData().getScientificNameTaxonId(), recoCreate, true);

				observation.setMaxVotedRecoId(maxVotedReco);
				observationDao.update(observation);
			}

			if (observationData.getResources() != null && !observationData.getResources().isEmpty()) {
				List<Resource> resources = observationHelper.createResourceMapping(request, userId,
						observationData.getResources());
				if (resources == null || resources.isEmpty()) {
					observationDao.delete(observation);
				}
				resourceService = headers.addResourceHeaders(resourceService,
						request.getHeader(HttpHeaders.AUTHORIZATION));

				resources = resourceService.createResource("OBSERVATION", String.valueOf(observation.getId()),
						resources);

				observation = observationDao
						.update(observationHelper.updateObservationResourceCount(observation, resources));
			}

			ObservationCreateThread createThread = new ObservationCreateThread(request, esUpdate, observation,
					observationData, headers, traitService, utilityServices, userGroupService, logActivity,
					activityService, observationImpl, integratorService, updateEs);
			Thread thread = new Thread(createThread);
			thread.start();

			if (observation != null) {
				return observation.getId();
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

}
