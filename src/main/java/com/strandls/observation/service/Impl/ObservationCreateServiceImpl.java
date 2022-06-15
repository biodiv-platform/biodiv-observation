package com.strandls.observation.service.Impl;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;

import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.service.ObservationCreateService;
import com.strandls.observation.util.ObservationCreateThread;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.utility.controller.UtilityServiceApi;

public class ObservationCreateServiceImpl implements ObservationCreateService {

	private final Logger logger = LoggerFactory.getLogger(ObservationServiceImpl.class);

	@Inject
	private LogActivities logActivity;

	@Inject
	private ObservationDAO observationDao;

	@Inject
	private TraitsServiceApi traitService;

	@Inject
	private ResourceServicesApi resourceService;

	@Inject
	private UserGroupSerivceApi userGroupService;

	@Inject
	private RecommendationServiceImpl recoService;

	@Inject
	private UtilityServiceApi utilityServices;

	@Inject
	private UserServiceApi userService;

	@Inject
	private ObservationMapperHelper observationHelper;

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private ActivitySerivceApi activityService;

	@Inject
	private Headers headers;

	@Override
	public Long createObservation(HttpServletRequest request, ObservationCreate observationData) {
		try {
			System.out.println("\n\n\n***** Observation Create Data: " + observationData.getResources().toString()
					+ " ***** \n\n\n");
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long maxVotedReco = null;
			Observation observation = observationHelper.createObservationMapping(userId, observationData);
			observation = observationDao.save(observation);

			if (observation != null) {
				return observation.getId();
			}

			ObservationCreateThread createThread = new ObservationCreateThread(request, esUpdate, userService,
					observationHelper, observationDao, resourceService, observation, observationData, headers,
					maxVotedReco, recoService, traitService, utilityServices, userGroupService, logActivity,
					activityService, null);
			Thread thread = new Thread(createThread);
			thread.start();

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

}
