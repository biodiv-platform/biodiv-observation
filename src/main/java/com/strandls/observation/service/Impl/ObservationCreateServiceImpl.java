package com.strandls.observation.service.Impl;

import java.util.List;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;

import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.net.HttpHeaders;
import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.authentication_utility.util.AuthUtil;
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

	@Inject
	private ObservationServiceImpl observationImpl;

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

			if (!(observationData.getHelpIdentify())) {
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

				Integer noOfImages = 0;
				Integer noOfAudio = 0;
				Integer noOfVideo = 0;

				Long reprImage = null;
				int rating = 0;
				for (Resource res : resources) {
					if (res.getType().equals("AUDIO"))
						noOfAudio++;
					else if (res.getType().equals("IMAGE")) {
						noOfImages++;
						if (reprImage == null)
							reprImage = res.getId();
						if (res.getRating() != null && res.getRating() > rating) {
							reprImage = res.getId();
							rating = res.getRating();
						}
					} else if (res.getType().equals("VIDEO"))
						noOfVideo++;
				}
				observation.setNoOfAudio(noOfAudio);
				observation.setNoOfImages(noOfImages);
				observation.setNoOfVideos(noOfVideo);
				observation.setReprImageId(reprImage);
				observation = observationDao.update(observation);
			}

			ObservationCreateThread createThread = new ObservationCreateThread(request, esUpdate, userService,
					observation, observationData, headers, traitService, utilityServices, userGroupService, logActivity,
					activityService, observationImpl, updateEs);
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
