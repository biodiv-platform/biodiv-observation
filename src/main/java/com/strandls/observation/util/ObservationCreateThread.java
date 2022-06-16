package com.strandls.observation.util;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.activity.pojo.ActivityLoggingData;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.es.util.ESCreateThread;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.service.Impl.LogActivities;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.observation.service.Impl.ObservationServiceImpl;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.Resource;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactsCreateData;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupMappingCreateData;
import com.strandls.userGroup.pojo.UserGroupObvFilterData;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.TagsMapping;
import com.strandls.utility.pojo.TagsMappingData;

public class ObservationCreateThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ObservationCreateThread.class);

	private final String requestAuthHeader;
	private final ESUpdate esUpdate;
	private final ObservationMapperHelper observationHelper;
	private final HttpServletRequest request;
	private final ObservationDAO observationDao;
	private ResourceServicesApi resourceService;
	private Observation observation;
	private final ObservationCreate observationData;
	private final Headers headers;
	private final Long userId;

	private TraitsServiceApi traitService;
	private UtilityServiceApi utilityServices;
	private UserGroupSerivceApi userGroupService;
	private final LogActivities logActivity;
	private ActivitySerivceApi activityService;
	private final ObservationServiceImpl observationImpl;

	public ObservationCreateThread(HttpServletRequest request, ESUpdate esUpdate, UserServiceApi userService,
			ObservationMapperHelper observationHelper,ObservationDAO observationDao,
			ResourceServicesApi resourceService, Observation observation, ObservationCreate observationData,
			Headers headers, Long userId, TraitsServiceApi traitService,
			UtilityServiceApi utilityServices, UserGroupSerivceApi userGroupService, LogActivities logActivity,
			ActivitySerivceApi activityService, ObservationServiceImpl observationImpl) {
		super();
		
		this.requestAuthHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		this.esUpdate = esUpdate;
		this.observationHelper = observationHelper;
		this.request = request;
		this.observationDao = observationDao;
		this.resourceService = resourceService;
		this.observation = observation;
		this.observationData = observationData;
		this.headers = headers;
		this.userId = userId;
		this.traitService = traitService;
		this.utilityServices = utilityServices;
		this.userGroupService = userGroupService;
		this.logActivity = logActivity;
		this.activityService = activityService;
		this.observationImpl = observationImpl;
	}

	public void run() {

		try {

			if (observationData.getResources() != null && !observationData.getResources().isEmpty()) {
				List<Resource> resources = observationHelper.createResourceMapping(request, userId,
						observationData.getResources());
				if (resources == null || resources.isEmpty()) {
					observationDao.delete(observation);
				}
				resourceService = headers.addResourceHeaders(resourceService, requestAuthHeader);

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
			logActivity.LogActivity(requestAuthHeader, null, observation.getId(), observation.getId(), "observation",
					null, "Observation created", null);


			if ((observationData.getFactValuePairs() != null && !observationData.getFactValuePairs().isEmpty())
					|| (observationData.getFactValueStringPairs() != null
							&& !observationData.getFactValueStringPairs().isEmpty())) {
				FactsCreateData factsCreateData = new FactsCreateData();
				factsCreateData.setFactValuePairs(observationData.getFactValuePairs());
				factsCreateData.setFactValueString(observationData.getFactValueStringPairs());
				factsCreateData.setMailData(null);
				traitService = headers.addTraitsHeaders(traitService, requestAuthHeader);
				traitService.createFacts("species.participation.Observation", String.valueOf(observation.getId()),
						factsCreateData);
			}

			if (observationData.getUserGroupId() != null && !observationData.getUserGroupId().isEmpty()) {
				UserGroupMappingCreateData userGroupData = new UserGroupMappingCreateData();

				userGroupData.setUserGroups(observationData.getUserGroupId());
				userGroupData.setMailData(null);
				userGroupData.setUgFilterData(observationImpl.getUGFilterObvData(observation));
				userGroupService = headers.addUserGroupHeader(userGroupService, requestAuthHeader);
				userGroupService.createObservationUserGroupMapping(String.valueOf(observation.getId()), userGroupData);
			}
			if (!(observationData.getTags().isEmpty())) {
				TagsMapping tagsMapping = new TagsMapping();
				tagsMapping.setObjectId(observation.getId());
				tagsMapping.setTags(observationData.getTags());
				TagsMappingData tagMappingData = new TagsMappingData();
				tagMappingData.setTagsMapping(tagsMapping);
				tagMappingData.setMailData(null);
				utilityServices = headers.addUtilityHeaders(utilityServices, requestAuthHeader);
				utilityServices.createTags("observation", tagMappingData);

			}

//		send observation create mail
			ActivityLoggingData activityLogging = new ActivityLoggingData();
			activityLogging.setRootObjectId(observation.getId());
			activityLogging.setSubRootObjectId(observation.getId());
			activityLogging.setRootObjectType("observation");
			activityLogging.setActivityType("Observation created");
			activityLogging.setMailData(observationImpl.generateMailData(observation.getId()));

			activityService = headers.addActivityHeaders(activityService, requestAuthHeader);
			activityService.sendMailCreateObservation(activityLogging);

//		----------------POST CREATE ACTIONS------------

//		----------------GEO PRIVACY CHECK-------------
			List<Observation> observationList = new ArrayList<Observation>();
			observationList.add(observation);
			observationImpl.updateGeoPrivacy(observationList);

//		---------------USER GROUP FILTER RULE----------
			UserGroupObvFilterData ugObvFilterData = new UserGroupObvFilterData();
			ugObvFilterData = observationImpl.getUGFilterObvData(observation);
			userGroupService = headers.addUserGroupHeader(userGroupService, requestAuthHeader);
			userGroupService.getFilterRule(ugObvFilterData);

			ESCreateThread esCreateThread = new ESCreateThread(esUpdate, observation.getId().toString());
			Thread thread = new Thread(esCreateThread);
			thread.start();

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

}
