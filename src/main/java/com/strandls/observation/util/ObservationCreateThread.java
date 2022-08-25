package com.strandls.observation.util;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.activity.pojo.ActivityLoggingData;
import com.strandls.integrator.controllers.IntergratorServicesApi;
import com.strandls.integrator.pojo.CheckFilterRule;
import com.strandls.observation.Headers;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.service.Impl.LogActivities;
import com.strandls.observation.service.Impl.ObservationServiceImpl;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactsCreateData;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupMappingCreateData;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.TagsMapping;
import com.strandls.utility.pojo.TagsMappingData;

public class ObservationCreateThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ObservationCreateThread.class);

	private final String requestAuthHeader;
	private final ESUpdate esUpdate;
	private Observation observation;
	private final ObservationCreate observationData;
	private final Headers headers;
	private final Boolean updateEs;

	private TraitsServiceApi traitService;
	private UtilityServiceApi utilityServices;
	private UserGroupSerivceApi userGroupService;
	private IntergratorServicesApi intergratorService;
	private final LogActivities logActivity;
	private ActivitySerivceApi activityService;
	private final ObservationServiceImpl observationImpl;

	public ObservationCreateThread(HttpServletRequest request, ESUpdate esUpdate, Observation observation,
			ObservationCreate observationData, Headers headers, TraitsServiceApi traitService,
			UtilityServiceApi utilityServices, UserGroupSerivceApi userGroupService, LogActivities logActivity,
			ActivitySerivceApi activityService, ObservationServiceImpl observationImpl,
			IntergratorServicesApi intergratorService, Boolean updateEs) {
		super();

		this.requestAuthHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		this.esUpdate = esUpdate;
		this.observation = observation;
		this.observationData = observationData;
		this.headers = headers;
		this.traitService = traitService;
		this.utilityServices = utilityServices;
		this.userGroupService = userGroupService;
		this.logActivity = logActivity;
		this.activityService = activityService;
		this.observationImpl = observationImpl;
		this.intergratorService = intergratorService;
		this.updateEs = updateEs;
	}

	public void run() {

		try {

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
				//filter usergroup by rule eligility
				CheckFilterRule checkFilterRule  = new CheckFilterRule();
				checkFilterRule.setUserGroupId(observationData.getUserGroupId());
				checkFilterRule.setUgObvFilterData(observationImpl.getUGFilterObvData(observation));
				intergratorService = headers.addIntergratorHeader(intergratorService,
						requestAuthHeader);
				
				userGroupData.setUserGroups(intergratorService.checkUserGroupEligiblity(checkFilterRule));
				userGroupData.setMailData(null);
				userGroupData.setUgFilterData(observationImpl.getFilterObvData(observation));
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
			List<Observation> observationList = new ArrayList<>();
			observationList.add(observation);
			observationImpl.updateGeoPrivacy(observationList);

//		---------------USER GROUP FILTER RULE----------
			intergratorService = headers.addIntergratorHeader(intergratorService, requestAuthHeader);
			intergratorService.getFilterRule(observationImpl.getUGFilterObvData(observation));

//		----------------ES UPDATE---------------------
			if (Boolean.TRUE.equals(updateEs)) {
				esUpdate.pushToElastic(observation.getId().toString());
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

}
