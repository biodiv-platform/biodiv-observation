/**
 * 
 */
package com.strandls.observation.service.Impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.activity.controller.ActivityServiceApi;
import com.strandls.activity.pojo.ActivityLoggingData;
import com.strandls.activity.pojo.MailData;
import com.strandls.observation.Headers;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class LogActivities {

	private final Logger logger = LoggerFactory.getLogger(LogActivities.class);

	@Inject
	private ActivityServiceApi activityService;

	@Inject
	private Headers headers;

	public void LogActivity(String authHeader, String activityDescription, Long rootObjectId, Long subRootObjectId,
			String rootObjectType, Long activityId, String activityType, MailData mailData) {

		try {
			ActivityLoggingData activityLogging = new ActivityLoggingData();
			activityLogging.setActivityDescription(activityDescription);
			activityLogging.setActivityId(activityId);
			activityLogging.setActivityType(activityType);
			activityLogging.setRootObjectId(rootObjectId);
			activityLogging.setRootObjectType(rootObjectType);
			activityLogging.setSubRootObjectId(subRootObjectId);
			activityLogging.setMailData(mailData);
			activityService = headers.addActivityHeaders(activityService, authHeader);
			activityService.logActivity(activityLogging);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

}
