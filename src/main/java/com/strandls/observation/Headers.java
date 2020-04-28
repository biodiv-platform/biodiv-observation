/**
 * 
 */
package com.strandls.observation;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.file.api.UploadApi;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.controller.CustomFieldServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.utility.controller.UtilityServiceApi;

/**
 * @author Abhishek Rudra
 *
 */
public class Headers {

	public ActivitySerivceApi addActivityHeaders(ActivitySerivceApi activityService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		activityService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return activityService;
	}

	public TraitsServiceApi addTraitsHeaders(TraitsServiceApi traitService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		traitService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return traitService;
	}

	public ResourceServicesApi addResourceHeaders(ResourceServicesApi resourceService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		resourceService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return resourceService;
	}

	public TaxonomyServicesApi addTaxonomyHeader(TaxonomyServicesApi taxonomyService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		taxonomyService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return taxonomyService;
	}

	public UserGroupSerivceApi addUserGroupHeader(UserGroupSerivceApi ugService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		ugService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return ugService;
	}

	public UtilityServiceApi addUtilityHeaders(UtilityServiceApi utilityServices, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		utilityServices.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return utilityServices;
	}

	public UserServiceApi addUserHeaders(UserServiceApi userService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		userService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return userService;
	}

	public UploadApi addFileUploadHeader(UploadApi uploadService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		uploadService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return uploadService;
	}

	public CustomFieldServiceApi addCFHeaders(CustomFieldServiceApi cfService, HttpServletRequest request) {
		String authHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		cfService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return cfService;
	}

}
