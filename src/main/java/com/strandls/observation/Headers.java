/**
 *
 */
package com.strandls.observation;

import com.strandls.activity.controller.ActivityServiceApi;
import com.strandls.dataTable.controllers.DataTableServiceApi;
import com.strandls.file.api.UploadApi;
import com.strandls.integrator.controllers.IntegratorServicesApi;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.controller.CustomFieldServiceApi;
import com.strandls.userGroup.controller.UserGroupServiceApi;
import com.strandls.utility.controller.UtilityServiceApi;

import jakarta.ws.rs.core.HttpHeaders;

/**
 * @author Abhishek Rudra
 *
 */
public class Headers {

	public ActivityServiceApi addActivityHeaders(ActivityServiceApi activityService, String authHeader) {
		activityService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return activityService;
	}

	public TraitsServiceApi addTraitsHeaders(TraitsServiceApi traitService, String authHeader) {
		traitService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return traitService;
	}

	public DataTableServiceApi addDataTableHeaders(DataTableServiceApi dataTableService, String authHeader) {
		dataTableService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return dataTableService;
	}

	public ResourceServicesApi addResourceHeaders(ResourceServicesApi resourceService, String authHeader) {
		resourceService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return resourceService;
	}

	public TaxonomyServicesApi addTaxonomyHeader(TaxonomyServicesApi taxonomyService, String authHeader) {
		taxonomyService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return taxonomyService;
	}

	public UserGroupServiceApi addUserGroupHeader(UserGroupServiceApi ugService, String authHeader) {
		ugService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return ugService;
	}

	public IntegratorServicesApi addIntegratorHeader(IntegratorServicesApi integratorService, String authHeader) {
		integratorService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return integratorService;
	}

	public UtilityServiceApi addUtilityHeaders(UtilityServiceApi utilityServices, String authHeader) {
		utilityServices.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return utilityServices;
	}

	public UserServiceApi addUserHeaders(UserServiceApi userService, String authHeader) {
		userService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return userService;
	}

	public UploadApi addFileUploadHeader(UploadApi uploadService, String authHeader) {
		uploadService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return uploadService;
	}

	public CustomFieldServiceApi addCFHeaders(CustomFieldServiceApi cfService, String authHeader) {
		cfService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, authHeader);
		return cfService;
	}

}
