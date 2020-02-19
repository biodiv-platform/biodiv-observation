/**
 * 
 */
package com.strandls.observation;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.naksha.controller.LayerServiceApi;
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
@Singleton
public class SwaggerFilter implements Filter {

	@Inject
	public TraitsServiceApi traitService;

	@Inject
	public ResourceServicesApi resourceService;

	@Inject
	public TaxonomyServicesApi taxonomyService;

	@Inject
	public UserGroupSerivceApi userGroupService;

	@Inject
	public CustomFieldServiceApi cfService;

	@Inject
	public LayerServiceApi layerService;

	@Inject
	public EsServicesApi esService;

	@Inject
	public UtilityServiceApi utilityService;

	@Inject
	public UserServiceApi userService;

	@Inject
	public ActivitySerivceApi activityService;

	/**
	 * 
	 */
	public SwaggerFilter() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void destroy() {
		// TODO Auto-generated method stub
	}

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		// TODO Auto-generated method stub

	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		HttpServletRequest request2 = (HttpServletRequest) request;

		String header = request2.getHeader(HttpHeaders.AUTHORIZATION);

		traitService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		resourceService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		taxonomyService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		userGroupService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		layerService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		esService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		utilityService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		userService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		activityService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);
		cfService.getApiClient().addDefaultHeader(HttpHeaders.AUTHORIZATION, header);

		chain.doFilter(request2, response);
	}

}
