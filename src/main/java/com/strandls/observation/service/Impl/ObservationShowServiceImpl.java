/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationShowService;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.traitsModule.controllers.TraitsServiceApi;
import com.strandls.traitsModule.pojo.FactValuePair;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;

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

	@Override
	public ShowData findById(Long id) {

		List<FactValuePair> facts;
		List<ObservationResourceUser> observationResource;
		List<UserGroupIbp> userGroups;
		Observation observation = observationDao.findById(id);

		try {
			facts = traitService.getFacts(id.toString());
			observationResource = resourceService.getImageResource(id.toString());
			userGroups = userGroupService.getObservationUserGroup(id.toString());
			ShowData data = new ShowData(observation, facts, observationResource, userGroups);
			return data;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

}
