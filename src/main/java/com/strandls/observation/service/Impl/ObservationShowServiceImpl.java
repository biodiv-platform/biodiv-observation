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
import com.strandls.trait.ApiException;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationShowServiceImpl implements ObservationShowService {

	private static final Logger logger = LoggerFactory.getLogger(ObservationShowServiceImpl.class);

	@Inject
	private ObservationDAO observationDAOImpl;

	@Inject
	private TraitsServiceApi traitService;

	@Override
	public ShowData findById(Long id) {

		List<FactValuePair> response;
		Observation observation = observationDAOImpl.findById(id);

		try {
			response = traitService.getFacts(id.toString());
			ShowData data = new ShowData(observation, response);
			return data;
		} catch (ApiException e) {
			logger.error(e.getMessage());
		}

		return null;
	}

}
