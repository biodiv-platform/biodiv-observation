/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.util.List;

import com.google.inject.Inject;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.FactValuePair;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationShowService;

import kong.unirest.GenericType;
import kong.unirest.Unirest;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationShowServiceImpl implements ObservationShowService {

	@Inject
	private ObservationDAO observationDAOImpl;

	@Override
	public ShowData findById(String id) {
		Observation observation = observationDAOImpl.findById(Long.parseLong(id));
		
		List<FactValuePair> response = Unirest.get("http://localhost:8080/traitsModule/api/v1/factservice/{obvId}")
									.routeParam("obvId", id)
									.asObject(new GenericType<List<FactValuePair>>() {})
									.getBody();
		ShowData data = new ShowData(observation,response);
		return data;
	}

}
