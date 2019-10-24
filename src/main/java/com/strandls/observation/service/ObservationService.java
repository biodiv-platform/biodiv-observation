/**
 * 
 */
package com.strandls.observation.service;

import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ShowData;

/**
 * @author Abhishek Rudra
 *
 */
public interface ObservationService {

	public ShowData findById(Long id);

	public void createObservation(ObservationCreate observationData);

}
