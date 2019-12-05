/**
 * 
 */
package com.strandls.observation.service;

import javax.servlet.http.HttpServletRequest;

import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ShowData;

/**
 * @author Abhishek Rudra
 *
 */
public interface ObservationService {

	public ShowData findById(Long id);

	public ShowData createObservation(HttpServletRequest request, ObservationCreate observationData);

	public Long updateSGroup(Long observationId, Long sGroupId);

	public Long updateMaxVotedReco(Long observationId, Long maxVotedReco);

}
