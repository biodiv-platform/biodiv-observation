package com.strandls.observation.service;

import javax.servlet.http.HttpServletRequest;

import com.strandls.observation.pojo.ObservationCreate;

public interface ObservationCreateService {

	public Long createObservation(HttpServletRequest request, ObservationCreate observationData);

}
