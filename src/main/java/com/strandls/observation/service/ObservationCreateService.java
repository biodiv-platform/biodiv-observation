package com.strandls.observation.service;

import com.strandls.observation.pojo.ObservationCreate;

import jakarta.servlet.http.HttpServletRequest;

public interface ObservationCreateService {

	public Long createObservation(HttpServletRequest request, ObservationCreate observationData, Boolean updateEs);

}
