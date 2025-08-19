/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.strandls.observation.service.ObservationService;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class GeoPrivacyBulkThread implements Runnable {

	@Inject
	private ObservationService service;

	@Override
	public void run() {
		service.applyGeoPrivacyObservaiton();
	}

}
