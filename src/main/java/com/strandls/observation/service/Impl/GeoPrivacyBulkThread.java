/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.google.inject.Inject;
import com.strandls.observation.service.ObservationService;

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
