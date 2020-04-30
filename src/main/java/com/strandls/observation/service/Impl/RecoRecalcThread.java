/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.google.inject.Inject;
import com.strandls.observation.service.RecommendationService;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoRecalcThread implements Runnable {

	@Inject
	private RecommendationService recoService;

	@Override
	public void run() {
		recoService.recoCountRecalculate();

	}

}
