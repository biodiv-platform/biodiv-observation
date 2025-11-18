package com.strandls.observation.service.Impl;

import javax.inject.Inject;

import com.strandls.observation.service.RecommendationService;

/**
 * @author Arun
 *
 */

public class RecoCleanupThread implements Runnable {

	@Inject
	private RecommendationService recoService;

	@Override
	public void run() {
		recoService.recoCleanUp();

	}

}
