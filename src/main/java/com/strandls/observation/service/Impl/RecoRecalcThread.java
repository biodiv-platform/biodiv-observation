/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.strandls.observation.service.RecommendationService;

import jakarta.inject.Inject;

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
