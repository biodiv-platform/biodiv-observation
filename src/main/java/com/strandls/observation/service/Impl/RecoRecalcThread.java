/**
 * 
 */
package com.strandls.observation.service.Impl;

import javax.inject.Inject;

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
