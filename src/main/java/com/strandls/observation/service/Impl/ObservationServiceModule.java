/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;
import com.strandls.observation.service.ObservationShowService;
import com.strandls.observation.service.RecommedationService;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServiceModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(ObservationShowService.class).to(ObservationShowServiceImpl.class).in(Scopes.SINGLETON);
		bind(RecommedationService.class).to(RecommendationServiceImpl.class).in(Scopes.SINGLETON);
	}
}
