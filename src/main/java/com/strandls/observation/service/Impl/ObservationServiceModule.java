/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.service.RecommendationService;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServiceModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(ObservationService.class).to(ObservationServiceImpl.class).in(Scopes.SINGLETON);
		bind(RecommendationService.class).to(RecommendationServiceImpl.class).in(Scopes.SINGLETON);
		bind(ObservationMapperHelper.class).in(Scopes.SINGLETON);
	}
}
