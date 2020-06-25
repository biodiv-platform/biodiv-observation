/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;
import com.strandls.observation.service.ObservationListService;
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
		bind(ObservationListService.class).to(ObservationListServiceImpl.class).in(Scopes.SINGLETON);
		bind(ObservationMapperHelper.class).in(Scopes.SINGLETON);
		bind(LogActivities.class).in(Scopes.SINGLETON);
		bind(UserGroupPostingFilterThread.class).in(Scopes.SINGLETON);
		bind(UserGroupUnPostingFilterThread.class).in(Scopes.SINGLETON);
		bind(GeoPrivacyBulkThread.class).in(Scopes.SINGLETON);
		bind(MailMetaDataConverter.class).in(Scopes.SINGLETON);
	}
}
