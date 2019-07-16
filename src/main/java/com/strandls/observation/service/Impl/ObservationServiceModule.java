/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;
import com.strandls.observation.service.ObservationShowService;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServiceModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(ObservationShowService.class).to(ObservationShowServiceImpl.class).in(Scopes.SINGLETON);
	}
}
