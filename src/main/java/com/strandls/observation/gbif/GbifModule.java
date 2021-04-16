package com.strandls.observation.gbif;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;

public class GbifModule extends AbstractModule {
	@Override
	protected void configure() {
		bind(GbifObservationService.class).in(Scopes.SINGLETON);
	}
}
