package com.strandls.observation.util;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;

public class ObservationUtilModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(TokenGenerator.class).in(Scopes.SINGLETON);
	}
}