/**
 * 
 */
package com.strandls.observation.contorller;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationControllerModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(ObservationController.class).in(Scopes.SINGLETON);
		bind(RecommedationController.class).in(Scopes.SINGLETON);
	}

}
