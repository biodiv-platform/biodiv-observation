/**
 * 
 */
package com.strandls.observation.es.util;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;

/**
 * @author Abhishek Rudra
 *
 */
public class ESUtilModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(ESUtility.class).in(Scopes.SINGLETON);
		bind(ESUpdate.class).in(Scopes.SINGLETON);
		bind(RabbitMQProducer.class).in(Scopes.SINGLETON);
		bind(RabbitMQConsumer.class).in(Scopes.SINGLETON);
		bind(ConstructESDocument.class).in(Scopes.SINGLETON);
		bind(ESCreateThread.class).in(Scopes.SINGLETON);
		bind(GbifObservationESMapper.class).in(Scopes.SINGLETON);
		bind(ESBulkUploadThread.class).in(Scopes.SINGLETON);

	}
}
