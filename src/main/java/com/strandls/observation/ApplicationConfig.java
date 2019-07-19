/**
 * 
 */
package com.strandls.observation;

import org.glassfish.jersey.server.ResourceConfig;


/**
 * @author Abhishek Rudra
 *
 */
public class ApplicationConfig extends ResourceConfig {

	/**
	 * 
	 */


	public ApplicationConfig() {
		packages("com.strandls.observation.controller");
		register(io.swagger.jaxrs.listing.ApiListingResource.class);
		register(io.swagger.jaxrs.listing.SwaggerSerializers.class);
	}

	
}
