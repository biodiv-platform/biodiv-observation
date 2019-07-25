/**
 * 
 */
package com.strandls.observation;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.core.Application;

import com.strandls.observation.contorller.ObservationShowController;
import com.strandls.observation.pojo.ShowData;

/**
 * @author Abhishek Rudra
 *
 */
public class ApplicationConfig extends Application {

	/**
	 * 
	 */
	@Override
	public Set<Class<?>> getClasses() {
		Set<Class<?>> resources = new HashSet<Class<?>>();

		resources.add(ObservationShowController.class);
		resources.add(ShowData.class);
        resources.add(io.swagger.jaxrs.listing.ApiListingResource.class);
        resources.add(io.swagger.jaxrs.listing.SwaggerSerializers.class);

        return resources;
	}	
}
