/**
 * 
 */
package com.strandls.observation;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.contorller.ObservationShowController;

import io.swagger.jaxrs.config.BeanConfig;


/**
 * @author Abhishek Rudra
 *
 */
@ApplicationPath("")
public class ApplicationConfig extends Application {

	/**
	 * 
	 */

	private static final Logger logger = LoggerFactory.getLogger(ApplicationConfig.class);

	public ApplicationConfig() {
		BeanConfig beanConfig = new BeanConfig();
		beanConfig.setVersion("1.0.0");
		beanConfig.setTitle("ObservationModule");
		beanConfig.setSchemes(new String[] { "http" });
		beanConfig.setHost("localhost:8080");
		beanConfig.setBasePath("/api");
		beanConfig.setResourcePackage("com.strandls.observation.controller");
		beanConfig.setScan(true);
		
		logger.info(beanConfig.toString());
	}

	@Override
	public Set<Class<?>> getClasses() {
		Set<Class<?>> resources = new HashSet<Class<?>>();

		resources.add(ObservationShowController.class);
		resources.add(io.swagger.jaxrs.listing.ApiListingResource.class);
		resources.add(io.swagger.jaxrs.listing.SwaggerSerializers.class);
		return resources;
	}
}
