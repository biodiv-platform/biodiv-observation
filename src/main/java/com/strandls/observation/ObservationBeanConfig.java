/**
 * 
 */
package com.strandls.observation;

import io.swagger.jaxrs.config.BeanConfig;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationBeanConfig extends BeanConfig{

	/**
	 * 
	 */
	public ObservationBeanConfig() {
		super();
	}
	
	public void config() {
		setVersion("1.0");
		setTitle("ObservationModule MicroServices");
		setSchemes(new String[] { "http" });
		setHost("localhost:8080");
		setBasePath("/observationModule/api");
		setResourcePackage("com.strandls.observation");
		setPrettyPrint(true);
		setScan(true);
	}
}
