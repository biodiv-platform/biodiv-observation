/**
 * 
 */
package com.strandls.observation.contorller;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.google.inject.Inject;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationShowService;


/**
 * @author Abhishek Rudra
 *
 */

@Path(ApiConstants.V1+ApiConstants.SHOW)
public class ObservationShowController {
	
	@Inject
	private ObservationShowService observationShowSerices;
	
	@GET
	@Path(ApiConstants.PING)
	@Produces(MediaType.TEXT_PLAIN)
	public String ping() {
		return "pong";
	}
	
	@GET
	@Path("/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	public ShowData show(@PathParam("observationId") String id) {
		ShowData show = observationShowSerices.findById(id);
		return show;
	}
	
	

}
