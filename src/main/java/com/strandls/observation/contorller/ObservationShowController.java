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
import javax.ws.rs.core.Response;

import com.google.inject.Inject;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationShowService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * @author Abhishek Rudra
 *
 */

@Api
@Path(ApiConstants.V1 + ApiConstants.SHOW)
public class ObservationShowController {

	@Inject
	private ObservationShowService observationShowSerices;

	@GET
	@ApiOperation(value = "ping", notes = "validity of war file", response = String.class)
	@Path(ApiConstants.PING)
	@Produces(MediaType.TEXT_PLAIN)
	public String ping() {
		return "pong";
	}

	@GET
	@Path("/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Find Observation by ID", notes = "Returns a pet when 0 < ID <= 10.  ID > 10 or nonintegers will simulate API error conditions", response = Observation.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Invalid ID supplied"),
			@ApiResponse(code = 404, message = "Observation not found") })
	public Response show(
			@ApiParam(value = "ID of Show that needs to be fetched", required = true)
			@PathParam("observationId") String id) {
		ShowData show = observationShowSerices.findById(id);
		return Response.ok().entity(show).build();
	}

}
