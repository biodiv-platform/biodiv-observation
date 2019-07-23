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
import javax.ws.rs.core.Response.Status;

import com.google.inject.Inject;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationShowService;
import com.strandls.traits.ApiException;
import com.strandls.traits.controller.TraitsServiceApi;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.SwaggerDefinition;
import io.swagger.annotations.Tag;

/**
 * @author Abhishek Rudra
 *
 */

@Api("Observation Show")
@SwaggerDefinition(tags = { @Tag(name = "V1 Show", description = "Rest endpoint for Observatin Service") })
@Path(ApiConstants.V1 + ApiConstants.SHOW)
public class ObservationShowController {

	@Inject
	private ObservationShowService observationShowSerices;
	
	@Inject
	private TraitsServiceApi traitService;

	@GET
	@ApiOperation(value = "Dummy API Ping", notes = "Checks validity of war file at deployment", response = String.class)
	@Path(ApiConstants.PING)
	@Produces(MediaType.TEXT_PLAIN)
	public String ping() {
		return "pong Observation";
	}

	@GET
	@Path("/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Find Observation by ID", notes = "Returns the complete Observation with all the specificaiton", response = Observation.class)
	@ApiResponses(value = { @ApiResponse(code = 200, message = "Success"),
			@ApiResponse(code = 404, message = "Observation not found") })
	public Response show(
			@ApiParam(value = "ID of Show that needs to be fetched", required = true) @PathParam("observationId") String id) {

		ShowData show = observationShowSerices.findById(id);
		if (show.getObservation() != null || show.getFactValuePair() != null)
			return Response.status(Status.OK).entity(show).build();
		else
			return Response.status(Status.NOT_FOUND).build();
	}
	
	@GET
	@Path("/pingTrait")
	@Produces(MediaType.TEXT_PLAIN)
	public String pingTraits() throws ApiException {
		String t = traitService.ping();
		return t;
	}

}
