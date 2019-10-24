/**
 * 
 */
package com.strandls.observation.contorller;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.google.inject.Inject;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.util.ObservationInputException;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * @author Abhishek Rudra
 *
 */

@Api("Observation Show")
@Path(ApiConstants.V1 + ApiConstants.OBSERVATION)
public class ObservationController {

	@Inject
	private ObservationService observationSerices;

	@GET
	@ApiOperation(value = "Dummy API Ping", notes = "Checks validity of war file at deployment", response = String.class)
	@Path(ApiConstants.PING)
	@Produces(MediaType.TEXT_PLAIN)
	public String ping() {
		return "pong Observation";
	}

	@GET
	@Path(ApiConstants.SHOW + "/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Find Observation by ID", notes = "Returns the complete Observation with all the specificaiton", response = ShowData.class)
	@ApiResponses(value = { @ApiResponse(code = 404, message = "Observation not found", response = String.class),
			@ApiResponse(code = 400, message = "Invalid ID", response = String.class) })
	public Response show(
			@ApiParam(value = "ID of Show that needs to be fetched", required = true) @PathParam("observationId") String id) {

		Long obvId;
		try {
			obvId = Long.parseLong(id);
			ShowData show = observationSerices.findById(obvId);

			if (show.getObservation() != null || show.getFactValuePair() != null)
				return Response.status(Status.OK).entity(show).build();
			else
				return Response.status(Status.NOT_FOUND).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).build();
		}

	}

	@POST
	@Path(ApiConstants.CREATE)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	public Response createObservation(ObservationCreate observationData) {
		try {
			if (observationData.getObservedOn() == null)
				throw new ObservationInputException("Observation Date Cannot be BLANK");
			if (observationData.getLatitude() == null || observationData.getLongitude() == null)
				throw new ObservationInputException("Observation LATITUDE/LONGITUDE MISSING");
			if (observationData.getObservedAt() == null)
				throw new ObservationInputException("Observation LOCATION cannot be BLANK");
			if (observationData.getsGroup() == null)
				throw new ObservationInputException("Species Group cannot be BLANK");

			return null;
		} catch (ObservationInputException e) {
			return Response.status(Status.NOT_ACCEPTABLE).entity(e.getMessage()).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).build();
		}
	}
}
