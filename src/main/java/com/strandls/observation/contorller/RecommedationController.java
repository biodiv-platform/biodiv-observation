/**
 * 
 */
package com.strandls.observation.contorller;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.google.inject.Inject;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.service.RecommendationService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * @author Abhishek Rudra
 *
 */

@Api("Recommendation Services")
@Path(ApiConstants.V1 + ApiConstants.RECO)
public class RecommedationController {

	@Inject
	private RecommendationService recoService;

	@GET
	@Path(ApiConstants.RECOVOTE + ApiConstants.IBP + "/{recoVoteId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Find RecommendationVote by ID", notes = "Returns the recommendation vote", response = RecoIbp.class)
	@ApiResponses(value = {
			@ApiResponse(code = 404, message = "Recommendation Vote not found", response = String.class),
			@ApiResponse(code = 400, message = "Invalid ID", response = String.class) })

	public Response getRecoVote(@PathParam("recoVoteId") String recoVoteId) {
		try {
			Long id = Long.parseLong(recoVoteId);
			RecoIbp recoIbp = recoService.fetchRecoVote(id);
			return Response.status(Status.OK).entity(recoIbp).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).build();
		}
	}

	@PUT
	@Path(ApiConstants.CANONICAL)
	@Produces(MediaType.TEXT_PLAIN)

	@ApiOperation(value = "Update the Canonical field of Recommendation", notes = "Updates the Canonical Field with the help of Name parser", response = Long.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = " Feature Not operable right now", response = String.class) })

	public Response getCanonicalUpdated() {
		try {
			List<Long> result = recoService.updateCanonicalName();
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).build();
		}
	}

}
