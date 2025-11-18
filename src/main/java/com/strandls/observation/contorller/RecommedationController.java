/**
 * 
 */
package com.strandls.observation.contorller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.pac4j.core.profile.CommonProfile;

import javax.inject.Inject;

import com.strandls.authentication_utility.filter.ValidateUser;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoData;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.RecoShow;
import com.strandls.observation.service.RecommendationService;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.observation.service.Impl.RecoCleanupThread;
import com.strandls.observation.service.Impl.RecoRecalcThread;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import net.minidev.json.JSONArray;

/**
 * @author Abhishek Rudra
 *
 */

@Api("Recommendation Services")
@Path(ApiConstants.V1 + ApiConstants.RECO)
public class RecommedationController {

	@Inject
	private RecommendationService recoService;

	@Inject
	private ObservationMapperHelper observaitonHelper;

	@Inject
	private RecoRecalcThread recoRecalcThread;

	@Inject
	private RecoCleanupThread recoCleanupThread;

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

	@POST
	@Path(ApiConstants.CREATE + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser
	@ApiOperation(value = "Create Reco Vote for a observation", notes = "Returns the RecoVote", response = RecoShow.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to make a database transaction", response = String.class) })

	public Response createRecoVote(@Context HttpServletRequest request,
			@PathParam("observationId") String observaitonId, @ApiParam(name = "recoData") RecoData recoData) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long obserId = Long.parseLong(observaitonId);
			Long userId = Long.parseLong(profile.getId());
			RecoCreate recoCreate = observaitonHelper.createRecoMapping(recoData);
			Long maxVotedReco = recoService.createRecoVote(request, userId, obserId,
					recoData.getScientificNameTaxonId(), recoCreate, false);
			RecoShow result = recoService.fetchCurrentRecoState(obserId, maxVotedReco);

			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.REMOVE + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser
	@ApiOperation(value = "Removes a reco Vote", notes = "Return the new RecoVote", response = RecoShow.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to remove the RecoVote", response = String.class) })

	public Response RemoveRecoVote(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId, @ApiParam(name = "recoSet") RecoSet recoSet) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long obvId = Long.parseLong(observationId);
			RecoShow result = recoService.removeRecoVote(request, obvId, userId, recoSet);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.AGREE + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser
	@ApiOperation(value = "Agrees on a recoVote", notes = "Returns the New maxVotedReco Details", response = RecoShow.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to create a recoVote", response = String.class) })

	public Response agree(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@ApiParam(name = "recoSet") RecoSet recoSet) {
		try {
			Long obvId = Long.parseLong(observationId);
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			RecoShow result = recoService.agreeRecoVote(request, obvId, userId, recoSet);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE).entity("Observation id Locked").build();
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
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

	@POST
	@Path(ApiConstants.VALIDATE + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser
	@ApiOperation(value = "Validates a Observation", notes = "Returns the maxVotedReco", response = RecoShow.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to lock a Observation", response = String.class) })

	public Response validateReco(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@ApiParam(name = "recoSet") RecoSet recoSet) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long obvId = Long.parseLong(observationId);
			RecoShow result = recoService.validateReco(request, profile, obvId, userId, recoSet);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE).entity("User Not allowed to validate").build();
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@PUT
	@Path(ApiConstants.UNLOCK + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser
	@ApiOperation(value = "Unlocks a Observation", notes = "Returns the new MaxVotedReco", response = RecoShow.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to unloack a observation", response = String.class) })

	public Response unlockReco(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@ApiParam(name = "recoSet") RecoSet recoSet) {

		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long obvId = Long.parseLong(observationId);
			RecoShow result = recoService.unlockReco(request, profile, obvId, userId, recoSet);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE)
						.entity("Observation is Not Locked or User dont have permission").build();

			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@ValidateUser

	@Path(ApiConstants.RECALCULATE + ApiConstants.RECOVOTE)
	@Produces(MediaType.TEXT_PLAIN)
	public Response reCalculateRecoVoteCount(@Context HttpServletRequest request) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");
			if (userRole.contains("ROLE_ADMIN")) {
				Thread thread = new Thread(recoRecalcThread);
				thread.start();
				return Response.status(Status.OK).entity("ReCalculation has started").build();
			}
			return Response.status(Status.NOT_ACCEPTABLE).entity("USER NOT ALLOWED TO PERFORM THE TASK").build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@GET
	@Path(ApiConstants.RECOVOTE + ApiConstants.CLEANUP)
	@Produces(MediaType.TEXT_PLAIN)

	public Response cleanRecoVote(@Context HttpServletRequest request) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");
			if (userRole.contains("ROLE_ADMIN")) {
				Thread thread = new Thread(recoCleanupThread);
				thread.start();
				return Response.status(Status.OK).entity("Recocleanup has started").build();
			}
			return Response.status(Status.NOT_ACCEPTABLE).entity("USER NOT ALLOWED TO PERFORM THE TASK").build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).build();
		}

	}

}
