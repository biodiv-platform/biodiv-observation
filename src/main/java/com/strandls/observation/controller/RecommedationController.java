package com.strandls.observation.controller;

import java.util.List;

import org.pac4j.core.profile.CommonProfile;

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
import com.strandls.observation.service.Impl.RecoRecalcThread;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;

/**
 * @author Abhishek Rudra
 *
 */

@Tag(name = "Recommendation Services")
@Path(ApiConstants.V1 + ApiConstants.RECO)
public class RecommedationController {

	@Inject
	private RecommendationService recoService;

	@Inject
	private ObservationMapperHelper observaitonHelper;

	@Inject
	private RecoRecalcThread recoRecalcThread;

	@GET
	@Path(ApiConstants.RECOVOTE + ApiConstants.IBP + "/{recoVoteId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Find RecommendationVote by ID", description = "Returns the recommendation vote", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = RecoIbp.class))),
			@ApiResponse(responseCode = "404", description = "Recommendation Vote not found", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Invalid ID", content = @Content(schema = @Schema(implementation = String.class))) })

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
	@Operation(summary = "Create Reco Vote for an observation", description = "Returns the RecoVote", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = RecoShow.class))),
			@ApiResponse(responseCode = "400", description = "Unable to make a database transaction", content = @Content(schema = @Schema(implementation = String.class))) })

	public Response createRecoVote(@Context HttpServletRequest request,
			@PathParam("observationId") String observaitonId,
			@Parameter(description = "recoData", required = true) RecoData recoData) {
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
	@Operation(summary = "Removes a reco Vote", description = "Return the new RecoVote", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = RecoShow.class))),
			@ApiResponse(responseCode = "400", description = "Unable to remove the RecoVote", content = @Content(schema = @Schema(implementation = String.class))) })

	public Response RemoveRecoVote(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@Parameter(description = "recoSet", required = true) RecoSet recoSet) {
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
	@Operation(summary = "Agrees on a recoVote", description = "Returns the New maxVotedReco Details", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = RecoShow.class))),
			@ApiResponse(responseCode = "400", description = "Unable to create a recoVote", content = @Content(schema = @Schema(implementation = String.class))) })

	public Response agree(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@Parameter(description = "recoSet", required = true) RecoSet recoSet) {
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
	@Operation(summary = "Update the Canonical field of Recommendation", description = "Updates the Canonical Field with the help of Name parser", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Long.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Feature Not operable right now", content = @Content(schema = @Schema(implementation = String.class))) })

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
	@Operation(summary = "Validates an Observation", description = "Returns the maxVotedReco", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = RecoShow.class))),
			@ApiResponse(responseCode = "400", description = "Unable to lock an Observation", content = @Content(schema = @Schema(implementation = String.class))) })

	public Response validateReco(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@Parameter(description = "recoSet", required = true) RecoSet recoSet) {
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
	@Operation(summary = "Unlocks an Observation", description = "Returns the new MaxVotedReco", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = RecoShow.class))),
			@ApiResponse(responseCode = "400", description = "Unable to unlock an observation", content = @Content(schema = @Schema(implementation = String.class))) })

	public Response unlockReco(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@Parameter(description = "recoSet", required = true) RecoSet recoSet) {

		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long obvId = Long.parseLong(observationId);
			RecoShow result = recoService.unlockReco(request, profile, obvId, userId, recoSet);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE)
						.entity("Observation is Not Locked or User don't have permission").build();

			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.RECALCULATE + ApiConstants.RECOVOTE)
	@Produces(MediaType.TEXT_PLAIN)
	@Operation(summary = "Recalculate Reco Vote Count", description = "Starts a process to recalculate recommendation vote counts", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to start recalculation", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response reCalculateRecoVoteCount() {
		try {
			Thread thread = new Thread(recoRecalcThread);
			thread.start();
			return Response.status(Status.OK).entity("ReCalculation has started").build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@GET
	@Path(ApiConstants.RECOVOTE + ApiConstants.CLEANUP)
	@Produces(MediaType.TEXT_PLAIN)
	@Operation(summary = "Clean up Reco Vote data", description = "Initiates a cleanup process for recommendation votes", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to start cleanup", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response cleanRecoVote() {
		try {
			recoService.recoCleanUp();
			return Response.status(Status.OK).entity("started").build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).build();
		}

	}

}