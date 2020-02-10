/**
 * 
 */
package com.strandls.observation.contorller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.pac4j.core.profile.CommonProfile;

import com.google.inject.Inject;
import com.strandls.authentication_utility.filter.ValidateUser;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ObservationUpdateData;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.service.Impl.GeoPrivacyBulkThread;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.observation.service.Impl.UserGroupFilterThread;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.pojo.Follow;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.FeaturedCreate;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.pojo.Flag;
import com.strandls.utility.pojo.FlagIbp;
import com.strandls.utility.pojo.Language;
import com.strandls.utility.pojo.Tags;
import com.strandls.utility.pojo.TagsMapping;

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

@Api("Observation Service")
@Path(ApiConstants.V1 + ApiConstants.OBSERVATION)
public class ObservationController {

	@Inject
	private ObservationService observationSerices;

	@Inject
	private GeoPrivacyBulkThread geoPrivacyThread;

	@Inject
	private ObservationMapperHelper observationHelper;

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

			if (show != null)
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

	@ValidateUser
	@ApiOperation(value = "Create a Observation", notes = "Returns the show Page of Observation", response = ShowData.class)
	@ApiResponses(value = {
			@ApiResponse(code = 404, message = "observation Cannot be created", response = String.class) })

	public Response createObservation(@Context HttpServletRequest request,
			@ApiParam(name = "observationData") ObservationCreate observationData) {
		try {
			if (observationData.getObservedOn() == null)
				throw new ObservationInputException("Observation Date Cannot be BLANK");
			if (observationData.getLatitude() == null || observationData.getLongitude() == null)
				throw new ObservationInputException("Observation LATITUDE/LONGITUDE MISSING");
			if (observationData.getObservedAt() == null)
				throw new ObservationInputException("Observation LOCATION cannot be BLANK");
			if (observationData.getsGroup() == null)
				throw new ObservationInputException("Species Group cannot be BLANK");
			if (observationData.getHidePreciseLocation() == null)
				throw new ObservationInputException("GeoPrivacy cannot be BLANK");
			if (observationData.getHelpIdentify() == false) {
				if (observationData.getRecoData().getTaxonScientificName() == null
						&& observationData.getRecoData().getTaxonCommonName() == null)
					throw new ObservationInputException("No Recommendation found");
			}
			if (observationHelper.checkIndiaBounds(observationData) == false) {
				throw new ObservationInputException("Observation Not within India Bounds");
			}

			ShowData result = observationSerices.createObservation(request, observationData);

			return Response.status(Status.OK).entity(result).build();
		} catch (ObservationInputException e) {
			return Response.status(Status.NOT_ACCEPTABLE).entity(e.getMessage()).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.EDIT + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Get the data for  Observation core part Update ", notes = "Returns the user the update page data", response = ObservationUpdateData.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to edit the observation", response = String.class) })

	public Response getEditPageData(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long obvId = Long.parseLong(observationId);
			ObservationUpdateData result = observationSerices.getObservationEditPageData(profile, obvId);
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UPDATE + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Update the Observation core part", notes = "Returns the user the complete show page", response = ShowData.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to edit the observation", response = String.class) })

	public Response updateObservation(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@ApiParam(name = "observationUpdateData") ObservationUpdateData observationUpdate) {
		try {

			if (observationUpdate.getObservedOn() == null)
				throw new ObservationInputException("Observation Date Cannot be BLANK");
			if (observationUpdate.getLatitude() == null || observationUpdate.getLongitude() == null)
				throw new ObservationInputException("Observation LATITUDE/LONGITUDE MISSING");
			if (observationUpdate.getObservedAt() == null)
				throw new ObservationInputException("Observation LOCATION cannot be BLANK");
			if (observationUpdate.getHidePreciseLocation() == null)
				throw new ObservationInputException("GeoPrivacy cannot be BLANK");

			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long obvId = Long.parseLong(observationId);
			ShowData result = observationSerices.editObservaitonCore(profile, obvId, observationUpdate);
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity("User not allowed to edit the observation").build();
		}
	}

	@DELETE
	@Path(ApiConstants.DELETE + "/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.TEXT_PLAIN)
	@ValidateUser

	@ApiOperation(value = "Delete the Observaiton", notes = "Return the Success or Failure Message", response = String.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Observation Cannot be Deleted", response = String.class),
			@ApiResponse(code = 406, message = "User not allowed to delete the Observation", response = String.class) })

	public Response deleteObservation(@Context HttpServletRequest request,
			@PathParam("observationId") String observaitonId) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long obvId = Long.parseLong(observaitonId);

			String result = observationSerices.removeObservation(profile, userId, obvId);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE).entity("User not Allowed to Delete the Observation")
						.build();
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity("Observation Cannot be Deleted").build();
		}
	}

	@PUT
	@Path(ApiConstants.SPECIESGROUP + "/{observationId}/{sGroupId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser
	@ApiOperation(value = "Update the Species group of the observation", notes = "Returns the updated Species group id", response = Long.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to update the Species Group", response = String.class) })

	public Response updateSGroup(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@PathParam("sGroupId") String sGroupId) {
		try {
			Long obvId = Long.parseLong(observationId);
			Long sGroup = Long.parseLong(sGroupId);

			Long result = observationSerices.updateSGroup(obvId, sGroup);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UPDATE + ApiConstants.TAGS)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "update tags for the observation", notes = "Returns Tags list", response = Tags.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to update the tags", response = String.class) })

	public Response updateTags(@Context HttpServletRequest request,
			@ApiParam(name = "tagsMapping") TagsMapping tagsMapping) {
		try {
			List<Tags> result = observationSerices.updateTags(tagsMapping);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UPDATE + ApiConstants.TRAITS + "/{observationId}/{traitId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Update the specific Trait with values", notes = "Returns all facts", response = FactValuePair.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to Update the Traits", response = String.class) })

	public Response updateTraits(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@PathParam("traitId") String traitId, @ApiParam(name = "valueList") List<Long> valueList) {
		try {
			List<FactValuePair> result = observationSerices.updateTraits(observationId, traitId, valueList);

			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UPDATE + ApiConstants.USERGROUP + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Update the UserGroup linked with a observation", notes = "Returns all the current userGroup Linked", response = UserGroupIbp.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to updated the userGroup of Observaiton", response = String.class) })

	public Response updateUserGroup(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@ApiParam(name = "userGroupList") List<Long> userGroupList) {
		try {
			List<UserGroupIbp> result = observationSerices.updateUserGroup(observationId, userGroupList);

			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.SPECIES + ApiConstants.ALL)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Get all the Specie Group", notes = "Returns all the Species Group", response = SpeciesGroup.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to fetch the UserGroup", response = String.class) })

	public Response getAllSpecies() {
		try {

			List<SpeciesGroup> result = observationSerices.getAllSpeciesGroup();
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.LANGUAGE)
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Find all the Languages based on IsDirty field", notes = "Returns all the Languages Details", response = Language.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Languages Not Found", response = String.class) })

	public Response getLanguaes(@QueryParam("isDirty") Boolean isDirty) {
		try {
			List<Language> result = observationSerices.getLanguages(isDirty);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.FEATURED)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Posting of Featured to a Group", notes = "Returns the Details of Featured", response = Featured.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 404, message = "Unable to Feature in a Group", response = String.class) })

	public Response createFeatured(@Context HttpServletRequest request,
			@ApiParam(name = "featuredCreate") FeaturedCreate featuredCreate) {
		try {
			List<Featured> result = observationSerices.createFeatured(featuredCreate);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UNFEATURED + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser
	@ApiOperation(value = "UnFeatures a Object from a UserGroup", notes = "Returns the Current Featured", response = Featured.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 404, message = "Unable to Unfeature", response = String.class) })

	public Response unFeatured(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@ApiParam(name = "userGroupList") List<Long> userGroupList) {
		try {
			List<Featured> result = observationSerices.unFeatured(observationId, userGroupList);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.TRAITS + "/{traitId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Find the value of Traits", notes = "Returns the values of traits based on trait's ID", response = TraitsValue.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to get the values", response = String.class) })

	public Response getValuesOfTraits(@Context HttpServletRequest request, @PathParam("traitId") String traitId) {
		try {
			List<TraitsValue> result = observationSerices.getTraitsValue(traitId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.SPECIES + "/{speciesId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@ApiOperation(value = "Find all Trait Values pair for Specific SpeciesId", notes = "Return the Key value pairs of Traits", response = TraitsValuePair.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Species Not Found", response = String.class) })

	public Response getTraitList(@PathParam("speciesId") String speciesId) {
		try {
			List<TraitsValuePair> result = observationSerices.getTraitList(speciesId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.PERMISSIONS + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Find all the user Permission for current observation", notes = "Returns list of permission for validate post and feature in a group", response = ObservationUserPermission.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to fetch the permission", response = String.class) })

	public Response getUserPermissions(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@ApiParam("taxonList") @QueryParam("taxonList") String taxonList) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());

			ObservationUserPermission result = observationSerices.getUserPermissions(profile, observationId, userId,
					taxonList);

			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.TAGS + ApiConstants.AUTOCOMPLETE)
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Find the Sugguestion for tags", notes = "Return list of Top 10 tags matching the phrase", response = Tags.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to fetch the tags", response = String.class) })

	public Response getTagsSuggetion(@QueryParam("phrase") String phrase) {
		try {
			List<Tags> result = observationSerices.getTagsSugguestions(phrase);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.USERGROUP)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Find all the userGroup Associated with a user", notes = "Returns a List of UserGroup", response = UserGroupIbp.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to get the userGroup", response = String.class) })
	public Response getUsersGroupList(@Context HttpServletRequest request) {
		try {
			List<UserGroupIbp> result = observationSerices.getUsersGroupList();
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.FLAG + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Flag a Observaiton", notes = "Return a list of flag to the Observaiton", response = Flag.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to flag a Observation", response = String.class),
			@ApiResponse(code = 406, message = "User has already flagged", response = String.class) })

	public Response createFlag(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@ApiParam(name = "flagIbp") FlagIbp flagIbp) {
		try {
			Long obsId = Long.parseLong(observationId);
			List<Flag> result = observationSerices.createFlag(obsId, flagIbp);
			if (result.isEmpty())
				return Response.status(Status.NOT_ACCEPTABLE).entity("User Allowed Flagged").build();
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UNFLAG + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Unflag a Observation", notes = "Return a list of flag to the Observation", response = Flag.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to unflag a Observation", response = String.class),
			@ApiResponse(code = 406, message = "User is not allowed to unflag", response = String.class) })

	public Response unFlag(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@ApiParam(name = "flag") Flag flag) {
		try {
			Long obsId = Long.parseLong(observationId);
			List<Flag> result = observationSerices.unFlag(obsId, flag);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE).entity("User not allowed to Unflag").build();
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.FOLLOW + "/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Marks follow for a User", notes = "Returnt the follow details", response = Follow.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to mark follow", response = String.class) })

	public Response followObservation(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId) {
		try {
			Long obvId = Long.parseLong(observationId);
			Follow result = observationSerices.followRequest(obvId);
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.UNFOLLOW + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Marks unfollow for a User", notes = "Returnt the unfollow details", response = Follow.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to mark unfollow", response = String.class) })

	public Response unfollow(@Context HttpServletRequest request, @PathParam("observationId") String observationId) {

		try {
			Long obvId = Long.parseLong(observationId);
			Follow result = observationSerices.unFollowRequest(obvId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.AUTHOR + "/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.TEXT_PLAIN)

	@ApiOperation(value = "Finds the authorId of the observation", notes = "Returns the authorid of a observation", response = String.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to fetch the authorid", response = String.class) })

	public Response getObservationAuthor(@PathParam("observationId") String observationId) {
		try {
			Long obvId = Long.parseLong(observationId);
			Long result = observationSerices.getObservationAuthor(obvId);
			return Response.status(Status.OK).entity(result.toString()).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity("Cannot find the Author").build();
		}
	}

	@POST
	@Path(ApiConstants.APPLYFILTER)
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.TEXT_PLAIN)

	@ValidateUser
	@ApiOperation(value = "Apply the new Filter Rule to the Observation Existings", notes = "Starts the process to apply the Rule", response = String.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to start the process", response = String.class) })

	public Response applyNewFilter(@Context HttpServletRequest request, @QueryParam("groupIds") String groupIds) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");

			if (userRole.contains("ROLE_ADMIN")) {
				UserGroupFilterThread groupFilterThread = new UserGroupFilterThread(observationSerices, groupIds);
				Thread thread = new Thread(groupFilterThread);
				thread.start();
				return Response.status(Status.OK).entity("Process has started to apply the new filter Rule").build();
			}
			return Response.status(Status.NOT_ACCEPTABLE).entity("USER NOT ALLOWED TO PERFORM THE TASK").build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity("Filter cannot be started").build();
		}
	}

	@POST
	@Path(ApiConstants.APPLYGEOPRIVACY)
	@Produces(MediaType.TEXT_PLAIN)

	@ValidateUser

	@ApiOperation(value = "Bulk update the geoPrivate traits in all observation", notes = "Starts a process to update the geoPrivacy Field", response = String.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to start the process", response = String.class),
			@ApiResponse(code = 406, message = "User not allowed to perform the task", response = String.class) })

	public Response applyGeoPrivacy(@Context HttpServletRequest request) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			JSONArray userProfile = (JSONArray) profile.getAttribute("roles");
			if (userProfile.contains("ROLE_ADMIN")) {
				Thread thread = new Thread(geoPrivacyThread);
				thread.start();
				return Response.status(Status.OK).entity("GeoPrivacy Migration has started").build();
			}
			return Response.status(Status.NOT_ACCEPTABLE).entity("USER NOT ALLOWED TO PERFORM THE TASK").build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity("Unable to Start the process").build();
		}
	}

}
