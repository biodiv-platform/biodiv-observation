/**
 * 
 */
package com.strandls.observation.contorller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;

import org.pac4j.core.profile.CommonProfile;

import com.strandls.activity.pojo.Activity;
import com.strandls.activity.pojo.CommentLoggingData;
import com.strandls.authentication_utility.filter.ValidateUser;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.esmodule.pojo.FilterPanelData;
import com.strandls.esmodule.pojo.MapBoundParams;
import com.strandls.esmodule.pojo.MapBounds;
import com.strandls.esmodule.pojo.MapGeoPoint;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchParams.SortTypeEnum;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.dao.ObservationDownloadLogDAO;
import com.strandls.observation.es.util.ESUtility;
import com.strandls.observation.es.util.ObservationListCSVThread;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.es.util.ObservationListMinimalData;
import com.strandls.observation.es.util.ObservationUtilityFunctions;
import com.strandls.observation.es.util.PublicationGrade;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.pojo.ListPagePermissions;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MaxVotedRecoPermission;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ObservationCreateUGContext;
import com.strandls.observation.pojo.ObservationHomePage;
import com.strandls.observation.pojo.ObservationListData;
import com.strandls.observation.pojo.ObservationUGContextCreatePageData;
import com.strandls.observation.pojo.ObservationUpdateData;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.MailService;
import com.strandls.observation.service.ObservationListService;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.service.Impl.GeoPrivacyBulkThread;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.observation.service.Impl.UserGroupPostingFilterThread;
import com.strandls.observation.service.Impl.UserGroupUnPostingFilterThread;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.resource.pojo.ResourceRating;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.pojo.Follow;
import com.strandls.userGroup.pojo.CustomFieldFactsInsert;
import com.strandls.userGroup.pojo.CustomFieldObservationData;
import com.strandls.userGroup.pojo.CustomFieldValues;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.FeaturedCreate;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.pojo.FlagIbp;
import com.strandls.utility.pojo.FlagShow;
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
	private ObservationService observationService;

	@Inject
	private GeoPrivacyBulkThread geoPrivacyThread;

	@Inject
	private ObservationMapperHelper observationHelper;

	@Inject
	private ESUtility esUtility;

	@Inject
	private ObservationDownloadLogDAO downloadLogDao;

	@Inject
	private ObservationListService observationListService;
	
	@Inject
	private MailService mailService;
	


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
			ShowData show = observationService.findById(obvId);

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

			ShowData result = observationService.createObservation(request, observationData);
			if (result != null)
				return Response.status(Status.OK).entity(result).build();
			return Response.status(Status.NOT_ACCEPTABLE).build();
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
			ObservationUpdateData result = observationService.getObservationEditPageData(profile, obvId);
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
			ShowData result = observationService.editObservaitonCore(request, profile, obvId, observationUpdate);
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

			String result = observationService.removeObservation(request, profile, userId, obvId);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE).entity("User not Allowed to Delete the Observation")
						.build();
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity("Observation Cannot be Deleted").build();
		}
	}

	@POST
	@Path(ApiConstants.UPDATE + ApiConstants.LASTREVISED + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Updates the last revised of Observation", notes = "Updates the last revised of observation", response = Boolean.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to update the Obsevation", response = String.class) })

	public Response updateLastRevised(@PathParam("observationId") String observationId) {
		try {
			Long obvId = Long.parseLong(observationId);
			Boolean result = observationService.updateLastRevised(obvId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.LIST + ApiConstants.ALL)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Get all the dynamic filters", notes = "Return all the filter", response = FilterPanelData.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to get the data", response = String.class) })

	public Response getAllFilters() {
		try {
			FilterPanelData result = observationListService.getAllFilter();
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.LIST + "/{index}/{type}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Fetch the observation based on the filter", notes = "Returns the observation list based on the the filters", response = ObservationListData.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to fetch the data", response = String.class) })

	public Response observationList(@PathParam("index") String index, @PathParam("type") String type,
			@DefaultValue("") @QueryParam("sGroup") String sGroup, @DefaultValue("") @QueryParam("taxon") String taxon,
			@DefaultValue("") @QueryParam("user") String user,
			@DefaultValue("") @QueryParam("userGroupList") String userGroupList,
			@DefaultValue("") @QueryParam("webaddress") String webaddress,
			@DefaultValue("") @QueryParam("speciesName") String speciesName,
			@DefaultValue("") @QueryParam("mediaFilter") String mediaFilter,
			@DefaultValue("") @QueryParam("months") String months,
			@DefaultValue("") @QueryParam("isFlagged") String isFlagged, @QueryParam("location") String location,
			@DefaultValue("last_revised") @QueryParam("sort") String sortOn, @QueryParam("minDate") String minDate,
			@QueryParam("maxDate") String maxDate, @QueryParam("createdOnMaxDate") String createdOnMaxDate,
			@QueryParam("createdOnMinDate") String createdOnMinDate, @QueryParam("status") String status,
			@QueryParam("taxonId") String taxonId, @QueryParam("validate") String validate,
			@QueryParam("recoName") String recoName,
			@DefaultValue("265799") @QueryParam("classification") String classificationid,
			@DefaultValue("10") @QueryParam("max") Integer max, @DefaultValue("0") @QueryParam("offset") Integer offset,
			@DefaultValue("location") @QueryParam("geoAggregationField") String geoAggregationField,
			@DefaultValue("1") @QueryParam("geoAggegationPrecision") Integer geoAggegationPrecision,
			@QueryParam("left") Double left, @QueryParam("right") Double right, @QueryParam("top") Double top,
			@QueryParam("bottom") Double bottom, @QueryParam("recom") String maxvotedrecoid,
			@QueryParam("onlyFilteredAggregation") Boolean onlyFilteredAggregation,
			@QueryParam("termsAggregationField") String termsAggregationField, @DefaultValue("list")@QueryParam("view") String view,
			@QueryParam("rank") String rank, @QueryParam("tahsil") String tahsil,
			@QueryParam("district") String district, @QueryParam("state") String state, @QueryParam("tags") String tags,
			@QueryParam("publicationgrade") String publicationGrade, @Context UriInfo uriInfo) {

		try {

			if (max > 50)
				max = 50;

			MultivaluedMap<String, String> queryParams = uriInfo.getQueryParameters();
			Map<String, List<String>> traitParams = queryParams.entrySet().stream()
					.filter(entry -> entry.getKey().startsWith("trait"))
					.collect(Collectors.toMap(p -> p.getKey(), p -> p.getValue()));

			Map<String, List<String>> customParams = queryParams.entrySet().stream()
					.filter(entry -> entry.getKey().startsWith("custom"))
					.collect(Collectors.toMap(p -> p.getKey(), p -> p.getValue()));

			MapBounds bounds = null;
			if (top != null || bottom != null || left != null || right != null) {
				bounds = new MapBounds();
				bounds.setBottom(bottom);
				bounds.setLeft(left);
				bounds.setRight(right);
				bounds.setTop(top);
			}
			List<MapGeoPoint> polygon = new ArrayList<MapGeoPoint>();
			if (location != null) {
				double[] point = Stream.of(location.split(",")).mapToDouble(Double::parseDouble).toArray();
				for (int i = 0; i < point.length; i = i + 2) {
					String singlePoint = point[i + 1] + "," + point[i];

					int comma = singlePoint.indexOf(',');
					if (comma != -1) {
						MapGeoPoint geoPoint = new MapGeoPoint();
						geoPoint.setLat(Double.parseDouble(singlePoint.substring(0, comma).trim()));
						geoPoint.setLon(Double.parseDouble(singlePoint.substring(comma + 1).trim()));
						polygon.add(geoPoint);
					}
				}
			}

			MapBoundParams mapBoundsParams = new MapBoundParams();
			mapBoundsParams.setBounds(bounds);
			mapBoundsParams.setPolygon(polygon);

			MapSearchParams mapSearchParams = new MapSearchParams();
			mapSearchParams.setFrom(offset);
			mapSearchParams.setLimit(max);
			mapSearchParams.setSortOn(sortOn);
			mapSearchParams.setSortType(SortTypeEnum.DESC);
			mapSearchParams.setMapBoundParams(mapBoundsParams);

			MapSearchQuery mapSearchQuery = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);

			MapAggregationResponse aggregationResult = observationListService.mapAggregate(index, type, sGroup, taxon,
					user, userGroupList, webaddress, speciesName, mediaFilter, months, isFlagged, minDate, maxDate,
					validate, traitParams, customParams, classificationid, mapSearchParams, maxvotedrecoid,
					createdOnMaxDate, createdOnMinDate, status, taxonId, recoName, geoAggregationField, rank, tahsil,
					district, state, tags, publicationGrade);

			ObservationListData result = observationListService.getObservationList(index, type, mapSearchQuery,
					geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation, termsAggregationField,
					aggregationResult, view);
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
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

			Long result = observationService.updateSGroup(request, obvId, sGroup);
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
			List<Tags> result = observationService.updateTags(request, tagsMapping);
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
			List<FactValuePair> result = observationService.updateTraits(request, observationId, traitId, valueList);

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
			List<UserGroupIbp> result = observationService.updateUserGroup(request, observationId, userGroupList);

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

			List<SpeciesGroup> result = observationService.getAllSpeciesGroup();
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
			List<Language> result = observationService.getLanguages(isDirty);
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
			List<Featured> result = observationService.createFeatured(request, featuredCreate);
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
			List<Featured> result = observationService.unFeatured(request, observationId, userGroupList);
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
			List<TraitsValue> result = observationService.getTraitsValue(request, traitId);
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
			List<TraitsValuePair> result = observationService.getTraitList(speciesId);
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
			@ApiParam("taxonList") @DefaultValue("") @QueryParam("taxonList") String taxonList) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());

			ObservationUserPermission result = observationService.getUserPermissions(request, profile, observationId,
					userId, taxonList);

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
			List<Tags> result = observationService.getTagsSugguestions(phrase);
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
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			List<UserGroupIbp> result = observationService.getUsersGroupList(request, profile);
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

	@ApiOperation(value = "Flag a Observaiton", notes = "Return a list of flag to the Observaiton", response = FlagShow.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to flag a Observation", response = String.class),
			@ApiResponse(code = 406, message = "User has already flagged", response = String.class) })

	public Response createFlag(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@ApiParam(name = "flagIbp") FlagIbp flagIbp) {
		try {
			Long obsId = Long.parseLong(observationId);
			List<FlagShow> result = observationService.createFlag(request, obsId, flagIbp);
			if (result.isEmpty())
				return Response.status(Status.NOT_ACCEPTABLE).entity("User Allowed Flagged").build();
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UNFLAG + "/{observationId}/{flagId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser

	@ApiOperation(value = "Unflag a Observation", notes = "Return a list of flag to the Observation", response = FlagShow.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to unflag a Observation", response = String.class),
			@ApiResponse(code = 406, message = "User is not allowed to unflag", response = String.class) })

	public Response unFlag(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@PathParam("flagId") String flagId) {
		try {
			Long obsId = Long.parseLong(observationId);
			List<FlagShow> result = observationService.unFlag(request, obsId, flagId);
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
			Follow result = observationService.followRequest(request, obvId);
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
			Follow result = observationService.unFollowRequest(request, obvId);
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
			Long result = observationService.getObservationAuthor(obvId);
			return Response.status(Status.OK).entity(result.toString()).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity("Cannot find the Author").build();
		}
	}

	@POST
	@Path(ApiConstants.APPLYFILTER + ApiConstants.POSTING)
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.TEXT_PLAIN)

	@ValidateUser
	@ApiOperation(value = "Apply the new Filter Rule to post the Observation Existings", notes = "Starts the process to apply the Rule", response = String.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to start the process", response = String.class) })

	public Response applyNewFilterPosting(@Context HttpServletRequest request,
			@QueryParam("groupIds") String groupIds) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");

			if (userRole.contains("ROLE_ADMIN")) {
				UserGroupPostingFilterThread groupFilterThread = new UserGroupPostingFilterThread(observationService,
						groupIds);
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
	@Path(ApiConstants.APPLYFILTER + ApiConstants.REMOVING)
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.TEXT_PLAIN)

	@ValidateUser
	@ApiOperation(value = "Apply the new Filter Rule to unpost the Observation Existings", notes = "Starts the process to apply the Rule", response = String.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to start the process", response = String.class) })

	public Response applyNewFilterRemoving(@Context HttpServletRequest request, @QueryParam("groupId") String groupId) {
		try {

			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");

			if (userRole.contains("ROLE_ADMIN")) {
				UserGroupUnPostingFilterThread groupUnPostingThread = new UserGroupUnPostingFilterThread(
						observationService, groupId);
				Thread thread = new Thread(groupUnPostingThread);
				thread.start();
				return Response.status(Status.OK).entity("Process has started to apply the new filter Rule").build();
			}
			return Response.status(Status.NOT_ACCEPTABLE).entity("USER NOT ALLOWED TO PERFORM THE TASK").build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
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

	@GET
	@Path(ApiConstants.CUSTOMFIELD + ApiConstants.OPTIONS + "/{observationId}/{userGroupId}/{cfId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Finds the set of Values for a Custom Field", notes = "Returns the Set of Values of Custom Field", response = CustomFieldValues.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to get the value list", response = String.class) })

	public Response getCustomFieldOptions(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId, @PathParam("userGroupId") String userGroupId,
			@PathParam("cfId") String cfId) {
		try {
			List<CustomFieldValues> result = observationService.getCustomFieldOptions(request, observationId,
					userGroupId, cfId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@POST
	@Path(ApiConstants.CUSTOMFIELD + ApiConstants.INSERT)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Insert/Update custom field Data", notes = "Return a complete customField Data for the Observaiton", response = CustomFieldObservationData.class, responseContainer = "List")
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to add/Update the data", response = String.class) })

	public Response addUpdateCustomFieldData(@Context HttpServletRequest request,
			@ApiParam(name = "factsCreateData") CustomFieldFactsInsert factsCreateData) {
		try {
			List<CustomFieldObservationData> result = observationService.addUpdateCustomFieldData(request,
					factsCreateData);
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.PRODUCE + "/{updateType}/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Publish the observationId to RabbitMQ", notes = "Return the result", response = String.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to push to rabbitMQ", response = String.class) })

	public Response pushToRabbitMQ(@PathParam("updateType") String updateType,
			@PathParam("observationId") String observationId) {
		try {
			observationService.produceToRabbitMQ(observationId, updateType);
			return Response.status(Status.OK).entity("Published to RabbitMQ").build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.USERGROUP + ApiConstants.CREATEOBSERVATION + "/{userGroupId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser
	@ApiOperation(value = "Get the observation create page data for ug context", notes = "Returns the create page data", response = ObservationUGContextCreatePageData.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to get the data", response = String.class) })

	public Response getUGContextObservaitonCreate(@Context HttpServletRequest request,
			@PathParam("userGroupId") String userGroupId) {
		try {
			Long ugId = Long.parseLong(userGroupId);
			ObservationUGContextCreatePageData result = observationService.getUGContextObservationCreateDetails(request,
					ugId);
			if (result == null)
				return Response.status(Status.NOT_ACCEPTABLE)
						.entity("USER NOT ALLOWED TO CREATE OBSERVATION IN A GROUP").build();

			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@POST
	@Path(ApiConstants.CREATE + ApiConstants.UGCONTEXT)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "create observation on UG context", notes = "Returns the user the complete show page", response = ShowData.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to create the observation", response = String.class) })

	public Response createObservationUGContext(@Context HttpServletRequest request,
			@ApiParam(name = "observationUGContext") ObservationCreateUGContext observationUGContext) {
		try {
			ShowData result = observationService.creteObservationUGContext(request, observationUGContext);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.LIST + ApiConstants.PERMISSIONS + ApiConstants.MAXVOTEDRECO)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "find the the maxvoted reco permission for list page", notes = "Return list of observationId with boolean value for permission", response = MaxVotedRecoPermission.class, responseContainer = "list")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to fetch the result", response = String.class) })

	public Response getPermissionListMaxVotedRecos(@Context HttpServletRequest request,
			@ApiParam(name = "observationTaxonId") Map<Long, Long> observationTaxonId) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			List<MaxVotedRecoPermission> result = observationService.listMaxRecoVotePermissions(request, profile,
					observationTaxonId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.LIST + ApiConstants.PERMISSIONS + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "search for the permission in list page for a user", notes = "Returns the permission details for the user", response = ListPagePermissions.class)
	@ApiResponses(value = {
			@ApiResponse(code = 400, message = "Unable to fetch the details", response = String.class) })
	public Response getLisPagePermissions(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@ApiParam("taxonList") @DefaultValue("") @QueryParam("taxonList") String taxonList) {
		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long obvId = Long.parseLong(observationId);
			ListPagePermissions result = observationService.getListPagePermissions(request, profile, obvId, taxonList);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.ADD)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "Adds a comment", notes = "Return the current activity", response = Activity.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "Unable to log a comment", response = String.class) })

	public Response addCommnet(@Context HttpServletRequest request,
			@ApiParam(name = "commentData") CommentLoggingData commentDatas) {
		try {
			Activity result = observationService.addObservationComment(request, commentDatas);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@POST
	@Path(ApiConstants.FIND)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "finds observation data on basis of resourceURl", notes = "returns the observation data", response = ObservationHomePage.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to fetch the data", response = String.class) })
	public Response getObservation(@ApiParam("resourcesURLs") String resourcesUrl) {
		try {
			List<ObservationHomePage> result = observationListService.getObservation(resourcesUrl);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.MINIMAL + "/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Gets the observationData", notes = "Returns the observation Data", response = ObservationListMinimalData.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to fetch the data", response = String.class) })

	public Response getObservationMinimal(@PathParam("observationId") String observationId) {
		try {
			ObservationListMinimalData result = observationListService.getObservationMinimal(observationId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@PUT
	@Path(ApiConstants.UPDATE + ApiConstants.RESOURCE + ApiConstants.RATING + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	@ValidateUser

	@ApiOperation(value = "update the rating for gallery resource", notes = "Returns the success or failuer", response = String.class)
	@ApiResponses(value = {
			@ApiResponse(code = 404, message = "unable to update the rating", response = String.class) })
	public Response gallaryRatingUpdate(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@ApiParam(name = "resourceRating") ResourceRating resourceRating) {
		try {
			Long obvId = Long.parseLong(observationId);
			Boolean result = observationService.updateGalleryResourceRating(request, obvId, resourceRating);
			if (result)
				return Response.status(Status.OK).entity("Rating updated").build();
			return Response.status(Status.NOT_FOUND).entity("Cannot Update the Rating").build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}
		
	@GET
	@Path(ApiConstants.LISTCSV + "/{index}/{type}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)

	@ApiOperation(value = "Fetch the observation based on the filter", notes = "Returns the observation list based on the the filters", response = ObservationListData.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to fetch the data", response = String.class) })

	public Response observationListCsv(@PathParam("index") String index, @PathParam("type") String type,
			@DefaultValue("") @QueryParam("sGroup") String sGroup, @DefaultValue("") @QueryParam("taxon") String taxon,
			@DefaultValue("") @QueryParam("user") String user,
			@DefaultValue("") @QueryParam("userGroupList") String userGroupList,
			@DefaultValue("") @QueryParam("webaddress") String webaddress,
			@DefaultValue("") @QueryParam("speciesName") String speciesName,
			@DefaultValue("") @QueryParam("mediaFilter") String mediaFilter,
			@DefaultValue("") @QueryParam("months") String months,
			@DefaultValue("") @QueryParam("isFlagged") String isFlagged, @QueryParam("location") String location,
			@DefaultValue("last_revised") @QueryParam("sort") String sortOn, @QueryParam("minDate") String minDate,
			@QueryParam("maxDate") String maxDate, @QueryParam("createdOnMaxDate") String createdOnMaxDate,
			@QueryParam("createdOnMinDate") String createdOnMinDate, @QueryParam("status") String status,
			@QueryParam("taxonId") String taxonId, @QueryParam("validate") String validate,
			@QueryParam("recoName") String recoName,
			@DefaultValue("265799") @QueryParam("classification") String classificationid,
			@DefaultValue("location") @QueryParam("geoAggregationField") String geoAggregationField,
			@DefaultValue("1") @QueryParam("geoAggegationPrecision") Integer geoAggegationPrecision,
			@QueryParam("left") Double left, @QueryParam("right") Double right, @QueryParam("top") Double top,
			@QueryParam("bottom") Double bottom, @QueryParam("recom") String maxvotedrecoid,
			@QueryParam("onlyFilteredAggregation") Boolean onlyFilteredAggregation,
			@QueryParam("termsAggregationField") String termsAggregationField, @QueryParam("rank") String rank,
			@QueryParam("tahsil") String tahsil, @QueryParam("district") String district,
			@QueryParam("state") String state, @QueryParam("tags") String tags,
			@QueryParam("publicationgrade") String publicationGrade,
			@DefaultValue("")@QueryParam("notes") String notes,
			@NotBlank@NotEmpty@NotNull@QueryParam("authorid") String authorId,
			@QueryParam("customfields") List<String> customfields, @QueryParam("taxonomic") List<String> taxonomic,
			@QueryParam("spatial") List<String> spatial, @QueryParam("traits") List<String> traits,
			@QueryParam("temporal") List<String> temporal, @QueryParam("misc") List<String> misc,
			@Context UriInfo uriInfo) {

		try {
			MultivaluedMap<String, String> queryParams = uriInfo.getQueryParameters();
			Map<String, List<String>> traitParams = queryParams.entrySet().stream()
					.filter(entry -> entry.getKey().startsWith("trait"))
					.collect(Collectors.toMap(p -> p.getKey(), p -> p.getValue()));
			Map<String, List<String>> customParams = queryParams.entrySet().stream()
					.filter(entry -> entry.getKey().startsWith("custom"))
					.collect(Collectors.toMap(p -> p.getKey(), p -> p.getValue()));

			MapBounds bounds = null;
			if (top != null || bottom != null || left != null || right != null) {
				bounds = new MapBounds();
				bounds.setBottom(bottom);
				bounds.setLeft(left);
				bounds.setRight(right);
				bounds.setTop(top);
			}
			List<MapGeoPoint> polygon = new ArrayList<MapGeoPoint>();
			if (location != null) {
				double[] point = Stream.of(location.split(",")).mapToDouble(Double::parseDouble).toArray();
				for (int i = 0; i < point.length; i = i + 2) {
					String singlePoint = point[i + 1] + "," + point[i];

					int comma = singlePoint.indexOf(',');
					if (comma != -1) {
						MapGeoPoint geoPoint = new MapGeoPoint();
						geoPoint.setLat(Double.parseDouble(singlePoint.substring(0, comma).trim()));
						geoPoint.setLon(Double.parseDouble(singlePoint.substring(comma + 1).trim()));
						polygon.add(geoPoint);
					}
				}
			}
			MapBoundParams mapBoundsParams = new MapBoundParams();
			mapBoundsParams.setBounds(bounds);
			mapBoundsParams.setPolygon(polygon);
			MapSearchParams mapSearchParams = new MapSearchParams();
			mapSearchParams.setSortOn(sortOn);
			mapSearchParams.setSortType(SortTypeEnum.DESC);
			mapSearchParams.setMapBoundParams(mapBoundsParams);
			
			ObservationListCSVThread csvThread = new ObservationListCSVThread(esUtility, observationListService,
					downloadLogDao, customfields, taxonomic,
					spatial, traits, temporal, misc, sGroup,
					taxon,  user,  userGroupList,  webaddress,  speciesName,  mediaFilter,
					months,  isFlagged,  minDate,  maxDate,  validate,
					traitParams,  customParams,  classificationid,
					mapSearchParams,  maxvotedrecoid,  createdOnMaxDate,  createdOnMinDate,
					status,  taxonId,  recoName,  rank,  tahsil,  district,  state,
					tags,  publicationGrade,  index,  type,  geoAggregationField,
					geoAggegationPrecision,  onlyFilteredAggregation,  termsAggregationField,
					authorId,  notes, uriInfo.getRequestUri().toString(), mailService);
			Thread thread = new Thread(csvThread);
			thread.start();
			return Response.status(Status.OK).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.PUBLICATIONGRADE + "/{index}/{type}/{documentId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@ApiOperation(value = "Fetch the observation based on the filter", notes = "Returns the observation list based on the the filters", response = PublicationGrade.class)
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to fetch the data", response = String.class) })
	public Response getObservationPublicationGrade(@PathParam("index") String index, @PathParam("type") String type,
			@PathParam("documentId") String documentId) {
		ObservationListElasticMapping obervation = observationService.getObservationPublicationGrade(index, type,
				documentId);

		ObservationUtilityFunctions obUtil = new ObservationUtilityFunctions();
		PublicationGrade observationGrade = obUtil.GradeObservation(obervation);
		return Response.status(Status.OK).entity(observationGrade).build();

	}
	
	@GET
	@Path(ApiConstants.LISTDOWNLOAD)
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@ApiOperation(value="fetch the download log table based on filter",
	notes = "Returns list of download log based on filter", response = DownloadLog.class, responseContainer = "List")
	@ApiResponses(value = { @ApiResponse(code = 400, message = "unable to fetch the data", response = String.class)})
	public Response fetchDownloadLog(@DefaultValue("")@QueryParam("authorid")String authorId,
			@DefaultValue("")@QueryParam("filetype")String fileType,
			@DefaultValue("-1")@QueryParam("offset")String offSet,
			@DefaultValue("-1")@QueryParam("limit")String limit) {
		List<Long> authorIds = new ArrayList<Long>();
		if(!authorId.isEmpty() || authorId != null) {
			authorIds = Arrays.asList(authorId.split(",")).stream().
					map(Long::parseLong).collect(Collectors.toList());
		}
		List<DownloadLog> records = observationService.fetchDownloadLog(authorIds, fileType, 
				Integer.parseInt(offSet),Integer.parseInt(limit));
		return Response.status(Status.OK).entity(records).build();	
	}
}