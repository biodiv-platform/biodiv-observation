package com.strandls.observation.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.pac4j.core.profile.CommonProfile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.activity.pojo.Activity;
import com.strandls.activity.pojo.CommentLoggingData;
import com.strandls.authentication_utility.filter.ValidateUser;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.FilterPanelData;
import com.strandls.esmodule.pojo.MapBoundParams;
import com.strandls.esmodule.pojo.MapBounds;
import com.strandls.esmodule.pojo.MapGeoPoint;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchParams.SortTypeEnum;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.integrator.controllers.IntegratorServicesApi;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dao.ObservationDownloadLogDAO;
import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.es.util.ESUtility;
import com.strandls.observation.es.util.ObservationBulkMappingThread;
import com.strandls.observation.es.util.ObservationListCSVThread;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.es.util.ObservationListMinimalData;
import com.strandls.observation.es.util.ObservationUtilityFunctions;
import com.strandls.observation.es.util.PublicationGrade;
import com.strandls.observation.pojo.DatatableUserGroupUpdateData;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.pojo.EsLocationListParams;
import com.strandls.observation.pojo.ListPagePermissions;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.MaxVotedRecoPermission;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ObservationCreateUGContext;
import com.strandls.observation.pojo.ObservationDataByUser;
import com.strandls.observation.pojo.ObservationDatatableList;
import com.strandls.observation.pojo.ObservationHomePage;
import com.strandls.observation.pojo.ObservationListData;
import com.strandls.observation.pojo.ObservationUGContextCreatePageData;
import com.strandls.observation.pojo.ObservationUpdateData;
import com.strandls.observation.pojo.ObservationUserPageInfo;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.Resources;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.pojo.ShowObervationDataTable;
import com.strandls.observation.service.MailService;
import com.strandls.observation.service.ObservationCreateService;
import com.strandls.observation.service.ObservationDataTableService;
import com.strandls.observation.service.ObservationListService;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.service.Impl.GeoPrivacyBulkThread;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.observation.service.Impl.UserGroupPostingFilterThread;
import com.strandls.observation.service.Impl.UserGroupUnPostingFilterThread;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.resource.pojo.ResourceRating;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.FactsUpdateData;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.Follow;
import com.strandls.userGroup.controller.UserGroupServiceApi;
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
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.io.WKTReader;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import jakarta.ws.rs.core.UriInfo;
import net.minidev.json.JSONArray;

/**
 * @author Abhishek Rudra
 *
 */

@Tag(name = "Observation Service")
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
	private ObservationCreateService observationCreateService;

	@Inject
	private MailService mailService;

	@Inject
	private ObservationDataTableService observationDataTableService;

	@Inject
	private UserServiceApi userService;

	@Inject
	private ObjectMapper objectMapper;

	@Inject
	private Headers headers;

	@Inject
	private ObservationDAO observationDao;

	@Inject
	private ObservationMapperHelper observationMapperHelper;

	@Inject
	private EsServicesApi esService;

	@Inject
	private UserGroupServiceApi ugService;

	@Inject
	private IntegratorServicesApi integratorService;

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private TraitsServiceApi traitService;

	@GET
	@Operation(summary = "Dummy API Ping", description = "Checks validity of war file at deployment", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))) })
	@Path(ApiConstants.PING)
	@Produces(MediaType.TEXT_PLAIN)
	public String ping() {
		return "pong Observation";
	}

	@GET
	@Path(ApiConstants.USERTEMPORALAGGREGATION + "/{userId}")
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Find Aggregation by day by user", description = "Returns observations grouped by day and month", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationDataByUser.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public ObservationDataByUser getObservationPerDay(@PathParam("userId") String userId) {

		return observationListService.getCountPerDay(userId);

	}

	@GET
	@Path(ApiConstants.SHOW + "/{observationId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Find Observation by ID", description = "Returns the complete Observation with all the specification", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ShowData.class))),
			@ApiResponse(responseCode = "404", description = "Observation not found", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Invalid ID", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response show(
			@Parameter(description = "ID of Show that needs to be fetched", required = true) @PathParam("observationId") String id) {

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
	@Operation(summary = "Create an Observation", description = "Returns the show Page of Observation", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Long.class))),
			@ApiResponse(responseCode = "404", description = "Observation Cannot be created", content = @Content(schema = @Schema(implementation = String.class))) })

	public Response createObservation(@Context HttpServletRequest request,
			@Parameter(description = "observationData", required = true) ObservationCreate observationData) {
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
			if (observationHelper.checkObservationBounds(observationData.getLatitude(),
					observationData.getLongitude()) == false) {
				throw new ObservationInputException("Observation not within geographical bounds");
			}
			if (observationData.getResources() == null || observationData.getResources().isEmpty()) {
				throw new ObservationInputException("Without resource observation");
			}

			Long result = observationCreateService.createObservation(request, observationData, true);
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
	@Operation(summary = "Get the data for Observation core part Update", description = "Returns the user the update page data", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationUpdateData.class))),
			@ApiResponse(responseCode = "400", description = "Unable to edit the observation", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Update the Observation core part", description = "Returns the user the complete show page", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ShowData.class))),
			@ApiResponse(responseCode = "400", description = "Unable to edit the observation", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response updateObservation(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@Parameter(description = "observationUpdateData", required = true) ObservationUpdateData observationUpdate) {
		try {

			if (observationUpdate.getDataTableId() == null && observationUpdate.getObservedOn() == null)
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
	@Operation(summary = "Delete the Observation", description = "Return the Success or Failure Message", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Observation Cannot be Deleted", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "406", description = "User not allowed to delete the Observation", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Updates the last revised of Observation", description = "Updates the last revised of observation", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Boolean.class))),
			@ApiResponse(responseCode = "400", description = "Unable to update the Observation", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Get all the dynamic filters", description = "Return all the filter", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = FilterPanelData.class))),
			@ApiResponse(responseCode = "400", description = "Unable to get the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getAllFilters() {
		try {
			FilterPanelData result = observationListService.getAllFilter();
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@POST
	@Path(ApiConstants.LIST + "/{index}/{type}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Fetch observations based on filter", description = "Returns a list of observations based on the filters", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationListData.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response observationList(@PathParam("index") String index, @PathParam("type") String type,
			@DefaultValue("") @QueryParam("sGroup") String sGroup, @DefaultValue("") @QueryParam("taxon") String taxon,
			@DefaultValue("") @QueryParam("user") String user,
			@DefaultValue("") @QueryParam("userGroupList") String userGroupList,
			@DefaultValue("") @QueryParam("webaddress") String webaddress,
			@DefaultValue("") @QueryParam("speciesName") String speciesName,
			@DefaultValue("") @QueryParam("mediaFilter") String mediaFilter,
			@DefaultValue("") @QueryParam("months") String months,
			@DefaultValue("") @QueryParam("isFlagged") String isFlagged,
			@DefaultValue("") @QueryParam("dataTableName") String dataTableName,
			@DefaultValue("") @QueryParam("dataSetName") String dataSetName,
			@DefaultValue("") @QueryParam("dataTableId") String dataTableId,
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
			@QueryParam("bottom") Double bottom, @QueryParam("recoId") String recoId,
			@QueryParam("maxVotedReco") String maxVotedReco, @QueryParam("authorVoted") String authorVoted,
			@QueryParam("onlyFilteredAggregation") Boolean onlyFilteredAggregation,
			@QueryParam("termsAggregationField") String termsAggregationField,
			@DefaultValue("list") @QueryParam("view") String view, @QueryParam("rank") String rank,
			@QueryParam("tahsil") String tahsil, @QueryParam("district") String district,
			@QueryParam("state") String state, @QueryParam("geoEntity") String geoEntity,
			@QueryParam("tags") String tags, @Parameter(description = "location") EsLocationListParams location,
			@QueryParam("geoShapeFilterField") String geoShapeFilterField,
			@QueryParam("nestedField") String nestedField, @QueryParam("publicationgrade") String publicationGrade,
			@DefaultValue("0") @QueryParam("lifelistoffset") Integer lifeListOffset,
			@DefaultValue("0") @QueryParam("uploadersoffset") Integer uploadersoffset,
			@DefaultValue("0") @QueryParam("identifiersoffset") Integer identifiersoffset,

			@QueryParam("recom") String maxvotedrecoid, @DefaultValue("") @QueryParam("notes") String notes,
			@DefaultValue("") @QueryParam("authorId") String authorId,
			@QueryParam("customfields") List<String> customfields, @QueryParam("taxonomic") List<String> taxonomic,
			@QueryParam("spatial") List<String> spatial, @QueryParam("traits") List<String> traits,
			@QueryParam("temporal") List<String> temporal, @QueryParam("misc") List<String> misc,
			@QueryParam("bulkAction") String bulkAction, @QueryParam("selectAll") Boolean selectAll,
			@QueryParam("bulkUsergroupIds") String bulkUsergroupIds,
			@QueryParam("bulkObservationIds") String bulkObservationIds,
			@DefaultValue("false") @QueryParam("showData") String showData,
			@DefaultValue("") @QueryParam("statsFilter") String statsFilter,

			@Context HttpServletRequest request, @Context UriInfo uriInfo) {

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
			MapBoundParams mapBoundsParams = new MapBoundParams();
			mapBoundsParams.setBounds(bounds);

			MapSearchParams mapSearchParams = new MapSearchParams();
			mapSearchParams.setFrom(offset);
			mapSearchParams.setLimit(max);
			mapSearchParams.setSortOn(sortOn);
			mapSearchParams.setSortType(SortTypeEnum.DESC);
			mapSearchParams.setMapBoundParams(mapBoundsParams);

			String loc = location.getLocation();
			if (loc != null) {
				if (loc.contains("/")) {
					String[] locationArray = loc.split("/");
					List<List<MapGeoPoint>> multiPolygonPoint = esUtility.multiPolygonGenerator(locationArray);
					mapBoundsParams.setMultipolygon(multiPolygonPoint);
				} else {
					mapBoundsParams.setPolygon(esUtility.polygonGenerator(loc));
				}
			}

			MapSearchQuery mapSearchQuery = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxVotedReco, recoId, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted, dataSetName,
					dataTableName, geoEntity, dataTableId);

			if ((view.equalsIgnoreCase("csv_download") || view.equalsIgnoreCase("resources_csv_download"))
					&& !authorId.isEmpty() && request.getHeader(HttpHeaders.AUTHORIZATION) != null
					&& !request.getHeader(HttpHeaders.AUTHORIZATION).isEmpty()) {
				userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));

				Boolean imageResourcesDownload = false;
				if (view.equalsIgnoreCase("resources_csv_download")) {
					imageResourcesDownload = true;
				}

				ObservationListCSVThread csvThread = new ObservationListCSVThread(esUtility, observationListService,
						downloadLogDao, customfields, taxonomic, spatial, traits, temporal, misc, sGroup, taxon, user,
						userGroupList, webaddress, speciesName, mediaFilter, months, isFlagged, minDate, maxDate,
						validate, traitParams, customParams, classificationid, mapSearchParams, maxvotedrecoid,
						createdOnMaxDate, createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state,
						tags, publicationGrade, index, type, geoAggregationField, geoAggegationPrecision,
						onlyFilteredAggregation, termsAggregationField, authorId, notes,
						uriInfo.getRequestUri().toString(), dataSetName, dataTableName, mailService, userService,
						objectMapper, mapSearchQuery, geoShapeFilterField, dataTableId, imageResourcesDownload);
				Thread thread = new Thread(csvThread);
				thread.start();
				return Response.status(Status.OK).build();

			}

			else if ((Boolean.FALSE.equals(selectAll) && bulkObservationIds != null && !bulkAction.isEmpty()
					&& !bulkObservationIds.isEmpty() && bulkUsergroupIds != null && !bulkUsergroupIds.isEmpty()
					&& view.equalsIgnoreCase("bulkMapping"))
					|| (Boolean.TRUE.equals(selectAll) && bulkUsergroupIds != null && !bulkUsergroupIds.isEmpty()
							&& !bulkAction.isEmpty() && view.equalsIgnoreCase("bulkMapping"))) {
				mapSearchParams.setFrom(0);
				mapSearchParams.setLimit(100000);
				ObservationBulkMappingThread bulkMappingThread = new ObservationBulkMappingThread(selectAll, bulkAction,
						bulkObservationIds, bulkUsergroupIds, mapSearchQuery, ugService, index, type,
						geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation, termsAggregationField,
						geoShapeFilterField, null, null, view, esService, observationMapperHelper, observationDao,
						request, headers, objectMapper, integratorService, esUpdate, traitService);

				Thread thread = new Thread(bulkMappingThread);
				thread.start();
				return Response.status(Status.OK).build();

			} else if (view.equalsIgnoreCase("map") || view.equalsIgnoreCase("stats") || view.equalsIgnoreCase("list")
					|| view.equalsIgnoreCase("list_minimal")) {

				MapAggregationResponse aggregationResult = null;
				MapAggregationStatsResponse aggregationStatsResult = null;

				if (offset == 0) {
					if (showData.equals("false") && statsFilter.isEmpty()) {
						aggregationResult = observationListService.mapAggregate(index, type, sGroup, taxon, user,
								userGroupList, webaddress, speciesName, mediaFilter, months, isFlagged, minDate,
								maxDate, validate, traitParams, customParams, classificationid, mapSearchParams,
								maxVotedReco, recoId, createdOnMaxDate, createdOnMinDate, status, taxonId, recoName,
								geoAggregationField, rank, tahsil, district, state, tags, publicationGrade, authorVoted,
								dataSetName, dataTableName, geoEntity, dataTableId);
					}

					if (view.equalsIgnoreCase("stats") && !statsFilter.isEmpty()) {
						aggregationStatsResult = observationListService.mapAggregateStats(index, type, sGroup, taxon,
								user, userGroupList, webaddress, speciesName, mediaFilter, months, isFlagged, minDate,
								maxDate, validate, traitParams, customParams, classificationid, mapSearchParams,
								maxVotedReco, recoId, createdOnMaxDate, createdOnMinDate, status, taxonId, recoName,
								geoAggregationField, rank, tahsil, district, state, tags, publicationGrade, authorVoted,
								lifeListOffset, uploadersoffset, identifiersoffset, dataSetName, dataTableName,
								geoEntity, geoShapeFilterField, dataTableId, statsFilter);

					}

				}

				ObservationListData result = observationListService.getObservationList(index, type, mapSearchQuery,
						geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation, termsAggregationField,
						geoShapeFilterField, aggregationStatsResult, aggregationResult, view);
				return Response.status(Status.OK).entity(result).build();

			}

			return Response.status(Status.OK).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@PUT
	@Path(ApiConstants.SPECIESGROUP + "/{observationId}/{sGroupId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser
	@Operation(summary = "Update the Species group of the observation", description = "Returns the updated Species group id", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Long.class))),
			@ApiResponse(responseCode = "400", description = "Unable to update the Species Group", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Update tags for the observation", description = "Returns Tags list", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Tags.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to update the tags", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response updateTags(@Context HttpServletRequest request,
			@Parameter(description = "tagsMapping", required = true) TagsMapping tagsMapping) {
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
	@Operation(summary = "Update the specific Trait with values", description = "Returns all facts", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = FactValuePair.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to Update the Traits", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response updateTraits(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@PathParam("traitId") String traitId,
			@Parameter(description = "updateData", required = true) FactsUpdateData updateData) {
		try {
			List<FactValuePair> result = observationService.updateTraits(request, observationId, traitId, updateData);

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
	@Operation(summary = "Update the UserGroup linked with an observation", description = "Returns all the current userGroup Linked", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = UserGroupIbp.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to updated the userGroup of Observation", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response updateUserGroup(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@Parameter(description = "userGroupList", required = true) List<Long> userGroupList) {
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
	@Operation(summary = "Get all the Species Group", description = "Returns all the Species Group", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = SpeciesGroup.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the UserGroup", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Find all the Languages based on IsDirty field", description = "Returns all the Languages Details", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Language.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Languages Not Found", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Posting of Featured to a Group", description = "Returns the Details of Featured", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Featured.class, type = "array"))),
			@ApiResponse(responseCode = "404", description = "Unable to Feature in a Group", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response createFeatured(@Context HttpServletRequest request,
			@Parameter(description = "featuredCreate", required = true) FeaturedCreate featuredCreate) {
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
	@Operation(summary = "Unfeatures an Object from a UserGroup", description = "Returns the Current Featured", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Featured.class, type = "array"))),
			@ApiResponse(responseCode = "404", description = "Unable to Unfeature", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response unFeatured(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@Parameter(description = "userGroupList", required = true) List<Long> userGroupList) {
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
	@Operation(summary = "Find the value of Traits", description = "Returns the values of traits based on trait's ID", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = TraitsValue.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to get the values", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getValuesOfTraits(@Context HttpServletRequest request, @PathParam("traitId") String traitId) {
		try {
			List<TraitsValue> result = observationService.getTraitsValue(request, traitId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.SPECIES + "/{speciesGroupId}/{languageId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Find all Trait Values pair for Specific SpeciesId", description = "Return the Key value pairs of Traits", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = TraitsValuePair.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Species Not Found", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getTraitList(@PathParam("speciesGroupId") String speciesGroupId,
			@PathParam("languageId") String languageId) {
		try {
			List<TraitsValuePair> result = observationService.getTraitList(speciesGroupId, languageId);
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
	@Operation(summary = "Find all the user Permission for current observation", description = "Returns list of permission for validate post and feature in a group", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationUserPermission.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the permission", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getUserPermissions(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@Parameter(description = "taxonList") @DefaultValue("") @QueryParam("taxonList") String taxonList) {
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
	@Operation(summary = "Find the Suggestion for tags", description = "Return list of Top 10 tags matching the phrase", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Tags.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the tags", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Find all the userGroup Associated with a user", description = "Returns a List of UserGroup", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = UserGroupIbp.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to get the userGroup", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Flag an Observation", description = "Return a list of flag to the Observation", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = FlagShow.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to flag an Observation", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "406", description = "User has already flagged", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response createFlag(@Context HttpServletRequest request, @PathParam("observationId") String observationId,
			@Parameter(description = "flagIbp", required = true) FlagIbp flagIbp) {
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
	@Operation(summary = "Unflag an Observation", description = "Return a list of flag to the Observation", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = FlagShow.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to unflag an Observation", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "406", description = "User is not allowed to unflag", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Marks follow for a User", description = "Returns the follow details", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Follow.class))),
			@ApiResponse(responseCode = "400", description = "Unable to mark follow", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Marks unfollow for a User", description = "Returns the unfollow details", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Follow.class))),
			@ApiResponse(responseCode = "400", description = "Unable to mark unfollow", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Finds the authorId of the observation", description = "Returns the authorid of an observation", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the authorid", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Apply the new Filter Rule to post the Observation Existings", description = "Starts the process to apply the Rule", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to start the process", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Apply the new Filter Rule to unpost existing Observations", description = "Starts the process to apply the Rule", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to start the process", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Bulk update the geoPrivate traits in all observation", description = "Starts a process to update the geoPrivacy Field", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to start the process", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "406", description = "User not allowed to perform the task", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Finds the set of Values for a Custom Field", description = "Returns the Set of Values of Custom Field", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = CustomFieldValues.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to get the value list", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Insert/Update custom field Data", description = "Return a complete customField Data for the Observation", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = CustomFieldObservationData.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to add/Update the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response addUpdateCustomFieldData(@Context HttpServletRequest request,
			@Parameter(description = "factsCreateData", required = true) CustomFieldFactsInsert factsCreateData) {
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
	@Operation(summary = "Publish the observationId to RabbitMQ", description = "Return the result", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to push to RabbitMQ", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Get the observation create page data for UG context", description = "Returns the create page data", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationUGContextCreatePageData.class))),
			@ApiResponse(responseCode = "400", description = "Unable to get the data", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Create observation on UG context", description = "Returns the user observation id", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Long.class))),
			@ApiResponse(responseCode = "400", description = "Unable to create the observation", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response createObservationUGContext(@Context HttpServletRequest request,
			@Parameter(description = "observationUGContext", required = true) ObservationCreateUGContext observationUGContext) {
		try {
			if (observationUGContext.getObservationData().getObservedOn() == null)
				throw new ObservationInputException("Observation Date Cannot be BLANK");
			if (observationUGContext.getObservationData().getLatitude() == null
					|| observationUGContext.getObservationData().getLongitude() == null)
				throw new ObservationInputException("Observation LATITUDE/LONGITUDE MISSING");
			if (observationUGContext.getObservationData().getObservedAt() == null)
				throw new ObservationInputException("Observation LOCATION cannot be BLANK");
			if (observationUGContext.getObservationData().getsGroup() == null)
				throw new ObservationInputException("Species Group cannot be BLANK");
			if (observationUGContext.getObservationData().getHidePreciseLocation() == null)
				throw new ObservationInputException("GeoPrivacy cannot be BLANK");
			if (observationUGContext.getObservationData().getHelpIdentify() == false) {
				if (observationUGContext.getObservationData().getRecoData().getTaxonScientificName() == null
						&& observationUGContext.getObservationData().getRecoData().getTaxonCommonName() == null)
					throw new ObservationInputException("No Recommendation found");
			}
			if (observationHelper.checkObservationBounds(observationUGContext.getObservationData().getLatitude(),
					observationUGContext.getObservationData().getLongitude()) == false) {
				throw new ObservationInputException("Observation not within geographical bounds");
			}
			if (observationUGContext.getObservationData().getResources() == null
					|| observationUGContext.getObservationData().getResources().isEmpty()) {
				throw new ObservationInputException("Without resource observation");
			}

			Long result = observationService.creteObservationUGContext(request, observationUGContext);
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
	@Operation(summary = "Find the maxvoted reco permission for list page", description = "Return list of observationId with boolean value for permission", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = MaxVotedRecoPermission.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the result", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getPermissionListMaxVotedRecos(@Context HttpServletRequest request,
			@Parameter(description = "observationTaxonId", required = true) Map<Long, Long> observationTaxonId) {
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
	@Operation(summary = "Search for the permission in list page for a user", description = "Returns the permission details for the user", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ListPagePermissions.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the details", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getLisPagePermissions(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@Parameter(description = "taxonList") @DefaultValue("") @QueryParam("taxonList") String taxonList) {
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
	@Operation(summary = "Adds a comment", description = "Returns the current activity", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Activity.class))),
			@ApiResponse(responseCode = "400", description = "Unable to log a comment", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response addCommnet(@Context HttpServletRequest request,
			@Parameter(description = "commentData", required = true) CommentLoggingData commentDatas) {
		try {
			Activity result = observationService.addObservationComment(request, commentDatas);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@POST
	@Path(ApiConstants.DELETE + ApiConstants.COMMENT + "/{commentId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser
	@Operation(summary = "Deletes a comment", description = "Returns the current activity", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Activity.class))),
			@ApiResponse(responseCode = "400", description = "Unable to log a comment", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response deleteCommnet(@Context HttpServletRequest request,
			@Parameter(description = "commentData", required = true) CommentLoggingData commentDatas,
			@PathParam("commentId") String commentId) {
		try {
			Activity result = observationService.removeObservationComment(request, commentDatas, commentId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@POST
	@Path(ApiConstants.FIND)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Finds observation data on basis of resourceURl", description = "Returns the observation data", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationHomePage.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getObservation(@Parameter(description = "resourcesURLs", required = true) String resourcesUrl) {
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
	@Operation(summary = "Gets the observationData", description = "Returns the observation Data", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationListMinimalData.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Update the rating for gallery resource", description = "Returns the success or failure", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "404", description = "Unable to update the rating", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response gallaryRatingUpdate(@Context HttpServletRequest request,
			@PathParam("observationId") String observationId,
			@Parameter(description = "resourceRating", required = true) ResourceRating resourceRating) {
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
	@Path(ApiConstants.PUBLICATIONGRADE + "/{index}/{type}/{documentId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Fetch the observation based on the filter", description = "Returns the observation list based on the filters", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = PublicationGrade.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
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
	@Operation(summary = "Fetch the download log table based on filter", description = "Returns list of download log based on filter", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = DownloadLog.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response fetchDownloadLog(@DefaultValue("") @QueryParam("authorid") String authorId,
			@DefaultValue("") @QueryParam("filetype") String fileType,
			@DefaultValue("-1") @QueryParam("offset") String offSet,
			@DefaultValue("-1") @QueryParam("limit") String limit) {
		List<Long> authorIds = new ArrayList<Long>();
		if (!authorId.isEmpty() || authorId != null) {
			authorIds = Arrays.asList(authorId.split(",")).stream().map(Long::parseLong).collect(Collectors.toList());
		}
		List<DownloadLog> records = observationService.fetchDownloadLog(authorIds, fileType, Integer.parseInt(offSet),
				Integer.parseInt(limit));
		return Response.status(Status.OK).entity(records).build();
	}

	@GET
	@Path(ApiConstants.USERINFO + ApiConstants.UPLOADED + "/{userId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Search for the unique species uploaded by user", description = "Returns the total and unique species list", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationUserPageInfo.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getObservationUploadedUserInfo(@PathParam("userId") String userId,
			@QueryParam("sGroupId") String sGroupIds, @DefaultValue("true") @QueryParam("hasMedia") Boolean hasMedia,
			@DefaultValue("0") @QueryParam("offset") String offset) {

		try {
			Long user = Long.parseLong(userId);
			Long sGroupId = null;
			if (sGroupIds != null)
				sGroupId = Long.parseLong(sGroupIds);
			Long Offset = Long.parseLong(offset);
			ObservationUserPageInfo result = observationService.observationUploadInfo(user, sGroupId, hasMedia, Offset);

			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@GET
	@Path(ApiConstants.USERINFO + ApiConstants.IDENTIFIED + "/{userId}")
	@Consumes(MediaType.TEXT_PLAIN)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Search for the unique species identified by user", description = "Returns the total and unique species list", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationUserPageInfo.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getObservationIdentifiedUserInfo(@PathParam("userId") String userId,
			@QueryParam("sGroupId") String sGroupIds, @DefaultValue("true") @QueryParam("hasMedia") Boolean hasMedia,
			@DefaultValue("0") @QueryParam("offset") String offset) {
		try {
			Long user = Long.parseLong(userId);
			Long sGroupId = null;
			if (sGroupIds != null)
				sGroupId = Long.parseLong(sGroupIds);
			Long Offset = Long.parseLong(offset);
			ObservationUserPageInfo result = observationService.observationIdentifiedInfo(user, sGroupId, hasMedia,
					Offset);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@POST
	@Path(ApiConstants.BULK + ApiConstants.OBSERVATION)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser
	@Operation(summary = "Perform Bulk Upload of Observations", description = "Empty response", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Long.class))),
			@ApiResponse(responseCode = "400", description = "Unable to perform bulk upload", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response bulkObservationUpload(@Context HttpServletRequest request,
			@Parameter(description = "observationBulkData", required = true) ObservationBulkDTO observationBulkData) {
		try {

			if (!observationBulkData.getWktString().isEmpty()) {
				GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);
				WKTReader wktRdr = new WKTReader(geofactory);
				Geometry geoBoundary = wktRdr.read(observationBulkData.getWktString());
				Point intPoint = geoBoundary.getInteriorPoint();
				observationBulkData.setLongitude(intPoint.getX());
				observationBulkData.setLatitude(intPoint.getY());
			}

			if (observationHelper.checkObservationBounds(observationBulkData.getLatitude(),
					observationBulkData.getLongitude()) == false) {
				throw new ObservationInputException("Observation Not within Geographic Bounds");
			}
			Long result = observationDataTableService.observationBulkUpload(request, observationBulkData);
			if (result != null) {
				return Response.status(Status.OK).entity(result).build();
			} else {
				return Response.status(Status.BAD_REQUEST).entity(result).build();
			}
		} catch (Exception ex) {
			return Response.status(Status.BAD_REQUEST).entity(ex.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.DATATABLEOBSERVATION + "/{dataTableId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Return showDataTableObservation by datatable id", description = "Returns list of observations", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ShowObervationDataTable.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getObservationDatatableId(@Context HttpServletRequest request,
			@PathParam("dataTableId") String dataTableId, @DefaultValue("0") @QueryParam("offset") String Offset,
			@DefaultValue("12") @QueryParam("limit") String Limit) {

		try {
			Long id = Long.parseLong(dataTableId);
			Integer limit = Integer.parseInt(Limit);
			Integer offset = Integer.parseInt(Offset);
			ShowObervationDataTable result = observationDataTableService.showObservatioDataTable(request, id, limit,
					offset);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@PUT
	@Path("/update/userGroup/datatable/{dataTableId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Update user groups for a datatable", description = "Updates the user groups associated with a datatable", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = com.strandls.dataTable.pojo.UserGroupIbp.class, type = "array"))),
			@ApiResponse(responseCode = "400", description = "Unable to update user groups", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response updateDatatableUserGroups(@Context HttpServletRequest request,
			@PathParam("dataTableId") Long dataTableId,
			@Parameter(description = "DatatableUserGroupUpdateData", required = true) DatatableUserGroupUpdateData datatableUgUpdateData) {

		try {

			List<com.strandls.dataTable.pojo.UserGroupIbp> result = observationDataTableService
					.updateDatatableUsergroup(request, dataTableId, datatableUgUpdateData.getUserGroupList(),
							datatableUgUpdateData.getBulkAction());
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@GET
	@Path(ApiConstants.DATATABLEOBSERVATION + ApiConstants.LIST + "/{dataTableId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Return Observation list by datatable id", description = "Returns list of observations", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = ObservationDatatableList.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch the data", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getObservationDatatableList(@PathParam("dataTableId") String dataTableId,
			@DefaultValue("0") @QueryParam("offset") String Offset,
			@DefaultValue("10") @QueryParam("limit") String Limit) {

		try {
			Long id = Long.parseLong(dataTableId);
			Integer limit = Integer.parseInt(Limit);
			Integer offset = Integer.parseInt(Offset);
			ObservationDatatableList result = observationDataTableService.fetchAllObservationByDataTableId(id, limit,
					offset);

			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@DELETE
	@Path(ApiConstants.DATATABLEOBSERVATION + "/{dataTableId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser
	@Operation(summary = "Delete observations by datatable id", description = "Deletes observations associated with a given datatable ID", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "400", description = "Unable to delete observations", content = @Content(schema = @Schema(implementation = String.class))),
			@ApiResponse(responseCode = "404", description = "Datatable not found", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response deleteObservaionByDatatableId(@Context HttpServletRequest request,
			@PathParam("dataTableId") String dataTableId) {

		try {
			Long id = Long.parseLong(dataTableId);
			String result = observationDataTableService.removeObservationByDataTableId(request, id);
			if (result == null) {
				return Response.status(Status.NOT_FOUND).entity(result).build();
			} else {
				return Response.status(Status.OK).entity(result).build();
			}

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@POST
	@Path(ApiConstants.SPECIES + ApiConstants.PULL + "/{taxonId}")
	@Operation(summary = "Validate the observation pulled to speciesPage", description = "Returns Boolean Values", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Boolean.class))),
			@ApiResponse(responseCode = "400", description = "Unable to validate the Observations", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response speciesPullObservationValidation(@Context HttpServletRequest request,
			@PathParam("taxonId") String taxonId,
			@Parameter(description = "observationList", required = true) List<Long> observationIds) {
		try {
			Long taxId = Long.parseLong(taxonId);
			Boolean result = observationService.speciesObservationValidate(request, taxId, observationIds);
			return Response.status(Status.OK).entity(result).build();

		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
	}

	@GET
	@Path(ApiConstants.CROPIMAGERESOURCES + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Get image crop resources for an observation", description = "Returns the resources associated with an observation for image cropping", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Resources.class))),
			@ApiResponse(responseCode = "400", description = "Unable to fetch resources", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response getImageCropResources(@PathParam("observationId") Long observationId) {
		try {
			Resources result = observationService.getObservationResources(observationId);
			return Response.status(Status.OK).entity(result).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}

	@PUT
	@Path(ApiConstants.CROPIMAGERESOURCES + "/{observationId}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@ValidateUser
	@Operation(summary = "Update image crop resources for an observation", description = "Updates the resources associated with an observation for image cropping", responses = {
			@ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Resources.class))),
			@ApiResponse(responseCode = "400", description = "Unable to update resources", content = @Content(schema = @Schema(implementation = String.class))) })
	public Response updatedImageCropResources(@Context HttpServletRequest request,
			@PathParam("observationId") Long observationId,
			@Parameter(description = "resourcesUpdateData", required = true) Resources resourcesUpdateData) {

		try {

			Resources resultResources = observationService.updateObservationImageResources(request, observationId,
					resourcesUpdateData);

			return Response.status(Status.OK).entity(resultResources).build();
		} catch (Exception e) {
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}

	}
}