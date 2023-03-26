package com.strandls.observation.contorller;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.core.Response.Status;

import com.strandls.esmodule.pojo.MapBoundParams;
import com.strandls.esmodule.pojo.MapBounds;
import com.strandls.esmodule.pojo.MapGeoPoint;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.esmodule.pojo.MapSearchParams.SortTypeEnum;
import com.strandls.observation.ApiConstants;
import com.strandls.observation.es.util.ESUtility;
import com.strandls.observation.pojo.EsLocationListParams;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.ObservationListData;
import com.strandls.observation.pojo.ObservationStatistics;
import com.strandls.observation.service.ObservationListService;
import com.strandls.observation.util.PropertyFileUtil;

import io.swagger.annotations.ApiParam;

@Path(ApiConstants.PUBLIC + ApiConstants.V1 + ApiConstants.OBSERVATION)
public class ObservationPublicApiController {

	@Inject
	private ObservationListService observationListService;

	@Inject
	private ESUtility esUtility;

	@POST
	@Path(ApiConstants.STATS)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	public Response getObservationStatsData(@DefaultValue("") @QueryParam("sGroup") String sGroup,
			@DefaultValue("") @QueryParam("taxon") String taxon, @DefaultValue("") @QueryParam("user") String user,
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
			@QueryParam("tags") String tags, @ApiParam(name = "location") EsLocationListParams location,
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
			@QueryParam("bulkObservationIds") String bulkObservationIds, @QueryParam("city") String city,

			@Context HttpServletRequest request, @Context UriInfo uriInfo) {

		Properties properties = PropertyFileUtil.fetchProperty("config.properties");

		String whitelistedDomainApiKey = properties.getProperty("whitelisted_domain_api_key");
		String apiKey = request.getHeader("api-key");

		if (apiKey.equals(whitelistedDomainApiKey)) {

			MultivaluedMap<String, String> queryParams = uriInfo.getQueryParameters();
			Map<String, List<String>> traitParams = queryParams.entrySet().stream()
					.filter(entry -> entry.getKey().startsWith("trait"))
					.collect(Collectors.toMap(p -> p.getKey(), p -> p.getValue()));

			Map<String, List<String>> customParams = queryParams.entrySet().stream()
					.filter(entry -> entry.getKey().startsWith("custom"))
					.collect(Collectors.toMap(p -> p.getKey(), p -> p.getValue()));

			customParams.put("custom_17813311.field_text", Arrays.asList(city));

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

			MapAggregationResponse aggregationResult = null;
			MapAggregationStatsResponse aggregationStatsResult = null;

			aggregationResult = observationListService.mapAggregate("extended_observation", "_doc", sGroup, taxon, user,
					userGroupList, webaddress, speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate,
					traitParams, customParams, classificationid, mapSearchParams, maxVotedReco, recoId,
					createdOnMaxDate, createdOnMinDate, status, taxonId, recoName, geoAggregationField, rank, tahsil,
					district, state, tags, publicationGrade, authorVoted, dataSetName, dataTableName, geoEntity,
					dataTableId);

			aggregationStatsResult = observationListService.mapAggregateStats("extended_observation", "_doc", sGroup,
					taxon, user, userGroupList, webaddress, speciesName, mediaFilter, months, isFlagged, minDate,
					maxDate, validate, traitParams, customParams, classificationid, mapSearchParams, maxVotedReco,
					recoId, createdOnMaxDate, createdOnMinDate, status, taxonId, recoName, geoAggregationField, rank,
					tahsil, district, state, tags, publicationGrade, authorVoted, lifeListOffset, uploadersoffset,
					identifiersoffset, dataSetName, dataTableName, geoEntity, geoShapeFilterField, dataTableId);

			ObservationListData observationListData = observationListService.getObservationList("extended_observation",
					"_doc", mapSearchQuery, geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation,
					termsAggregationField, geoShapeFilterField, aggregationStatsResult, aggregationResult, view);

			ObservationStatistics result = new ObservationStatistics();
			result.setTitle("Some title");
			result.setObservations(observationListData.getTotalCount());
			result.setPeople(aggregationStatsResult.getTotalCounts().get("totalUploaders"));
			result.setSpecies(aggregationStatsResult.getTotalCounts().get("totalTaxa"));

			return Response.status(Status.OK)
					.header("Access-Control-Allow-Origin", "https://www.citynaturechallenge.org/").entity(result)
					.build();

		} else {
			return Response.status(Status.UNAUTHORIZED).entity(null).build();

		}
	}
}
