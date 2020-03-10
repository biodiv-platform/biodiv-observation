/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.inject.Inject;
import com.strandls.esmodule.ApiException;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.AggregationResponse;
import com.strandls.esmodule.pojo.FilterPanelData;
import com.strandls.esmodule.pojo.GeoHashAggregationData;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapResponse;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.observation.es.util.ESUtility;
import com.strandls.observation.es.util.ObservationIndex;
import com.strandls.observation.es.util.ObservationListMinimalData;
import com.strandls.observation.es.util.ObservationListPageMapper;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapTraitsAggregation;
import com.strandls.observation.pojo.ObservationListData;
import com.strandls.observation.service.ObservationListService;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationListServiceImpl implements ObservationListService {

	private final Logger logger = LoggerFactory.getLogger(ObservationListServiceImpl.class);

	@Inject
	private EsServicesApi esService;

	@Inject
	private ObjectMapper objectMapper;

	@Inject
	private ESUtility esUtility;

	@Override
	public ObservationListData getObservationList(String index, String type, MapSearchQuery querys,
			String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField, MapAggregationResponse aggregationResult, String view) {

		ObservationListData listData = null;

		try {
			Map<String, Long> geoHashResult = null;
			List<ObservationListPageMapper> observationList = new ArrayList<ObservationListPageMapper>();
			List<ObservationListMinimalData> observationListMinimal = new ArrayList<ObservationListMinimalData>();
			Long totalCount = null;
			if (view.equalsIgnoreCase("map")) {
				GeoHashAggregationData geoHashAggregationData = esService.getGeoHashAggregation(index, type,
						geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation, termsAggregationField,
						querys);
				geoHashResult = geoHashAggregationData.getGeoHashData();
				totalCount = geoHashAggregationData.getTotalCount();

			} else {

				MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
						onlyFilteredAggregation, termsAggregationField, querys);
				List<MapDocument> documents = result.getDocuments();
				totalCount = result.getTotalDocuments();

				if (view.equalsIgnoreCase("list_minimal")) {
					for (MapDocument document : documents) {
						try {

							observationListMinimal.add(objectMapper.readValue(String.valueOf(document.getDocument()),
									ObservationListMinimalData.class));
						} catch (IOException e) {
							logger.error(e.getMessage());
						}
					}

				} else {
					for (MapDocument document : documents) {
						try {

							observationList.add(objectMapper.readValue(String.valueOf(document.getDocument()),
									ObservationListPageMapper.class));
						} catch (IOException e) {
							logger.error(e.getMessage());
						}
					}

				}

			}

			listData = new ObservationListData(observationList, totalCount, geoHashResult, aggregationResult,observationListMinimal);

		} catch (ApiException e) {
			logger.error(e.getMessage());
		}
		return listData;

	}

	private AggregationResponse getAggregate(String index, String type, String filter, String geoAggregationField,
			MapSearchQuery searchQuery) {
		try {
			AggregationResponse response = esService.getAggregation(index, type, filter, geoAggregationField,
					searchQuery);

			return response;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public MapAggregationResponse mapAggregate(String index, String type, String sGroup, String taxon, String user,
			String userGroupList, String webaddress, String speciesName, String mediaFilter, String months,
			String isFlagged, String minDate, String maxDate, String validate, Map<String, List<String>> traitParams,
			Map<String, List<String>> customParams, String classificationid, MapSearchParams mapSearchParams,
			String maxvotedrecoid, String createdOnMaxDate, String createdOnMinDate, String status, String taxonId,
			String recoName, String geoAggregationField, String rank, String tahsil, String district, String state) {

		MapSearchQuery mapSearchQuery = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
				speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
				classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status, taxonId,
				recoName, rank, tahsil, district, state);

		MapSearchQuery mapSearchQueryFilter;

		String omiter = null;
		MapAggregationResponse aggregationResponse = new MapAggregationResponse();

		if (sGroup != null) {

			mapSearchQueryFilter = esUtility.getMapSearchQuery(omiter, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state);
			aggregationResponse.setGroupSpeciesName(
					getAggregate(index, type, "group_name.keyword", geoAggregationField, mapSearchQueryFilter)
							.getGroupAggregation());
		} else {
			aggregationResponse.setGroupSpeciesName(
					getAggregate(index, type, "group_name.keyword", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}
		if (status != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, omiter,
					taxonId, recoName, rank, tahsil, district, state);
			aggregationResponse.setGroupStatus(getAggregate(index, type, "max_voted_reco.taxonstatus.keyword",
					geoAggregationField, mapSearchQueryFilter).getGroupAggregation());
		} else {
			aggregationResponse.setGroupStatus(
					getAggregate(index, type, "max_voted_reco.taxonstatus.keyword", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}

		if (rank != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, omiter, tahsil, district, state);
			aggregationResponse.setGroupRank(getAggregate(index, type, "max_voted_reco.ranktext.keyword",
					geoAggregationField, mapSearchQueryFilter).getGroupAggregation());
		} else {
			aggregationResponse.setGroupRank(
					getAggregate(index, type, "max_voted_reco.ranktext.keyword", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}

		if (state != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, omiter);
			aggregationResponse.setGroupRank(getAggregate(index, type, "location_information.state.keyword",
					geoAggregationField, mapSearchQueryFilter).getGroupAggregation());

		} else {
			aggregationResponse.setGroupRank(
					getAggregate(index, type, "location_information.state.keyword", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}

		if (userGroupList != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, omiter, webaddress, speciesName,
					mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state);
			aggregationResponse.setGroupUserGroupName(getAggregate(index, type, "user_group_observations.name.keyword",
					geoAggregationField, mapSearchQueryFilter).getGroupAggregation());
		} else {
			aggregationResponse.setGroupUserGroupName(getAggregate(index, type, "user_group_observations.name.keyword",
					geoAggregationField, mapSearchQuery).getGroupAggregation());
		}
		if (isFlagged != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, omiter, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state);
			aggregationResponse
					.setGroupFlag(getAggregate(index, type, "flag_count", geoAggregationField, mapSearchQueryFilter)
							.getGroupAggregation());
		} else {
			aggregationResponse.setGroupFlag(
					getAggregate(index, type, "flag_count", geoAggregationField, mapSearchQuery).getGroupAggregation());
		}
		if (validate != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, omiter, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state);
			aggregationResponse
					.setGroupValidate(getAggregate(index, type, "is_locked", geoAggregationField, mapSearchQueryFilter)
							.getGroupAggregation());
		} else {
			aggregationResponse.setGroupValidate(
					getAggregate(index, type, "is_locked", geoAggregationField, mapSearchQuery).getGroupAggregation());
		}
		if (months != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, omiter, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state);
			aggregationResponse.setGroupMonth(
					getAggregate(index, type, "observed_in_month.keyword", geoAggregationField, mapSearchQueryFilter)
							.getGroupAggregation());
		} else {
			aggregationResponse.setGroupMonth(
					getAggregate(index, type, "observed_in_month.keyword", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}
		if (mediaFilter != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state);

			aggregationResponse.setGroupAudio(
					getTotal(getAggregate(index, type, "no_of_audio", geoAggregationField, mapSearchQuery)
							.getGroupAggregation()));
			aggregationResponse.setGroupVideo(
					getTotal(getAggregate(index, type, "no_of_videos", geoAggregationField, mapSearchQuery)
							.getGroupAggregation()));
			aggregationResponse.setGroupImages(
					getTotal(getAggregate(index, type, "no_of_images", geoAggregationField, mapSearchQuery)
							.getGroupAggregation()));
			aggregationResponse.setGroupNoMedia(getTotal(
					getAggregate(index, type, "no_media", geoAggregationField, mapSearchQuery).getGroupAggregation()));

		} else {
			aggregationResponse.setGroupAudio(
					getTotal(getAggregate(index, type, "no_of_audio", geoAggregationField, mapSearchQuery)
							.getGroupAggregation()));
			aggregationResponse.setGroupVideo(
					getTotal(getAggregate(index, type, "no_of_videos", geoAggregationField, mapSearchQuery)
							.getGroupAggregation()));
			aggregationResponse.setGroupImages(
					getTotal(getAggregate(index, type, "no_of_images", geoAggregationField, mapSearchQuery)
							.getGroupAggregation()));
			aggregationResponse.setGroupNoMedia(getTotal(
					getAggregate(index, type, "no_media", geoAggregationField, mapSearchQuery).getGroupAggregation()));

		}
		if (speciesName != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress, omiter,
					mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state);
			aggregationResponse.setGroupIdentificationNameExists(
					getAggregate(index, type, "max_voted_reco", geoAggregationField, mapSearchQueryFilter)
							.getGroupAggregation());

		} else {
			aggregationResponse.setGroupIdentificationNameExists(
					getAggregate(index, type, "max_voted_reco", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}
		if (taxonId != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					omiter, recoName, rank, tahsil, district, state);
			aggregationResponse.setGroupTaxonIDExists(
					getAggregate(index, type, "max_voted_reco.taxonstatus", geoAggregationField, mapSearchQueryFilter)
							.getGroupAggregation());

		} else {
			aggregationResponse.setGroupTaxonIDExists(
					getAggregate(index, type, "max_voted_reco.taxonstatus", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}

		if (!traitParams.isEmpty()) {
			MapTraitsAggregation traitAggregation = new MapTraitsAggregation();
			List<String> temp = new ArrayList<String>();

			if (traitParams.containsKey("trait_8.string")) {
				temp = traitParams.remove("trait_8.string");
				mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
						speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
						customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
						createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state);

				traitAggregation
						.setTrait_8(
								getTraitsAggregation(
										getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
												geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
										"Sex"));
				traitParams.put("trait_8.string", temp);
			} else {

				traitAggregation.setTrait_8(getTraitsAggregation(
						getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
								geoAggregationField, mapSearchQuery).getGroupAggregation(),
						"Sex"));
			}
			if (traitParams.containsKey("trait_11.string")) {
				temp = traitParams.remove("trait_11.string");
				mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
						speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
						customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
						createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state);
				traitAggregation
						.setTrait_11(getTraitsAggregation(
								getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
										geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
								"Phenology State"));
				traitParams.put("trait_11.string", temp);
			} else {
				traitAggregation
						.setTrait_11(
								getTraitsAggregation(
										getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
												geoAggregationField, mapSearchQuery).getGroupAggregation(),
										"Phenology State"));
			}
			if (traitParams.containsKey("trait_13.string")) {
				temp = traitParams.remove("trait_13.string");
				mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
						speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
						customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
						createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state);
				traitAggregation
						.setTrait_13(
								getTraitsAggregation(
										getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
												geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
										"Abundance"));
				traitParams.put("trait_13.string", temp);
			} else {
				traitAggregation
						.setTrait_13(
								getTraitsAggregation(
										getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
												geoAggregationField, mapSearchQuery).getGroupAggregation(),
										"Abundance"));
			}

			if (traitParams.containsKey("trait_9.string")) {
				temp = traitParams.remove("trait_9.string");
				mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
						speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
						customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
						createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state);
				traitAggregation.setTrait_9(getTraitsAggregation(
						getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
								geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
						"Life Stage (Complete Metamorphosis)"));
				traitParams.put("trait_9.string", temp);
			} else {
				traitAggregation.setTrait_9(getTraitsAggregation(
						getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
								geoAggregationField, mapSearchQuery).getGroupAggregation(),
						"Life Stage (Complete Metamorphosis)"));
			}
			if (traitParams.containsKey("trait_12.string")) {
				temp = traitParams.remove("trait_12.string");
				mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
						speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
						customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
						createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state);
				traitAggregation
						.setTrait_12(getTraitsAggregation(
								getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
										geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
								"Habitat Type"));
				traitParams.put("trait_12.string", temp);
			} else {
				traitAggregation
						.setTrait_12(
								getTraitsAggregation(
										getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
												geoAggregationField, mapSearchQuery).getGroupAggregation(),
										"Habitat Type"));
			}
			if (traitParams.containsKey("trait_10.string")) {
				temp = traitParams.remove("trait_10.string");
				mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
						speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
						customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
						createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state);
				traitAggregation.setTrait_10(getTraitsAggregation(
						getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
								geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
						"Life Stage (Incomplete Metamorphosis)"));
				traitParams.put("trait_10.string", temp);
			} else {
				traitAggregation.setTrait_10(getTraitsAggregation(
						getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
								geoAggregationField, mapSearchQuery).getGroupAggregation(),
						"Life Stage (Incomplete Metamorphosis)"));
			}

			aggregationResponse.setTraits(traitAggregation);
		} else {
			MapTraitsAggregation traitAggregation = new MapTraitsAggregation();
			traitAggregation
					.setTrait_11(
							getTraitsAggregation(
									getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
											geoAggregationField, mapSearchQuery).getGroupAggregation(),
									"Phenology State"));
			traitAggregation.setTrait_13(
					getTraitsAggregation(getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
							geoAggregationField, mapSearchQuery).getGroupAggregation(), "Abundance"));
			traitAggregation.setTrait_8(
					getTraitsAggregation(getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
							geoAggregationField, mapSearchQuery).getGroupAggregation(), "Sex"));
			traitAggregation
					.setTrait_9(getTraitsAggregation(
							getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
									geoAggregationField, mapSearchQuery).getGroupAggregation(),
							"Life Stage (Complete Metamorphosis)"));
			traitAggregation
					.setTrait_12(
							getTraitsAggregation(
									getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
											geoAggregationField, mapSearchQuery).getGroupAggregation(),
									"Habitat Type"));
			traitAggregation
					.setTrait_10(getTraitsAggregation(
							getAggregate(index, type, "facts.trait_value.trait_aggregation_field.keyword",
									geoAggregationField, mapSearchQuery).getGroupAggregation(),
							"Life Stage (Incomplete Metamorphosis)"));
			aggregationResponse.setTraits(traitAggregation);
		}

		return aggregationResponse;

	}

	private Long getTotal(Map<String, Long> media) {
		Long sum = 0L;

		for (Map.Entry<String, Long> entry : media.entrySet()) {
			if (!(entry.getKey().equals("0"))) {
				sum += entry.getValue();
			}
		}
		return sum;
	}

	private Map<String, Long> getTraitsAggregation(Map<String, Long> aggregation, String traitName) {
		Map<String, Long> traitsAgg = new HashMap<String, Long>();

		for (Entry<String, Long> entry : aggregation.entrySet()) {
			if (entry.getKey().startsWith(traitName)) {
				traitsAgg.put(entry.getKey(), entry.getValue());
			}
		}
		return traitsAgg;

	}

	@Override
	public FilterPanelData getAllFilter() {
		FilterPanelData result = null;
		try {
			result = esService.getFilterLists(ObservationIndex.index.getValue(), ObservationIndex.type.getValue());

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

}
