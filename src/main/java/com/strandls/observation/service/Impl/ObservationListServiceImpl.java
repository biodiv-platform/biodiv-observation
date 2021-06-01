/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.CountDownLatch;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.ApiException;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.AggregationResponse;
import com.strandls.esmodule.pojo.CustomFields;
import com.strandls.esmodule.pojo.FilterPanelData;
import com.strandls.esmodule.pojo.GeoHashAggregationData;
import com.strandls.esmodule.pojo.IdentifiersInfo;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapResponse;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.esmodule.pojo.Traits;
import com.strandls.esmodule.pojo.UploadersInfo;
import com.strandls.observation.es.util.ESUtility;
import com.strandls.observation.es.util.ObservationIndex;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.es.util.ObservationListMinimalData;
import com.strandls.observation.es.util.ObservationListPageMapper;
import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.ObservationHomePage;
import com.strandls.observation.pojo.ObservationListData;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoShow;
import com.strandls.observation.pojo.TopIdentifiersInfo;
import com.strandls.observation.pojo.TopUploadersInfo;
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
			String termsAggregationField, MapAggregationResponse aggregationResult,
			MapAggregationStatsResponse aggregationStatsResult, String view) {

		ObservationListData listData = null;

		try {
			Map<String, Long> geoHashResult = null;
			List<ObservationListPageMapper> observationList = new ArrayList<ObservationListPageMapper>();
			List<ObservationListMinimalData> observationListMinimal = new ArrayList<ObservationListMinimalData>();
			Long totalCount = null;
			MapAggregationStatsResponse statsAggregates = null;
			if (view.equalsIgnoreCase("map")) {
				GeoHashAggregationData geoHashAggregationData = esService.getGeoHashAggregation(index, type,
						geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation, termsAggregationField,
						querys);
				geoHashResult = geoHashAggregationData.getGeoHashData();
				totalCount = geoHashAggregationData.getTotalCount();

			}

			else if (view.equalsIgnoreCase("stats")) {

				statsAggregates = aggregationStatsResult;

			}

			else {

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
							ObservationListPageMapper observationMapper = objectMapper
									.readValue(String.valueOf(document.getDocument()), ObservationListPageMapper.class);
							if (observationMapper.getRecoShow() != null) {
								int targetIndex = 0;
								int flag = 0;

								RecoShow recoShow = observationMapper.getRecoShow();
								RecoIbp recoIbp = recoShow.getRecoIbp();
								List<AllRecoSugguestions> allRecoVote = recoShow.getAllRecoVotes();
								for (AllRecoSugguestions allrecoSuggestion : allRecoVote) {
									if (recoIbp.getTaxonId() != null && allrecoSuggestion.getTaxonId() != null
											&& recoIbp.getTaxonId().equals(allrecoSuggestion.getTaxonId())) {
										flag = 1;
										break;
									}

									if (recoIbp.getScientificName() != null
											&& allrecoSuggestion.getScientificName() != null
											&& !recoIbp.getScientificName().isEmpty()
											&& !allrecoSuggestion.getScientificName().isEmpty()
											&& recoIbp.getScientificName()
													.equalsIgnoreCase(allrecoSuggestion.getScientificName())) {
										flag = 1;
										break;
									}

									if (recoIbp.getCommonName() != null && allrecoSuggestion.getCommonName() != null
											&& !recoIbp.getCommonName().isEmpty()
											&& !allrecoSuggestion.getCommonName().isEmpty() && recoIbp.getCommonName()
													.equalsIgnoreCase(allrecoSuggestion.getCommonName())) {
										flag = 1;
										break;
									}

									targetIndex++;

								}

								if (targetIndex != 0 && flag == 1) {
									Collections.swap(allRecoVote, 0, targetIndex);
									recoShow.setAllRecoVotes(allRecoVote);
									observationMapper.setRecoShow(recoShow);
								}

							}
							observationList.add(observationMapper);
						} catch (IOException e) {
							logger.error(e.getMessage());
						}
					}

				}

			}

			listData = new ObservationListData(observationList, totalCount, geoHashResult, aggregationResult,
					statsAggregates, observationListMinimal);

		} catch (ApiException e) {
			logger.error(e.getMessage());
		}
		return listData;

	}

	private void getAggregateLatch(String index, String type, String filter, String geoAggregationField,
			MapSearchQuery searchQuery, Map<String, AggregationResponse> mapResponse, CountDownLatch latch,
			String namedAgg) {

		LatchThreadWorker worker = new LatchThreadWorker(index, type, filter, geoAggregationField, searchQuery,
				mapResponse, namedAgg, latch, esService);
		worker.start();

	}

	@Override
	public MapAggregationResponse mapAggregate(String index, String type, String sGroup, String taxon, String user,
			String userGroupList, String webaddress, String speciesName, String mediaFilter, String months,
			String isFlagged, String minDate, String maxDate, String validate, Map<String, List<String>> traitParams,
			Map<String, List<String>> customParams, String classificationid, MapSearchParams mapSearchParams,
			String maxvotedrecoid, String recoId, String createdOnMaxDate, String createdOnMinDate, String status,
			String taxonId, String recoName, String geoAggregationField, String rank, String tahsil, String district,
			String state, String tags, String publicationGrade, String authorVoted) {

		MapSearchQuery mapSearchQuery = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
				speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
				classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate, status,
				taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

		MapSearchQuery mapSearchQueryFilter;

		String omiter = null;
		MapAggregationResponse aggregationResponse = new MapAggregationResponse();

		Map<String, AggregationResponse> mapAggResponse = new HashMap<String, AggregationResponse>();

//		filter panel data

		FilterPanelData filterList = null;
		try {
			filterList = esService.getFilterLists(ObservationIndex.index.getValue(), ObservationIndex.type.getValue());
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		List<Traits> traitList = null;
		List<CustomFields> customFieldList = null;
		if (filterList != null) {
			traitList = filterList.getTraits();
			customFieldList = filterList.getCustomFields();
		}

		int totalLatch = 14 + traitList.size() + customFieldList.size();
//		latch count down
		CountDownLatch latch = new CountDownLatch(totalLatch);

		if (sGroup != null && !sGroup.isEmpty()) {

			mapSearchQueryFilter = esUtility.getMapSearchQuery(omiter, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "group_name.keyword", geoAggregationField, mapSearchQueryFilter,
					mapAggResponse, latch, null);

		} else {
			getAggregateLatch(index, type, "group_name.keyword", geoAggregationField, mapSearchQuery, mapAggResponse,
					latch, null);

		}
		if (status != null && !status.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					omiter, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "max_voted_reco.taxonstatus.keyword", geoAggregationField,
					mapSearchQueryFilter, mapAggResponse, latch, null);

		} else {

			getAggregateLatch(index, type, "max_voted_reco.taxonstatus.keyword", geoAggregationField, mapSearchQuery,
					mapAggResponse, latch, null);
		}

		if (rank != null && !rank.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, omiter, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "max_voted_reco.ranktext.keyword", geoAggregationField, mapSearchQueryFilter,
					mapAggResponse, latch, null);
		} else {

			getAggregateLatch(index, type, "max_voted_reco.ranktext.keyword", geoAggregationField, mapSearchQuery,
					mapAggResponse, latch, null);
		}

		if (state != null && !state.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, omiter, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "location_information.state.raw", geoAggregationField, mapSearchQueryFilter,
					mapAggResponse, latch, null);

		} else {

			getAggregateLatch(index, type, "location_information.state.raw", geoAggregationField, mapSearchQuery,
					mapAggResponse, latch, null);
		}

		if (userGroupList != null && !userGroupList.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, omiter, webaddress, speciesName,
					mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "user_group_observations.name.keyword", geoAggregationField,
					mapSearchQueryFilter, mapAggResponse, latch, null);

		} else {

			getAggregateLatch(index, type, "user_group_observations.name.keyword", geoAggregationField, mapSearchQuery,
					mapAggResponse, latch, null);

		}
		if (isFlagged != null && !isFlagged.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, omiter, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "flag_count", geoAggregationField, mapSearchQueryFilter, mapAggResponse,
					latch, null);

		} else {

			getAggregateLatch(index, type, "flag_count", geoAggregationField, mapSearchQuery, mapAggResponse, latch,
					null);
		}
		if (validate != null && !validate.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, omiter, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "is_locked", geoAggregationField, mapSearchQueryFilter, mapAggResponse,
					latch, null);
		} else {

			getAggregateLatch(index, type, "is_locked", geoAggregationField, mapSearchQuery, mapAggResponse, latch,
					null);

		}
		if (months != null && !months.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, omiter, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "observed_in_month.keyword", geoAggregationField, mapSearchQueryFilter,
					mapAggResponse, latch, null);

		} else {

			getAggregateLatch(index, type, "observed_in_month.keyword", geoAggregationField, mapSearchQuery,
					mapAggResponse, latch, null);
		}
		if (mediaFilter != null && !mediaFilter.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "no_of_audio", geoAggregationField, mapSearchQueryFilter, mapAggResponse,
					latch, null);
			getAggregateLatch(index, type, "no_of_videos", geoAggregationField, mapSearchQueryFilter, mapAggResponse,
					latch, null);
			getAggregateLatch(index, type, "no_of_images", geoAggregationField, mapSearchQueryFilter, mapAggResponse,
					latch, null);
			getAggregateLatch(index, type, "no_media", geoAggregationField, mapSearchQueryFilter, mapAggResponse, latch,
					null);

		} else {

			getAggregateLatch(index, type, "no_of_audio", geoAggregationField, mapSearchQuery, mapAggResponse, latch,
					null);
			getAggregateLatch(index, type, "no_of_videos", geoAggregationField, mapSearchQuery, mapAggResponse, latch,
					null);
			getAggregateLatch(index, type, "no_of_images", geoAggregationField, mapSearchQuery, mapAggResponse, latch,
					null);
			getAggregateLatch(index, type, "no_media", geoAggregationField, mapSearchQuery, mapAggResponse, latch,
					null);

		}
		if (speciesName != null && !speciesName.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress, omiter,
					mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "no_of_identifications", geoAggregationField, mapSearchQueryFilter,
					mapAggResponse, latch, null);

		} else {

			getAggregateLatch(index, type, "no_of_identifications", geoAggregationField, mapSearchQuery, mapAggResponse,
					latch, null);

		}
		if (taxonId != null && !taxonId.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, omiter, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "max_voted_reco.taxonstatus", geoAggregationField, mapSearchQueryFilter,
					mapAggResponse, latch, null);

		} else {

			getAggregateLatch(index, type, "max_voted_reco.taxonstatus", geoAggregationField, mapSearchQuery,
					mapAggResponse, latch, null);
		}

//		new trait aggregation

		Map<String, Map<String, Long>> traitMaps = new HashMap<String, Map<String, Long>>();
		for (Traits trait : traitList) {
			String keyword = "trait_" + trait.getId() + "." + trait.getType();
			if (!traitParams.isEmpty()) {
				List<String> tempTraitParams = new ArrayList<String>();
				if (traitParams.containsKey(keyword)) {
					tempTraitParams = traitParams.remove(keyword);

					mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
							speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
							customParams, classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate,
							createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state, tags,
							publicationGrade, authorVoted);

					getAggregateLatch(index, type, "facts.trait_value.trait_aggregation.raw", geoAggregationField,
							mapSearchQueryFilter, mapAggResponse, latch, trait.getName());

					traitParams.put(keyword, tempTraitParams);
				}
			}
			if (traitParams.isEmpty() || !(traitParams.containsKey(keyword))) {
				getAggregateLatch(index, type, "facts.trait_value.trait_aggregation.raw", geoAggregationField,
						mapSearchQuery, mapAggResponse, latch, trait.getName());
			}
		}

//		custom Field Aggregation Start
		String namedAggs = "";

		Map<String, Map<String, Long>> cfMaps = new HashMap<String, Map<String, Long>>();
		List<String> tempCFParams = new ArrayList<String>();
		for (CustomFields cf : customFieldList) {
			String keyword = "custom_" + cf.getId() + "." + cf.getFieldtype();
			String fieldType = cf.getFieldtype();

			namedAggs = cf.getId().toString() + "||" + fieldType;
			if (!customParams.isEmpty()) {
				tempCFParams = customParams.remove(keyword);

				mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
						speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
						customParams, classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate,
						createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state, tags,
						publicationGrade, authorVoted);

				getAggregateLatch(index, type,
						"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
						geoAggregationField, mapSearchQueryFilter, mapAggResponse, latch, namedAggs);

				customParams.put(keyword, tempCFParams);
			}
			if (customParams.isEmpty() || !customParams.containsKey(keyword)) {
				getAggregateLatch(index, type,
						"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
						geoAggregationField, mapSearchQuery, mapAggResponse, latch, namedAggs);

			}
		}
//		custom Field Aggregation ENDS

		try {
			latch.await();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		aggregationResponse.setGroupSpeciesName(mapAggResponse.get("group_name.keyword").getGroupAggregation());
		aggregationResponse
				.setGroupStatus(mapAggResponse.get("max_voted_reco.taxonstatus.keyword").getGroupAggregation());
		aggregationResponse.setGroupRank(mapAggResponse.get("max_voted_reco.ranktext.keyword").getGroupAggregation());
		aggregationResponse.setGroupState(mapAggResponse.get("location_information.state.raw").getGroupAggregation());
		aggregationResponse.setGroupUserGroupName(
				mapAggResponse.get("user_group_observations.name.keyword").getGroupAggregation());
		aggregationResponse.setGroupFlag(mapAggResponse.get("flag_count").getGroupAggregation());
		aggregationResponse.setGroupValidate(mapAggResponse.get("is_locked").getGroupAggregation());
		aggregationResponse.setGroupMonth(mapAggResponse.get("observed_in_month.keyword").getGroupAggregation());
		aggregationResponse.setGroupAudio(getTotal(mapAggResponse.get("no_of_audio").getGroupAggregation()));
		aggregationResponse.setGroupVideo(getTotal(mapAggResponse.get("no_of_videos").getGroupAggregation()));
		aggregationResponse.setGroupImages(getTotal(mapAggResponse.get("no_of_images").getGroupAggregation()));
		aggregationResponse.setGroupNoMedia(getTotal(mapAggResponse.get("no_media").getGroupAggregation()));
		aggregationResponse.setGroupIdentificationNameExists(
				getIdentificationSum(mapAggResponse.get("no_of_identifications").getGroupAggregation()));
		aggregationResponse
				.setGroupTaxonIDExists(mapAggResponse.get("max_voted_reco.taxonstatus").getGroupAggregation());

//		record traits aggregation
		for (Traits traits : traitList) {
			traitMaps.put(traits.getName(),
					getTraitsAggregation(mapAggResponse.get(traits.getName()).getGroupAggregation(), traits.getName()));
		}
		aggregationResponse.setGroupTraits(traitMaps);

//		record custom field aggreation
		for (CustomFields cf : customFieldList) {
			String fieldType = cf.getFieldtype();
			namedAggs = cf.getId().toString() + "||" + fieldType;
			if (fieldType.equalsIgnoreCase("FIELD TEXT")) {

				cfMaps.put(cf.getName(), getCustomFieldAggregationFieldText(
						mapAggResponse.get(namedAggs).getGroupAggregation(), fieldType, cf.getId().toString()));

			} else if (fieldType.equalsIgnoreCase("SINGLE CATEGORICAL")
					|| fieldType.equalsIgnoreCase("MULTIPLE CATEGORICAL")) {

				cfMaps.put(cf.getName(), getCustomFieldAggregationCategorical(
						mapAggResponse.get(namedAggs).getGroupAggregation(), fieldType, cf.getId().toString()));

			} else {
//				field type range
				cfMaps.put(cf.getName(),
						getCustomFieldAggregationRange(mapAggResponse.get(namedAggs).getGroupAggregation(),
								cf.getDataType(), fieldType, cf.getId().toString()));
			}
		}
		aggregationResponse.setGroupCustomField(cfMaps);

		return aggregationResponse;

	}

	@Override
	public MapAggregationStatsResponse mapAggregateStats(String index, String type, String sGroup, String taxon,
			String user, String userGroupList, String webaddress, String speciesName, String mediaFilter, String months,
			String isFlagged, String minDate, String maxDate, String validate, Map<String, List<String>> traitParams,
			Map<String, List<String>> customParams, String classificationid, MapSearchParams mapSearchParams,
			String maxvotedrecoid, String recoId, String createdOnMaxDate, String createdOnMinDate, String status,
			String taxonId, String recoName, String geoAggregationField, String rank, String tahsil, String district,
			String state, String tags, String publicationGrade, String authorVoted, Integer lifeListOffset,
			Integer uploadersoffset, Integer identifiersoffset) {

		MapSearchQuery mapSearchQuery = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
				speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
				classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate, status,
				taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

		MapSearchQuery mapSearchQueryFilter;

		String omiter = null;
		MapAggregationStatsResponse aggregationStatsResponse = new MapAggregationStatsResponse();

		Map<String, AggregationResponse> mapAggStatsResponse = new HashMap<String, AggregationResponse>();

		int totalLatch = 3;
//		latch count down
		CountDownLatch latch = new CountDownLatch(totalLatch);

		getAggregateLatch(index, type, "max_voted_reco.scientific_name.keyword", geoAggregationField, mapSearchQuery,
				mapAggStatsResponse, latch, null);

		// for top Uploaders

		if (user != null && !user.isEmpty()) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, omiter, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, recoId, createdOnMaxDate, createdOnMinDate,
					status, taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade, authorVoted);

			getAggregateLatch(index, type, "author_id", geoAggregationField, mapSearchQueryFilter, mapAggStatsResponse,
					latch, null);
			getAggregateLatch(index, type, "all_reco_vote.authors_voted.id", geoAggregationField, mapSearchQueryFilter,
					mapAggStatsResponse, latch, null);

		} else {
			getAggregateLatch(index, type, "author_id", geoAggregationField, mapSearchQuery, mapAggStatsResponse, latch,
					null);
			getAggregateLatch(index, type, "all_reco_vote.authors_voted.id", geoAggregationField, mapSearchQuery,
					mapAggStatsResponse, latch, null);

		}

		try {
			latch.await();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		int size = lifeListOffset + 10;
		int count = 1;

		Map<String, Long> temp = mapAggStatsResponse.get("max_voted_reco.scientific_name.keyword")
				.getGroupAggregation();
		Map<String, Long> t = new LinkedHashMap<>();

		for (Map.Entry<String, Long> entry : temp.entrySet()) {
			if (count <= (size - 10)) {
				count++;
			} else {
				if (count > size) {
					break;
				}
				t.put(entry.getKey(), entry.getValue());
				count++;
			}
		}
		aggregationStatsResponse.setGroupUniqueSpecies(t);

		Map<String, Long> uploaders = mapAggStatsResponse.get("author_id").getGroupAggregation();
		List<TopUploadersInfo> uploadersResult = extractUploaders(uploadersoffset, user, uploaders);
		aggregationStatsResponse.setGroupTopUploaders(uploadersResult);

		Map<String, Long> identifiers = mapAggStatsResponse.get("all_reco_vote.authors_voted.id").getGroupAggregation();
		List<TopIdentifiersInfo> identifiersResult = extractIdentifiers(identifiersoffset, user, identifiers);
		aggregationStatsResponse.setGroupTopIdentifiers(identifiersResult);

		Long totalUploaders = Long.valueOf(0);
		Long totalIdentifiers = Long.valueOf(0);
		Long totalTaxa = Long.valueOf(temp.size());

		if (user != null && !user.isEmpty()) {
			totalUploaders = Long.valueOf(uploadersResult.size());
			totalIdentifiers = Long.valueOf(identifiersResult.size());
		} else {
			totalUploaders = Long.valueOf(uploaders.size());
			totalIdentifiers = Long.valueOf(identifiers.size());
		}

		Map<String, Long> totals = new HashMap<>();
		totals.put("totalTaxa", totalTaxa);
		totals.put("totalUploaders", totalUploaders);
		totals.put("totalIdentifiers", totalIdentifiers);

		aggregationStatsResponse.setTotalCounts(totals);

		return aggregationStatsResponse;
	}

	private List<TopIdentifiersInfo> extractIdentifiers(Integer identifierssoffset, String user,
			Map<String, Long> identifiers) {
		int identifiersSize = identifierssoffset + 10;
		int identifiersCount = 1;
		String authorIds = "";
		List<Long> counts = new ArrayList<>();
		if (user != null && !user.isEmpty()) {
			List<String> l = Arrays.asList(user.split(","));
			for (int i = 0; i < l.size(); i++) {
				authorIds = authorIds + l.get(i) + ",";
				if (identifiers.containsKey(l.get(i))) {
					counts.add(identifiers.get(l.get(i)));
				} else {
					counts.add(Long.valueOf(0));
				}
			}

		} else {
			for (Map.Entry<String, Long> entry : identifiers.entrySet()) {
				if (identifiersCount <= (identifiersSize - 10)) {
					identifiersCount++;
				} else {
					if (identifiersCount > identifiersSize) {
						break;
					}
					entry.getValue();
					authorIds = authorIds + entry.getKey() + ",";
					counts.add(entry.getValue());
					identifiersCount++;
				}
			}
		}

		try {
			List<IdentifiersInfo> allIdentifiersInfo = esService.getIdentifierInfo("extended_observation", authorIds);
			List<TopIdentifiersInfo> identifiersResult = new ArrayList<>();
			for (int k = 0; k < allIdentifiersInfo.size(); k++) {
				String name = allIdentifiersInfo.get(k).getName();
				String pic = allIdentifiersInfo.get(k).getPic();
				Long authorId = allIdentifiersInfo.get(k).getAuthorId();
				TopIdentifiersInfo tempUploader = new TopIdentifiersInfo(name, pic, authorId, counts.get(k));
				identifiersResult.add(tempUploader);
			}

			return (identifiersResult);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return (null);
	}

	private List<TopUploadersInfo> extractUploaders(Integer uploadersoffset, String user, Map<String, Long> uploaders) {
		int uploadersSize = uploadersoffset + 10;
		int uploadersCount = 1;
		String authorIds = "";
		List<Long> counts = new ArrayList<>();
		if (user != null && !user.isEmpty()) {
			List<String> l = Arrays.asList(user.split(","));
			for (int i = 0; i < l.size(); i++) {
				authorIds = authorIds + l.get(i) + ",";
				if (uploaders.containsKey(l.get(i))) {
					counts.add(uploaders.get(l.get(i)));
				} else {
					counts.add(Long.valueOf(0));
				}

			}

		} else {
			for (Map.Entry<String, Long> entry : uploaders.entrySet()) {
				if (uploadersCount <= (uploadersSize - 10)) {
					uploadersCount++;
				} else {
					if (uploadersCount > uploadersSize) {
						break;
					}
					entry.getValue();
					authorIds = authorIds + entry.getKey() + ",";
					counts.add(entry.getValue());
					uploadersCount++;
				}
			}
		}

		try {
			List<UploadersInfo> allUploadersInfo = esService.getUploaderInfo("extended_observation", authorIds);
			List<TopUploadersInfo> uploadersResult = new ArrayList<>();
			for (int k = 0; k < allUploadersInfo.size(); k++) {
				String name = allUploadersInfo.get(k).getName();
				String pic = allUploadersInfo.get(k).getPic();
				Long authorId = allUploadersInfo.get(k).getAuthorId();
				TopUploadersInfo tempUploader = new TopUploadersInfo(name, pic, authorId, counts.get(k));
				uploadersResult.add(tempUploader);
			}

			return (uploadersResult);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return (null);
	}

//	for media data
	private Long getTotal(Map<String, Long> media) {
		Long sum = 0L;

		for (Map.Entry<String, Long> entry : media.entrySet()) {
			if (!(entry.getKey().equals("0"))) {
				sum += entry.getValue();
			}
		}
		return sum;
	}

	private Map<String, Long> getIdentificationSum(Map<String, Long> identification) {
		Long identified = 0L;
		Long unIdentified = 0L;
		for (Entry<String, Long> entry : identification.entrySet()) {
			if (entry.getKey().equals("0")) {
				unIdentified = entry.getValue();
			} else {
				identified += entry.getValue();
			}
		}
		Map<String, Long> result = new HashMap<String, Long>();
		result.put("available", identified);
		result.put("missing", unIdentified);
		return result;
	}

	private Map<String, Long> getTraitsAggregation(Map<String, Long> aggregation, String traitName) {
		Map<String, Long> traitsAgg = new HashMap<String, Long>();

		for (Entry<String, Long> entry : aggregation.entrySet()) {
			if (entry.getKey().split("\\|")[0].equalsIgnoreCase(traitName)) {
				String capitalizeWord = toTitleCase(entry.getKey().split("\\|")[1]);
				traitsAgg.put(capitalizeWord, entry.getValue());
			}
		}
		return traitsAgg;
	}

	private String toTitleCase(String input) {
		StringBuilder titleCase = new StringBuilder(input.length());
		boolean nextTitleCase = true;

		for (char c : input.toCharArray()) {
			if (Character.isSpaceChar(c)) {
				nextTitleCase = true;
			} else if (nextTitleCase) {
				c = Character.toTitleCase(c);
				nextTitleCase = false;
			}

			titleCase.append(c);
		}

		return titleCase.toString();
	}

	private Map<String, Long> getCustomFieldAggregationFieldText(Map<String, Long> aggregation, String fieldType,
			String cfId) {
		Map<String, Long> cfAgg = new HashMap<String, Long>();
		for (Entry<String, Long> entry : aggregation.entrySet()) {
			if (entry.getKey().split("\\|")[0].equalsIgnoreCase(cfId)) {
				if (entry.getKey().split("\\|")[2].equalsIgnoreCase(fieldType)) {
					if (entry.getKey().split("\\|")[4].equalsIgnoreCase("0"))
						cfAgg.put("NO CONTENT", entry.getValue());
					else
						cfAgg.put("HAS CONTENT", entry.getValue());
				}
			}
		}
		return cfAgg;
	}

	private Map<String, Long> getCustomFieldAggregationCategorical(Map<String, Long> aggregation, String fieldType,
			String cfId) {

		Map<String, Long> cfAgg = new HashMap<String, Long>();
		for (Entry<String, Long> entry : aggregation.entrySet()) {
			if (entry.getKey().split("\\|")[0].equalsIgnoreCase(cfId)) {
				if (entry.getKey().split("\\|")[2].equalsIgnoreCase(fieldType)) {

					cfAgg.put(entry.getKey().split("\\|")[4], entry.getValue());
				}
			}
		}
		return cfAgg;
	}

	private Map<String, Long> getCustomFieldAggregationRange(Map<String, Long> aggregation, String dataType,
			String fieldType, String cfId) {

		Map<String, Long> filterdAggregation = new HashMap<String, Long>();
		Map<String, Long> cfAgg = new HashMap<String, Long>();
		for (Entry<String, Long> entry : aggregation.entrySet()) {
			if (entry.getKey().split("\\|")[0].equalsIgnoreCase(cfId)) {
				if (entry.getKey().split("\\|")[2].equalsIgnoreCase(fieldType)) {
					filterdAggregation.put(entry.getKey(), entry.getValue());
				}
			}
		}
		cfAgg = getMinMax(filterdAggregation, dataType);
		return cfAgg;

	}

	private Map<String, Long> getMinMax(Map<String, Long> filteredAggregation, String dataType) {
		Map<String, Long> result = new HashMap<String, Long>();
		String minFinal = "";
		String maxFinal = "";
		Long total = 0L;
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
		for (Entry<String, Long> entry : filteredAggregation.entrySet()) {

			String value = entry.getKey().split("\\|")[4];
			String minValue = value.split("-")[0];
			String maxValue = value.split("-")[1];
			if (dataType.equalsIgnoreCase("Integer")) {
				if (Integer.parseInt(minFinal) > Integer.parseInt(minValue))
					minFinal = minValue;
				if (Integer.parseInt(maxFinal) < Integer.parseInt(maxValue))
					maxFinal = maxValue;

			} else if (dataType.equalsIgnoreCase("decimal")) {
				if (Double.parseDouble(minFinal) > Double.parseDouble(minValue))
					minFinal = minValue;
				if (Double.parseDouble(maxFinal) < Double.parseDouble(maxValue))
					maxFinal = maxValue;

			} else {
//				data type  = date
				try {
					if (df.parse(minFinal).compareTo(df.parse(minValue)) > 0)
						minFinal = minValue;
					if (df.parse(maxFinal).compareTo(df.parse(maxValue)) < 0)
						maxFinal = maxValue;

				} catch (Exception e) {
					logger.error(e.getMessage());
				}

			}

		}
		String key = minFinal + "-" + maxFinal;
		result.put(key, total);
		return result;

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

	@Override
	public List<ObservationHomePage> getObservation(String resourceUrls) {

		try {
			List<ObservationHomePage> obvHomePage = new ArrayList<ObservationHomePage>();
			for (String s : resourceUrls.split(",")) {
				MapSearchQuery query = esUtility.getSearchQueryResource(s);
				MapResponse result = esService.search(ObservationIndex.index.getValue(),
						ObservationIndex.type.getValue(), null, null, null, null, query);
				List<MapDocument> documents = result.getDocuments();
				MapDocument document = documents.get(0);
				try {
					obvHomePage.add(new ObservationHomePage(s, objectMapper
							.readValue(String.valueOf(document.getDocument()), ObservationListMinimalData.class)));
				} catch (IOException e) {
					logger.error(e.getMessage());
				}
			}
			return obvHomePage;
		} catch (

		Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public ObservationListMinimalData getObservationMinimal(String observationId) {
		try {
			ObservationListMinimalData result = null;
			MapDocument response = esService.fetch(ObservationIndex.index.getValue(), ObservationIndex.type.getValue(),
					observationId);
			if (response.getDocument() != null) {
				result = objectMapper.readValue(String.valueOf(response.getDocument()),
						ObservationListMinimalData.class);
			}
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public List<ObservationListElasticMapping> getObservationListCsv(String index, String type, MapSearchQuery querys,
			String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField) {
		try {
			List<ObservationListElasticMapping> observationList = new ArrayList<ObservationListElasticMapping>();
			MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
					onlyFilteredAggregation, termsAggregationField, querys);
			List<MapDocument> documents = result.getDocuments();
			for (MapDocument document : documents) {
				try {
					observationList.add(objectMapper.readValue(String.valueOf(document.getDocument()),
							ObservationListElasticMapping.class));
				} catch (IOException e) {
					logger.error(e.getMessage());
				}
			}
			return observationList;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

}
