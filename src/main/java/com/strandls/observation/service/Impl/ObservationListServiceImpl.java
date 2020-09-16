/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

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
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapResponse;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.esmodule.pojo.Traits;
import com.strandls.observation.es.util.ESUtility;
import com.strandls.observation.es.util.ObservationIndex;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.es.util.ObservationListMinimalData;
import com.strandls.observation.es.util.ObservationListPageMapper;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.ObservationHomePage;
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

			listData = new ObservationListData(observationList, totalCount, geoHashResult, aggregationResult,
					observationListMinimal);

		} catch (ApiException e) {
			e.printStackTrace();
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
			String recoName, String geoAggregationField, String rank, String tahsil, String district, String state,
			String tags, String publicationGrade) {

		MapSearchQuery mapSearchQuery = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
				speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
				classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status, taxonId,
				recoName, rank, tahsil, district, state, tags, publicationGrade);

		MapSearchQuery mapSearchQueryFilter;

		String omiter = null;
		MapAggregationResponse aggregationResponse = new MapAggregationResponse();

		if (sGroup != null) {

			mapSearchQueryFilter = esUtility.getMapSearchQuery(omiter, taxon, user, userGroupList, webaddress,
					speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);
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
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);
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
					taxonId, recoName, omiter, tahsil, district, state, tags, publicationGrade);
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
					taxonId, recoName, rank, tahsil, district, omiter, tags, publicationGrade);
			aggregationResponse.setGroupState(getAggregate(index, type, "location_information.state.raw",
					geoAggregationField, mapSearchQueryFilter).getGroupAggregation());

		} else {
			aggregationResponse.setGroupState(
					getAggregate(index, type, "location_information.state.raw", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}

		if (userGroupList != null) {
			mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, omiter, webaddress, speciesName,
					mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
					classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate, createdOnMinDate, status,
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);
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
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);
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
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);
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
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);
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
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);

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
					taxonId, recoName, rank, tahsil, district, state, tags, publicationGrade);
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
					omiter, recoName, rank, tahsil, district, state, tags, publicationGrade);
			aggregationResponse.setGroupTaxonIDExists(
					getAggregate(index, type, "max_voted_reco.taxonstatus", geoAggregationField, mapSearchQueryFilter)
							.getGroupAggregation());

		} else {
			aggregationResponse.setGroupTaxonIDExists(
					getAggregate(index, type, "max_voted_reco.taxonstatus", geoAggregationField, mapSearchQuery)
							.getGroupAggregation());
		}

//		getting filter panel
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

//		new trait aggregation

		Map<String, Long> traitValuesAggregation = new HashMap<String, Long>();
		Map<String, Map<String, Long>> traitMaps = new HashMap<String, Map<String, Long>>();
		if (traitList != null) {
			for (Traits trait : traitList) {
				String keyword = "trait_" + trait.getId() + "." + trait.getType();
				if (!traitParams.isEmpty()) {
					List<String> tempTraitParams = new ArrayList<String>();
					if (traitParams.containsKey(keyword)) {
						tempTraitParams = traitParams.remove(keyword);
						mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList,
								webaddress, speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate,
								traitParams, customParams, classificationid, mapSearchParams, maxvotedrecoid,
								createdOnMaxDate, createdOnMinDate, status, taxonId, recoName, rank, tahsil, district,
								state, tags, publicationGrade);

						traitValuesAggregation = getTraitsAggregation(
								getAggregate(index, type, "facts.trait_value.trait_aggregation.raw",
										geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
								trait.getName());

						traitParams.put(keyword, tempTraitParams);
					}
				}
				if (traitParams.isEmpty() || !(traitParams.containsKey(keyword))) {
					traitValuesAggregation = getTraitsAggregation(
							getAggregate(index, type, "facts.trait_value.trait_aggregation.raw", geoAggregationField,
									mapSearchQuery).getGroupAggregation(),
							trait.getName());
				}

				traitMaps.put(trait.getName(), traitValuesAggregation);

			}
		}
		aggregationResponse.setGroupTraits(traitMaps);

//		custom Field Aggregation Start
		Map<String, Long> cfValuesAggregation = new HashMap<String, Long>();
		Map<String, Map<String, Long>> cfMaps = new HashMap<String, Map<String, Long>>();
		List<String> tempCFParams = new ArrayList<String>();
		if (customFieldList != null) {
			for (CustomFields cf : customFieldList) {
				String keyword = "custom_" + cf.getId() + "." + cf.getFieldtype();
				String fieldType = cf.getFieldtype();

				if (!customParams.isEmpty()) {
					tempCFParams = customParams.remove(keyword);

					mapSearchQueryFilter = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress,
							speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams,
							customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
							createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state, tags,
							publicationGrade);

					if (fieldType.equalsIgnoreCase("FIELD TEXT")) {
						cfValuesAggregation = getCustomFieldAggregationFieldText(
								getAggregate(index, type,
										"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
										geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
								fieldType, cf.getId().toString());
					} else if (fieldType.equalsIgnoreCase("SINGLE CATEGORICAL")
							|| fieldType.equalsIgnoreCase("MULTIPLE CATEGORICAL")) {
						cfValuesAggregation = getCustomFieldAggregationCategorical(
								getAggregate(index, type,
										"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
										geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
								fieldType, cf.getId().toString());
					} else {
//					field type = RANGE
						cfValuesAggregation = getCustomFieldAggregationRange(
								getAggregate(index, type,
										"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
										geoAggregationField, mapSearchQueryFilter).getGroupAggregation(),
								cf.getDataType(), fieldType, cf.getId().toString());
					}
					customParams.put(keyword, tempCFParams);
				}

				if (customParams.isEmpty() || !customParams.containsKey(keyword)) {
					if (fieldType.equalsIgnoreCase("FIELD TEXT")) {
						cfValuesAggregation = getCustomFieldAggregationFieldText(
								getAggregate(index, type,
										"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
										geoAggregationField, mapSearchQuery).getGroupAggregation(),
								fieldType, cf.getId().toString());
					} else if (fieldType.equalsIgnoreCase("SINGLE CATEGORICAL")
							|| fieldType.equalsIgnoreCase("MULTIPLE CATEGORICAL")) {
						cfValuesAggregation = getCustomFieldAggregationCategorical(
								getAggregate(index, type,
										"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
										geoAggregationField, mapSearchQuery).getGroupAggregation(),
								fieldType, cf.getId().toString());
					} else {
//					field type = RANGE
						cfValuesAggregation = getCustomFieldAggregationRange(
								getAggregate(index, type,
										"custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw",
										geoAggregationField, mapSearchQuery).getGroupAggregation(),
								cf.getDataType(), fieldType, cf.getId().toString());
					}
				}
				cfMaps.put(cf.getName(), cfValuesAggregation);
			}
		}
		aggregationResponse.setGroupCustomField(cfMaps);

//		custom Field Aggregation ENDS

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
			e.printStackTrace();
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
