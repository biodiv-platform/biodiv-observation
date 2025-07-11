/**
 * 
 */
package com.strandls.observation.service;

import java.util.List;
import java.util.Map;

import com.strandls.esmodule.pojo.FilterPanelData;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.es.util.ObservationListMinimalData;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.ObservationDataByUser;
import com.strandls.observation.pojo.ObservationHomePage;
import com.strandls.observation.pojo.ObservationListData;

/**
 * @author Abhishek Rudra
 *
 */
public interface ObservationListService {

	public ObservationListData getObservationList(String index, String type, MapSearchQuery querys,
			String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField, String geoShapeFilterField,
			MapAggregationStatsResponse aggregationStatsResult, MapAggregationResponse aggregationResult, String view);

	public MapAggregationResponse mapAggregate(String index, String type, String sGroup, String taxon, String user,
			String userGroupList, String webaddress, String speciesName, String mediaFilter, String months,
			String isFlagged, String minDate, String maxDate, String validate, Map<String, List<String>> traitParams,
			Map<String, List<String>> customParams, String classificationid, MapSearchParams mapSearchParams,
			String maxvotedrecoid, String recoId, String createdOnMaxDate, String createdOnMinDate, String status,
			String taxonId, String recoName, String geoAggregationField, String rank, String tahsil, String district,
			String state, String tags, String publicationGrade, String auhtorVoted, String dataSetName,
			String dataTableName, String geoEntity, String dataTableId);

	public MapAggregationStatsResponse mapAggregateStats(String index, String type, String sGroup, String taxon,
			String user, String userGroupList, String webaddress, String speciesName, String mediaFilter, String months,
			String isFlagged, String minDate, String maxDate, String validate, Map<String, List<String>> traitParams,
			Map<String, List<String>> customParams, String classificationid, MapSearchParams mapSearchParams,
			String maxvotedrecoid, String recoId, String createdOnMaxDate, String createdOnMinDate, String status,
			String taxonId, String recoName, String geoAggregationField, String rank, String tahsil, String district,
			String state, String tags, String publicationGrade, String authorVoted, Integer lifeListOffset,
			Integer uploadersoffset, Integer identifiersoffset, String dataSetName, String dataTableName,
			String geoEntity, String geoShapeFilterField, String dataTableId, String statsFilter);

	public FilterPanelData getAllFilter();

	public List<ObservationHomePage> getObservation(String resourceUrls);

	public ObservationListMinimalData getObservationMinimal(String observationId);

	public List<ObservationListElasticMapping> getObservationListCsv(String index, String type, MapSearchQuery querys,
			String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField, String geoShapeFilterField);

	public ObservationDataByUser getCountPerDay(String userId);
}
