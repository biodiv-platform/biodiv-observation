/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;
import java.util.Map;

import com.strandls.observation.es.util.ObservationListMinimalData;
import com.strandls.observation.es.util.ObservationListPageMapper;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationListData {

	private List<ObservationListPageMapper> observationList;
	private Long totalCount;
	private Map<String, Long> geohashAggregation;
	private MapAggregationResponse aggregationData;
	private MapAggregationStatsResponse aggregateStatsData;
	private List<ObservationListMinimalData> observationListMinimal;

	/**
	 * 
	 */
	public ObservationListData() {
		super();
	}

	/**
	 * @param observationList
	 * @param totalCount
	 * @param geohashAggregation
	 * @param aggregationData
	 * @param observationListMinimal
	 */
	public ObservationListData(List<ObservationListPageMapper> observationList, Long totalCount,
			Map<String, Long> geohashAggregation, MapAggregationResponse aggregationData,MapAggregationStatsResponse aggregateStatsData,
			List<ObservationListMinimalData> observationListMinimal) {
		super();
		this.observationList = observationList;
		this.totalCount = totalCount;
		this.geohashAggregation = geohashAggregation;
		this.aggregationData = aggregationData;
		this.observationListMinimal = observationListMinimal;
		this.aggregateStatsData=aggregateStatsData;

	}

	public MapAggregationStatsResponse getAggregateStatsData() {
		return aggregateStatsData;
	}

	public void setAggregateStatsData(MapAggregationStatsResponse aggregateStatsData) {
		this.aggregateStatsData = aggregateStatsData;
	}

	public List<ObservationListPageMapper> getObservationList() {
		return observationList;
	}

	public void setObservationList(List<ObservationListPageMapper> observationList) {
		this.observationList = observationList;
	}

	public Long getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(Long totalCount) {
		this.totalCount = totalCount;
	}

	public Map<String, Long> getGeohashAggregation() {
		return geohashAggregation;
	}

	public void setGeohashAggregation(Map<String, Long> geohashAggregation) {
		this.geohashAggregation = geohashAggregation;
	}

	public MapAggregationResponse getAggregationData() {
		return aggregationData;
	}

	public void setAggregationData(MapAggregationResponse aggregationData) {
		this.aggregationData = aggregationData;
	}

	public List<ObservationListMinimalData> getObservationListMinimal() {
		return observationListMinimal;
	}

	public void setObservationListMinimal(List<ObservationListMinimalData> observationListMinimal) {
		this.observationListMinimal = observationListMinimal;
	}

}
