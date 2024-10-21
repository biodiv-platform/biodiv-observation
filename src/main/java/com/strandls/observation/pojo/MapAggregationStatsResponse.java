package com.strandls.observation.pojo;

import java.util.List;
import java.util.Map;

public class MapAggregationStatsResponse {

	private Map<String, Long> groupUniqueSpecies;
	private List<TopUploadersInfo> groupTopUploaders;
	private List<TopUploadersInfo> groupTopIdentifiers;
	private Map<String, Long> totalCounts;
	private Map<String, List<Map<String, Object>>> countPerDay;
	private Map<String, List<Map<String, Object>>> groupObservedOn;
	private List<Map<String, Object>> groupTraits;

	public Map<String, Long> getTotalCounts() {
		return totalCounts;
	}

	public void setTotalCounts(Map<String, Long> totalCounts) {
		this.totalCounts = totalCounts;
	}

	public List<Map<String, Object>> getGroupTraits() {
		return groupTraits;
	}

	public void setGroupTraits(List<Map<String, Object>> groupByTraits) {
		this.groupTraits = groupByTraits;
	}

	public Map<String, List<Map<String, Object>>> getCountPerDay() {
		return countPerDay;
	}

	public void setCountPerDay(Map<String, List<Map<String, Object>>> countPerDay) {
		this.countPerDay = countPerDay;
	}

	public Map<String, List<Map<String, Object>>> getGroupObservedOn() {
		return groupObservedOn;
	}

	public void setGroupObservedOn(Map<String, List<Map<String, Object>>> groupByMonth) {
		this.groupObservedOn = groupByMonth;
	}

	public List<TopUploadersInfo> getGroupTopIdentifiers() {
		return groupTopIdentifiers;
	}

	public void setGroupTopIdentifiers(List<TopUploadersInfo> groupTopIdentifiers) {
		this.groupTopIdentifiers = groupTopIdentifiers;
	}

	public List<TopUploadersInfo> getGroupTopUploaders() {
		return groupTopUploaders;
	}

	public void setGroupTopUploaders(List<TopUploadersInfo> groupTopUploaders) {
		this.groupTopUploaders = groupTopUploaders;
	}

	public Map<String, Long> getGroupUniqueSpecies() {
		return groupUniqueSpecies;
	}

	public void setGroupUniqueSpecies(Map<String, Long> groupUniqueSpecies) {
		this.groupUniqueSpecies = groupUniqueSpecies;
	}

}
