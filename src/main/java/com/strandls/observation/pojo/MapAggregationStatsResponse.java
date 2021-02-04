package com.strandls.observation.pojo;

import java.util.Map;

public class MapAggregationStatsResponse {

	private Map<String, Long> groupUniqueSpecies;

	public Map<String, Long> getGroupUniqueSpecies() {
		return groupUniqueSpecies;
	}

	public void setGroupUniqueSpecies(Map<String, Long> groupUniqueSpecies) {
		this.groupUniqueSpecies = groupUniqueSpecies;
	}

}
