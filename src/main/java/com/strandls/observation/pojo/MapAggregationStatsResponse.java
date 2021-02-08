package com.strandls.observation.pojo;

import java.util.List;
import java.util.Map;

public class MapAggregationStatsResponse {

	private Map<String, Long> groupUniqueSpecies;
	private List<TopUploadersInfo>groupTopUploaders;
	

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
