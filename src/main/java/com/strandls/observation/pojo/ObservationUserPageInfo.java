package com.strandls.observation.pojo;

import java.util.List;

public class ObservationUserPageInfo {

	private List<UniqueSpeciesInfo> uniqueSpeciesInfos;
	private Long totalCount;

	public ObservationUserPageInfo() {
		super();
	}

	public ObservationUserPageInfo(List<UniqueSpeciesInfo> uniqueSpeciesInfos, Long totalCount) {
		super();
		this.uniqueSpeciesInfos = uniqueSpeciesInfos;
		this.totalCount = totalCount;
	}

	public List<UniqueSpeciesInfo> getUniqueSpeciesInfos() {
		return uniqueSpeciesInfos;
	}

	public void setUniqueSpeciesInfos(List<UniqueSpeciesInfo> uniqueSpeciesInfos) {
		this.uniqueSpeciesInfos = uniqueSpeciesInfos;
	}

	public Long getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(Long totalCount) {
		this.totalCount = totalCount;
	}

}
