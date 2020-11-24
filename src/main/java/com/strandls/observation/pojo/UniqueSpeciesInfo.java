package com.strandls.observation.pojo;

public class UniqueSpeciesInfo {

	private String name;
	private Long maxVotedRecoId;
	private Long speciesId;
	private Long taxonId;
	private Long freq;

	public UniqueSpeciesInfo() {
		super();
	}

	public UniqueSpeciesInfo(String name, Long maxVotedRecoId, Long speciesId, Long taxonId, Long freq) {
		super();
		this.name = name;
		this.maxVotedRecoId = maxVotedRecoId;
		this.speciesId = speciesId;
		this.taxonId = taxonId;
		this.freq = freq;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Long getMaxVotedRecoId() {
		return maxVotedRecoId;
	}

	public void setMaxVotedRecoId(Long maxVotedRecoId) {
		this.maxVotedRecoId = maxVotedRecoId;
	}

	public Long getSpeciesId() {
		return speciesId;
	}

	public void setSpeciesId(Long speciesId) {
		this.speciesId = speciesId;
	}

	public Long getTaxonId() {
		return taxonId;
	}

	public void setTaxonId(Long taxonId) {
		this.taxonId = taxonId;
	}

	public Long getFreq() {
		return freq;
	}

	public void setFreq(Long freq) {
		this.freq = freq;
	}

}
