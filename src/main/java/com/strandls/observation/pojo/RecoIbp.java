/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoIbp {

	private String commonName;
	private String scientificName;
	private Long speciesId;
	private List<String> breadCrum;
	private Integer recoVoteCount;

	/**
	 * @param commonName
	 * @param scientificName
	 * @param speciesId
	 * @param breadCrum
	 * @param recoVoteCount
	 */
	public RecoIbp(String commonName, String scientificName, Long speciesId, List<String> breadCrum,
			Integer recoVoteCount) {
		super();
		this.commonName = commonName;
		this.scientificName = scientificName;
		this.speciesId = speciesId;
		this.breadCrum = breadCrum;
		this.recoVoteCount = recoVoteCount;
	}

	public String getCommonName() {
		return commonName;
	}

	public void setCommonName(String commonName) {
		this.commonName = commonName;
	}

	public String getScientificName() {
		return scientificName;
	}

	public void setScientificName(String scientificName) {
		this.scientificName = scientificName;
	}

	public Long getSpeciesId() {
		return speciesId;
	}

	public void setSpeciesId(Long speciesId) {
		this.speciesId = speciesId;
	}

	public List<String> getBreadCrum() {
		return breadCrum;
	}

	public void setBreadCrum(List<String> breadCrum) {
		this.breadCrum = breadCrum;
	}

	public Integer getRecoVoteCount() {
		return recoVoteCount;
	}

	public void setRecoVoteCount(Integer recoVoteCount) {
		this.recoVoteCount = recoVoteCount;
	}

}
