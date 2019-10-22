/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.taxonomy.pojo.BreadCrumb;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoIbp {

	private String commonName;
	private String scientificName;
	private Long speciesId;
	private List<BreadCrumb> breadCrumb;
	private Integer recoVoteCount;
	private String status;

	/**
	 * @param commonName
	 * @param scientificName
	 * @param speciesId
	 * @param breadCrumb
	 * @param recoVoteCount
	 * @param status
	 */
	public RecoIbp(String commonName, String scientificName, Long speciesId, List<BreadCrumb> breadCrumb,
			Integer recoVoteCount, String status) {
		super();
		this.commonName = commonName;
		this.scientificName = scientificName;
		this.speciesId = speciesId;
		this.breadCrumb = breadCrumb;
		this.recoVoteCount = recoVoteCount;
		this.status = status;
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

	public List<BreadCrumb> getBreadCrumb() {
		return breadCrumb;
	}

	public void setBreadCrumb(List<BreadCrumb> breadCrumb) {
		this.breadCrumb = breadCrumb;
	}

	public Integer getRecoVoteCount() {
		return recoVoteCount;
	}

	public void setRecoVoteCount(Integer recoVoteCount) {
		this.recoVoteCount = recoVoteCount;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

}
