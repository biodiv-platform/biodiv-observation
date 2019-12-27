/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.taxonomy.pojo.BreadCrumb;
import com.strandls.user.pojo.UserIbp;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoIbp {

	private String commonName;
	private String scientificName;
	private Long taxonId;
	private Long speciesId;
	private List<BreadCrumb> breadCrumbs;
	private Integer recoVoteCount;
	private String status;
	private UserIbp userIbp;

	/**
	 * 
	 */
	public RecoIbp() {
		super();
	}

	/**
	 * @param commonName
	 * @param scientificName
	 * @param taxonId
	 * @param speciesId
	 * @param breadCrumbs
	 * @param recoVoteCount
	 * @param status
	 * @param userIbp
	 */
	public RecoIbp(String commonName, String scientificName, Long taxonId, Long speciesId, List<BreadCrumb> breadCrumbs,
			Integer recoVoteCount, String status, UserIbp userIbp) {
		super();
		this.commonName = commonName;
		this.scientificName = scientificName;
		this.taxonId = taxonId;
		this.speciesId = speciesId;
		this.breadCrumbs = breadCrumbs;
		this.recoVoteCount = recoVoteCount;
		this.status = status;
		this.userIbp = userIbp;
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

	public Long getTaxonId() {
		return taxonId;
	}

	public void setTaxonId(Long taxonId) {
		this.taxonId = taxonId;
	}

	public Long getSpeciesId() {
		return speciesId;
	}

	public void setSpeciesId(Long speciesId) {
		this.speciesId = speciesId;
	}

	public List<BreadCrumb> getBreadCrumbs() {
		return breadCrumbs;
	}

	public void setBreadCrumbs(List<BreadCrumb> breadCrumbs) {
		this.breadCrumbs = breadCrumbs;
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

	public UserIbp getUserIbp() {
		return userIbp;
	}

	public void setUserIbp(UserIbp userIbp) {
		this.userIbp = userIbp;
	}

}