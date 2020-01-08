/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.user.pojo.UserIbp;

/**
 * @author Abhishek Rudra
 *
 */
public class AllRecoSugguestions {

	private String commonName;
	private String scientificName;
	private Long taxonId;
	private Long speciesId;
	private List<UserIbp> userList;

	/**
	 * 
	 */
	public AllRecoSugguestions() {
		super();
	}

	/**
	 * @param commonName
	 * @param scientificName
	 * @param taxonId
	 * @param speciesId
	 * @param userList
	 */
	public AllRecoSugguestions(String commonName, String scientificName, Long taxonId, Long speciesId,
			List<UserIbp> userList) {
		super();
		this.commonName = commonName;
		this.scientificName = scientificName;
		this.taxonId = taxonId;
		this.speciesId = speciesId;
		this.userList = userList;
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

	public List<UserIbp> getUserList() {
		return userList;
	}

	public void setUserList(List<UserIbp> userList) {
		this.userList = userList;
	}

}
