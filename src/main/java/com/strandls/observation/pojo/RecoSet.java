/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoSet {

	private Long taxonId;
	private String commonName;
	private String scientificName;

	/**
	 * 
	 */
	public RecoSet() {
		super();
	}

	/**
	 * @param taxonId
	 * @param commonName
	 * @param scientificName
	 */
	public RecoSet(Long taxonId, String commonName, String scientificName) {
		super();
		this.taxonId = taxonId;
		this.commonName = commonName;
		this.scientificName = scientificName;
	}

	public Long getTaxonId() {
		return taxonId;
	}

	public void setTaxonId(Long taxonId) {
		this.taxonId = taxonId;
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

}
