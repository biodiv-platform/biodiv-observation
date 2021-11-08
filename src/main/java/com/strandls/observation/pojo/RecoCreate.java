/**
 * 
 */
package com.strandls.observation.pojo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author Abhishek Rudra
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecoCreate {

	private Long commonNameId;
	private String commonName;
	private Long scientificNameId;
	private Long taxonId;
	private String scientificName;
	private Boolean flag;

	/**
	 * 
	 */
	public RecoCreate() {
		super();
	}

	/**
	 * @param commonNameId
	 * @param commonName
	 * @param scientificNameId
	 * @param taxonId
	 * @param scientificName
	 * @param flag
	 */
	public RecoCreate(Long commonNameId, String commonName, Long scientificNameId, Long taxonId, String scientificName,
			Boolean flag) {
		super();
		this.commonNameId = commonNameId;
		this.commonName = commonName;
		this.scientificNameId = scientificNameId;
		this.taxonId = taxonId;
		this.scientificName = scientificName;
		this.flag = flag;
	}

	public Long getCommonNameId() {
		return commonNameId;
	}

	public void setCommonNameId(Long commonNameId) {
		this.commonNameId = commonNameId;
	}

	public String getCommonName() {
		return commonName;
	}

	public void setCommonName(String commonName) {
		this.commonName = commonName;
	}

	public Long getScientificNameId() {
		return scientificNameId;
	}

	public void setScientificNameId(Long scientificNameId) {
		this.scientificNameId = scientificNameId;
	}

	public String getScientificName() {
		return scientificName;
	}

	public void setScientificName(String scientificName) {
		this.scientificName = scientificName;
	}

	public Boolean getFlag() {
		return flag;
	}

	public void setFlag(Boolean flag) {
		this.flag = flag;
	}

	public Long getTaxonId() {
		return taxonId;
	}

	public void setTaxonId(Long taxonId) {
		this.taxonId = taxonId;
	}

}
