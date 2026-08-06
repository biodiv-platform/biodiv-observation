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
public class RecoData {

	private String taxonCommonName;
	private Long scientificNameTaxonId;
	private String taxonScientificName;
	private Long languageId;
	private String source;
	private Long acceptedNameId;

	public RecoData() {
		super();
	}

	/**
	 * 
	 * @param taxonCommonName
	 * @param scientificNameTaxonId
	 * @param taxonScientificName
	 * @param languageId
	 * @param acceptedNameId
	 */

	public RecoData(String taxonCommonName, Long scientificNameTaxonId, String taxonScientificName, Long languageId,
			String source, Long acceptedNameId) {
		super();
		this.taxonCommonName = taxonCommonName;
		this.scientificNameTaxonId = scientificNameTaxonId;
		this.taxonScientificName = taxonScientificName;
		this.languageId = languageId;
		this.source = source;
		this.acceptedNameId = acceptedNameId;
	}

	public String getTaxonCommonName() {
		return taxonCommonName;
	}

	public void setTaxonCommonName(String taxonCommonName) {
		this.taxonCommonName = taxonCommonName;
	}

	public Long getScientificNameTaxonId() {
		return scientificNameTaxonId;
	}

	public void setScientificNameTaxonId(Long scientificNameTaxonId) {
		this.scientificNameTaxonId = scientificNameTaxonId;
	}

	public String getTaxonScientificName() {
		return taxonScientificName;
	}

	public void setTaxonScientificName(String taxonScientificName) {
		this.taxonScientificName = taxonScientificName;
	}

	public Long getLanguageId() {
		return languageId;
	}

	public void setLanguageId(Long languageId) {
		this.languageId = languageId;
	}

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}

	public Long getAcceptedNameId() {
		return acceptedNameId;
	}

	public void setAcceptedNameId(Long acceptedNameId) {
		this.acceptedNameId = acceptedNameId;
	}

}
