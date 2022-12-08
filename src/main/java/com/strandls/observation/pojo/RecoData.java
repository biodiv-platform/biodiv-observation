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

	/**
	 * 
	 */
	public RecoData() {
		super();
	}

	public RecoData(String taxonCommonName, Long scientificNameTaxonId, String taxonScientificName, Long languageId,
			String source) {
		super();
		this.taxonCommonName = taxonCommonName;
		this.scientificNameTaxonId = scientificNameTaxonId;
		this.taxonScientificName = taxonScientificName;
		this.languageId = languageId;
		this.source = source;
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

	/**
	 * @param taxonCommonName
	 * @param scientificNameTaxonId
	 * @param taxonScientificName
	 * @param languageId
	 */
//	public RecoData(String taxonCommonName, Long scientificNameTaxonId, String taxonScientificName, Long languageId) {
//		super();
//		this.taxonCommonName = taxonCommonName;
//		this.scientificNameTaxonId = scientificNameTaxonId;
//		this.taxonScientificName = taxonScientificName;
//		this.languageId = languageId;
//	}
//
//	public String getTaxonCommonName() {
//		return taxonCommonName;
//	}
//
//	public void setTaxonCommonName(String taxonCommonName) {
//		this.taxonCommonName = taxonCommonName;
//	}
//
//	public Long getScientificNameTaxonId() {
//		return scientificNameTaxonId;
//	}
//
//	public void setScientificNameTaxonId(Long scientificNameTaxonId) {
//		this.scientificNameTaxonId = scientificNameTaxonId;
//	}
//
//	public String getTaxonScientificName() {
//		return taxonScientificName;
//	}
//
//	public void setTaxonScientificName(String taxonScientificName) {
//		this.taxonScientificName = taxonScientificName;
//	}
//
//	public Long getLanguageId() {
//		return languageId;
//	}
//
//	public void setLanguageId(Long languageId) {
//		this.languageId = languageId;
//	}

}
