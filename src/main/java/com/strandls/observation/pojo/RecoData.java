/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoData {

	private String taxonCommonName;
	private Long scientificNameTaxonId;
	private String taxonScientificName;
	private String recoComment;
	private String confidence;
	private Long languageId;

	/**
	 * 
	 */
	public RecoData() {
		super();
	}

	/**
	 * @param taxonCommonName
	 * @param scientificNameTaxonId
	 * @param taxonScientificName
	 * @param recoComment
	 * @param confidence
	 * @param languageId
	 */
	public RecoData(String taxonCommonName, Long scientificNameTaxonId, String taxonScientificName, String recoComment,
			String confidence, Long languageId) {
		super();
		this.taxonCommonName = taxonCommonName;
		this.scientificNameTaxonId = scientificNameTaxonId;
		this.taxonScientificName = taxonScientificName;
		this.recoComment = recoComment;
		this.confidence = confidence;
		this.languageId = languageId;
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

	public String getRecoComment() {
		return recoComment;
	}

	public void setRecoComment(String recoComment) {
		this.recoComment = recoComment;
	}

	public String getConfidence() {
		return confidence;
	}

	public void setConfidence(String confidence) {
		this.confidence = confidence;
	}

	public Long getLanguageId() {
		return languageId;
	}

	public void setLanguageId(Long languageId) {
		this.languageId = languageId;
	}

}
