/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoCreate {

	private String confidence;
	private String recoComment;
	private Long commonNameId;
	private String commonName;
	private Long scientificNameId;
	private String scientificName;
	private Boolean flag;

	/**
	 * 
	 */
	public RecoCreate() {
		super();
	}

	/**
	 * @param confidence
	 * @param recoComment
	 * @param commonNameId
	 * @param commonName
	 * @param scientificNameId
	 * @param scientificName
	 * @param flag
	 */
	public RecoCreate(String confidence, String recoComment, Long commonNameId, String commonName,
			Long scientificNameId, String scientificName, Boolean flag) {
		super();
		this.confidence = confidence;
		this.recoComment = recoComment;
		this.commonNameId = commonNameId;
		this.commonName = commonName;
		this.scientificNameId = scientificNameId;
		this.scientificName = scientificName;
		this.flag = flag;
	}

	public String getConfidence() {
		return confidence;
	}

	public void setConfidence(String confidence) {
		this.confidence = confidence;
	}

	public String getRecoComment() {
		return recoComment;
	}

	public void setRecoComment(String recoComment) {
		this.recoComment = recoComment;
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
	

}
