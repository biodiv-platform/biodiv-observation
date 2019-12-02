/**
 * 
 */
package com.strandls.observation.pojo;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author Abhishek Rudra
 *
 */
@Entity
@Table(name = "recommendation_vote")
public class RecommendationVote implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7166935130639476076L;

	private Long id;
	private Long version;
	private Long authorId;
	private String confidence;
	private Long observationId;
	private Long recommendationId;
	private Integer userWeight;
	private Date votedOn;
	private String comment;
	private Long commonNameRecoId;
	private String givenCommonName;
	private String givenSciName;
	private String originalAuthor;
	private Boolean flag;

	/**
	 * 
	 */
	public RecommendationVote() {
		super();
	}

	/**
	 * @param id
	 * @param version
	 * @param authorId
	 * @param confidence
	 * @param observationId
	 * @param recommendationId
	 * @param userWeight
	 * @param votedOn
	 * @param comment
	 * @param commonNameRecoId
	 * @param givenCommonName
	 * @param givenSciName
	 * @param originalAuthor
	 * @param flag
	 */
	public RecommendationVote(Long id, Long version, Long authorId, String confidence, Long observationId,
			Long recommendationId, Integer userWeight, Date votedOn, String comment, Long commonNameRecoId,
			String givenCommonName, String givenSciName, String originalAuthor, Boolean flag) {
		super();
		this.id = id;
		this.version = version;
		this.authorId = authorId;
		this.confidence = confidence;
		this.observationId = observationId;
		this.recommendationId = recommendationId;
		this.userWeight = userWeight;
		this.votedOn = votedOn;
		this.comment = comment;
		this.commonNameRecoId = commonNameRecoId;
		this.givenCommonName = givenCommonName;
		this.givenSciName = givenSciName;
		this.originalAuthor = originalAuthor;
		this.flag = flag;
	}

	@Id
	@GeneratedValue
	@Column(name = "id")
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	@Column(name = "version")
	public Long getVersion() {
		return version;
	}

	public void setVersion(Long version) {
		this.version = version;
	}

	@Column(name = "author_id")
	public Long getAuthorId() {
		return authorId;
	}

	public void setAuthorId(Long authorId) {
		this.authorId = authorId;
	}

	@Column(name = "confidence")
	public String getConfidence() {
		return confidence;
	}

	public void setConfidence(String confidence) {
		this.confidence = confidence;
	}

	@Column(name = "observation_id")
	public Long getObservationId() {
		return observationId;
	}

	public void setObservationId(Long observationId) {
		this.observationId = observationId;
	}

	@Column(name = "recommendation_id")
	public Long getRecommendationId() {
		return recommendationId;
	}

	public void setRecommendationId(Long recommendationId) {
		this.recommendationId = recommendationId;
	}

	@Column(name = "user_weight")
	public Integer getUserWeight() {
		return userWeight;
	}

	public void setUserWeight(Integer userWeight) {
		this.userWeight = userWeight;
	}

	@Column(name = "voted_on")
	public Date getVotedOn() {
		return votedOn;
	}

	public void setVotedOn(Date votedOn) {
		this.votedOn = votedOn;
	}

	@Column(name = "comment")
	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	@Column(name = "common_name_reco_id")
	public Long getCommonNameRecoId() {
		return commonNameRecoId;
	}

	public void setCommonNameRecoId(Long commonNameRecoId) {
		this.commonNameRecoId = commonNameRecoId;
	}

	@Column(name = "given_common_name")
	public String getGivenCommonName() {
		return givenCommonName;
	}

	public void setGivenCommonName(String givenCommonName) {
		this.givenCommonName = givenCommonName;
	}

	@Column(name = "given_sci_name")
	public String getGivenSciName() {
		return givenSciName;
	}

	public void setGivenSciName(String givenSciName) {
		this.givenSciName = givenSciName;
	}

	@Column(name = "original_author")
	public String getOriginalAuthor() {
		return originalAuthor;
	}

	public void setOriginalAuthor(String originalAuthor) {
		this.originalAuthor = originalAuthor;
	}

	@Column(name = "flag")
	public Boolean getFlag() {
		return flag;
	}

	public void setFlag(Boolean flag) {
		this.flag = flag;
	}

}
