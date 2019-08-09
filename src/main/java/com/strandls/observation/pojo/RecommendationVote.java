/**
 * 
 */
package com.strandls.observation.pojo;

import java.io.Serializable;
import java.sql.Date;

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
	private Long authorId;
	private String confidence;
	private Long observationId;
	private Long recommendationId;
	private Date votedOn;
	private String comment;
	private Long commonNameRecoId;
	private String givenCommonName;
	private String givenSciName;
	private String originalAuthor;

	@Id
	@GeneratedValue
	@Column(name = "id")
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
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

}
