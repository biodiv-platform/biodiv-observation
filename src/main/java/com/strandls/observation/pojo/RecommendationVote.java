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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author Abhishek Rudra
 *
 */
@Entity
@Table(name = "recommendation_vote")
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecommendationVote implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7166935130639476076L;

	private Long id;
	private Long authorId;
	private Long observationId;
	private Long recommendationId;
	private Date votedOn;
	private Long commonNameRecoId;
	private String givenCommonName;
	private String givenSciName;
	private Boolean flag;

	/**
	 * 
	 */
	public RecommendationVote() {
		super();
	}

	/**
	 * @param id
	 * @param authorId
	 * @param observationId
	 * @param recommendationId
	 * @param votedOn
	 * @param commonNameRecoId
	 * @param givenCommonName
	 * @param givenSciName
	 * @param flag
	 */
	public RecommendationVote(Long id, Long authorId, Long observationId, Long recommendationId, Date votedOn,
			Long commonNameRecoId, String givenCommonName, String givenSciName, Boolean flag) {
		super();
		this.id = id;
		this.authorId = authorId;
		this.observationId = observationId;
		this.recommendationId = recommendationId;
		this.votedOn = votedOn;
		this.commonNameRecoId = commonNameRecoId;
		this.givenCommonName = givenCommonName;
		this.givenSciName = givenSciName;
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

	@Column(name = "author_id")
	public Long getAuthorId() {
		return authorId;
	}

	public void setAuthorId(Long authorId) {
		this.authorId = authorId;
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

	@Column(name = "flag")
	public Boolean getFlag() {
		return flag;
	}

	public void setFlag(Boolean flag) {
		this.flag = flag;
	}

}
