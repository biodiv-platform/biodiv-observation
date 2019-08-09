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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author Abhishek Rudra
 *
 */

@Entity
@Table(name = "recommendation")
@JsonIgnoreProperties(ignoreUnknown = true)
public class Recommendation implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1592197494436773044L;

	private Long id;
	private Date lastModified;
	private String name;
	private Long taxonConceptId;
	private Boolean isScientificName;
	private Long languageId;
	private String lowercaseName;
	private String flaggingReason;
	private Boolean isFlagged;
	private Long acceptedNameId;

	@Id
	@GeneratedValue
	@Column(name = "id")
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	@Column(name = "last_modified")
	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	@Column(name = "name")
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Column(name = "taxon_concept_id")
	public Long getTaxonConceptId() {
		return taxonConceptId;
	}

	public void setTaxonConceptId(Long taxonConceptId) {
		this.taxonConceptId = taxonConceptId;
	}

	@Column(name = "is_scientific_name")
	public Boolean getIsScientificName() {
		return isScientificName;
	}

	public void setIsScientificName(Boolean isScientificName) {
		this.isScientificName = isScientificName;
	}

	@Column(name = "language_id")
	public Long getLanguageId() {
		return languageId;
	}

	public void setLanguageId(Long languageId) {
		this.languageId = languageId;
	}

	@Column(name = "lowercase_name")
	public String getLowercaseName() {
		return lowercaseName;
	}

	public void setLowercaseName(String lowercaseName) {
		this.lowercaseName = lowercaseName;
	}

	@Column(name = "flagging_reason")
	public String getFlaggingReason() {
		return flaggingReason;
	}

	public void setFlaggingReason(String flaggingReason) {
		this.flaggingReason = flaggingReason;
	}

	@Column(name = "is_flagged")
	public Boolean getIsFlagged() {
		return isFlagged;
	}

	public void setIsFlagged(Boolean isFlagged) {
		this.isFlagged = isFlagged;
	}

	@Column(name = "accepted_name_id")
	public Long getAcceptedNameId() {
		return acceptedNameId;
	}

	public void setAcceptedNameId(Long acceptedNameId) {
		this.acceptedNameId = acceptedNameId;
	}

}
