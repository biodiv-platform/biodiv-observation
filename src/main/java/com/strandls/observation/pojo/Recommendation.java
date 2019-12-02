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
import javax.persistence.Transient;

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
	private String canonicalName;

	/**
	 * 
	 */
	public Recommendation() {
		super();
	}

	/**
	 * @param id
	 * @param lastModified
	 * @param name
	 * @param taxonConceptId
	 * @param isScientificName
	 * @param languageId
	 * @param lowercaseName
	 * @param flaggingReason
	 * @param isFlagged
	 * @param acceptedNameId
	 * @param canonicalName
	 */
	public Recommendation(Long id, Date lastModified, String name, Long taxonConceptId, Boolean isScientificName,
			Long languageId, String lowercaseName, String flaggingReason, Boolean isFlagged, Long acceptedNameId,
			String canonicalName) {
		super();
		this.id = id;
		this.lastModified = lastModified;
		this.name = name;
		this.taxonConceptId = taxonConceptId;
		this.isScientificName = isScientificName;
		this.languageId = languageId;
		this.lowercaseName = lowercaseName;
		this.flaggingReason = flaggingReason;
		this.isFlagged = isFlagged;
		this.acceptedNameId = acceptedNameId;
		this.canonicalName = canonicalName;
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
	
	
	@Column(name = "canonical_name")
	public String getCanonicalName() {
		return canonicalName;
	}

	public void setCanonicalName(String canonicalName) {
		this.canonicalName = canonicalName;
	}

	@Transient
	public boolean isAcceptedName() {
		if (this.taxonConceptId != null && this.acceptedNameId != null) {
			if (taxonConceptId.equals(acceptedNameId))
				return true;
			return false;
		}
		return false;

	}
}
