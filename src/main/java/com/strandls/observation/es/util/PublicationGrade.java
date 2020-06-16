/**
 * 
 */
package com.strandls.observation.es.util;

import java.io.Serializable;

/**
 * @author ashish
 *
 */

public class PublicationGrade implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 5474070852004845688L;
	
	Boolean hasMediaEvidence;
	Boolean hasDateDefined;
	Boolean isLocationDefined;
	Boolean isIdValidated ;//or has 2 or more supporters
	Boolean hasTaxonName;
	Boolean hasfamilyRankOrLower;
	Boolean isNotFlagged;
	Boolean isNativeObservation;
	
	public PublicationGrade() {
		super();
	};
	
	public PublicationGrade(Boolean hasMediaEvidence, Boolean hasDateDefined, Boolean isLocationDefined,
			Boolean isIdValidated, Boolean hasTaxonName, Boolean hasfamilyRankOrLower, Boolean isNotFlagged,
			Boolean isNativeObservation) {
		super();
		this.hasMediaEvidence = hasMediaEvidence;
		this.hasDateDefined = hasDateDefined;
		this.isLocationDefined = isLocationDefined;
		this.isIdValidated = isIdValidated;
		this.hasTaxonName = hasTaxonName;
		this.hasfamilyRankOrLower = hasfamilyRankOrLower;
		this.isNotFlagged = isNotFlagged;
		this.isNativeObservation = isNativeObservation;
	}

	public Boolean getHasMediaEvidence() {
		return hasMediaEvidence;
	}

	public void setHasMediaEvidence(Boolean hasMediaEvidence) {
		this.hasMediaEvidence = hasMediaEvidence;
	}

	public Boolean getHasDateDefined() {
		return hasDateDefined;
	}

	public void setHasDateDefined(Boolean hasDateDefined) {
		this.hasDateDefined = hasDateDefined;
	}

	public Boolean getIsLocationDefined() {
		return isLocationDefined;
	}

	public void setIsLocationDefined(Boolean isLocationDefined) {
		this.isLocationDefined = isLocationDefined;
	}

	public Boolean getIsIdValidated() {
		return isIdValidated;
	}

	public void setIsIdValidated(Boolean isIdValidated) {
		this.isIdValidated = isIdValidated;
	}

	public Boolean getHasTaxonName() {
		return hasTaxonName;
	}

	public void setHasTaxonName(Boolean hasTaxonName) {
		this.hasTaxonName = hasTaxonName;
	}

	public Boolean getHasfamilyRankOrLower() {
		return hasfamilyRankOrLower;
	}

	public void setHasfamilyRankOrLower(Boolean hasfamilyRankOrLower) {
		this.hasfamilyRankOrLower = hasfamilyRankOrLower;
	}

	public Boolean getIsNotFlagged() {
		return isNotFlagged;
	}

	public void setIsNotFlagged(Boolean isNotFlagged) {
		this.isNotFlagged = isNotFlagged;
	}

	public Boolean getIsNativeObservation() {
		return isNativeObservation;
	}

	public void setIsNativeObservation(Boolean isNativeObservation) {
		this.isNativeObservation = isNativeObservation;
	}

	
	
	
}
