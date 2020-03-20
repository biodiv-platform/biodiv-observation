/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.Date;

/**
 * @author Abhishek Rudra
 *
 */
public class observationMailData {

	private Long observationId;
	private String location;
	private Date observedOn;
	private String iconURl;
	private String scientificName;
	private String commonName;
	private Long authorId;

	/**
	 * 
	 */
	public observationMailData() {
		super();
	}

	/**
	 * @param observationId
	 * @param location
	 * @param observedOn
	 * @param iconURl
	 * @param scientificName
	 * @param commonName
	 * @param authorId
	 */
	public observationMailData(Long observationId, String location, Date observedOn, String iconURl,
			String scientificName, String commonName, Long authorId) {
		super();
		this.observationId = observationId;
		this.location = location;
		this.observedOn = observedOn;
		this.iconURl = iconURl;
		this.scientificName = scientificName;
		this.commonName = commonName;
		this.authorId = authorId;
	}

	public Long getObservationId() {
		return observationId;
	}

	public void setObservationId(Long observationId) {
		this.observationId = observationId;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public Date getObservedOn() {
		return observedOn;
	}

	public void setObservedOn(Date observedOn) {
		this.observedOn = observedOn;
	}

	public String getIconURl() {
		return iconURl;
	}

	public void setIconURl(String iconURl) {
		this.iconURl = iconURl;
	}

	public String getScientificName() {
		return scientificName;
	}

	public void setScientificName(String scientificName) {
		this.scientificName = scientificName;
	}

	public String getCommonName() {
		return commonName;
	}

	public void setCommonName(String commonName) {
		this.commonName = commonName;
	}

	public Long getAuthorId() {
		return authorId;
	}

	public void setAuthorId(Long authorId) {
		this.authorId = authorId;
	}

}
