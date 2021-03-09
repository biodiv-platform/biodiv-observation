/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.Date;
import java.util.List;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationUpdateData {

	private List<ObservationResourceData> resources;
	private String notes;
//	------Date Data----------
	private String dateAccuracy;
	private Date observedOn;

//	-----Location Data--------
	private String observedAt;
	private String reverseGeocoded;
	private String locationScale;
	private Double latitude;
	private Double longitude;
	private Boolean hidePreciseLocation;

	/**
	 * 
	 */
	public ObservationUpdateData() {
		super();
	}

	/**
	 * @param resources
	 * @param notes
	 * @param dateAccuracy
	 * @param observedOn
	 * @param observedAt
	 * @param reverseGeocoded
	 * @param locationScale
	 * @param latitude
	 * @param longitude
	 * @param hidePreciseLocation
	 */
	public ObservationUpdateData(List<ObservationResourceData> resources, String notes, String dateAccuracy, Date observedOn,
			String observedAt, String reverseGeocoded, String locationScale, Double latitude, Double longitude,
			Boolean hidePreciseLocation) {
		super();
		this.resources = resources;
		this.notes = notes;
		this.dateAccuracy = dateAccuracy;
		this.observedOn = observedOn;
		this.observedAt = observedAt;
		this.reverseGeocoded = reverseGeocoded;
		this.locationScale = locationScale;
		this.latitude = latitude;
		this.longitude = longitude;
		this.hidePreciseLocation = hidePreciseLocation;
	}

	public List<ObservationResourceData> getResources() {
		return resources;
	}

	public void setResources(List<ObservationResourceData> resources) {
		this.resources = resources;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public String getDateAccuracy() {
		return dateAccuracy;
	}

	public void setDateAccuracy(String dateAccuracy) {
		this.dateAccuracy = dateAccuracy;
	}

	public Date getObservedOn() {
		return observedOn;
	}

	public void setObservedOn(Date observedOn) {
		this.observedOn = observedOn;
	}

	public String getObservedAt() {
		return observedAt;
	}

	public void setObservedAt(String observedAt) {
		this.observedAt = observedAt;
	}

	public String getReverseGeocoded() {
		return reverseGeocoded;
	}

	public void setReverseGeocoded(String reverseGeocoded) {
		this.reverseGeocoded = reverseGeocoded;
	}

	public String getLocationScale() {
		return locationScale;
	}

	public void setLocationScale(String locationScale) {
		this.locationScale = locationScale;
	}

	public Double getLatitude() {
		return latitude;
	}

	public void setLatitude(Double latitude) {
		this.latitude = latitude;
	}

	public Double getLongitude() {
		return longitude;
	}

	public void setLongitude(Double longitude) {
		this.longitude = longitude;
	}

	public Boolean getHidePreciseLocation() {
		return hidePreciseLocation;
	}

	public void setHidePreciseLocation(Boolean hidePreciseLocation) {
		this.hidePreciseLocation = hidePreciseLocation;
	}

}
