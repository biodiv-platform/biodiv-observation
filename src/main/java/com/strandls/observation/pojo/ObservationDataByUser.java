/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Mekala Rishitha Ravi
 *
 */
public class ObservationDataByUser {

	private Object createdOn;
	private Object observedOn;

	/**
	 * 
	 */
	public ObservationDataByUser() {
		super();
	}

	/**
	 * @param createdOn
	 * @param observedOn
	 */
	public ObservationDataByUser(Object createdOn, Object observedOn) {
		super();
		this.createdOn = createdOn;
		this.observedOn = observedOn;

	}

	public Object getCreatedOn() {
		return createdOn;
	}

	public void setCreatedOn(Object createdOn) {
		this.createdOn = createdOn;
	}

	public Object getObservedOn() {
		return observedOn;
	}

	public void setObservedOn(Object observedOn) {
		this.observedOn = observedOn;
	}

}
