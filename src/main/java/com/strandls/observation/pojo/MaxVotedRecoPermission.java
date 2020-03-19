/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class MaxVotedRecoPermission {

	private Long observationId;
	private Boolean permission;

	/**
	 * 
	 */
	public MaxVotedRecoPermission() {
		super();
	}

	/**
	 * @param observationId
	 * @param permission
	 */
	public MaxVotedRecoPermission(Long observationId, Boolean permission) {
		super();
		this.observationId = observationId;
		this.permission = permission;
	}

	public Long getObservationId() {
		return observationId;
	}

	public void setObservationId(Long observationId) {
		this.observationId = observationId;
	}

	public Boolean getPermission() {
		return permission;
	}

	public void setPermission(Boolean permission) {
		this.permission = permission;
	}

}
