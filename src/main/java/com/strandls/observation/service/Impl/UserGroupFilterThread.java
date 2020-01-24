/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.strandls.observation.service.ObservationService;

/**
 * @author Abhishek Rudra
 *
 */
public class UserGroupFilterThread implements Runnable {

	private ObservationService service;

	private String userGroupIds;

	/**
	 * 
	 */
	public UserGroupFilterThread() {
		super();
	}

	/**
	 * @param service
	 * @param userGroupIds
	 */

	@Override
	public void run() {
		service.applyFilterObservation(userGroupIds);

	}

	/**
	 * @param service
	 * @param userGroupIds
	 */
	public UserGroupFilterThread(ObservationService service, String userGroupIds) {
		super();
		this.service = service;
		this.userGroupIds = userGroupIds;
	}

}
