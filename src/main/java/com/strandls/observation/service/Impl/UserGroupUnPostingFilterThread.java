/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.strandls.observation.service.ObservationService;

/**
 * @author Abhishek Rudra
 *
 */
public class UserGroupUnPostingFilterThread implements Runnable {

	private ObservationService service;

	private String userGroupId;

	/**
	 * 
	 */
	public UserGroupUnPostingFilterThread() {
		super();
	}

	/**
	 * @param service
	 * @param userGroupId
	 */
	public UserGroupUnPostingFilterThread(ObservationService service, String userGroupId) {
		super();
		this.service = service;
		this.userGroupId = userGroupId;
	}

	@Override
	public void run() {
		service.applyFilterObservationRemoving(userGroupId);

	}

}
