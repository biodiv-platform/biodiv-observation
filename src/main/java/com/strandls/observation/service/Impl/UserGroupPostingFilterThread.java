/**
 * 
 */
package com.strandls.observation.service.Impl;

import com.strandls.observation.service.ObservationService;

/**
 * @author Abhishek Rudra
 *
 */
public class UserGroupPostingFilterThread implements Runnable {

	private ObservationService service;

	private String userGroupId;

	/**
	 * 
	 */
	public UserGroupPostingFilterThread() {
		super();
	}

	/**
	 * @param service
	 * @param userGroupIds
	 */
	public UserGroupPostingFilterThread(ObservationService service, String userGroupId) {
		super();
		this.service = service;
		this.userGroupId = userGroupId;
	}

	@Override
	public void run() {
		service.applyFilterObservationPosting(userGroupId);

	}

}
