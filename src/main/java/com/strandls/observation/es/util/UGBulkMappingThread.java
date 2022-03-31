package com.strandls.observation.es.util;

import com.strandls.observation.Headers;
import com.strandls.userGroup.ApiException;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.BulkGroupPostingData;
import com.strandls.userGroup.pojo.BulkGroupUnPostingData;

public class UGBulkMappingThread implements Runnable {

	private BulkGroupPostingData bulkPostingData;

	private UserGroupSerivceApi ugService;
	private BulkGroupUnPostingData bulkUnPostingData;
	private Headers headers;
	private String requestAuthHeader;

	public UGBulkMappingThread(BulkGroupPostingData bulkPostingData, UserGroupSerivceApi ugService,
			BulkGroupUnPostingData bulkUnPostingData, Headers headers, String requestAuthHeader) {
		super();
		this.bulkPostingData = bulkPostingData;
		this.ugService = ugService;
		this.bulkUnPostingData = bulkUnPostingData;
		this.headers = headers;
		this.requestAuthHeader = requestAuthHeader;
	}

	@Override
	public void run() {
		ugService = headers.addUserGroupHeader(ugService, requestAuthHeader);
		try {
			if (bulkPostingData != null) {
				ugService.bulkPostingObservationUG(bulkPostingData);
			} else if (bulkUnPostingData != null) {
				ugService.bulkRemovingObservation(bulkUnPostingData);
			}

		} catch (ApiException e) {
			e.printStackTrace();
		}

	}

}
