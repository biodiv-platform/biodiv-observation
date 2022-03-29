package com.strandls.observation.es.util;

import com.strandls.userGroup.ApiException;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.BulkGroupPostingData;
import com.strandls.userGroup.pojo.BulkGroupUnPostingData;

public class UGBulkMappingThread implements Runnable {

	private BulkGroupPostingData bulkPostingData;
	private UserGroupSerivceApi ugService;
	private BulkGroupUnPostingData bulkUnPostingData;

	@Override
	public void run() {

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
