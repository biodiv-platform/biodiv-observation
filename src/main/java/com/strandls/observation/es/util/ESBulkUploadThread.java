package com.strandls.observation.es.util;

import java.util.List;

public class ESBulkUploadThread implements Runnable  {
	
	
	private ESUpdate esUpdate;
	private List<Long> observationIds;

	/**
	 * 
	 */
	public ESBulkUploadThread() {
		super();
	}

	/**
	 * @param esUpdate
	 * @param observationIds
	 */
	public ESBulkUploadThread(ESUpdate esUpdate, List<Long> observationIds) {
		super();
		this.esUpdate = esUpdate;
		this.observationIds = observationIds;
	}

	@Override
	public void run() {
		esUpdate.esBulkUpload(observationIds);
	}

}
