package com.strandls.observation.es.util;

import java.util.List;

public class ESBulkUploadThread implements Runnable {

	private ESUpdate esUpdate;
	private String observationIds;

	/**
	 * 
	 */
	public ESBulkUploadThread() {
		super();
	}

	/**
	 * @param esUpdate
	 * @param observationList
	 */
	public ESBulkUploadThread(ESUpdate esUpdate, String observationList) {
		super();
		this.esUpdate = esUpdate;
		this.observationIds = observationList;
	}

	@Override
	public void run() {

		esUpdate.esBulkUpload(observationIds);

	}

}
