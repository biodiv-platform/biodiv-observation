package com.strandls.observation.es.util;

import java.util.List;

/**	
 * 
 * @author vishnu
 *
 */
public class ESBulkUpload implements Runnable {

	private ESUpdate esUpdate;
	private String observationIds;

	/**
	 * 
	 */
	public ESBulkUpload() {
		super();
	}

	/**
	 * @param esUpdate
	 * @param observationId
	 */
	public ESBulkUpload(ESUpdate esUpdate, String observationIds) {
		super();
		this.esUpdate = esUpdate;
		this.observationIds = observationIds;
	}

	@Override
	public void run() {
		esUpdate.esBulkUpload(observationIds);
	}

}
