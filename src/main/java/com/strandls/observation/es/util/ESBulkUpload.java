package com.strandls.observation.es.util;

import java.util.List;

/**	
 * 
 * @author vishnu
 *
 */
public class ESBulkUpload implements Runnable {

	private ESUpdate esUpdate;
	private List<Long> observationIds;

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
	public ESBulkUpload(ESUpdate esUpdate, List<Long> observationIds) {
		super();
		this.esUpdate = esUpdate;
		this.observationIds = observationIds;
	}

	@Override
	public void run() {
		esUpdate.esBulkUpload(observationIds);
	}

}
