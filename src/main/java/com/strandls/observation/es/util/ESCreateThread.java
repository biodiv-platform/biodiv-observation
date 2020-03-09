/**
 * 
 */
package com.strandls.observation.es.util;

/**
 * @author Abhishek Rudra
 *
 */
public class ESCreateThread implements Runnable {

	private ESUpdate esUpdate;
	private String observationId;

	/**
	 * 
	 */
	public ESCreateThread() {
		super();
	}

	/**
	 * @param esUpdate
	 * @param observationId
	 */
	public ESCreateThread(ESUpdate esUpdate, String observationId) {
		super();
		this.esUpdate = esUpdate;
		this.observationId = observationId;
	}

	@Override
	public void run() {
		esUpdate.pushToElastic(observationId);
	}

}
