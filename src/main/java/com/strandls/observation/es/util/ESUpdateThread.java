/**
 * 
 */
package com.strandls.observation.es.util;

/**
 * @author Abhishek Rudra
 *
 */
public class ESUpdateThread implements Runnable {

	private ESUpdate esUpdate;
	private String observationId;

	/**
	 * 
	 */
	public ESUpdateThread() {
		super();
	}

	/**
	 * @param esUpdate
	 * @param observationId
	 */
	public ESUpdateThread(ESUpdate esUpdate, String observationId) {
		super();
		this.esUpdate = esUpdate;
		this.observationId = observationId;
	}

	@Override
	public void run() {
		esUpdate.updateESInstance(observationId);
	}

}
