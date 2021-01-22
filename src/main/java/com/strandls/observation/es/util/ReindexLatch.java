/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.Map;
import java.util.concurrent.CountDownLatch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;

/**
 * @author Abhishek Rudra
 *
 * 
 */
public class ReindexLatch extends Thread {

	private final Logger logger = LoggerFactory.getLogger(ReindexLatch.class);

	private Long id;
	private Map<Long, ShowData> showDataMap;
	private ObservationService observationService;
	private CountDownLatch latch;

	/**
	 * 
	 */
	public ReindexLatch() {
		super();
	}

	/**
	 * @param id
	 * @param showDataMap
	 * @param observationService
	 * @param latch
	 */
	public ReindexLatch(Long id, Map<Long, ShowData> showDataMap, ObservationService observationService,
			CountDownLatch latch) {
		super();
		this.id = id;
		this.showDataMap = showDataMap;
		this.observationService = observationService;
		this.latch = latch;
	}

	@Override
	public void run() {
		try {
			ShowData result = observationService.findById(id);
			if (result != null) {
				result.setId(result.getObservation().getId());
				showDataMap.put(result.getId(), result);
			}
			latch.countDown();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

}
