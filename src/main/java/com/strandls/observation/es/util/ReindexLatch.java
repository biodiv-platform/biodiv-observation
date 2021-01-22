/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.List;
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
	private List<ShowData> showDataList;
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
	 * @param showDataList
	 * @param observationService
	 * @param latch
	 */
	public ReindexLatch(Long id, List<ShowData> showDataList, ObservationService observationService,
			CountDownLatch latch) {
		super();
		this.id = id;
		this.showDataList = showDataList;
		this.observationService = observationService;
		this.latch = latch;
	}

	@Override
	public void run() {
		try {
			ShowData result = observationService.findById(id);
			if (result != null) {
				result.setId(result.getObservation().getId());
				showDataList.add(result);
			}
			latch.countDown();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

}
