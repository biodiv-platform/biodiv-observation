/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;

/**
 * @author Abhishek Rudra
 *
 * 
 */
public class ReindexLatchManager extends Thread {

	private Long startPoint;
	private ObservationDAO observationDao;
	private ObservationService observationService;
	private EsServicesApi esService;
	private ObjectMapper om;
	private CountDownLatch outerLatch;

	/**
	 * 
	 */
	public ReindexLatchManager() {
		super();
	}

	/**
	 * @param startPoint
	 * @param observationDao
	 * @param observationService
	 * @param esService
	 * @param om
	 * @param outerLatch
	 */
	public ReindexLatchManager(Long startPoint, ObservationDAO observationDao, ObservationService observationService,
			EsServicesApi esService, ObjectMapper om, CountDownLatch outerLatch) {
		super();
		this.startPoint = startPoint;
		this.observationDao = observationDao;
		this.observationService = observationService;
		this.esService = esService;
		this.om = om;
		this.outerLatch = outerLatch;
	}

	@Override
	public void run() {
		List<Long> observationIds = observationDao.fetchObservationIdsList(startPoint);

		int length = observationIds.size();

		int i = 0;
		List<ShowData> result = new ArrayList<ShowData>();
		CountDownLatch latch = new CountDownLatch(5);
		length = length - 5;
		for (Long id : observationIds) {

			System.out.println("Inner Latch  observation Id: " + id);
			ReindexLatch reindexLatch = new ReindexLatch(id, result, observationService, latch);
			reindexLatch.start();
			i++;

			if (i % 5 == 0 || i % length == 0) {
				try {
					latch.await();
					System.out.println("INNER LATCH AWAIT START POINT :" + startPoint);

					if (result.size() == 500 || result.size() == length) {
						String jsonArray = om.writeValueAsString(result);
						esService.bulkUpload("extended_observation123", ObservationIndex.type.getValue(), jsonArray);

						result.clear();
					}

					if (length <= 5) {
						i = 0;
						latch = new CountDownLatch(length);
					} else {
						latch = new CountDownLatch(5);
						length = length - 5;
					}

				} catch (Exception e) {
					e.printStackTrace();
				}
			}

		}
		System.out.println(startPoint + " startpoint inner latch completed");
		System.out.println("OUTER LATCH COUNT DOWN");
		outerLatch.countDown();

	}

}
