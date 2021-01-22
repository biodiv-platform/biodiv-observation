/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.Date;
import java.util.concurrent.CountDownLatch;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.service.ObservationService;

/**
 * @author Abhishek Rudra
 *
 * 
 */
public class Reindexing {

	private final Logger logger = LoggerFactory.getLogger(Reindexing.class);
	@Inject
	private ObservationDAO observationDao;

	@Inject
	private ObservationService observationService;

	@Inject
	private EsServicesApi esService;

	@Inject
	private ObjectMapper objectMapper;

	public void reIndex() {
		try {
			System.out.println("");
			System.out.println("---------------------------");
			System.out.println("Start Time" + new Date());
			System.out.println("---------------------------");
			Long total = observationDao.findTotalObservation();
			Integer batchSize = 30000;
			long totalNumberLatch = total / batchSize;
			System.out.println("TOTAL :" + total);
			System.out.println("total / batchSize :" + total / batchSize);
			System.out.println("total % batchSize :" + total % batchSize);
			if (total % batchSize > 0)
				totalNumberLatch = totalNumberLatch + 1L;
			System.out.println("TOTAL NUMBER OF OUT LATCH REQ : " + totalNumberLatch);
			int length = (int) totalNumberLatch;
			CountDownLatch latch = new CountDownLatch(4);
			length = length - 4;
			Long startPoint = 1L;
			for (int i = 1; i <= totalNumberLatch; i++) {
				ReindexLatchManager manager = new ReindexLatchManager(startPoint, observationDao, observationService,
						esService, objectMapper, latch);
				manager.start();
				startPoint = startPoint + 30000L;

				if (i % 4 == 0 || i == totalNumberLatch) {
					try {
						latch.await();

						if (length <= 4)
							latch = new CountDownLatch(length);
						else {
							latch = new CountDownLatch(4);
							length = length - 4;
						}

					} catch (Exception e) {
						e.printStackTrace();
					}
				}

			}
			System.out.println("---------------------------");
			System.out.println("END TIME " + new Date());
			System.out.println("---------------------------");

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}
}
