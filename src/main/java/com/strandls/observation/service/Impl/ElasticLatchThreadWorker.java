package com.strandls.observation.service.Impl;

import com.strandls.observation.es.util.ESCreateThread;
import com.strandls.observation.es.util.ESUpdate;
import org.apache.poi.ss.formula.functions.Count;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class ElasticLatchThreadWorker extends Thread {

    private final Logger logger = LoggerFactory.getLogger(ElasticLatchThreadWorker.class);

    private final ESUpdate esUpdate;
    private final Long id;
    private final CountDownLatch latch;

    public ElasticLatchThreadWorker(CountDownLatch latch, ESUpdate esUpdate, Long id) {
        this.esUpdate = esUpdate;
        this.id = id;
        this.latch = latch;
    }

    @Override
    public void run() {
        try {
            ExecutorService executorService = Executors.newSingleThreadExecutor();
            ESCreateThread esThread = new ESCreateThread(esUpdate, id.toString());
            executorService.submit(esThread).get();
            executorService.shutdownNow();
            executorService.awaitTermination(5, TimeUnit.MINUTES);
            latch.countDown();
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }
}
