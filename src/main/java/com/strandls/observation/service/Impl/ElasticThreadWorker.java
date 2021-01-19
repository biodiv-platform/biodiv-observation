package com.strandls.observation.service.Impl;

import com.strandls.observation.es.util.ESCreateThread;
import com.strandls.observation.es.util.ESUpdate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class ElasticThreadWorker extends Thread {

    private final Logger logger = LoggerFactory.getLogger(ElasticThreadWorker.class);

    private final ESUpdate esUpdate;
    private final Long id;

    public ElasticThreadWorker(ESUpdate esUpdate, Long id) {
        this.esUpdate = esUpdate;
        this.id = id;
    }

    @Override
    public void run() {
        try {
            ExecutorService executorService = Executors.newSingleThreadExecutor();
            ESCreateThread esThread = new ESCreateThread(esUpdate, id.toString());
            executorService.submit(esThread).get();
            executorService.shutdownNow();
            executorService.awaitTermination(5, TimeUnit.MINUTES);
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }
}
