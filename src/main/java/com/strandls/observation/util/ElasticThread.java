package com.strandls.observation.util;

import com.strandls.observation.es.util.ESCreateThread;
import com.strandls.observation.es.util.ESUpdate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class ElasticThread implements Runnable {
    private final Logger logger = LoggerFactory.getLogger(ElasticThread.class);
    private final BlockingQueue<Long> queue;
    private final ESUpdate esUpdate;

    public ElasticThread(BlockingQueue<Long> queue, ESUpdate esUpdate) {
        this.queue = queue;
        this.esUpdate = esUpdate;
    }

    @Override
    public void run() {
        Long id;
        while (true) {
            try {
                id = queue.take();
                performElasticUpdate(id);
            } catch (Exception ex) {
                break;
            }
        }
        while ((id = queue.poll()) != null) {
            performElasticUpdate(id);
        }
    }

    public void performElasticUpdate(Long id) {
        try {
            ExecutorService executorService = Executors.newSingleThreadExecutor();
            ESCreateThread esThread = new ESCreateThread(esUpdate, id.toString());
            executorService.submit(esThread).get();
            executorService.shutdownNow();
            executorService.awaitTermination(5, TimeUnit.MINUTES);
        } catch (Exception ex) {
            
            logger.error(ex.getMessage());
        }
    }
}
