package com.strandls.observation.util;

import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.Observation;

import java.util.List;
import java.util.concurrent.BlockingQueue;

public class ObservationThread implements Runnable {

    private final Long datatableId;
    private final BlockingQueue<Long> queue;
    private final ObservationDAO observationDAO;

    public ObservationThread(BlockingQueue<Long> queue, ObservationDAO observationDAO, Long datatableId) {
        this.queue = queue;
        this.observationDAO = observationDAO;
        this.datatableId = datatableId;
    }

    @Override
    public void run() {
        try {
            List<Observation> observationList = observationDAO.getObservationCountForDatatable(datatableId);
            String ObservationIds = null;
            
            
            for (Observation observation: observationList) {
            	ObservationIds = ObservationIds + ","+observation.getId().toString();
            	if(ObservationIds.length() >= 100 ) {
            		//call BulkUpload function as a thread;
            		//clear observation;
            	}
            	
                queue.put(observation.getId());
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
}
