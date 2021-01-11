package com.strandls.observation.util;

import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationBulkData;
import com.strandls.observation.service.Impl.ObservationBulkMapperHelper;
import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.BlockingQueue;

public class ObservationTask implements Runnable {
    private final Logger logger = LoggerFactory.getLogger(ObservationTask.class);
    private final BlockingQueue<ObservationBulkData> queue;
    private final ObservationBulkMapperHelper mapper;
    private final ObservationDAO observationDAO;

    public ObservationTask(BlockingQueue<ObservationBulkData> queue, ObservationBulkMapperHelper mapper,
                           ObservationDAO observationDAO) {
        this.queue = queue;
        this.mapper = mapper;
        this.observationDAO = observationDAO;
    }

    @Override
    public void run() {
        ObservationBulkData observationBulkData;
        while (true) {
            try {
                observationBulkData = queue.take();
                createObservationAndMappings(observationBulkData);
            } catch (InterruptedException ex) {
                break;
            }
        }
        while ((observationBulkData = queue.poll()) != null) {
            createObservationAndMappings(observationBulkData);
        }
    }

    private void createObservationAndMappings(ObservationBulkData observationData) {
        try {
            CommonProfile profile = AuthUtil.getProfileFromRequest(observationData.getRequest());
            Long userId = Long.parseLong(profile.getId());
            Observation observation = mapper.creationObservationMapping(userId,
                    observationData.getFieldMapping(), observationData.getDataRow(),
                    observationData.getDataTable(), observationData.getSpeciesGroupList());
            if (observation == null) return;

            observation = observationDAO.save(observation);
            mapper.createObservationResource(observationData.getRequest(), observationData.getDataRow(),
                    observationData.getFieldMapping(), observationData.getLicenses(), userId, observation);
            mapper.createRecoMapping(observationData.getRequest(), observationData.getFieldMapping(),
                    observationData.getDataRow(), observation, userId);
            mapper.createFactsMapping(observationData.getRequest(), observationData.getFieldMapping(),
                    observationData.getDataRow(), observationData.getPairs(), observation.getId());
            mapper.createTags(observationData.getRequest(), observationData.getFieldMapping(),
                    observationData.getDataRow(), observation.getId());
            mapper.createUserGroupMapping(observationData.getRequest(), observationData.getFieldMapping(),
                    observationData.getDataRow(), observationData.getUserGroupsList(), observation.getId());
            mapper.updateGeoPrivacy(observation);
            mapper.updateUserGroupFilter(observationData.getRequest(), observation);

            // add ESCreateThread here
//            mapper.updateESThread(observation.getId());
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }
}
