package com.strandls.observation.es.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;

import jakarta.inject.Inject;

/**
 * Service to fetch full observation documents from Elasticsearch
 * Reuses existing ObservationESDocument class
 *
 * @author Optimization Team
 */
public class ObservationESFetcher {

    private static final Logger logger = LoggerFactory.getLogger(ObservationESFetcher.class);

    @Inject
    private EsServicesApi esService;

    @Inject
    private ObjectMapper objectMapper;

    /**
     * Fetch full observation document from Elasticsearch
     *
     * @param observationId The observation ID to fetch
     * @return ObservationESDocument or null if not found/error
     */
    public ObservationESDocument fetchObservationDocument(Long observationId) {
        if (observationId == null) {
            return null;
        }

        try {
            String index = ObservationIndex.INDEX.getValue();
            String type = ObservationIndex.TYPE.getValue();

            logger.debug("Fetching observation {} from ES index {}", observationId, index);

            // Fetch document from ES using the esService
            // The ES module API returns MapDocument
            MapDocument mapDocument = esService.fetch(index, type, observationId.toString());

            if (mapDocument == null || mapDocument.getDocument() == null) {
                logger.debug("Observation {} not found in ES", observationId);
                return null;
            }

            // Extract JSON string from MapDocument
            String jsonSource = String.valueOf(mapDocument.getDocument());

            logger.debug("ES Response JSON (first 500 chars): {}",
                jsonSource.length() > 500 ? jsonSource.substring(0, 500) + "..." : jsonSource);

            // Parse JSON to ObservationESDocument
            ObservationESDocument doc = objectMapper.readValue(jsonSource, ObservationESDocument.class);

            if (doc != null) {
                logger.debug("Successfully fetched observation {} from ES - authorId: {}, lat: {}, lon: {}",
                    observationId, doc.getAuthor_id(), doc.getLocationLat(), doc.getLocationLon());
            } else {
                logger.warn("Failed to parse ES response for observation {}", observationId);
            }

            return doc;

        } catch (Exception e) {
            logger.error("Error fetching observation {} from ES: {}", observationId, e.getMessage(), e);
            return null;
        }
    }


    /**
     * Check if observation exists in ES
     */
    public boolean observationExistsInES(Long observationId) {
        try {
            ObservationESDocument doc = fetchObservationDocument(observationId);
            return doc != null;
        } catch (Exception e) {
            logger.warn("Error checking if observation {} exists in ES", observationId, e);
            return false;
        }
    }
}
