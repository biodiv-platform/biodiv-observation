package com.strandls.observation.es.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.controllers.EsServicesApi;

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
            // The ES module API returns the document as a Map or JSON
            Object response = esService.fetch(index, type, observationId.toString());

            if (response == null) {
                logger.debug("Observation {} not found in ES", observationId);
                return null;
            }

            // Convert response to ObservationESDocument
            ObservationESDocument doc = convertResponseToDocument(response);

            if (doc != null) {
                logger.debug("Successfully fetched observation {} from ES", observationId);
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
     * Convert ES response to ObservationESDocument
     * Handles both direct document and wrapped responses
     */
    private ObservationESDocument convertResponseToDocument(Object response) {
        try {
            // ES responses are typically wrapped in { "_source": {...} }
            // or directly returned as the document

            if (response instanceof String) {
                // Parse JSON string
                JsonNode rootNode = objectMapper.readTree((String) response);
                return extractDocument(rootNode);
            } else {
                // Convert object to JSON and then to ObservationESDocument
                String json = objectMapper.writeValueAsString(response);
                JsonNode rootNode = objectMapper.readTree(json);
                return extractDocument(rootNode);
            }

        } catch (Exception e) {
            logger.error("Error converting ES response to document", e);
            return null;
        }
    }

    /**
     * Extract document from various ES response formats
     */
    private ObservationESDocument extractDocument(JsonNode rootNode) throws Exception {
        // Check if response has _source field (standard ES response)
        if (rootNode.has("_source")) {
            return objectMapper.treeToValue(rootNode.get("_source"), ObservationESDocument.class);
        }

        // Check if response has hits (search response)
        if (rootNode.has("hits")) {
            JsonNode hits = rootNode.get("hits");
            if (hits.has("hits") && hits.get("hits").isArray() && hits.get("hits").size() > 0) {
                JsonNode firstHit = hits.get("hits").get(0);
                if (firstHit.has("_source")) {
                    return objectMapper.treeToValue(firstHit.get("_source"), ObservationESDocument.class);
                }
            }
        }

        // Assume the root node is the document itself
        return objectMapper.treeToValue(rootNode, ObservationESDocument.class);
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
