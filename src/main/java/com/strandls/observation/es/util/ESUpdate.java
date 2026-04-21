/**
 * 
 */
package com.strandls.observation.es.util;

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.ApiException;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapQueryResponse;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class ESUpdate {

	private final Logger logger = LoggerFactory.getLogger(ESUpdate.class);

	@Inject
	private EsServicesApi esService;

	@Inject
	private ConstructESDocument constructESDocument;

	@Inject
	private ObjectMapper om;

	public void pushToElastic(String observationId) {
		try {
			System.out.println("Observation getting pushed to elastic, ID:" + observationId);
			List<ObservationESDocument> result = constructESDocument.getESDocumentStub(observationId);
//			SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
//			om.setDateFormat(df);
			String resultString = om.writeValueAsString(result.get(0));
			MapDocument doc = new MapDocument();
			doc.setDocument(resultString);
			MapQueryResponse response = esService.create(ObservationIndex.INDEX.getValue(),
					ObservationIndex.TYPE.getValue(), observationId, doc);
			System.out.println(response.getResult());

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
	}

	public void esBulkUpload(String observationIds) {
		System.out.println("--------------------observation es Bulk Upload Started---------" + observationIds);
		try {
			List<ObservationESDocument> ESObservationList = constructESDocument.getESDocumentStub(observationIds);

			if (!ESObservationList.isEmpty()) {

				logger.info("DEBUG: All observation IDs in list (total {}): {}", ESObservationList.size(),
						ESObservationList.stream().map(d -> d.getObservation_id()).collect(Collectors.toList()));

				// Deduplicate by observation_id
				Map<Long, ObservationESDocument> uniqueObservations = new java.util.LinkedHashMap<>();
				for (ObservationESDocument doc : ESObservationList) {
					uniqueObservations.putIfAbsent(doc.getObservation_id(), doc);
				}
				ESObservationList = new java.util.ArrayList<>(uniqueObservations.values());

				logger.info("DEBUG: After deduplication, unique documents: {}", ESObservationList.size());

				List<Map<String, Object>> bulkEsDoc = ESObservationList.stream().map(s -> {
					SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
					om.setDateFormat(df);

					@SuppressWarnings("unchecked")
					Map<String, Object> doc = om.convertValue(s, Map.class);
					doc.putIfAbsent("id", s.getObservation_id());
					return doc;
				}).collect(Collectors.toList());

				if (!bulkEsDoc.isEmpty()) {
					Map<String, Object> firstMap = bulkEsDoc.get(0);
					logger.info("DEBUG: First Map after conversion:");
					logger.info("  id: {}", firstMap.get("id"));
					logger.info("  observation_id: {}", firstMap.get("observation_id"));
					logger.info("  location field type: {}",
							firstMap.get("location") != null ? firstMap.get("location").getClass().getName() : "null");
					logger.info("  location value: {}", firstMap.get("location"));
				}

				esService.bulkUpload(ObservationIndex.INDEX.getValue(), ObservationIndex.TYPE.getValue(), bulkEsDoc);

				System.out.println("--------------completed-------------observationId");
			}

		} catch (Exception e) {
			logger.error("ERROR in esBulkUpload: ", e);
		}
	}

	/**
	 * Bulk upload observations using new endpoint that accepts JSON string directly
	 * This preserves null values during serialization
	 * Used for list page bulk post/unpost operations
	 */
	public void esBulkUploadObservations(String observationIds) {
		System.out.println("--------------------observation es Bulk Upload (Observations) Started---------" + observationIds);
		try {
			List<ObservationESDocument> ESObservationList;

			ESObservationList = constructESDocument.getESDocumentStub(observationIds);
			if (!ESObservationList.isEmpty()) {

				// Remove duplicates
				Map<Long, ObservationESDocument> uniqueObservations = new java.util.LinkedHashMap<>();
				for (ObservationESDocument doc : ESObservationList) {
					uniqueObservations.putIfAbsent(doc.getObservation_id(), doc);
				}
				ESObservationList = new java.util.ArrayList<>(uniqueObservations.values());

				// Convert to Maps and add "id" field (required by ES)
				List<Map<String, Object>> bulkEsDoc = ESObservationList.stream().map(s -> {
					try {
						Map<String, Object> doc = om.convertValue(s, new TypeReference<Map<String, Object>>() {
						});
						// Add "id" field required by Elasticsearch
						doc.putIfAbsent("id", s.getObservation_id());
						return doc;
					} catch (Exception e) {
						logger.error("Error converting ObservationESDocument to Map: {}", e.getMessage());
						throw new RuntimeException(e);
					}
				}).collect(Collectors.toList());

				// Serialize Maps to JSON string (preserves null values with ALWAYS inclusion)
				SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
				om.setDateFormat(df);
				String jsonArray = om.writeValueAsString(bulkEsDoc);

				// Debug: Log first 500 chars of JSON being sent
				System.out.println("DEBUG: JSON being sent to ES (first 500 chars): " +
					(jsonArray.length() > 500 ? jsonArray.substring(0, 500) + "..." : jsonArray));

				// Call NEW endpoint that accepts String directly (no Map conversion)
				List<MapQueryResponse> response = esService.bulkUploadObservations(ObservationIndex.INDEX.getValue(),
						ObservationIndex.TYPE.getValue(), jsonArray);

				// Debug: Log response
				System.out.println("DEBUG: ES bulk upload response: " + response);
				System.out.println("--------------completed-------------observationId");
			}

		} catch (Exception e) {
			logger.error("ERROR in esBulkUploadObservations: ", e);
		}
	}

	public void updateESInstance(String observationId) {
		try {
			System.out.println("--------------------observation es Update---------");
			System.out.println();
			System.out.println("------started----------");
			System.out.println("Observation getting UPDATED to elastic, ID:" + observationId);
			List<ObservationESDocument> result = constructESDocument.getESDocumentStub(observationId);
//			SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
//			om.setDateFormat(df);
			String resultString = om.writeValueAsString(result.get(0));
			MapDocument doc = new MapDocument();
			doc.setDocument(resultString);
			MapQueryResponse response = esService.create(ObservationIndex.INDEX.getValue(),
					ObservationIndex.TYPE.getValue(), observationId, doc);
			System.out.println();
			System.out.println();
			System.out.println("-----------updated----------");
			System.out.println(response.getResult());
			System.out.println("--------------completed-------------observationId :" + observationId);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

}
