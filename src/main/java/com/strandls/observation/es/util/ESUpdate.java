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

import com.fasterxml.jackson.core.JsonProcessingException;
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
			List<ObservationESDocument> ESObservationList;

			ESObservationList = constructESDocument.getESDocumentStub(observationIds);
			if (!ESObservationList.isEmpty()) {

				// DEBUG: Log all observation IDs to check for duplicates
				logger.info("DEBUG: All observation IDs in list (total {}): {}",
					ESObservationList.size(),
					ESObservationList.stream().map(d -> d.getObservation_id()).collect(Collectors.toList()));

				// CRITICAL FIX: Remove duplicates - SQL query returns multiple rows per observation
				// Use LinkedHashMap to preserve order and keep first occurrence
				Map<Long, ObservationESDocument> uniqueObservations = new java.util.LinkedHashMap<>();
				for (ObservationESDocument doc : ESObservationList) {
					uniqueObservations.putIfAbsent(doc.getObservation_id(), doc);
				}
				ESObservationList = new java.util.ArrayList<>(uniqueObservations.values());
				logger.info("DEBUG: After deduplication, unique documents: {}", ESObservationList.size());

				// DEBUG: Log first document before conversion
				logger.info("DEBUG: First ObservationESDocument before Map conversion:");
				ObservationESDocument firstDoc = ESObservationList.get(0);
				logger.info("  observation_id: {}", firstDoc.getObservation_id());
				logger.info("  location field type: {}", firstDoc.getLocation() != null ? firstDoc.getLocation().getClass().getName() : "null");
				logger.info("  location value: {}", firstDoc.getLocation());
				logger.info("  max_voted_reco field type: {}", firstDoc.getMax_voted_reco() != null ? firstDoc.getMax_voted_reco().getClass().getName() : "null");
				logger.info("  user_group_observations field type: {}", firstDoc.getUser_group_observations() != null ? firstDoc.getUser_group_observations().getClass().getName() : "null");

				List<Map<String, Object>> bulkEsDoc = ESObservationList.stream().map(s -> {
					SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
					om.setDateFormat(df);
					@SuppressWarnings("unchecked")
					Map<String, Object> doc = om.convertValue(s, Map.class);
					doc.putIfAbsent("id", s.getObservation_id());
					return doc;
				}).collect(Collectors.toList());

				// DEBUG: Log first Map after conversion
				logger.info("DEBUG: First Map after conversion:");
				Map<String, Object> firstMap = bulkEsDoc.get(0);
				logger.info("  id: {}", firstMap.get("id"));
				logger.info("  observation_id: {}", firstMap.get("observation_id"));
				logger.info("  location field type: {}", firstMap.get("location") != null ? firstMap.get("location").getClass().getName() : "null");
				logger.info("  location value: {}", firstMap.get("location"));

				String json = om.writeValueAsString(bulkEsDoc);

				// DEBUG: Log JSON sample (first 2000 chars to avoid huge logs)
				logger.info("DEBUG: Final JSON being sent to ES (first 2000 chars):");
				logger.info(json.length() > 2000 ? json.substring(0, 2000) + "..." : json);

				esService.bulkUpload(ObservationIndex.INDEX.getValue(), ObservationIndex.TYPE.getValue(),
						json);
				System.out.println("--------------completed-------------observationId");

			}

		} catch (ApiException | JsonProcessingException e) {
			logger.error("ERROR in esBulkUpload: ", e);
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
