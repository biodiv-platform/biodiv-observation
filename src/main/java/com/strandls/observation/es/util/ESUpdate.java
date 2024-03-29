/**
 * 
 */
package com.strandls.observation.es.util;

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.ApiException;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapQueryResponse;

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

				List<Map<String, Object>> bulkEsDoc = ESObservationList.stream().map(s -> {
					SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
					om.setDateFormat(df);
					@SuppressWarnings("unchecked")
					Map<String, Object> doc = om.convertValue(s, Map.class);
					doc.putIfAbsent("id", s.getObservation_id());
					return doc;
				}).collect(Collectors.toList());
				String json = om.writeValueAsString(bulkEsDoc);
				esService.bulkUpload(ObservationIndex.INDEX.getValue(), ObservationIndex.TYPE.getValue(),
						json.toString());
				System.out.println("--------------completed-------------observationId");

			}

		} catch (ApiException | JsonProcessingException e) {
			logger.error(e.getMessage());
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
