package com.strandls.observation.gbif;

import java.io.File;
import java.io.FileReader;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.opencsv.CSVParser;
import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.ExtendedTaxonDefinition;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.LocationInfo;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.es.util.ExternalObservationESDocument;
import com.strandls.observation.es.util.GbifObservationESMapper;
import com.strandls.observation.es.util.ObservationESDocument;
import com.strandls.observation.pojo.RecoData;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.observation.util.PropertyFileUtil;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.ParsedName;

import java.text.DateFormatSymbols;

public class GbifObservationThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(GbifObservationService.class);
	private final UtilityServiceApi utilityService;
	private final EsServicesApi esService;
	private final RecommendationDao recoDao;
	private final GbifObservationESMapper gbifMapper;
	private final LayerServiceApi layerService;
	// private final Map<String, Integer> headerIndex;
	private final int startRow;
	private final int endRow;

	public GbifObservationThread(UtilityServiceApi utilityService, EsServicesApi esService, RecommendationDao recoDao,
			GbifObservationESMapper gbifMapper, LayerServiceApi layerService, int startRow, int endRow) {
		super();
		this.utilityService = utilityService;
		this.esService = esService;
		this.recoDao = recoDao;
		this.gbifMapper = gbifMapper;
		this.layerService = layerService;
		this.startRow = startRow;
		this.endRow = endRow;
	}

	@Override
	public void run() {

		try {

			String path = PropertyFileUtil.fetchProperty("config.properties", "datasetPath");
			String dataSourcePrefix = PropertyFileUtil.fetchProperty("config.properties", "gbifUniqueIdPrefix");

			String excludingPublishingOrgKey = PropertyFileUtil.fetchProperty("config.properties",
					"excludingPublishingOrgKey");
			FileReader filereader1 = new FileReader(path);
			CSVReader csvReader1 = new CSVReader(filereader1, '\t');

			String r[] = csvReader1.readNext();
			String[] columns = r.clone();

			List<String> markedColumns = new ArrayList<>();
			Map<String, Integer> headerIndex = new LinkedHashMap<>();
			for (int j = 0; j < r.length; j++) {
				headerIndex.put(r[j], j);
			}
			FileReader filereader2 = new FileReader(path);
			CSVReader csvReader = new CSVReader(filereader2, '\t', '"', startRow);

			/*
			 * CSVReader csvReader = new CSVReaderBuilder(filereader)
			 * 
			 * .withSkipLines(startRow).withCSVParser(new
			 * CSVParserBuilder().withSeparator('\t').build()).build();
			 */

			ObjectMapper jsonReadMapper = new ObjectMapper();

			/*
			 * @SuppressWarnings("unchecked") Map<String, Object> jsonFileMap =
			 * jsonReadMapper.readValue(new File("/home/prakhar/Desktop/metaData.json"),
			 * HashMap.class); System.out.println(jsonFileMap);
			 */

			String batchEsJson = "";

			// String[] row = csvReader.readNext();

			/*
			 * for (int j = 0; j < row.length; j++) { headerIndex.put(row[j], j); }
			 */
			List<ExternalObservationESDocument> observations = new ArrayList<>();
			// int ctr = startRow;
			String[] row;
			for (int i = startRow; i <= endRow; i++) {
				row = csvReader.readNext();
				/*
				 * if (ctr > endRow) { break; } ctr++;
				 */
				// System.out.println(headerIndex);
				if (row == null) {
					System.out.println("row empty i=" + i);
				}
				if (row[headerIndex.get("publishingOrgKey")] == null) {
					System.out.println("id=" + row[headerIndex.get("gbifID")]);
				}
				String publishingOrgKey = row[headerIndex.get("publishingOrgKey")];
				String latitude = row[headerIndex.get("decimalLatitude")];
				String longitude = row[headerIndex.get("decimalLongitude")];
				String tempId = row[headerIndex.get("gbifID")];

				if (publishingOrgKey != null && !publishingOrgKey.equals(excludingPublishingOrgKey)
						&& !isBadRecord(latitude, longitude)) {
					String externalOriginalReferenceLink = row[headerIndex.get("occurrenceID")];
					String gbifId = row[headerIndex.get("gbifID")];
					String datetime = row[headerIndex.get("eventDate")];
					LocalDateTime dateTime = LocalDateTime.parse(datetime);
					Date date = Timestamp.valueOf(dateTime);
					Date lastModified = null;
					String lastModifiedDateString = row[headerIndex.get("dateIdentified")];
					if (!lastModifiedDateString.isEmpty()) {
						LocalDateTime dateTime2 = LocalDateTime.parse(lastModifiedDateString);
						lastModified = Timestamp.valueOf(dateTime2);
					} else {
						lastModified = null;
					}

					String month = row[headerIndex.get("month")];
					String monthName = getMonthName(month);
					Double lat = null;
					/*
					 * CSVReader csvReader = new CSVReaderBuilder(filereader)
					 * 
					 * .withSkipLines(startRow).withCSVParser(new
					 * CSVParserBuilder().withSeparator('\t').build()).build();
					 */

					Double lon = null;

					lat = Double.parseDouble(row[headerIndex.get("decimalLatitude")]);
					lon = Double.parseDouble(row[headerIndex.get("decimalLongitude")]);

					String placeName = row[headerIndex.get("locality")];

					if (placeName.equals("")) {
						placeName = null;
					}

					String gbifScientificName = row[headerIndex.get("scientificName")];

					RecoData recoData = new RecoData();
					recoData.setTaxonScientificName(gbifScientificName);

					Map<String, Object> recoAndTaxonId = getRecoAndTaxonId(recoData);
					Long recoId = null;
					Long taxonId = null;
					if (recoAndTaxonId.get("recoId") != null) {
						recoId = Long.parseLong(recoAndTaxonId.get("recoId").toString());
					}

					String scientificName = gbifScientificName;
					if (recoAndTaxonId.get("taxonId") != null) {
						taxonId = Long.parseLong(recoAndTaxonId.get("taxonId").toString());

					}

					ExtendedTaxonDefinition taxonDetails = (ExtendedTaxonDefinition) recoAndTaxonId.get("etd");
					if (taxonId != null) {
						scientificName = taxonDetails.getName();
					} /*
						 * else if (recoId != null) { Recommendation reco = recoDao.findById(recoId);
						 * scientificName = reco.getName(); }
						 */

					Long rank = null;
					Long speciesId = null;
					String taxonStatus = null;

					Long acceptedNameIds = null;
					String italicisedForm = null;
					String position = null;
					String cannonicalName = null;
					String name = null;
					Long groupId = null;
					String groupName = null;
					List<Map<String, String>> hierarchy = new ArrayList<>();

					if (taxonId != null) {
						rank = Long.parseLong(taxonDetails.getRank().toString());

						if (taxonDetails.getAcceptedIds() != null) {
							acceptedNameIds = Long.parseLong(taxonDetails.getAcceptedIds().get(0).toString());
						} else {
							acceptedNameIds = taxonId;
						}

						if (taxonDetails.getItalicisedForm() != null) {
							italicisedForm = taxonDetails.getItalicisedForm().toString();
						}

						if (taxonDetails.getPosition() != null) {
							position = taxonDetails.getPosition().toString();
						}

						if (taxonDetails.getCanonicalForm() != null) {
							cannonicalName = taxonDetails.getCanonicalForm();
						}

						if (taxonDetails.getGroupId() != null) {
							groupId = (long) Math.round(taxonDetails.getGroupId());
						}

						if (taxonDetails.getGroupName() != null) {
							groupName = taxonDetails.getGroupName().toString();
						}

						if (taxonDetails.getPath() != null) {
							String[] taxonPath = taxonDetails.getPath().toString().split("_");
							hierarchy = getHierarchy(taxonPath);
						}

						if (taxonDetails.getName() != null) {
							name = taxonDetails.getName().toString();
						}

						if (taxonDetails.getStatus() != null) {
							taxonStatus = taxonDetails.getStatus().toString();
						}

						if (taxonDetails.getSpeciesId() != null) {
							speciesId = Long.parseLong(taxonDetails.getSpeciesId().toString());
						}
					}

					//LocationInfo locationInfo = new LocationInfo();
				//	ObservationLocationInfo layerInfo = new ObservationLocationInfo();
					String state = null;
					String district = null;
					String tahsil = null;

					/*
					 * locationInfo = layerService.fetchLocationInfo(String.valueOf(lat),
					 * String.valueOf(lon)); state = locationInfo.getState(); district =
					 * locationInfo.getDistrict(); tahsil = locationInfo.getTahsil();
					 */

					String externalGbifReferenceLink = "https://www.gbif.org/occurrence/" + gbifId.toString();

					markedColumns.add("publishingOrgKey");
					markedColumns.add("decimalLatitude");
					markedColumns.add("decimalLongitude");
					markedColumns.add("gbifID");
					markedColumns.add("occurrenceID");
					markedColumns.add("eventDate");
					markedColumns.add("dateIdentified");
					markedColumns.add("month");
					markedColumns.add("locality");
					markedColumns.add("scientificName");

					String annotations = getAnnotations(headerIndex, markedColumns, columns, row);
					System.out.println(annotations);

					ExternalObservationESDocument obj = gbifMapper.mapToESDocument(date, monthName, lat, lon, placeName,
							recoId, taxonId, rank, speciesId, taxonStatus, hierarchy, scientificName, cannonicalName,
							acceptedNameIds, italicisedForm, position, Long.parseLong(gbifId), lastModified, name,
							state, district, tahsil, groupId, groupName, externalOriginalReferenceLink,
							externalGbifReferenceLink, null, annotations,dataSourcePrefix);

					observations.add(obj);
					ObjectMapper objectMapper = new ObjectMapper();

					if (observations.size() >= 100) {
						/*
						 * List<Map<String, Object>> batchEsDoc = observations.stream().map(s -> {
						 * 
						 * @SuppressWarnings("unchecked") Map<String, Object> doc =
						 * objectMapper.convertValue(s, Map.class); doc.put("id", "gbif-" +
						 * s.getObservation_id()); return doc; }).collect(Collectors.toList());
						 */

						batchEsJson = objectMapper.writeValueAsString(observations);

						esService.bulkUpload("extended_observation", "_doc", batchEsJson);

						/*
						 * ESPushThread esPushThread = new ESPushThread("extended_observation", "_doc",
						 * batchEsJson, esService); Thread t1 = new Thread(esPushThread); t1.start();
						 */
						observations.clear();
					}

				}

			}

			ObjectMapper objectMapper = new ObjectMapper();
			if (!observations.isEmpty()) {
				List<Map<String, Object>> batchEsDoc = observations.stream().map(s -> {
					@SuppressWarnings("unchecked")
					Map<String, Object> doc = objectMapper.convertValue(s, Map.class);
					doc.put("id", "gbif-" + s.getObservation_id());
					return doc;
				}).collect(Collectors.toList());

				batchEsJson = objectMapper.writeValueAsString(batchEsDoc);

				esService.bulkUpload("extended_observation", "_doc", batchEsJson);

				/*
				 * ESPushThread esPushThread = new ESPushThread("extended_observation", "_doc",
				 * batchEsJson, esService); Thread t2 = new Thread(esPushThread); t2.start();
				 */
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private String getMonthName(String month) {
		int monthNumber = Integer.parseInt(month);
		String monthName = null;
		monthName = new DateFormatSymbols().getMonths()[monthNumber - 1];
		return monthName;
	}

	private Map<String, Object> getRecoAndTaxonId(RecoData recoData) throws Exception {
		Map<String, Object> result = new HashMap<String, Object>();
		try {
			String providedSciName = recoData.getTaxonScientificName();
			ParsedName parsedName = utilityService.getNameParsed(providedSciName);
			if (parsedName.getCanonicalName() == null)
				throw new ObservationInputException("Scientific Name Cannot start with Small letter");

			String canonicalName = parsedName.getCanonicalName().getSimple();
			ExtendedTaxonDefinition esResult = esService.matchPhrase("etd", "er", "name", providedSciName,
					"canonical_form", canonicalName);
			// System.out.println(esResult);

			if (esResult != null) {
				recoData.setScientificNameTaxonId((long) esResult.getId());
				//Recommendation recommendation = recoDao.findRecoByTaxonId(recoData.getScientificNameTaxonId(), true);

				result.put("taxonId", Long.valueOf(esResult.getId()));
				/*
				 * if (recommendation != null) { result.put("recoId", recommendation.getId()); }
				 */

			} /*
				 * else { List<Recommendation> resultList =
				 * recoDao.findByCanonicalName(canonicalName); if (!resultList.isEmpty()) {
				 * result.put("recoId", resultList.get(0).getId()); } }
				 */
			result.put("etd", esResult);
		} catch (Exception e) {
			logger.error(e.getMessage());
			throw e;
		}
		return result;
	}

	private List<Map<String, String>> getHierarchy(String[] taxonIds) {

		MapDocument taxonDoc;
		try {

			List<Map<String, String>> hierarchy = new ArrayList<>();

			for (String id : taxonIds) {
				taxonDoc = esService.fetch("extended_taxon_definition", "_doc", id);
				@SuppressWarnings("unchecked")
				Map<String, Object> taxonResponse = new ObjectMapper().readValue(taxonDoc.getDocument().toString(),
						HashMap.class);
				String taxonId = taxonResponse.get("id").toString();
				String normalisedName = taxonResponse.get("name").toString();
				String rank = taxonResponse.get("rank").toString();
				Map<String, String> node = new HashMap<>();
				node.put("taxon_id", taxonId);
				node.put("normalised_name", normalisedName);
				node.put("rank", rank);
				hierarchy.add(node);
			}
			return hierarchy;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	private Boolean isBadRecord(String lat, String lon) {

		if (lat.isEmpty() || lon.isEmpty()) {
			return true;
		} else {
			return false;
		}

	}

	private String getAnnotations(Map<String, Integer> headerIndex, List<String> markedColums, String[] columns,
			String[] row) {

		ObjectMapper mapper = new ObjectMapper();
		Map<String, Object> annotations = new HashMap<>();
		for (String column : columns) {
			if (!markedColums.contains(column)) {
				// System.out.println(row[0]);
				annotations.put(column, row[headerIndex.get(column)]);
			}
		}

		try {
			String annotationsString = mapper.writeValueAsString(annotations);
			return annotationsString;
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}
		return null;
	}

}