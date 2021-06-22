package com.strandls.observation.gbif;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Stopwatch;
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
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.pojo.BreadCrumb;
import com.strandls.utility.ApiException;
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
	private final TaxonomyServicesApi taxonomyService;
	// private final Map<String, Integer> headerIndex;
	private final int startRow;
	private final int endRow;
	private final int threadNum;

	Long timeNameParser = 0L;
	Long timeEsPush = 0L;
	Long timeExactMatch = 0L;
	Long timeRead = 0L;
	Long timeRecoDb = 0L;
	Long timehierarchy = 0L;
	Long timeMapper = 0L;
	Long timeSerialisation = 0L;
	// a++;

	public GbifObservationThread(UtilityServiceApi utilityService, EsServicesApi esService, RecommendationDao recoDao,
			GbifObservationESMapper gbifMapper, LayerServiceApi layerService, TaxonomyServicesApi taxonomyservice,
			int startRow, int endRow, int threadNum) {
		super();
		this.utilityService = utilityService;
		this.esService = esService;
		this.recoDao = recoDao;
		this.gbifMapper = gbifMapper;
		this.layerService = layerService;
		this.taxonomyService = taxonomyservice;
		this.startRow = startRow;
		this.endRow = endRow;
		this.threadNum = threadNum;
	}

	@Override
	public void run() {

		try {
			String filename = threadNum + ".txt";
			FileWriter fw = new FileWriter(filename);

			Long timeRecoAndtaxonId = 0L;
			Long timeLocationInfo = 0l;

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

				StopWatch watchRead = new StopWatch();
				watchRead.start();
				row = csvReader.readNext();
				watchRead.stop();
				timeRead = timeRead + watchRead.getTime();
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

					// ------------time to complete reco and taxon id------------------

					StopWatch watch = new StopWatch();
					watch.start();

					Map<String, Object> recoAndTaxonId = getRecoAndTaxonId(recoData);
					//Map<String, Object> recoAndTaxonId =new HashMap<>();
					watch.stop();

					Long time1 = watch.getTime();
					timeRecoAndtaxonId = timeRecoAndtaxonId + time1;
					// ---------------------------------------------------------------------

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
							List<String> taxonPath = Arrays.asList(taxonDetails.getPath().toString().split("_"));

							StopWatch watchth = new StopWatch();
							watchth.start();
							hierarchy = getHierarchy(taxonPath);
							watchth.stop();
							timehierarchy = timehierarchy + watchth.getTime();
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

					// ----------time to fetch location info--------------------------

					StopWatch watchLocation = new StopWatch();

					watchLocation.start();

					LocationInfo locationInfo = new LocationInfo();
					// ObservationLocationInfo layerInfo = new ObservationLocationInfo();
					String state = null;
					String district = null;
					String tahsil = null;

					locationInfo = layerService.fetchLocationInfo(String.valueOf(lat), String.valueOf(lon));

					watchLocation.stop();
					Long time2 = watchLocation.getTime();
					timeLocationInfo = timeLocationInfo + time2;

					// ------------------------------------------------------------------------------

					state = locationInfo.getState();
					district = locationInfo.getDistrict();
					tahsil = locationInfo.getTahsil();

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

					// ---------------------time for mapper class
					// -------------------------------------------------------------
					StopWatch watchMapper = new StopWatch();

					watchMapper.start();

					ExternalObservationESDocument obj = gbifMapper.mapToESDocument(date, monthName, lat, lon, placeName,
							recoId, taxonId, rank, speciesId, taxonStatus, hierarchy, scientificName, cannonicalName,
							acceptedNameIds, italicisedForm, position, Long.parseLong(gbifId), lastModified, name,
							state, district, tahsil, groupId, groupName, externalOriginalReferenceLink,
							externalGbifReferenceLink, null, annotations, dataSourcePrefix);
					watchMapper.stop();
					timeMapper = timeMapper + watchMapper.getTime();

					// ------------------------------------------------------------------------------------------------------
					
					observations.add(obj);obj.getAll_reco_vote();
					ObjectMapper objectMapper = new ObjectMapper();

					if (observations.size() >= 100) {
						/*
						 * List<Map<String, Object>> batchEsDoc = observations.stream().map(s -> {
						 * 
						 * @SuppressWarnings("unchecked") Map<String, Object> doc =
						 * objectMapper.convertValue(s, Map.class); doc.put("id", "gbif-" +
						 * s.getObservation_id()); return doc; }).collect(Collectors.toList());
						 */

						// -------------time for serialisation---------------------------------------
						StopWatch watchSerial = new StopWatch();
						watchSerial.start();
						batchEsJson = objectMapper.writeValueAsString(observations);
						watchSerial.stop();
						timeSerialisation = timeSerialisation + watchSerial.getTime();
						// --------------------time for pushing into es-----------------------------
						StopWatch watchInside = new StopWatch();
						watchInside.start();
						esService.bulkUpload("extended_observation", "_doc", batchEsJson);

						watchInside.stop();

						timeEsPush = timeEsPush + watchInside.getTime();
						// ------------------------------------------------------------------------

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
				/*
				 * List<Map<String, Object>> batchEsDoc = observations.stream().map(s -> {
				 * 
				 * @SuppressWarnings("unchecked") Map<String, Object> doc =
				 * objectMapper.convertValue(s, Map.class); doc.put("id", "gbif-" +
				 * s.getObservation_id()); return doc; }).collect(Collectors.toList());
				 */
				StopWatch watchSertialOutside = new StopWatch();
				watchSertialOutside.start();
				batchEsJson = objectMapper.writeValueAsString(observations);
				watchSertialOutside.stop();
				timeSerialisation = timeSerialisation + watchSertialOutside.getTime();

				StopWatch watchOutside = new StopWatch();
				watchOutside.start();

				esService.bulkUpload("extended_observation", "_doc", batchEsJson);

				watchOutside.stop();

				timeEsPush = timeEsPush + watchOutside.getTime();

				/*
				 * ESPushThread esPushThread = new ESPushThread("extended_observation", "_doc",
				 * batchEsJson, esService); Thread t2 = new Thread(esPushThread); t2.start();
				 */
			}

			fw.write("time to read csv=" + timeRead + "\n");
			fw.write("time for reco and taxonId=" + timeRecoAndtaxonId + "\n");
			fw.write("time for name pasrsing=" + timeNameParser + "\n");
			fw.write("time for reco db calls=" + timeRecoDb + "\n");
			fw.write("time for finding exact match in es=" + timeExactMatch + "\n");
			fw.write("time for fetching location info=" + timeLocationInfo + "\n");
			fw.write("time es push=" + timeEsPush + "\n");
			fw.write("time for hierarchy=" + timehierarchy + "\n");
			fw.write("time for mapping=" + timeMapper + "\n");
			fw.write("time for serialisation=" + timeSerialisation + "\n");

			fw.close();
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
			// ------------------------time for name
			// parsing-----------------------------------------
			StopWatch watchName = new StopWatch();
			watchName.start();
			ParsedName parsedName = utilityService.getNameParsed(providedSciName);
			watchName.stop();
			timeNameParser = timeNameParser + watchName.getTime();
			// ----------------------------------------------------------------------------------------
			if (parsedName.getCanonicalName() == null)
				throw new ObservationInputException("Scientific Name Cannot start with Small letter");

			String canonicalName = parsedName.getCanonicalName().getSimple();

			// ----------------------time for exact match search-----------------------
			StopWatch watchExactMatch = new StopWatch();
			watchExactMatch.start();
			ExtendedTaxonDefinition esResult = esService.matchPhrase("etd", "er", "name", providedSciName,
					"canonical_form", canonicalName);

			watchExactMatch.stop();

			timeExactMatch = timeExactMatch + watchExactMatch.getTime();

			// -----------------------------------------------------------------------------------------
			// System.out.println(esResult);

			StopWatch watchDb = new StopWatch();
			watchDb.start();

			if (esResult != null) {
				recoData.setScientificNameTaxonId((long) esResult.getId());
				Recommendation recommendation = recoDao.findRecoByTaxonId(recoData.getScientificNameTaxonId(), true);

				result.put("taxonId", Long.valueOf(esResult.getId()));

				if (recommendation != null) {
					result.put("recoId", recommendation.getId());
				}

			} else {
				List<Recommendation> resultList = recoDao.findByCanonicalName(canonicalName);
				if (!resultList.isEmpty()) {
					result.put("recoId", resultList.get(0).getId());
				}
			}

			watchDb.stop();
			timeRecoDb = timeRecoDb + watchDb.getTime();

			result.put("etd", esResult);
		} catch (Exception e) {
			logger.error(e.getMessage());
			throw e;
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private List<Map<String, String>> getHierarchy(List<String> taxonIds) {
		// ObjectMapper obj=new ObjectMapper();
		List<Map<String, String>> hierarchy = new ArrayList<>();
		// MapDocument taxonDoc;
		try {
			List<MapDocument> taxonDocs = esService.bulkFetch("extended_taxon_definition", "_doc", taxonIds);
			for (MapDocument mapDoc : taxonDocs) {
				Map<String, Object> a = (Map<String, Object>) mapDoc.getDocument();
				String taxonId = a.get("id").toString();
				String normaliseName = a.get("name").toString();
				String rank = a.get("rank").toString();

				Map<String, String> node = new HashMap<>();
				node.put("taxon_id", taxonId);
				node.put("normalised_name", normaliseName);
				node.put("rank", rank);
				hierarchy.add(node);
			}

			return hierarchy;

			/*
			 * List<Map<String, String>> hierarchy = new ArrayList<>();
			 * 
			 * List<BreadCrumb> breadcrumb =
			 * taxonomyService.getTaxonomyBreadCrumb(taxonIds.toString());
			 * 
			 * for (BreadCrumb b : breadcrumb) { Map<String, String> node = new HashMap<>();
			 * node.put("taxon_id", b.getId().toString()); node.put("normalised_name",
			 * b.getName()); node.put("rank", b.getRank().toString()); hierarchy.add(node);
			 * 
			 * }
			 * 
			 * return hierarchy;
			 */

			/*
			 * for (String id : taxonIds) { taxonDoc =
			 * esService.fetch("extended_taxon_definition", "_doc", id);
			 * 
			 * @SuppressWarnings("unchecked") Map<String, Object> taxonResponse = new
			 * ObjectMapper().readValue(taxonDoc.getDocument().toString(), HashMap.class);
			 * String taxonId = taxonResponse.get("id").toString(); String normalisedName =
			 * taxonResponse.get("name").toString(); String rank =
			 * taxonResponse.get("rank").toString(); Map<String, String> node = new
			 * HashMap<>(); node.put("taxon_id", taxonId); node.put("normalised_name",
			 * normalisedName); node.put("rank", rank); hierarchy.add(node); } return
			 * hierarchy;
			 */

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
	
	private List<ParsedName>getParsedNames(List<String>scientificNames) {
		try {
			List<ParsedName> ans=utilityService.getNamesParsed(scientificNames);
			return ans;
		} catch (ApiException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

}
