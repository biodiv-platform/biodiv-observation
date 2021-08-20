package com.strandls.observation.gbif;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
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
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Stopwatch;
import com.opencsv.CSVParser;
import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.opencsv.CSVWriter;
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
	private final BulkNameParser bulkNameParser;
	private final String filenameToPick;
	// private final Map<String, Integer> headerIndex;
	private final int startRow;
	private final int endRow;
	private final int threadNum;
	private final Map<Long, Recommendation> allScientificNames;
	private final Map<String, Recommendation> cache;
	private final Map<String, Recommendation> findByCannocalNames;

	Long timeNameParser = 0L;
	Long timeEsPush = 0L;
	Long timeExactMatch = 0L;
	Long timeRead = 0L;
	Long timeRecoDb = 0L;
	Long timehierarchy = 0L;
	Long timeMapper = 0L;
	Long timeSerialisation = 0L;
	Long timeRecoAndtaxonId = 0L;
	// a++;

	public GbifObservationThread(UtilityServiceApi utilityService, EsServicesApi esService, RecommendationDao recoDao,
			GbifObservationESMapper gbifMapper, LayerServiceApi layerService, TaxonomyServicesApi taxonomyservice,
			int startRow, int endRow, int threadNum, BulkNameParser bulkNameParser,
			Map<Long, Recommendation> allScientificNames, Map<String, Recommendation> cache,
			Map<String, Recommendation> findByCannocalNames, String filenameToPick) {
		super();
		this.utilityService = utilityService;
		this.esService = esService;
		this.recoDao = recoDao;
		this.gbifMapper = gbifMapper;
		this.layerService = layerService;
		this.taxonomyService = taxonomyservice;
		this.bulkNameParser = bulkNameParser;
		this.filenameToPick = filenameToPick;
		this.startRow = startRow;
		this.endRow = endRow;
		this.threadNum = threadNum;
		this.allScientificNames = allScientificNames;
		this.cache = cache;
		this.findByCannocalNames = findByCannocalNames;
	}

	@Override
	public void run() {

		try {
			

			// Long timeRecoAndtaxonId = 0L;
			Long timeLocationInfo = 0l;

			// String path = PropertyFileUtil.fetchProperty("config.properties",
			// "datasetPath");
			String path = filenameToPick;

			String dataSourcePrefix = PropertyFileUtil.fetchProperty("config.properties", "gbifUniqueIdPrefix");

			String excludingPublishingOrgKey = PropertyFileUtil.fetchProperty("config.properties",
					"excludingPublishingOrgKey");
			FileReader filereader1 = new FileReader(path);
			CSVReader csvReader1 = new CSVReader(filereader1, '\t');

			String r[] = csvReader1.readNext();
			String[] columns = r.clone();

			System.out.println(columns);

			List<String> markedColumns = new ArrayList<>();
			Map<String, Integer> headerIndex = new LinkedHashMap<>();
			for (int j = 0; j < r.length; j++) {
				headerIndex.put(r[j], j);
			}
			FileReader filereader2 = new FileReader(path);
			CSVReader csvReader = new CSVReader(filereader2, '\t', '"', startRow);

			String batchEsJson = "";

			/*
			 * FileWriter outputfile = new
			 * FileWriter("/home/prakhar/scientific_names/new_query_results.csv", true);
			 * CSVWriter writer = new CSVWriter(outputfile);
			 */

			/*
			 * String[] outputFileHeaders = { "gbifScientificName", "parsedCanonicalName",
			 * "matchedScientificName", "matchedCanonicalName", "taxonId", "recoId" };
			 * 
			 * writer.writeNext(outputFileHeaders);
			 */

			List<String> scientificNames = new ArrayList<>();
			List<ExternalObservationESDocument> observations = new ArrayList<>();
			// int ctr = startRow;
			String[] row;
			for (int i = startRow; i <= endRow; i++) {

				//StopWatch watchRead = new StopWatch();
			//	watchRead.start();
				row = csvReader.readNext();
				//watchRead.stop();
				//timeRead = timeRead + watchRead.getTime();

				/*
				 * if (row == null) { System.out.println("row empty i=" + i); }
				 * 
				 * System.out.println(row[0]);
				 */
				String publishingOrgKey = row[headerIndex.get("publishingOrgKey")];
				String latitude = row[headerIndex.get("decimalLatitude")];
				String longitude = row[headerIndex.get("decimalLongitude")];
				String tempId = row[headerIndex.get("gbifID")];

				if (publishingOrgKey != null && !publishingOrgKey.equals(excludingPublishingOrgKey)
						&& !isBadRecord(latitude, longitude, row[headerIndex.get("month")],
								row[headerIndex.get("scientificName")])) {
					String externalOriginalReferenceLink = row[headerIndex.get("occurrenceID")];
					String gbifId = row[headerIndex.get("gbifID")];

					String datetime = null;
					LocalDateTime dateTime1 = null;
					Date date = null;

					datetime = row[headerIndex.get("eventDate")];
					// System.out.println("datetime is =" + datetime);

					if (datetime != null && !datetime.isEmpty()) {
						dateTime1 = LocalDateTime.parse(datetime);
						date = Timestamp.valueOf(dateTime1);

					}

					Date lastModified = null;

					String lastModifiedDateString = row[headerIndex.get("dateIdentified")];
					if (!lastModifiedDateString.isEmpty()) {
						LocalDateTime dateTime2 = LocalDateTime.parse(lastModifiedDateString);
						lastModified = Timestamp.valueOf(dateTime2);
					} else {
						lastModified = null;
					}

					String gbifScientificName = row[headerIndex.get("scientificName")];
					scientificNames.add(gbifScientificName);

					String month = null;
					String monthName = null;

					month = row[headerIndex.get("month")];
					// System.out.println("month=" + month + ",gbifId=" + gbifId);
					monthName = getMonthName(month);

					Double lat = null;
					Double lon = null;

					lat = Double.parseDouble(row[headerIndex.get("decimalLatitude")]);
					lon = Double.parseDouble(row[headerIndex.get("decimalLongitude")]);

					String placeName = row[headerIndex.get("locality")];

					if (placeName.equals("")) {
						placeName = null;
					}

					// ----------time to fetch location info--------------------------

					//StopWatch watchLocation = new StopWatch();

					//watchLocation.start();

					LocationInfo locationInfo = new LocationInfo();
					// ObservationLocationInfo layerInfo = new ObservationLocationInfo();
					String state = null;
					String district = null;
					String tahsil = null;

					locationInfo = layerService.fetchLocationInfo(String.valueOf(lat), String.valueOf(lon));

					//watchLocation.stop();
					//Long time2 = watchLocation.getTime();
					//timeLocationInfo = timeLocationInfo + time2;

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
					// System.out.println(annotations);

					// ---------------------time for mapper class
					// -------------------------------------------------------------
					//StopWatch watchMapper = new StopWatch();

					//watchMapper.start();

					ExternalObservationESDocument obj = gbifMapper.mapToESDocument(date, monthName, lat, lon, placeName,
							null, null, null, null, null, null, null, null, null, null, null, Long.parseLong(gbifId),
							lastModified, null, state, district, tahsil, null, null, externalOriginalReferenceLink,
							externalGbifReferenceLink, null, annotations, dataSourcePrefix);
					//watchMapper.stop();
					//timeMapper = timeMapper + watchMapper.getTime();

					// ------------------------------------------------------------------------------------------------------

					observations.add(obj);
					obj.getAll_reco_vote();
					ObjectMapper objectMapper = new ObjectMapper();

					if (observations.size() >= 200) {
						List<ParsedName> parsedNames = getParsedNames(scientificNames);
						processRecoAndTaxonDetails(parsedNames, observations);

						// -------------time for serialisation---------------------------------------
						//StopWatch watchSerial = new StopWatch();
						//watchSerial.start();
						batchEsJson = objectMapper.writeValueAsString(observations);
						//watchSerial.stop();
						//timeSerialisation = timeSerialisation + watchSerial.getTime();
						// --------------------time for pushing into es-----------------------------
						//StopWatch watchInside = new StopWatch();
						//watchInside.start();
						esService.bulkUpload("extended_observation", "_doc", batchEsJson);

						//watchInside.stop();

						//timeEsPush = timeEsPush + watchInside.getTime();
						// ------------------------------------------------------------------------

						observations.clear();
						scientificNames.clear();

					}

				}

			}

			ObjectMapper objectMapper = new ObjectMapper();
			if (!observations.isEmpty()) {
				List<ParsedName> parsedNames = getParsedNames(scientificNames);
				processRecoAndTaxonDetails(parsedNames, observations);

				//StopWatch watchSertialOutside = new StopWatch();
				//watchSertialOutside.start();
				batchEsJson = objectMapper.writeValueAsString(observations);
				//watchSertialOutside.stop();
				//timeSerialisation = timeSerialisation + watchSertialOutside.getTime();

				//StopWatch watchOutside = new StopWatch();
				//watchOutside.start();

				esService.bulkUpload("extended_observation", "_doc", batchEsJson);

				//watchOutside.stop();

				//timeEsPush = timeEsPush + watchOutside.getTime();

			}

			csvReader.close();
			csvReader1.close();
			filereader1.close();
			filereader2.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void processRecoAndTaxonDetails(List<ParsedName> parsedNames,
			List<ExternalObservationESDocument> observations) {

		try {

			for (int i = 0; i < observations.size(); i++) {
				Long recoId = null;
				Long taxonId = null;
				String matchedScientificName = null;
				String scientificName = null;
				ExtendedTaxonDefinition taxonDetails = null;

				String gbifScientificName = parsedNames.get(i).getVerbatim();
				RecoData recoData = new RecoData();
				recoData.setTaxonScientificName(gbifScientificName);

				//StopWatch watchRecoAndTaxon = new StopWatch();
				//watchRecoAndTaxon.start();
				if (parsedNames.get(i).getCanonicalName() == null) {
					String t = parsedNames.get(i).getVerbatim();
					System.out.println("scientific name error=" + t);
				}

				if (parsedNames.get(i).getCanonicalName() != null
						&& !getRecoAndTaxonId(recoData, parsedNames.get(i).getCanonicalName().getSimple()).isEmpty()) {

					Map<String, Object> recoAndTaxonId = getRecoAndTaxonId(recoData,
							parsedNames.get(i).getCanonicalName().getSimple());

					//watchRecoAndTaxon.stop();
					//timeRecoAndtaxonId = timeRecoAndtaxonId + watchRecoAndTaxon.getTime();

					if (recoAndTaxonId.get("recoId") != null) {
						recoId = Long.parseLong(recoAndTaxonId.get("recoId").toString());
					}

					scientificName = gbifScientificName;
					if (recoAndTaxonId.get("taxonId") != null) {
						taxonId = Long.parseLong(recoAndTaxonId.get("taxonId").toString());

					}

					taxonDetails = (ExtendedTaxonDefinition) recoAndTaxonId.get("etd");
					if (taxonId != null) {
						scientificName = taxonDetails.getName();
						matchedScientificName = taxonDetails.getName();
					}

				}

				String rank = null;
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

					rank = taxonDetails.getRank();
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

						//StopWatch watchth = new StopWatch();
						//watchth.start();

						hierarchy = taxonDetails.getHierarchy();

						//watchth.stop();
						//timehierarchy = timehierarchy + watchth.getTime();
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

				//StopWatch watchMapperOutside = new StopWatch();
				//watchMapperOutside.start();
				ExternalObservationESDocument tempObj = gbifMapper.mapToESDocument(null, null, null, null, null, recoId,
						taxonId, rank, speciesId, taxonStatus, hierarchy, scientificName, cannonicalName,
						acceptedNameIds, italicisedForm, position, null, null, name, null, null, null, groupId,
						groupName, null, null, null, null, null);
				//watchMapperOutside.stop();
				//timeMapper = timeMapper + watchMapperOutside.getTime();

				observations.get(i).setAll_reco_vote(tempObj.getAll_reco_vote());
				observations.get(i).setMax_voted_reco(tempObj.getMax_voted_reco());
				observations.get(i).setGroup_id(tempObj.getGroup_id());
				observations.get(i).setGroup_name(tempObj.getGroup_name());

				String tid = null;
				String rid = null;

				if (taxonId != null) {
					tid = taxonId.toString();
				}
				if (recoId != null) {
					rid = recoId.toString();
				}

			}

		} catch (Exception e) {

			// e.printStackTrace();
			logger.error(e.getMessage());
		}

	}

	private String getMonthName(String month) {
		// System.out.println("month = " + month);

		int monthNumber = Integer.parseInt(month);
		String monthName = null;
		monthName = new DateFormatSymbols().getMonths()[monthNumber - 1];
		return monthName;
	}

	private Map<String, Object> getRecoAndTaxonId(RecoData recoData, String parsedCannonicalName) throws Exception {
		Map<String, Object> result = new HashMap<String, Object>();
		try {
			String providedSciName = recoData.getTaxonScientificName();
			// ------------------------time for name
			// parsing-----------------------------------------
			// StopWatch watchName = new StopWatch();
			// watchName.start();
			// ParsedName parsedName = utilityService.getNameParsed(providedSciName);
			// watchName.stop();
			// timeNameParser = timeNameParser + watchName.getTime();
			// ----------------------------------------------------------------------------------------
			// if (parsedName.getCanonicalName() == null)
			// throw new ObservationInputException("Scientific Name Cannot start with Small
			// letter");

			// String canonicalName = parsedName.getCanonicalName().getSimple();
			String canonicalName = parsedCannonicalName;
			// ----------------------time for exact match search-----------------------
			//StopWatch watchExactMatch = new StopWatch();
			//watchExactMatch.start();
			ExtendedTaxonDefinition esResult = esService.matchPhrase("etd", "er", "name", providedSciName,
					"canonical_form", canonicalName);

			//watchExactMatch.stop();

			//timeExactMatch = timeExactMatch + watchExactMatch.getTime();

			StopWatch watchDb = new StopWatch();
			watchDb.start();

			if (esResult != null) {
				recoData.setScientificNameTaxonId((long) esResult.getId());

				Recommendation recommendation = allScientificNames.get(recoData.getScientificNameTaxonId());
				result.put("taxonId", Long.valueOf(esResult.getId()));

				if (recommendation != null) {
					result.put("recoId", recommendation.getId());
				}
				result.put("etd", esResult);
			} else {

				if (findByCannocalNames.get(canonicalName) != null) {
					Recommendation r = findByCannocalNames.get(canonicalName);
					result.put("recoId", r.getId());
				}

			}

			watchDb.stop();
			timeRecoDb = timeRecoDb + watchDb.getTime();

		} catch (Exception e) {
			//e.printStackTrace();
			logger.error(e.getMessage());

		}
		return result;
	}


	private Boolean isBadRecord(String lat, String lon, String month, String scientificname) {

		char firstChar = scientificname.charAt(0);

		if (lat.isEmpty() || lon.isEmpty() || month.isEmpty() || scientificname == null || scientificname.isEmpty()
				|| Character.isLowerCase(firstChar)) {
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

	private List<ParsedName> getParsedNames(List<String> scientificNames) {
		try {
			//StopWatch watchName = new StopWatch();
			//watchName.start();
			// List<ParsedName> ans = utilityService.getNamesParsed(scientificNames);
			List<ParsedName> ans = bulkNameParser.findParsedNames(scientificNames);
		//	watchName.stop();
		//	timeNameParser = timeNameParser + watchName.getTime();
			return ans;
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

}
