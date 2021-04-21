package com.strandls.observation.gbif;

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
import com.fasterxml.jackson.databind.ObjectMapper;
import com.opencsv.CSVReader;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.ExtendedTaxonDefinition;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.TaxonomyInfo;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.LocationInfo;
import com.strandls.observation.dao.RecommendationDao;
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

	public GbifObservationThread(UtilityServiceApi utilityService, EsServicesApi esService, RecommendationDao recoDao,
			GbifObservationESMapper gbifMapper, LayerServiceApi layerService) {
		super();
		this.utilityService = utilityService;
		this.esService = esService;
		this.recoDao = recoDao;
		this.gbifMapper = gbifMapper;
		this.layerService = layerService;
	}

	@Override
	public void run() {

		try {

			String path = PropertyFileUtil.fetchProperty("config.properties", "datasetPath");
			FileReader filereader = new FileReader(path);
			CSVReader csvReader = new CSVReader(filereader, '\t');

			Map<String, Integer> headerIndex = new LinkedHashMap<>();
			String batchEsJson = "";

			String[] row = csvReader.readNext();
			for (int j = 0; j < row.length; j++) {
				headerIndex.put(row[j], j);
			}

			List<ObservationESDocument> observations = new ArrayList<>();
			while ((row = csvReader.readNext()) != null) {
				String externalReferenceLink = row[headerIndex.get("occurrenceID")];
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

				double lat = Double.parseDouble(row[headerIndex.get("decimalLatitude")]);
				double lon = Double.parseDouble(row[headerIndex.get("decimalLongitude")]);

				String verbatimScientificName = row[headerIndex.get("verbatimScientificName")];

				RecoData recoData = new RecoData();
				recoData.setTaxonScientificName(verbatimScientificName);

				Map<String, Object> recoAndTaxonId = getRecoAndTaxonId(recoData);
				Long recoId = null;
				Long taxonId = null;
				if (recoAndTaxonId.get("recoId") != null) {
					recoId = Long.parseLong(recoAndTaxonId.get("recoId").toString());
				}

				if (recoAndTaxonId.get("taxonId") != null) {
					taxonId = Long.parseLong(recoAndTaxonId.get("taxonId").toString());
				}

				ExtendedTaxonDefinition taxonDetails = (ExtendedTaxonDefinition) recoAndTaxonId.get("etd");

				String scientificName = null;
				if (recoId != null) {
					Recommendation reco = recoDao.findById(recoId);
					scientificName = reco.getName();
				}

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

				LocationInfo locationInfo = layerService.fetchLocationInfo(String.valueOf(lat), String.valueOf(lon));
				String state = locationInfo.getState();
				String district = locationInfo.getDistrict();
				String tahsil = locationInfo.getTahsil();

				ObservationESDocument obj = gbifMapper.mapToESDocument(date, monthName, lat, lon, recoId, taxonId, rank,
						speciesId, taxonStatus, hierarchy, scientificName, cannonicalName, acceptedNameIds,
						italicisedForm, position, Long.parseLong(gbifId), lastModified, name, state, district, tahsil,
						groupId, groupName, externalReferenceLink);

				observations.add(obj);
				ObjectMapper objectMapper = new ObjectMapper();

				if (observations.size() >= 100) {
					List<Map<String, Object>> batchEsDoc = observations.stream().map(s -> {
						@SuppressWarnings("unchecked")
						Map<String, Object> doc = objectMapper.convertValue(s, Map.class);
						doc.put("id", "gbif-" + s.getObservation_id());
						return doc;
					}).collect(Collectors.toList());

					batchEsJson = objectMapper.writeValueAsString(batchEsDoc);

					ESPushThread esPushThread = new ESPushThread("extended_observation", "_doc", batchEsJson,
							esService);
					Thread t1 = new Thread(esPushThread);
					t1.start();
					observations.clear();
				}
			}

			ObjectMapper objectMapper = new ObjectMapper();
			if ((row = csvReader.readNext()) == null && !observations.isEmpty()) {
				List<Map<String, Object>> batchEsDoc = observations.stream().map(s -> {
					@SuppressWarnings("unchecked")
					Map<String, Object> doc = objectMapper.convertValue(s, Map.class);
					doc.put("id", "gbif-" + s.getObservation_id());
					return doc;
				}).collect(Collectors.toList());

				batchEsJson = objectMapper.writeValueAsString(batchEsDoc);
				ESPushThread esPushThread = new ESPushThread("extended_observation", "_doc", batchEsJson, esService);
				Thread t2 = new Thread(esPushThread);
				t2.start();
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
}
