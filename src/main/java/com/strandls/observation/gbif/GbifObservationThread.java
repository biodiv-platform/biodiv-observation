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
import com.strandls.esmodule.pojo.TaxonHierarchy;
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
				String externalReferenceLink=row[headerIndex.get("occurrenceID")];
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

				Map<String, Long> recoAndTaxonId = getRecoAndTaxonId(recoData);
				Long recoId = recoAndTaxonId.get("recoId");
				Long taxonId = recoAndTaxonId.get("taxonId");

				String scientificName = null;
				if (recoId != null) {
					Recommendation reco = recoDao.findById(recoId);
					scientificName = reco.getName();
				}

				TaxonomyInfo taxonInfo;
				Long idTaxon;
				Long rank = null;
				Long speciesId = null;
				String taxonStatus = null;

				Long acceptedNameId = null;
				String italicisedForm = null;
				String position = null;
				String cannonicalName = null;
				String name = null;
				Long groupId = null;
				String groupName = null;
				List<TaxonHierarchy> hierarchy = new ArrayList<>();

				if (taxonId != null) {
					taxonInfo = esService.getTaxonomyDetails(taxonId);
					idTaxon = taxonInfo.getId();
					rank = taxonInfo.getRank();
					if (taxonInfo.getSpeciesId() != null) {
						speciesId = taxonInfo.getSpeciesId();
					}
					taxonStatus = taxonInfo.getTaxonstatus();
					hierarchy = taxonInfo.getHierarchy();
					acceptedNameId = taxonInfo.getAcceptedNameId();
					italicisedForm = taxonInfo.getItalicisedForm();
					position = taxonInfo.getPosition();
					cannonicalName = taxonInfo.getCannonicalName();
					name = taxonInfo.getCannonicalName();
					groupId = taxonInfo.getGroupId();
					groupName = taxonInfo.getGroupName();

				}

				LocationInfo locationInfo = layerService.fetchLocationInfo(String.valueOf(lat), String.valueOf(lon));
				String state = locationInfo.getState();
				String district = locationInfo.getDistrict();
				String tahsil = locationInfo.getTahsil();
				

				ObservationESDocument obj = gbifMapper.mapToESDocument(date, monthName, lat, lon, recoId, taxonId, rank,
						speciesId, taxonStatus, hierarchy, scientificName, cannonicalName, acceptedNameId,
						italicisedForm, position, Long.parseLong(gbifId), lastModified, name, state, district, tahsil,
						groupId, groupName,externalReferenceLink);

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

					ESPushThread esPushThread = new ESPushThread("test", "_doc", batchEsJson, esService);
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
				ESPushThread esPushThread = new ESPushThread("test", "_doc", batchEsJson, esService);
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

	private Map<String, Long> getRecoAndTaxonId(RecoData recoData) throws Exception {
		Map<String, Long> result = new HashMap<String, Long>();
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
		} catch (Exception e) {
			logger.error(e.getMessage());
			throw e;
		}
		return result;
	}
}
