/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.io.InputStream;
import java.math.RoundingMode;
import java.security.SecureRandom;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.ExtendedTaxonDefinition;
import com.strandls.file.api.UploadApi;
import com.strandls.file.model.FilesDTO;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.es.util.RabbitMQProducer;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoData;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.pojo.ResourceDataObs;
import com.strandls.observation.service.RecommendationService;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.observation.util.PropertyFileUtil;
import com.strandls.resource.pojo.Resource;
import com.strandls.resource.pojo.ResourceData;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.userGroup.pojo.UserGroupObvFilterData;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.ParsedName;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationMapperHelper {

	private final Logger logger = LoggerFactory.getLogger(ObservationMapperHelper.class);

	@Inject
	private RecommendationDao recoDao;

	@Inject
	private RecommendationService recoSerivce;

	@Inject
	private UtilityServiceApi utilitySerivce;

	@Inject
	private EsServicesApi esService;

	@Inject
	private UploadApi fileUploadService;

	@Inject
	private Headers headers;

	@Inject
	private SecureRandom random;

	private Long defaultLanguageId = Long
			.parseLong(PropertyFileUtil.fetchProperty("config.properties", "defaultLanguageId"));

	private Long defaultLicenseId = Long
			.parseLong(PropertyFileUtil.fetchProperty("config.properties", "defaultLicenseId"));

	@Inject
	private RabbitMQProducer rabbitMQProducer;

	@Inject
	private ObservationDAO observationDAO;

	@Inject
	private TraitsServiceApi traitsServiceApi;

	public Boolean checkObservationBounds(Double lat, Double lon) {

		if (lat == null || lon == null) {
			return false;
		}

		try {
			String topleft = "";
			String bottomright = "";
			InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

			Properties properties = new Properties();
			try {
				properties.load(in);
			} catch (IOException e) {
				logger.error(e.getMessage());
			}

			topleft = properties.getProperty("topLeft");
			bottomright = properties.getProperty("bottomRight");
			in.close();

			if (topleft.equalsIgnoreCase("NA") || bottomright.equalsIgnoreCase("NA"))
				return true;

			String point1[] = topleft.split(",");
			String point2[] = bottomright.split(",");

			Coordinate[] cords = new Coordinate[] {
					new Coordinate(Double.parseDouble(point1[0]), Double.parseDouble(point1[1])),
					new Coordinate(Double.parseDouble(point2[0]), Double.parseDouble(point1[1])),
					new Coordinate(Double.parseDouble(point2[0]), Double.parseDouble(point2[1])),
					new Coordinate(Double.parseDouble(point1[0]), Double.parseDouble(point2[1])),
					new Coordinate(Double.parseDouble(point1[0]), Double.parseDouble(point1[1])), };

			GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);

			Polygon indiaBounds = geofactory.createPolygon(geofactory.createLinearRing(cords));

			DecimalFormat df = new DecimalFormat("#.####");
			df.setRoundingMode(RoundingMode.HALF_EVEN);
			double latitude = Double.parseDouble(df.format(lat));
			double longitude = Double.parseDouble(df.format(lon));
			Coordinate c = new Coordinate(longitude, latitude);
			Geometry topology = geofactory.createPoint(c);
			Boolean withinIndia = indiaBounds.intersects(topology);
			if (withinIndia == false) {
				return false;
			}
			return true;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	public Observation createObservationMapping(Long userId, ObservationCreate observationData) {
		try {

			Observation observation = new Observation();
			observation.setAuthorId(userId);
			observation.setCreatedOn(new Date());
			observation.setGroupId(observationData.getsGroup());
			observation.setLatitude(observationData.getLatitude());
			observation.setLongitude(observationData.getLongitude());
			observation.setNotes(observationData.getNotes());
			observation.setFromDate(observationData.getFromDate());
			observation.setPlaceName(observationData.getObservedAt()); // place name given by user
			observation.setRating(0);// what to insert
			observation.setReverseGeocodedName(observationData.getReverseGeocoded()); // google reversed name for the
																						// lat
																						// and long
			observation.setFlagCount(0);// during creation it should be 0
			observation.setGeoPrivacy(observationData.getHidePreciseLocation());
			observation.setIsDeleted(false);
			observation.setLastRevised(new Date());// initially same as date of creation of object
													// later
													// when updated
			observation.setLocationAccuracy(null); // what to insert
			observation.setVisitCount(0L); // updateble field

			observation.setAgreeTerms(true);
			observation.setIsShowable(true);
			observation.setToDate(observationData.getToDate());

			GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);
			DecimalFormat df = new DecimalFormat("#.####");
			df.setRoundingMode(RoundingMode.HALF_EVEN);
			double latitude = Double.parseDouble(df.format(observationData.getLatitude()));
			double longitude = Double.parseDouble(df.format(observationData.getLongitude()));
			Coordinate c = new Coordinate(longitude, latitude);
			Geometry topology = geofactory.createPoint(c);

			observation.setTopology(topology);

			observation.setFeatureCount(0);// update field initially 0, used only after its attached and featured to a
											// group
			observation.setIsLocked(false);// update field , initially false
			observation.setLicenseId(defaultLicenseId);// default 822
			if (observationData.getObsvLanguageId() != null)
				observation.setLanguageId(observationData.getObsvLanguageId());
			else
				observation.setLanguageId(defaultLanguageId);
			observation.setLocationScale(observationData.getLocationScale()); // 5 options

			observation.setReprImageId(null);
			observation.setProtocol(observationData.getProtocol());
			observation.setBasisOfRecord(observationData.getBasisOfRecords());
			observation.setNoOfImages(0);
			observation.setNoOfAudio(0);
			observation.setNoOfVideos(0);

			if (observationData.getHelpIdentify() == true)
				observation.setNoOfIdentifications(0);// initailly 0-1 but can increase with the no of reco vote
			else
				observation.setNoOfIdentifications(1);

			observation.setDataTableId(null);//
			observation.setDateAccuracy(observationData.getDateAccuracy());

			observation.setIsChecklist(false);// false for nrml case only used in DATATABLE
			observation.setSourceId(null);// observation id in nrml case, used only in GBIF
			observation.setChecklistAnnotations(null);// from data set
			observation.setDatasetId(null);// null for nrml case only used in GBIF

			return observation;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	public RecoCreate createRecoMapping(RecoData recoData) throws Exception {

		Long commonNameId = null;
		Long taxonId = null;
		String commonName = recoData.getTaxonCommonName();
		Long scientificNameId = null;
		String scientificName = recoData.getTaxonScientificName();
		Map<String, Long> scientificResult = new HashMap<String, Long>();

		if (recoData.getScientificNameTaxonId() != null && scientificName != null) {
			scientificResult = scientificNameExists(recoData);
		} else if (recoData.getScientificNameTaxonId() == null && scientificName != null) {
			scientificResult = scientificNameNotExists(recoData);
		}

		if (commonName != null) {
			commonNameId = commonNameMapper(commonName, recoData.getLanguageId());
		}

		if (scientificResult != null) {
			scientificNameId = scientificResult.get("recoId");
			taxonId = scientificResult.get("taxonId");
		}

		RecoCreate recoCreate = new RecoCreate();
		recoCreate.setCommonName(commonName);
		recoCreate.setCommonNameId(commonNameId);
		recoCreate.setScientificName(scientificName);
		recoCreate.setScientificNameId(scientificNameId);
		recoCreate.setTaxonId(taxonId);
		if (scientificResult != null && scientificResult.isEmpty() == false && scientificResult.get("flag") == 1L)
			recoCreate.setFlag(true);
		else
			recoCreate.setFlag(false);

		return recoCreate;

	}

//	Scientific name has a taxonId
	private Map<String, Long> scientificNameExists(RecoData recoData) {
		Map<String, Long> result = new HashMap<String, Long>();
		try {
			Recommendation recommendation = recoDao.findRecoByTaxonId(recoData.getScientificNameTaxonId(), true);
			if (recommendation == null) {
				ParsedName parsedName = utilitySerivce.getNameParsed(recoData.getTaxonScientificName());
				String canonicalName = parsedName.getCanonicalName().getSimple();
				recommendation = recoSerivce.createRecommendation(recoData.getTaxonScientificName(),
						recoData.getScientificNameTaxonId(), canonicalName, true, recoData.getLanguageId());
			}
			Long taxonId = recommendation.getAcceptedNameId() != null ? recommendation.getAcceptedNameId()
					: recommendation.getTaxonConceptId();
			result.put("taxonId", taxonId);
			result.put("recoId", recommendation.getId());
			result.put("flag", 0L);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return result;
	}

//	COMMON NAME LOGIC IMPLEMENTED
	private Long commonNameMapper(String commonName, Long languageId) {

		Recommendation resultCommonName = recoDao.findByCommonName(commonName, languageId);
		if (resultCommonName == null)
			resultCommonName = recoSerivce.createRecommendation(commonName, null, null, false, languageId);

		return resultCommonName.getId();

	}

//	scientific Name DON'T have a taxonId
	private Map<String, Long> scientificNameNotExists(RecoData recoData) throws Exception {
		Map<String, Long> result = new HashMap<String, Long>();
		try {
			String providedSciName = recoData.getTaxonScientificName();
			ParsedName parsedName = utilitySerivce.getNameParsed(providedSciName);

			if (parsedName.getCanonicalName() == null)
				throw new ObservationInputException("Scientific Name Cannot start with Small letter");

			String canonicalName = parsedName.getCanonicalName().getSimple();
			ExtendedTaxonDefinition esResult = esService.matchPhrase("etd", "er", "name", providedSciName,
					"canonical_form", canonicalName);
			if (esResult != null) {
				recoData.setScientificNameTaxonId((long) esResult.getId());
				result = scientificNameExists(recoData);
			} else {

				List<Recommendation> resultList = recoDao.findByCanonicalName(canonicalName);
				if (resultList.isEmpty() || resultList.size() == 1) {
					if (resultList.isEmpty())
						resultList.add(recoSerivce.createRecommendation(providedSciName, null, canonicalName, true,
								recoData.getLanguageId()));
					Long taxonId = resultList.get(0).getAcceptedNameId() != null ? resultList.get(0).getAcceptedNameId()
							: resultList.get(0).getTaxonConceptId();
					result.put("taxonId", taxonId);
					result.put("recoId", resultList.get(0).getId());
					result.put("flag", 0L);
				} else {
					result = taxonIdEqualsAccpetedNameId(resultList, providedSciName);
				}
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
			throw e;
		}

		return result;
	}

//	PRIORITY 1 : taxonId == accpetedNameId 
	private Map<String, Long> taxonIdEqualsAccpetedNameId(List<Recommendation> recommendations,
			String providedSciName) {

		Map<String, Long> result = new HashMap<String, Long>();
		List<Recommendation> filteredList = new ArrayList<Recommendation>();
		for (Recommendation recommendation : recommendations) {
			if (recommendation.getTaxonConceptId().equals(recommendation.getAcceptedNameId()))
				filteredList.add(recommendation);
		}
		if (filteredList.isEmpty())
			return taxonIdExists(recommendations, providedSciName);
		else if (filteredList.size() == 1) {
			long taxonId = filteredList.get(0).getAcceptedNameId() != null ? filteredList.get(0).getAcceptedNameId()
					: filteredList.get(0).getTaxonConceptId();
			result.put("taxonId", taxonId);
			result.put("recoId", filteredList.get(0).getId());
			result.put("flag", 0L);
			return result;
		} else
			return fullNameSearch(filteredList, providedSciName);
	}

//	PRIORITY 2 :CHECKS IF TAXON ID EXISTS IF YES PICK IT ELSE SERACH FULL NAME
	private Map<String, Long> taxonIdExists(List<Recommendation> recommendations, String providedSciName) {
		Map<String, Long> result = new HashMap<String, Long>();
		List<Recommendation> filteredList = new ArrayList<Recommendation>();
		for (Recommendation recommendation : recommendations) {
			if (recommendation.getTaxonConceptId() != null)
				filteredList.add(recommendation);
		}
		if (filteredList.isEmpty())
			return fullNameSearch(recommendations, providedSciName);
		else if (filteredList.size() == 1) {
			long taxonId = filteredList.get(0).getAcceptedNameId() != null ? filteredList.get(0).getAcceptedNameId()
					: filteredList.get(0).getTaxonConceptId();
			result.put("taxonId", taxonId);
			result.put("recoId", filteredList.get(0).getId());
			result.put("flag", 0L);
			return result;
		} else
			return fullNameSearch(filteredList, providedSciName);
	}

//	PRIORITY 3
//	DOES A FULL NAME SEARCH IF MATCHED SENT WITH FLAG 0 , IF NOT SEND 1st ID AND FLAG 1
	private Map<String, Long> fullNameSearch(List<Recommendation> recommendations, String providedSciName) {

		Map<String, Long> result = new HashMap<String, Long>();
		long taxonId = recommendations.get(0).getAcceptedNameId() != null ? recommendations.get(0).getAcceptedNameId()
				: recommendations.get(0).getTaxonConceptId();
		for (Recommendation recommendation : recommendations) {
			if (recommendation.getName().equals(providedSciName)) {
				taxonId = recommendation.getAcceptedNameId() != null ? recommendation.getAcceptedNameId()
						: recommendation.getTaxonConceptId();
				result.put("recoId", recommendation.getId());
				result.put("flag", 0L);
				return result;
			}

		}
		result.put("taxonId", taxonId);
		result.put("recoId", recommendations.get(0).getId());
		result.put("flag", 1L);
		return result;
	}

	@SuppressWarnings("unchecked")
	public List<Resource> createResourceMapping(HttpServletRequest request, Long userId,
			List<ResourceDataObs> resourceDataList) {
		List<Resource> resources = new ArrayList<Resource>();
		try {
			List<String> fileList = new ArrayList<String>();
			for (ResourceDataObs rd : resourceDataList) {
				if (rd.getPath() != null && rd.getPath().trim().length() > 0) {
					fileList.add(rd.getPath());
				}

			}
			Map<String, Object> fileMap = new HashMap<String, Object>();
			if (!fileList.isEmpty()) {
				fileUploadService = headers.addFileUploadHeader(fileUploadService,
						request.getHeader(HttpHeaders.AUTHORIZATION));

				FilesDTO filesDTO = new FilesDTO();
				filesDTO.setFiles(fileList);
				filesDTO.setFolder("observations");
				filesDTO.setModule("observation");
				fileMap = fileUploadService.moveFiles(filesDTO);
			}

			for (ResourceDataObs resourceData : resourceDataList) {
				Resource resource = new Resource();
				if (resourceData.getCaption() != null)
					resource.setDescription(
							(resourceData.getCaption().trim().length() != 0) ? resourceData.getCaption().trim() : null);

				if (resourceData.getPath() != null) {
					if (fileMap != null && !fileMap.isEmpty() && fileMap.containsKey(resourceData.getPath())) {
						// new path getting extracted from the map
						System.out.println(fileMap);
						Map<String, String> files = (Map<String, String>) fileMap.get(resourceData.getPath());
						System.out.println(files);
						String relativePath = files.get("name").toString();
						resource.setFileName(relativePath);

					} else if (resourceData.getPath().startsWith("/ibpmu")) {
						continue;
					} else {
						resource.setFileName(resourceData.getPath()); // skip the resource as no new path has been
					} // returned
				}
				resource.setMimeType(null);
				if (resourceData.getType().startsWith("image") || resourceData.getType().equalsIgnoreCase("image"))
					resource.setType("IMAGE");
				else if (resourceData.getType().startsWith("audio") || resourceData.getType().equalsIgnoreCase("audio"))
					resource.setType("AUDIO");
				else if (resourceData.getType().startsWith("video") || resourceData.getType().equalsIgnoreCase("video"))
					resource.setType("VIDEO");
				if (resourceData.getPath() == null) {
					resource.setFileName(resource.getType().substring(0, 1).toLowerCase());
				}
				resource.setUrl(resourceData.getUrl());
				resource.setRating(resourceData.getRating());
				resource.setUploadTime(new Date());
				resource.setUploaderId(userId);
				resource.setContext("OBSERVATION");
				if (resourceData.getLanguageId() != null)
					resource.setLanguageId(resourceData.getLanguageId());
				else
					resource.setLanguageId(defaultLanguageId);
				resource.setLicenseId(resourceData.getLicenseId());
				resource.setContributor(resourceData.getContributor());

				resources.add(resource);
			}
			return resources;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

//	GETS A RANDOM LAT,LON WITH LOWER LIMIT AS 5KM AND UPPER LIMIT AS 25KM
	public Map<String, Double> getRandomLatLong(Double lat, Double lon) {

		Map<String, Double> latlon = new HashMap<String, Double>();
		double x0 = lon;
		double y0 = lat;

		// Convert radius from meters to degrees.
		double innerRadiusInDegrees = 5000D / 111320f;
		Double radius = 20000D; // taking radius as 20km to make upper limit as 20km + 5Km(innerLimit)
		double radiusInDegrees = radius / 111320f;

		// Get a random distance and a random angle.
		double u = random.nextDouble();
		double v = random.nextDouble();
		double w = radiusInDegrees * Math.sqrt(u);// random distance from center to radius
		double new_w = (w + innerRadiusInDegrees);// adding 5Km as innerLimit to make it between 5km and 25km
		double t = 2 * Math.PI * v;
		// Get the x and y delta values.
		double x = new_w * Math.cos(t);
		double y = new_w * Math.sin(t);

		// Compensate the x value.
		double new_x = x / Math.cos(Math.toRadians(y0));

		double foundLatitude;
		double foundLongitude;

		foundLatitude = y0 + y;
		foundLongitude = x0 + new_x;

		latlon.put("lat", foundLatitude);
		latlon.put("lon", foundLongitude);

		return latlon;
	}

	public List<ResourceDataObs> createEditResourceMapping(List<ResourceData> resources) {
		List<ResourceDataObs> editResource = new ArrayList<ResourceDataObs>();
		for (ResourceData resourceUser : resources) {
			Resource resource = resourceUser.getResource();
			editResource.add(new ResourceDataObs(resource.getFileName(), resource.getUrl(), resource.getType(),
					resource.getDescription(), resource.getRating(), resource.getLicenseId(), resource.getContext(),
					resource.getLanguageId(), resource.getContributor()));

		}
		return editResource;

	}

	public UserGroupObvFilterData getUGFilterObvData(Observation observation) {
		UserGroupObvFilterData ugFilterData = new UserGroupObvFilterData();
		Long taxonomyId = null;
		if (observation.getMaxVotedRecoId() != null)
			taxonomyId = recoSerivce.fetchTaxonId(observation.getMaxVotedRecoId());
		ugFilterData.setObservationId(observation.getId());
		ugFilterData.setCreatedOnDate(observation.getCreatedOn());
		ugFilterData.setLatitude(observation.getLatitude());
		ugFilterData.setLongitude(observation.getLongitude());
		ugFilterData.setObservedOnDate(observation.getFromDate());
		ugFilterData.setAuthorId(observation.getAuthorId());
		ugFilterData.setTaxonomyId(taxonomyId);

		return ugFilterData;
	}

	public Observation updateObservationResourceCount(Observation observation, List<Resource> resources) {

		Integer noOfImages = 0;
		Integer noOfAudio = 0;
		Integer noOfVideo = 0;

		Long reprImage = null;
		int rating = 0;
		for (Resource res : resources) {
			if (res.getType().equals("AUDIO"))
				noOfAudio++;
			else if (res.getType().equals("IMAGE")) {
				noOfImages++;
				if (reprImage == null)
					reprImage = res.getId();
				if (res.getRating() != null && res.getRating() > rating) {
					reprImage = res.getId();
					rating = res.getRating();
				}
			} else if (res.getType().equals("VIDEO"))
				noOfVideo++;
		}
		observation.setNoOfAudio(noOfAudio);
		observation.setNoOfImages(noOfImages);
		observation.setNoOfVideos(noOfVideo);
		observation.setReprImageId(reprImage);

		return observation;

	}

	public void updateGeoPrivacy(List<Observation> observationList) {

		try {

			InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

			Properties properties = new Properties();
			try {
				properties.load(in);
			} catch (IOException e) {
				logger.error(e.getMessage());
			}
			String geoPrivacyTraitsValue = properties.getProperty("geoPrivacyValues");
			in.close();

			List<Long> geoPrivateTaxonId = traitsServiceApi.getTaxonListByValueId(geoPrivacyTraitsValue);

			for (Observation observation : observationList) {
				System.out.println("--------START---------");
				System.out.println("Observation Id : " + observation.getId());
				System.out.println("---------END----------");

				if (observation.getGeoPrivacy() == false && observation.getMaxVotedRecoId() != null) {
					Long taxonId = recoSerivce.fetchTaxonId(observation.getMaxVotedRecoId());
					if (taxonId != null) {

						if (geoPrivateTaxonId.contains(taxonId)) {
							System.out.println("---------BEGIN----------");
							System.out.println("Observation Id : " + observation.getId());
							observation.setGeoPrivacy(true);
							observationDAO.update(observation);
							rabbitMQProducer.setMessage("esmodule", observation.getId().toString(), "Observation Core");
							System.out.println("----------END------------");
						}

					}
				}
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

}
