package com.strandls.observation.service.Impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.file.api.UploadApi;
import com.strandls.file.model.FilesDTO;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dao.RecommendationVoteDao;
import com.strandls.dataTable.pojo.DataTableWkt;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoData;
import com.strandls.observation.service.RecommendationService;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.observation.util.PropertyFileUtil;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.License;
import com.strandls.resource.pojo.Resource;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactsCreateData;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.userGroup.controller.CustomFieldServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.*;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.Tags;
import com.strandls.utility.pojo.TagsMapping;
import com.strandls.utility.pojo.TagsMappingData;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.*;

public class ObservationBulkMapperHelper {

	private final Logger logger = LoggerFactory.getLogger(ObservationBulkMapperHelper.class);

	@Inject
	UtilityServiceApi utilityServiceApi;

	@Inject
	TraitsServiceApi traitServiceApi;

	@Inject
	RecommendationService recoService;

	@Inject
	UploadApi fileUploadApi;

	@Inject
	ResourceServicesApi resourceServicesApi;

	@Inject
	UserGroupSerivceApi userGroupServiceApi;

	@Inject
	CustomFieldServiceApi cfServiceApi;

	@Inject
	UserGroupSerivceApi userGroupSerivceApi;

	@Inject
	LogActivities logActivities;

	@Inject
	ObservationMapperHelper observationMapperHelper;

	@Inject
	private ObjectMapper om;

	@Inject
	private RecommendationVoteDao recoVoteDao;

	@Inject
	ObservationDAO observationDAO;

	@Inject
	private Headers headers;

	private Long defaultLanguageId = Long
			.parseLong(PropertyFileUtil.fetchProperty("config.properties", "defaultLanguageId"));

	private Long defaultLicenseId = Long
			.parseLong(PropertyFileUtil.fetchProperty("config.properties", "defaultLicenseId"));

	@SuppressWarnings("deprecation")
	public Observation creationObservationMapping(Long userId, String requestAuthHeader,
			Map<String, Integer> fieldMapping, Row dataRow, DataTableWkt dataTable, List<SpeciesGroup> speciesGroupList,
			Map<String, Integer> checklistAnnotation, Boolean isVerified, String basisOfRecord) {
		try {
			Boolean geoPrivacy = Boolean.FALSE;
			Observation observation = new Observation();
			if (fieldMapping.get("geoPrivacy") != null) {
				Cell geoPrivacyCell = dataRow.getCell(fieldMapping.get("geoPrivacy"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (geoPrivacyCell != null) {
					geoPrivacyCell.setCellType(CellType.BOOLEAN);
					geoPrivacy = geoPrivacyCell.getBooleanCellValue();
				}
			}

			SpeciesGroup speciesGroup = null;
			if (fieldMapping.get("sGroup") != null) {
				Cell sGroupCell = dataRow.getCell(fieldMapping.get("sGroup"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (sGroupCell != null) {
					sGroupCell.setCellType(CellType.STRING);

					speciesGroup = speciesGroupList.stream().filter(group -> {
						final String sGroup = sGroupCell.getStringCellValue();
						return group.getName().equalsIgnoreCase(sGroup);
					}).findFirst().orElse(null);

				} else { // get value from dataTable metadata if not mentioned in excel
					speciesGroup = new SpeciesGroup();
					speciesGroup.setId(Long.parseLong(dataTable.getTaxonomicCoverageGroupIds().split(",")[0].trim()));
				}
			} else { // get value from dataTable metadata if not mentioned in excel
				speciesGroup = new SpeciesGroup();
				speciesGroup.setId(Long.parseLong(dataTable.getTaxonomicCoverageGroupIds().split(",")[0].trim()));
			}

			Date fromDate = null;
			if (fieldMapping.get("fromDate") != null) {
				Cell fromDateCell = dataRow.getCell(fieldMapping.get("fromDate"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (fromDateCell != null) {
					fromDateCell.setCellType(CellType.NUMERIC);
					fromDate = fromDateCell.getDateCellValue();
				}
			}

			Date toDate = null;
			if (fieldMapping.get("toDate") != null) {
				Cell toDateCell = dataRow.getCell(fieldMapping.get("toDate"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (toDateCell != null) {
					toDateCell.setCellType(CellType.NUMERIC);
					toDate = toDateCell.getDateCellValue();
				}
			}

			String observedAt = "";
			if (fieldMapping.get("observedAt") != null) {
				Cell observedAtCell = dataRow.getCell(fieldMapping.get("observedAt"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (observedAtCell != null) {
					observedAtCell.setCellType(CellType.STRING);
					observedAt = observedAtCell.getStringCellValue();
				} else {
					observedAt = dataTable.getGeographicalCoveragePlaceName();
				}
			} else {
				observedAt = dataTable.getGeographicalCoveragePlaceName();
			}

			String locationScale = "APPROXIMATE";
			if (fieldMapping.get("locationScale") != null) {
				Cell locationScaleCell = dataRow.getCell(fieldMapping.get("locationScale"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (locationScaleCell != null) {
					locationScaleCell.setCellType(CellType.STRING);
					locationScale = locationScaleCell.getStringCellValue();
				}
			}

			Double latitude = null;
			Double longitude = null;

			if (fieldMapping.get("latitude") != null) {
				Cell latitudeCell = dataRow.getCell(fieldMapping.get("latitude"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (latitudeCell != null) {
					latitudeCell.setCellType(CellType.STRING);
					latitude = Double.parseDouble(latitudeCell.getStringCellValue());
				}
			}

			if (fieldMapping.get("longitude") != null) {
				Cell longitudeCell = dataRow.getCell(fieldMapping.get("longitude"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (longitudeCell != null) {
					longitudeCell.setCellType(CellType.STRING);
					longitude = Double.parseDouble(longitudeCell.getStringCellValue());
				}
			}

			if (longitude == null && latitude == null) {
				longitude = dataTable.getGeographicalCoverageLongitude();
				latitude = dataTable.getGeographicalCoverageLatitude();
			}

			if (Boolean.FALSE.equals(observationMapperHelper.checkObservationBounds(latitude, longitude))) {
				throw new ObservationInputException("Observation latitude/longitude not within bounds");

			}
			String dateAccuracy = "ACCURATE";
			if (fieldMapping.get("dateAccuracy") != null) {
				Cell dateAccuracyCell = dataRow.getCell(fieldMapping.get("dateAccuracy"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (dateAccuracyCell != null) {
					dateAccuracyCell.setCellType(CellType.STRING);
					dateAccuracy = dateAccuracyCell.getStringCellValue();
				} else {
					dateAccuracy = dataTable.getTemporalCoverageDateAccuracy();
				}
			} else {
				dateAccuracy = dataTable.getTemporalCoverageDateAccuracy();
			}

			String notes = null;
			if (fieldMapping.get("notes") != null) {
				Cell notesCell = dataRow.getCell(fieldMapping.get("notes"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (notesCell != null) {
					notesCell.setCellType(CellType.STRING);
					notes = notesCell.getStringCellValue();
				}
			}

			String checklistString = null;
			if (!checklistAnnotation.isEmpty()) {

				Map<String, String> checklistMap = new HashMap<String, String>();
				checklistAnnotation.forEach((k, v) -> {

					Cell cellValue = dataRow.getCell(v, MissingCellPolicy.RETURN_BLANK_AS_NULL);
					if (cellValue != null) {
						cellValue.setCellType(CellType.STRING);
						checklistMap.put(k, cellValue.getStringCellValue());
					} else {
						checklistMap.put(k, "");
					}

				});

				try {
					checklistString = om.writeValueAsString(checklistMap);
				} catch (JsonProcessingException e) {
					logger.error(e.getMessage());
				}

			}

			Geometry topology = null;
			if (latitude != null && longitude != null) {
				GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);
				DecimalFormat df = new DecimalFormat("#.####");
				df.setRoundingMode(RoundingMode.HALF_EVEN);
				double lat = Double.parseDouble(df.format(latitude));
				double lon = Double.parseDouble(df.format(longitude));
				Coordinate c = new Coordinate(lat, lon);
				topology = geofactory.createPoint(c);
			}

			observation.setAuthorId(userId);
			observation.setIsShowable(true);
			observation.setVersion(0L);
			observation.setCreatedOn(new Date());
			observation.setGroupId(speciesGroup != null ? speciesGroup.getId() : 830);
			observation.setLatitude(latitude);
			observation.setLongitude(longitude);
			observation.setNotes(notes);
			observation.setIsDeleted(false);
			observation.setFromDate(fromDate);
			observation.setPlaceName(observedAt); // place name given by user
			observation.setRating(0);// what to insert
			observation.setToDate(toDate);
			observation.setGeoPrivacy(geoPrivacy);
			observation.setTopology(topology);
			observation.setFeatureCount(0);// update field initially 0, used only after its attached and featured to a
			observation.setIsLocked(isVerified);// same value as isVerified
			observation.setLicenseId(defaultLicenseId);// default 822
			observation.setLanguageId(defaultLanguageId);
			observation.setLocationScale(locationScale); // 5 options

			observation.setReprImageId(null);
			observation.setProtocol("LIST");
			observation.setBasisOfRecord(basisOfRecord != null ? basisOfRecord : "PRIMARY_OBSERVATION");
			observation.setNoOfImages(0);
			observation.setNoOfAudio(0);
			observation.setNoOfVideos(0);

			observation.setNoOfIdentifications(0);
			observation.setDataTableId(dataTable.getId());//
			observation.setDateAccuracy(dateAccuracy);
			observation.setFlagCount(0);
			observation.setVisitCount(0L);
			observation.setIsChecklist(false);// false for nrml case only used in DATATABLE
			observation.setSourceId(null);// observation id in nrml case, used only in GBIF
			observation.setChecklistAnnotations(checklistString);// from data set
			observation.setAccessRights(null);// null for nrml case only used in GBIF
			observation.setCatalogNumber(null);// null for nrml case only used in GBIF
			observation.setDatasetId(null);// null for nrml case only used in GBIF
			observation.setExternalDatasetKey(null);// null for nrml case only used in GBIF
			observation.setExternalId(null);// null for nrml case only used in GBIF
			observation.setExternalUrl(null);// null for nrml case only used in GBIF
			observation.setInformationWithheld(null);// null for nrml case only used in GBIF
			observation.setLastCrawled(null);// null for nrml case only used in GBIF
			observation.setLastInterpreted(null);// null for nrml case only used in GBIF
			observation.setOriginalAuthor(null);// null for nrml case only used in GBIF
			observation.setPublishingCountry(null);// from IP address
			observation.setViaCode(null);// null for nrml case only used in GBIF
			observation.setViaId(null);// null for nrml case only used in GBIF
			observation.setIsVerified(isVerified);
			observation = observationDAO.save(observation);
			if (observation != null) {
				logActivities.LogActivity(requestAuthHeader, null, observation.getId(), observation.getId(),
						"observation", null, "Observation created", null);
			}
			return observation;

		} catch (

		Exception ex) {
			logger.error(ex.getMessage());
		}
		return null;
	}

	public void createTags(String requestAuthHeader, Map<String, Integer> fieldMapping, Row dataRow,
			Long observationId) {
		try {
			if (fieldMapping.get("tags") != null) {
				Cell cell = dataRow.getCell(fieldMapping.get("tags"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
				String tagsInCell = cell != null ? cell.getStringCellValue() : "";
				if (!tagsInCell.isEmpty()) {
					TagsMapping tagsMapping = new TagsMapping();
					List<Tags> tags = new ArrayList<>();
					for (String tag : tagsInCell.split(",")) {
						Tags t = new Tags();
						t.setName(tag);
						tags.add(t);
					}
					tagsMapping.setObjectId(observationId);
					tagsMapping.setTags(tags);

					TagsMappingData tagsMappingData = new TagsMappingData();
					tagsMappingData.setTagsMapping(tagsMapping);
					tagsMappingData.setMailData(null);

					utilityServiceApi = headers.addUtilityHeaders(utilityServiceApi, requestAuthHeader);
					utilityServiceApi.createTags("observation", tagsMappingData);
				}
			}
		} catch (Exception ex) {
			logger.error(ex.getMessage());
		}
	}

	@SuppressWarnings("deprecation")
	public void createFactsMapping(String requestAuthHeader, Map<String, Integer> fieldMapping, Row dataRow,
			List<TraitsValuePair> pairs, Long observationId) {
		try {
			Map<String, List<Long>> facts = new HashMap<>();
			for (TraitsValuePair pair : pairs) {
				Integer field = fieldMapping.get(pair.getTraits().getName());
				if (field == null)
					continue;

				Cell traitCell = dataRow.getCell(field, MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (traitCell != null) {
					traitCell.setCellType(CellType.STRING);
					String[] traitsValues = traitCell.getStringCellValue().split(",");

					List<Long> traits = new ArrayList<>();
					for (TraitsValue tv : pair.getValues()) {
						for (String t : traitsValues) {
							if (t.equalsIgnoreCase(tv.getValue())) {
								traits.add(tv.getId());
							}
						}
					}

					facts.put(String.valueOf(pair.getTraits().getId()), traits);
				}
			}
			if (facts.isEmpty())
				return;
			FactsCreateData factsCreateData = new FactsCreateData();
			factsCreateData.setFactValuePairs(facts);
			factsCreateData.setMailData(null);
			traitServiceApi = headers.addTraitsHeaders(traitServiceApi, requestAuthHeader);
			traitServiceApi.createFacts("species.participation.Observation", String.valueOf(observationId),
					factsCreateData);
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
	}

	public void createRecoMapping(HttpServletRequest request, String requestAuthHeader,
			Map<String, Integer> fieldMapping, Row dataRow, Observation observation, Long userId) {
		try {
			RecoCreate recoCreate = prepareRecoMapping(dataRow, fieldMapping);
			if (recoCreate != null) {
				Long maxVotedReco = recoService.createRecoVote(request, userId, observation.getId(),
						recoCreate.getTaxonId(), recoCreate, true);
				observation.setMaxVotedRecoId(maxVotedReco);
				observation.setNoOfIdentifications(recoVoteDao.findRecoVoteCount(observation.getId()));
				observationDAO.update(observation);
			}
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
	}

	@SuppressWarnings("deprecation")
	private RecoCreate prepareRecoMapping(Row dataRow, Map<String, Integer> fieldMapping) {
		RecoCreate recoCreate = null;
		try {
			RecoData recoData = new RecoData();
			String commonName;
			if (fieldMapping.get("commonName") != null) {
				Cell commonNameCell = dataRow.getCell(fieldMapping.get("commonName"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (commonNameCell != null) {
					commonNameCell.setCellType(CellType.STRING);
					commonName = commonNameCell.getStringCellValue();

					recoData.setTaxonCommonName(commonName);
				}
			}

			String scientificName;
			if (fieldMapping.get("scientificName") != null) {
				Cell scientificNameCell = dataRow.getCell(fieldMapping.get("scientificName"),
						MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (scientificNameCell != null) {
					scientificNameCell.setCellType(CellType.STRING);
					scientificName = scientificNameCell.getStringCellValue();

					recoData.setTaxonScientificName(scientificName);
				}
			}

			String comment;
			if (fieldMapping.get("comment") != null) {
				Cell commentCell = dataRow.getCell(fieldMapping.get("comment"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
				if (commentCell != null) {
					commentCell.setCellType(CellType.STRING);
					comment = commentCell.getStringCellValue();

					recoData.setRecoComment(comment);
				}
			}

			recoCreate = observationMapperHelper.createRecoMapping(recoData);
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
		return recoCreate;
	}

	@SuppressWarnings("deprecation")
	public void createObservationResource(String requestAuthHeader, Row dataRow, Map<String, Integer> fieldMapping,
			List<License> licenses, Long userId, Observation observation, Map<String, String> myImageUpload) {
		List<String> filesWithPath = new ArrayList<>();
		try {

			if (fieldMapping.get("fileName") == null || myImageUpload.isEmpty())
				return;
			Cell cell = dataRow.getCell(fieldMapping.get("fileName"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
			if (cell == null)
				return;
			cell.setCellType(CellType.STRING);

			String fileNames = cell.getStringCellValue();
			List<String> cellFiles = Arrays.asList(fileNames.split(","));
			if (cellFiles.isEmpty())
				return;

			for (String file : cellFiles) {
				myImageUpload.forEach((k, v) -> {
					if (k.contains(file)) {
						filesWithPath.add(myImageUpload.get(k));
					}
				});

			}

			if (filesWithPath.isEmpty())
				return;

			FilesDTO files = new FilesDTO();
			files.setFiles(filesWithPath);
			files.setFolder("observations");
			files.setModule("observation");

			fileUploadApi = headers.addFileUploadHeader(fileUploadApi, requestAuthHeader);
			Map<String, Object> fileResponse = fileUploadApi.moveFiles(files);
			if (fileResponse != null && !fileResponse.isEmpty()) {
				List<Resource> resources = mapFileResponseToResource(fieldMapping, dataRow, licenses, fileResponse,
						userId);
				if (resources == null)
					return;

				resourceServicesApi = headers.addResourceHeaders(resourceServicesApi, requestAuthHeader);
				List<Resource> response = resourceServicesApi.createResource("OBSERVATION",
						String.valueOf(observation.getId()), resources);

				Integer noOfImages = 0;
				Integer noOfAudio = 0;
				Integer noOfVideo = 0;

				Long reprImage = null;
				int rating = 0;
				for (Resource res : response) {
					switch (res.getType()) {
					case "AUDIO":
						noOfAudio++;
						break;
					case "IMAGE":
						noOfImages++;
						if (reprImage == null)
							reprImage = res.getId();
						if (res.getRating() != null && res.getRating() > rating) {
							reprImage = res.getId();
							rating = res.getRating();
						}
						break;
					case "VIDEO":
						noOfVideo++;
						break;
					}
				}
				observation.setNoOfAudio(noOfAudio);
				observation.setNoOfImages(noOfImages);
				observation.setNoOfVideos(noOfVideo);
				observation.setReprImageId(reprImage);
				observationDAO.update(observation);
			}

		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
	}

	@SuppressWarnings({ "deprecation", "unchecked" })
	private List<Resource> mapFileResponseToResource(Map<String, Integer> fieldMapping, Row dataRow,
			List<License> licenses, Map<String, Object> fileResponse, Long userId) {
		List<Resource> resources = new ArrayList<>();
		try {
			for (Map.Entry<String, Object> resourceData : fileResponse.entrySet()) {
				Map<String, String> values = (Map<String, String>) resourceData.getValue();
				Resource resource = new Resource();
				resource.setVersion(0L);
				resource.setDescription(null);

				resource.setMimeType(null);
				if (values.get("mimeType").startsWith("image") || values.get("mimeType").equalsIgnoreCase("image"))
					resource.setType("IMAGE");
				else if (values.get("mimeType").startsWith("audio") || values.get("mimeType").equalsIgnoreCase("audio"))
					resource.setType("AUDIO");
				else if (values.get("mimeType").startsWith("video") || values.get("mimeType").equalsIgnoreCase("video"))
					resource.setType("VIDEO");
				resource.setFileName(values.get("name"));
				resource.setUrl(null);
				resource.setRating(null);
				resource.setUploadTime(new Date());
				resource.setUploaderId(userId);
				resource.setContext("OBSERVATION");
				resource.setLanguageId(defaultLanguageId);
				resource.setAccessRights(null);
				resource.setAnnotations(null);
				resource.setGbifId(null);

				License license;
				if (fieldMapping.get("license") != null) {
					Cell licenseCell = dataRow.getCell(fieldMapping.get("license"),
							MissingCellPolicy.RETURN_BLANK_AS_NULL);
					if (licenseCell != null) {
						licenseCell.setCellType(CellType.STRING);

						license = licenses.stream().filter(l -> {
							final String docLicense;
							docLicense = licenseCell.getStringCellValue().replaceAll("-", "_").toUpperCase();
							return l.getName().endsWith(docLicense);
						}).findAny().orElse(null);

						if (license == null) {
							return null;
						}
					} else {
						license = new License();
						license.setId(822L);
					}
				} else {
					license = new License();
					license.setId(822L);
				}

				resource.setLicenseId(license.getId());

				resources.add(resource);
			}
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
		return resources;
	}

	@SuppressWarnings("deprecation")
	public void createUserGroupMapping(String requestAuthHeader, Map<String, Integer> fieldMapping, Row dataRow,
			List<UserGroupIbp> userGroupsList, String userGroup, Long observationId) {
		String[] cellGroups ;
		try {
			if (fieldMapping.get("userGroups") == null&& userGroup.isEmpty())
				return;
			

			if (fieldMapping.get("userGroups") != null) {
				Cell cell = dataRow.getCell(fieldMapping.get("userGroups"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
				cell.setCellType(CellType.STRING);
				cellGroups =   cell.getStringCellValue().split(",");
			}else {
				cellGroups = userGroup.split(",");
			}
		
			List<Long> userGroupIds = new ArrayList<>();
			for (String cellGroup : cellGroups) {
				for (UserGroupIbp group : userGroupsList) {
					if (cellGroup.contains(group.getId().toString())) {
						userGroupIds.add(group.getId());
						break;
					}
				}
			}

			if (userGroupIds.isEmpty())
				return;
			UserGroupMappingCreateData userGroupMappingCreateData = new UserGroupMappingCreateData();
			userGroupMappingCreateData.setUserGroups(userGroupIds);
			userGroupMappingCreateData.setMailData(null);

			userGroupServiceApi = headers.addUserGroupHeader(userGroupServiceApi, requestAuthHeader);
			userGroupServiceApi.createObservationUserGroupMapping(String.valueOf(observationId),
					userGroupMappingCreateData);

			// custom field function call
			createCustomFieldMapping(requestAuthHeader, fieldMapping, dataRow, userGroupIds, observationId);
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
	}

	@SuppressWarnings("deprecation")
	public void createCustomFieldMapping(String requestAuthHeader, Map<String, Integer> fieldMapping, Row dataRow,
			List<Long> userGroupIds, Long observationId) {
		try {
			List<CustomFieldFactsInsert> customFieldFactsInsertList = new ArrayList<>();
			cfServiceApi = headers.addCFHeaders(cfServiceApi, requestAuthHeader);
			for (Long userGroupId : userGroupIds) {
				List<CustomFieldDetails> customFieldDetails = cfServiceApi
						.getUserGroupCustomFields(String.valueOf(userGroupId));
				for (CustomFieldDetails customFieldDetail : customFieldDetails) {
					String customFieldLabel = customFieldDetail.getCustomFields().getName();
					if (fieldMapping.get(customFieldLabel) != null) {
						Cell docCustomFieldCell = dataRow.getCell(fieldMapping.get(customFieldLabel),
								MissingCellPolicy.RETURN_BLANK_AS_NULL);
						if (docCustomFieldCell == null)
							continue;

						docCustomFieldCell.setCellType(CellType.STRING);
						String docCellValue = docCustomFieldCell.getStringCellValue();
						List<CustomFieldValues> customFieldValues = cfServiceApi.getCustomFieldOptions(
								String.valueOf(observationId), String.valueOf(userGroupId),
								String.valueOf(customFieldDetail.getCustomFields().getId()));

						CustomFieldFactsInsert customFieldFactsInsert = new CustomFieldFactsInsert();
						customFieldFactsInsert.setCustomFieldId(customFieldDetail.getCustomFields().getId());
						customFieldFactsInsert.setObservationId(observationId);
						customFieldFactsInsert.setUserGroupId(userGroupId);

						if (customFieldDetail.getCustomFields().getFieldType().equalsIgnoreCase("FIELD TEXT")) {
							customFieldFactsInsert.setTextBoxValue(docCellValue);
						} else if (customFieldDetail.getCustomFields().getFieldType()
								.equalsIgnoreCase("SINGLE CATEGORICAL")) {
							CustomFieldValues customFieldValue = null;
							for (CustomFieldValues customField : customFieldValues) {
								if (customField.getValues().equalsIgnoreCase(docCellValue)) {
									customFieldValue = customField;
									break;
								}
							}
							if (customFieldValue == null)
								continue;

							customFieldFactsInsert.setSingleCategorical(customFieldValue.getId());
						} else if (customFieldDetail.getCustomFields().getFieldType()
								.equalsIgnoreCase("MULTIPLE CATEGORICAL")) {
							List<Long> multiCatValues = new ArrayList<>();
							for (CustomFieldValues customField : customFieldValues) {
								if (customField.getValues().equalsIgnoreCase(docCellValue)) {
									multiCatValues.add(customField.getId());
								}
							}
							if (multiCatValues.isEmpty())
								continue;

							customFieldFactsInsert.setMultipleCategorical(multiCatValues);
						} else if (customFieldDetail.getCustomFields().getFieldType().equalsIgnoreCase("RANGE")) {
							// fill this
						}

						customFieldFactsInsertList.add(customFieldFactsInsert);
					}
				}
			}

			if (customFieldFactsInsertList.isEmpty())
				return;

			for (CustomFieldFactsInsert customFieldFactsInsert : customFieldFactsInsertList) {
				CustomFieldFactsInsertData customFieldFactsInsertData = new CustomFieldFactsInsertData();
				customFieldFactsInsertData.setFactsCreateData(customFieldFactsInsert);
				customFieldFactsInsertData.setMailData(null);
				cfServiceApi.addUpdateCustomFieldData(customFieldFactsInsertData);
			}
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
	}

	public void updateUserGroupFilter(String requestAuthHeader, Observation observation) {
		try {
			UserGroupObvFilterData ugObvFilterData = observationMapperHelper.getUGFilterObvData(observation);
			userGroupServiceApi = headers.addUserGroupHeader(userGroupServiceApi, requestAuthHeader);
			userGroupServiceApi.getFilterRule(ugObvFilterData);
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
	}

	public void updateGeoPrivacy(Observation observation) {
		try {
			observationMapperHelper.updateGeoPrivacy(Collections.singletonList(observation));
		} catch (Exception ex) {

			logger.error(ex.getMessage());
		}
	}
}
