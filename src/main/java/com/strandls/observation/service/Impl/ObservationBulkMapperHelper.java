package com.strandls.observation.service.Impl;

import com.strandls.file.api.UploadApi;
import com.strandls.file.model.FilesDTO;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.es.util.ESCreateThread;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.pojo.DataTable;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoData;
import com.strandls.observation.service.RecommendationService;
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
import javax.ws.rs.core.HttpHeaders;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

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
    ObservationDAO observationDAO;

    @Inject
    private Headers headers;

    @Inject
    ESUpdate esUpdate;

    @SuppressWarnings("deprecation")
    public Observation creationObservationMapping(Long userId, Map<String, Integer> fieldMapping, Row dataRow,
                                                  DataTable dataTable, List<SpeciesGroup> speciesGroupList) {
        Observation observation;
        try {
            Boolean geoPrivacy = Boolean.TRUE;
            if (fieldMapping.get("geoPrivacy") != null) {
                Cell geoPrivacyCell = dataRow.getCell(fieldMapping.get("geoPrivacy"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
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
            }

            Date fromDate = null;
            if (fieldMapping.get("fromDate") != null) {
                Cell fromDateCell = dataRow.getCell(fieldMapping.get("fromDate"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (fromDateCell != null) {
                    fromDate = fromDateCell.getDateCellValue();
                } else { // get value from dataTable metadata if not mentioned in excel
                    fromDate = dataTable.getTemporalCoverageFromDate();
                }
            }

            Date toDate = null;
            if (fieldMapping.get("toDate") != null) {
                Cell toDateCell = dataRow.getCell(fieldMapping.get("toDate"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (toDateCell != null) {
                    toDate = toDateCell.getDateCellValue();
                } else { // get value from dataTable metadata if not mentioned in excel
                    toDate = dataTable.getTemporalCoverageToDate();
                }
            }

            String observedAt = "";
            if (fieldMapping.get("observedAt") != null) {
                Cell observedAtCell = dataRow.getCell(fieldMapping.get("observedAt"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (observedAtCell != null) {
                    observedAtCell.setCellType(CellType.STRING);
                    observedAt = observedAtCell.getStringCellValue();
                }
            }

            String locationScale = "APPROXIMATE";
            if (fieldMapping.get("locationScale") != null) {
                Cell locationScaleCell = dataRow.getCell(fieldMapping.get("locationScale"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (locationScaleCell != null) {
                    locationScaleCell.setCellType(CellType.STRING);
                    locationScale = locationScaleCell.getStringCellValue();
                }
            }

            Double latitude = null;
            if (fieldMapping.get("latitude") != null) {
                Cell latitudeCell = dataRow.getCell(fieldMapping.get("latitude"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (latitudeCell != null) {
                    latitudeCell.setCellType(CellType.STRING);
                    latitude = Double.parseDouble(latitudeCell.getStringCellValue());
                } else { // get value from dataTable metadata if not mentioned in excel
                    latitude = dataTable.getGeographicalCoverageLatitude();
                }
            }

            Double longitude = null;
            if (fieldMapping.get("longitude") != null) {
                Cell longitudeCell = dataRow.getCell(fieldMapping.get("longitude"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (longitudeCell != null) {
                    longitudeCell.setCellType(CellType.STRING);
                    longitude = Double.parseDouble(longitudeCell.getStringCellValue());
                } else { // get value from dataTable metadata if not mentioned in excel
                    longitude = dataTable.getGeographicalCoverageLongitude();
                }
            }

            String dateAccuracy = "ACCURATE";
            if (fieldMapping.get("dateAccuracy") != null) {
                Cell dateAccuracyCell = dataRow.getCell(fieldMapping.get("dateAccuracy"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (dateAccuracyCell != null) {
                    dateAccuracyCell.setCellType(CellType.STRING);
                    dateAccuracy = dateAccuracyCell.getStringCellValue();
                }
            }

            String notes = null;
            if (fieldMapping.get("notes") != null) {
                Cell notesCell = dataRow.getCell(fieldMapping.get("notes"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (notesCell != null) {
                    notesCell.setCellType(CellType.STRING);
                    notes = notesCell.getStringCellValue();
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

            observation = new Observation(null, 0L, userId, new Date(), speciesGroup != null ? speciesGroup.getId() : null,
                    latitude, longitude, notes, fromDate, observedAt, 0, null,
                    0, geoPrivacy, null, false, new Date(),
                    null, 0L, null, null, true, false, false, null, toDate, topology,
                    null /* checklist annotations */, 0, false, 822L, 1L,
                    locationScale, null, null, null, null,
                    null, null, null, null, null,
                    null, null, null, null, null,
                    "LIST", "HUMAN_OBSERVATION", 0, 0, 0,
                    0, dataTable.getId(), dateAccuracy);

            return observation;
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
        return null;
    }

    public void createTags(HttpServletRequest request, Map<String, Integer> fieldMapping, Row dataRow, Long observationId) {
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

                    utilityServiceApi = headers.addUtilityHeaders(utilityServiceApi, request.getHeader(HttpHeaders.AUTHORIZATION));
                    utilityServiceApi.createTags("observation", tagsMappingData);
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    @SuppressWarnings("deprecation")
    public void createFactsMapping(HttpServletRequest request, Map<String, Integer> fieldMapping, Row dataRow, List<TraitsValuePair> pairs, Long observationId) {
        try {
            Map<String, List<Long>> facts = new HashMap<>();
            for (TraitsValuePair pair : pairs) {
                Integer field = fieldMapping.get(pair.getTraits().getName());
                if (field == null) continue;

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
            if (facts.isEmpty()) return;
            FactsCreateData factsCreateData = new FactsCreateData();
            factsCreateData.setFactValuePairs(facts);
            factsCreateData.setMailData(null);
            traitServiceApi = headers.addTraitsHeaders(traitServiceApi, request.getHeader(HttpHeaders.AUTHORIZATION));
            traitServiceApi.createFacts("species.participation.Observation", String.valueOf(observationId),
                    factsCreateData);
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    public void createRecoMapping(HttpServletRequest request, Map<String, Integer> fieldMapping,
                                  Row dataRow, Observation observation, Long userId) {
        try {
            RecoCreate recoCreate = prepareRecoMapping(dataRow, fieldMapping);
            if (recoCreate != null) {
                Long maxVotedReco = recoService.createRecoVote(request, userId, observation.getId(),
                        recoCreate.getScientificNameId(), recoCreate, true);
                observation.setMaxVotedRecoId(maxVotedReco);
                observationDAO.update(observation);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    private RecoCreate prepareRecoMapping(Row dataRow, Map<String, Integer> fieldMapping) {
        RecoCreate recoCreate = null;
        try {
            RecoData recoData = new RecoData();
            String commonName;
            if (fieldMapping.get("commonName") != null) {
                Cell commonNameCell = dataRow.getCell(fieldMapping.get("commonName"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (commonNameCell != null) {
                    commonNameCell.setCellType(CellType.STRING);
                    commonName = commonNameCell.getStringCellValue();

                    recoData.setTaxonCommonName(commonName);
                }
            }

            String scientificName;
            if (fieldMapping.get("scientificName") != null) {
                Cell scientificNameCell = dataRow.getCell(fieldMapping.get("scientificName"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
                if (scientificNameCell != null) {
                    scientificNameCell.setCellType(CellType.STRING);
                    scientificName = scientificNameCell.getStringCellValue();

                    recoData.setTaxonCommonName(scientificName);
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
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
        return recoCreate;
    }

    public void createObservationResource(HttpServletRequest request, Row dataRow, Map<String, Integer> fieldMapping,
                                          List<License> licenses, Long userId, Observation observation) {
        try {
            if (fieldMapping.get("fileName") == null) return;
            Cell cell = dataRow.getCell(fieldMapping.get("fileName"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
            if (cell == null) return;
            cell.setCellType(CellType.STRING);

            String fileNames = cell.getStringCellValue();
            List<String> cellFiles = Arrays.asList(fileNames.split(","));
            if (cellFiles.isEmpty()) return;

            FilesDTO files = new FilesDTO();
            files.setFiles(cellFiles);
            files.setFolder("observations");
            files.setModule("observation");

            fileUploadApi = headers.addFileUploadHeader(fileUploadApi, request.getHeader(HttpHeaders.AUTHORIZATION));
            Map<String, Object> fileResponse = fileUploadApi.handleBulkUploadMoveFiles(files);
            if (fileResponse != null && !fileResponse.isEmpty()) {
                List<Resource> resources = mapFileResponseToResource(fieldMapping, dataRow,
                        licenses, fileResponse, userId);
                if (resources == null) return;

                resourceServicesApi = headers.addResourceHeaders(resourceServicesApi, request.getHeader(HttpHeaders.AUTHORIZATION));
                resourceServicesApi.createResource("OBSERVATION", String.valueOf(observation.getId()), resources);

                Integer noOfImages = 0;
                Integer noOfAudio = 0;
                Integer noOfVideo = 0;

                Long reprImage = null;
                int rating = 0;
                for (Resource res : resources) {
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
            logActivities.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), null, observation.getId(),
                    observation.getId(), "observation", null, "Observation created", null);
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    private List<Resource> mapFileResponseToResource(Map<String, Integer> fieldMapping, Row dataRow, List<License> licenses,
                                                     Map<String, Object> fileResponse, Long userId) {
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
                resource.setLanguageId(205L);
                resource.setAccessRights(null);
                resource.setAnnotations(null);
                resource.setGbifId(null);

                License license;
                if (fieldMapping.get("license") != null) {
                    Cell licenseCell = dataRow.getCell(fieldMapping.get("license"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
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
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
        return resources;
    }

    public void createUserGroupMapping(HttpServletRequest request, Map<String, Integer> fieldMapping,
                                       Row dataRow, List<UserGroupIbp> userGroupsList, Long observationId) {
        try {
            if (fieldMapping.get("userGroups") == null) return;
            Cell cell = dataRow.getCell(fieldMapping.get("userGroups"), MissingCellPolicy.RETURN_BLANK_AS_NULL);
            if (cell == null) return;

            cell.setCellType(CellType.STRING);
            String[] cellGroups = cell.getStringCellValue().split(",");
            List<Long> userGroupIds = new ArrayList<>();
            for (String cellGroup : cellGroups) {
            for (UserGroupIbp group : userGroupsList) {
                    if (group.getName().equalsIgnoreCase(cellGroup.trim())) {
                        userGroupIds.add(group.getId());
                        break;
                    }
                }
            }

            if (userGroupIds.isEmpty()) return;
            UserGroupMappingCreateData userGroupMappingCreateData = new UserGroupMappingCreateData();
            userGroupMappingCreateData.setUserGroups(userGroupIds);
            userGroupMappingCreateData.setMailData(null);

            userGroupServiceApi = headers.addUserGroupHeader(userGroupServiceApi, request.getHeader(HttpHeaders.AUTHORIZATION));
            userGroupServiceApi.createObservationUserGroupMapping(String.valueOf(observationId), userGroupMappingCreateData);

            // custom field function call
            createCustomFieldMapping(request, fieldMapping, dataRow, userGroupIds, observationId);
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    public void createCustomFieldMapping(HttpServletRequest request, Map<String, Integer> fieldMapping,
                                         Row dataRow, List<Long> userGroupIds, Long observationId) {
        try {
            List<CustomFieldFactsInsert> customFieldFactsInsertList = new ArrayList<>();
            cfServiceApi = headers.addCFHeaders(cfServiceApi, request.getHeader(HttpHeaders.AUTHORIZATION));
            for (Long userGroupId: userGroupIds) {
                List<CustomFieldDetails> customFieldDetails = cfServiceApi.getUserGroupCustomFields(
                        String.valueOf(userGroupId));
                for (CustomFieldDetails customFieldDetail: customFieldDetails) {
                    String customFieldLabel = customFieldDetail.getCustomFields().getName();
                    if (fieldMapping.get(customFieldLabel) != null) {
                        Cell docCustomFieldCell = dataRow.getCell(fieldMapping.get(customFieldLabel),
                                MissingCellPolicy.RETURN_BLANK_AS_NULL);
                        if (docCustomFieldCell == null) continue;

                        docCustomFieldCell.setCellType(CellType.STRING);
                        String docCellValue = docCustomFieldCell.getStringCellValue();
                        List<CustomFieldValues> customFieldValues = cfServiceApi.getCustomFieldOptions(
                                String.valueOf(observationId),
                                String.valueOf(userGroupId),
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
                            for (CustomFieldValues customField: customFieldValues) {
                                if (customField.getValues().equalsIgnoreCase(docCellValue)) {
                                    customFieldValue = customField;
                                    break;
                                }
                            }
                            if (customFieldValue == null) continue;

                            customFieldFactsInsert.setSingleCategorical(customFieldValue.getId());
                        } else if (customFieldDetail.getCustomFields().getFieldType()
                                .equalsIgnoreCase("MULTIPLE CATEGORICAL")) {
                            List<Long> multiCatValues = new ArrayList<>();
                            for (CustomFieldValues customField: customFieldValues) {
                                if (customField.getValues().equalsIgnoreCase(docCellValue)) {
                                    multiCatValues.add(customField.getId());
                                }
                            }
                            if (multiCatValues.isEmpty()) continue;

                            customFieldFactsInsert.setMultipleCategorical(multiCatValues);
                        } else if (customFieldDetail.getCustomFields().getFieldType()
                                .equalsIgnoreCase("RANGE")) {
                            // fill this
                        }

                        customFieldFactsInsertList.add(customFieldFactsInsert);
                    }
                }
            }

            if (customFieldFactsInsertList.isEmpty()) return;

            for (CustomFieldFactsInsert customFieldFactsInsert: customFieldFactsInsertList) {
                CustomFieldFactsInsertData customFieldFactsInsertData = new CustomFieldFactsInsertData();
                customFieldFactsInsertData.setFactsCreateData(customFieldFactsInsert);
                customFieldFactsInsertData.setMailData(null);
                cfServiceApi.addUpdateCustomFieldData(customFieldFactsInsertData);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    public void updateUserGroupFilter(HttpServletRequest request, Observation observation) {
        try {
            UserGroupObvFilterData ugObvFilterData = observationMapperHelper.getUGFilterObvData(observation);
            userGroupServiceApi = headers.addUserGroupHeader(userGroupServiceApi,
                    request.getHeader(HttpHeaders.AUTHORIZATION));
            userGroupServiceApi.getFilterRule(ugObvFilterData);
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    public void updateGeoPrivacy(Observation observation) {
        try {
            observationMapperHelper.updateGeoPrivacy(Collections.singletonList(observation));
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }

    public void updateESThread(Long id) {
        try {
            ExecutorService executorService = Executors.newSingleThreadExecutor();
            ESCreateThread esThread = new ESCreateThread(esUpdate, id.toString());
            executorService.submit(esThread).get();
            executorService.shutdownNow();
            executorService.awaitTermination(5, TimeUnit.MINUTES);
        } catch (Exception ex) {
            ex.printStackTrace();
            logger.error(ex.getMessage());
        }
    }
}
