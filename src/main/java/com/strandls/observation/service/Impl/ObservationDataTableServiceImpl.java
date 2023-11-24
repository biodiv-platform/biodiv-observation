package com.strandls.observation.service.Impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.dataTable.controllers.DataTableServiceApi;
import com.strandls.dataTable.pojo.BulkDTO;
import com.strandls.dataTable.pojo.DataTableWkt;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.UserScore;
import com.strandls.file.api.UploadApi;
import com.strandls.file.model.FilesDTO;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationDataTableShow;
import com.strandls.observation.pojo.ObservationDatatableList;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowObervationDataTable;
import com.strandls.observation.service.ObservationDataTableService;
import com.strandls.observation.util.DataTableMappingField;
import com.strandls.observation.util.ObservationBulkUploadThread;
import com.strandls.observation.util.ObservationDeleteThread;
import com.strandls.observation.util.TokenGenerator;
import com.strandls.resource.controllers.LicenseControllerApi;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.License;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.controller.CustomFieldServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.userGroup.pojo.CustomFieldData;
import com.strandls.userGroup.pojo.CustomFieldObservationData;
import com.strandls.dataTable.pojo.UserGroupCreateDatatable;

public class ObservationDataTableServiceImpl implements ObservationDataTableService {

	private final Logger logger = LoggerFactory.getLogger(ObservationDataTableServiceImpl.class);

	@Inject
	private ObservationDAO observationDao;

	@Inject
	private TraitsServiceApi traitService;

	@Inject
	private ResourceServicesApi resourceService;

	@Inject
	private UserGroupSerivceApi userGroupService;

	@Inject
	private CustomFieldServiceApi customFieldService;

	@Inject
	private LayerServiceApi layerService;

	@Inject
	private EsServicesApi esService;

	@Inject
	private UserServiceApi userService;

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private Headers headers;

	@Inject
	private DataTableHelper dataTableHelper;

	@Inject
	private UploadApi fileUploadApi;

	@Inject
	private ObservationBulkMapperHelper observationBulkMapperHelper;

	@Inject
	private LicenseControllerApi licenseControllerApi;

	@Inject
	private DataTableServiceApi dataTableService;

	@Inject
	private ObservationServiceImpl observationImpl;

	@Inject
	private RecommendationServiceImpl recoService;
	@Inject
	private ObjectMapper om;
	@Inject
	private TokenGenerator tokenGenerator;

	@Override
	public Long observationBulkUpload(HttpServletRequest request, ObservationBulkDTO observationBulkData) {
		CommonProfile profile = AuthUtil.getProfileFromRequest(request);
		Long userId = Long.parseLong(profile.getId());
		InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");
		Properties properties = new Properties();

		try {
			properties.load(in);
		} catch (IOException e) {
			logger.error(e.getMessage());
		}

		try {

			if (observationBulkData.getFilename() == null || observationBulkData.getFilename().length() <= 0) {
				throw new NullPointerException("Sheet Filename nout found");
			}

			String storageBasePath = properties.getProperty("storage_dir", "/apps/biodiv-image");
			String sheetDirectory = storageBasePath + File.separatorChar + "myUploads" + File.separatorChar + userId
					+ observationBulkData.getFilename();

			BulkDTO dataTableDTO = dataTableHelper.createDataTableBulkDTO(observationBulkData);

			dataTableService = headers.addDataTableHeaders(dataTableService,
					tokenGenerator.generate(userService.getUser(observationBulkData.getContributors().toString())));
			DataTableWkt dataTable = dataTableService.createDataTable(dataTableDTO);

			if (dataTable == null) {
				throw new NullPointerException("Unable to create DataTable, Unresolved Constrain");
			}

			try (XSSFWorkbook workbook = new XSSFWorkbook(new File(sheetDirectory))) {
				List<TraitsValuePair> traitsList = traitService.getAllTraits();
				List<UserGroupIbp> userGroupIbpList = userGroupService.getAllUserGroup();
				List<License> licenseList = licenseControllerApi.getAllLicenses();

				List<Long> accpectedList = userGroupIbpList.stream().map(s -> Long.parseLong(s.getId().toString()))
						.collect(Collectors.toList());

				List<Long> userGroupIds = observationBulkData.getUserGroup().isEmpty() ? new ArrayList<Long>()
						: Arrays.asList(observationBulkData.getUserGroup().split(",")).stream()
								.map(s -> Long.parseLong(s.trim())).filter(s -> accpectedList.contains(s))
								.collect(Collectors.toList());

				if (!userGroupIds.isEmpty()) {
					userGroupService = headers.addUserGroupHeader(userGroupService,
							request.getHeader(HttpHeaders.AUTHORIZATION));
					UserGroupCreateDatatable ugMapping = new UserGroupCreateDatatable();
					ugMapping.setUserGroupIds(userGroupIds);
					dataTableService.updateDatatableUserGroupMapping(dataTable.getId().toString(), ugMapping);
				}

				FilesDTO filesDto = new FilesDTO();
				filesDto.setFolder("observations");
				filesDto.setModule("observation");
				Map<String, String> myImageUpload = headers
						.addFileUploadHeader(fileUploadApi, request.getHeader(HttpHeaders.AUTHORIZATION))
						.getAllFilePathsByUser(filesDto).entrySet().stream()
						.collect(Collectors.toMap(Map.Entry::getKey, e -> (String) e.getValue()));

				ObservationBulkUploadThread uploadThread = new ObservationBulkUploadThread(observationBulkData, request,
						observationDao, observationBulkMapperHelper, esUpdate, userService, dataTable,
						observationBulkData.getContributors(), observationImpl.getAllSpeciesGroup(), traitsList,
						userGroupIbpList, licenseList, workbook, myImageUpload, resourceService, fileUploadApi,
						dataTableService, tokenGenerator, observationBulkData.getUserGroup(), headers);
				Thread thread = new Thread(uploadThread);
				thread.start();
			}
			return dataTable.getId();

		} catch (Exception ex) {
			logger.error(ex.getMessage());
		}

		return null;
	}

	@Override
	public ShowObervationDataTable showObservatioDataTable(HttpServletRequest request, Long dataTableId, Integer limit,
			Integer offset) {
		Map<String, String> authorScore = null;
		DataTableWkt dataTable = null;
		UserIbp user = null;
		Long count = null;
		List<UserGroupIbp> userGroups = null;
		Long userId = null;
		ObservationLocationInfo locationInfo = null;
		ObservationDatatableList observationList = null;
		ShowObervationDataTable dataTableRes = new ShowObervationDataTable();
		dataTableService = headers.addDataTableHeaders(dataTableService, request.getHeader(HttpHeaders.AUTHORIZATION));

		try {
			dataTable = dataTableService.showDataTable(dataTableId.toString());

			if (dataTable == null) {
				return null;
			}
			userId = dataTable.getUploaderId();
			user = userService.getUserIbp(userId.toString());
			userGroups = userGroupService.getObservationUserGroup(dataTableId.toString());
			observationList = fetchAllObservationByDataTableId(dataTableId, limit, offset);
			count = observationDao.getObservationCountForDatatable(dataTableId.toString());
			UserScore score = esService.getUserScore("eaf", "er", userId.toString(), "f");
				locationInfo = layerService.getLayerInfo(dataTable.getGeographicalCoverageLatitude().toString(),
					dataTable.getGeographicalCoverageLongitude().toString());
			dataTableRes.setAuthorInfo(user);
			dataTableRes.setLayerInfo(null);
			dataTableRes.setObservationList(observationList.getObservationList());
			dataTableRes.setLayerInfo(locationInfo);
			dataTableRes.setUserGroups(userGroups);
			dataTableRes.setDatatable(dataTable);
			dataTableRes.setCount(count);
			if (score.getRecord() != null && !score.getRecord().isEmpty()) {
				authorScore = score.getRecord().get(0).get("details");
				dataTableRes.setAuthorScore(authorScore);
			}
			return dataTableRes;
		} catch (Exception er) {
			logger.error(er.getMessage());
		}

		return null;
	}

	@Override
	public ObservationDatatableList fetchAllObservationByDataTableId(Long dataTableId, Integer limit, Integer offset) {
		List<Observation> observationList;
		DataTableWkt dataTable;
		List<Long> list = new ArrayList<Long>();
		Long total = null;
		List<ObservationDataTableShow> showDataList = new ArrayList<ObservationDataTableShow>();
		list.add(dataTableId);
		try {
			observationList = observationDao.fetchByDataTableId(list, limit, offset);
			total = observationDao.getObservationCountForDatatable(dataTableId.toString());
			dataTable = dataTableService.showDataTable(dataTableId.toString());
			if (observationList.isEmpty()) {
				return new ObservationDatatableList();
			}

			observationList.forEach((ob) -> {
				try {
					List<CustomFieldObservationData> cfDataList = customFieldService
							.getObservationCustomFields(ob.getId().toString());
					Map<String, Object> checkListAnnotation = new HashMap<String, Object>();
					List<FactValuePair> facts = traitService.getFacts("species.participation.Observation",
							ob.getId().toString());

					RecoIbp reco = null;
					UserIbp userInfo = null;
					String commonName = null;
					String scientificName = null;
					String fromDate = null;
					String toDate = null;
					if (ob.getFromDate() != null) {
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy");
						fromDate = dateFormat.format(ob.getFromDate());
					}

					if (ob.getToDate() != null) {
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy");
						toDate = dateFormat.format(ob.getToDate());
					}

					
					if (ob.getMaxVotedRecoId() != null) {
						reco = recoService.fetchRecoName(ob.getId(), ob.getMaxVotedRecoId());
						scientificName = reco.getScientificName() != null ? reco.getScientificName() : null;
						commonName = reco.getCommonName() != null ? reco.getCommonName() : null;
					}

					if (ob.getAuthorId() != null) {
						userInfo = userService.getUserIbp(ob.getAuthorId().toString());
					}
					
					if(ob.getChecklistAnnotations() != null) {
						checkListAnnotation=om.readValue(ob.getChecklistAnnotations(), new TypeReference<Map<String, Object>>() {
						});
					}

					if (facts != null && !facts.isEmpty()) {

						for (FactValuePair fact : facts) {

							checkListAnnotation.put(fact.getName(),
									checkListAnnotation.containsKey(fact.getName())
											? checkListAnnotation.get(fact.getName()) + "," + fact.getValue()
											: fact.getValue());

						}

					}
					if (cfDataList != null && !cfDataList.isEmpty()) {
						for (CustomFieldObservationData item : cfDataList) {
							for (CustomFieldData cf : item.getCustomField()) {
								if (cf.getCustomFieldValues() != null) {
									if (cf.getFieldType().equalsIgnoreCase("FIELD TEXT")) {
										checkListAnnotation.put(cf.getCfName(),
												cf.getCustomFieldValues().getFieldTextData());
									} else if (cf.getFieldType().equalsIgnoreCase("SINGLE CATEGORICAL")) {
										checkListAnnotation.put(cf.getCfName(),
												cf.getCustomFieldValues().getSingleCategoricalData().getValues());
									} else if (cf.getFieldType().equalsIgnoreCase("MULTIPLE CATEGORICAL")) {
										List<String> customFieldValue = cf.getCustomFieldValues()
												.getMultipleCategoricalData().stream().map((cVal) -> cVal.getValues())
												.collect(Collectors.toList());
										checkListAnnotation.put(cf.getCfName(),
												(customFieldValue.isEmpty() || customFieldValue == null) ? ""
														: String.join(",", customFieldValue));
									}

								}

							}
						}

					}

					ObservationDataTableShow data = new ObservationDataTableShow();
					data.setId(ob.getId());
					data.setChecklistAnnotation(checkListAnnotation);
					if (dataTable.getFieldMapping() != null) {
						String[] fieldMapping = dataTable.getFieldMapping().split(",");
						for (String field : fieldMapping) {
							if (field.contains(DataTableMappingField.SGROUP.getValue())) {
								data.setsGroup(ob.getGroupId());
							} else if (field.contains(DataTableMappingField.SCIENTIFICNAME.getValue())) {
								data.setScientificName(scientificName);
							} else if (field.contains(DataTableMappingField.COMMONNAME.getValue())) {
								data.setCommonName(commonName);
							} else if (field.contains(DataTableMappingField.USER.getValue())) {
								data.setUserInfo(userInfo);
							} else if (field.contains(DataTableMappingField.FROMDATE.getValue())) {
								data.setFromDate(fromDate);
							} 	else if (field.contains(DataTableMappingField.TODATE.getValue())) {
								data.setToDate(toDate);
							} 
							else if (field.contains(DataTableMappingField.OBSERVEDAT.getValue())) {
								data.setObservedAt(ob.getPlaceName());
							} else if (field.contains(DataTableMappingField.LOCATIONSCALE.getValue())) {
								data.setLocationScale(ob.getLocationScale());
							} else if (field.contains(DataTableMappingField.LONGITUDE.getValue())) {
								data.setLongitude(ob.getLongitude());
							} else if (field.contains(DataTableMappingField.LATITUDE.getValue())) {
								data.setLatitude(ob.getLatitude());
							} else if (field.contains(DataTableMappingField.DATEACCURACY.getValue())) {
								data.setDateAccuracy(ob.getDateAccuracy());
							} else if (field.contains(DataTableMappingField.NOTES.getValue())) {
								data.setNotes(ob.getNotes());
							} else if (field.contains(DataTableMappingField.GEOPRIVACY.getValue())) {
								data.setGeoPrivacy(ob.getGeoPrivacy());
							}
						}
					}

					showDataList.add(data);
				} catch (Exception e) {
					logger.error(e.getMessage());
				}

			});

		} catch (Exception ex) {
			logger.error(ex.getMessage());
		}
		return new ObservationDatatableList(showDataList, total);
	}

	@Override
	public String removeObservationByDataTableId(HttpServletRequest request, Long dataTableId) {
		List<Observation> observList = null;
		List<Long> list = new ArrayList<Long>();
		list.add(dataTableId);
		try {
			dataTableService = headers.addDataTableHeaders(dataTableService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			dataTableService.deleteDataTable(dataTableId.toString());
			observList = observationDao.fetchByDataTableId(list, null, 0);
			if (observList != null && !observList.isEmpty()) {
				ObservationDeleteThread deleteThread = new ObservationDeleteThread(observList, observationImpl,
						request);
				Thread thread = new Thread(deleteThread);
				thread.start();
			}
			return "Successfully Remove Observation for the Id" + dataTableId.toString();
		} catch (Exception error) {
			logger.error(error.getMessage());
		}

		return null;
	}

}