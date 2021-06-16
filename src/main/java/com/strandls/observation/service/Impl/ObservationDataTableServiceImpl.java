package com.strandls.observation.service.Impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
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
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowObervationDataTable;
import com.strandls.observation.service.ObservationDataTableService;
import com.strandls.observation.util.DataTableMappingField;
import com.strandls.observation.util.ObservationBulkUploadThread;
import com.strandls.observation.util.ObservationDeleteThread;
import com.strandls.resource.controllers.LicenseControllerApi;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.License;
import com.strandls.resource.pojo.UFile;
import com.strandls.resource.pojo.UFileCreateData;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;

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
				throw new Exception("Sheet Filename nout found");
			}

			Map<String, Object> sheetResult = moveSheet(observationBulkData, request);

			String storageBasePath = properties.getProperty("storage_dir", "/apps/biodiv-image");
			String sheetDirectory = storageBasePath + File.separatorChar + "content/dataTables"
					+ sheetResult.get("destinationPath");

			BulkDTO dataTableDTO = dataTableHelper.createDataTableBulkDTO(observationBulkData);
			Long uFileId = Long.parseLong(sheetResult.get("uFileId").toString());
			dataTableDTO.setUserFileId(uFileId);

			dataTableService = headers.addDataTableHeaders(dataTableService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			DataTableWkt dataTable = dataTableService.createDataTable(dataTableDTO);

			if (dataTable == null) {
				throw new Exception("Unable to create DataTable, Unresolved Constrain");
			}
			XSSFWorkbook workbook = new XSSFWorkbook(new File(sheetDirectory));
			List<TraitsValuePair> traitsList = traitService.getAllTraits();
			List<UserGroupIbp> userGroupIbpList = userGroupService.getAllUserGroup();
			List<License> licenseList = licenseControllerApi.getAllLicenses();

			FilesDTO filesDto = new FilesDTO();
			filesDto.setFolder("observations");
			filesDto.setModule("observation");
			Map<String, String> myImageUpload = headers
					.addFileUploadHeader(fileUploadApi, request.getHeader(HttpHeaders.AUTHORIZATION))
					.getAllFilePathsByUser(filesDto).entrySet().stream()
					.collect(Collectors.toMap(Map.Entry::getKey, e -> (String) e.getValue()));

			ObservationBulkUploadThread uploadThread = new ObservationBulkUploadThread(observationBulkData, request,
					observationDao, observationBulkMapperHelper, esUpdate, userService, dataTable, userId,
					observationImpl.getAllSpeciesGroup(), traitsList, userGroupIbpList, licenseList, workbook,
					myImageUpload, resourceService, fileUploadApi, headers);
			Thread thread = new Thread(uploadThread);
			thread.start();

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
		List<UserGroupIbp> userGroups = null;
		Long userId = null;
		ObservationLocationInfo locationInfo = null;
		List<ObservationDataTableShow> observationList = null;
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
			UserScore score = esService.getUserScore("eaf", "er", userId.toString(), "f");
			locationInfo = layerService.getLayerInfo(dataTable.getGeographicalCoverageLatitude().toString(),
					dataTable.getGeographicalCoverageLongitude().toString());
			dataTableRes.setAuthorInfo(user);
			dataTableRes.setLayerInfo(null);
			dataTableRes.setObservationList(observationList);
			dataTableRes.setLayerInfo(locationInfo);
			dataTableRes.setUserGroups(userGroups);
			dataTableRes.setDatatable(dataTable);
			if (score.getRecord() != null) {
				authorScore = score.getRecord().get(0).get("details");
				dataTableRes.setAuthorScore(authorScore);
			}
			return dataTableRes;
		} catch (Exception er) {
			logger.error(er.getMessage());
		}

		return null;
	}

	private Map<String, Object> moveSheet(ObservationBulkDTO observationBulkData, HttpServletRequest request)
			throws Exception {
		try {
			List<String> myUploadFilesPath = new ArrayList<String>();
			myUploadFilesPath.add(observationBulkData.getFilename());
			UFile uFileData = null;
			FilesDTO filesDataTable = new FilesDTO();
			Map<String, Object> result = new HashMap<String, Object>();
			filesDataTable.setFolder("datatables");
			filesDataTable.setModule("DATASETS");
			filesDataTable.setFiles(myUploadFilesPath);
			Map<String, Object> fileRes;
			fileUploadApi = headers.addFileUploadHeader(fileUploadApi, request.getHeader(HttpHeaders.AUTHORIZATION));
			resourceService = headers.addResourceHeaders(resourceService, request.getHeader(HttpHeaders.AUTHORIZATION));

			fileRes = fileUploadApi.moveFiles(filesDataTable);
			List<UFileCreateData> createUfileList = new ArrayList<>();
			fileRes.entrySet().forEach((item) -> {
				@SuppressWarnings("unchecked")
				Map<String, String> values = (Map<String, String>) item.getValue();
				UFileCreateData createUFileData = new UFileCreateData();
				createUFileData.setWeight(0);
				createUFileData.setSize(values.get("size"));
				createUFileData.setPath(values.get("name"));
				createUfileList.add(createUFileData);

			});

			uFileData = resourceService.createUFile(createUfileList.get(0));
			result.put("uFileId", uFileData.getId());
			result.put("destinationPath", createUfileList.get(0).getPath());
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
			throw new Exception("Sheet Filename nout found");
		}
	}

	@Override
	public List<ObservationDataTableShow> fetchAllObservationByDataTableId(Long dataTableId, Integer limit,
			Integer offset) {
		List<Observation> observationList = null;
		DataTableWkt dataTable;
		List<Long> list = new ArrayList<Long>();
		List<ObservationDataTableShow> showDataList = new ArrayList<ObservationDataTableShow>();
		list.add(dataTableId);
		try {
			observationList = observationDao.fetchByDataTableId(list, limit, offset);
			dataTable = dataTableService.showDataTable(dataTableId.toString());
			if (observationList.isEmpty()) {
				return showDataList;
			}

			observationList.forEach((ob) -> {
				Map<String, Object> checkListAnnotation = new HashMap<String, Object>();
				RecoIbp reco = null;
				UserIbp userInfo = null;
				String commonName = null;
				String scientificName = null;
				String fromDate = null;
				if (ob.getFromDate() != null) {
					SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
					fromDate = dateFormat.format(ob.getFromDate());
				}

				try {
					if (ob.getMaxVotedRecoId() != null) {
						reco = recoService.fetchRecoName(ob.getId(), ob.getMaxVotedRecoId());
						scientificName = reco.getScientificName() != null ? reco.getScientificName() : null;
						commonName = reco.getCommonName() != null ? reco.getCommonName() : null;
					}

					if (ob.getAuthorId() != null) {
						userInfo = userService.getUserIbp(ob.getAuthorId().toString());
					}
					checkListAnnotation = ob.getChecklistAnnotations() != null
							? om.readValue(ob.getChecklistAnnotations(), new TypeReference<Map<String, Object>>() {
							})
							: null;
				} catch (Exception e) {
					logger.error(e.getMessage());
				}
				String[] fieldMapping = dataTable.getFieldMapping().split(",");
				ObservationDataTableShow data = new ObservationDataTableShow();
				data.setId(ob.getId());
				data.setScientificName(scientificName);
				data.setCommonName(commonName);
				data.setUserInfo(userInfo);
				data.setChecklistAnnotation(checkListAnnotation);
				for (String field : fieldMapping) {
					if (field == DataTableMappingField.sGroup.getValue()) {
						data.setsGroup(ob.getGroupId());
					} else if (field == DataTableMappingField.fromDate.getValue()) {
						data.setFromDate(fromDate);
					} else if (field == DataTableMappingField.observedAt.getValue()) {
						data.setObservedAt(ob.getPlaceName());
					} else if (field == DataTableMappingField.locationScale.getValue()) {
						data.setLocationScale(ob.getLocationScale());
					} else if (field == DataTableMappingField.longitude.getValue()) {
						data.setLongitude(ob.getLongitude());
					} else if (field == DataTableMappingField.latitude.getValue()) {
						data.setLatitude(ob.getLatitude());
					} else if (field == DataTableMappingField.dateAccuracy.getValue()) {
						data.setDateAccuracy(ob.getDateAccuracy());
					} else if (field == DataTableMappingField.notes.getValue()) {
						data.setNotes(ob.getNotes());
					} else if (field == DataTableMappingField.geoPrivacy.getValue()) {
						data.setGeoPrivacy(ob.getGeoPrivacy());
					} 
				}
				showDataList.add(data);

			});
			return showDataList;
		} catch (Exception ex) {
			logger.error(ex.getMessage());
		}
		return null;
	}

	@Override
	public String removeObservationByDataTableId(HttpServletRequest request, Long dataTableId) {
		List<Observation> observList = null;
		List<Long> list = new ArrayList<Long>();
		list.add(dataTableId);
		try {
			dataTableService.deleteDataTable(dataTableId.toString());
			observList = observationDao.fetchByDataTableId(list, null, 0);
			if (observList != null && observList.size() > 0) {
				ObservationDeleteThread deleteThread = new ObservationDeleteThread(observList, observationImpl,
						request);
				Thread thread = new Thread(deleteThread);
				thread.start();
				return "Successfully Remove Observation for the Id" + dataTableId.toString();
			}
		} catch (Exception error) {
			logger.error(error.getMessage());
		}

		return null;
	}

}