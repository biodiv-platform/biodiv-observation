package com.strandls.observation.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.file.api.UploadApi;
import com.strandls.file.model.FilesDTO;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.es.util.ESBulkUploadThread;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.es.util.ObservationUtilityFunctions;
import com.strandls.dataTable.controllers.DataTableServiceApi;
import com.strandls.dataTable.pojo.DataTableWkt;
import com.strandls.observation.pojo.ObservationBulkData;
import com.strandls.observation.service.Impl.ObservationBulkMapperHelper;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.License;
import com.strandls.resource.pojo.UFile;
import com.strandls.resource.pojo.UFileCreateData;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;

public class ObservationBulkUploadThread implements Runnable {
	private final Logger logger = LoggerFactory.getLogger(ObservationBulkUploadThread.class);
	private final ObservationBulkDTO observationBulkData;
	private final HttpServletRequest request;
	private final ObservationDAO observationDao;
	private ResourceServicesApi resourceService;
	private UploadApi fileUploadApi;
	private DataTableServiceApi dataTableService;
	private final Headers headers;
	private final ObservationBulkMapperHelper observationBulkMapperHelper;
	private final ESUpdate esUpdate;
	private final UserServiceApi userService;
	private final DataTableWkt dataTable;
	private final Long userId;
	private final List<SpeciesGroup> speciesGroupList;
	private final List<TraitsValuePair> traitsList;
	private final String requestAuthHeader;
	private final List<UserGroupIbp> userGroupIbpList;
	private final List<License> licenseList;
	private final XSSFWorkbook workbook;
	private final Map<String, String> myImageUpload;
	private final TokenGenerator tokenGenerator;

	public ObservationBulkUploadThread(ObservationBulkDTO observationBulkData, HttpServletRequest request,
			ObservationDAO observationDao, ObservationBulkMapperHelper observationBulkMapperHelper, ESUpdate esUpdate,
			UserServiceApi userService, DataTableWkt dataTable, Long userId, List<SpeciesGroup> speciesGroupList,
			List<TraitsValuePair> traitsList, List<UserGroupIbp> userGroupIbpList, List<License> licenseList,
			XSSFWorkbook workbook, Map<String, String> myImageUpload, ResourceServicesApi resourceService,
			UploadApi fileUploadApi, DataTableServiceApi dataTableService,TokenGenerator tokenGenerator, Headers headers) {
		super();
		this.observationBulkData = observationBulkData;
		this.observationDao = observationDao;
		this.observationBulkMapperHelper = observationBulkMapperHelper;
		this.resourceService = resourceService;
		this.fileUploadApi = fileUploadApi;
		this.dataTableService = dataTableService;
		this.headers = headers;
		this.esUpdate = esUpdate;
		this.userService = userService;
		this.dataTable = dataTable;
		this.userId = userId;
		this.request = request;
		this.speciesGroupList = speciesGroupList;
		this.traitsList = traitsList;
		this.userGroupIbpList = userGroupIbpList;
		this.licenseList = licenseList;
		this.requestAuthHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		this.workbook = workbook;
		this.tokenGenerator = tokenGenerator;
		this.myImageUpload = myImageUpload;
	}

	public void run() {

		try {
			XSSFSheet sheet = workbook.getSheetAt(0);
			Iterator<Row> rows = sheet.iterator();
			List<Long> observationIds = new ArrayList<Long>();
			fileUploadApi = headers.addFileUploadHeader(fileUploadApi, requestAuthHeader);
			resourceService = headers.addResourceHeaders(resourceService, requestAuthHeader);
			Row dataRow;
			// skip header
			rows.next();

			while (rows.hasNext()) {
				dataRow = rows.next();
				if (isEmptyRow(dataRow)) {
					break;
				}

				ObservationUtilityFunctions obUtil = new ObservationUtilityFunctions();
				ObservationBulkData data = new ObservationBulkData(observationBulkData.getColumns(), dataRow, request,
						dataTable, speciesGroupList, traitsList, userGroupIbpList, licenseList,
						observationBulkData.getIsVerified(), observationBulkData.getChecklistAnnotation(),
						observationBulkData.getBasisOfData());

				Long obsId = obUtil.createObservationAndMappings(requestAuthHeader, observationBulkMapperHelper,
						observationDao, userService, data, myImageUpload,tokenGenerator, userId);
				if (obsId != null) {
					observationIds.add(obsId);
				}

				if (observationIds.size() >= 100) {
					String observationList = StringUtils.join(observationIds, ',');
					ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
					Thread thread = new Thread(updateThread);
					thread.start();
					observationIds.clear();
				}

			}

			if (!observationIds.isEmpty()) {
				String observationList = StringUtils.join(observationIds, ',');
				ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
				Thread thread = new Thread(updateThread);
				thread.start();
				try {
					Map<String, Object> sheetResult = moveSheet(observationBulkData, request);
					Long uFileId = Long.parseLong(sheetResult.get("uFileId").toString());
					dataTable.setUfileId(uFileId);
					dataTableService.updateDataTable(dataTable);
				} catch (Exception e) {
					logger.error(e.getMessage());
				}

			}
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

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
			throw new NullPointerException("Sheet Filename nout found");
		}
	}

	private boolean isEmptyRow(Row row) {
		boolean isEmptyRow = true;
		for (int cellNum = row.getFirstCellNum(); cellNum < row.getLastCellNum(); cellNum++) {
			Cell cell = row.getCell(cellNum);
			if (cell != null && cell.getCellType() != CellType.BLANK && StringUtils.isNotBlank(cell.toString())) {
				isEmptyRow = false;
			}
		}
		return isEmptyRow;
	}

}
