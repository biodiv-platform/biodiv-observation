package com.strandls.observation.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.es.util.ESBulkUploadThread;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.es.util.ObservationUtilityFunctions;
import com.strandls.observation.pojo.DataTable;
import com.strandls.observation.pojo.ObservationBulkData;
import com.strandls.observation.service.Impl.ObservationBulkMapperHelper;
import com.strandls.resource.pojo.License;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;

public class ObservationBulkUploadThread implements Runnable {

	private final ObservationBulkDTO observationBulkData;
	private final HttpServletRequest request;
	private final ObservationDAO observationDao;
	private final ObservationBulkMapperHelper observationBulkMapperHelper;
	private final ESUpdate esUpdate;
	private final UserServiceApi userService;
	private final DataTable dataTable;
	private final Long userId;
	private final List<SpeciesGroup> speciesGroupList;
	private final List<TraitsValuePair> traitsList;
	private final String requestAuthHeader;
	private final List<UserGroupIbp> userGroupIbpList;
	private final List<License> licenseList;
	private final XSSFWorkbook workbook;
	private final Map<String, String> myImageUpload;

	public ObservationBulkUploadThread(ObservationBulkDTO observationBulkData, HttpServletRequest request,
			ObservationDAO observationDao, ObservationBulkMapperHelper observationBulkMapperHelper, ESUpdate esUpdate,
			UserServiceApi userService, DataTable dataTable, Long userId, List<SpeciesGroup> speciesGroupList, List<TraitsValuePair> traitsList,
			List<UserGroupIbp> userGroupIbpList, List<License> licenseList, XSSFWorkbook workbook,
			Map<String, String> myImageUpload) {
		super();
		this.observationBulkData = observationBulkData;
		this.observationDao = observationDao;
		this.observationBulkMapperHelper = observationBulkMapperHelper;
		this.esUpdate = esUpdate;
		this.userService  = userService;
		this.dataTable = dataTable;
		this.userId = userId;
		this.request = request;
		this.speciesGroupList = speciesGroupList;
		this.traitsList = traitsList;
		this.userGroupIbpList = userGroupIbpList;
		this.licenseList = licenseList;
		this.requestAuthHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		this.workbook = workbook;
		this.myImageUpload = myImageUpload;
	}

	public void run() {

		XSSFSheet sheet = workbook.getSheetAt(0);
		Iterator<Row> rows = sheet.iterator();
		List<Long> observationIds = new ArrayList<Long>();
		Row dataRow;
		// skip header
		rows.next();

		while (rows.hasNext()) {
			dataRow = rows.next();
			if (true) {
				ObservationUtilityFunctions obUtil = new ObservationUtilityFunctions();
				ObservationBulkData data = new ObservationBulkData(observationBulkData.getColumns(), dataRow, request,
						dataTable, speciesGroupList, traitsList, userGroupIbpList, licenseList);

				Long obsId = obUtil.createObservationAndMappings(requestAuthHeader, observationBulkMapperHelper,
						observationDao, userService,data, myImageUpload, userId);
				observationIds.add(obsId);
				if (observationIds.size() >= 100) {
					esUpdate.esBulkUpload(observationIds);
					observationIds.clear();
				}
			}

		}

		if (!rows.hasNext() && !observationIds.isEmpty()) {
			ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationIds);
			Thread thread = new Thread(updateThread);
			thread.start();
		}

	}

}
