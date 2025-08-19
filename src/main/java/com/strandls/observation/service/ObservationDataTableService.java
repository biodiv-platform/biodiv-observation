package com.strandls.observation.service;

import java.util.List;
import java.util.concurrent.ExecutionException;

import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.pojo.ObservationDatatableList;
import com.strandls.observation.pojo.ShowObervationDataTable;

import jakarta.servlet.http.HttpServletRequest;

public interface ObservationDataTableService {

	public Long observationBulkUpload(HttpServletRequest request, ObservationBulkDTO observationBulkData)
			throws InterruptedException, ExecutionException;

	public ShowObervationDataTable showObservatioDataTable(HttpServletRequest request, Long dataTableId, Integer limit,
			Integer offset);

	public ObservationDatatableList fetchAllObservationByDataTableId(Long dataTableId, Integer limit, Integer offset);

	public String removeObservationByDataTableId(HttpServletRequest request, Long dataTableId);

	public List<com.strandls.dataTable.pojo.UserGroupIbp> updateDatatableUsergroup(HttpServletRequest request,
			Long dataTableId, List<Long> userGroupList, String bulkAction);

}
