package com.strandls.observation.service;

import java.util.List;
import java.util.concurrent.ExecutionException;

import javax.servlet.http.HttpServletRequest;

import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ShowObervationDataTable;

public interface ObservationDataTableService  {

	
	public Long observationBulkUpload(HttpServletRequest request, ObservationBulkDTO observationBulkData) throws InterruptedException, ExecutionException;
	
	public ShowObervationDataTable showObservatioDataTable(HttpServletRequest request,Long dataTableId,Integer limit, Integer offset);
	
	public List<Observation> fetchAllObservationByDataTableId(Long dataTableId,Integer limit,Integer offset);
	
	public String removeObservationByDataTableId(HttpServletRequest request,Long dataTableId);
	
}
