package com.strandls.observation.service.Impl;


import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.dataTable.pojo.BulkDTO;

public class DataTableHelper {

	
	public BulkDTO createDataTableBulkDTO(ObservationBulkDTO observationBulkData) {
		BulkDTO dataTableDto = new BulkDTO();
		dataTableDto.setLicenseId(observationBulkData.getLicenseId());
		dataTableDto.setCreatedOn(observationBulkData.getCreatedOn());
		dataTableDto.setDescription(observationBulkData.getDescription());
		dataTableDto.setMethods(observationBulkData.getMethods());
		dataTableDto.setAttribution(observationBulkData.getAttribution());
		dataTableDto.setContributors(observationBulkData.getContributors());

		// geo fields
		dataTableDto.setLatitude(observationBulkData.getLatitude());
		dataTableDto.setLongitude(observationBulkData.getLongitude());
		dataTableDto.setLocationAccuracy(observationBulkData.getLocationAccuracy());
		dataTableDto.setLocationScale(observationBulkData.getLocationScale());
		dataTableDto.setObservedAt(observationBulkData.getObservedAt());
		dataTableDto.setWktString(observationBulkData.getWktString());
		dataTableDto.setDataset(observationBulkData.getDataset());
		
		dataTableDto.setProject(observationBulkData.getProject());
		dataTableDto.setSummary(observationBulkData.getSummary());
		dataTableDto.setSpeciesGroup(observationBulkData.getSGroup());
		dataTableDto.setDateAccuracy(observationBulkData.getDateAccuracy());
		dataTableDto.setObservedFromDate(observationBulkData.getObservedFromDate());
		dataTableDto.setObservedToDate(observationBulkData.getObservedToDate());
		dataTableDto.setTitle(observationBulkData.getTitle());
		dataTableDto.setBasisOfData(observationBulkData.getBasisOfData());
		dataTableDto.setBasisOfRecord(observationBulkData.getBasisOfRecord());
		dataTableDto.setIsVerified(observationBulkData.getIsVerified());
		dataTableDto.setFieldMapping(String.join(",", observationBulkData.getColumns().keySet()));

	
		return dataTableDto;
	}
	
}
