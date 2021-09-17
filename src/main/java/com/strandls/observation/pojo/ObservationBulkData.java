package com.strandls.observation.pojo;

import com.strandls.resource.pojo.License;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.dataTable.pojo.DataTableWkt;

import org.apache.poi.ss.usermodel.Row;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;

public class ObservationBulkData {
	private final Map<String, Integer> fieldMapping;
	private final Map<String, Integer> checklistAnnotaion;
	private final Row dataRow;
	private final HttpServletRequest request;
	private final DataTableWkt dataTable;
	private final String contributors;
	private final List<SpeciesGroup> speciesGroupList;
	private final List<TraitsValuePair> pairs;
	private final List<UserGroupIbp> userGroupsList;
	private final List<License> licenses;
	private final Boolean isVerified;
	private final String basisOfRecord;

	public ObservationBulkData(Map<String, Integer> fieldMapping, Row dataRow, HttpServletRequest request,
			DataTableWkt dataTable, List<SpeciesGroup> speciesGroupList, List<TraitsValuePair> pairs,
			List<UserGroupIbp> userGroupsList, List<License> licenses, Boolean isVerified,
			Map<String, Integer> checklistAnnotaion, String contributors, String basisOfRecord) {
		this.fieldMapping = fieldMapping;
		this.checklistAnnotaion = checklistAnnotaion;
		this.dataRow = dataRow;
		this.request = request;
		this.dataTable = dataTable;
		this.isVerified = isVerified;
		this.speciesGroupList = speciesGroupList;
		this.pairs = pairs;
		this.userGroupsList = userGroupsList;
		this.licenses = licenses;
		this.contributors = contributors;
		this.basisOfRecord = basisOfRecord;
	}

	public Map<String, Integer> getFieldMapping() {
		return fieldMapping;
	}

	public Row getDataRow() {
		return dataRow;
	}

	public HttpServletRequest getRequest() {
		return request;
	}

	public List<SpeciesGroup> getSpeciesGroupList() {
		return speciesGroupList;
	}

	public List<TraitsValuePair> getPairs() {
		return pairs;
	}

	public List<UserGroupIbp> getUserGroupsList() {
		return userGroupsList;
	}

	public List<License> getLicenses() {
		return licenses;
	}

	public DataTableWkt getDataTable() {
		return dataTable;
	}

	public Boolean getIsVerified() {
		return isVerified;
	}

	public Map<String, Integer> getChecklistAnnotaion() {
		return checklistAnnotaion;
	}

	public String getBasisOfRecord() {
		return basisOfRecord;
	}

	public String getContributors() {
		return contributors;
	}
}
