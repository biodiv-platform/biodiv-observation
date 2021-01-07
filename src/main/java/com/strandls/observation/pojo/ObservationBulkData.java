package com.strandls.observation.pojo;

import com.strandls.resource.pojo.License;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.userGroup.pojo.UserGroupIbp;
import org.apache.poi.ss.usermodel.Row;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;

public class ObservationBulkData {
    private final Map<String, Integer> fieldMapping;
    private final Row dataRow;
    private final HttpServletRequest request;
    private final DataTable dataTable;
    private final List<SpeciesGroup> speciesGroupList;
    private final List<TraitsValuePair> pairs;
    private final List<UserGroupIbp> userGroupsList;
    private final List<License> licenses;

    public ObservationBulkData(Map<String, Integer> fieldMapping, Row dataRow, HttpServletRequest request,
                               DataTable dataTable, List<SpeciesGroup> speciesGroupList, List<TraitsValuePair> pairs,
                               List<UserGroupIbp> userGroupsList, List<License> licenses) {
        this.fieldMapping = fieldMapping;
        this.dataRow = dataRow;
        this.request = request;
        this.dataTable = dataTable;
        this.speciesGroupList = speciesGroupList;
        this.pairs = pairs;
        this.userGroupsList = userGroupsList;
        this.licenses = licenses;
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

    public DataTable getDataTable() {
        return dataTable;
    }
}
