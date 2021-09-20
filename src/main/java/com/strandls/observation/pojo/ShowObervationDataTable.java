package com.strandls.observation.pojo;

import java.util.List;
import java.util.Map;

import com.strandls.dataTable.pojo.DataTableWkt;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.pojo.UserGroupIbp;

/**
 * 
 * @author vishnu
 *
 */

public class ShowObervationDataTable {

	/**
	 * 
	 */
	private DataTableWkt datatable;
	private List<UserGroupIbp> userGroups;
	private ObservationLocationInfo layerInfo;
	private UserIbp authorInfo;
	private Map<String, String> authorScore;
	private List<ObservationDataTableShow> observationList;
	private Long count;

	/**
	 * 
	 * @param id
	 * @param datatable
	 * @param userGroups
	 * @param layerInfo
	 * @param authorInfo
	 * @param authorScore
	 */
	public ShowObervationDataTable(DataTableWkt datatable, List<UserGroupIbp> userGroups,
			ObservationLocationInfo layerInfo, UserIbp authorInfo, Map<String, String> authorScore, Long count) {
		super();
		this.datatable = datatable;
		this.userGroups = userGroups;
		this.layerInfo = layerInfo;
		this.authorInfo = authorInfo;
		this.authorScore = authorScore;
		this.count = count;
	}

	public ShowObervationDataTable() {
		super();
	}

	public DataTableWkt getDatatable() {
		return datatable;
	}

	public void setDatatable(DataTableWkt datatable) {
		this.datatable = datatable;
	}

	public List<UserGroupIbp> getUserGroups() {
		return userGroups;
	}

	public void setUserGroups(List<UserGroupIbp> userGroups) {
		this.userGroups = userGroups;
	}

	public ObservationLocationInfo getLayerInfo() {
		return layerInfo;
	}

	public void setLayerInfo(ObservationLocationInfo layerInfo) {
		this.layerInfo = layerInfo;
	}

	public UserIbp getAuthorInfo() {
		return authorInfo;
	}

	public void setAuthorInfo(UserIbp authorInfo) {
		this.authorInfo = authorInfo;
	}

	public Map<String, String> getAuthorScore() {
		return authorScore;
	}

	public void setAuthorScore(Map<String, String> authorScore) {
		this.authorScore = authorScore;
	}

	public List<ObservationDataTableShow> getObservationList() {
		return observationList;
	}

	public void setObservationList(List<ObservationDataTableShow> observationList) {
		this.observationList = observationList;
	}

	public Long getCount() {
		return count;
	}

	public void setCount(Long count) {
		this.count = count;
	}

}