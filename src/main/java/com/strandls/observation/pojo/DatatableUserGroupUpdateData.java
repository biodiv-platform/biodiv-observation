package com.strandls.observation.pojo;

import java.util.List;

public class DatatableUserGroupUpdateData {
	private List<Long> userGroupList;
	private String bulkAction;

	public List<Long> getUserGroupList() {
		return userGroupList;
	}

	public void setUserGroupList(List<Long> userGroupList) {
		this.userGroupList = userGroupList;
	}

	public String getBulkAction() {
		return bulkAction;
	}

	public void setBulkAction(String bulkAction) {
		this.bulkAction = bulkAction;
	}

}
