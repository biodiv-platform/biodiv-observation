/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.userGroup.pojo.CustomFieldDetails;
import com.strandls.userGroup.pojo.UserGroupSpeciesGroup;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationUGContextCreatePageData {

	private List<UserGroupSpeciesGroup> userGroupSGroup;
	private List<CustomFieldDetails> customField;

	/**
	 * 
	 */
	public ObservationUGContextCreatePageData() {
		super();
	}

	/**
	 * @param userGroupSGroup
	 * @param customField
	 */
	public ObservationUGContextCreatePageData(List<UserGroupSpeciesGroup> userGroupSGroup,
			List<CustomFieldDetails> customField) {
		super();
		this.userGroupSGroup = userGroupSGroup;
		this.customField = customField;
	}

	public List<UserGroupSpeciesGroup> getUserGroupSGroup() {
		return userGroupSGroup;
	}

	public void setUserGroupSGroup(List<UserGroupSpeciesGroup> userGroupSGroup) {
		this.userGroupSGroup = userGroupSGroup;
	}

	public List<CustomFieldDetails> getCustomField() {
		return customField;
	}

	public void setCustomField(List<CustomFieldDetails> customField) {
		this.customField = customField;
	}

}
