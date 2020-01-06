/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.userGroup.pojo.UserGroupIbp;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationUserPermission {

	List<Long> validatePermissionTaxon;
	List<UserGroupIbp> userGroupMember;
	List<UserGroupIbp> userGroupFeature;

	/**
	 * 
	 */
	public ObservationUserPermission() {
		super();
	}

	/**
	 * @param validatePermissionTaxon
	 * @param userGroupMember
	 * @param userGroupFeature
	 */
	public ObservationUserPermission(List<Long> validatePermissionTaxon, List<UserGroupIbp> userGroupMember,
			List<UserGroupIbp> userGroupFeature) {
		super();
		this.validatePermissionTaxon = validatePermissionTaxon;
		this.userGroupMember = userGroupMember;
		this.userGroupFeature = userGroupFeature;
	}

	public List<Long> getValidatePermissionTaxon() {
		return validatePermissionTaxon;
	}

	public void setValidatePermissionTaxon(List<Long> validatePermissionTaxon) {
		this.validatePermissionTaxon = validatePermissionTaxon;
	}

	public List<UserGroupIbp> getUserGroupMember() {
		return userGroupMember;
	}

	public void setUserGroupMember(List<UserGroupIbp> userGroupMember) {
		this.userGroupMember = userGroupMember;
	}

	public List<UserGroupIbp> getUserGroupFeature() {
		return userGroupFeature;
	}

	public void setUserGroupFeature(List<UserGroupIbp> userGroupFeature) {
		this.userGroupFeature = userGroupFeature;
	}

}
