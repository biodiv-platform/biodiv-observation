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

	private List<Long> validatePermissionTaxon;
	private List<UserGroupIbp> userGroupMember;
	private List<UserGroupIbp> userGroupFeature;
	private Boolean following;

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
	 * @param following
	 */
	public ObservationUserPermission(List<Long> validatePermissionTaxon, List<UserGroupIbp> userGroupMember,
			List<UserGroupIbp> userGroupFeature, Boolean following) {
		super();
		this.validatePermissionTaxon = validatePermissionTaxon;
		this.userGroupMember = userGroupMember;
		this.userGroupFeature = userGroupFeature;
		this.following = following;
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

	public Boolean getFollowing() {
		return following;
	}

	public void setFollowing(Boolean following) {
		this.following = following;
	}

}
