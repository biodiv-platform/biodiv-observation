/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.userGroup.pojo.CustomFieldPermission;

/**
 * @author Abhishek Rudra
 *
 */
public class ListPagePermissions {

	private List<Long> validatePermissionTaxon;
	private List<CustomFieldPermission> cfPermission;

	/**
	 * 
	 */
	public ListPagePermissions() {
		super();
	}

	/**
	 * @param validatePermissionTaxon
	 * @param cfPermission
	 */
	public ListPagePermissions(List<Long> validatePermissionTaxon, List<CustomFieldPermission> cfPermission) {
		super();
		this.validatePermissionTaxon = validatePermissionTaxon;
		this.cfPermission = cfPermission;
	}

	public List<Long> getValidatePermissionTaxon() {
		return validatePermissionTaxon;
	}

	public void setValidatePermissionTaxon(List<Long> validatePermissionTaxon) {
		this.validatePermissionTaxon = validatePermissionTaxon;
	}

	public List<CustomFieldPermission> getCfPermission() {
		return cfPermission;
	}

	public void setCfPermission(List<CustomFieldPermission> cfPermission) {
		this.cfPermission = cfPermission;
	}

}
