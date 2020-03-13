/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

import com.strandls.userGroup.pojo.CustomFieldFactsInsert;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationCreateUGContext {

	private ObservationCreate observationData;
	private List<CustomFieldFactsInsert> customFieldData;

	/**
	 * 
	 */
	public ObservationCreateUGContext() {
		super();
	}

	/**
	 * @param observationData
	 * @param customFieldData
	 */
	public ObservationCreateUGContext(ObservationCreate observationData, List<CustomFieldFactsInsert> customFieldData) {
		super();
		this.observationData = observationData;
		this.customFieldData = customFieldData;
	}

	public ObservationCreate getObservationData() {
		return observationData;
	}

	public void setObservationData(ObservationCreate observationData) {
		this.observationData = observationData;
	}

	public List<CustomFieldFactsInsert> getCustomFieldData() {
		return customFieldData;
	}

	public void setCustomFieldData(List<CustomFieldFactsInsert> customFieldData) {
		this.customFieldData = customFieldData;
	}

}
