/**
 * 
 */
package com.strandls.observation.es.util;

/**
 * @author Abhishek Rudra
 *
 */
public enum ObservationIndex {

	index("extended_observation"), type("extended_records"), sGroup("group_id"),
	userGroupId("user_group_observations.id"), speciesNames("group_name"),
	scientificName("max_voted_reco.scientific_name"), commonName("max_voted_reco.common_names"),
	flagCount("flag_count"), isLocked("is_locked"), authorid("author_id"), fromDate("from_date"),
	createdOn("created_on"), ObservedOnMonth("observed_in_month"), isChecklist("is_checklist"),
	path("max_voted_reco.hierarchy.taxon_id"), traitId("facts.trait_id"), traitValue("facts.trait_value.value"),
	status("max_voted_reco.taxonstatus"), no_of_identifications("no_of_identifications"),
	tahsil("location_information.tahsil"), state("location_information.state"),
	district("location_information.district"), rank("max_voted_reco.ranktext"),
	customFieldId("custom_fields.custom_field.custom_field_id"),
	customFieldTextValue("custom_fields.custom_field.custom_field_values.field_text_data"),
	customFieldAggregation("custom_fields.custom_field.custom_field_values.custom_field_aggregation"),
	customFieldSingleCategoricalValue("custom_fields.custom_field.custom_field_values.single_categorical_data"),
	customFieldMultipleCategoricalValue("custom_fields.custom_field.custom_field_values.multiple_categorical_data"),
	customFieldRangeMinValue("custom_fields.custom_field.custom_field_values.min_range"),
	customFieldRangeMaxValue("custom_fields.custom_field.custom_field_values.max_range"), tags("tags.name"),
	resource("observation_resource.file_name.keyword");

	private String field;

	private ObservationIndex(String field) {
		this.field = field;
	}

	public String getValue() {
		return field;
	}

}
