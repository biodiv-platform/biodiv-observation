/**
 * 
 */
package com.strandls.observation.es.util;

/**
 * @author Abhishek Rudra
 *
 */
public enum ObservationIndex {

	index("extended_observation"), type("_doc"), sGroup("group_id"),
	userGroupId("user_group_observations.id"), speciesNames("group_name"),
	scientificName("max_voted_reco.scientific_name"), commonName("max_voted_reco.common_names"),
	flagCount("flag_count"), isLocked("is_locked"), authorid("author_id"), fromDate("from_date"),
	createdOn("created_on"), ObservedOnMonth("observed_in_month"), isChecklist("is_checklist"),
	path("max_voted_reco.hierarchy.taxon_id"), traitsAggregation("facts.trait_value.trait_aggregation.raw"),
	status("max_voted_reco.taxonstatus"), no_of_identifications("no_of_identifications"),
	tahsil("location_information.tahsil.raw"), state("location_information.state.raw"),
	district("location_information.district.raw"), rank("max_voted_reco.rank"),
	customFieldId("custom_fields.custom_field.custom_field_id"),
	customFieldAggregation("custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw"),
	customFieldIdValue("custom_fields.custom_field.custom_field_values.cfid_value"),
	customFieldRangeMinValue("custom_fields.custom_field.custom_field_values.min_range"),
	customFieldRangeMaxValue("custom_fields.custom_field.custom_field_values.max_range"), tags("tags.name.raw"),
	resource("observation_resource.file_name.keyword"), maxVotedReco("max_voted_reco.id"),
	authorVoted("all_reco_vote.authors_voted.id"),
	recoId("all_reco_vote.recommendation_id"), publicationgrade("is_publication_grade");

	private String field;

	private ObservationIndex(String field) {
		this.field = field;
	}

	public String getValue() {
		return field;
	}

}
