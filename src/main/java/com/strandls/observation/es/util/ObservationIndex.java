/**
 * 
 */
package com.strandls.observation.es.util;

/**
 * @author Abhishek Rudra
 *
 */
public enum ObservationIndex {

	INDEX("extended_observation"), TYPE("_doc"), SGROUP("group_id"), USERGROUPID("user_group_observations.id"),
	SPECIESNAMES("group_name"), SCIENTIFICNAME("max_voted_reco.scientific_name"),
	COMMONNAME("max_voted_reco.common_names"), FLAGCOUNT("flag_count"), ISLOCKED("is_locked"), AUTHORID("author_id"),
	FROMDATE("from_date"), CREATEDON("created_on"), OBSERVATIONMONTH("observed_in_month"), ISCHECKLIST("is_checklist"),
	PATH("max_voted_reco.hierarchy.taxon_id"), TRAITSAGGREGATION("facts.trait_value.trait_aggregation.raw"),
	STATUS("max_voted_reco.taxonstatus"), NOOFIDENTIFICATION("no_of_identifications"),
	TAHSIL("location_information.tahsil.raw"), STATE("location_information.state.raw"),
	DISTRICT("location_information.district.raw"), RANK("max_voted_reco.rank"),
	CUSTOMFIELDID("custom_fields.custom_field.custom_field_id"),
	CUSTOMFIELDAGGREGATION("custom_fields.custom_field.custom_field_values.custom_field_aggregation.raw"),
	CUSTOMFIELDIDVALUE("custom_fields.custom_field.custom_field_values.cfid_value"),
	CUSTOMFIELDRANGEMINVALUE("custom_fields.custom_field.custom_field_values.min_range"),
	CUSTOMFIELDRANGEMAXVALUE("custom_fields.custom_field.custom_field_values.max_range"), TAGS("tags.name"),
	RESOURCE("observation_resource.file_name.keyword"), MAXVTEDRECO("max_voted_reco.id"),
	AUTHORVOTED("all_reco_vote.authors_voted.id"), GEOENTITY("location_information.name.raw"),
	RECOID("all_reco_vote.recommendation_id"), PUBLICATIONGRADE("is_publication_grade"), DATASETNAME("dataset_title"),
	DATATABLENAME("data_table_title"), DATATABLEID("data_table_id");

	private String field;

	private ObservationIndex(String field) {
		this.field = field;
	}

	public String getValue() {
		return field;
	}

}
