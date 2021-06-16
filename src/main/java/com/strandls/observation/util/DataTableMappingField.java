package com.strandls.observation.util;

public enum DataTableMappingField {

	sGroup("sGroup"), checklistAnnotation("checklistAnnotation"), fromDate("fromDate"), geoPrivacy("geoPrivacy"),
	toDate("toDate"), observedAt("observedAt"), user("user"), locationScale("locationScale"),latitude("latitude"), longitude("longitude"),
	dateAccuracy("dateAccuracy"), notes("notes"), fileName("fileName"), commonName("commonName"), comment("comment"),
	scientificName("scientificName"), tags("tags"), license("license"), userGroups("userGroups");

	private String field;

	private DataTableMappingField(String field) {
		this.field = field;
	}

	public String getValue() {
		return field;
	}

}
