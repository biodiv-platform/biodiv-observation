package com.strandls.observation.util;

public enum DataTableMappingField {

	SGROUP("sGroup"), CHECKLISTANNOTATION("checklistAnnotation"), FROMDATE("fromDate"), GEOPRIVACY("geoPrivacy"),
	TODATE("toDate"), OBSERVEDAT("observedAt"), USER("user"), LOCATIONSCALE("locationScale"),LATITUDE("latitude"), LONGITUDE("longitude"),
	DATEACCURACY("dateAccuracy"), NOTES("notes"), fileName("fileName"), COMMONNAME("commonName"), COMMENT("comment"),
	SCIENTIFICNAME("scientificName"), TAGS("tags"), LICENSE("license"), USERGROUPS("userGroups");

	private String field;

	private DataTableMappingField(String field) {
		this.field = field;
	}

	public String getValue() {
		return field;
	}

}
