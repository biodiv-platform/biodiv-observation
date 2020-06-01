package com.strandls.observation.util;

public enum DownloadLogHeader {
	
	// Core Section
	observationId("observationId"),
	createdBy("recordedBy"),
	placeName("verbatimLocality"),
	flagNotes("flagReason"),
	noOfIdentifications("noOfIdentifications"),
	geoPrivacy("geoPrivacy"),
	createdOn("createdOn"),
	reprImageUrl("associatedMedia"),
	group_id("groupId"),
	dateAccuracy("dateAccuracy"),
	isLocked("identificationVerificationStatus"),
	locationLat("decimalLatitude"),
	locationLon("decimalLongitude"),
	fromDate("eventDate"),
// max_voted	
	rank("taxonRank"),
	scientificName("scientificName"),
	commonName("vernacularName"),
	//hierarchy
	kingdom("kingdom"),
	phylum("phylum"),
	_class("class"),
	order("order"),
	superFamily("superfamily"),
	family("family"),
	genus("genus"),
	species("species"),
	
// temporal section
	toDate("toDate"),
	observedInMonth("observedInMonth"),
	lastRevised("lastRevised"),
	
// Traits Section	
	// all trait name
	
// CustomField Section
	//all custom fields name

// Spatial
	state("state"),
	tahsil("tahsil"),
	district("distrcit");
	
//	"locationScale",
//	"noOfImages","noOfVideos","noOfAudio",
//,
	
	private String headerValue;
	
	private DownloadLogHeader(String value) {
		this.headerValue = value;
	}
	
	public String getHeader() {
		return this.headerValue;
	}
}
