package com.strandls.observation.es.util;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationListCsvHeaders {
	
	//Core 
	private Long observationId;
	private String placeName;
	private String flagNotes;
	private String noOfIdentifications;
	private String geoPrivacy;
	private String createdOn;
	
	private String reprImageUrl;
	private String noOfImages;
	private String reverseGeocodedName;
	private String group_id;
	private String dateAccuracy;
	private String isLocked;
	private String fromDate;
	private String createdBy;
	private String locationLat;
	private String locationLon;
	private String locationScale;
	private String toDate;
	
	// maxvoted_rank
	private String rank;
	
	
	private String taxonStatus;
	// maxvoted common name
	private String commonName;
	
	// maxvoted scientific name
	private String scientificName;
	// maxvoted heirarchy
	private String kingdom;
	private String phylum;
	private String _class;
	private String order;
	private String family;
	private String genus;
	private String species;
	
	// optional
	private String datasetTitle;
	private String observedInMonth;
	private String lastRevised;
	
	private String allRecoVoteDetails;
	private String allRecoVoteCommonNames;
	
	private String noMedia;
	private String maxVotedRecoSpeciesId;
	private String profile_pic;
	
	private String flagCount;
	private String notes;
	private String checklistAnnotations;
	
	private String tags;
	private String state;
	private String tahsil;
	private String district;
	private String groupName;
	

}
