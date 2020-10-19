package com.strandls.observation.pojo;

import java.util.List;

public class ObservationUserPageInfo {

	private List<UniqueSpeciesInfo> observationUploaded;
	private Long observationUploadedCount;
	private List<UniqueSpeciesInfo> identifiedSpecies;
	private Long identifiedSpeciesCount;

	public ObservationUserPageInfo() {
		super();
	}

	public ObservationUserPageInfo(List<UniqueSpeciesInfo> observationUploaded, Long observationUploadedCount,
			List<UniqueSpeciesInfo> identifiedSpecies, Long identifiedSpeciesCount) {
		super();
		this.observationUploaded = observationUploaded;
		this.observationUploadedCount = observationUploadedCount;
		this.identifiedSpecies = identifiedSpecies;
		this.identifiedSpeciesCount = identifiedSpeciesCount;
	}

	public List<UniqueSpeciesInfo> getObservationUploaded() {
		return observationUploaded;
	}

	public void setObservationUploaded(List<UniqueSpeciesInfo> observationUploaded) {
		this.observationUploaded = observationUploaded;
	}

	public Long getObservationUploadedCount() {
		return observationUploadedCount;
	}

	public void setObservationUploadedCount(Long observationUploadedCount) {
		this.observationUploadedCount = observationUploadedCount;
	}

	public List<UniqueSpeciesInfo> getIdentifiedSpecies() {
		return identifiedSpecies;
	}

	public void setIdentifiedSpecies(List<UniqueSpeciesInfo> identifiedSpecies) {
		this.identifiedSpecies = identifiedSpecies;
	}

	public Long getIdentifiedSpeciesCount() {
		return identifiedSpeciesCount;
	}

	public void setIdentifiedSpeciesCount(Long identifiedSpeciesCount) {
		this.identifiedSpeciesCount = identifiedSpeciesCount;
	}

}
