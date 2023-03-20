package com.strandls.observation.pojo;

import java.io.Serializable;

public class ObservationStatistics implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 134516488847318013L;
	private String title;
	private Long observations;
	private Long species;
	private Long people;

	public ObservationStatistics() {
		super();
	}

	public ObservationStatistics(String title, Long observations, Long species, Long people) {
		super();
		this.title = title;
		this.observations = observations;
		this.species = species;
		this.people = people;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Long getObservations() {
		return observations;
	}

	public void setObservations(Long observations) {
		this.observations = observations;
	}

	public Long getSpecies() {
		return species;
	}

	public void setSpecies(Long species) {
		this.species = species;
	}

	public Long getPeople() {
		return people;
	}

	public void setPeople(Long people) {
		this.people = people;
	}

}
