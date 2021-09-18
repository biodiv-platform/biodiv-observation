package com.strandls.observation.pojo;

import java.util.List;

public class ObservationDatatableList {
	private List<ObservationDataTableShow> observationList;
	private Long total;

	public ObservationDatatableList() {
		super();
	}

	public ObservationDatatableList(List<ObservationDataTableShow> observationList, Long total) {
		super();
		this.observationList = observationList;
		this.total = total;
	}

	public List<ObservationDataTableShow> getObservationList() {
		return observationList;
	}

	public void setObservationList(List<ObservationDataTableShow> observationList) {
		this.observationList = observationList;
	}

	public Long getTotal() {
		return total;
	}

	public void setTotal(Long total) {
		this.total = total;
	}

}
