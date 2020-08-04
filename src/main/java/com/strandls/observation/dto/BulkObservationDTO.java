package com.strandls.observation.dto;

import java.util.Map;

public class BulkObservationDTO {
	
	private Long dataset;
	private String filename;
	private Map<Integer, String> columns;
	
	public Long getDataset() {
		return dataset;
	}
	public void setDataset(Long dataset) {
		this.dataset = dataset;
	}
	public String getFilename() {
		return filename;
	}
	public void setFilename(String filename) {
		this.filename = filename;
	}
	public Map<Integer, String> getColumns() {
		return columns;
	}
	public void setColumns(Map<Integer, String> columns) {
		this.columns = columns;
	}

	@Override
	public String toString() {
		return "BulkObservationDTO [dataset=" + dataset + ", filename=" + filename + ", columns=" + columns + "]";
	}

}
