package com.strandls.observation.util;

public class DatasetThreadRunner implements Runnable {

	private String datasetTitle = "";
	private final DatasetStandaloneHelper datasetMapper;

	public DatasetThreadRunner(String DatasetTitle, DatasetStandaloneHelper datasetMapper) {
		this.datasetTitle = DatasetTitle;
		this.datasetMapper = datasetMapper;
	}

	@Override
	public void run() {
		datasetMapper.configureDataSetDefault(datasetTitle);

	}

}
