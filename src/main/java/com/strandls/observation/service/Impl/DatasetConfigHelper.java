package com.strandls.observation.service.Impl;

import javax.inject.Inject;

import com.strandls.observation.util.DatasetStandaloneHelper;
import com.strandls.observation.util.DatasetThreadRunner;

public class DatasetConfigHelper {
	@Inject
	private DatasetStandaloneHelper datasetStandalone;

	public void createStandaloneRecord(String title) {
		DatasetThreadRunner standaloneThread = new DatasetThreadRunner(title, datasetStandalone);
		Thread thread = new Thread(standaloneThread);
		thread.start();
	}

}
