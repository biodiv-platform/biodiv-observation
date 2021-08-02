package com.strandls.observation.gbif;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.inject.Inject;

import org.apache.commons.lang3.time.StopWatch;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.opencsv.CSVReader;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.es.util.GbifObservationESMapper;
import com.strandls.observation.util.PropertyFileUtil;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.utility.controller.UtilityServiceApi;

public class GbifObservationService {

	@Inject
	private UtilityServiceApi utilityService;

	@Inject
	private EsServicesApi esService;

	@Inject
	private RecommendationDao recoDao;

	@Inject
	private GbifObservationESMapper gbifMapper;

	@Inject
	private LayerServiceApi layerService;

	@Inject
	private TaxonomyServicesApi taxonomyService;

	public String gbifData() {

		String path = PropertyFileUtil.fetchProperty("config.properties", "datasetPath");

		try {
		

			FileReader fileReader2 = new FileReader(path);
			CSVReader csvReader2 = new CSVReader(fileReader2, '\t');
			int numRows = 0;
			while ((csvReader2.readNext()) != null) {
				numRows++;
			}
			numRows = numRows - 1;
			System.out.println("rows=" + numRows);

			ExecutorService executor = Executors.newFixedThreadPool(10);

			int numThreads = 5;

			int batchSize = (numRows / numThreads);
			int s = 1;

			int m;
			if (numRows % numThreads == 0) {
				m = numThreads;
			} else {
				m = numThreads + 1;
			}

			StopWatch stopWatch = new StopWatch();
			stopWatch.start();

			for (int i = 1; i <= m; i++) {
				if (i == m) {

					int t = numRows - s; // System.out.println(s+","+(s+t));
					GbifObservationThread gbifThread1 = new GbifObservationThread(utilityService, esService, recoDao,
							gbifMapper, layerService, taxonomyService, s, s + t, i);
					executor.execute(gbifThread1);

				} else {
					GbifObservationThread gbifThread1 = new GbifObservationThread(utilityService, esService, recoDao,
							gbifMapper, layerService, taxonomyService, s, (i * batchSize), i);
					executor.execute(gbifThread1);

				}

				s = (i) * batchSize;
			}

		

			executor.shutdown();
			executor.awaitTermination(1,TimeUnit.HOURS);
			stopWatch.stop();
			
			System.out.println("Elapsed Time in mili-seconds: " + stopWatch.getTime());

			return ("running");

		} catch (Exception e) {
			// TODO Auto-generated catch block
			System.out.println("Process failed");
			e.printStackTrace();
		}

		return ("not succesfull");

	}
}
