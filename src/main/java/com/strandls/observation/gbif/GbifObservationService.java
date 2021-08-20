package com.strandls.observation.gbif;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import com.strandls.observation.pojo.Recommendation;
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

	@Inject
	BulkNameParser bulkNameParser;

	@SuppressWarnings("unused")
	public String gbifData() {

		String path = PropertyFileUtil.fetchProperty("config.properties", "datasetPath");

		try {

			/*
			 * Path path2 = Paths.get(path); Long lines = Files.lines(path2).count();
			 * System.out.println("lines = " + lines);
			 */

			FileWriter fw = new FileWriter("files_ingested.txt");

			// FileReader fileReader2 = new FileReader(path);
			// CSVReader csvReader2 = new CSVReader(fileReader2, '\t');

			String pathToFile = PropertyFileUtil.fetchProperty("config.properties", "datasetPath");

			//for (int j = 1; j <= 1; j++) {
				String filenameToPick = pathToFile + "data_" + 1 + ".csv";
				//String filenameToPick = pathToFile + "sample.csv";
				Path path2 = Paths.get(filenameToPick);
				Long lines = Files.lines(path2).count();
				System.out.println("lines = " + lines);
				

				System.out.println(filenameToPick);

				lines = lines - 1;
				System.out.println("rows=" + lines);

				ExecutorService executor = Executors.newFixedThreadPool(1000);
				// ExecutorService executor = Executors.newCachedThreadPool();
				int numThreads = 8;

				int batchSize = (Integer.parseInt(lines.toString()) / numThreads);
				int s = 1;

				int m;
				if (lines % numThreads == 0) {
					m = numThreads;
				} else {
					m = numThreads + 1;
				}

				List<Recommendation> listOfNames = recoDao.findAllScientificName();
				Map<Long, Recommendation> allScientificNames = new HashMap<Long, Recommendation>();
				for (Recommendation reco : listOfNames) {
					allScientificNames.put(reco.getTaxonConceptId(), reco);
				}

				Map<String, Recommendation> cache = new HashMap<String, Recommendation>();

				List<Recommendation> findNamesWithoutTaxonId = recoDao.findAllNamesWithoutTaxonId();
				Map<String, Recommendation> findByCanonicalNames = new HashMap<String, Recommendation>();

				for (Recommendation reco : findNamesWithoutTaxonId) {
					findByCanonicalNames.put(reco.getCanonicalName(), reco);
				}

				StopWatch stopWatch = new StopWatch();
				stopWatch.start();

				for (int i = 1; i <= m; i++) {
					if (i == m) {

						int t = Integer.parseInt(lines.toString()) - s; //
						System.out.println(s + "," + (s + t));
						GbifObservationThread gbifThread1 = new GbifObservationThread(utilityService, esService,
								recoDao, gbifMapper, layerService, taxonomyService, s, s + t, i, bulkNameParser,
								allScientificNames, cache, findByCanonicalNames, filenameToPick);
						executor.execute(gbifThread1);

					} else {
						GbifObservationThread gbifThread1 = new GbifObservationThread(utilityService, esService,
								recoDao, gbifMapper, layerService, taxonomyService, s, (i * batchSize), i,
								bulkNameParser, allScientificNames, cache, findByCanonicalNames, filenameToPick);
						executor.execute(gbifThread1);

					}

					s = (i) * batchSize;
				}

				executor.shutdown();
				executor.awaitTermination(1, TimeUnit.HOURS);
				stopWatch.stop();

				long time = stopWatch.getTime();
				time = time / (1000);
				System.out.println("Elapsed Time in seconds: " + time);
				
				System.out.println("file ingested = "+filenameToPick);
				fw.write("file ingested = "+filenameToPick+" in time = "+time+" secs");
				fw.close();
				

				//return ("running");

			//}
            
			return("running");
			// int numRows = 0;
			/*
			 * while ((csvReader2.readNext()) != null) {
			 * 
			 * numRows++; }
			 */
			// numRows = numRows - 1;
			

		} catch (Exception e) {

			System.out.println("Process failed");
			e.printStackTrace();
		}

		return ("not succesfull");

	}
}
