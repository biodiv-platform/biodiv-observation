package com.strandls.observation.gbif;

import javax.inject.Inject;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.es.util.GbifObservationESMapper;
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

	public String gbifData() {

		GbifObservationThread gbifThread = new GbifObservationThread(utilityService, esService, recoDao, gbifMapper,
				layerService);
		Thread t = new Thread(gbifThread);
		t.start();
		return t.getState().toString();
	}
}
