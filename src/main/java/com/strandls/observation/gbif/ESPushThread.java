package com.strandls.observation.gbif;

import java.util.List;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapQueryResponse;

public class ESPushThread implements Runnable {

	private String index;
	private String type;
	private String observations;
	private EsServicesApi esService;

	public ESPushThread() {
		super();
	}

	public ESPushThread(String index, String type, String observations, EsServicesApi esService) {
		super();
		this.index = index;
		this.type = type;
		this.observations = observations;
		this.esService = esService;
	}

	@Override
	public void run() {
		List<MapQueryResponse> response;
		try {
			response = esService.bulkUpload(index, type, observations);
			System.out.println(response);
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

}
