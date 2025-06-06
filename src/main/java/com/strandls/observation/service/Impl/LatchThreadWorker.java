/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.util.Map;
import java.util.concurrent.CountDownLatch;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.AggregationResponse;
import com.strandls.esmodule.pojo.MapSearchQuery;

/**
 * @author Abhishek Rudra
 *
 * 
 */
public class LatchThreadWorker extends Thread {

	private final Logger logger = LoggerFactory.getLogger(LatchThreadWorker.class);

	private String index;
	private String type;
	private String filter;
	private String geoAggregationField;
	private MapSearchQuery searchQuery;
	private Map<String, AggregationResponse> mapResponse;
//	for traits and custom field
	private String namedAgg;
	private CountDownLatch latch;
	private String geoShapeFilterField;

	@Inject
	private EsServicesApi esService;

	/**
	 * @param index
	 * @param type
	 * @param filter
	 * @param geoAggregationField
	 * @param searchQuery
	 * @param mapResponse
	 * @param namedAgg
	 * @param latch
	 * @param esService
	 */
	public LatchThreadWorker(String index, String type, String filter, String geoAggregationField,
			MapSearchQuery searchQuery, Map<String, AggregationResponse> mapResponse, String namedAgg,
			CountDownLatch latch, EsServicesApi esService, String geoShapeFilterField) {
		super();
		this.index = index;
		this.type = type;
		this.filter = filter;
		this.geoShapeFilterField = geoShapeFilterField;
		this.geoAggregationField = geoAggregationField;
		this.searchQuery = searchQuery;
		this.mapResponse = mapResponse;
		this.namedAgg = namedAgg;
		this.latch = latch;
		this.esService = esService;
	}

	@Override
	public void run() {
		try {
			AggregationResponse response = esService.getAggregation(index, type, filter, geoAggregationField,
					geoShapeFilterField, searchQuery);
			if (namedAgg != null && !namedAgg.isEmpty())
				mapResponse.put(namedAgg, response);
			else
				mapResponse.put(filter.split("\\|")[0], response);
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			latch.countDown();
		}

	}

}
