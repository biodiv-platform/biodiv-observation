package com.strandls.observation.es.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.ApiException;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapResponse;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.BulkGroupPostingData;
import com.strandls.userGroup.pojo.BulkGroupUnPostingData;
import com.strandls.userGroup.pojo.UserGroupObvFilterData;

public class ObservationBulkMappingThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ObservationBulkMappingThread.class);

	private Boolean selectAll;
	private String bulkAction;
	private List<Long> bulkObservationIds;
	private List<Long> bulkUsergroupIds;
	private MapSearchQuery mapSearchQuery;
	private UserGroupSerivceApi ugService;
	private String index;
	private String type;
	private String geoAggregationField;
	private Integer geoAggegationPrecision;
	private Boolean onlyFilteredAggregation;
	private String termsAggregationField;
	private String geoShapeFilterField;
	private EsServicesApi esService;
	private ObservationMapperHelper observationMapperHelper;
	private ObservationDAO observationDao;
	private ObjectMapper objectMapper;
	private final HttpServletRequest request;
	private final Headers headers;
	private final String requestAuthHeader;


	public ObservationBulkMappingThread(Boolean selectAll, String bulkAction, List<Long> bulkObservationIds,
			List<Long> bulkUsergroupIds, MapSearchQuery mapSearchQuery, UserGroupSerivceApi ugService, String index,
			String type, String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField, String geoShapeFilterField,
			MapAggregationStatsResponse aggregationStatsResult, MapAggregationResponse aggregationResult, String view,
			EsServicesApi esService, ObservationMapperHelper observationMapperHelper, ObservationDAO observationDao,HttpServletRequest request, Headers headers) {
		super();
		this.selectAll = selectAll;
		this.bulkAction = bulkAction;
		this.bulkObservationIds = bulkObservationIds;
		this.bulkUsergroupIds = bulkUsergroupIds;
		this.mapSearchQuery = mapSearchQuery;
		this.ugService = ugService;
		this.index = index;
		this.type = type;
		this.geoAggregationField = geoAggregationField;
		this.geoAggegationPrecision = geoAggegationPrecision;
		this.onlyFilteredAggregation = onlyFilteredAggregation;
		this.termsAggregationField = termsAggregationField;
		this.geoShapeFilterField = geoShapeFilterField;
		this.esService = esService;
		this.observationMapperHelper = observationMapperHelper;
		this.observationDao = observationDao;
		this.request = request;
		this.headers = headers;
		this.requestAuthHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
	}

	@Override
	public void run() {

		List<UserGroupObvFilterData> list = new ArrayList<UserGroupObvFilterData>();

		if (!bulkObservationIds.isEmpty() && bulkObservationIds != null && Boolean.FALSE.equals(selectAll)) {
			List<Observation> obsDataList = observationDao.fecthByListOfIds(bulkObservationIds);

			for (Observation obs : obsDataList) {
				UserGroupObvFilterData data = observationMapperHelper.getUGFilterObvData(obs);
				list.add(data);
			}

		}

		if (Boolean.TRUE.equals(selectAll)) {
			try {
				MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
						onlyFilteredAggregation, termsAggregationField, geoShapeFilterField, mapSearchQuery);
				List<MapDocument> documents = result.getDocuments();
				for (MapDocument document : documents) {
					ObservationListMinimalData data = objectMapper.readValue(String.valueOf(document.getDocument()),
							ObservationListMinimalData.class);
					UserGroupObvFilterData ugFilterData = new UserGroupObvFilterData();
					ugFilterData.setObservationId(data.getObservationId());
					ugFilterData.setCreatedOnDate(data.getCreatedOn());
					ugFilterData.setLatitude(data.getLatitude());
					ugFilterData.setLongitude(data.getLongitude());
					ugFilterData.setObservedOnDate(data.getObservedOn());
					ugFilterData.setAuthorId(data.getUser().getId());
					ugFilterData.setTaxonomyId(data.getRecoIbp().getTaxonId());
					list.add(ugFilterData);
				}

			} catch (IOException | ApiException e) {
				logger.error(e.getMessage());
			}

		}

		if (!list.isEmpty() && bulkAction.isEmpty()
				&& (bulkAction.contains("ugBulkPosting") || bulkAction.contains("ugBulkUnPosting"))) {

			List<UserGroupObvFilterData> ugObsList = new ArrayList<UserGroupObvFilterData>();
			;
			Integer count = 0;

			while (count < list.size()) {
				ugObsList.add(list.get(count));

				if (ugObsList.size() >= 200) {

					BulkGroupPostingData ugBulkPostingData = bulkAction.contains("ugBulkPosting")
							? new BulkGroupPostingData()
							: null;
					BulkGroupUnPostingData ugBulkUnPostingData = bulkAction.contains("ugBulkUnPosting")
							? new BulkGroupUnPostingData()
							: null;
					if (ugBulkPostingData != null) {
						ugBulkPostingData.setRecordType("observation");
						ugBulkPostingData.setUgObvFilterDataList(ugObsList);
						ugBulkPostingData.setUserGroupList(bulkUsergroupIds);
					} else if (ugBulkUnPostingData != null) {
						ugBulkUnPostingData.setRecordType("observation");
						ugBulkUnPostingData.setUgFilterDataList(ugObsList);
						ugBulkUnPostingData.setUserGroupList(bulkUsergroupIds);
					}
					ugService = headers.addUserGroupHeader(ugService,requestAuthHeader);
					UGBulkMappingThread ugThread = new UGBulkMappingThread(ugBulkPostingData, ugService,
							ugBulkUnPostingData);
					Thread thread = new Thread(ugThread);
					thread.start();
					ugObsList.clear();
				}

				count++;
			}

			if (!ugObsList.isEmpty()) {
				BulkGroupPostingData ugBulkPostingData = bulkAction.contains("ugBulkPosting")
						? new BulkGroupPostingData()
						: null;
				BulkGroupUnPostingData ugBulkUnPostingData = bulkAction.contains("ugBulkUnPosting")
						? new BulkGroupUnPostingData()
						: null;
				if (ugBulkPostingData != null) {
					ugBulkPostingData.setRecordType("observation");
					ugBulkPostingData.setUgObvFilterDataList(ugObsList);
					ugBulkPostingData.setUserGroupList(bulkUsergroupIds);
				} else if (ugBulkUnPostingData != null) {
					ugBulkUnPostingData.setRecordType("observation");
					ugBulkUnPostingData.setUgFilterDataList(ugObsList);
					ugBulkUnPostingData.setUserGroupList(bulkUsergroupIds);
				}
				
				ugService = headers.addUserGroupHeader(ugService,requestAuthHeader);
				UGBulkMappingThread ugThread = new UGBulkMappingThread(ugBulkPostingData, ugService,
						ugBulkUnPostingData);
				Thread thread = new Thread(ugThread);
				thread.start();
				ugObsList.clear();

			}
		}

	}

}
