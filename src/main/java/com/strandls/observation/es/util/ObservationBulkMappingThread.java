package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapResponse;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.integrator.controllers.IntegratorServicesApi;
import com.strandls.integrator.pojo.CheckFilterRule;
import com.strandls.integrator.pojo.UserGroupObvRuleData;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.userGroup.controller.UserGroupServiceApi;
import com.strandls.userGroup.pojo.BulkGroupPostingData;
import com.strandls.userGroup.pojo.BulkGroupUnPostingData;
import com.strandls.userGroup.pojo.UserGroupObvFilterData;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.core.HttpHeaders;

public class ObservationBulkMappingThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ObservationBulkMappingThread.class);

	private enum BULK_ACTION {
		UG_BULK_POSTING("ugBulkPosting"), UG_BULK_UNPOSTING("ugBulkUnPosting");

		private String action;

		private BULK_ACTION(String action) {
			this.action = action;
		}

		public String getAction() {
			return action;
		}
	}

	private Boolean selectAll;
	private String bulkAction;
	private String bulkObservationIds;
	private String bulkUsergroupIds;
	private MapSearchQuery mapSearchQuery;
	private UserGroupServiceApi ugService;
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
	private final ESUpdate esUpdate;
	private IntegratorServicesApi integratorService;
	private TraitsServiceApi traitService;

	public ObservationBulkMappingThread(Boolean selectAll, String bulkAction, String bulkObservationIds,
			String bulkUsergroupIds, MapSearchQuery mapSearchQuery, UserGroupServiceApi ugService, String index,
			String type, String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField, String geoShapeFilterField,
			MapAggregationStatsResponse aggregationStatsResult, MapAggregationResponse aggregationResult, String view,
			EsServicesApi esService, ObservationMapperHelper observationMapperHelper, ObservationDAO observationDao,
			HttpServletRequest request, Headers headers, ObjectMapper objectMapper,
			IntegratorServicesApi integratorService, ESUpdate esUpdate, TraitsServiceApi traitService) {
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
		this.objectMapper = objectMapper;
		this.requestAuthHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		this.integratorService = integratorService;
		this.esUpdate = esUpdate;
		this.traitService = traitService;
	}

	@Override
	public void run() {

		try {

			List<UserGroupObvFilterData> list = new ArrayList<UserGroupObvFilterData>();
			List<Long> oservationIds = new ArrayList<Long>();
			List<Long> ugIds = new ArrayList<Long>();

			if (bulkObservationIds != null && !bulkObservationIds.isEmpty() && Boolean.FALSE.equals(selectAll)) {
				oservationIds.addAll(
						Arrays.stream(bulkObservationIds.split(",")).map(Long::valueOf).collect(Collectors.toList()));
			}

			if (bulkUsergroupIds != null && !bulkUsergroupIds.isEmpty()) {
				ugIds.addAll(
						Arrays.stream(bulkUsergroupIds.split(",")).map(Long::valueOf).collect(Collectors.toList()));
			}

			if (!oservationIds.isEmpty()) {
				List<Observation> obsDataList = observationDao.fecthByListOfIds(oservationIds);

				for (Observation obs : obsDataList) {
					UserGroupObvRuleData data = observationMapperHelper.getUGObvRuleData(obs);
					UserGroupObvRuleData ugObvFilterData = data;
					List<FactValuePair> traits = traitService.getFacts("species.participation.Observation",
							data.getObservationId().toString());
					Map<String, List<Long>> facts = traits.stream()
							.collect(Collectors.groupingBy(trait -> trait.getNameId().toString(),
									Collectors.mapping(FactValuePair::getValueId, Collectors.toList())));
					ugObvFilterData.setTraits(facts);
					CheckFilterRule checkFilterRule = new CheckFilterRule();
					checkFilterRule.setUserGroupId(ugIds);
					checkFilterRule.setUgObvFilterData(ugObvFilterData);
					integratorService = headers.addIntegratorHeader(integratorService, requestAuthHeader);
					List<Long> filterUGId = integratorService.checkUserGroupEligiblity(checkFilterRule);
					if (filterUGId != null && !filterUGId.isEmpty()) {
						list.add(observationMapperHelper.getUGFilterObvData(obs));
					}

				}

			}

			if (Boolean.TRUE.equals(selectAll)) {

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
					ugFilterData.setObservedOnDate(data.getObservedOn() != null ? data.getObservedOn() : null);
					ugFilterData.setAuthorId(data.getUser() != null ? data.getUser().getId() : null);
					ugFilterData.setTaxonomyId(data.getRecoIbp() != null ? data.getRecoIbp().getTaxonId() : null);

					if (bulkAction.equalsIgnoreCase(BULK_ACTION.UG_BULK_UNPOSTING.getAction())) {
						list.add(ugFilterData);
					} else if (bulkAction.equalsIgnoreCase(BULK_ACTION.UG_BULK_POSTING.getAction())) {
						UserGroupObvRuleData filterData = observationMapperHelper
								.getUGObvRuleData(observationDao.findById(data.getObservationId()));
						List<FactValuePair> traits = traitService.getFacts("species.participation.Observation",
								data.getObservationId().toString());
						Map<String, List<Long>> facts = traits.stream()
								.collect(Collectors.groupingBy(trait -> trait.getNameId().toString(),
										Collectors.mapping(FactValuePair::getValueId, Collectors.toList())));
						filterData.setTraits(facts);
						CheckFilterRule checkFilterRule = new CheckFilterRule();
						checkFilterRule.setUserGroupId(ugIds);
						checkFilterRule.setUgObvFilterData(filterData);
						integratorService = headers.addIntegratorHeader(integratorService, requestAuthHeader);
						List<Long> filterUGId = integratorService.checkUserGroupEligiblity(checkFilterRule);
						if (filterUGId != null && !filterUGId.isEmpty()) {
							list.add(observationMapperHelper
									.getUGFilterObvData(observationDao.findById(data.getObservationId())));
						}

					}

				}

			}

			if (!list.isEmpty() && !bulkAction.isEmpty()
					&& (bulkAction.contains(BULK_ACTION.UG_BULK_POSTING.getAction())
							|| bulkAction.contains(BULK_ACTION.UG_BULK_UNPOSTING.getAction()))) {

				List<UserGroupObvFilterData> ugObsList = new ArrayList<UserGroupObvFilterData>();
				;
				Integer count = 0;

				while (count < list.size()) {
					ugObsList.add(list.get(count));

					if (ugObsList.size() >= 200) {
						bulkGroupAction(ugObsList, ugIds);
						ugObsList.clear();
					}
					count++;
				}

				bulkGroupAction(ugObsList, ugIds);
				ugObsList.clear();
			}

		} catch (Exception e) {
			logger.error(e.getMessage());

		}

	}

	private void bulkGroupAction(List<UserGroupObvFilterData> ugObsList, List<Long> ugIds) {
		if (!ugObsList.isEmpty()) {
			BulkGroupPostingData ugBulkPostingData = bulkAction.contains("ugBulkPosting") ? new BulkGroupPostingData()
					: null;
			BulkGroupUnPostingData ugBulkUnPostingData = bulkAction.contains("ugBulkUnPosting")
					? new BulkGroupUnPostingData()
					: null;
			if (ugBulkPostingData != null) {
				ugBulkPostingData.setRecordType("observation");
				ugBulkPostingData.setUgObvFilterDataList(ugObsList);
				ugBulkPostingData.setUserGroupList(ugIds);
			} else if (ugBulkUnPostingData != null) {
				ugBulkUnPostingData.setRecordType("observation");
				ugBulkUnPostingData.setUgFilterDataList(ugObsList);
				ugBulkUnPostingData.setUserGroupList(ugIds);
			}

			ugService = headers.addUserGroupHeader(ugService, requestAuthHeader);
			try {
				if (ugBulkPostingData != null) {
					ugService.bulkPostingObservationUG(ugBulkPostingData);
				} else if (ugBulkUnPostingData != null) {
					ugService.bulkRemovingObservation(ugBulkUnPostingData);
				}

			} catch (com.strandls.userGroup.ApiException e) {
				logger.error(e.getMessage());
			}

			List<Long> obsIds = ugObsList.stream().map(item -> item.getObservationId()).collect(Collectors.toList());
			String observationList = StringUtils.join(obsIds, ',');
			ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
			Thread esThreadUpdate = new Thread(updateThread);
			esThreadUpdate.start();

		}
	}

}
