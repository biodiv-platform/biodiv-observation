package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.apache.commons.lang3.StringUtils;
import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapResponse;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.integrator.controllers.IntergratorServicesApi;
import com.strandls.integrator.pojo.CheckFilterRule;
import com.strandls.integrator.pojo.UserGroupObvRuleData;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoData;
import com.strandls.observation.service.RecommendationService;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.BulkGroupPostingData;
import com.strandls.userGroup.pojo.BulkGroupUnPostingData;
import com.strandls.userGroup.pojo.UserGroupObvFilterData;

public class ObservationBulkMappingThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ObservationBulkMappingThread.class);

	private enum BULK_ACTION {
		UG_BULK_POSTING("ugBulkPosting"), UG_BULK_UNPOSTING("ugBulkUnPosting"),SPECIES_BULK_POSTING("speciesBulkPosting"), RECO_BULK_POSTING("recoBulkPosting");

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
	private String bulkSpeciesGroupId;
	private String bulkRecoSuggestion;
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
	private final ESUpdate esUpdate;
	private IntergratorServicesApi intergratorService;
	private TraitsServiceApi traitService;
	private RecommendationService recoService;

	public ObservationBulkMappingThread(Boolean selectAll, String bulkAction, String bulkObservationIds,
			String bulkUsergroupIds, String bulkSpeciesGroupId, String bulkRecoSuggestion, MapSearchQuery mapSearchQuery, UserGroupSerivceApi ugService, String index,
			String type, String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField, String geoShapeFilterField,
			MapAggregationStatsResponse aggregationStatsResult, MapAggregationResponse aggregationResult, String view,
			EsServicesApi esService, ObservationMapperHelper observationMapperHelper, ObservationDAO observationDao,
			HttpServletRequest request, Headers headers, ObjectMapper objectMapper,
			IntergratorServicesApi intergratorService, ESUpdate esUpdate, TraitsServiceApi traitService, RecommendationService recoService) {
		super();
		this.selectAll = selectAll;
		this.bulkAction = bulkAction;
		this.bulkObservationIds = bulkObservationIds;
		this.bulkUsergroupIds = bulkUsergroupIds;
		this.bulkSpeciesGroupId = bulkSpeciesGroupId;
		this.bulkRecoSuggestion = bulkRecoSuggestion;
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
		this.intergratorService = intergratorService;
		this.esUpdate = esUpdate;
		this.traitService = traitService;
		this.recoService = recoService;
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

			if (!oservationIds.isEmpty() && !bulkAction.isEmpty()
					&& (bulkAction.contains(BULK_ACTION.UG_BULK_POSTING.getAction())
							|| bulkAction.contains(BULK_ACTION.UG_BULK_UNPOSTING.getAction()))) {
				List<Observation> obsDataList = observationDao.fecthByListOfIds(oservationIds);

				for (Observation obs : obsDataList) {
					UserGroupObvRuleData data = observationMapperHelper.getUGObvRuleData(obs);
					UserGroupObvRuleData ugObvFilterData = data;
					List<FactValuePair> traits = traitService.getFacts("species.participation.Observation", data.getObservationId().toString());
					Map<String, List<Long>> facts = traits.stream()
				    .collect(Collectors.groupingBy(
				    		trait -> trait.getNameId().toString(), 
				            Collectors.mapping(FactValuePair::getValueId, Collectors.toList())
				        ));
					ugObvFilterData.setTraits(facts);
					CheckFilterRule checkFilterRule = new CheckFilterRule();
					checkFilterRule.setUserGroupId(ugIds);
					checkFilterRule.setUgObvFilterData(ugObvFilterData);
					intergratorService = headers.addIntergratorHeader(intergratorService, requestAuthHeader);
					List<Long> filterUGId = intergratorService.checkUserGroupEligiblity(checkFilterRule);
					if (filterUGId != null && !filterUGId.isEmpty()) {
						list.add(observationMapperHelper.getUGFilterObvData(obs));
					}

				}

			}

			if (Boolean.TRUE.equals(selectAll) && !bulkAction.isEmpty()
					&& (bulkAction.contains(BULK_ACTION.UG_BULK_POSTING.getAction())
							|| bulkAction.contains(BULK_ACTION.UG_BULK_UNPOSTING.getAction()))) {

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
						List<FactValuePair> traits = traitService.getFacts("species.participation.Observation", data.getObservationId().toString());
						Map<String, List<Long>> facts = traits.stream()
					    .collect(Collectors.groupingBy(
					    		trait -> trait.getNameId().toString(), 
					            Collectors.mapping(FactValuePair::getValueId, Collectors.toList())
					        ));
						filterData.setTraits(facts);
						CheckFilterRule checkFilterRule = new CheckFilterRule();
						checkFilterRule.setUserGroupId(ugIds);
						checkFilterRule.setUgObvFilterData(filterData);
						intergratorService = headers.addIntergratorHeader(intergratorService, requestAuthHeader);
						List<Long> filterUGId = intergratorService.checkUserGroupEligiblity(checkFilterRule);
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
			
			if (!bulkAction.isEmpty() && (bulkAction.contains(BULK_ACTION.SPECIES_BULK_POSTING.getAction()))) {
				List<Observation> obsDataList = new ArrayList<Observation>();
				if (bulkSpeciesGroupId != null && !bulkSpeciesGroupId.isEmpty()) {
					Long sGroupId = Long.parseLong(bulkSpeciesGroupId);
					if (!oservationIds.isEmpty()) {
						obsDataList = observationDao.fecthByListOfIds(oservationIds);

					}
					if (Boolean.TRUE.equals(selectAll)) {
						MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
								onlyFilteredAggregation, termsAggregationField, geoShapeFilterField, mapSearchQuery);
						List<MapDocument> documents = result.getDocuments();
						for (MapDocument document : documents) {
							Observation data = objectMapper.readValue(String.valueOf(document.getDocument()),
									Observation.class);
							obsDataList.add(data);
						}
					}
					List<Observation> ObsList = new ArrayList<Observation>();
					;
					Integer count = 0;

					while (count < obsDataList.size()) {
						ObsList.add(obsDataList.get(count));

						if (ObsList.size() >= 200) {
							bulkSpeciesGroupAction(ObsList, sGroupId);
							ObsList.clear();
						}
						count++;
					}

					bulkSpeciesGroupAction(ObsList, sGroupId);
					ObsList.clear();
				}
			}

			if (!bulkAction.isEmpty() && (bulkAction.contains(BULK_ACTION.RECO_BULK_POSTING.getAction()))) {
				List<Observation> obsDataList = new ArrayList<Observation>();
				if (bulkRecoSuggestion != null && !bulkRecoSuggestion.isEmpty()) {
					RecoData recoData = objectMapper.readValue(bulkRecoSuggestion,
							RecoData.class);
					if (!oservationIds.isEmpty()) {
						obsDataList = observationDao.fecthByListOfIds(oservationIds);

					}
					if (Boolean.TRUE.equals(selectAll)) {
						MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
								onlyFilteredAggregation, termsAggregationField, geoShapeFilterField, mapSearchQuery);
						List<MapDocument> documents = result.getDocuments();
						for (MapDocument document : documents) {
							Observation data = objectMapper.readValue(String.valueOf(document.getDocument()),
									Observation.class);
							obsDataList.add(data);
						}
					}
					List<Observation> ObsList = new ArrayList<Observation>();
					;
					Integer count = 0;

					while (count < obsDataList.size()) {
						ObsList.add(obsDataList.get(count));

						if (ObsList.size() >= 200) {
							bulkRecoSuggestionAction(ObsList, recoData);
							ObsList.clear();
						}
						count++;
					}

					bulkRecoSuggestionAction(ObsList, recoData);
					ObsList.clear();
				}
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
	
	private void bulkSpeciesGroupAction(List<Observation> obsList, Long sGroupId) {
		for (Observation observation : obsList) {
			observation.setGroupId(sGroupId);
			observation.setLastRevised(new Date());
			observation = observationDao.update(observation);
		}
		List<Long> obsIds = obsList.stream().map(item -> item.getId()).collect(Collectors.toList());
		String observationList = StringUtils.join(obsIds, ',');
		ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
		Thread esThreadUpdate = new Thread(updateThread);
		esThreadUpdate.start();
	}
	
	private void bulkRecoSuggestionAction(List<Observation> obsList, RecoData recoData) {
		for (Observation observation : obsList) {
			try {
				CommonProfile profile = AuthUtil.getProfileFromRequest(request);
				Long userId = Long.parseLong(profile.getId());
				RecoCreate recoCreate = observationMapperHelper.createRecoMapping(recoData);
				recoService.createRecoVote(request, (long) 1, observation.getId(),
						recoData.getScientificNameTaxonId(), recoCreate, false);
				
			} catch (Exception e) {
			}
		}
	}

}
