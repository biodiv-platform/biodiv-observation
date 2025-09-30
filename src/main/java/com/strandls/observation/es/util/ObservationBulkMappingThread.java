package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.apache.commons.lang3.StringUtils;
import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.activity.ApiException;
import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.activity.pojo.ActivityLoggingData;
import com.strandls.activity.pojo.RecoVoteActivity;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapResponse;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.integrator.controllers.IntergratorServicesApi;
import com.strandls.integrator.pojo.CheckFilterRule;
import com.strandls.integrator.pojo.UserGroupObvRuleData;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.dao.RecommendationVoteDao;
import com.strandls.observation.pojo.MapAggregationResponse;
import com.strandls.observation.pojo.MapAggregationStatsResponse;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoData;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.pojo.RecommendationVote;
import com.strandls.observation.pojo.UniqueRecoVote;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.service.RecommendationService;
import com.strandls.observation.service.Impl.ObservationMapperHelper;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.taxonomy.pojo.TaxonomyDefinition;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.FactsCreateData;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.BulkGroupPostingData;
import com.strandls.userGroup.pojo.BulkGroupUnPostingData;
import com.strandls.userGroup.pojo.UserGroupObvFilterData;

public class ObservationBulkMappingThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ObservationBulkMappingThread.class);

	private enum BULK_ACTION {
		UG_BULK_POSTING("ugBulkPosting"), UG_BULK_UNPOSTING("ugBulkUnPosting"),
		SPECIES_BULK_POSTING("speciesBulkPosting"), RECO_BULK_POSTING("recoBulkPosting"),
		TRAITS_BULK_POSTING("traitsBulkPosting"), VALIDATE_BULK_POSTING("validateBulkObservations"),
		UNLOCK_BULK_POSTING("unlockBulkObservations");

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
	private String bulkTraits;
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
	private RecommendationDao recoDao;
	private RecommendationVoteDao recoVoteDao;
	private ObjectMapper objectMapper;
	private final HttpServletRequest request;
	private final Headers headers;
	private final String requestAuthHeader;
	private final ESUpdate esUpdate;
	private IntergratorServicesApi intergratorService;
	private TraitsServiceApi traitService;
	private RecommendationService recoService;
	private CommonProfile profile;
	private ObservationService observationService;
	private ActivitySerivceApi activityService;
	private TaxonomyServicesApi taxonomyService;

	public ObservationBulkMappingThread(Boolean selectAll, String bulkAction, String bulkObservationIds,
			String bulkUsergroupIds, String bulkSpeciesGroupId, String bulkRecoSuggestion, String bulkTraits,
			MapSearchQuery mapSearchQuery, UserGroupSerivceApi ugService, String index, String type,
			String geoAggregationField, Integer geoAggegationPrecision, Boolean onlyFilteredAggregation,
			String termsAggregationField, String geoShapeFilterField,
			MapAggregationStatsResponse aggregationStatsResult, MapAggregationResponse aggregationResult, String view,
			EsServicesApi esService, ObservationMapperHelper observationMapperHelper, ObservationDAO observationDao,
			RecommendationDao recoDao, RecommendationVoteDao recoVoteDao, HttpServletRequest request, Headers headers,
			ObjectMapper objectMapper, IntergratorServicesApi intergratorService, ESUpdate esUpdate,
			TraitsServiceApi traitService, RecommendationService recoService, CommonProfile profile,
			ObservationService observationService, ActivitySerivceApi activityService,
			TaxonomyServicesApi taxonomyService) {
		super();
		this.selectAll = selectAll;
		this.bulkAction = bulkAction;
		this.bulkObservationIds = bulkObservationIds;
		this.bulkUsergroupIds = bulkUsergroupIds;
		this.bulkSpeciesGroupId = bulkSpeciesGroupId;
		this.bulkRecoSuggestion = bulkRecoSuggestion;
		this.bulkTraits = bulkTraits;
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
		this.recoDao = recoDao;
		this.recoVoteDao = recoVoteDao;
		this.request = request;
		this.headers = headers;
		this.objectMapper = objectMapper;
		this.requestAuthHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		this.intergratorService = intergratorService;
		this.esUpdate = esUpdate;
		this.traitService = traitService;
		this.recoService = recoService;
		this.profile = profile;
		this.observationService = observationService;
		this.activityService = activityService;
		this.taxonomyService = taxonomyService;
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
					List<FactValuePair> traits = traitService.getFacts("species.participation.Observation",
							data.getObservationId().toString());
					Map<String, List<Long>> facts = traits.stream()
							.collect(Collectors.groupingBy(trait -> trait.getNameId().toString(),
									Collectors.mapping(FactValuePair::getValueId, Collectors.toList())));
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
						List<FactValuePair> traits = traitService.getFacts("species.participation.Observation",
								data.getObservationId().toString());
						Map<String, List<Long>> facts = traits.stream()
								.collect(Collectors.groupingBy(trait -> trait.getNameId().toString(),
										Collectors.mapping(FactValuePair::getValueId, Collectors.toList())));
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
				List<Long> obIds = new ArrayList<Long>();
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
							ObservationListMinimalData data = objectMapper.readValue(
									String.valueOf(document.getDocument()), ObservationListMinimalData.class);
							obIds.add(data.getObservationId());
						}
					}
					List<Observation> ObsList = new ArrayList<Observation>();
					List<Long> ObsIdList = new ArrayList<Long>();
					;
					Integer count = 0;

					if (Boolean.FALSE.equals(selectAll)) {
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
					} else {
						while (count < obIds.size()) {
							ObsIdList.add(obIds.get(count));
							if (ObsIdList.size() >= 200) {
								bulkSpeciesGroupAction(observationDao.fecthByListOfIds(ObsIdList), sGroupId);
								ObsIdList.clear();
							}
							count++;
						}
						bulkSpeciesGroupAction(observationDao.fecthByListOfIds(ObsIdList), sGroupId);
						ObsIdList.clear();
					}
				}
			}

			if (!bulkAction.isEmpty() && (bulkAction.contains(BULK_ACTION.RECO_BULK_POSTING.getAction()))) {
				List<Long> obsIdList = new ArrayList<Long>();
				if (bulkRecoSuggestion != null && !bulkRecoSuggestion.isEmpty()) {
					RecoData recoData = objectMapper.readValue(bulkRecoSuggestion, RecoData.class);
					if (!oservationIds.isEmpty()) {
						obsIdList = oservationIds;

					}
					if (Boolean.TRUE.equals(selectAll)) {
						MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
								onlyFilteredAggregation, termsAggregationField, geoShapeFilterField, mapSearchQuery);
						List<MapDocument> documents = result.getDocuments();
						for (MapDocument document : documents) {
							ObservationListMinimalData data = objectMapper.readValue(
									String.valueOf(document.getDocument()), ObservationListMinimalData.class);
							obsIdList.add(data.getObservationId());
						}
					}
					List<Long> ObsBatchList = new ArrayList<Long>();
					;
					Integer count = 0;

					while (count < obsIdList.size()) {
						ObsBatchList.add(obsIdList.get(count));

						if (ObsBatchList.size() >= 200) {
							bulkRecoSuggestionAction(ObsBatchList, recoData);
							ObsBatchList.clear();
						}
						count++;
					}

					bulkRecoSuggestionAction(ObsBatchList, recoData);
					ObsBatchList.clear();
				}
			}

			if (!bulkAction.isEmpty() && (bulkAction.contains(BULK_ACTION.VALIDATE_BULK_POSTING.getAction()))) {
				List<Observation> obsDataList = new ArrayList<Observation>();
				List<Long> obIds = new ArrayList<Long>();
				if (!oservationIds.isEmpty()) {
					obsDataList = observationDao.fecthByListOfIds(oservationIds);

				}
				if (Boolean.TRUE.equals(selectAll)) {
					MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
							onlyFilteredAggregation, termsAggregationField, geoShapeFilterField, mapSearchQuery);
					List<MapDocument> documents = result.getDocuments();
					for (MapDocument document : documents) {
						ObservationListMinimalData data = objectMapper.readValue(String.valueOf(document.getDocument()),
								ObservationListMinimalData.class);
						obIds.add(data.getObservationId());
					}
				}
				List<Observation> ObsList = new ArrayList<Observation>();
				List<Long> ObsIdList = new ArrayList<Long>();
				;
				Integer count = 0;
				if (Boolean.FALSE.equals(selectAll)) {
					while (count < obsDataList.size()) {
						ObsList.add(obsDataList.get(count));

						if (ObsList.size() >= 200) {
							bulkValidateAction(ObsList);
							ObsList.clear();
						}
						count++;
					}

					bulkValidateAction(ObsList);
					ObsList.clear();
				} else {
					while (count < obIds.size()) {
						ObsIdList.add(obIds.get(count));
						if (ObsIdList.size() >= 200) {
							System.out.println(ObsIdList);
							bulkValidateAction(observationDao.fecthByListOfIds(ObsIdList));
							ObsIdList.clear();
						}
						count++;
					}
					bulkValidateAction(observationDao.fecthByListOfIds(ObsIdList));
					ObsIdList.clear();
				}

			}

			if (!bulkAction.isEmpty() && (bulkAction.contains(BULK_ACTION.UNLOCK_BULK_POSTING.getAction()))) {
				List<Observation> obsDataList = new ArrayList<Observation>();
				List<Long> obIds = new ArrayList<Long>();
				if (!oservationIds.isEmpty()) {
					obsDataList = observationDao.fecthByListOfIds(oservationIds);

				}
				if (Boolean.TRUE.equals(selectAll)) {
					MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
							onlyFilteredAggregation, termsAggregationField, geoShapeFilterField, mapSearchQuery);
					List<MapDocument> documents = result.getDocuments();
					for (MapDocument document : documents) {
						ObservationListMinimalData data = objectMapper.readValue(String.valueOf(document.getDocument()),
								ObservationListMinimalData.class);
						obIds.add(data.getObservationId());
					}
				}
				List<Observation> ObsList = new ArrayList<Observation>();
				List<Long> ObsIdList = new ArrayList<Long>();
				;
				Integer count = 0;
				if (Boolean.FALSE.equals(selectAll)) {
					while (count < obsDataList.size()) {
						ObsList.add(obsDataList.get(count));

						if (ObsList.size() >= 200) {
							bulkUnlockAction(ObsList);
							ObsList.clear();
						}
						count++;
					}

					bulkUnlockAction(ObsList);
					ObsList.clear();
				} else {
					while (count < obIds.size()) {
						ObsIdList.add(obIds.get(count));
						if (ObsIdList.size() >= 200) {
							System.out.println(ObsIdList);
							bulkUnlockAction(observationDao.fecthByListOfIds(ObsIdList));
							ObsIdList.clear();
						}
						count++;
					}
					bulkUnlockAction(observationDao.fecthByListOfIds(ObsIdList));
					ObsIdList.clear();
				}

			}

			if (!bulkAction.isEmpty() && (bulkAction.contains(BULK_ACTION.TRAITS_BULK_POSTING.getAction()))) {
				List<Long> obsIdList = new ArrayList<Long>();
				Map<String, List<Long>> map = new HashMap<>();
				if (bulkTraits != null && !bulkTraits.isEmpty()) {
					String[] factPairs = bulkTraits.split("\\|");
					for (String pair : factPairs) {
						String[] keyValue = pair.split(":", 2);
						if (keyValue.length == 2) {
							String key = keyValue[0];
							List<Long> values = new ArrayList<>();
							values.addAll(Arrays.stream(keyValue[1].split(",")).map(Long::valueOf)
									.collect(Collectors.toList()));
							map.put(key, values);
						}
					}
					if (!oservationIds.isEmpty()) {
						obsIdList = oservationIds;

					}
					if (Boolean.TRUE.equals(selectAll)) {
						MapResponse result = esService.search(index, type, geoAggregationField, geoAggegationPrecision,
								onlyFilteredAggregation, termsAggregationField, geoShapeFilterField, mapSearchQuery);
						List<MapDocument> documents = result.getDocuments();
						for (MapDocument document : documents) {
							ObservationListMinimalData data = objectMapper.readValue(
									String.valueOf(document.getDocument()), ObservationListMinimalData.class);
							obsIdList.add(data.getObservationId());
						}
					}
					List<Long> ObsBatchList = new ArrayList<Long>();
					;
					Integer count = 0;

					while (count < obsIdList.size()) {
						ObsBatchList.add(obsIdList.get(count));

						if (ObsBatchList.size() >= 200) {
							bulkTraitsAction(ObsBatchList, map);
							ObsBatchList.clear();
						}
						count++;
					}

					bulkTraitsAction(ObsBatchList, map);
					ObsBatchList.clear();
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
		List<SpeciesGroup> SpeciesGroupList = observationService.getAllSpeciesGroup();
		Map<Long, String> speciesNameMapping = new HashMap<>();
		for (SpeciesGroup speciesGroup : SpeciesGroupList) {
			speciesNameMapping.put(speciesGroup.getId(), speciesGroup.getName());
		}
		for (Observation observation : obsList) {
			Long previousGroupId = observation.getGroupId();
			observation.setGroupId(sGroupId);
			observation.setLastRevised(new Date());
			observation = observationDao.update(observation);
			String description = speciesNameMapping.get(previousGroupId) + " to " + speciesNameMapping.get(sGroupId);
			ActivityLoggingData activityLogging = new ActivityLoggingData();
			activityLogging.setActivityDescription(description);
			activityLogging.setActivityId(observation.getId());
			activityLogging.setActivityType("Observation species group updated");
			activityLogging.setRootObjectId(observation.getId());
			activityLogging.setRootObjectType("observation");
			activityLogging.setSubRootObjectId(observation.getId());
			activityLogging.setMailData(observationService.generateMailData(observation.getId()));
			activityService = headers.addActivityHeaders(activityService, requestAuthHeader);
			try {
				activityService.logActivity(activityLogging);
			} catch (ApiException e) {
				logger.error(e.getMessage());
			}
		}
		List<Long> obsIds = obsList.stream().map(item -> item.getId()).collect(Collectors.toList());
		String observationList = StringUtils.join(obsIds, ',');
		ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
		Thread esThreadUpdate = new Thread(updateThread);
		esThreadUpdate.start();
	}

	private void bulkRecoSuggestionAction(List<Long> obsListIds, RecoData recoData) {
		for (Long obId : obsListIds) {
			try {
				Long userId = Long.parseLong(profile.getId());
				RecoCreate recoCreate = observationMapperHelper.createRecoMapping(recoData);
				recoService.createRecoVote(request, userId, obId, recoData.getScientificNameTaxonId(), recoCreate,
						false);

			} catch (Exception e) {
				logger.error(e.getMessage());
			}
		}
	}

	private void bulkTraitsAction(List<Long> obsListIds, Map<String, List<Long>> traits) {
		for (Long obId : obsListIds) {
			try {
				FactsCreateData factsCreateData = new FactsCreateData();
				factsCreateData.setFactValuePairs(traits);
				factsCreateData.setFactValueString(new HashMap<>());
				factsCreateData.setMailData(null);
				traitService = headers.addTraitsHeaders(traitService, requestAuthHeader);
				traitService.createFacts("species.participation.Observation", String.valueOf(obId), factsCreateData);
				String observationList = StringUtils.join(obsListIds, ',');
				ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
				Thread esThreadUpdate = new Thread(updateThread);
				esThreadUpdate.start();

			} catch (Exception e) {
				logger.error(e.getMessage());
			}
		}
	}

	private void bulkValidateAction(List<Observation> obsList) {
		for (Observation observation : obsList) {
			Integer count = 0;
			try {
				Long userId = Long.parseLong(profile.getId());
				List<RecoIbp> allRecoVotes = recoService.allRecoVote(observation.getId());
				for (RecoIbp recoVoteData : allRecoVotes) {
					RecoSet recoSet = new RecoSet();
					recoSet.setScientificName(recoVoteData.getScientificName());
					recoSet.setTaxonId(recoVoteData.getTaxonId());
					recoSet.setCommonName(recoVoteData.getCommonName());
					ObservationUserPermission permission = observationService.getUserPermissions(requestAuthHeader,
							profile, observation.getId().toString(), userId, recoSet.getTaxonId().toString());
					List<Long> permissionList = new ArrayList<Long>();
					if (permission.getValidatePermissionTaxon() != null)
						permissionList = permission.getValidatePermissionTaxon();
					if (permissionList.contains(recoSet.getTaxonId())) {
						if (!(observation.getIsLocked()) && count==0) {
							Recommendation scientificNameReco = new Recommendation();
							List<Recommendation> scientificNameRecoList = new ArrayList<Recommendation>();
							List<Recommendation> commonNameRecoList = new ArrayList<Recommendation>();
							if (recoSet.getTaxonId() != null) {
								scientificNameReco = recoDao.findRecoByTaxonId(recoSet.getTaxonId(), true);
								scientificNameRecoList.add(scientificNameReco);
							}
							if (recoSet.getScientificName() != null && recoSet.getScientificName().trim().length() != 0
									&& scientificNameReco.getId() == null)
								scientificNameRecoList = recoDao.findByRecoName(recoSet.getScientificName(), true);

							if (recoSet.getCommonName() != null && recoSet.getCommonName().trim().length() != 0)
								commonNameRecoList = recoDao.findByRecoName(recoSet.getCommonName(), false);

							List<RecommendationVote> recoVoteList = recoVoteDao
									.findRecoVoteOnObservation(observation.getId());
							List<RecommendationVote> filteredList = new ArrayList<RecommendationVote>();
							for (RecommendationVote recoVote : recoVoteList) {
								if (scientificNameRecoList.isEmpty()) {
									for (Recommendation reco : commonNameRecoList) {
										if (recoVote.getRecommendationId().equals(reco.getId())) {
											filteredList.add(recoVote);
										}
									}
								} else {
									for (Recommendation reco : scientificNameRecoList) {
										if (recoVote.getRecommendationId().equals(reco.getId())) {
											filteredList.add(recoVote);
										}
									}
								}

							}
							RecommendationVote recoVote = new RecommendationVote();
							if (filteredList.size() == 1) {
								recoVote = filteredList.get(0);
							} else {
								List<RecommendationVote> finalFilteredList = new ArrayList<RecommendationVote>();
								for (RecommendationVote rVote : filteredList) {
									for (Recommendation reco : commonNameRecoList) {
										if (rVote.getCommonNameRecoId() != null
												&& rVote.getCommonNameRecoId().equals(reco.getId()))
											finalFilteredList.add(rVote);
									}
								}
								if (finalFilteredList.isEmpty())
									recoVote = filteredList.get(0);
								else
									recoVote = finalFilteredList.get(0);
							}

							if (recoVote != null) {
								RecommendationVote previousVote = recoVoteDao
										.findRecoVoteIdByRecoId(observation.getId(), userId, null, null);

								if (previousVote == null || !(previousVote.getRecommendationId()
										.equals(recoVote.getRecommendationId()))) {
									if (previousVote != null)
										recoVoteDao.delete(previousVote);

									recoVote.setId(null);
									recoVote.setAuthorId(userId);
									recoVote = recoVoteDao.save(recoVote);

								}
								Long maxVotedReco = recoVote.getRecommendationId();
								observation.setIsLocked(true);
								observation.setMaxVotedRecoId(maxVotedReco);
								observation.setLastRevised(new Date());
								observation.setNoOfIdentifications(recoVoteDao.findRecoVoteCount(observation.getId()));
								observationDao.update(observation);
								count = count+1;
								RecoVoteActivity rvActivity = new RecoVoteActivity();

								if (recoSet.getTaxonId() != null) {
									taxonomyService = headers.addTaxonomyHeader(taxonomyService, requestAuthHeader);
									TaxonomyDefinition taxonomyDef = taxonomyService
											.getTaxonomyConceptName(recoSet.getTaxonId().toString());
									rvActivity.setScientificName((taxonomyDef.getItalicisedForm() != null
											&& !taxonomyDef.getItalicisedForm().isEmpty())
													? taxonomyDef.getItalicisedForm()
													: taxonomyDef.getNormalizedForm());

								}
								if (recoSet.getCommonName() != null && recoSet.getCommonName().trim().length() > 0)
									rvActivity.setCommonName(recoSet.getCommonName());
								if (recoSet.getScientificName() != null
										&& recoSet.getScientificName().trim().length() > 0)
									rvActivity.setGivenName(recoSet.getScientificName());

								String description = objectMapper.writeValueAsString(rvActivity);
								ActivityLoggingData activityLogging = new ActivityLoggingData();
								activityLogging.setActivityDescription(description);
								activityLogging.setActivityId(recoVote.getId());
								activityLogging.setActivityType("obv locked");
								activityLogging.setRootObjectId(observation.getId());
								activityLogging.setRootObjectType("observation");
								activityLogging.setSubRootObjectId(observation.getId());
								activityLogging.setMailData(observationService.generateMailData(observation.getId()));
								activityService = headers.addActivityHeaders(activityService, requestAuthHeader);
								try {
									activityService.logActivity(activityLogging);
								} catch (ApiException e) {
									logger.error(e.getMessage());
								}
							}

						}
					}

				}
				List<Long> obsIds = obsList.stream().map(item -> item.getId()).collect(Collectors.toList());
				String observationList = StringUtils.join(obsIds, ',');
				ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
				Thread esThreadUpdate = new Thread(updateThread);
				esThreadUpdate.start();

			} catch (Exception e) {
				logger.error(e.getMessage());
			}
		}
	}
	
	private UniqueRecoVote mapToUniqueRecoVote(RecommendationVote recommendationVote, Recommendation reco) {

		UniqueRecoVote uniqueRecoVote = new UniqueRecoVote();
		Long recoId = recommendationVote.getRecommendationId();
		Long cnId = recommendationVote.getCommonNameRecoId();
		if (cnId == null) {
			uniqueRecoVote.setIsScientificName(true);
			uniqueRecoVote.setIsCommonName(false);
		} else if (recoId.equals(cnId)) {
			uniqueRecoVote.setIsScientificName(false);
			uniqueRecoVote.setIsCommonName(true);
		} else {
			uniqueRecoVote.setIsScientificName(true);
			uniqueRecoVote.setIsCommonName(true);
		}
		if (reco.getTaxonConceptId() != null)
			uniqueRecoVote.setIsTaxon(true);
		else
			uniqueRecoVote.setIsTaxon(false);
		uniqueRecoVote.setIsAccepted(reco.isAcceptedName());
		uniqueRecoVote.setVoteCount(1);
		uniqueRecoVote.setLastestDate(recommendationVote.getVotedOn());
		uniqueRecoVote.setRecoId(recoId);

		return uniqueRecoVote;
	}
	
	private UniqueRecoVote updateToUniqueRecoVote(UniqueRecoVote originalRecoVote, UniqueRecoVote uniqueRecoVote) {
		if (uniqueRecoVote.getIsCommonName())
			originalRecoVote.setIsCommonName(true);
		if (uniqueRecoVote.getIsScientificName())
			originalRecoVote.setIsScientificName(true);
		if (uniqueRecoVote.getIsTaxon())
			originalRecoVote.setIsTaxon(true);
		if (uniqueRecoVote.getLastestDate().getTime() > originalRecoVote.getLastestDate().getTime())
			originalRecoVote.setLastestDate(uniqueRecoVote.getLastestDate());
		if (uniqueRecoVote.getIsAccepted())
			originalRecoVote.setIsAccepted(true);
		originalRecoVote.setVoteCount(originalRecoVote.getVoteCount() + 1);
		return originalRecoVote;
	}

	private void bulkUnlockAction(List<Observation> obsList) {
		Long userId = Long.parseLong(profile.getId());
		for (Observation observation : obsList) {
			try {
				if (observation.getIsLocked()) {
					Recommendation recoSet = recoDao.findById(observation.getMaxVotedRecoId());
					ObservationUserPermission permission = observationService.getUserPermissions(requestAuthHeader, profile,
							observation.getId().toString(), userId, recoSet.getTaxonConceptId().toString());
					List<Long> permissionList = new ArrayList<Long>();
					if (permission.getValidatePermissionTaxon() != null)
						permissionList = permission.getValidatePermissionTaxon();
					if (permissionList.contains(recoSet.getTaxonConceptId())) {
						List<RecommendationVote> recoVoteList = recoVoteDao.findRecoVoteOnObservation(observation.getId());
						if (!(recoVoteList.isEmpty())) {
							Map<Long, UniqueRecoVote> uniqueRecoVotes = new HashMap<Long, UniqueRecoVote>();

							for (RecommendationVote recommendationVote : recoVoteList) {
								Long recoId = recommendationVote.getRecommendationId();
								Recommendation reco = recoDao.findById(recoId);

								UniqueRecoVote uniqueRecoVote = mapToUniqueRecoVote(recommendationVote, reco);
								if (uniqueRecoVotes.containsKey(recoId)) {
									UniqueRecoVote originalRecoVote = uniqueRecoVotes.get(recoId);
									originalRecoVote = updateToUniqueRecoVote(originalRecoVote, uniqueRecoVote);
								} else {
									uniqueRecoVotes.put(recoId, uniqueRecoVote);
								}
							}
							UniqueRecoVote maxRecoVote = null;
							for (Entry<Long, UniqueRecoVote> entry : uniqueRecoVotes.entrySet()) {
								int value = entry.getValue().compareTo(maxRecoVote);
								if (value > 0) {
									maxRecoVote = entry.getValue();
								}
							}
							if (maxRecoVote != null) {
							observation.setIsLocked(false);	
							observation.setMaxVotedRecoId(maxRecoVote.getRecoId());
							observation.setLastRevised(new Date());
							observation.setNoOfIdentifications(recoVoteDao.findRecoVoteCount(observation.getId()));
							observationDao.update(observation);
							}
						}
						
						String description = "";

						RecoVoteActivity rvActivity = new RecoVoteActivity();
						String scientificName = "";
						String commonName = "";

						if (recoSet.getTaxonConceptId() != null) {
							TaxonomyDefinition taxonomyDefinition = taxonomyService
									.getTaxonomyConceptName(recoSet.getTaxonConceptId().toString());
							scientificName = (taxonomyDefinition.getItalicisedForm() != null
									&& !taxonomyDefinition.getItalicisedForm().isEmpty())
											? taxonomyDefinition.getItalicisedForm()
											: taxonomyDefinition.getNormalizedForm();

						}
						else {
							scientificName = recoSet.getName();
						}
						List<RecommendationVote> recoVotes = recoVoteDao.findByRecommendationId(observation.getId(), observation.getMaxVotedRecoId());
						
						for (RecommendationVote recoVote : recoVotes) {
							if (recoVote.getCommonNameRecoId() != null) {
								String tempName = recoDao.findById(recoVote.getCommonNameRecoId()).getName();
								if (!commonName.contains(tempName))
									commonName = commonName + tempName + "||";
							}
						}
						if (!(commonName.isEmpty()))
							commonName = commonName.substring(0, commonName.length() - 2);
						
						rvActivity.setScientificName(scientificName);
						rvActivity.setCommonName(commonName);
						rvActivity.setGivenName(scientificName);

						description = objectMapper.writeValueAsString(rvActivity);
						ActivityLoggingData activityLogging = new ActivityLoggingData();
						activityLogging.setActivityDescription(description);
						activityLogging.setActivityId(observation.getMaxVotedRecoId());
						activityLogging.setActivityType("obv unlocked");
						activityLogging.setRootObjectId(observation.getId());
						activityLogging.setRootObjectType("observation");
						activityLogging.setSubRootObjectId(observation.getId());
						activityLogging.setMailData(observationService.generateMailData(observation.getId()));
						activityService = headers.addActivityHeaders(activityService, requestAuthHeader);
						try {
							activityService.logActivity(activityLogging);
						} catch (ApiException e) {
							logger.error(e.getMessage());
						}
						
						List<Long> obsIds = obsList.stream().map(item -> item.getId()).collect(Collectors.toList());
						String observationList = StringUtils.join(obsIds, ',');
						ESBulkUploadThread updateThread = new ESBulkUploadThread(esUpdate, observationList);
						Thread esThreadUpdate = new Thread(updateThread);
						esThreadUpdate.start();
					}
				}
			} catch(Exception e) {
				logger.error(e.getMessage());
				}
			}
	}

}
