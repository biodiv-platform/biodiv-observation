/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.activity.pojo.Activity;
import com.strandls.activity.pojo.CommentLoggingData;
import com.strandls.activity.pojo.MailData;
import com.strandls.activity.pojo.ObservationMailData;
import com.strandls.activity.pojo.UserGroupMailData;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.dataTable.controllers.DataTableServiceApi;
import com.strandls.dataTable.pojo.DataTableWkt;
import com.strandls.esmodule.ApiException;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.AuthorUploadedObservationInfo;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MaxVotedRecoFreq;
import com.strandls.esmodule.pojo.ObservationInfo;
import com.strandls.esmodule.pojo.ObservationNearBy;
import com.strandls.esmodule.pojo.UserScore;
import com.strandls.integrator.controllers.IntergratorServicesApi;
import com.strandls.integrator.pojo.CheckFilterRule;
import com.strandls.integrator.pojo.UserGroupObvRuleData;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dao.ObservationDownloadLogDAO;
import com.strandls.observation.dao.RecommendationVoteDao;
import com.strandls.observation.es.util.ObservationIndex;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.es.util.RabbitMQProducer;
import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.pojo.ListPagePermissions;
import com.strandls.observation.pojo.MaxVotedRecoPermission;
import com.strandls.observation.pojo.ObservatioImageResourceCropinfo;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreateUGContext;
import com.strandls.observation.pojo.ObservationUGContextCreatePageData;
import com.strandls.observation.pojo.ObservationUpdateData;
import com.strandls.observation.pojo.ObservationUserPageInfo;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.Resources;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.pojo.UniqueSpeciesInfo;
import com.strandls.observation.service.ObservationCreateService;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.Resource;
import com.strandls.resource.pojo.ResourceCropInfo;
import com.strandls.resource.pojo.ResourceData;
import com.strandls.resource.pojo.ResourceRating;
import com.strandls.taxonomy.controllers.SpeciesServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyTreeServicesApi;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.taxonomy.pojo.SpeciesPermission;
import com.strandls.taxonomy.pojo.TaxonTree;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.FactsUpdateData;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.Follow;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.controller.CustomFieldServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.CustomFieldDetails;
import com.strandls.userGroup.pojo.CustomFieldFactsInsert;
import com.strandls.userGroup.pojo.CustomFieldFactsInsertData;
import com.strandls.userGroup.pojo.CustomFieldObservationData;
import com.strandls.userGroup.pojo.CustomFieldPermission;
import com.strandls.userGroup.pojo.CustomFieldValues;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.FeaturedCreate;
import com.strandls.userGroup.pojo.FeaturedCreateData;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.userGroup.pojo.UserGroupMappingCreateData;
import com.strandls.userGroup.pojo.UserGroupMemberRole;
import com.strandls.userGroup.pojo.UserGroupObvFilterData;
import com.strandls.userGroup.pojo.UserGroupPermissions;
import com.strandls.userGroup.pojo.UserGroupSpeciesGroup;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.FlagCreateData;
import com.strandls.utility.pojo.FlagIbp;
import com.strandls.utility.pojo.FlagShow;
import com.strandls.utility.pojo.Language;
import com.strandls.utility.pojo.Tags;
import com.strandls.utility.pojo.TagsMapping;
import com.strandls.utility.pojo.TagsMappingData;

import net.minidev.json.JSONArray;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServiceImpl implements ObservationService {

	private final Logger logger = LoggerFactory.getLogger(ObservationServiceImpl.class);

	@Inject
	private LogActivities logActivity;

	@Inject
	private ObservationDAO observationDao;

	@Inject
	private TraitsServiceApi traitService;

	@Inject
	private TaxonomyServicesApi taxonomyService;

	@Inject
	private ResourceServicesApi resourceService;

	@Inject
	private UserGroupSerivceApi userGroupService;

	@Inject
	private CustomFieldServiceApi cfService;

	@Inject
	private LayerServiceApi layerService;

	@Inject
	private EsServicesApi esService;

	@Inject
	private RecommendationServiceImpl recoService;

	@Inject
	private UtilityServiceApi utilityServices;

	@Inject
	private UserServiceApi userService;

	@Inject
	private ObservationMapperHelper observationHelper;

	@Inject
	private RecommendationVoteDao recoVoteDao;

	@Inject
	private RabbitMQProducer producer;

	@Inject
	private MailMetaDataConverter converter;

	@Inject
	private ActivitySerivceApi activityService;

	@Inject
	private Headers headers;

	@Inject
	private ObjectMapper objectMapper;

	@Inject
	private ObservationDownloadLogDAO downloadLogDao;

	@Inject
	private SpeciesServicesApi speciesGroupService;

	@Inject
	private TaxonomyTreeServicesApi taxonomyTreeService;

	@Inject
	private DataTableServiceApi dataTableService;

	@Inject
	private IntergratorServicesApi intergratorService;

	@Inject
	private ObservationCreateService observationCreateService;

	@Override
	public ShowData findById(Long id) {

		InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

		Properties properties = new Properties();
		try {
			properties.load(in);
		} catch (IOException e) {
			logger.error(e.getMessage());
		}

		List<FactValuePair> facts;
		List<ResourceData> observationResource;
		List<UserGroupIbp> userGroups;
		List<CustomFieldObservationData> customField = null;
		ObservationLocationInfo layerInfo;
		ObservationInfo esLayerInfo = null;
		RecoIbp reco = null;
		List<FlagShow> flag = new ArrayList<FlagShow>();
		List<Tags> tags;
		List<Featured> fetaured;
		UserIbp userInfo;
		List<RecoIbp> allRecoVotes = null;
		Map<String, String> authorScore = null;
		List<AllRecoSugguestions> recoaggregated = null;
		Observation observation = observationDao.findById(id);
		DataTableWkt dataTable = null;
		Map<String, Object> checkListAnnotation = new HashMap<String, Object>();
		if (observation != null && observation.getIsDeleted() != true) {
			try {
				in.close();
				UserScore score = esService.getUserScore("eaf", "er", observation.getAuthorId().toString(), "f");
				if (score.getRecord() != null && !score.getRecord().isEmpty()) {
					authorScore = score.getRecord().get(0).get("details");
				}
				if (observation.getDataTableId() != null) {
					dataTable = dataTableService.showDataTable(observation.getDataTableId().toString());
				}
				facts = traitService.getFacts("species.participation.Observation", id.toString());
				observationResource = resourceService.getImageResource("observation", id.toString());
				userGroups = userGroupService.getObservationUserGroup(id.toString());
				customField = cfService.getObservationCustomFields(id.toString());

				layerInfo = layerService.getLayerInfo(String.valueOf(observation.getLatitude()),
						String.valueOf(observation.getLongitude()));
				if (observation.getFlagCount() > 0)
					flag = utilityServices.getFlagByObjectType("observation", id.toString());
				tags = utilityServices.getTags("observation", id.toString());
				userInfo = userService.getUserIbp(observation.getAuthorId().toString());
				fetaured = userGroupService.getAllFeatured("species.participation.Observation", id.toString());
				if (observation.getMaxVotedRecoId() != null) {
					reco = recoService.fetchRecoName(id, observation.getMaxVotedRecoId());
					esLayerInfo = esService.getObservationInfo(ObservationIndex.INDEX.getValue(),
							ObservationIndex.TYPE.getValue(), observation.getMaxVotedRecoId().toString(), true);
					allRecoVotes = recoService.allRecoVote(id);
					recoaggregated = aggregateAllRecoSuggestions(allRecoVotes);
				}

				observation.setVisitCount(observation.getVisitCount() + 1);
				observationDao.update(observation);

				if (observation.getGeoPrivacy()) {
					Map<String, Double> latlon = observationHelper.getRandomLatLong(observation.getLatitude(),
							observation.getLongitude());
					observation.setLatitude(latlon.get("lat"));
					observation.setLongitude(latlon.get("lon"));
				}

				if (observation.getChecklistAnnotations() != null && !observation.getChecklistAnnotations().isEmpty()) {
					checkListAnnotation = objectMapper.readValue(observation.getChecklistAnnotations(),
							new TypeReference<Map<String, Object>>() {
							});
				}

				List<ObservationNearBy> observationNearBy = esService.getNearByObservation(
						ObservationIndex.INDEX.getValue(), ObservationIndex.TYPE.getValue(),
						observation.getLatitude().toString(), observation.getLongitude().toString());

				Integer activityCount = activityService.getActivityCount("observation", observation.getId().toString());
				return new ShowData(observation, facts, observationResource, userGroups, customField, layerInfo,
						esLayerInfo, reco, flag, tags, fetaured, userInfo, authorScore, recoaggregated,
						observationNearBy, dataTable, checkListAnnotation, activityCount);
			} catch (Exception e) {
				logger.error(e.getMessage());
			}
		}
		return null;
	}

	@Override
	public List<AllRecoSugguestions> aggregateAllRecoSuggestions(List<RecoIbp> allRecoVote) {
		List<AllRecoSugguestions> result = new ArrayList<AllRecoSugguestions>();
		for (RecoIbp reco : allRecoVote) {
			int updated = 0;
			for (AllRecoSugguestions recoSuggestions : result) {
				if (reco.getTaxonId() != null && recoSuggestions.getTaxonId() != null) {
					if (reco.getTaxonId().equals(recoSuggestions.getTaxonId())) {
						result.remove(recoSuggestions);
						if (recoSuggestions.getCommonName().trim().length() == 0
								&& reco.getCommonName().trim().length() != 0)
							recoSuggestions.setCommonName(reco.getCommonName());
						if (recoSuggestions.getScientificName().trim().length() == 0
								&& reco.getScientificName().trim().length() != 0)
							recoSuggestions.setScientificName(reco.getScientificName());
						if (recoSuggestions.getSpeciesId() == null && reco.getSpeciesId() != null)
							recoSuggestions.setSpeciesId(reco.getSpeciesId());
						List<UserIbp> userList = recoSuggestions.getUserList();
						userList.add(reco.getUserIbp());
						recoSuggestions.setUserList(userList);
						result.add(recoSuggestions);
						updated = 1;
						break;
					}
				} else if (reco.getTaxonId() == null && recoSuggestions.getTaxonId() == null) {
					if (recoSuggestions.getCommonName().equals(reco.getCommonName())
							|| recoSuggestions.getScientificName().equals(reco.getScientificName())) {
						result.remove(recoSuggestions);
						if (recoSuggestions.getCommonName().trim().length() == 0
								&& reco.getCommonName().trim().length() != 0)
							recoSuggestions.setCommonName(reco.getCommonName());
						if (recoSuggestions.getScientificName().trim().length() == 0
								&& reco.getScientificName().trim().length() != 0)
							recoSuggestions.setScientificName(reco.getScientificName());
						if (recoSuggestions.getSpeciesId() == null && reco.getSpeciesId() != null)
							recoSuggestions.setSpeciesId(reco.getSpeciesId());
						List<UserIbp> userList = recoSuggestions.getUserList();
						userList.add(reco.getUserIbp());
						recoSuggestions.setUserList(userList);
						result.add(recoSuggestions);
						updated = 1;
						break;

					}
				}

			}
			if (updated == 0) {
				List<UserIbp> userList = new ArrayList<UserIbp>();
				userList.add(reco.getUserIbp());
				AllRecoSugguestions suggestion = new AllRecoSugguestions(reco.getCommonName(), reco.getScientificName(),
						reco.getTaxonId(), reco.getSpeciesId(), userList);
				result.add(suggestion);
			}
		}

		return result;

	}

	@Override
	public Long updateSGroup(HttpServletRequest request, Long observationId, Long sGroupId) {
		Map<String, Object> partialEsDoc = new HashMap<>();
		Observation observation = observationDao.findById(observationId);
		Long previousGroupId = observation.getGroupId();
		observation.setGroupId(sGroupId);
		observation.setLastRevised(new Date());
		observation = observationDao.update(observation);
		List<SpeciesGroup> SpeciesGroupList = getAllSpeciesGroup();
		String previousGroupName = "";
		String newGroupName = "";
		String sgroupFilter = "";

		for (SpeciesGroup speciesGroup : SpeciesGroupList) {
			if (speciesGroup.getId().equals(previousGroupId))
				previousGroupName = speciesGroup.getName();
			if (speciesGroup.getId().equals(sGroupId)) {
				newGroupName = speciesGroup.getName();
				sgroupFilter = speciesGroup.getId() + "|" + newGroupName + "|" + speciesGroup.getGroupOrder();
			}
		}
		String description = previousGroupName + " to " + newGroupName;

		partialEsDoc.put(ObservationIndex.SGROUP.getValue(), sGroupId);
		partialEsDoc.put(ObservationIndex.SPECIESNAMES.getValue(), newGroupName);
		partialEsDoc.put("sgroup_filter", sgroupFilter);

		try {
			esService.update(ObservationIndex.INDEX.getValue(), ObservationIndex.TYPE.getValue(),
					observation.getId().toString(), partialEsDoc);
		} catch (ApiException e) {
			logger.error(e.getMessage());
		}

		// produceToRabbitMQ(observationId.toString(), "Species Group");

		/*
		 * The above line is commented out because we are trying to perform a partial
		 * update of es document instead of running the entire elastic stub query and
		 * updating the entire document. It is, our first attempt towards decoupling
		 * that big query into smaller chunks.
		 */
		logActivity.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), description, observationId, observationId,
				"observation", observationId, "Observation species group updated", generateMailData(observationId));
		return observation.getGroupId();
	}

	@Override
	public Long updateMaxVotedReco(Long observationId, Long maxVotedReco) {
		Observation observation = observationDao.findById(observationId);
		if (observation.getMaxVotedRecoId() == null || !observation.getMaxVotedRecoId().equals(maxVotedReco)) {
			observation.setMaxVotedRecoId(maxVotedReco);
			observation.setLastRevised(new Date());
			observation.setNoOfIdentifications(recoVoteDao.findRecoVoteCount(observationId));
			observationDao.update(observation);
			List<Observation> observationList = new ArrayList<Observation>();
			observationList.add(observation);
			updateGeoPrivacy(observationList);
			return maxVotedReco;
		}
		return observation.getMaxVotedRecoId();
	}

	@Override
	public List<Tags> updateTags(HttpServletRequest request, TagsMapping tagsMapping) {
		List<Tags> result = null;
		try {
			TagsMappingData tagsMappingData = new TagsMappingData();
			tagsMappingData.setTagsMapping(tagsMapping);
			tagsMappingData.setMailData(converter.utilityMetaData(generateMailData(tagsMapping.getObjectId())));
			utilityServices = headers.addUtilityHeaders(utilityServices, request.getHeader(HttpHeaders.AUTHORIZATION));
			result = utilityServices.updateTags("observation", tagsMappingData);
			Observation observation = observationDao.findById(tagsMapping.getObjectId());
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(tagsMapping.getObjectId().toString(), "Tags");
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<FactValuePair> updateTraits(HttpServletRequest request, String observationId, String traitId,
			FactsUpdateData updateData) {

		List<FactValuePair> facts = null;
		try {
			updateData.setMailData(converter.traitMetaData(generateMailData(Long.parseLong(observationId))));
			traitService = headers.addTraitsHeaders(traitService, request.getHeader(HttpHeaders.AUTHORIZATION));
			facts = traitService.updateTraits("species.participation.Observation", observationId, traitId, updateData);
			Observation observation = observationDao.findById(Long.parseLong(observationId));
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(observationId, "Traits");
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return facts;
	}

	public UserGroupObvRuleData getUGObvRuleData(Observation observation) {
		UserGroupObvRuleData ugFilterData = new UserGroupObvRuleData();
		Long taxonomyId = null;
		if (observation.getMaxVotedRecoId() != null)
			taxonomyId = recoService.fetchTaxonId(observation.getMaxVotedRecoId());
		ugFilterData.setObservationId(observation.getId());
		ugFilterData.setCreatedOnDate(observation.getCreatedOn());
		ugFilterData.setLatitude(observation.getLatitude());
		ugFilterData.setLongitude(observation.getLongitude());
		ugFilterData.setObservedOnDate(observation.getFromDate());
		ugFilterData.setAuthorId(observation.getAuthorId());
		ugFilterData.setTaxonomyId(taxonomyId);

		return ugFilterData;
	}

	public UserGroupObvFilterData getUGFilterObvData(Observation observation) {
		UserGroupObvFilterData ugFilterData = new UserGroupObvFilterData();
		Long taxonomyId = null;
		if (observation.getMaxVotedRecoId() != null)
			taxonomyId = recoService.fetchTaxonId(observation.getMaxVotedRecoId());
		ugFilterData.setObservationId(observation.getId());
		ugFilterData.setCreatedOnDate(observation.getCreatedOn());
		ugFilterData.setLatitude(observation.getLatitude());
		ugFilterData.setLongitude(observation.getLongitude());
		ugFilterData.setObservedOnDate(observation.getFromDate());
		ugFilterData.setAuthorId(observation.getAuthorId());
		ugFilterData.setTaxonomyId(taxonomyId);

		return ugFilterData;
	}

	@Override
	public List<UserGroupIbp> updateUserGroup(HttpServletRequest request, String observationId,
			List<Long> userGroupList) throws Exception {

		List<UserGroupIbp> result = null;

		// filter usergroup by rule eligility
		CheckFilterRule checkFilterRule = new CheckFilterRule();
		checkFilterRule.setUserGroupId(userGroupList);
		UserGroupObvRuleData ugObvFilterData = getUGObvRuleData(observationDao.findById(Long.parseLong(observationId)));
		List<FactValuePair> traits = traitService.getFacts("species.participation.Observation", observationId);
		Map<String, List<Long>> facts = traits.stream()
	    .collect(Collectors.groupingBy(
	    		trait -> trait.getNameId().toString(), 
	            Collectors.mapping(FactValuePair::getValueId, Collectors.toList())
	        ));
		ugObvFilterData.setTraits(facts);
		checkFilterRule.setUgObvFilterData(ugObvFilterData);
		intergratorService = headers.addIntergratorHeader(intergratorService,
				request.getHeader(HttpHeaders.AUTHORIZATION));
		List<Long> verifiedUgIds = intergratorService.checkUserGroupEligiblity(checkFilterRule);

		if (!userGroupList.isEmpty() && (verifiedUgIds == null || verifiedUgIds.isEmpty())) {

			throw new Exception("Unable to update usergroup,Check group rules");

		}
		if (verifiedUgIds != null) {
			UserGroupMappingCreateData userGroupData = new UserGroupMappingCreateData();
			userGroupData.setUserGroups(verifiedUgIds);
			userGroupData.setUgFilterData(getUGFilterObvData(observationDao.findById(Long.parseLong(observationId))));
			userGroupData.setMailData(converter.userGroupMetadata(generateMailData(Long.parseLong(observationId))));
			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			result = userGroupService.updateUserGroupMapping(observationId, userGroupData);
			Observation observation = observationDao.findById(Long.parseLong(observationId));
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(observationId, "UserGroups");
		}

		return result;
	}

	@Override
	public List<SpeciesGroup> getAllSpeciesGroup() {

		List<SpeciesGroup> result = null;
		try {
			result = speciesGroupService.getAllSpeciesGroup();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<Language> getLanguages(Boolean isDirty) {

		List<Language> result = null;
		try {
			result = utilityServices.getAllLanguages(isDirty);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<Featured> createFeatured(HttpServletRequest request, FeaturedCreate featuredCreate) {
		List<Featured> result = null;

		try {
			FeaturedCreateData featuredCreateData = new FeaturedCreateData();
			featuredCreateData.setFeaturedCreate(featuredCreate);
			featuredCreateData.setMailData(converter.userGroupMetadata(generateMailData(featuredCreate.getObjectId())));
			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			result = userGroupService.createFeatured(featuredCreateData);
			Observation observation = observationDao.findById(featuredCreate.getObjectId());
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(observation.getId().toString(), "Featured");
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<Featured> unFeatured(HttpServletRequest request, String observaitonId, List<Long> userGroupList) {
		List<Featured> result = null;
		try {
			UserGroupMappingCreateData userGroupData = new UserGroupMappingCreateData();
			userGroupData.setUserGroups(userGroupList);
			userGroupData.setUgFilterData(null);
			userGroupData.setMailData(converter.userGroupMetadata(generateMailData(Long.parseLong(observaitonId))));
			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			result = userGroupService.unFeatured("observation", observaitonId, userGroupData);
			Observation observation = observationDao.findById(Long.parseLong(observaitonId));
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(observation.getId().toString(), "Featured");
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<TraitsValue> getTraitsValue(HttpServletRequest request, String traitId) {
		List<TraitsValue> result = null;
		try {
			result = traitService.getTraitsValue(traitId);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<TraitsValuePair> getTraitList(String speciesGroupId, String languageId) {

		List<TraitsValuePair> result = null;
		try {
			result = traitService.getTraitList(speciesGroupId, languageId);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<MaxVotedRecoPermission> listMaxRecoVotePermissions(HttpServletRequest request, CommonProfile profile,
			Map<Long, Long> observationTaxonId) {

		try {
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");
			List<MaxVotedRecoPermission> result = new ArrayList<MaxVotedRecoPermission>();
			if (userRole.contains("ROLE_ADMIN")) {
				for (Entry<Long, Long> entry : observationTaxonId.entrySet()) {
					result.add(new MaxVotedRecoPermission(entry.getKey(), true));
				}
			} else {
				for (Entry<Long, Long> entry : observationTaxonId.entrySet()) {

					taxonomyService = headers.addTaxonomyHeader(taxonomyService,
							request.getHeader(HttpHeaders.AUTHORIZATION));
					List<SpeciesPermission> speciesPermssion = speciesGroupService.getSpeciesPermission();
					List<TaxonTree> taxonTree = taxonomyTreeService.getTaxonTree(entry.getValue().toString());
					List<Long> validateAllowed = ValidatePermission(taxonTree, speciesPermssion);
					if (validateAllowed.contains(entry.getValue()))
						result.add(new MaxVotedRecoPermission(entry.getKey(), true));
					else
						result.add(new MaxVotedRecoPermission(entry.getKey(), false));
				}
			}
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public ListPagePermissions getListPagePermissions(HttpServletRequest request, CommonProfile profile,
			Long observationId, String taxonList) {
		try {
			List<Long> validateAllowed = new ArrayList<Long>();
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");
			if (userRole.contains("ROLE_ADMIN")) {
				if (taxonList.trim().length() != 0) {
					for (String s : taxonList.split(",")) {
						validateAllowed.add(Long.parseLong(s));
					}
				}
			} else {

				taxonomyService = headers.addTaxonomyHeader(taxonomyService,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				List<SpeciesPermission> speciesPermssion = speciesGroupService.getSpeciesPermission();

				if (taxonList.trim().length() != 0) {
					taxonomyService = headers.addTaxonomyHeader(taxonomyService,
							request.getHeader(HttpHeaders.AUTHORIZATION));
					List<TaxonTree> taxonTree = taxonomyTreeService.getTaxonTree(taxonList);
					validateAllowed = ValidatePermission(taxonTree, speciesPermssion);
				}
			}
			cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<CustomFieldPermission> cfPermission = cfService.getCustomFieldPermission(observationId.toString());
			ListPagePermissions permissions = new ListPagePermissions(validateAllowed, cfPermission);
			return permissions;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public ObservationUserPermission getUserPermissions(HttpServletRequest request, CommonProfile profile,
			String observationId, Long userId, String taxonList) throws Exception {
		try {
			List<UserGroupIbp> associatedUserGroup = userGroupService.getObservationUserGroup(observationId);
			List<Long> validateAllowed = new ArrayList<Long>();
			List<UserGroupIbp> allowedUserGroup = new ArrayList<UserGroupIbp>();
			List<Long> userGroupFeatureRole = new ArrayList<Long>();
			userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));
			Follow follow = userService.getFollowByObject("observation", observationId);
			taxonomyService = headers.addTaxonomyHeader(taxonomyService, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<SpeciesPermission> speciesPermissions = speciesGroupService.getSpeciesPermission();

			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			UserGroupPermissions userGroupPermission = userGroupService.getUserGroupObservationPermission();

			JSONArray userRole = (JSONArray) profile.getAttribute("roles");
			if (userRole.contains("ROLE_ADMIN")) {
				if (taxonList.trim().length() != 0) {
					for (String s : taxonList.split(",")) {
						validateAllowed.add(Long.parseLong(s));
					}
				}

				allowedUserGroup = userGroupService.getAllUserGroup();
				for (UserGroupIbp ug : allowedUserGroup) {
					userGroupFeatureRole.add(ug.getId());
				}

			} else {
				if (taxonList.trim().length() != 0) {
					taxonomyService = headers.addTaxonomyHeader(taxonomyService,
							request.getHeader(HttpHeaders.AUTHORIZATION));
					List<TaxonTree> taxonTree = taxonomyTreeService.getTaxonTree(taxonList);
					validateAllowed = ValidatePermission(taxonTree, speciesPermissions);

				}

				List<Long> userGroupMember = new ArrayList<Long>();
				for (UserGroupMemberRole userMemberRole : userGroupPermission.getUserMemberRole()) {
					userGroupMember.add(userMemberRole.getUserGroupId());
				}
				String s = userGroupMember.toString();
				if (s.substring(1, s.length() - 1).trim().length() != 0)
					allowedUserGroup = userGroupService.getUserGroupList(s.substring(1, s.length() - 1));

				for (UserGroupMemberRole userFeatureRole : userGroupPermission.getUserFeatureRole()) {
					userGroupFeatureRole.add(userFeatureRole.getUserGroupId());
				}
			}

			List<Long> userGroupIdList = new ArrayList<Long>();
			List<UserGroupIbp> featureableGroup = new ArrayList<UserGroupIbp>();
			if (associatedUserGroup != null && !associatedUserGroup.isEmpty()) {
				for (UserGroupIbp userGroup : associatedUserGroup) {
					userGroupIdList.add(userGroup.getId());
					if (userGroupFeatureRole.contains(userGroup.getId()))
						featureableGroup.add(userGroup);
				}
			}

			cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<CustomFieldPermission> cfPermission = cfService.getCustomFieldPermission(observationId);

			ObservationUserPermission permission = new ObservationUserPermission(validateAllowed, allowedUserGroup,
					featureableGroup, cfPermission, (follow != null) ? true : false);

			return permission;

		} catch (Exception e) {
			logger.error(e.getMessage());
			throw e;
		}
	}

	private List<Long> ValidatePermission(List<TaxonTree> taxonTrees, List<SpeciesPermission> allowedTaxons) {

		List<Long> validateAllowable = new ArrayList<Long>();

		for (SpeciesPermission allowtaxon : allowedTaxons) {
			for (TaxonTree taxonTree : taxonTrees) {
				if (taxonTree.getTaxonList().contains(allowtaxon.getTaxonConceptId())) {
					if (!(validateAllowable.contains(taxonTree.getTaxonId())))
						validateAllowable.add(taxonTree.getTaxonId());
				}
			}
		}
		return validateAllowable;

	}

	@Override
	public List<Tags> getTagsSugguestions(String phrase) {
		try {
			List<Tags> result = utilityServices.getTagsAutoComplete(phrase);
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public List<UserGroupIbp> getUsersGroupList(HttpServletRequest request, CommonProfile profile) {
		try {
			List<UserGroupIbp> allowedUserGroup = null;
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");
			if (userRole.contains("ROLE_ADMIN")) {
				allowedUserGroup = userGroupService.getAllUserGroup();
			} else {

				userGroupService = headers.addUserGroupHeader(userGroupService,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				UserGroupPermissions userGroupPermission = userGroupService.getUserGroupObservationPermission();

				List<Long> userGroupMember = new ArrayList<Long>();
				for (UserGroupMemberRole userMemberRole : userGroupPermission.getUserMemberRole()) {
					userGroupMember.add(userMemberRole.getUserGroupId());
				}
				String s = userGroupMember.toString();
				allowedUserGroup = userGroupService.getUserGroupList(s.substring(1, s.length() - 1));
			}

			return allowedUserGroup;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public String removeObservation(HttpServletRequest request, CommonProfile profile, Long userId,
			Long observationId) {
		try {
			JSONArray userRole = (JSONArray) profile.getAttribute("roles");
			Observation observation = observationDao.findById(observationId);
			if (observation.getId() != null
					&& (observation.getAuthorId().equals(userId) || userRole.contains("ROLE_ADMIN"))) {
				deleteObservation(request, observation, true);
				return "Observation Deleted Succesfully";
			}
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	public Boolean deleteObservation(HttpServletRequest request, Observation observation, Boolean hasMail)
			throws ApiException {

		Long observationId = observation.getId();
		MailData mailData = Boolean.TRUE.equals(hasMail) ? generateMailData(observationId) : null;

		observation.setIsDeleted(true);
		observation = observationDao.update(observation);
		if (Boolean.TRUE.equals(observation.getIsDeleted())) {
			esService.delete(ObservationIndex.INDEX.getValue(), ObservationIndex.TYPE.getValue(),
					observationId.toString());
			logActivity.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), null, observationId, observationId,
					"observation", observationId, "Observation Deleted", mailData);
			return true;
		}

		return false;
	}

	@Override
	public List<FlagShow> createFlag(HttpServletRequest request, Long observationId, FlagIbp flagIbp) {
		try {
			FlagCreateData flagData = new FlagCreateData();
			flagData.setFlagIbp(flagIbp);
			flagData.setMailData(converter.utilityMetaData(generateMailData(observationId)));
			utilityServices = headers.addUtilityHeaders(utilityServices, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<FlagShow> flagList = utilityServices.createFlag("observation", observationId.toString(), flagData);
			int flagCount = 0;
			if (flagList != null)
				flagCount = flagList.size();

			Observation observation = observationDao.findById(observationId);
			observation.setLastRevised(new Date());
			observation.setFlagCount(flagCount);
			observationDao.update(observation);
			produceToRabbitMQ(observationId.toString(), "Flags");
			return flagList;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public List<FlagShow> unFlag(HttpServletRequest request, Long observationId, String flagId) {
		try {

			com.strandls.utility.pojo.MailData mailData = converter.utilityMetaData(generateMailData(observationId));
			utilityServices = headers.addUtilityHeaders(utilityServices, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<FlagShow> result = utilityServices.unFlag("observation", observationId.toString(), flagId, mailData);
			int flagCount = 0;
			if (result != null)
				flagCount = result.size();

			Observation observation = observationDao.findById(observationId);
			observation.setLastRevised(new Date());
			observation.setFlagCount(flagCount);
			observationDao.update(observation);
			produceToRabbitMQ(observationId.toString(), "Flags");
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public Follow followRequest(HttpServletRequest request, Long observationId) {
		try {
			userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));
			Follow result = userService.updateFollow("observation", observationId.toString());
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public Follow unFollowRequest(HttpServletRequest request, Long observationId) {
		try {
			userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));
			Follow result = userService.unfollow("observation", observationId.toString());
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public Long getObservationAuthor(Long observationId) {
		Observation observation = observationDao.findById(observationId);
		Long authorId = observation.getAuthorId();
		return authorId;
	}

	@Override
	public ShowData editObservaitonCore(HttpServletRequest request, CommonProfile profile, Long observationId,
			ObservationUpdateData observationUpdate) throws Exception {

		try {
			JSONArray userRoles = (JSONArray) profile.getAttribute("roles");
			Long userId = Long.parseLong(profile.getId());
			Observation observation = observationDao.findById(observationId);
			if (observation.getAuthorId().equals(userId) || userRoles.contains("ROLE_ADMIN")) {

				if (observation.getDataTableId() == null && observationUpdate.getResources() == null
						&& observationUpdate.getResources().isEmpty()) {
					throw new ObservationInputException("Observation Resources not found");
				}
//				location data
				observation.setPlaceName(observationUpdate.getObservedAt());
				observation.setReverseGeocodedName(observationUpdate.getReverseGeocoded());
				observation.setLocationScale(observationUpdate.getLocationScale());
				observation.setLatitude(observationUpdate.getLatitude());
				observation.setLongitude(observationUpdate.getLongitude());
				observation.setGeoPrivacy(observationUpdate.getHidePreciseLocation());
//				notes
				observation.setNotes(observationUpdate.getNotes());
//				date data
				observation.setFromDate(observationUpdate.getObservedOn());
				observation.setToDate(observationUpdate.getObservedOn());
				observation.setDateAccuracy(observationUpdate.getDateAccuracy());
				observation.setLastRevised(new Date());
				observation.setChecklistAnnotations(observationUpdate.getChecklistAnnotations());
				observation.setBasisOfRecord(observationUpdate.getBasisOfRecord());
//				resource data

				List<Resource> resources = observationUpdate.getResources() != null
						? observationHelper.createResourceMapping(request, userId, observationUpdate.getResources())
						: null;

				if (resources != null && !resources.isEmpty()) {
					resourceService = headers.addResourceHeaders(resourceService,
							request.getHeader(HttpHeaders.AUTHORIZATION));
					resources = resourceService.updateResources("OBSERVATION", String.valueOf(observation.getId()),
							resources);
//					calculate reprImageof observation
					observation = observationHelper.updateObservationResourceCount(observation, resources);

				}
				observationDao.update(observation);

//				---------GEO PRIVACY CHECK------------
				List<Observation> observationList = new ArrayList<Observation>();
				observationList.add(observation);
				updateGeoPrivacy(observationList);
//				------------BG rules-----------------
				UserGroupObvRuleData ugObvFilterData = getUGObvRuleData(observation);
				List<FactValuePair> traits = traitService.getFacts("species.participation.Observation", observationId.toString());
				Map<String, List<Long>> facts = traits.stream()
			    .collect(Collectors.groupingBy(
			    		trait -> trait.getNameId().toString(), 
			            Collectors.mapping(FactValuePair::getValueId, Collectors.toList())
			        ));
				ugObvFilterData.setTraits(facts);
				intergratorService = headers.addIntergratorHeader(intergratorService,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				intergratorService.getFilterRule(ugObvFilterData);

				produceToRabbitMQ(observationId.toString(), "Observation Core-Resource");

				logActivity.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), null, observationId,
						observationId, "observation", observationId, "Observation updated",
						generateMailData(observationId));
				return findById(observationId);
			} else {
				try {
					throw new ObservationInputException("USER NOT ALLOWED TO UPDATE THE OBSERVATION");
				} catch (Exception e) {
					throw e;
				}
			}
		} catch (Exception e) {
			logger.error(e.getMessage());
			throw e;
		}

	}

	@Override
	public ObservationUpdateData getObservationEditPageData(CommonProfile profile, Long observationId)
			throws Exception {
		ObservationUpdateData editData = new ObservationUpdateData();
		try {
			JSONArray userRoles = (JSONArray) profile.getAttribute("roles");
			Long userId = Long.parseLong(profile.getId());
			Observation observation = observationDao.findById(observationId);
			if (observation.getAuthorId().equals(userId) || userRoles.contains("ROLE_ADMIN")) {
//				notes data
				editData.setNotes(observation.getNotes());
//				Date data
				editData.setDateAccuracy(observation.getDateAccuracy());
				editData.setObservedOn(observation.getFromDate());
//				location data
				editData.setObservedAt(observation.getPlaceName());
				editData.setReverseGeocoded(observation.getReverseGeocodedName());
				editData.setLocationScale(observation.getLocationScale());
				editData.setLatitude(observation.getLatitude());
				editData.setLongitude(observation.getLongitude());
				editData.setHidePreciseLocation(observation.getGeoPrivacy());
				editData.setChecklistAnnotations(observation.getChecklistAnnotations());
				editData.setBasisOfRecord(observation.getBasisOfRecord());
				editData.setDataTableId(observation.getDataTableId());

//				resources Data
				List<ResourceData> resourceData = resourceService.getImageResource("observation",
						observationId.toString());
				if (resourceData != null && !resourceData.isEmpty()) {
					editData.setResources(observationHelper.createEditResourceMapping(resourceData));
				}

			} else {
				throw new ObservationInputException("USER NOT ALLOWED TO EDIT THE PAGE");
			}
		} catch (Exception e) {
			logger.error(e.getMessage());
			throw e;
		}
		return editData;
	}

	@Override
	public void applyFilterObservationPosting(String userGroupIds) {
		try {

			Boolean hasNext = true;
			int totalObservation = 0;
			int startPoint = 0;
			while (hasNext) {
				List<Observation> observationList = observationDao.fetchInBatch(startPoint);
				if (observationList.size() != 20000)
					hasNext = false;
				totalObservation = totalObservation + observationList.size();
				startPoint = totalObservation + 1;
				List<UserGroupObvRuleData> ugObvFilterDataList = new ArrayList<UserGroupObvRuleData>();
				for (Observation observation : observationList) {
					UserGroupObvRuleData ugObvFilterData = getUGObvRuleData(observation);
					List<FactValuePair> traits = traitService.getFacts("species.participation.Observation", observation.getId().toString());
					Map<String, List<Long>> facts = traits.stream()
				    .collect(Collectors.groupingBy(
				    		trait -> trait.getNameId().toString(), 
				            Collectors.mapping(FactValuePair::getValueId, Collectors.toList())
				        ));
					ugObvFilterData.setTraits(facts);
					ugObvFilterDataList.add(ugObvFilterData);
				}
				intergratorService.bulkFilterRulePosting(userGroupIds, ugObvFilterDataList);
			}

			System.out.println("Filter Rule Process Completed");

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

	@Override
	public void applyFilterObservationRemoving(String userGroupId) {
		try {

			List<Long> observationIdList = userGroupService.getAllObservation(userGroupId);
			List<Long> idList = new ArrayList<Long>();
			int total = 0;
			while (observationIdList.size() != total) {
				if (observationIdList.size() > 20000) {
					for (int i = 0; i < 200000; i++) {
						idList.add(observationIdList.get(i));
					}

				} else {
					idList.addAll(observationIdList);
				}
				total = total + idList.size();
				List<Observation> observationList = observationDao.fecthByListOfIds(idList);
				List<UserGroupObvRuleData> ugObvFilterDataList = new ArrayList<UserGroupObvRuleData>();
				for (Observation observation : observationList) {
					UserGroupObvRuleData ugObvFilterData = getUGObvRuleData(observation);
					List<FactValuePair> traits = traitService.getFacts("species.participation.Observation", observation.getId().toString());
					Map<String, List<Long>> facts = traits.stream()
				    .collect(Collectors.groupingBy(
				    		trait -> trait.getNameId().toString(), 
				            Collectors.mapping(FactValuePair::getValueId, Collectors.toList())
				        ));
					ugObvFilterData.setTraits(facts);
					ugObvFilterDataList.add(ugObvFilterData);
				}
				intergratorService.bulkFilterRuleRemoving(userGroupId, ugObvFilterDataList);
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

	@Override
	public void applyGeoPrivacyObservaiton() {
		try {
			Boolean hasNext = true;
			int totalObservation = 0;
			int startPoint = 0;
			while (hasNext) {

				System.out.println("--------START----------");
				System.out.println("START POINT : " + startPoint);
				System.out.println("total Observation till this point : " + totalObservation);
				System.out.println("---------END-----------");
				List<Observation> observationList = observationDao.fetchInBatch(startPoint);
				if (observationList.size() != 20000)
					hasNext = false;
				totalObservation = totalObservation + observationList.size();
				startPoint = totalObservation + 1;
				updateGeoPrivacy(observationList);
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

	public void updateGeoPrivacy(List<Observation> observationList) {

		try {

			InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

			Properties properties = new Properties();
			try {
				properties.load(in);
			} catch (IOException e) {
				logger.error(e.getMessage());
			}
			String geoPrivacyTraitsValue = properties.getProperty("geoPrivacyValues");
			in.close();

			if (!geoPrivacyTraitsValue.equals("NA")) {
				List<Long> geoPrivateTaxonId = traitService.getTaxonListByValueId(geoPrivacyTraitsValue);

				for (Observation observation : observationList) {
					System.out.println("--------START---------");
					System.out.println("Observation Id : " + observation.getId());
					System.out.println("---------END----------");

					if (observation.getGeoPrivacy() == false && observation.getMaxVotedRecoId() != null) {
						Long taxonId = recoService.fetchTaxonId(observation.getMaxVotedRecoId());
						if (taxonId != null) {

							if (geoPrivateTaxonId.contains(taxonId)) {
								System.out.println("---------BEGIN----------");
								System.out.println("Observation Id : " + observation.getId());
								observation.setGeoPrivacy(true);
								observationDao.update(observation);
								produceToRabbitMQ(observation.getId().toString(), "Observation Core");
								System.out.println("----------END------------");
							}

						}
					}
				}

			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

	@Override
	public List<CustomFieldObservationData> addUpdateCustomFieldData(HttpServletRequest request,
			CustomFieldFactsInsert factsCreateData) {
		try {

			CustomFieldFactsInsertData factsInsertData = new CustomFieldFactsInsertData();
			factsInsertData.setFactsCreateData(factsCreateData);
			factsInsertData
					.setMailData(converter.userGroupMetadata(generateMailData(factsCreateData.getObservationId())));

			cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<CustomFieldObservationData> result = cfService.addUpdateCustomFieldData(factsInsertData);
			if (result != null && !result.isEmpty())
				produceToRabbitMQ(factsCreateData.getObservationId().toString(), "Custom Field");
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public List<CustomFieldValues> getCustomFieldOptions(HttpServletRequest request, String observationId,
			String userGroupId, String cfId) {
		try {
			cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<CustomFieldValues> result = cfService.getCustomFieldOptions(observationId, userGroupId, cfId);
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public void produceToRabbitMQ(String observationId, String updateType) {
		try {
			producer.setMessage("observation", observationId, updateType);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

	@Override
	public ObservationUGContextCreatePageData getUGContextObservationCreateDetails(HttpServletRequest request,
			Long userGroupId) {
		try {

			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			UserGroupPermissions userGroupPermission = userGroupService.getUserGroupObservationPermission();
			List<UserGroupMemberRole> memberRole = userGroupPermission.getUserMemberRole();
			int flag = 0;
			for (UserGroupMemberRole ugMemberRole : memberRole) {
				if (ugMemberRole.getUserGroupId().equals(userGroupId)) {
					flag = 1;
					break;
				}
			}

			if (flag == 1) {
				List<UserGroupSpeciesGroup> sGroup = userGroupService.getUserGroupSGroup(userGroupId.toString());

				cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
				List<CustomFieldDetails> customFields = cfService.getUserGroupCustomFields(userGroupId.toString());
				ObservationUGContextCreatePageData observationCreateData = new ObservationUGContextCreatePageData(
						sGroup, customFields);
				return observationCreateData;
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;

	}

	@Override
	public Long creteObservationUGContext(HttpServletRequest request, ObservationCreateUGContext observationUGContext) {
		try {
			Long observationId = observationCreateService.createObservation(request,
					observationUGContext.getObservationData(), true);
			for (CustomFieldFactsInsert cfInsert : observationUGContext.getCustomFieldData()) {
				cfInsert.setObservationId(observationId);
				CustomFieldFactsInsertData factsInsertData = new CustomFieldFactsInsertData();
				factsInsertData.setFactsCreateData(cfInsert);
				factsInsertData.setMailData(converter.userGroupMetadata(generateMailData(observationId)));
				cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
				cfService.addUpdateCustomFieldData(factsInsertData);
			}

			produceToRabbitMQ(observationId.toString(), "new Observation");

			return observationId;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public Boolean updateLastRevised(Long observationId) {
		try {
			Observation observation = observationDao.findById(observationId);
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(observationId.toString(), "Comment");
			return true;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return false;

	}

	@Override
	public MailData generateMailData(Long observationId) {
		MailData mailData = null;
		try {
			ObservationMailData observationData = getObservationMailData(observationId);
			List<UserGroupIbp> userGroupIbp = userGroupService.getObservationUserGroup(observationId.toString());
			List<UserGroupMailData> userGroupData = new ArrayList<UserGroupMailData>();
			if (userGroupIbp != null && !userGroupIbp.isEmpty()) {
				for (UserGroupIbp ugIbp : userGroupIbp) {
					UserGroupMailData ugMailData = new UserGroupMailData();
					ugMailData.setId(ugIbp.getId());
					ugMailData.setIcon(ugIbp.getIcon());
					ugMailData.setName(ugIbp.getName());
					ugMailData.setWebAddress(ugIbp.getWebAddress());
					userGroupData.add(ugMailData);
				}
			}
			mailData = new MailData();
			mailData.setObservationData(observationData);
			mailData.setUserGroupData(userGroupData);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return mailData;
	}

	private ObservationMailData getObservationMailData(Long obvId) {
		Observation observation = observationDao.findById(obvId);
		try {
			RecoIbp reco = new RecoIbp();
			String iconurl = null;
			if (observation.getMaxVotedRecoId() != null) {
				reco = recoService.fetchRecoName(obvId, observation.getMaxVotedRecoId());
			}
			List<ResourceData> resources = new ArrayList<ResourceData>();
			;
			if (observation.getReprImageId() != null)
				resources = resourceService.getImageResource("observation", observation.getId().toString());

			for (ResourceData resource : resources) {
				if (observation.getReprImageId().equals(resource.getResource().getId()))
					iconurl = resource.getResource().getFileName();
			}

			ObservationMailData observationData = new ObservationMailData();
			observationData.setAuthorId(observation.getAuthorId());
			observationData.setCommonName(reco.getCommonName());
			observationData.setIconURl(iconurl);
			observationData.setLocation(observation.getPlaceName() != null ? observation.getPlaceName()
					: observation.getReverseGeocodedName());
			observationData.setObservationId(observation.getId());
			observationData.setObservedOn(observation.getFromDate());
			observationData.setScientificName(reco.getScientificName());

			return observationData;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public Activity addObservationComment(HttpServletRequest request, CommentLoggingData comment) {
		try {
			comment.setMailData(generateMailData(comment.getRootHolderId()));
			activityService = headers.addActivityHeaders(activityService, request.getHeader(HttpHeaders.AUTHORIZATION));
			Activity result = activityService.addComment("observation", comment);
			updateLastRevised(comment.getRootHolderId());
			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public Activity removeObservationComment(HttpServletRequest request, CommentLoggingData comment, String commentId) {
		try {
			comment.setMailData(generateMailData(comment.getRootHolderId()));
			activityService = headers.addActivityHeaders(activityService, request.getHeader(HttpHeaders.AUTHORIZATION));

			return activityService.deleteComment("observation", commentId, comment);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public Boolean updateGalleryResourceRating(HttpServletRequest request, Long observationId,
			ResourceRating resourceRating) {
		try {
			resourceService = headers.addResourceHeaders(resourceService, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<Resource> resources = resourceService.updateRating("OBSERVATION", observationId.toString(),
					resourceRating);

			Long reprImage = null;
			int rating1 = 0;
			for (Resource res : resources) {
				if (res.getType().equals("IMAGE")) {
					if (reprImage == null)
						reprImage = res.getId();
					if (res.getRating() != null && res.getRating() > rating1) {
						reprImage = res.getId();
						rating1 = res.getRating();
					}
				}
			}
			Observation observation = observationDao.findById(observationId);
			observation.setLastRevised(new Date());
			observation.setReprImageId(reprImage);
			observation = observationDao.update(observation);
			produceToRabbitMQ(observationId.toString(), "Rating update");

			logActivity.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), null, observationId, observationId,
					"observation", observationId, "Rated media resource", generateMailData(observationId));
			return true;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public void bgfilterRule(HttpServletRequest request, Long observationId) {
		try {
			Observation observation = observationDao.findById(observationId);
			UserGroupObvRuleData ugObvFilterData = new UserGroupObvRuleData();
			ugObvFilterData = getUGObvRuleData(observation);
			List<FactValuePair> traits = traitService.getFacts("species.participation.Observation", observation.getId().toString());
			Map<String, List<Long>> facts = traits.stream()
		    .collect(Collectors.groupingBy(
		    		trait -> trait.getNameId().toString(), 
		            Collectors.mapping(FactValuePair::getValueId, Collectors.toList())
		        ));
			ugObvFilterData.setTraits(facts);
			intergratorService = headers.addIntergratorHeader(intergratorService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			intergratorService.getFilterRule(ugObvFilterData);
			produceToRabbitMQ(observationId.toString(), "Recommendation");

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

	@Override
	public ObservationListElasticMapping getObservationPublicationGrade(String index, String type,
			String observationId) {
		try {
			MapDocument document = esService.fetch(index, type, observationId);
			return objectMapper.readValue(String.valueOf(document.getDocument()), ObservationListElasticMapping.class);
		} catch (ApiException | IOException e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public List<DownloadLog> fetchDownloadLog(List<Long> authorIds, String fileType, Integer offSet, Integer limit) {
		String authorAttribute = "authorId";
		String filetypeAttribute = "type";
		String orderBy = "createdOn";
		if (authorIds == null || authorIds.isEmpty()) {
			authorAttribute = null;
		}
		if (fileType == null || fileType.isEmpty()) {
			filetypeAttribute = null;
		}
		List<DownloadLog> records = downloadLogDao.fetchFilteredRecordsWithCriteria(authorAttribute, filetypeAttribute,
				authorIds, fileType != null ? fileType.toUpperCase() : null, orderBy, offSet, limit);
		return records;
	}

	@Override
	public ObservationUserPageInfo observationUploadInfo(Long userId, Long sGroupId, Boolean hasMedia, Long offset) {

		try {
			Long size = offset + 10;
			AuthorUploadedObservationInfo authorUploadedObservationInfo = esService.getUploadUserInfo(
					ObservationIndex.INDEX.getValue(), ObservationIndex.TYPE.getValue(), userId.toString(),
					size.toString(), (sGroupId != null) ? sGroupId.toString() : null, hasMedia);

			List<UniqueSpeciesInfo> observationUploaded = new ArrayList<UniqueSpeciesInfo>();
			List<MaxVotedRecoFreq> maxVotedRecoFreqs = authorUploadedObservationInfo.getMaxVotedRecoFreqs();
			for (MaxVotedRecoFreq maxVotedRecoFreq : maxVotedRecoFreqs) {

				RecoIbp recoIbp = recoService.fetchByRecoId(maxVotedRecoFreq.getMaxVotedRecoId());
				if (recoIbp != null)
					observationUploaded.add(new UniqueSpeciesInfo(
							(recoIbp.getScientificName() != null) ? recoIbp.getScientificName()
									: recoIbp.getCommonName(),
							maxVotedRecoFreq.getMaxVotedRecoId(), recoIbp.getSpeciesId(), recoIbp.getTaxonId(),
							maxVotedRecoFreq.getFreq()));
			}
			ObservationUserPageInfo result = new ObservationUserPageInfo(observationUploaded,
					authorUploadedObservationInfo.getTotalCount());

			return result;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	@Override
	public ObservationUserPageInfo observationIdentifiedInfo(Long userId, Long sGroupId, Boolean hasMedia,
			Long offset) {

		Long identifiedSpeciesCount = null;
		Map<Long, List<UniqueSpeciesInfo>> identifiedFreq = recoService.getIdentifiedObservationInfo(userId, sGroupId,
				hasMedia, offset);
		if (identifiedFreq != null) {
			Set<Long> identifiedCount = identifiedFreq.keySet();
			identifiedSpeciesCount = identifiedCount.iterator().next();

			ObservationUserPageInfo result = new ObservationUserPageInfo(identifiedFreq.get(identifiedSpeciesCount),
					identifiedSpeciesCount);
			return result;
		}
		return null;

	}

	@Override
	public Boolean speciesObservationValidate(HttpServletRequest request, Long taxonId, List<Long> observationIdList) {

		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			for (Long observationId : observationIdList) {
				RecoSet recoSet = new RecoSet(taxonId, null, null);
				recoService.validateReco(request, profile, observationId, userId, recoSet);
			}
			return true;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return false;

	}

	@SuppressWarnings({ "null", "unused" })
	public Resources getObservationResources(Long observationId) {
		try {
			List<ResourceData> observationImageResources = resourceService.getImageResource("observation",
					observationId.toString());
			List<Long> resourceIds = new ArrayList<Long>();
			Map<Long, ResourceData> findResourceDataById = new HashMap<>();

			Resources result = new Resources();
			Integer countOfSelectedCropStatus = 0;
			Integer countOfRejected = 0;

			if (observationImageResources != null) {
				for (ResourceData resourceData : observationImageResources) {
					resourceIds.add(resourceData.getResource().getId());
					findResourceDataById.put(resourceData.getResource().getId(), resourceData);
				}

				String commaSeparatedStringOfResourceIds = resourceIds.stream().map(i -> i.toString())
						.collect(Collectors.joining(","));

				List<ResourceCropInfo> resourcesCropInfo = resourceService
						.getResourcesCropInfo(commaSeparatedStringOfResourceIds);

				Map<Long, ResourceCropInfo> cropInfo = new HashMap<Long, ResourceCropInfo>();

				if (resourcesCropInfo.size() > 0) {
					for (ResourceCropInfo info : resourcesCropInfo) {
						cropInfo.put(info.getId(), info);
					}

				}

				result.setId(observationId);
				List<ObservatioImageResourceCropinfo> observationResources = new ArrayList<>();
				for (Long id : resourceIds) {
					ObservatioImageResourceCropinfo observationImageCropInfo = new ObservatioImageResourceCropinfo();

					observationImageCropInfo.setResource(findResourceDataById.get(id).getResource());
					observationImageCropInfo.setUserIbp(findResourceDataById.get(id).getUserIbp());
					observationImageCropInfo.setLicense(findResourceDataById.get(id).getLicense());

					if (resourcesCropInfo.size() > 0) {

						if (cropInfo.get(id).getSelectionStatus() != null) {
							observationImageCropInfo.setSelectionStatus(cropInfo.get(id).getSelectionStatus());

						}

						if (cropInfo.get(id).getSelectionStatus() != null
								&& cropInfo.get(id).getSelectionStatus().equals("SELECTED")) {
							countOfSelectedCropStatus++;
						}

						if (cropInfo.get(id).getSelectionStatus() != null
								&& cropInfo.get(id).getSelectionStatus().equals("REJECTED")) {
							countOfRejected++;
						}

						Long[] box = new Long[4];
						box[0] = cropInfo.get(id).getX();
						box[1] = cropInfo.get(id).getY();
						box[2] = cropInfo.get(id).getWidth();
						box[3] = cropInfo.get(id).getHeight();

						observationImageCropInfo.setBbox(box);

					}

					observationResources.add(observationImageCropInfo);

				}

				if (countOfSelectedCropStatus == observationImageResources.size()) {
					result.setCurationStatus("CURATED");
				} else if (countOfSelectedCropStatus > 0
						&& countOfSelectedCropStatus < observationImageResources.size()) {
					result.setCurationStatus("PARTIALLY_CURATED");
				} else if (countOfRejected == observationImageResources.size()) {
					result.setCurationStatus("REJECTED");

				} else {
					result.setCurationStatus("NOT_CURATED");
				}

				result.setObservationResource(observationResources);

			}

			return result;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	@Override
	public Resources updateObservationImageResources(HttpServletRequest request, Long observationId,
			Resources resourcesUpdatedInfo) {

		resourceService = headers.addResourceHeaders(resourceService, request.getHeader(HttpHeaders.AUTHORIZATION));

		try {

			for (ObservatioImageResourceCropinfo cropInfo : resourcesUpdatedInfo.getObservationResource()) {
				ResourceCropInfo imageCropInfo = new ResourceCropInfo();

				imageCropInfo.setSelectionStatus(cropInfo.getSelectionStatus());
				imageCropInfo.setId(cropInfo.getResource().getId());

				if (cropInfo.getSelectionStatus() != null && cropInfo.getSelectionStatus().equals("SELECTED")) {
					imageCropInfo.setX(cropInfo.getBbox()[0]);
					imageCropInfo.setY(cropInfo.getBbox()[1]);
					imageCropInfo.setWidth(cropInfo.getBbox()[2]);
					imageCropInfo.setHeight(cropInfo.getBbox()[3]);

				}

				resourceService.updateResourcesCropInfo(imageCropInfo);

			}
			produceToRabbitMQ(observationId.toString(), "Observation-image-resource-update");
			return resourcesUpdatedInfo;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

}
