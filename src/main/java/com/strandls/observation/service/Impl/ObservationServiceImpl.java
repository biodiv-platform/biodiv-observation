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

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.activity.pojo.Activity;
import com.strandls.activity.pojo.ActivityLoggingData;
import com.strandls.activity.pojo.CommentLoggingData;
import com.strandls.activity.pojo.MailData;
import com.strandls.activity.pojo.ObservationMailData;
import com.strandls.activity.pojo.UserGroupMailData;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.esmodule.ApiException;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.MapDocument;
import com.strandls.esmodule.pojo.MapQueryResponse;
import com.strandls.esmodule.pojo.MapQueryResponse.ResultEnum;
import com.strandls.esmodule.pojo.ObservationInfo;
import com.strandls.esmodule.pojo.ObservationNearBy;
import com.strandls.esmodule.pojo.UserScore;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.observation.Headers;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dao.ObservationDownloadLogDAO;
import com.strandls.observation.dao.RecommendationVoteDao;
import com.strandls.observation.es.util.ESCreateThread;
import com.strandls.observation.es.util.ESUpdate;
import com.strandls.observation.es.util.ObservationIndex;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.es.util.RabbitMQProducer;
import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.pojo.ListPagePermissions;
import com.strandls.observation.pojo.MaxVotedRecoPermission;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ObservationCreateUGContext;
import com.strandls.observation.pojo.ObservationUGContextCreatePageData;
import com.strandls.observation.pojo.ObservationUpdateData;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.resource.pojo.Resource;
import com.strandls.resource.pojo.ResourceRating;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.taxonomy.pojo.TaxonTree;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.FactsCreateData;
import com.strandls.traits.pojo.FactsUpdateData;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.Follow;
import com.strandls.user.pojo.SpeciesPermission;
import com.strandls.user.pojo.UserGroupMemberRole;
import com.strandls.user.pojo.UserIbp;
import com.strandls.user.pojo.UserPermissions;
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
import com.strandls.userGroup.pojo.UserGroupObvFilterData;
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

	private static final Logger logger = LoggerFactory.getLogger(ObservationServiceImpl.class);

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
	private ESUpdate esUpdate;

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
		List<ObservationResourceUser> observationResource;
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
		if (observation != null && observation.getIsDeleted() != true) {
			try {
				in.close();
				UserScore score = esService.getUserScore("eaf", "er", observation.getAuthorId().toString());
				if (!score.getRecord().isEmpty()) {
					authorScore = score.getRecord().get(0).get("details");
				}
				facts = traitService.getFacts("species.participation.Observation", id.toString());
				observationResource = resourceService.getImageResource(id.toString());
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
					esLayerInfo = esService.getObservationInfo(ObservationIndex.index.getValue(),
							ObservationIndex.type.getValue(), observation.getMaxVotedRecoId().toString());
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

				List<ObservationNearBy> observationNearBy = esService.getNearByObservation(
						ObservationIndex.index.getValue(), ObservationIndex.type.getValue(),
						observation.getLatitude().toString(), observation.getLongitude().toString());

				Integer activityCount = activityService.getActivityCount("observation", observation.getId().toString());
				ShowData data = new ShowData(observation, facts, observationResource, userGroups, customField,
						layerInfo, esLayerInfo, reco, flag, tags, fetaured, userInfo, authorScore, recoaggregated,
						observationNearBy, activityCount);				
				return data;
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
	public ShowData createObservation(HttpServletRequest request, ObservationCreate observationData) {

		try {

			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long maxVotedReco = null;
			Observation observation = observationHelper.createObservationMapping(userId, observationData);
			observation = observationDao.save(observation);

			if (observationData.getResources() != null && !observationData.getResources().isEmpty()) {
				List<Resource> resources = observationHelper.createResourceMapping(request, userId,
						observationData.getResources());
				if (resources == null) {
					observationDao.delete(observation);
					return null;
				}
				resourceService = headers.addResourceHeaders(resourceService,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				resources = resourceService.createResource("OBSERVATION", String.valueOf(observation.getId()),
						resources);

				Integer noOfImages = 0;
				Integer noOfAudio = 0;
				Integer noOfVideo = 0;

				Long reprImage = null;
				int rating = 0;
				for (Resource res : resources) {
					if (res.getType().equals("AUDIO"))
						noOfAudio++;
					else if (res.getType().equals("IMAGE")) {
						noOfImages++;
						if (reprImage == null)
							reprImage = res.getId();
						if (res.getRating() != null && res.getRating() > rating) {
							reprImage = res.getId();
							rating = res.getRating();
						}
					} else if (res.getType().equals("VIDEO"))
						noOfVideo++;
				}
				observation.setNoOfAudio(noOfAudio);
				observation.setNoOfImages(noOfImages);
				observation.setNoOfVideos(noOfVideo);
				observation.setReprImageId(reprImage);
				observation = observationDao.update(observation);
			}
			logActivity.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), null, observation.getId(),
					observation.getId(), "observation", null, "Observation created", null);

			if (!(observationData.getHelpIdentify())) {
				RecoCreate recoCreate = observationHelper.createRecoMapping(observationData.getRecoData());
				maxVotedReco = recoService.createRecoVote(request, userId, observation.getId(),
						observationData.getRecoData().getScientificNameTaxonId(), recoCreate, true);

				observation.setMaxVotedRecoId(maxVotedReco);
				observationDao.update(observation);
			}

			if (observationData.getFacts() != null && !observationData.getFacts().isEmpty()) {
				FactsCreateData factsCreateData = new FactsCreateData();
				factsCreateData.setFactValuePairs(observationData.getFacts());
				factsCreateData.setMailData(null);
				traitService = headers.addTraitsHeaders(traitService, request.getHeader(HttpHeaders.AUTHORIZATION));
				traitService.createFacts("species.participation.Observation", String.valueOf(observation.getId()),
						factsCreateData);
			}

			if (observationData.getUserGroupId() != null && !observationData.getUserGroupId().isEmpty()) {
				UserGroupMappingCreateData userGroupData = new UserGroupMappingCreateData();

				userGroupData.setUserGroups(observationData.getUserGroupId());
				userGroupData.setMailData(null);
				userGroupData.setUgFilterData(getUGFilterObvData(observation));
				userGroupService = headers.addUserGroupHeader(userGroupService,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				userGroupService.createObservationUserGroupMapping(String.valueOf(observation.getId()), userGroupData);
			}
			if (!(observationData.getTags().isEmpty())) {
				TagsMapping tagsMapping = new TagsMapping();
				tagsMapping.setObjectId(observation.getId());
				tagsMapping.setTags(observationData.getTags());
				TagsMappingData tagMappingData = new TagsMappingData();
				tagMappingData.setTagsMapping(tagsMapping);
				tagMappingData.setMailData(null);
				utilityServices = headers.addUtilityHeaders(utilityServices,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				utilityServices.createTags("observation", tagMappingData);

			}

//			send observation create mail
			ActivityLoggingData activityLogging = new ActivityLoggingData();
			activityLogging.setRootObjectId(observation.getId());
			activityLogging.setSubRootObjectId(observation.getId());
			activityLogging.setRootObjectType("observation");
			activityLogging.setActivityType("Observation created");
			activityLogging.setMailData(generateMailData(observation.getId()));

			activityService = headers.addActivityHeaders(activityService, request.getHeader(HttpHeaders.AUTHORIZATION));
			activityService.sendMailCreateObservation(activityLogging);

//			----------------POST CREATE ACTIONS------------

//			----------------GEO PRIVACY CHECK-------------
			List<Observation> observationList = new ArrayList<Observation>();
			observationList.add(observation);
			updateGeoPrivacy(observationList);

//			---------------USER GROUP FILTER RULE----------
			UserGroupObvFilterData ugObvFilterData = new UserGroupObvFilterData();
			ugObvFilterData = getUGFilterObvData(observation);
			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			userGroupService.getFilterRule(ugObvFilterData);

			ESCreateThread esCreateThread = new ESCreateThread(esUpdate, observation.getId().toString());
			Thread thread = new Thread(esCreateThread);
			thread.start();

			return findById(observation.getId());

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;

	}

	@Override
	public Long updateSGroup(HttpServletRequest request, Long observationId, Long sGroupId) {
		Observation observation = observationDao.findById(observationId);
		Long previousGroupId = observation.getGroupId();
		observation.setGroupId(sGroupId);
		observation.setLastRevised(new Date());
		observation = observationDao.update(observation);
		List<SpeciesGroup> SpeciesGroupList = getAllSpeciesGroup();
		String previousGroupName = "";
		String newGroupName = "";
		for (SpeciesGroup speciesGroup : SpeciesGroupList) {
			if (speciesGroup.getId().equals(previousGroupId))
				previousGroupName = speciesGroup.getName();
			if (speciesGroup.getId().equals(sGroupId))
				newGroupName = speciesGroup.getName();
		}
		String description = previousGroupName + " to " + newGroupName;

		produceToRabbitMQ(observationId.toString(), "Species Group");
		logActivity.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), description, observationId, observationId,
				"observation", observationId, "Observation species group updated", generateMailData(observationId));
		return observation.getGroupId();
	}

	@Override
	public Long updateMaxVotedReco(Long observationId, Long maxVotedReco) {
		Observation observation = observationDao.findById(observationId);
		if (observation.getMaxVotedRecoId() == null || observation.getMaxVotedRecoId() != maxVotedReco) {
			observation.setMaxVotedRecoId(maxVotedReco);
			observation.setLastRevised(new Date());
			observation.setNoOfIdentifications(recoVoteDao.findRecoVoteCount(observationId));
			observationDao.update(observation);
			List<Observation> observationList = new ArrayList<Observation>();
			observationList.add(observation);
			updateGeoPrivacy(observationList);
			produceToRabbitMQ(observationId.toString(), "Recommendation");
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
			List<Long> valueList) {

		List<FactValuePair> facts = null;
		try {

			FactsUpdateData factsUpdatedata = new FactsUpdateData();
			factsUpdatedata.setTraitValueList(valueList);
			factsUpdatedata.setMailData(converter.traitMetaData(generateMailData(Long.parseLong(observationId))));
			traitService = headers.addTraitsHeaders(traitService, request.getHeader(HttpHeaders.AUTHORIZATION));
			facts = traitService.updateTraits("species.participation.Observation", observationId, traitId,
					factsUpdatedata);
			Observation observation = observationDao.findById(Long.parseLong(observationId));
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(observationId, "Traits");
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return facts;
	}

	private UserGroupObvFilterData getUGFilterObvData(Observation observation) {
		UserGroupObvFilterData ugFilterData = new UserGroupObvFilterData();
		Long taxonomyId = null;
		if (observation.getMaxVotedRecoId() != null)
			taxonomyId = recoService.fetchTaxonId(observation.getMaxVotedRecoId());
		ugFilterData.setObservationId(observation.getId());
		ugFilterData.setCreatedOnDate(observation.getCreatedOn());
		ugFilterData.setLatitude(observation.getLatitude());
		ugFilterData.setLongitude(observation.getLongitude());
		ugFilterData.setObservedOnDate(observation.getFromDate());
		ugFilterData.setTaxonomyId(taxonomyId);

		return ugFilterData;
	}

	@Override
	public List<UserGroupIbp> updateUserGroup(HttpServletRequest request, String observationId,
			List<Long> userGroupList) {

		List<UserGroupIbp> result = null;
		try {
			UserGroupMappingCreateData userGroupData = new UserGroupMappingCreateData();
			userGroupData.setUserGroups(userGroupList);
			userGroupData.setUgFilterData(getUGFilterObvData(observationDao.findById(Long.parseLong(observationId))));
			userGroupData.setMailData(converter.userGroupMetadata(generateMailData(Long.parseLong(observationId))));
			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			result = userGroupService.updateUserGroupMapping(observationId, userGroupData);
			Observation observation = observationDao.findById(Long.parseLong(observationId));
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			produceToRabbitMQ(observationId, "UserGroups");
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return result;
	}

	@Override
	public List<SpeciesGroup> getAllSpeciesGroup() {

		List<SpeciesGroup> result = null;
		try {
			result = taxonomyService.getAllSpeciesGroup();
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
	public List<TraitsValuePair> getTraitList(String speciesId) {

		List<TraitsValuePair> result = null;
		try {
			result = traitService.getTraitList(speciesId);
		} catch (Exception e) {
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
					userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));
					UserPermissions userPermission = userService.getAllUserPermission("observation",
							entry.getKey().toString());
					taxonomyService = headers.addTaxonomyHeader(taxonomyService,
							request.getHeader(HttpHeaders.AUTHORIZATION));
					List<TaxonTree> taxonTree = taxonomyService.getTaxonTree(entry.getValue().toString());
					List<Long> validateAllowed = ValidatePermission(taxonTree, userPermission.getAllowedTaxonList());
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
				userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));
				UserPermissions userPermission = userService.getAllUserPermission("observation",
						observationId.toString());

				if (taxonList.trim().length() != 0) {
					taxonomyService = headers.addTaxonomyHeader(taxonomyService,
							request.getHeader(HttpHeaders.AUTHORIZATION));
					List<TaxonTree> taxonTree = taxonomyService.getTaxonTree(taxonList);
					validateAllowed = ValidatePermission(taxonTree, userPermission.getAllowedTaxonList());
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
			UserPermissions userPermission = userService.getAllUserPermission("observation", observationId);

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
					List<TaxonTree> taxonTree = taxonomyService.getTaxonTree(taxonList);
					validateAllowed = ValidatePermission(taxonTree, userPermission.getAllowedTaxonList());

				}

				List<Long> userGroupMember = new ArrayList<Long>();
				for (UserGroupMemberRole userMemberRole : userPermission.getUserMemberRole()) {
					userGroupMember.add(userMemberRole.getUserGroupId());
				}
				String s = userGroupMember.toString();
				if (s.substring(1, s.length() - 1).trim().length() != 0)
					allowedUserGroup = userGroupService.getUserGroupList(s.substring(1, s.length() - 1));

				for (UserGroupMemberRole userFeatureRole : userPermission.getUserFeatureRole()) {
					userGroupFeatureRole.add(userFeatureRole.getUserGroupId());
				}
			}

			List<Long> userGroupIdList = new ArrayList<Long>();
			List<UserGroupIbp> featureableGroup = new ArrayList<UserGroupIbp>();
			for (UserGroupIbp userGroup : associatedUserGroup) {
				userGroupIdList.add(userGroup.getId());
				if (userGroupFeatureRole.contains(userGroup.getId()))
					featureableGroup.add(userGroup);

			}

			cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
			List<CustomFieldPermission> cfPermission = cfService.getCustomFieldPermission(observationId);

			ObservationUserPermission permission = new ObservationUserPermission(validateAllowed, allowedUserGroup,
					featureableGroup, cfPermission, userPermission.getFollowing());

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
				userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));
				UserPermissions userPermission = userService.getUserGroupPermissions();
				List<Long> userGroupMember = new ArrayList<Long>();
				for (UserGroupMemberRole userMemberRole : userPermission.getUserMemberRole()) {
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
			if (observation.getAuthorId().equals(userId) || userRole.contains("ROLE_ADMIN")) {

				MailData mailData = generateMailData(observationId);

				observation.setIsDeleted(true);
				MapQueryResponse esResponse = esService.delete(ObservationIndex.index.getValue(),
						ObservationIndex.type.getValue(), observationId.toString());
				ResultEnum result = esResponse.getResult();
				if (result.getValue().equals("DELETED")) {
					observationDao.update(observation);
					logActivity.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), null, observationId,
							observationId, "observation", observationId, "Observation Deleted", mailData);
					return "Observation Deleted Succesfully";
				}

			}
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
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
//				resource data

				List<Resource> resources = observationHelper.createResourceMapping(request, userId,
						observationUpdate.getResources());
				resourceService = headers.addResourceHeaders(resourceService,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				resources = resourceService.updateResources("OBSERVATION", String.valueOf(observation.getId()),
						resources);

//				calculate reprImageof observation

				Integer noOfImages = 0;
				Integer noOfAudio = 0;
				Integer noOfVideo = 0;

				Long reprImage = null;
				int rating = 0;
				for (Resource res : resources) {
					if (res.getType().equals("AUDIO"))
						noOfAudio++;
					else if (res.getType().equals("IMAGE")) {
						noOfImages++;
						if (reprImage == null)
							reprImage = res.getId();
						if (res.getRating() != null && res.getRating() > rating) {
							reprImage = res.getId();
							rating = res.getRating();
						}
					} else if (res.getType().equals("VIDEO"))
						noOfVideo++;

				}
				observation.setNoOfAudio(noOfAudio);
				observation.setNoOfImages(noOfImages);
				observation.setNoOfVideos(noOfVideo);
				observation.setReprImageId(reprImage);
				observationDao.update(observation);

//				---------GEO PRIVACY CHECK------------
				List<Observation> observationList = new ArrayList<Observation>();
				observationList.add(observation);
				updateGeoPrivacy(observationList);
//				------------BG rules-----------------
				UserGroupObvFilterData ugObvFilterData = new UserGroupObvFilterData();
				ugObvFilterData = getUGFilterObvData(observation);
				userGroupService = headers.addUserGroupHeader(userGroupService,
						request.getHeader(HttpHeaders.AUTHORIZATION));
				userGroupService.getFilterRule(ugObvFilterData);

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

//				resources Data
				List<ObservationResourceUser> resourceData = resourceService.getImageResource(observationId.toString());
				editData.setResources(observationHelper.createEditResourceMapping(resourceData));
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
				List<UserGroupObvFilterData> ugObvFilterDataList = new ArrayList<UserGroupObvFilterData>();
				for (Observation observation : observationList) {
					ugObvFilterDataList.add(getUGFilterObvData(observation));
				}
				userGroupService.bulkFilterRulePosting(userGroupIds, ugObvFilterDataList);
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
				List<UserGroupObvFilterData> ugObvFilterDataList = new ArrayList<UserGroupObvFilterData>();
				for (Observation observation : observationList) {
					ugObvFilterDataList.add(getUGFilterObvData(observation));
				}
				userGroupService.bulkFilterRuleRemoving(userGroupId, ugObvFilterDataList);
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

	private void updateGeoPrivacy(List<Observation> observationList) {

		try {

			InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

			Properties properties = new Properties();
			try {
				properties.load(in);
			} catch (IOException e) {
				e.printStackTrace();
			}
			String geoPrivacyTraitsValue = properties.getProperty("geoPrivacyValues");
			in.close();

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
			producer.setMessage("esmodule", observationId, updateType);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

	}

	@Override
	public ObservationUGContextCreatePageData getUGContextObservationCreateDetails(HttpServletRequest request,
			Long userGroupId) {
		try {
			userService = headers.addUserHeaders(userService, request.getHeader(HttpHeaders.AUTHORIZATION));
			UserPermissions userGroupPermission = userService.getUserGroupPermissions();
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
	public ShowData creteObservationUGContext(HttpServletRequest request,
			ObservationCreateUGContext observationUGContext) {
		try {
			ShowData observationData = createObservation(request, observationUGContext.getObservationData());
			for (CustomFieldFactsInsert cfInsert : observationUGContext.getCustomFieldData()) {
				cfInsert.setObservationId(observationData.getObservation().getId());
				CustomFieldFactsInsertData factsInsertData = new CustomFieldFactsInsertData();
				factsInsertData.setFactsCreateData(cfInsert);
				factsInsertData.setMailData(
						converter.userGroupMetadata(generateMailData(observationData.getObservation().getId())));
				cfService = headers.addCFHeaders(cfService, request.getHeader(HttpHeaders.AUTHORIZATION));
				cfService.addUpdateCustomFieldData(factsInsertData);
			}

			return findById(observationData.getObservation().getId());

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
			for (UserGroupIbp ugIbp : userGroupIbp) {
				UserGroupMailData ugMailData = new UserGroupMailData();
				ugMailData.setId(ugIbp.getId());
				ugMailData.setIcon(ugIbp.getIcon());
				ugMailData.setName(ugIbp.getName());
				ugMailData.setWebAddress(ugIbp.getWebAddress());
				userGroupData.add(ugMailData);
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
			List<ObservationResourceUser> resources = new ArrayList<ObservationResourceUser>();
			;
			if (observation.getReprImageId() != null)
				resources = resourceService.getImageResource(observation.getId().toString());

			for (ObservationResourceUser resource : resources) {
				if (observation.getReprImageId().equals(resource.getResource().getId()))
					iconurl = resource.getResource().getFileName();
			}

			ObservationMailData observationData = new ObservationMailData();
			observationData.setAuthorId(observation.getAuthorId());
			observationData.setCommonName(reco.getCommonName());
			observationData.setIconURl(iconurl);
			observationData.setLocation(observation.getReverseGeocodedName());
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
			Activity result = activityService.addComment(comment);
			updateLastRevised(comment.getRootHolderId());
			return result;
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
			UserGroupObvFilterData ugObvFilterData = new UserGroupObvFilterData();
			ugObvFilterData = getUGFilterObvData(observation);
			userGroupService = headers.addUserGroupHeader(userGroupService,
					request.getHeader(HttpHeaders.AUTHORIZATION));
			userGroupService.getFilterRule(ugObvFilterData);
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
				authorIds, fileType.toUpperCase(), orderBy, offSet, limit);
		return records;
	}

	@Override
	public String forceUpdateIndexField(String index, String type,String field, String value, Long dataTableId){
	List<String>columnNames = new ArrayList<>();
	Map<String, Object> filterOn = new HashMap<String, Object>();
	columnNames.add("id");
	filterOn.put("dataTableId", dataTableId);
	List<Object[]>keys = observationDao.getValuesOfColumnsBasedOnFilter(columnNames, filterOn);
	String ids =  keys.toString();
	try {
		return esService.forceUpdateIndexField(index, type, field, value, ids.toString().substring(1, ids.length()));
	} catch (ApiException e) {
		logger.error(e.getMessage());
	}
	return null;
	}
}
