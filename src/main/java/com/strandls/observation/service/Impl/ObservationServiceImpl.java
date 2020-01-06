/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.servlet.http.HttpServletRequest;

import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.authentication_utility.util.AuthUtil;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.ObservationInfo;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.naksha.pojo.ObservationLocationInfo;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.resource.pojo.Resource;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.taxonomy.pojo.TaxonTree;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.Facts;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.SpeciesPermission;
import com.strandls.user.pojo.UserGroupMemberRole;
import com.strandls.user.pojo.UserIbp;
import com.strandls.user.pojo.UserPermissions;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.FeaturedCreate;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.Flag;
import com.strandls.utility.pojo.Language;
import com.strandls.utility.pojo.Tags;
import com.strandls.utility.pojo.TagsMapping;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServiceImpl implements ObservationService {

	private static final Logger logger = LoggerFactory.getLogger(ObservationServiceImpl.class);

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
		ObservationLocationInfo layerInfo;
		ObservationInfo esLayerInfo = null;
		RecoIbp reco = null;
		Flag flag;
		List<Tags> tags;
		List<Featured> fetaured;
		UserIbp userInfo;
		List<RecoIbp> allRecoVotes = null;
		Observation observation = observationDao.findById(id);
		if (observation != null && observation.getIsDeleted() != true) {
			try {
				facts = traitService.getFacts("species.participation.Observation", id.toString());
				observationResource = resourceService.getImageResource(id.toString());
				userGroups = userGroupService.getObservationUserGroup(id.toString());
				layerInfo = layerService.getLayerInfo(String.valueOf(observation.getLatitude()),
						String.valueOf(observation.getLongitude()));
				flag = utilityServices.getFlagByObservation("species.participation.Observation", id.toString());
				tags = utilityServices.getTags("observation", id.toString());
				userInfo = userService.getUserIbp(observation.getAuthorId().toString());
				fetaured = userGroupService.getAllFeatured("species.participation.Observation", id.toString());
				if (observation.getMaxVotedRecoId() != null) {
					reco = recoService.fetchRecoName(id, observation.getMaxVotedRecoId());
					esLayerInfo = esService.getObservationInfo("observation", "observation",
							observation.getMaxVotedRecoId().toString());
				}

				if (observation.getMaxVotedRecoId() != null) {
					allRecoVotes = recoService.allRecoVote(id);
					Long taxonId = recoService.fetchTaxonId(observation.getMaxVotedRecoId());
					if (taxonId != null) {

						List<Facts> resultList = traitService.getFactsBytaxonId(taxonId.toString());
						if (resultList != null) {
							for (Facts fact : resultList) {

								if (fact.getTraitInstanceId() == 14 && fact.getTraitValueId() == 53) {
									observation.setGeoPrivacy(true);
									break;
								}
							}
						}

					}
				}

				if (observation.getGeoPrivacy()) {
					Map<String, Double> latlon = observationHelper.getRandomLatLong(observation.getLatitude(),
							observation.getLongitude());
					observation.setLatitude(latlon.get("lat"));
					observation.setLongitude(latlon.get("lon"));
				}

				ShowData data = new ShowData(observation, facts, observationResource, userGroups, layerInfo,
						esLayerInfo, reco, flag, tags, fetaured, userInfo, allRecoVotes);

				observation.setVisitCount(observation.getVisitCount() + 1);
				observationDao.update(observation);
				return data;
			} catch (Exception e) {
				logger.error(e.getMessage());
			}
		}
		return null;
	}

	@Override
	public ShowData createObservation(HttpServletRequest request, ObservationCreate observationData) {

		try {
			CommonProfile profile = AuthUtil.getProfileFromRequest(request);
			Long userId = Long.parseLong(profile.getId());
			Long maxVotedReco = null;
			Observation observation = observationHelper.createObservationMapping(userId, observationData);
			observation = observationDao.save(observation);
			if (!(observationData.getHelpIdentify())) {
				RecoCreate recoCreate = observationHelper.createRecoMapping(observationData.getRecoData());
				maxVotedReco = recoService.createRecoVote(userId, observation.getId(), recoCreate);
			}

			List<Resource> resources = observationHelper.createResourceMapping(userId, observationData);
			resourceService.createResource("OBSERVATION", String.valueOf(observation.getId()), resources);

			traitService.createFacts("species.participation.Observation", String.valueOf(observation.getId()),
					observationData.getFacts());

			userGroupService.createObservationUserGroupMapping(String.valueOf(observation.getId()),
					observationData.getUserGroupId());
			TagsMapping tagsMapping = new TagsMapping();
			tagsMapping.setObjectId(observation.getId());
			tagsMapping.setTags(observationData.getTags());
			utilityServices.createTags("observation", tagsMapping);

//			update observaiton object

			observation.setMaxVotedRecoId(maxVotedReco);
			Integer noOfImages = 0;
			Integer noOfAudio = 0;
			Integer noOfVideo = 0;

			Long reprImage = resources.get(0).getId();
			int rating = 0;
			for (Resource res : resources) {
				if (res.getType().equals("AUDIO"))
					noOfAudio++;
				else if (res.getType().equals("IMAGE")) {
					noOfImages++;
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

			return findById(observation.getId());

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;

	}

	@Override
	public Long updateSGroup(Long observationId, Long sGroupId) {
		Observation observation = observationDao.findById(observationId);
		observation.setGroupId(sGroupId);
		observation.setLastRevised(new Date());
		observation = observationDao.update(observation);
		return observation.getGroupId();
	}

	@Override
	public Long updateMaxVotedReco(Long observationId, Long maxVotedReco) {
		Observation observation = observationDao.findById(observationId);
		if (observation.getMaxVotedRecoId() != maxVotedReco) {
			observation.setMaxVotedRecoId(maxVotedReco);
			observation.setLastRevised(new Date());
			observationDao.update(observation);
			return maxVotedReco;
		}
		return observation.getMaxVotedRecoId();
	}

	@Override
	public List<Tags> updateTags(TagsMapping tagsMapping) {
		List<Tags> result = null;
		try {
			result = utilityServices.updateTags("observation", tagsMapping);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<FactValuePair> updateTraits(String observationId, String traitId, List<Long> valueList) {

		List<FactValuePair> facts = null;
		try {
			facts = traitService.updateTraits("species.participation.Observation", observationId, traitId, valueList);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return facts;
	}

	@Override
	public List<UserGroupIbp> updateUserGroup(String observationId, List<Long> userGroupList) {

		List<UserGroupIbp> result = null;
		try {
			result = userGroupService.updateUserGroupMapping(observationId, userGroupList);
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
	public List<Featured> createFeatured(FeaturedCreate featuredCreate) {
		List<Featured> result = null;

		try {
			result = userGroupService.createFeatured(featuredCreate);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<Featured> unFeatured(String observaitonId, String userGroupList) {
		List<Featured> result = null;
		try {
			result = userGroupService.unFeatured("observation", observaitonId, userGroupList);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

	@Override
	public List<TraitsValue> getTraitsValue(String traitId) {
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
	public ObservationUserPermission getUserPermissions(String observationId, Long userId, String taxonList) {
		try {
			List<UserGroupIbp> associatedUserGroup = userGroupService.getObservationUserGroup(observationId);
			List<TaxonTree> taxonTree = taxonomyService.getTaxonTree(taxonList);
			UserPermissions userPermission = userService.getAllUserPermission();
			List<Long> validateAllowed = ValidatePermission(taxonTree, userPermission.getAllowedTaxonList());

			List<Long> userGroupMember = new ArrayList<Long>();
			for (UserGroupMemberRole userMemberRole : userPermission.getUserMemberRole()) {
				userGroupMember.add(userMemberRole.getUserGroupId());
			}
			String s = userGroupMember.toString();
			List<UserGroupIbp> allowedUserGroup = userGroupService.getUserGroupList(s.substring(1, s.length() - 1));

			List<Long> userGroupFeatureRole = new ArrayList<Long>();
			for (UserGroupMemberRole userFeatureRole : userPermission.getUserFeatureRole()) {
				userGroupFeatureRole.add(userFeatureRole.getUserGroupId());
			}
			List<UserGroupIbp> featureableGroup = new ArrayList<UserGroupIbp>();
			for (UserGroupIbp userGroup : associatedUserGroup) {
				if (userGroupFeatureRole.contains(userGroup.getId()))
					featureableGroup.add(userGroup);

			}

			ObservationUserPermission permission = new ObservationUserPermission(validateAllowed, allowedUserGroup,
					featureableGroup);
			return permission;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
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

}
