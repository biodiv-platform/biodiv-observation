/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.io.InputStream;
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
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.service.ObservationService;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.resource.pojo.ObservationResourceUser;
import com.strandls.resource.pojo.Resource;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.Facts;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.UserIbp;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.Featured;
import com.strandls.utility.pojo.Flag;
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
		List<String> tags;
		List<Featured> fetaured;
		UserIbp userInfo;
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
				fetaured = utilityServices.getAllFeatured("species.participation.Observation", id.toString());
				if (observation.getMaxVotedRecoId() != null) {
					reco = recoService.fetchRecoName(id, observation.getMaxVotedRecoId());
					esLayerInfo = esService.getObservationInfo("observation", "observation",
							observation.getMaxVotedRecoId().toString());
				}

				if (observation.getMaxVotedRecoId() != null) {
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
						esLayerInfo, reco, flag, tags, fetaured, userInfo);

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
			if (!(observationData.getHelpIdentified())) {
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
			observationDao.update(observation);
			return maxVotedReco;
		}
		return observation.getMaxVotedRecoId();
	}

}
