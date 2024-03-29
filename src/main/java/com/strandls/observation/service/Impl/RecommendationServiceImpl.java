/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.activity.pojo.RecoVoteActivity;
import com.strandls.observation.dao.ObservationDAO;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.dao.RecommendationVoteDao;
import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.RecoShow;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.pojo.RecommendationVote;
import com.strandls.observation.pojo.UniqueRecoVote;
import com.strandls.observation.pojo.UniqueSpeciesInfo;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.service.RecommendationService;
import com.strandls.observation.util.ObservationInputException;
import com.strandls.observation.util.PropertyFileUtil;
import com.strandls.observation.util.TokenGenerator;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyTreeServicesApi;
import com.strandls.taxonomy.pojo.BreadCrumb;
import com.strandls.taxonomy.pojo.TaxonomyDefinition;
import com.strandls.user.ApiException;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.UserIbp;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.ParsedName;

/**
 * @author Abhishek Rudra
 *
 */
public class RecommendationServiceImpl implements RecommendationService {

	private final Logger logger = LoggerFactory.getLogger(RecommendationServiceImpl.class);

	@Inject
	private ObjectMapper objectMapper;

	@Inject
	private ObservationService observaitonService;

	@Inject
	private LogActivities logActivities;

	@Inject
	private ObservationDAO observationDao;

	@Inject
	private RecommendationVoteDao recoVoteDao;

	@Inject
	private RecommendationDao recoDao;

	@Inject
	private TaxonomyServicesApi taxonomyService;

	@Inject
	private TaxonomyTreeServicesApi taxonomyTreeService;

	@Inject
	private UtilityServiceApi utilityService;

	@Inject
	private UserServiceApi userService;

	private Long defaultLanguageId = Long
			.parseLong(PropertyFileUtil.fetchProperty("config.properties", "defaultLanguageId"));

	@Override
	public RecoIbp fetchRecoVote(Long id) {

		String givenName = "";
		String scientificName = "";
		Long speciesId = null;
		RecoIbp ibpData = null;
		RecommendationVote recoVote = recoVoteDao.findById(id);

		if (recoVote == null)
			return null;
		if (recoVote.getGivenSciName() != null)
			givenName = givenName + " " + recoVote.getGivenSciName();
		if (recoVote.getGivenCommonName() != null)
			givenName = givenName + " " + recoVote.getGivenCommonName();

		Recommendation reco = recoDao.findById(recoVote.getRecommendationId());

		try {
			scientificName = scientificName + reco.getName();
			if (recoVote.getCommonNameRecoId() != null && recoVote.getGivenCommonName() != null) {
				Recommendation recoCommon = recoDao.findById(recoVote.getCommonNameRecoId());
				scientificName = scientificName + " " + recoCommon.getName();
			}

			UserIbp user = userService.getUserIbp(recoVote.getAuthorId().toString());

			ibpData = new RecoIbp(givenName, scientificName, null, speciesId, null, null, null, user);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return ibpData;
	}

	@Override
	public RecoIbp fetchByRecoId(Long recoId) {

		try {
			Recommendation reco = recoDao.findById(recoId);
			Long speciesId = null;
			String scientificName = "";
			Long taxonId = null;
			if (reco.getTaxonConceptId() != null) {

				TaxonomyDefinition taxonomyDefinition = taxonomyService
						.getTaxonomyConceptName(reco.getTaxonConceptId().toString());
				taxonId = reco.getTaxonConceptId();
				scientificName = (taxonomyDefinition.getItalicisedForm() != null
						&& !taxonomyDefinition.getItalicisedForm().isEmpty()) ? taxonomyDefinition.getItalicisedForm()
								: taxonomyDefinition.getNormalizedForm();

			} else {
				scientificName = reco.getName();
			}

			RecoIbp recoIbp = new RecoIbp(null, scientificName, taxonId, speciesId, null, null, null, null);
			return recoIbp;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public RecoIbp fetchRecoName(Long obvId, Long recoId) {

		Long speciesId = null;
		String commonName = "";
		String scientificName = "";
		Long taxonId = null;
		List<BreadCrumb> breadCrumb = null;
		String status = null;

		try {
			List<RecommendationVote> recoVotes = recoVoteDao.findByRecommendationId(obvId, recoId);
			Integer recoVoteCount = recoVoteDao.findRecoVoteCount(obvId);
			Recommendation reco = recoDao.findById(recoId);
			if (reco.getTaxonConceptId() != null) {

				TaxonomyDefinition taxonomyDefinition = taxonomyService
						.getTaxonomyConceptName(reco.getTaxonConceptId().toString());
				taxonId = reco.getTaxonConceptId();
				breadCrumb = taxonomyTreeService.getTaxonomyBreadCrumb(reco.getTaxonConceptId().toString());
				scientificName = (taxonomyDefinition.getItalicisedForm() != null
						&& !taxonomyDefinition.getItalicisedForm().isEmpty()) ? taxonomyDefinition.getItalicisedForm()
								: taxonomyDefinition.getNormalizedForm();
				status = taxonomyDefinition.getStatus();

			} else {
				scientificName = reco.getName();
			}

			for (RecommendationVote recoVote : recoVotes) {
				if (recoVote.getCommonNameRecoId() != null) {
					String tempName = recoDao.findById(recoVote.getCommonNameRecoId()).getName();
					if (!commonName.contains(tempName))
						commonName = commonName + tempName + "||";
				}
			}
			if (!(commonName.isEmpty()))
				commonName = commonName.substring(0, commonName.length() - 2);

			return new RecoIbp(commonName, scientificName, taxonId, speciesId, breadCrumb, recoVoteCount, status, null);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public Long createRecoVote(HttpServletRequest request, Long userId, Long observationId, Long taxonid,
			RecoCreate recoCreate, Boolean createObservation) {
		RecommendationVote previousVote = recoVoteDao.findRecoVoteIdByRecoId(observationId, userId, null, null);
		if (previousVote != null) {
			recoVoteDao.delete(previousVote);
		}

		RecommendationVote recoVote = null;
		if (recoCreate.getScientificNameId() != null) {
			recoVote = new RecommendationVote(null, userId, observationId, recoCreate.getScientificNameId(), new Date(),
					recoCreate.getCommonNameId(), recoCreate.getCommonName(), recoCreate.getScientificName(),
					recoCreate.getFlag(), recoCreate.getSource());
		} else {
			recoVote = new RecommendationVote(null, userId, observationId, recoCreate.getCommonNameId(), new Date(),
					recoCreate.getCommonNameId(), recoCreate.getCommonName(), null, recoCreate.getFlag(),
					recoCreate.getSource());
		}
		recoVote = recoVoteDao.save(recoVote);
		Long maxRecoVote = maxRecoVote(observationId);

		String description = "";
		RecoVoteActivity rvActivity = new RecoVoteActivity();
		try {
			if (taxonid != null) {
				TaxonomyDefinition taxonomyDef = taxonomyService.getTaxonomyConceptName(taxonid.toString());
				rvActivity.setScientificName(
						(taxonomyDef.getItalicisedForm() != null && !taxonomyDef.getItalicisedForm().isEmpty())
								? taxonomyDef.getItalicisedForm()
								: taxonomyDef.getNormalizedForm());

			}
			if (recoCreate.getCommonName() != null && recoCreate.getCommonName().trim().length() > 0)
				rvActivity.setCommonName(recoCreate.getCommonName());
			if (recoCreate.getScientificName() != null && recoCreate.getScientificName().trim().length() > 0)
				rvActivity.setGivenName(recoCreate.getScientificName());

			rvActivity.setSource(recoCreate.getSource());

			description = objectMapper.writeValueAsString(rvActivity);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		Observation observation = observationDao.findById(observationId);
		observation.setLastRevised(new Date());
		observationDao.update(observation);
		TokenGenerator tokenGenerator = new TokenGenerator();
		try {
			String userAuthToken = tokenGenerator.generate(userService.getUser(userId.toString()));
			if (createObservation) {
				logActivities.LogActivity(userAuthToken, description, observationId, observationId, "observation",
						recoVote.getId(), "Suggested species name", null);
			} else {
				maxRecoVote = observaitonService.updateMaxVotedReco(observationId, maxRecoVote);
//				Bg process for userGroup filter rule
				logActivities.LogActivity(userAuthToken, description, observationId, observationId, "observation",
						recoVote.getId(), "Suggested species name", observaitonService.generateMailData(observationId));
				observaitonService.bgfilterRule(request, observationId);

			}
		} catch (ApiException e) {
			logger.error(e.getMessage());
		}
		return maxRecoVote;

	}

	private Long maxRecoVote(Long observationId) {
		List<RecommendationVote> recoVoteList = recoVoteDao.findRecoVoteOnObservation(observationId);
		if (!(recoVoteList.isEmpty())) {
			Map<Long, UniqueRecoVote> resultMap = prepareUniqueRecord(recoVoteList);
			UniqueRecoVote maxRecoVote = null;
			for (Entry<Long, UniqueRecoVote> entry : resultMap.entrySet()) {
				int value = entry.getValue().compareTo(maxRecoVote);
				if (value > 0) {
					maxRecoVote = entry.getValue();
				}
			}
			if (maxRecoVote != null)
				return maxRecoVote.getRecoId();
		}
		return null;

	}

	private Map<Long, UniqueRecoVote> prepareUniqueRecord(List<RecommendationVote> recoVotes) {
		Map<Long, UniqueRecoVote> uniqueRecoVotes = new HashMap<Long, UniqueRecoVote>();

		for (RecommendationVote recommendationVote : recoVotes) {
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
		return uniqueRecoVotes;
	}

	/**
	 * @param originalRecoVote
	 * @param uniqueRecoVote
	 * @return
	 */
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

	/**
	 * @param recommendationVote
	 * @return
	 */
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

	@Override
	public Long fetchTaxonId(Long maxRecoVoteId) {
		Recommendation reco = recoDao.findById(maxRecoVoteId);
		if (reco.getTaxonConceptId() != null)
			return reco.getTaxonConceptId();
		return null;
	}

	@Override
	public Recommendation createRecommendation(String name, Long taxonId, String canonicalName, Boolean isScientific,
			Long languageId) {
		Recommendation reco = new Recommendation(null, new Date(), name, taxonId, isScientific,
				languageId != null ? languageId : defaultLanguageId, name.toLowerCase(), null, false, null,
				canonicalName);

		Recommendation result = recoDao.save(reco);
		return result;
	}

	@Override
	public List<Long> updateCanonicalName() {
		List<Recommendation> recoList = recoDao.findAllScientificName();
		int counter = 0;
		List<Long> errorList = new ArrayList<Long>();
		for (Recommendation recommendation : recoList) {

			try {
				System.out.println("RECO ID : " + recommendation.getId());
				ParsedName parsedName = utilityService.getNameParsed(recommendation.getName());
				if (parsedName == null)
					errorList.add(recommendation.getId());
				else {
					recommendation.setCanonicalName(parsedName.getCanonicalName().getSimple());
					recoDao.update(recommendation);
					counter++;
					System.out.println("COUNTER :" + counter);
				}

			} catch (Exception e) {
				logger.error(e.getMessage());
			}

		}

		if (counter == recoList.size())
			System.out.println("ALL Reco updated");
		return errorList;

	}

	@Override
	public RecoShow removeRecoVote(HttpServletRequest request, Long observationId, Long userId, RecoSet recoSet) {

		Observation observation = observationDao.findById(observationId);
		if (!(observation.getIsLocked())) {

			RecommendationVote recoVote = recoVoteDao.findRecoVoteIdByRecoId(observationId, userId, null, null);
			if (recoVote != null) {
				recoVoteDao.delete(recoVote);
			}
			Long maxRecoVote = maxRecoVote(observationId);
			RecoShow result = new RecoShow();
			Long newMaxRecoVote = observaitonService.updateMaxVotedReco(observationId, maxRecoVote);
			if (maxRecoVote != null) {
				result = fetchCurrentRecoState(observationId, newMaxRecoVote);
			}
			String description = "";

			try {
				RecoVoteActivity rvActivity = new RecoVoteActivity();

				if (recoSet.getTaxonId() != null) {
					TaxonomyDefinition taxonomyDef = taxonomyService
							.getTaxonomyConceptName(recoSet.getTaxonId().toString());
					rvActivity.setScientificName(
							(taxonomyDef.getItalicisedForm() != null && !taxonomyDef.getItalicisedForm().isEmpty())
									? taxonomyDef.getItalicisedForm()
									: taxonomyDef.getNormalizedForm());

				}
				if (recoSet.getCommonName().trim().length() > 0)
					rvActivity.setCommonName(recoSet.getCommonName());
				if (recoSet.getScientificName().trim().length() > 0)
					rvActivity.setGivenName(recoSet.getScientificName());

				description = objectMapper.writeValueAsString(rvActivity);
//				Bg process for userGroup filter rule

				logActivities.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), description, observationId,
						observationId, "observation", observationId, "Suggestion removed",
						observaitonService.generateMailData(observationId));

				observaitonService.bgfilterRule(request, observationId);

			} catch (Exception e) {
				logger.error(e.getMessage());
			}

			return result;

		} else {
			try {
				throw new ObservationInputException("Observaiton Is locked");
			} catch (ObservationInputException e) {
				logger.error(e.getMessage());
			}
		}

		return null;

	}

	@Override
	public RecoShow agreeRecoVote(HttpServletRequest request, Long observationId, Long userId, RecoSet recoSet) {

		Observation observation = observationDao.findById(observationId);
		if (!(observation.getIsLocked())) {

			observation.setLastRevised(new Date());
			observationDao.update(observation);

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

			List<RecommendationVote> recoVoteList = recoVoteDao.findRecoVoteOnObservation(observationId);
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
						if (rVote.getCommonNameRecoId() != null && rVote.getCommonNameRecoId().equals(reco.getId()))
							finalFilteredList.add(rVote);
					}
				}
				if (finalFilteredList.isEmpty())
					recoVote = filteredList.get(0);
				else
					recoVote = finalFilteredList.get(0);
			}

			if (recoVote != null) {
				RecommendationVote previousVote = recoVoteDao.findRecoVoteIdByRecoId(observationId, userId, null, null);

				if (previousVote == null
						|| !(previousVote.getRecommendationId().equals(recoVote.getRecommendationId()))) {
					if (previousVote != null)
						recoVoteDao.delete(previousVote);

					recoVote.setId(null);
					recoVote.setAuthorId(userId);
					recoVote = recoVoteDao.save(recoVote);
					String description = "";
					try {
						RecoVoteActivity rvActivity = new RecoVoteActivity();

						if (recoSet.getTaxonId() != null) {
							TaxonomyDefinition taxonomyDef = taxonomyService
									.getTaxonomyConceptName(recoSet.getTaxonId().toString());
							rvActivity.setScientificName((taxonomyDef.getItalicisedForm() != null
									&& !taxonomyDef.getItalicisedForm().isEmpty()) ? taxonomyDef.getItalicisedForm()
											: taxonomyDef.getNormalizedForm());

						}
						if (recoSet.getCommonName() != null && recoSet.getCommonName().trim().length() > 0)
							rvActivity.setCommonName(recoSet.getCommonName());
						if (recoSet.getScientificName() != null && recoSet.getScientificName().trim().length() > 0)
							rvActivity.setGivenName(recoSet.getScientificName());

						description = objectMapper.writeValueAsString(rvActivity);
					} catch (Exception e) {
						logger.error(e.getMessage());
					}

					logActivities.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), description, observationId,
							observationId, "observation", recoVote.getId(), "Agreed on species name",
							observaitonService.generateMailData(observationId));

				}

			}
			Long maxRecoVote = maxRecoVote(observationId);
			Long newMaxRecoVote = observaitonService.updateMaxVotedReco(observationId, maxRecoVote);
//			Bg process for userGroup filter rule
			observaitonService.bgfilterRule(request, observationId);
			RecoShow result = fetchCurrentRecoState(observationId, newMaxRecoVote);

			return result;

		}
		return null;

	}

	@Override
	public RecoShow validateReco(HttpServletRequest request, CommonProfile profile, Long observationId, Long userId,
			RecoSet recoSet) {

		try {

			Observation observation = observationDao.findById(observationId);
			if (observation.getIsLocked())
				return null;

			ObservationUserPermission permission = observaitonService.getUserPermissions(request, profile,
					observationId.toString(), userId, recoSet.getTaxonId().toString());
			List<Long> permissionList = new ArrayList<Long>();
			if (permission.getValidatePermissionTaxon() != null)
				permissionList = permission.getValidatePermissionTaxon();

			if (permissionList.contains(recoSet.getTaxonId())) {

				agreeRecoVote(request, observationId, userId, recoSet);
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

				List<RecommendationVote> recoVoteList = recoVoteDao.findRecoVoteOnObservation(observationId);
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
							if (rVote.getCommonNameRecoId() != null && rVote.getCommonNameRecoId().equals(reco.getId()))
								finalFilteredList.add(rVote);
						}
					}
					if (finalFilteredList.isEmpty())
						recoVote = filteredList.get(0);
					else
						recoVote = finalFilteredList.get(0);
				}
				Long maxVotedReco = recoVote.getRecommendationId();
				observation.setIsLocked(true);
				observation.setMaxVotedRecoId(maxVotedReco);
				observation.setLastRevised(new Date());
				observation.setNoOfIdentifications(recoVoteDao.findRecoVoteCount(observationId));
				observationDao.update(observation);
				RecoShow result = fetchCurrentRecoState(observationId, maxVotedReco);

				String description = "";

				RecoVoteActivity rvActivity = new RecoVoteActivity();

				if (recoSet.getTaxonId() != null) {
					TaxonomyDefinition taxonomyDef = taxonomyService
							.getTaxonomyConceptName(recoSet.getTaxonId().toString());
					rvActivity.setScientificName(
							(taxonomyDef.getItalicisedForm() != null && !taxonomyDef.getItalicisedForm().isEmpty())
									? taxonomyDef.getItalicisedForm()
									: taxonomyDef.getNormalizedForm());

				}
				if (recoSet.getCommonName() != null && recoSet.getCommonName().trim().length() > 0)
					rvActivity.setCommonName(recoSet.getCommonName());
				if (recoSet.getScientificName() != null && recoSet.getScientificName().trim().length() > 0)
					rvActivity.setGivenName(recoSet.getScientificName());

				description = objectMapper.writeValueAsString(rvActivity);
//				Bg process for userGroup filter rule

				logActivities.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), description, observationId,
						observationId, "observation", recoVote.getId(), "obv locked",
						observaitonService.generateMailData(observationId));
				observaitonService.bgfilterRule(request, observationId);

				observaitonService.produceToRabbitMQ(observationId.toString(), "obv locked");

				return result;
			}

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public RecoShow unlockReco(HttpServletRequest request, CommonProfile profile, Long observationId, Long userId,
			RecoSet recoSet) {
		try {
			Observation observation = observationDao.findById(observationId);
			if (observation.getIsLocked()) {

				ObservationUserPermission permission = observaitonService.getUserPermissions(request, profile,
						observationId.toString(), userId, recoSet.getTaxonId().toString());
				List<Long> permissionList = new ArrayList<Long>();
				if (permission.getValidatePermissionTaxon() != null)
					permissionList = permission.getValidatePermissionTaxon();

				if (permissionList.contains(recoSet.getTaxonId())) {

					Long maxVotedReco = maxRecoVote(observationId);
					observation.setIsLocked(false);
					observation.setMaxVotedRecoId(maxVotedReco);
					observation.setLastRevised(new Date());
					observation.setNoOfIdentifications(recoVoteDao.findRecoVoteCount(observationId));
					observationDao.update(observation);
					RecoShow result = fetchCurrentRecoState(observationId, maxVotedReco);
					String description = "";

					RecoVoteActivity rvActivity = new RecoVoteActivity();

					if (recoSet.getTaxonId() != null) {
						TaxonomyDefinition taxonomyDef = taxonomyService
								.getTaxonomyConceptName(recoSet.getTaxonId().toString());
						rvActivity.setScientificName(
								(taxonomyDef.getItalicisedForm() != null && !taxonomyDef.getItalicisedForm().isEmpty())
										? taxonomyDef.getItalicisedForm()
										: taxonomyDef.getNormalizedForm());

					}
					if (recoSet.getCommonName().trim().length() > 0)
						rvActivity.setCommonName(recoSet.getCommonName());
					if (recoSet.getScientificName().trim().length() > 0)
						rvActivity.setGivenName(recoSet.getScientificName());

					description = objectMapper.writeValueAsString(rvActivity);

//					Bg process for userGroup filter rule

					logActivities.LogActivity(request.getHeader(HttpHeaders.AUTHORIZATION), description, observationId,
							observationId, "observation", observation.getMaxVotedRecoId(), "obv unlocked",
							observaitonService.generateMailData(observationId));
					observaitonService.bgfilterRule(request, observationId);

					return result;
				}
			}
			return null;
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	@Override
	public List<RecoIbp> allRecoVote(Long observationId) {
		try {
			List<RecommendationVote> recoVoteList = recoVoteDao.findRecoVoteOnObservation(observationId);
			List<RecoIbp> allRecoVotes = new ArrayList<RecoIbp>();
			for (RecommendationVote recoVote : recoVoteList) {
				Long speciesId = null;
				String scientificName = "";
				String commonName = "";
				Long taxon = null;
				UserIbp user = userService.getUserIbp(recoVote.getAuthorId().toString());
				Recommendation reco = recoDao.findById(recoVote.getRecommendationId());
				if (reco.getTaxonConceptId() != null) {
					taxon = reco.getTaxonConceptId();
					TaxonomyDefinition taxonomyDefinition = taxonomyService
							.getTaxonomyConceptName(reco.getTaxonConceptId().toString());
					scientificName = (taxonomyDefinition.getItalicisedForm() != null
							&& !taxonomyDefinition.getItalicisedForm().isEmpty())
									? taxonomyDefinition.getItalicisedForm()
									: taxonomyDefinition.getNormalizedForm();

				} else {
					scientificName = reco.getName();
				}

				if (recoVote.getCommonNameRecoId() != null) {
					commonName = recoDao.findById(recoVote.getCommonNameRecoId()).getName();
				}

				allRecoVotes.add(new RecoIbp(commonName, scientificName, taxon, speciesId, null, null, null, user));

			}
			return allRecoVotes;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;

	}

	@Override
	public RecoShow fetchCurrentRecoState(Long observationId, Long maxVotedReco) {

		RecoIbp recoIbp = fetchRecoName(observationId, maxVotedReco);
		List<RecoIbp> recoVoteList = allRecoVote(observationId);
		List<AllRecoSugguestions> allReco = observaitonService.aggregateAllRecoSuggestions(recoVoteList);
		Boolean isLocked = observationDao.findById(observationId).getIsLocked();
		RecoShow recoShow = new RecoShow(recoIbp, allReco, isLocked);
		return recoShow;
	}

	@Override
	public void recoCountRecalculate() {
		Boolean hasNext = true;
		int startPoint = 0;
		int total = 0;
		int counter = 0;
		System.out.println("Processing started!!!!!!");
		while (hasNext) {
			System.out.println("START POINT = " + startPoint);
			System.out.println("TOTAL = " + total);
			List<Observation> observationBatch = observationDao.fetchInBatchRecoCalculate(startPoint);
			System.out.println("Observation Batch Size = " + observationBatch.size());
			total = total + observationBatch.size();
			startPoint = total + 1;

			for (Observation obv : observationBatch) {
				System.out.println("Observation Id: " + obv.getId());
				int recoCount = recoVoteDao.findRecoVoteCount(obv.getId());
				System.out.println("");

				System.out.println("Actully reco Count : " + recoCount);
				System.out.println("Previous reco Count : " + obv.getNoOfIdentifications());

				System.out.println("");
				if (recoCount != obv.getNoOfIdentifications() && recoCount != 0) {
					obv.setNoOfIdentifications(recoCount);
					observationDao.update(obv);
					counter++;
					System.out.println(" Updated Observaion id : " + obv.getId());
					System.out.println("");

				}
			}

			if (observationBatch.size() != 5000) {
				System.out.println("Observation Batch inside if = " + observationBatch.size());
				System.out.println("Total = " + total);
				hasNext = false;
			}
		}
		System.out.println("Total " + total + " observation Modified  =   " + counter + "!!!!!!!!!!!!!!!!");
		System.out.println("Process complted!!!!!!!!!!!!");

	}

	@Override
	public Map<Long, List<UniqueSpeciesInfo>> getIdentifiedObservationInfo(Long userId, Long sGroupId, Boolean hasMedia,
			Long offset) {
		try {
//			map of recoId to freq count
			Map<Long, Long> recoFreq = recoVoteDao.getUniqueRecoVoteByUser(userId, sGroupId, hasMedia, offset);

			List<UniqueSpeciesInfo> uniqueSpeciesList = new ArrayList<>();
			for (Entry<Long, Long> entrySet : recoFreq.entrySet()) {
				if (entrySet.getKey() != null) {
					Recommendation reco = recoDao.findById(entrySet.getKey());
					if (reco != null) {
						Long speciesId = null;
						uniqueSpeciesList.add(new UniqueSpeciesInfo(reco.getName(), entrySet.getKey(), speciesId,
								reco.getTaxonConceptId(), entrySet.getValue()));
					}
				}

			}
			Map<Long, List<UniqueSpeciesInfo>> result = new HashMap<Long, List<UniqueSpeciesInfo>>();
			result.put(recoFreq.get(null), uniqueSpeciesList);
			return result;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public void recoCleanUp() {
		List<Recommendation> recoList = recoDao.findAll();
		Map<Long, List<Recommendation>> taxonRecoListMap = new HashMap<Long, List<Recommendation>>();
		List<Recommendation> duplicateRecoList = null;
//		grouping reco on basis of taxonId
		for (Recommendation reco : recoList) {
			if (reco.getTaxonConceptId() != null) {
				if (taxonRecoListMap.containsKey(reco.getTaxonConceptId())) {
					duplicateRecoList = taxonRecoListMap.get(reco.getTaxonConceptId());
					duplicateRecoList.add(reco);
					taxonRecoListMap.put(reco.getTaxonConceptId(), duplicateRecoList);
				} else {
					duplicateRecoList = new ArrayList<Recommendation>();
					duplicateRecoList.add(reco);
					taxonRecoListMap.put(reco.getTaxonConceptId(), duplicateRecoList);
				}
			}
		}
//		filtering out taxon having multiple reco
		for (Entry<Long, List<Recommendation>> entry : taxonRecoListMap.entrySet()) {
			if (entry.getValue().size() > 1) {
				Long latestRecoId = findLatestReco(entry.getValue());
				cleanRecoVote(latestRecoId, entry.getValue());
			}
		}

	}

//	find the latest reco Id
	private Long findLatestReco(List<Recommendation> recoList) {
		Long recoId = recoList.get(0).getId();
		Date recoDate = recoList.get(0).getLastModified();
		for (Recommendation reco : recoList) {
			if (reco.getLastModified().compareTo(recoDate) > 0) {
				recoId = reco.getId();
				recoDate = reco.getLastModified();
			}
		}

		return recoId;
	}

//	update recoVote recommendationId, update maxVotedReco and delete extra recoId
	private void cleanRecoVote(Long latestRecoId, List<Recommendation> recoList) {
		Set<Long> observationIdSet = new HashSet<Long>();
		List<Long> recoIdList = new ArrayList<Long>();
		for (Recommendation reco : recoList) {
			if (!reco.getId().equals(latestRecoId)) {
				recoIdList.add(reco.getId());
			}

		}

//		scientific name
		List<RecommendationVote> recoVoteList = recoVoteDao.findByRecoIdList(recoIdList);
		for (RecommendationVote recoVote : recoVoteList) {
			observationIdSet.add(recoVote.getObservationId());
			recoVote.setRecommendationId(latestRecoId);
			recoVoteDao.update(recoVote);
		}

//		common name
		List<RecommendationVote> recoCommonNameVoteList = recoVoteDao.findCommonNameMatchByRecoIdList(recoIdList);
		for (RecommendationVote recoVote : recoCommonNameVoteList) {
			observationIdSet.add(recoVote.getObservationId());
			recoVote.setCommonNameRecoId(null);
			recoVoteDao.update(recoVote);
		}

		for (Long obvId : observationIdSet) {
			Long maxRecoVote = maxRecoVote(obvId);
			observaitonService.updateMaxVotedReco(obvId, maxRecoVote);
		}

		for (Recommendation reco : recoList) {
			if (!reco.getId().equals(latestRecoId)) {
				recoDao.delete(reco);
			} else {
				if (!reco.getIsScientificName()) {
					reco.setIsScientificName(true);
					recoDao.update(reco);
				}

			}
		}
	}

}