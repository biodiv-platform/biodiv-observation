/**
 * 
 */
package com.strandls.observation.service.Impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.dao.RecommendationVoteDao;
import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.pojo.RecommendationVote;
import com.strandls.observation.pojo.UniqueRecoVote;
import com.strandls.observation.service.ObservationService;
import com.strandls.observation.service.RecommendationService;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.pojo.BreadCrumb;
import com.strandls.taxonomy.pojo.TaxonomyDefinition;
import com.strandls.utility.controller.UtilityServiceApi;
import com.strandls.utility.pojo.ParsedName;

/**
 * @author Abhishek Rudra
 *
 */
public class RecommendationServiceImpl implements RecommendationService {

	private final Logger logger = LoggerFactory.getLogger(RecommendationServiceImpl.class);

	@Inject
	private ObservationService observaitonService;

	@Inject
	private RecommendationVoteDao recoVoteDao;

	@Inject
	private RecommendationDao recoDao;

	@Inject
	private TaxonomyServicesApi taxonomyService;

	@Inject
	private UtilityServiceApi utilityService;

	@Override
	public RecoIbp fetchRecoVote(Long id) {

		String givenName = "";
		String scientificName = "";
		Long speciesId = null;
		RecoIbp ibpData = null;
		RecommendationVote recoVote = recoVoteDao.findById(id);

		if (recoVote.getGivenSciName() != null)
			givenName = givenName + " " + recoVote.getGivenSciName();
		if (recoVote.getGivenCommonName() != null)
			givenName = givenName + " " + recoVote.getGivenCommonName();

		Recommendation reco = recoDao.findById(recoVote.getRecommendationId());

		try {
			if (reco.getTaxonConceptId() != null) {

				TaxonomyDefinition taxonomyDefinition = taxonomyService
						.getTaxonomyConceptName(reco.getTaxonConceptId().toString());
				speciesId = taxonomyDefinition.getSpeciesId();
			}
			scientificName = scientificName + reco.getName();
			if (recoVote.getCommonNameRecoId() != null && recoVote.getGivenCommonName() != null) {
				Recommendation recoCommon = recoDao.findById(recoVote.getCommonNameRecoId());
				scientificName = scientificName + " " + recoCommon.getName();
			}

			ibpData = new RecoIbp(givenName, scientificName, speciesId, null, null, null);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return ibpData;
	}

	@Override
	public RecoIbp fetchRecoName(Long obvId, Long recoId) {

		Long speciesId = null;
		String commonName = "";
		String scientificName = "";
		List<BreadCrumb> breadCrumb = null;
		String status = null;

		try {
			List<RecommendationVote> recoVotes = recoVoteDao.findByRecommendationId(obvId, recoId);
			Integer recoVoteCount = recoVoteDao.findRecoVoteCount(obvId);
			Recommendation reco = recoDao.findById(recoId);
			if (reco.getTaxonConceptId() != null) {

				TaxonomyDefinition taxonomyDefinition = taxonomyService
						.getTaxonomyConceptName(reco.getTaxonConceptId().toString());
				speciesId = taxonomyDefinition.getSpeciesId();
				breadCrumb = taxonomyService.getTaxonomyBreadCrumb(reco.getTaxonConceptId().toString());
				scientificName = taxonomyDefinition.getNormalizedForm();
				status = taxonomyDefinition.getStatus();

			} else {
				scientificName = reco.getName();
			}

			for (RecommendationVote recoVote : recoVotes) {
				if (recoVote.getCommonNameRecoId() != null) {
					commonName = commonName + recoDao.findById(recoVote.getCommonNameRecoId()).getName() + "||";
				}
			}
			if (!(commonName.isEmpty()))
				commonName = commonName.substring(0, commonName.length() - 2);

			return new RecoIbp(commonName, scientificName, speciesId, breadCrumb, recoVoteCount, status);

		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return null;
	}

	@Override
	public Long createRecoVote(Long userId, Long observationId, RecoCreate recoCreate) {

		RecommendationVote previousVote = recoVoteDao.findRecoVoteIdByRecoId(observationId, userId, null, null);
		if (previousVote != null) {
			recoVoteDao.delete(previousVote);
		}

		RecommendationVote recoVote = null;
		if (recoCreate.getScientificNameId() != null) {
			recoVote = new RecommendationVote(null, 0L, userId, recoCreate.getConfidence(), observationId,
					recoCreate.getScientificNameId(), 0, new Date(), recoCreate.getRecoComment(),
					recoCreate.getCommonNameId(), recoCreate.getCommonName(), recoCreate.getScientificName(), null,
					recoCreate.getFlag());
		} else {
			recoVote = new RecommendationVote(null, 0L, userId, recoCreate.getConfidence(), observationId,
					recoCreate.getCommonNameId(), 0, new Date(), recoCreate.getRecoComment(),
					recoCreate.getCommonNameId(), recoCreate.getCommonName(), null, null, recoCreate.getFlag());
		}
		recoVote = recoVoteDao.save(recoVote);
		Long maxRecoVote = maxRecoVote(observationId);
		return maxRecoVote;

	}

	private Long maxRecoVote(Long observationId) {
		List<RecommendationVote> recoVoteList = recoVoteDao.findRecoVoteOnObservation(observationId);

		Map<Long, UniqueRecoVote> resultMap = prepareUniqueRecord(recoVoteList);
		UniqueRecoVote maxRecoVote = null;
		for (Entry<Long, UniqueRecoVote> entry : resultMap.entrySet()) {
			int value = entry.getValue().compareTo(maxRecoVote);
			if (value > 0) {
				maxRecoVote = entry.getValue();
			}
		}
		return maxRecoVote.getRecoId();
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
	public Recommendation createRecommendation(String name, String canonicalName, Boolean isScientific) {
		Recommendation reco = new Recommendation(null, new Date(), name, null, isScientific, 205L, name.toLowerCase(),
				null, false, null, canonicalName);

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
	public RecoIbp removeRecoVote(Long observationId, Long userId, RecoSet recoSet) {
		Recommendation scientificNameReco = null;
		Recommendation commonNameReco = null;
		if (recoSet.getScientificName() != null)
			scientificNameReco = recoDao.findByRecoName(recoSet.getScientificName(), true);
		if (recoSet.getCommonName() != null)
			commonNameReco = recoDao.findByRecoName(recoSet.getCommonName(), false);
		RecommendationVote recoVote = recoVoteDao.findRecoVoteIdByRecoId(observationId, userId,
				scientificNameReco.getId(), commonNameReco.getId());
		if (recoVote != null) {
			recoVoteDao.delete(recoVote);
		}
		Long maxRecoVote = maxRecoVote(observationId);
		Long newMaxRecoVote = observaitonService.updateMaxVotedReco(observationId, maxRecoVote);
		RecoIbp result = fetchRecoName(observationId, newMaxRecoVote);
		return result;

	}

	@Override
	public RecoIbp agreeRecoVote(Long observationId, Long userId, RecoSet recoSet) {

		Recommendation scientificNameReco = null;
		Recommendation commonNameReco = null;
		if (recoSet.getScientificName() != null)
			scientificNameReco = recoDao.findByRecoName(recoSet.getScientificName(), true);
		if (recoSet.getCommonName() != null)
			commonNameReco = recoDao.findByRecoName(recoSet.getCommonName(), false);
		RecommendationVote recoVote = recoVoteDao.findRecoVoteIdByRecoId(observationId, null,
				scientificNameReco.getId(), commonNameReco.getId());
		if (recoVote != null) {
			RecommendationVote previousVote = recoVoteDao.findRecoVoteIdByRecoId(observationId, userId, null, null);
			if (previousVote != null) {
				recoVoteDao.delete(previousVote);
			}
			recoVote.setId(null);
			recoVote.setAuthorId(userId);
			recoVoteDao.save(recoVote);
		}
		Long maxRecoVote = maxRecoVote(observationId);
		Long newMaxRecoVote = observaitonService.updateMaxVotedReco(observationId, maxRecoVote);
		RecoIbp result = fetchRecoName(observationId, newMaxRecoVote);

		return result;
	}

}