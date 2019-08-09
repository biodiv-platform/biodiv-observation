/**
 * 
 */
package com.strandls.observation.service.Impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.observation.dao.RecommendationDao;
import com.strandls.observation.dao.RecommendationVoteDao;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.pojo.RecommendationVote;
import com.strandls.observation.service.RecommedationService;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.pojo.TaxonomyDefinition;

/**
 * @author Abhishek Rudra
 *
 */
public class RecommendationServiceImpl implements RecommedationService {

	private final Logger logger = LoggerFactory.getLogger(RecommendationServiceImpl.class);
	
	@Inject
	private RecommendationVoteDao recoVoteDao;

	@Inject
	private RecommendationDao recoDao;

	@Inject
	private TaxonomyServicesApi taxonomyService;

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
			
			ibpData = new RecoIbp(givenName, scientificName, speciesId);
			
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return ibpData;
	}

}
