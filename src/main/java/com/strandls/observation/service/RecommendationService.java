/**
 * 
 */
package com.strandls.observation.service;

import java.util.List;

import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.Recommendation;

/**
 * @author Abhishek Rudra
 *
 */
public interface RecommendationService {

	public RecoIbp fetchRecoVote(Long id);

	public RecoIbp fetchRecoName(Long obvId, Long recoId);

	public Long createRecoVote(Long userId, Long observationId, RecoCreate recoCreate);

	public Long fetchTaxonId(Long maxRecoVoteId);

	public Recommendation createRecommendation(String name, String canoncialName, Boolean isScientific);

	public List<Long> updateCanonicalName();

	public RecoIbp removeRecoVote(Long observationId, Long userId, RecoSet recoSet);

	public RecoIbp agreeRecoVote(Long observationId, Long userId, RecoSet recoSet);

	public RecoIbp validateReco(Long observationId, Long userId, RecoSet recoSet);

	public RecoIbp unlockReco(Long observationId, Long userId, RecoSet recoSet);

	public List<RecoIbp> allRecoVote(Long observationId);
}
