/**
 * 
 */
package com.strandls.observation.service;

import java.util.List;

import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.RecoShow;
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

	public Recommendation createRecommendation(String name, Long taxonId, String canonicalName, Boolean isScientific);

	public List<Long> updateCanonicalName();

	public RecoShow removeRecoVote(Long observationId, Long userId, RecoSet recoSet);

	public RecoShow agreeRecoVote(Long observationId, Long userId, RecoSet recoSet);

	public RecoShow validateReco(Long observationId, Long userId, RecoSet recoSet);

	public RecoShow unlockReco(Long observationId, Long userId, RecoSet recoSet);

	public List<RecoIbp> allRecoVote(Long observationId);

	public RecoShow fetchCurrentRecoState(Long observationId, Long maxVotedReco);
}
