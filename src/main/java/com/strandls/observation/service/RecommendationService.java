/**
 * 
 */
package com.strandls.observation.service;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.pac4j.core.profile.CommonProfile;

import com.strandls.observation.pojo.RecoCreate;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.RecoSet;
import com.strandls.observation.pojo.RecoShow;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.pojo.UniqueSpeciesInfo;

/**
 * @author Abhishek Rudra
 *
 */
public interface RecommendationService {

	public RecoIbp fetchRecoVote(Long id);

	public RecoIbp fetchByRecoId(Long recoId);

	public RecoIbp fetchRecoName(Long obvId, Long recoId);

	public Long createRecoVote(HttpServletRequest request, Long userId, Long observationId, Long taxonId,
			RecoCreate recoCreate, Boolean creataObservation);

	public Long fetchTaxonId(Long maxRecoVoteId);

	public Recommendation createRecommendation(String name, Long taxonId, String canonicalName, Boolean isScientific,
			Long languageId);

	public List<Long> updateCanonicalName();

	public RecoShow removeRecoVote(HttpServletRequest request, Long observationId, Long userId, RecoSet recoSet);

	public RecoShow agreeRecoVote(HttpServletRequest request, Long observationId, Long userId, RecoSet recoSet);

	public RecoShow validateReco(HttpServletRequest request, CommonProfile profile, Long observationId, Long userId,
			RecoSet recoSet);

	public RecoShow unlockReco(HttpServletRequest request, CommonProfile profile, Long observationId, Long userId,
			RecoSet recoSet);

	public List<RecoIbp> allRecoVote(Long observationId);

	public RecoShow fetchCurrentRecoState(Long observationId, Long maxVotedReco);

	public void recoCountRecalculate();

	public Map<Long, List<UniqueSpeciesInfo>> getIdentifiedObservationInfo(Long userId, Long sGroupId, Boolean hasMedia,
			Long offset);

	public void recoCleanUp();
}
