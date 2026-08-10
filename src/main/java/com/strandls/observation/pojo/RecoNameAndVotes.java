/**
 *
 */
package com.strandls.observation.pojo;

import java.util.List;

/**
 * Bundles the top-voted identification and the full list of identification
 * votes for an observation, both derived from a single
 * {@code recommendation_vote} fetch instead of two separate ones.
 *
 * @author Abhishek Rudra
 *
 */
public class RecoNameAndVotes {

	private RecoIbp reco;
	private List<RecoIbp> allRecoVotes;

	/**
	 *
	 */
	public RecoNameAndVotes() {
		super();
	}

	/**
	 * @param reco
	 * @param allRecoVotes
	 */
	public RecoNameAndVotes(RecoIbp reco, List<RecoIbp> allRecoVotes) {
		super();
		this.reco = reco;
		this.allRecoVotes = allRecoVotes;
	}

	public RecoIbp getReco() {
		return reco;
	}

	public void setReco(RecoIbp reco) {
		this.reco = reco;
	}

	public List<RecoIbp> getAllRecoVotes() {
		return allRecoVotes;
	}

	public void setAllRecoVotes(List<RecoIbp> allRecoVotes) {
		this.allRecoVotes = allRecoVotes;
	}

}
