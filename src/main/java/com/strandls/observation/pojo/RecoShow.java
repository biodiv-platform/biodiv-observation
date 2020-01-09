/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.List;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoShow {

	private RecoIbp recoIbp;
	private List<AllRecoSugguestions> allRecoVotes;

	/**
	 * 
	 */
	public RecoShow() {
		super();
	}

	/**
	 * @param recoIbp
	 * @param allRecoVotes
	 */
	public RecoShow(RecoIbp recoIbp, List<AllRecoSugguestions> allRecoVotes) {
		super();
		this.recoIbp = recoIbp;
		this.allRecoVotes = allRecoVotes;
	}

	public RecoIbp getRecoIbp() {
		return recoIbp;
	}

	public void setRecoIbp(RecoIbp recoIbp) {
		this.recoIbp = recoIbp;
	}

	public List<AllRecoSugguestions> getAllRecoVotes() {
		return allRecoVotes;
	}

	public void setAllRecoVotes(List<AllRecoSugguestions> allRecoVotes) {
		this.allRecoVotes = allRecoVotes;
	}

}
