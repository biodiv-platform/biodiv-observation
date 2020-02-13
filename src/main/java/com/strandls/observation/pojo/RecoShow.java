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
	private Boolean isLocked;

	/**
	 * 
	 */
	public RecoShow() {
		super();
	}

	/**
	 * @param recoIbp
	 * @param allRecoVotes
	 * @param isLocked
	 */
	public RecoShow(RecoIbp recoIbp, List<AllRecoSugguestions> allRecoVotes, Boolean isLocked) {
		super();
		this.recoIbp = recoIbp;
		this.allRecoVotes = allRecoVotes;
		this.isLocked = isLocked;
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

	public Boolean getIsLocked() {
		return isLocked;
	}

	public void setIsLocked(Boolean isLocked) {
		this.isLocked = isLocked;
	}

}
