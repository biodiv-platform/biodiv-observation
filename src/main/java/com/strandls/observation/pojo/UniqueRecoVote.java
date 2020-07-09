/**
 * 
 */
package com.strandls.observation.pojo;

import java.util.Date;

/**
 * @author Abhishek Rudra
 *
 */
public class UniqueRecoVote implements Comparable<UniqueRecoVote> {

	private Long recoId;
	private Boolean isCommonName;
	private Boolean isScientificName;
	private Boolean isTaxon;
	private Boolean isAccepted;
	private Integer voteCount;
	private Date lastestDate;

	/**
	 * 
	 */
	public UniqueRecoVote() {
		super();
	}

	/**
	 * @param recoId
	 * @param isCommonName
	 * @param isScientificName
	 * @param isTaxon
	 * @param isAccepted
	 * @param voteCount
	 * @param lastestDate
	 */
	public UniqueRecoVote(Long recoId, Boolean isCommonName, Boolean isScientificName, Boolean isTaxon,
			Boolean isAccepted, Integer voteCount, Date lastestDate) {
		super();
		this.recoId = recoId;
		this.isCommonName = isCommonName;
		this.isScientificName = isScientificName;
		this.isTaxon = isTaxon;
		this.isAccepted = isAccepted;
		this.voteCount = voteCount;
		this.lastestDate = lastestDate;
	}

	public Long getRecoId() {
		return recoId;
	}

	public void setRecoId(Long recoId) {
		this.recoId = recoId;
	}

	public Boolean getIsCommonName() {
		return isCommonName;
	}

	public void setIsCommonName(Boolean isCommonName) {
		this.isCommonName = isCommonName;
	}

	public Boolean getIsScientificName() {
		return isScientificName;
	}

	public void setIsScientificName(Boolean isScientificName) {
		this.isScientificName = isScientificName;
	}

	public Boolean getIsTaxon() {
		return isTaxon;
	}

	public void setIsTaxon(Boolean isTaxon) {
		this.isTaxon = isTaxon;
	}

	public Boolean getIsAccepted() {
		return isAccepted;
	}

	public void setIsAccepted(Boolean isAccepted) {
		this.isAccepted = isAccepted;
	}

	public Integer getVoteCount() {
		return voteCount;
	}

	public void setVoteCount(Integer voteCount) {
		this.voteCount = voteCount;
	}

	public Date getLastestDate() {
		return lastestDate;
	}

	public void setLastestDate(Date lastestDate) {
		this.lastestDate = lastestDate;
	}

	@Override
	public int compareTo(UniqueRecoVote o) {
		if (o == null)
			return 1;
		return checkForTaxon(o);
	}

	private int checkForTaxon(UniqueRecoVote o) {
		if (this.getIsTaxon() == o.getIsTaxon()) {
			if (this.getIsTaxon() == true) {
				return checkForAcceptedName(o);
			}
			return checkForScientificName(o);
		} else
			return this.isTaxon ? 1 : -1;
	}

	/**
	 * @param o
	 * @return
	 */
	private int checkForAcceptedName(UniqueRecoVote o) {
		if (this.getIsAccepted() == o.getIsAccepted())
			return checkForScientificName(o);
		else
			return this.getIsAccepted() ? 1 : -1;
	}

	private int checkForScientificName(UniqueRecoVote o) {
		if (this.getIsScientificName() == o.getIsScientificName())
			return checkForVote(o);
		else
			return this.isScientificName ? 1 : -1;
	}

	private int checkForVote(UniqueRecoVote o) {
		if (this.getVoteCount() > o.getVoteCount())
			return 1;
		else if (this.getVoteCount() < o.getVoteCount())
			return -1;
		else
			return checkForDate(o);
	}

	private int checkForDate(UniqueRecoVote o) {
		if (this.getLastestDate().getTime() > o.getLastestDate().getTime())
			return 1;
		else
			return -1;
	}

}