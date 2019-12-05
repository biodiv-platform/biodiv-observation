/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoSet {

	private String commonName;
	private String scientificName;

	/**
	 * 
	 */
	public RecoSet() {
		super();
	}

	/**
	 * @param commonName
	 * @param scientificName
	 */
	public RecoSet(String commonName, String scientificName) {
		super();
		this.commonName = commonName;
		this.scientificName = scientificName;
	}

	public String getCommonName() {
		return commonName;
	}

	public void setCommonName(String commonName) {
		this.commonName = commonName;
	}

	public String getScientificName() {
		return scientificName;
	}

	public void setScientificName(String scientificName) {
		this.scientificName = scientificName;
	}

}
