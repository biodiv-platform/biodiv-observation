/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class RecoIbp {

	private String commonName;
	private String ScientificName;
	private Long speciesId;

	/**
	 * @param commonName
	 * @param scientificName
	 * @param speciesId
	 */
	public RecoIbp(String commonName, String scientificName, Long speciesId) {
		super();
		this.commonName = commonName;
		ScientificName = scientificName;
		this.speciesId = speciesId;
	}

	public String getCommonName() {
		return commonName;
	}

	public void setCommonName(String commonName) {
		this.commonName = commonName;
	}

	public String getScientificName() {
		return ScientificName;
	}

	public void setScientificName(String scientificName) {
		ScientificName = scientificName;
	}

	public Long getSpeciesId() {
		return speciesId;
	}

	public void setSpeciesId(Long speciesId) {
		this.speciesId = speciesId;
	}

}
