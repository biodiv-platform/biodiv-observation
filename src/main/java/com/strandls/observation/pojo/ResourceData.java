/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class ResourceData {

	private String path;
	private String url;
	private String type;
	private String caption;
	private Integer rating;
	private Long licenceId;

	/**
	 * 
	 */
	public ResourceData() {
		super();
	}

	/**
	 * @param path
	 * @param url
	 * @param type
	 * @param caption
	 * @param rating
	 * @param licenceId
	 */
	public ResourceData(String path, String url, String type, String caption, Integer rating, Long licenceId) {
		super();
		this.path = path;
		this.url = url;
		this.type = type;
		this.caption = caption;
		this.rating = rating;
		this.licenceId = licenceId;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getCaption() {
		return caption;
	}

	public void setCaption(String caption) {
		this.caption = caption;
	}

	public Integer getRating() {
		return rating;
	}

	public void setRating(Integer rating) {
		this.rating = rating;
	}

	public Long getLicenceId() {
		return licenceId;
	}

	public void setLicenceId(Long licenceId) {
		this.licenceId = licenceId;
	}

}
