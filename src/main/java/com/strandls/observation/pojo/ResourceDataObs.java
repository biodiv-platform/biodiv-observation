/**
 * 
 */
package com.strandls.observation.pojo;

/**
 * @author Abhishek Rudra
 *
 */
public class ResourceDataObs {

	private String path;
	private String url;
	private String type;
	private String caption;
	private Integer rating;
	private Long licenseId;
	private String context;

	/**
	 * 
	 */
	public ResourceDataObs() {
		super();
	}

	/**
	 * @param path
	 * @param url
	 * @param type
	 * @param caption
	 * @param rating
	 * @param licenseId
	 * @param context
	 */
	public ResourceDataObs(String path, String url, String type, String caption, Integer rating, Long licenseId,
			String context) {
		super();
		this.path = path;
		this.url = url;
		this.type = type;
		this.caption = caption;
		this.rating = rating;
		this.licenseId = licenseId;
		this.context = context;
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

	public Long getLicenseId() {
		return licenseId;
	}

	public void setLicenseId(Long licenseId) {
		this.licenseId = licenseId;
	}

	public String getContext() {
		return context;
	}

	public void setContext(String context) {
		this.context = context;
	}

}
