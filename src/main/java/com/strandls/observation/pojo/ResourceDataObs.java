/**
 * 
 */
package com.strandls.observation.pojo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author Abhishek Rudra
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ResourceDataObs {

	private String path;
	private String url;
	private String type;
	private String caption;
	private Integer rating;
	private Long licenseId;
	private String context;
	private Long languageId;
	private String contributor;

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
	 * @param languageId
	 * @param contributor
	 */
	public ResourceDataObs(String path, String url, String type, String caption, Integer rating, Long licenseId,
			String context, Long languageId, String contributor) {
		super();
		this.path = path;
		this.url = url;
		this.type = type;
		this.caption = caption;
		this.rating = rating;
		this.licenseId = licenseId;
		this.context = context;
		this.languageId = languageId;
		this.contributor = contributor;
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

	public Long getLanguageId() {
		return languageId;
	}

	public void setLanguageId(Long languageId) {
		this.languageId = languageId;
	}

	public String getContributor() {
		return contributor;
	}

	public void setContributor(String contributor) {
		this.contributor = contributor;
	}

	@Override
	public String toString() {
		return "ResourceDataObs [path=" + path + ", url=" + url + ", type=" + type + ", caption=" + caption
				+ ", rating=" + rating + ", licenseId=" + licenseId + ", context=" + context + ", languageId="
				+ languageId + ", contributor=" + contributor + "]";
	}

}
