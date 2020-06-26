package com.strandls.observation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "download_log")
public class DownloadLog implements Serializable{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 9083318923072177738L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE)
	@Column(name = "id", nullable = false)
	private Long id;
	
	@Column(name = "version", columnDefinition = "integer default 2", nullable = false)
	private Long version;
	
	@Column(name = "author_id", nullable = false)
	private Long authorId;
	
	@Column(name = "created_on", nullable = false)
	private Timestamp createdOn;
	
	@Column(name = "file_path")
	private String filePath;
	
	@Column(name = "filter_url", nullable = false)
	private String filterUrl;
	
	@Column(name = "notes")
	private String notes;
	
	@Column(name = "params_map_as_text")
	private String paramsMapAsText;
	
	@Column(name = "status", nullable = false)
	private String status;
	
	@Column(name = "type",  nullable = false)
	private String type;
	
	@Column(name = "source_type")
	private String sourceType;
	
	@Column(name = "offset_param", nullable = false)
	private Long offsetParam;

	public DownloadLog(Long id, Long version, Long authorId, Timestamp createdOn, String filePath, String filterUrl,
			String notes, String paramsMapAsText, String status, String type, String sourceType, Long offsetParam) {
		super();
		this.id = id;
		this.version = version;
		this.authorId = authorId;
		this.createdOn = createdOn;
		this.filePath = filePath;
		this.filterUrl = filterUrl;
		this.notes = notes;
		this.paramsMapAsText = paramsMapAsText;
		this.status = status;
		this.type = type;
		this.sourceType = sourceType;
		this.offsetParam = offsetParam;
	}

	public DownloadLog() {
		}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Long getVersion() {
		return version;
	}

	public void setVersion(Long version) {
		this.version = version;
	}

	public Long getAuthorId() {
		return authorId;
	}

	public void setAuthorId(Long authorId) {
		this.authorId = authorId;
	}

	public Date getCreatedOn() {
		return createdOn;
	}

	public void setCreatedOn(Timestamp createdOn) {
		this.createdOn = createdOn;
	}

	public String getFilePath() {
		return filePath;
	}

	public void setFilePath(String filePath) {
		this.filePath = filePath;
	}

	public String getFilterUrl() {
		return filterUrl;
	}

	public void setFilterUrl(String filterUrl) {
		this.filterUrl = filterUrl;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public String getParamsMapAsText() {
		return paramsMapAsText;
	}

	public void setParamsMapAsText(String paramsMapAsText) {
		this.paramsMapAsText = paramsMapAsText;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getSourceType() {
		return sourceType;
	}

	public void setSourceType(String sourceType) {
		this.sourceType = sourceType;
	}

	public Long getOffsetParam() {
		return offsetParam;
	}

	public void setOffsetParam(Long offsetParam) {
		this.offsetParam = offsetParam;
	}
}
