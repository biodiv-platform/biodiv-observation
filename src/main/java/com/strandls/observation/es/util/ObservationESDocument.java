/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityResult;
import javax.persistence.FieldResult;
import javax.persistence.Id;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.SqlResultSetMappings;

import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.TypeDefs;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vladmihalcea.hibernate.type.json.JsonStringType;

/**
 * @author Abhishek Rudra
 *
 */

@SqlResultSetMappings({ @SqlResultSetMapping(name = "ObservaationESDocumentMapping", entities = {
		@EntityResult(entityClass = ObservationESDocument.class, fields = {
				@FieldResult(name = "observation_id", column = "observation_id"),
				@FieldResult(name = "author_id", column = "author_id"),
				@FieldResult(name = "created_by", column = "created_by"),
				@FieldResult(name = "profile_pic", column = "profile_pic"),
				@FieldResult(name = "created_on", column = "created_on"),
				@FieldResult(name = "group_id", column = "group_id"),
				@FieldResult(name = "group_name", column = "group_name"),
				@FieldResult(name = "location", column = "location"), @FieldResult(name = "notes", column = "notes"),
				@FieldResult(name = "from_date", column = "from_date"),
				@FieldResult(name = "observed_in_month", column = "observed_in_month"),
				@FieldResult(name = "place_name", column = "place_name"),
				@FieldResult(name = "reverse_geocoded_name", column = "reverse_geocoded_name"),
				@FieldResult(name = "flag_count", column = "flag_count"),
				@FieldResult(name = "geo_privacy", column = "geo_privacy"),
				@FieldResult(name = "last_revised", column = "last_revised"),
				@FieldResult(name = "visit_count", column = "visit_count"),
				@FieldResult(name = "is_checklist", column = "is_checklist"),
				@FieldResult(name = "to_date", column = "to_date"),
				@FieldResult(name = "is_locked", column = "is_locked"),
				@FieldResult(name = "language_id", column = "language_id"),
				@FieldResult(name = "location_scale", column = "location_scale"),
				@FieldResult(name = "dataset_id", column = "dataset_id"),
				@FieldResult(name = "dataset_title", column = "dataset_title"),
				@FieldResult(name = "repr_image_id", column = "repr_image_id"),
				@FieldResult(name = "protocol", column = "protocol"),
				@FieldResult(name = "no_of_images", column = "no_of_images"),
				@FieldResult(name = "no_of_videos", column = "no_of_videos"),
				@FieldResult(name = "no_of_audio", column = "no_of_audio"),
				@FieldResult(name = "no_media", column = "no_media"),
				@FieldResult(name = "no_of_identifications", column = "no_of_identifications"),
				@FieldResult(name = "data_table_id", column = "data_table_id"),
				@FieldResult(name = "date_accuracy", column = "date_accuracy"),
				@FieldResult(name = "max_voted_reco", column = "max_voted_reco"),
				@FieldResult(name = "all_reco_vote", column = "all_reco_vote"),
				@FieldResult(name = "observation_resource", column = "observation_resource"),
				@FieldResult(name = "user_group_observations", column = "user_group_observations"),
				@FieldResult(name = "tags", column = "tags"), @FieldResult(name = "flags", column = "flags"),
				@FieldResult(name = "featured", column = "featured"),
				@FieldResult(name = "facts", column = "facts") }) }) })

@Entity
@TypeDefs({ @TypeDef(name = "json", typeClass = JsonStringType.class) })
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObservationESDocument {

	@Id
	private Long observation_id;
	private Long author_id;
	private String created_by;
	private String profile_pic;
	private Date created_on;
	private Long group_id;
	private String group_name;
	private String sgroup_filter;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private Location location;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private LocationInformation location_information;
	private String notes;
	private Date from_date;
	private String observed_in_month;
	private String place_name;
	private String reverse_geocoded_name;
	private Long flag_count;
	private Boolean geo_privacy;
	private Date last_revised;
	private Long visit_count;
	private Boolean is_checklist;
	private Date to_date;
	private Boolean is_locked;
	private Integer language_id;
	private String location_scale;
	private Long dataset_id;
	private String dataset_title;
	private Long repr_image_id;
	private String repr_image_url;
	private String protocol;
	private Integer no_of_images;
	private Integer no_of_videos;
	private Integer no_of_audio;
	private Integer no_media;
	private Integer no_of_identifications;
	private Long data_table_id;
	private String date_accuracy;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private Max_voted_reco max_voted_reco;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<All_reco_vote> all_reco_vote;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<Observation_resource> observation_resource;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<Custom_fields> custom_fields;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<User_group_observations> user_group_observations;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<Tags> tags;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<Flags> flags;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<Featured> featured;
	@Type(type = "json")
	@Column(columnDefinition = "json")
	private List<Facts> facts;

	/**
	 * 
	 */
	public ObservationESDocument() {
		super();
	}

	/**
	 * @param observation_id
	 * @param author_id
	 * @param created_by
	 * @param profile_pic
	 * @param created_on
	 * @param group_id
	 * @param group_name
	 * @param sgroup_filter
	 * @param location
	 * @param location_information
	 * @param notes
	 * @param from_date
	 * @param observed_in_month
	 * @param place_name
	 * @param reverse_geocoded_name
	 * @param flag_count
	 * @param geo_privacy
	 * @param last_revised
	 * @param visit_count
	 * @param is_checklist
	 * @param to_date
	 * @param is_locked
	 * @param language_id
	 * @param location_scale
	 * @param dataset_id
	 * @param dataset_title
	 * @param repr_image_id
	 * @param repr_image_url
	 * @param protocol
	 * @param no_of_images
	 * @param no_of_videos
	 * @param no_of_audio
	 * @param no_media
	 * @param no_of_identifications
	 * @param data_table_id
	 * @param date_accuracy
	 * @param max_voted_reco
	 * @param all_reco_vote
	 * @param observation_resource
	 * @param custom_fields
	 * @param user_group_observations
	 * @param tags
	 * @param flags
	 * @param featured
	 * @param facts
	 */
	public ObservationESDocument(Long observation_id, Long author_id, String created_by, String profile_pic,
			Date created_on, Long group_id, String group_name, String sgroup_filter, Location location,
			LocationInformation location_information, String notes, Date from_date, String observed_in_month,
			String place_name, String reverse_geocoded_name, Long flag_count, Boolean geo_privacy, Date last_revised,
			Long visit_count, Boolean is_checklist, Date to_date, Boolean is_locked, Integer language_id,
			String location_scale, Long dataset_id, String dataset_title, Long repr_image_id, String repr_image_url,
			String protocol, Integer no_of_images, Integer no_of_videos, Integer no_of_audio, Integer no_media,
			Integer no_of_identifications, Long data_table_id, String date_accuracy, Max_voted_reco max_voted_reco,
			List<All_reco_vote> all_reco_vote, List<Observation_resource> observation_resource,
			List<Custom_fields> custom_fields, List<User_group_observations> user_group_observations, List<Tags> tags,
			List<Flags> flags, List<Featured> featured, List<Facts> facts) {
		super();
		this.observation_id = observation_id;
		this.author_id = author_id;
		this.created_by = created_by;
		this.profile_pic = profile_pic;
		this.created_on = created_on;
		this.group_id = group_id;
		this.group_name = group_name;
		this.sgroup_filter = sgroup_filter;
		this.location = location;
		this.location_information = location_information;
		this.notes = notes;
		this.from_date = from_date;
		this.observed_in_month = observed_in_month;
		this.place_name = place_name;
		this.reverse_geocoded_name = reverse_geocoded_name;
		this.flag_count = flag_count;
		this.geo_privacy = geo_privacy;
		this.last_revised = last_revised;
		this.visit_count = visit_count;
		this.is_checklist = is_checklist;
		this.to_date = to_date;
		this.is_locked = is_locked;
		this.language_id = language_id;
		this.location_scale = location_scale;
		this.dataset_id = dataset_id;
		this.dataset_title = dataset_title;
		this.repr_image_id = repr_image_id;
		this.repr_image_url = repr_image_url;
		this.protocol = protocol;
		this.no_of_images = no_of_images;
		this.no_of_videos = no_of_videos;
		this.no_of_audio = no_of_audio;
		this.no_media = no_media;
		this.no_of_identifications = no_of_identifications;
		this.data_table_id = data_table_id;
		this.date_accuracy = date_accuracy;
		this.max_voted_reco = max_voted_reco;
		this.all_reco_vote = all_reco_vote;
		this.observation_resource = observation_resource;
		this.custom_fields = custom_fields;
		this.user_group_observations = user_group_observations;
		this.tags = tags;
		this.flags = flags;
		this.featured = featured;
		this.facts = facts;
	}

	public Long getObservation_id() {
		return observation_id;
	}

	public void setObservation_id(Long observation_id) {
		this.observation_id = observation_id;
	}

	public Long getAuthor_id() {
		return author_id;
	}

	public void setAuthor_id(Long author_id) {
		this.author_id = author_id;
	}

	public String getCreated_by() {
		return created_by;
	}

	public void setCreated_by(String created_by) {
		this.created_by = created_by;
	}

	public String getProfile_pic() {
		return profile_pic;
	}

	public void setProfile_pic(String profile_pic) {
		this.profile_pic = profile_pic;
	}

	public Date getCreated_on() {
		return created_on;
	}

	public void setCreated_on(Date created_on) {
		this.created_on = created_on;
	}

	public Long getGroup_id() {
		return group_id;
	}

	public void setGroup_id(Long group_id) {
		this.group_id = group_id;
	}

	public String getGroup_name() {
		return group_name;
	}

	public void setGroup_name(String group_name) {
		this.group_name = group_name;
	}

	public String getSgroup_filter() {
		return sgroup_filter;
	}

	public void setSgroup_filter(String sgroup_filter) {
		this.sgroup_filter = sgroup_filter;
	}

	public Location getLocation() {
		return location;
	}

	public void setLocation(Location location) {
		this.location = location;
	}

	public LocationInformation getLocation_information() {
		return location_information;
	}

	public void setLocation_information(LocationInformation location_information) {
		this.location_information = location_information;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public Date getFrom_date() {
		return from_date;
	}

	public void setFrom_date(Date from_date) {
		this.from_date = from_date;
	}

	public String getObserved_in_month() {
		return observed_in_month;
	}

	public void setObserved_in_month(String observed_in_month) {
		this.observed_in_month = observed_in_month;
	}

	public String getPlace_name() {
		return place_name;
	}

	public void setPlace_name(String place_name) {
		this.place_name = place_name;
	}

	public String getReverse_geocoded_name() {
		return reverse_geocoded_name;
	}

	public void setReverse_geocoded_name(String reverse_geocoded_name) {
		this.reverse_geocoded_name = reverse_geocoded_name;
	}

	public Long getFlag_count() {
		return flag_count;
	}

	public void setFlag_count(Long flag_count) {
		this.flag_count = flag_count;
	}

	public Boolean getGeo_privacy() {
		return geo_privacy;
	}

	public void setGeo_privacy(Boolean geo_privacy) {
		this.geo_privacy = geo_privacy;
	}

	public Date getLast_revised() {
		return last_revised;
	}

	public void setLast_revised(Date last_revised) {
		this.last_revised = last_revised;
	}

	public Long getVisit_count() {
		return visit_count;
	}

	public void setVisit_count(Long visit_count) {
		this.visit_count = visit_count;
	}

	public Boolean getIs_checklist() {
		return is_checklist;
	}

	public void setIs_checklist(Boolean is_checklist) {
		this.is_checklist = is_checklist;
	}

	public Date getTo_date() {
		return to_date;
	}

	public void setTo_date(Date to_date) {
		this.to_date = to_date;
	}

	public Boolean getIs_locked() {
		return is_locked;
	}

	public void setIs_locked(Boolean is_locked) {
		this.is_locked = is_locked;
	}

	public Integer getLanguage_id() {
		return language_id;
	}

	public void setLanguage_id(Integer language_id) {
		this.language_id = language_id;
	}

	public String getLocation_scale() {
		return location_scale;
	}

	public void setLocation_scale(String location_scale) {
		this.location_scale = location_scale;
	}

	public Long getDataset_id() {
		return dataset_id;
	}

	public void setDataset_id(Long dataset_id) {
		this.dataset_id = dataset_id;
	}

	public String getDataset_title() {
		return dataset_title;
	}

	public void setDataset_title(String dataset_title) {
		this.dataset_title = dataset_title;
	}

	public Long getRepr_image_id() {
		return repr_image_id;
	}

	public void setRepr_image_id(Long repr_image_id) {
		this.repr_image_id = repr_image_id;
	}

	public String getRepr_image_url() {
		return repr_image_url;
	}

	public void setRepr_image_url(String repr_image_url) {
		this.repr_image_url = repr_image_url;
	}

	public String getProtocol() {
		return protocol;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	public Integer getNo_of_images() {
		return no_of_images;
	}

	public void setNo_of_images(Integer no_of_images) {
		this.no_of_images = no_of_images;
	}

	public Integer getNo_of_videos() {
		return no_of_videos;
	}

	public void setNo_of_videos(Integer no_of_videos) {
		this.no_of_videos = no_of_videos;
	}

	public Integer getNo_of_audio() {
		return no_of_audio;
	}

	public void setNo_of_audio(Integer no_of_audio) {
		this.no_of_audio = no_of_audio;
	}

	public Integer getNo_media() {
		return no_media;
	}

	public void setNo_media(Integer no_media) {
		this.no_media = no_media;
	}

	public Integer getNo_of_identifications() {
		return no_of_identifications;
	}

	public void setNo_of_identifications(Integer no_of_identifications) {
		this.no_of_identifications = no_of_identifications;
	}

	public Long getData_table_id() {
		return data_table_id;
	}

	public void setData_table_id(Long data_table_id) {
		this.data_table_id = data_table_id;
	}

	public String getDate_accuracy() {
		return date_accuracy;
	}

	public void setDate_accuracy(String date_accuracy) {
		this.date_accuracy = date_accuracy;
	}

	public Max_voted_reco getMax_voted_reco() {
		return max_voted_reco;
	}

	public void setMax_voted_reco(Max_voted_reco max_voted_reco) {
		this.max_voted_reco = max_voted_reco;
	}

	public List<All_reco_vote> getAll_reco_vote() {
		return all_reco_vote;
	}

	public void setAll_reco_vote(List<All_reco_vote> all_reco_vote) {
		this.all_reco_vote = all_reco_vote;
	}

	public List<Observation_resource> getObservation_resource() {
		return observation_resource;
	}

	public void setObservation_resource(List<Observation_resource> observation_resource) {
		this.observation_resource = observation_resource;
	}

	public List<Custom_fields> getCustom_fields() {
		return custom_fields;
	}

	public void setCustom_fields(List<Custom_fields> custom_fields) {
		this.custom_fields = custom_fields;
	}

	public List<User_group_observations> getUser_group_observations() {
		return user_group_observations;
	}

	public void setUser_group_observations(List<User_group_observations> user_group_observations) {
		this.user_group_observations = user_group_observations;
	}

	public List<Tags> getTags() {
		return tags;
	}

	public void setTags(List<Tags> tags) {
		this.tags = tags;
	}

	public List<Flags> getFlags() {
		return flags;
	}

	public void setFlags(List<Flags> flags) {
		this.flags = flags;
	}

	public List<Featured> getFeatured() {
		return featured;
	}

	public void setFeatured(List<Featured> featured) {
		this.featured = featured;
	}

	public List<Facts> getFacts() {
		return facts;
	}

	public void setFacts(List<Facts> facts) {
		this.facts = facts;
	}

	// sequence

}

//========Location========

class Location {
	private Double lat;
	private Double lon;

	/**
	 * 
	 */
	public Location() {
		super();
	}

	/**
	 * @param lat
	 * @param lon
	 */
	public Location(Double lat, Double lon) {
		super();
		this.lat = lat;
		this.lon = lon;
	}

	public Double getLat() {
		return lat;
	}

	public void setLat(Double lat) {
		this.lat = lat;
	}

	public Double getLon() {
		return lon;
	}

	public void setLon(Double lon) {
		this.lon = lon;
	}

}

//==========Observation Resource ===========
class Observation_resource {

	@Id
	private Long id;
	private String description;
	private String file_name;
	private String type;
	private String url;
	private Integer rating;
	private Date upload_time;
	private Integer uploader_id;
	private Integer license_id;

	/**
	 * 
	 */
	public Observation_resource() {
		super();
	}

	/**
	 * @param id
	 * @param description
	 * @param file_name
	 * @param type
	 * @param url
	 * @param rating
	 * @param upload_time
	 * @param uploader_id
	 * @param license_id
	 */
	public Observation_resource(Long id, String description, String file_name, String type, String url, Integer rating,
			Date upload_time, Integer uploader_id, Integer license_id) {
		super();
		this.id = id;
		this.description = description;
		this.file_name = file_name;
		this.type = type;
		this.url = url;
		this.rating = rating;
		this.upload_time = upload_time;
		this.uploader_id = uploader_id;
		this.license_id = license_id;
	}

	public String getFile_name() {
		return file_name;
	}

	public void setFile_name(String file_name) {
		this.file_name = file_name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Integer getRating() {
		return rating;
	}

	public void setRating(Integer rating) {
		this.rating = rating;
	}

	public Integer getLicense_id() {
		return license_id;
	}

	public void setLicense_id(Integer license_id) {
		this.license_id = license_id;
	}

	public Integer getUploader_id() {
		return uploader_id;
	}

	public void setUploader_id(Integer uploader_id) {
		this.uploader_id = uploader_id;
	}

	public Date getUpload_time() {
		return upload_time;
	}

	public void setUpload_time(Date upload_time) {
		this.upload_time = upload_time;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

}

//	============featured================

class Featured {
	private Long id;
	private Long author_id;
	private String author_name;
	private String profile_pic;
	private Date created_on;
	private String notes;
	private Long user_group_id;
	private Long language_id;
	private String language_name;

	/**
	 * 
	 */
	public Featured() {
		super();
	}

	/**
	 * @param id
	 * @param author_id
	 * @param author_name
	 * @param profile_pic
	 * @param created_on
	 * @param notes
	 * @param user_group_id
	 * @param language_id
	 * @param language_name
	 */
	public Featured(Long id, Long author_id, String author_name, String profile_pic, Date created_on, String notes,
			Long user_group_id, Long language_id, String language_name) {
		super();
		this.id = id;
		this.author_id = author_id;
		this.author_name = author_name;
		this.profile_pic = profile_pic;
		this.created_on = created_on;
		this.notes = notes;
		this.user_group_id = user_group_id;
		this.language_id = language_id;
		this.language_name = language_name;
	}

	public String getProfile_pic() {
		return profile_pic;
	}

	public void setProfile_pic(String profile_pic) {
		this.profile_pic = profile_pic;
	}

	public Long getUser_group_id() {
		return user_group_id;
	}

	public void setUser_group_id(Long user_group_id) {
		this.user_group_id = user_group_id;
	}

	public Long getAuthor_id() {
		return author_id;
	}

	public void setAuthor_id(Long author_id) {
		this.author_id = author_id;
	}

	public String getAuthor_name() {
		return author_name;
	}

	public void setAuthor_name(String author_name) {
		this.author_name = author_name;
	}

	public Long getLanguage_id() {
		return language_id;
	}

	public void setLanguage_id(Long language_id) {
		this.language_id = language_id;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Date getCreated_on() {
		return created_on;
	}

	public void setCreated_on(Date created_on) {
		this.created_on = created_on;
	}

	public String getLanguage_name() {
		return language_name;
	}

	public void setLanguage_name(String language_name) {
		this.language_name = language_name;
	}

}

//===========tags===========
class Tags {
	private Long id;
	private String name;

	/**
	 * 
	 */
	public Tags() {
		super();
	}

	/**
	 * @param id
	 * @param name
	 */
	public Tags(Long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
// ============facts===============

class Trait_value {
	private Long fact_id;
	private Long contributor_id;
	private String from_value;
	private String to_value;
	private Date from_date;
	private Date to_date;
	private Long trait_value_id;
	private String description;
	private String icon;
	private String value;
	private String trait_aggregation;
	private String trait_filter;

	/**
	 * 
	 */
	public Trait_value() {
		super();
	}

	/**
	 * @param fact_id
	 * @param contributor_id
	 * @param from_value
	 * @param to_value
	 * @param from_date
	 * @param to_date
	 * @param trait_value_id
	 * @param description
	 * @param icon
	 * @param value
	 * @param trait_aggregation
	 * @param trait_filter
	 */
	public Trait_value(Long fact_id, Long contributor_id, String from_value, String to_value, Date from_date,
			Date to_date, Long trait_value_id, String description, String icon, String value, String trait_aggregation,
			String trait_filter) {
		super();
		this.fact_id = fact_id;
		this.contributor_id = contributor_id;
		this.from_value = from_value;
		this.to_value = to_value;
		this.from_date = from_date;
		this.to_date = to_date;
		this.trait_value_id = trait_value_id;
		this.description = description;
		this.icon = icon;
		this.value = value;
		this.trait_aggregation = trait_aggregation;
		this.trait_filter = trait_filter;
	}

	public Long getFact_id() {
		return fact_id;
	}

	public void setFact_id(Long fact_id) {
		this.fact_id = fact_id;
	}

	public Long getContributor_id() {
		return contributor_id;
	}

	public void setContributor_id(Long contributor_id) {
		this.contributor_id = contributor_id;
	}

	public String getFrom_value() {
		return from_value;
	}

	public void setFrom_value(String from_value) {
		this.from_value = from_value;
	}

	public String getTo_value() {
		return to_value;
	}

	public void setTo_value(String to_value) {
		this.to_value = to_value;
	}

	public Date getFrom_date() {
		return from_date;
	}

	public void setFrom_date(Date from_date) {
		this.from_date = from_date;
	}

	public Date getTo_date() {
		return to_date;
	}

	public void setTo_date(Date to_date) {
		this.to_date = to_date;
	}

	public Long getTrait_value_id() {
		return trait_value_id;
	}

	public void setTrait_value_id(Long trait_value_id) {
		this.trait_value_id = trait_value_id;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getTrait_aggregation() {
		return trait_aggregation;
	}

	public void setTrait_aggregation(String trait_aggregation) {
		this.trait_aggregation = trait_aggregation;
	}

	public String getTrait_filter() {
		return trait_filter;
	}

	public void setTrait_filter(String trait_filter) {
		this.trait_filter = trait_filter;
	}

}

class Facts {
	private Long trait_id;
	private String description;
	private Long field_id;
	private String trait_icon;
	private String name;
	private Boolean is_participatory;
	private String units;
	private String trait_types;
	private String data_types;
	private List<Trait_value> trait_value;

	/**
	 * 
	 */
	public Facts() {
		super();
	}

	/**
	 * @param trait_id
	 * @param description
	 * @param field_id
	 * @param trait_icon
	 * @param name
	 * @param is_participatory
	 * @param units
	 * @param trait_types
	 * @param data_types
	 * @param trait_value
	 */
	public Facts(Long trait_id, String description, Long field_id, String trait_icon, String name,
			Boolean is_participatory, String units, String trait_types, String data_types,
			List<Trait_value> trait_value) {
		super();
		this.trait_id = trait_id;
		this.description = description;
		this.field_id = field_id;
		this.trait_icon = trait_icon;
		this.name = name;
		this.is_participatory = is_participatory;
		this.units = units;
		this.trait_types = trait_types;
		this.data_types = data_types;
		this.trait_value = trait_value;
	}

	public Long getTrait_id() {
		return trait_id;
	}

	public void setTrait_id(Long trait_id) {
		this.trait_id = trait_id;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Long getField_id() {
		return field_id;
	}

	public void setField_id(Long field_id) {
		this.field_id = field_id;
	}

	public String getTrait_icon() {
		return trait_icon;
	}

	public void setTrait_icon(String trait_icon) {
		this.trait_icon = trait_icon;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Boolean getIs_participatory() {
		return is_participatory;
	}

	public void setIs_participatory(Boolean is_participatory) {
		this.is_participatory = is_participatory;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}

	public String getTrait_types() {
		return trait_types;
	}

	public void setTrait_types(String trait_types) {
		this.trait_types = trait_types;
	}

	public String getData_types() {
		return data_types;
	}

	public void setData_types(String data_types) {
		this.data_types = data_types;
	}

	public List<Trait_value> getTrait_value() {
		return trait_value;
	}

	public void setTrait_value(List<Trait_value> trait_value) {
		this.trait_value = trait_value;
	}

}

class Authors_voted {

	private Long id;
	private String name;
	private String profile_pic;
	private String confidence;
	private String comment;

	/**
	 * 
	 */
	public Authors_voted() {
		super();
	}

	/**
	 * @param id
	 * @param name
	 * @param profile_pic
	 * @param confidence
	 * @param comment
	 */
	public Authors_voted(Long id, String name, String profile_pic, String confidence, String comment) {
		super();
		this.id = id;
		this.name = name;
		this.profile_pic = profile_pic;
		this.confidence = confidence;
		this.comment = comment;
	}

	public String getProfile_pic() {
		return profile_pic;
	}

	public void setProfile_pic(String profile_pic) {
		this.profile_pic = profile_pic;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getConfidence() {
		return confidence;
	}

	public void setConfidence(String confidence) {
		this.confidence = confidence;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}

class Taxon_detail {

	private Long id;
	private String name;
	private String canonical_form;
	private String scientific_name;
	private String italicised_form;
	private Long rank;
	private String status;
	private String position;
	private String species_id;

	/**
	 * 
	 */
	public Taxon_detail() {
		super();
	}

	/**
	 * @param id
	 * @param name
	 * @param canonical_form
	 * @param scientific_name
	 * @param italicised_form
	 * @param rank
	 * @param status
	 * @param position
	 * @param species_id
	 */
	public Taxon_detail(Long id, String name, String canonical_form, String scientific_name, String italicised_form,
			Long rank, String status, String position, String species_id) {
		super();
		this.id = id;
		this.name = name;
		this.canonical_form = canonical_form;
		this.scientific_name = scientific_name;
		this.italicised_form = italicised_form;
		this.rank = rank;
		this.status = status;
		this.position = position;
		this.species_id = species_id;
	}

	public String getPosition() {
		return position;
	}

	public void setPosition(String position) {
		this.position = position;
	}

	public String getCanonical_form() {
		return canonical_form;
	}

	public void setCanonical_form(String canonical_form) {
		this.canonical_form = canonical_form;
	}

	public String getItalicised_form() {
		return italicised_form;
	}

	public void setItalicised_form(String italicised_form) {
		this.italicised_form = italicised_form;
	}

	public String getScientific_name() {
		return scientific_name;
	}

	public void setScientific_name(String scientific_name) {
		this.scientific_name = scientific_name;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getSpecies_id() {
		return species_id;
	}

	public void setSpecies_id(String species_id) {
		this.species_id = species_id;
	}

	public Long getRank() {
		return rank;
	}

	public void setRank(Long rank) {
		this.rank = rank;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}

//==========ScientificName===========
class Scientific_name {

	private Taxon_detail taxon_detail;
	private String name;
	private Long accepted_name_id;

	/**
	 * 
	 */
	public Scientific_name() {
		super();
	}

	/**
	 * @param taxon_detail
	 * @param name
	 * @param accepted_name_id
	 */
	public Scientific_name(Taxon_detail taxon_detail, String name, Long accepted_name_id) {
		super();
		this.taxon_detail = taxon_detail;
		this.name = name;
		this.accepted_name_id = accepted_name_id;
	}

	public Taxon_detail getTaxon_detail() {
		return taxon_detail;
	}

	public void setTaxon_detail(Taxon_detail taxon_detail) {
		this.taxon_detail = taxon_detail;
	}

	public Long getAccepted_name_id() {
		return accepted_name_id;
	}

	public void setAccepted_name_id(Long accepted_name_id) {
		this.accepted_name_id = accepted_name_id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}

//============All voted Reco================

class All_reco_vote {
	private Long recommendation_id;
	private List<Common_names> common_names;
	private Scientific_name scientific_name;
	private List<Authors_voted> authors_voted;
	private Date last_modified;

	/**
	 * 
	 */
	public All_reco_vote() {
		super();
	}

	/**
	 * @param recommendation_id
	 * @param common_names
	 * @param scientific_name
	 * @param authors_voted
	 * @param last_modified
	 */
	public All_reco_vote(Long recommendation_id, List<Common_names> common_names, Scientific_name scientific_name,
			List<Authors_voted> authors_voted, Date last_modified) {
		super();
		this.recommendation_id = recommendation_id;
		this.common_names = common_names;
		this.scientific_name = scientific_name;
		this.authors_voted = authors_voted;
		this.last_modified = last_modified;
	}

	public Long getRecommendation_id() {
		return recommendation_id;
	}

	public void setRecommendation_id(Long recommendation_id) {
		this.recommendation_id = recommendation_id;
	}

	public List<Common_names> getCommon_names() {
		return common_names;
	}

	public void setCommon_names(List<Common_names> common_names) {
		this.common_names = common_names;
	}

	public Scientific_name getScientific_name() {
		return scientific_name;
	}

	public void setScientific_name(Scientific_name scientific_name) {
		this.scientific_name = scientific_name;
	}

	public List<Authors_voted> getAuthors_voted() {
		return authors_voted;
	}

	public void setAuthors_voted(List<Authors_voted> authors_voted) {
		this.authors_voted = authors_voted;
	}

	public Date getLast_modified() {
		return last_modified;
	}

	public void setLast_modified(Date last_modified) {
		this.last_modified = last_modified;
	}

}

//=============max_voted_reco===========

class Max_voted_reco {

	private Long id;
	private List<Common_names> common_names;
	private List<Hierarchy> hierarchy;
	private String scientific_name;
	private Long rank;
	private String ranktext;
	private String taxonstatus;

	/**
	 * 
	 */
	public Max_voted_reco() {
		super();
	}

	/**
	 * @param id
	 * @param common_names
	 * @param hierarchy
	 * @param scientific_name
	 * @param rank
	 * @param ranktext
	 * @param taxonstatus
	 */
	public Max_voted_reco(Long id, List<Common_names> common_names, List<Hierarchy> hierarchy, String scientific_name,
			Long rank, String ranktext, String taxonstatus) {
		super();
		this.id = id;
		this.common_names = common_names;
		this.hierarchy = hierarchy;
		this.scientific_name = scientific_name;
		this.rank = rank;
		this.ranktext = ranktext;
		this.taxonstatus = taxonstatus;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getTaxonstatus() {
		return taxonstatus;
	}

	public void setTaxonstatus(String taxonstatus) {
		this.taxonstatus = taxonstatus;
	}

	public String getScientific_name() {
		return scientific_name;
	}

	public void setScientific_name(String scientific_name) {
		this.scientific_name = scientific_name;
	}

	public String getRanktext() {
		return ranktext;
	}

	public void setRanktext(String ranktext) {
		this.ranktext = ranktext;
	}

	public List<Common_names> getCommon_names() {
		return common_names;
	}

	public void setCommon_names(List<Common_names> common_names) {
		this.common_names = common_names;
	}

	public List<Hierarchy> getHierarchy() {
		return hierarchy;
	}

	public void setHierarchy(List<Hierarchy> hierarchy) {
		this.hierarchy = hierarchy;
	}

	public Long getRank() {
		return rank;
	}

	public void setRank(Long rank) {
		this.rank = rank;
	}

}

//========Common Name===========
class Common_names {

	private String common_name;
	private String language_id;
	private String language_name;

	/**
	 * 
	 */
	public Common_names() {
		super();
	}

	/**
	 * @param common_name
	 * @param language_id
	 * @param language_name
	 */
	public Common_names(String common_name, String language_id, String language_name) {
		super();
		this.common_name = common_name;
		this.language_id = language_id;
		this.language_name = language_name;
	}

	public String getCommon_name() {
		return common_name;
	}

	public void setCommon_name(String common_name) {
		this.common_name = common_name;
	}

	public String getLanguage_id() {
		return language_id;
	}

	public void setLanguage_id(String language_id) {
		this.language_id = language_id;
	}

	public String getLanguage_name() {
		return language_name;
	}

	public void setLanguage_name(String language_name) {
		this.language_name = language_name;
	}

}

//==========Hierarchy=========
class Hierarchy {

	private Long taxon_id;
	private String normalized_name;
	private Long rank;

	/**
	 * 
	 */
	public Hierarchy() {
		super();
	}

	/**
	 * @param taxon_id
	 * @param normalized_name
	 * @param rank
	 */
	public Hierarchy(Long taxon_id, String normalized_name, Long rank) {
		super();
		this.taxon_id = taxon_id;
		this.normalized_name = normalized_name;
		this.rank = rank;
	}

	public String getNormalized_name() {
		return normalized_name;
	}

	public void setNormalized_name(String normalized_name) {
		this.normalized_name = normalized_name;
	}

	public Long getTaxon_id() {
		return taxon_id;
	}

	public void setTaxon_id(Long taxon_id) {
		this.taxon_id = taxon_id;
	}

	public Long getRank() {
		return rank;
	}

	public void setRank(Long rank) {
		this.rank = rank;
	}
}

class User_group_observations {

	private Long id;
	private String icon;
	private String name;
	private String ug_filter;
	private String webaddress;

	/**
	 * 
	 */
	public User_group_observations() {
		super();
	}

	/**
	 * @param id
	 * @param icon
	 * @param name
	 * @param ug_filter
	 * @param webaddress
	 */
	public User_group_observations(Long id, String icon, String name, String ug_filter, String webaddress) {
		super();
		this.id = id;
		this.icon = icon;
		this.name = name;
		this.ug_filter = ug_filter;
		this.webaddress = webaddress;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getUg_filter() {
		return ug_filter;
	}

	public void setUg_filter(String ug_filter) {
		this.ug_filter = ug_filter;
	}

	public String getWebaddress() {
		return webaddress;
	}

	public void setWebaddress(String webaddress) {
		this.webaddress = webaddress;
	}

}

class Custom_field_values {

	private String field_text_data;
	private String single_categorical_data;
	private List<String> multiple_categorical_data;
	private String min_range;
	private String max_range;
	private List<String> custom_field_aggregation;
	private List<String> custom_field_filter;

	/**
	 * 
	 */
	public Custom_field_values() {
		super();
	}

	/**
	 * @param field_text_data
	 * @param single_categorical_data
	 * @param multiple_categorical_data
	 * @param min_range
	 * @param max_range
	 * @param custom_field_aggregation
	 * @param custom_field_filter
	 */
	public Custom_field_values(String field_text_data, String single_categorical_data,
			List<String> multiple_categorical_data, String min_range, String max_range,
			List<String> custom_field_aggregation, List<String> custom_field_filter) {
		super();
		this.field_text_data = field_text_data;
		this.single_categorical_data = single_categorical_data;
		this.multiple_categorical_data = multiple_categorical_data;
		this.min_range = min_range;
		this.max_range = max_range;
		this.custom_field_aggregation = custom_field_aggregation;
		this.custom_field_filter = custom_field_filter;
	}

	public String getField_text_data() {
		return field_text_data;
	}

	public void setField_text_data(String field_text_data) {
		this.field_text_data = field_text_data;
	}

	public String getSingle_categorical_data() {
		return single_categorical_data;
	}

	public void setSingle_categorical_data(String single_categorical_data) {
		this.single_categorical_data = single_categorical_data;
	}

	public List<String> getMultiple_categorical_data() {
		return multiple_categorical_data;
	}

	public void setMultiple_categorical_data(List<String> multiple_categorical_data) {
		this.multiple_categorical_data = multiple_categorical_data;
	}

	public String getMin_range() {
		return min_range;
	}

	public void setMin_range(String min_range) {
		this.min_range = min_range;
	}

	public String getMax_range() {
		return max_range;
	}

	public void setMax_range(String max_range) {
		this.max_range = max_range;
	}

	public List<String> getCustom_field_aggregation() {
		return custom_field_aggregation;
	}

	public void setCustom_field_aggregation(List<String> custom_field_aggregation) {
		this.custom_field_aggregation = custom_field_aggregation;
	}

	public List<String> getCustom_field_filter() {
		return custom_field_filter;
	}

	public void setCustom_field_filter(List<String> custom_field_filter) {
		this.custom_field_filter = custom_field_filter;
	}

}

class Custom_field {

	private Long custom_field_id;
	private Boolean allowed_participation;
	private String default_value;
	private Long display_order;
	private Long cf_author_id;
	private String data_type;
	private String field_type;
	private String cf_icon_url;
	private String cf_name;
	private String cf_notes;
	private String units;
	private Custom_field_values custom_field_values;

	/**
	 * 
	 */
	public Custom_field() {
		super();
	}

	/**
	 * @param custom_field_id
	 * @param allowed_participation
	 * @param default_value
	 * @param display_order
	 * @param cf_author_id
	 * @param data_type
	 * @param field_type
	 * @param cf_icon_url
	 * @param cf_name
	 * @param cf_notes
	 * @param units
	 * @param custom_field_values
	 */
	public Custom_field(Long custom_field_id, Boolean allowed_participation, String default_value, Long display_order,
			Long cf_author_id, String data_type, String field_type, String cf_icon_url, String cf_name, String cf_notes,
			String units, Custom_field_values custom_field_values) {
		super();
		this.custom_field_id = custom_field_id;
		this.allowed_participation = allowed_participation;
		this.default_value = default_value;
		this.display_order = display_order;
		this.cf_author_id = cf_author_id;
		this.data_type = data_type;
		this.field_type = field_type;
		this.cf_icon_url = cf_icon_url;
		this.cf_name = cf_name;
		this.cf_notes = cf_notes;
		this.units = units;
		this.custom_field_values = custom_field_values;
	}

	public String getCf_notes() {
		return cf_notes;
	}

	public void setCf_notes(String cf_notes) {
		this.cf_notes = cf_notes;
	}

	public Boolean getAllowed_participation() {
		return allowed_participation;
	}

	public void setAllowed_participation(Boolean allowed_participation) {
		this.allowed_participation = allowed_participation;
	}

	public String getDefault_value() {
		return default_value;
	}

	public void setDefault_value(String default_value) {
		this.default_value = default_value;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}

	public String getCf_name() {
		return cf_name;
	}

	public void setCf_name(String cf_name) {
		this.cf_name = cf_name;
	}

	public String getField_type() {
		return field_type;
	}

	public void setField_type(String field_type) {
		this.field_type = field_type;
	}

	public String getData_type() {
		return data_type;
	}

	public void setData_type(String data_type) {
		this.data_type = data_type;
	}

	public Long getCf_author_id() {
		return cf_author_id;
	}

	public void setCf_author_id(Long cf_author_id) {
		this.cf_author_id = cf_author_id;
	}

	public Custom_field_values getCustom_field_values() {
		return custom_field_values;
	}

	public void setCustom_field_values(Custom_field_values custom_field_values) {
		this.custom_field_values = custom_field_values;
	}

	public Long getDisplay_order() {
		return display_order;
	}

	public void setDisplay_order(Long display_order) {
		this.display_order = display_order;
	}

	public Long getCustom_field_id() {
		return custom_field_id;
	}

	public void setCustom_field_id(Long custom_field_id) {
		this.custom_field_id = custom_field_id;
	}

	public String getCf_icon_url() {
		return cf_icon_url;
	}

	public void setCf_icon_url(String cf_icon_url) {
		this.cf_icon_url = cf_icon_url;
	}

}

class Custom_fields {

	private Long user_group_id;
	private List<Custom_field> custom_field;

	/**
	 * 
	 */
	public Custom_fields() {
		super();
	}

	/**
	 * @param user_group_id
	 * @param custom_field
	 */
	public Custom_fields(Long user_group_id, List<Custom_field> custom_field) {
		super();
		this.user_group_id = user_group_id;
		this.custom_field = custom_field;
	}

	public Long getUser_group_id() {
		return user_group_id;
	}

	public void setUser_group_id(Long user_group_id) {
		this.user_group_id = user_group_id;
	}

	public void setCustom_field(List<Custom_field> custom_field) {
		this.custom_field = custom_field;
	}

	public List<Custom_field> getCustom_field() {
		return this.custom_field;
	}
}

class Flags {
	private Long id;
	private String notes;
	private Long author_id;
	private String author_name;
	private String profile_pic;
	private Date created_on;
	private String flag;

	/**
	 * 
	 */
	public Flags() {
		super();
	}

	/**
	 * @param id
	 * @param notes
	 * @param author_id
	 * @param author_name
	 * @param profile_pic
	 * @param created_on
	 * @param flag
	 */
	public Flags(Long id, String notes, Long author_id, String author_name, String profile_pic, Date created_on,
			String flag) {
		super();
		this.id = id;
		this.notes = notes;
		this.author_id = author_id;
		this.author_name = author_name;
		this.profile_pic = profile_pic;
		this.created_on = created_on;
		this.flag = flag;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public Long getAuthor_id() {
		return author_id;
	}

	public void setAuthor_id(Long author_id) {
		this.author_id = author_id;
	}

	public String getAuthor_name() {
		return author_name;
	}

	public void setAuthor_name(String author_name) {
		this.author_name = author_name;
	}

	public String getProfile_pic() {
		return profile_pic;
	}

	public void setProfile_pic(String profile_pic) {
		this.profile_pic = profile_pic;
	}

	public Date getCreated_on() {
		return created_on;
	}

	public void setCreated_on(Date created_on) {
		this.created_on = created_on;
	}

	public String getFlag() {
		return flag;
	}

	public void setFlag(String flag) {
		this.flag = flag;
	}

}

class LocationInformation {
	private String tahsil;
	private String state;
	private String district;

	/**
	 * 
	 */
	public LocationInformation() {
		super();
	}

	/**
	 * @param tahsil
	 * @param state
	 * @param district
	 */
	public LocationInformation(String tahsil, String state, String district) {
		super();
		this.tahsil = tahsil;
		this.state = state;
		this.district = district;
	}

	public String getTahsil() {
		return tahsil;
	}

	public void setTahsil(String tahsil) {
		this.tahsil = tahsil;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getDistrict() {
		return district;
	}

	public void setDistrict(String district) {
		this.district = district;
	}

}
