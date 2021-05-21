package com.strandls.observation.es.util;

import java.util.Date;
import java.util.List;

import com.strandls.naksha.pojo.ObservationLocationInfo;

public class ExternalObservationESDocument extends ObservationESDocument {
	private boolean is_external;
	private String data_source;
	private String external_original_reference_link;
	private String external_gbif_reference_link;
	private ObservationLocationInfo layer_info;
	private String unique_id_prefix;

	public ExternalObservationESDocument() {
		super();
	}

	public ExternalObservationESDocument(Long observation_id, Long author_id, String created_by, String profile_pic,
			Date created_on, Long group_id, String group_name, String sgroup_filter, Location location,
			LocationInformation location_information, Integer reco_vote_count, String notes, Date from_date,
			String observed_in_month, String place_name, String reverse_geocoded_name, Long flag_count,
			Boolean geo_privacy, Date last_revised, Long visit_count, Boolean is_checklist, Date to_date,
			Boolean is_locked, Integer language_id, String location_scale, Long dataset_id, String dataset_title,
			Long repr_image_id, String repr_image_url, String protocol, Integer no_of_images, Integer no_of_videos,
			Integer no_of_audio, Integer no_media, Integer no_of_identifications, Long data_table_id,
			String date_accuracy, Max_voted_reco max_voted_reco, List<All_reco_vote> all_reco_vote,
			List<Observation_resource> observation_resource, List<Custom_fields> custom_fields,
			List<User_group_observations> user_group_observations, List<Tags> tags, List<Flags> flags,
			List<Featured> featured, List<Facts> facts, Boolean is_publication_grade) {
		super(observation_id, author_id, created_by, profile_pic, created_on, group_id, group_name, sgroup_filter,
				location, location_information, reco_vote_count, notes, from_date, observed_in_month, place_name,
				reverse_geocoded_name, flag_count, geo_privacy, last_revised, visit_count, is_checklist, to_date,
				is_locked, language_id, location_scale, dataset_id, dataset_title, repr_image_id, repr_image_url,
				protocol, no_of_images, no_of_videos, no_of_audio, no_media, no_of_identifications, data_table_id,
				date_accuracy, max_voted_reco, all_reco_vote, observation_resource, custom_fields,
				user_group_observations, tags, flags, featured, facts, is_publication_grade);
	}

	public ExternalObservationESDocument(boolean is_external, String data_source,
			String external_original_reference_link, String external_gbif_reference_link,
			ObservationLocationInfo layer_info, String unique_id_prefix) {
		super();
		this.is_external = is_external;
		this.data_source = data_source;
		this.external_original_reference_link = external_original_reference_link;
		this.external_gbif_reference_link = external_gbif_reference_link;
		this.layer_info = layer_info;
		this.unique_id_prefix = unique_id_prefix;
	}

	public boolean isIs_external() {
		return is_external;
	}

	public void setIs_external(boolean is_external) {
		this.is_external = is_external;
	}

	public String getData_source() {
		return data_source;
	}

	public void setData_source(String data_source) {
		this.data_source = data_source;
	}

	public String getExternal_original_reference_link() {
		return external_original_reference_link;
	}

	public void setExternal_original_reference_link(String external_original_reference_link) {
		this.external_original_reference_link = external_original_reference_link;
	}

	public String getExternal_gbif_reference_link() {
		return external_gbif_reference_link;
	}

	public void setExternal_gbif_reference_link(String external_gbif_reference_link) {
		this.external_gbif_reference_link = external_gbif_reference_link;
	}

	public ObservationLocationInfo getLayer_info() {
		return layer_info;
	}

	public void setLayer_info(ObservationLocationInfo layer_info) {
		this.layer_info = layer_info;
	}

	public String getUnique_id_prefix() {
		return unique_id_prefix;
	}

	public void setUnique_id_prefix(String unique_id_prefix) {
		this.unique_id_prefix = unique_id_prefix;
	}

}
