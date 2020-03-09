/**
 * 
 */
package com.strandls.observation.es.util;

import java.util.List;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;

import com.google.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class ConstructESDocument {

	@Inject
	private SessionFactory sessionFactory;

	public ObservationESDocument getESDocumentStub(String observationId) {

		String query = "SELECT id observation_id, author_id, created_by, profile_pic, created_on, group_id, group_name, "
				+ "row_to_json((SELECT t FROM (SELECT latitude AS lat, longitude as lon)t))\\:\\:text AS location,"
				+ " notes, from_date, " + " TRIM(to_char(from_date,'Month')) observed_in_month,"
				+ " place_name, reverse_geocoded_name, flag_count, geo_privacy, last_revised, "
				+ " visit_count, is_checklist, to_date, " + " is_locked, language_id, location_scale, "
				+ " dataset_id, dataset_title, repr_image_id, protocol, no_of_images, no_of_videos, no_of_audio,"
				+ " CASE " + " 	WHEN no_of_images = 0 AND no_of_videos = 0 AND no_of_audio = 0 THEN 1" + " 	ELSE 0"
				+ " END AS no_media," + " no_of_identifications, data_table_id, date_accuracy,"
				+ " row_to_json((SELECT t FROM (SELECT max_voted_reco_id id,common_names, hierarchy, scientific_name, (taxon_detail->'rank')\\:\\:text\\:\\:integer AS rank,"
				+ " case " + " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 0 then 'Kingdom'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 1 then 'Phylum'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 2 then 'Class'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 3 then 'Order'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 4 then 'Superfamily'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 5 then 'Family'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 6 then 'Subfamily'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 7 then 'Genus'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 8 then 'Subgenus'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 9 then 'Species'"
				+ " 	when (taxon_detail->'rank')\\:\\:text\\:\\:integer = 10 then 'Infraspecies'" + " 	else null"
				+ "  end as ranktext,  (taxon_detail->'status') AS taxonstatus WHERE common_names is not null OR scientific_name is not null)t))\\:\\:text AS max_voted_reco, "
				+ " all_reco_vote\\:\\:text AS all_reco_vote, observation_resource\\:\\:text AS observation_resource,"
				+ " custom_fields\\:\\:json custom_fields, "
				+ " user_group_observations\\:\\:text user_group_observations, (tags)\\:\\:text AS tags, (flags\\:\\:text) flags, (featured\\:\\:text) AS featured, "
				+ " (facts)\\:\\:text facts" + " FROM" + "  "
				+ " (SELECT id, author_id, created_on, group_id, latitude, longitude, "
				+ " TRIM(regexp_replace(replace(strip_tags(notes),'&nbsp;',' '),'s+', ' ', 'g')) notes, "
				+ " from_date, " + " place_name, reverse_geocoded_name, flag_count, geo_privacy, last_revised, "
				+ " visit_count, max_voted_reco_id, is_checklist, to_date, " + " "
				+ " regexp_replace(regexp_replace((regexp_replace(checklist_annotations,'\"\":','\"unknown\":','g')),'\"S. No\":|\"S.No.\":|\"si. no.\":','\"s_no\":','g'),'\"no. specimens\":','\"no_specimens\":','g')\\:\\:jsonb checklist_annotations,"
				+ " is_locked, language_id, location_scale, "
				+ " dataset_id, repr_image_id, protocol, no_of_images, no_of_videos, no_of_audio, "
				+ " no_of_identifications, data_table_id, date_accuracy FROM observation where is_deleted = false AND id = "
				+ observationId + " ) O" + " LEFT OUTER JOIN"
				+ " (SELECT id d_id, title as dataset_title FROM dataset) D ON O.dataset_id = D.d_id"
				+ " LEFT OUTER JOIN "
				+ " (SELECT id s_id, name created_by, profile_pic FROM suser ) U ON U.s_id = O.author_id"
				+ " LEFT OUTER JOIN" + " (SELECT id s_id, name group_name from species_group )S ON S.s_id = O.group_id"
				+ " " + " LEFT OUTER JOIN" + " (" + " SELECT " + " observation_id, "
				+ " (reco_vote->>'recommendation_id')\\:\\:bigint AS recommendation_id," + " CASE"
				+ " 	WHEN  reco_vote-> 'common_names' != 'null' THEN reco_vote-> 'common_names'" + " 	ELSE null"
				+ " END AS common_names," + " 	" + " (reco_vote->> 'scientific_name')\\:\\:text as scientific_name,"
				+ " CASE" + " 	WHEN  reco_vote->'hierarchy' != 'null' THEN reco_vote-> 'hierarchy'" + " 	ELSE null"
				+ " END AS hierarchy," + " CASE"
				+ " 	WHEN reco_vote->'taxon_detail' !='null' THEN reco_vote->'taxon_detail'->0" + " 	ELSE NULL"
				+ " END AS taxon_detail, " + " all_reco_vote " + " FROM" + " " + " (SELECT " + " observation_id,"
				+ " jsonb_array_elements(maybe_max_voted) reco_vote," + " all_reco_vote" + " FROM" + " 	(SELECT "
				+ " 	observation_id,"
				+ " 	(jsonb_agg(DISTINCT reco_vote) FILTER (WHERE reco_vote is not null))\\:\\:json all_reco_vote,"
				+ " 	jsonb_agg(DISTINCT recos) FILTER (WHERE recos is not null) maybe_max_voted" + " 	FROM" + " "
				+ "  (" + "  SELECT  observation_id, "
				+ " 	row_to_json((SELECT t FROM (SELECT recommendation_id, common_names, scientific_name, authors_voted,"
				+ " 	last_modified, "
				+ " 	row_to_json((SELECT t FROM (SELECT taxon_detail->0 AS taxon_detail, NULLIF(scientific_name,'') AS name, accepted_name_id)t)) scientific_name) t))\\:\\:jsonb "
				+ " 	reco_vote," + " "
				+ " 	row_to_json(( SELECT t FROM (SELECT recommendation_id, common_names, scientific_name, hierarchy, taxon_detail) "
				+ " 	t))\\:\\:jsonb recos" + "  FROM" + " 	(" + " 	SELECT "
				+ " 	observation_id, recommendation_id,"
				+ " 	array_agg(DISTINCT common_name_reco_id) filter (where common_name_reco_id is not null) common_name_reco_ids,"
				+ " 		MIN(Distinct taxon_concept_id) taxon_id, "
				+ " 		to_jsonb(array_agg(Distinct common_name) filter (where common_name is not null)) common_names,"
				+ " 		MAX(scientific_name) scientific_name,"
				+ " 		MAX(is_scientific_name\\:\\:int)\\:\\:boolean is_scientific_name,"
				+ " 		jsonb_agg(DISTINCT authors_voted)authors_voted,"
				+ " 		MAX(last_modified) last_modified,"
				+ " 		jsonb_agg(DISTINCT taxon_detail) filter (where taxon_detail is not null)taxon_detail,"
				+ " 		jsonb_agg(hierarchy) filter (WHERE hierarchy IS NOT NULL)\\:\\:jsonb->0 hierarchy,"
				+ " 		MAX(path) path," + " 		MAX(accepted_name_id)accepted_name_id		" + " 	FROM "
				+ " 	(" + " 	SELECT" + " 	observation_id, recommendation_id, common_name_reco_id, "
				+ " 	row_to_json((SELECT t FROM (SELECT common_name, language_id, language_name WHERE common_name is not null) t))\\:\\:jsonb "
				+ " 	AS common_name, " + " 	CASE "
				+ " 		WHEN is_scientific_name= true and taxon_concept_id is null THEN recommendation_name"
				+ " 		ELSE normalized_form" + " 		END AS scientific_name,"
				+ " 		last_modified, is_scientific_name, taxon_concept_id, recommendation_name," + " 	CASE "
				+ " 		WHEN voted_by_author_id is not null THEN"
				+ " 		to_jsonb(row_to_json((SELECT t FROM (SELECT voted_by_author_id id, voted_by AS name, "
				+ " 		profile_pic, confidence, comment)t)))" + " 		ELSE NULL END AS authors_voted,"
				+ " 	language_id, language_name, accepted_name_id, hierarchy, path," + " 	CASE "
				+ " 		WHEN t_id is not null THEN"
				+ " 		to_jsonb(row_to_json((SELECT t FROM (SELECT taxon_concept_id id,  name, canonical_form, "
				+ " 		normalized_form AS scientific_name, italicised_form, rank, status,"
				+ " 		position, species_id)t)))" + " 		ELSE NULL" + " 	END AS taxon_detail" + " 		"
				+ " 	FROM " + " 		( " + " 		SELECT "
				+ " 		observation_id, id reco_vote_id, author_id voted_by_author_id, voted_by , confidence, recommendation_id,"
				+ " 		common_name_reco_id, voted_on, comment, taxon_concept_id, last_modified, profile_pic, recommendation_name,"
				+ " 		CASE "
				+ " 			WHEN is_scientific_name =  false OR recommendation_id = common_name_reco_id THEN recommendation_name"
				+ " 			ELSE NULL"
				+ " 			END AS common_name,language_id, is_scientific_name, accepted_name_id" + " 		FROM "
				+ " 		(SELECT id, observation_id, author_id , confidence, recommendation_id, common_name_reco_id, voted_on, comment "
				+ " 		FROM recommendation_vote) extended_rv" + " 		INNER JOIN	"
				+ " 		(SELECT id rid, taxon_concept_id, name AS recommendation_name, is_scientific_name, language_id, "
				+ " 		accepted_name_id, last_modified FROM recommendation) extended_reco"
				+ " 		ON rid =  recommendation_id" + " 		LEFT OUTER JOIN"
				+ " 		(SELECT id a_id, name voted_by, profile_pic from suser ) A ON a_id = author_id"
				+ " 		" + " 		UNION" + " 		" + " 		SELECT "
				+ " 		observation_id, id reco_vote_id, author_id voted_by_author_id, voted_by, confidence, recommendation_id, "
				+ " 		common_name_reco_id, voted_on, comment, taxon_concept_id, last_modified, profile_pic, recommendation_name,"
				+ " 		CASE "
				+ " 			WHEN is_scientific_name = false or recommendation_id = common_name_reco_id THEN recommendation_name"
				+ " 			ELSE NULL"
				+ " 			END AS common_name, language_id, is_scientific_name, accepted_name_id" + " 		FROM "
				+ " 		(SELECT "
				+ " 		id, observation_id, author_id, confidence, recommendation_id, common_name_reco_id, voted_on, comment "
				+ " 		FROM recommendation_vote) extended_rv" + " 		INNER JOIN	"
				+ " 		(SELECT id rid, taxon_concept_id, name AS recommendation_name, is_scientific_name, language_id, "
				+ " 		accepted_name_id, last_modified FROM recommendation) extended_reco"
				+ " 		ON rid =  common_name_reco_id" + " 		LEFT OUTER JOIN"
				+ " 		(SELECT id a_id, name voted_by, profile_pic FROM suser ) A ON a_id = author_id "
				+ " 		) group_observation_reco" + " 	LEFT OUTER JOIN " + " 	(SELECT "
				+ " 		t_id, path,hierarchy, name, canonical_form, normalized_form, italicised_form, rank, status,"
				+ " 		position, species_id   " + " 		FROM"
				+ " 		( SELECT id t_id, canonical_form, normalized_form, italicised_form, name, rank, status, position, species_id "
				+ " 		FROM " + " 		taxonomy_definition where is_deleted = false ) T" + " 		LEFT OUTER JOIN"
				+ " 		( SELECT id tr_id, taxon_definition_id, path from taxonomy_registry where classification_id = 265799 ) TR"
				+ " 		ON T.t_id = TR.taxon_definition_id" + " 		LEFT OUTER JOIN "
				+ " 		pull_path P ON P.tr_id = TR.tr_id" + " 		) Taxons" + " 	ON taxon_concept_id = t_id "
				+ " 	LEFT OUTER JOIN"
				+ " 	(SELECT id l_id, name language_name FROM language) Lang ON l_id = language_id"
				+ " 	) R WHERE observation_id = " + observationId + "   "
				+ " 	GROUP BY observation_id, recommendation_id "
				+ " 	) O) OG GROUP BY observation_id) OG_unnest ) OG) ORV"
				+ " 	ON ORV.observation_id = O.id AND ORV.recommendation_id = O.max_voted_reco_id" + " "
				+ " LEFT OUTER JOIN" + " 		(" + " 		SELECT  observation_id , " + " 		jsonb_agg( DISTINCT "
				+ " 		to_jsonb((row_to_json((SELECT t FROM (SELECT resource_id id, description , file_name , type ,url, rating , upload_time , uploader_id, license_id) t )) "
				+ " 		)))\\:\\:json observation_resource" + " 		FROM "
				+ " 		(SELECT resource_id or_resource_id, observation_id FROM observation_resource WHERE observation_id = "
				+ observationId + " ) EO" + " 		INNER JOIN"
				+ " 		(SELECT id resource_id, description , file_name , type ,url, rating , upload_time , uploader_id, license_id "
				+ " 		FROM resource) extended_resource"
				+ " 		ON or_resource_id = resource_id GROUP BY observation_id"
				+ " 		) obr ON obr.observation_id = O.id " + " 		" + " " + " LEFT OUTER JOIN"
				+ " 		(SELECT UGO.observation_id observation_id, custom_fields, user_group_observations"
				+ " 		FROM " + " 		(SELECT ugo_observation_id observation_id, "
				+ " 		jsonb_agg(DISTINCT to_jsonb(row_to_json((SELECT t FROM "
				+ " 			(SELECT ugo_user_group_id user_group_id, custom_field)t )))) custom_fields" + " "
				+ " 		FROM" + " 		("
				+ " 		SELECT ugo_observation_id, ugo_user_group_id, (jsonb_agg(DISTINCT custom_field) "
				+ " 		FILTER (WHERE custom_field->>0 is not null))\\:\\:json->0 custom_field" + " 		FROM"
				+ " 		(SELECT ugo_observation_id, ugo_user_group_id," + " 		jsonb_agg("
				+ " 		DISTINCT to_jsonb(row_to_json"
				+ " 		((SELECT t FROM (SELECT u_custom_field_id custom_field_id, u_allowed_participation allowed_participation, "
				+ " 		u_deafult_value default_value, "
				+ " 		u_display_order display_order, cf_author_id cf_author_id, cf_data_type data_type, "
				+ " 		cf_field_type field_type, cf_icon_url cf_icon_url, cf_name, cf_notes, cf_units units,"
				+ " 		row_to_json((SELECT f FROM (SELECT field_text_data, single_categorical_data, multiple_categorical_data, min_range, max_range)f"
				+ " 		)) custom_field_values WHERE u_custom_field_id is not null)t))"
				+ " 		)) custom_field " + " " + " 		FROM"
				+ " 		(SELECT ugo_observation_id, ugo_user_group_id, u_custom_field_id, u_allowed_participation, u_deafult_value, "
				+ " 		u_display_order, cf_author_id, cf_data_type, cf_field_type, cf_icon_url, cf_name, cf_notes, cf_units,"
				+ " 		max(field_text_data) field_text_data, max(single_categorical_data) single_categorical_data,"
				+ " 		array_agg(DISTINCT multiple_categorical_data) FILTER (WHERE multiple_categorical_data is not null) multiple_categorical_data,"
				+ " 		max(min_range)min_range ,  max(max_range)max_range" + " 		 " + " 		FROM" + " "
				+ " 		(SELECT ugo_observation_id, o_observation_id, ugo_user_group_id, u_user_group_id, u_custom_field_id, "
				+ " 		o_custom_field_id, u_author_id, u_allowed_participation, u_deafult_value, u_display_order, u_is_mandatory, "
				+ " 		cf_author_id, cf_data_type, cf_field_type, cf_icon_url, cf_name, cf_notes, cf_units, o_custom_field_value_id, "
				+ " 		o_created_on, o_last_modified, o_custom_field_id, o_user_group_id, "
				+ " 		o_value_date, o_value_numeric, o_value_string, cv_author_id, cv_icon_url, cv_notes, cv_value,"
				+ " 		CASE" + " 			WHEN cf_field_type != 'FIELD TEXT' THEN o_value_string"
				+ " 			ELSE cv_value " + " 		END AS field_text_data, " + " " + " 		CASE "
				+ " 			WHEN cf_field_type = 'SINGLE CATEGORICAL' THEN cv_value" + " 			ELSE NULL"
				+ " 		END AS single_categorical_data," + " 		CASE "
				+ " 			WHEN cf_field_type = 'MULTIPLE CATEGORICAL' THEN cv_value" + " 			ELSE NULL"
				+ " 		END AS multiple_categorical_data," + " " + " 		CASE "
				+ " 			WHEN cf_field_type = 'RANGE' THEN" + " 				CASE "
				+ " 					WHEN cf_data_type != 'DATE' THEN" + " 						CASE "
				+ " 							WHEN cv_value = 'min' THEN o_value_numeric\\:\\:varchar"
				+ " 							ELSE NULL" + " " + " 						END"
				+ " 					WHEN cf_data_type = 'DATE' THEN " + " 						CASE "
				+ " 							WHEN cv_value = 'min' THEN o_value_date\\:\\:varchar"
				+ " 							ELSE NULL" + " " + " 						END" + " 				END"
				+ " 		END AS min_range," + " " + " 		CASE " + " 			WHEN cf_field_type = 'RANGE' THEN"
				+ " 				CASE " + " 					WHEN cf_data_type != 'DATE' THEN"
				+ " 						CASE "
				+ " 							WHEN cv_value = 'max' THEN o_value_numeric\\:\\:varchar"
				+ " 							ELSE NULL" + " " + " 						END"
				+ " 					WHEN cf_data_type = 'DATE' THEN " + " 						CASE "
				+ " 							WHEN cv_value = 'max' THEN o_value_date\\:\\:varchar"
				+ " 							ELSE NULL" + " " + " 						END" + " 				END"
				+ " 		END AS max_range" + " " + " 		FROM " + " 		("
				+ " 			(SELECT observation_id ugo_observation_id, user_group_id ugo_user_group_id FROM user_group_observations "
				+ " 			WHERE observation_id = " + observationId + " ) UGO" + " 			LEFT OUTER JOIN"
				+ " 			(SELECT u_custom_field_id, cf_author_id, cf_data_type, cf_field_type, cf_icon_url, cf_name, "
				+ " 			cf_notes, cf_units, u_allowed_participation, u_author_id, u_deafult_value, u_display_order, "
				+ " 			u_is_mandatory, u_user_group_id " + " 			FROM "
				+ " 				(SELECT allowed_participation u_allowed_participation, author_id u_author_id, "
				+ " 				custom_field_id u_custom_field_id, deafult_value u_deafult_value, "
				+ " 				display_order u_display_order, is_mandatory u_is_mandatory, "
				+ " 				user_group_id u_user_group_id" + " 				FROM "
				+ " 				user_group_cf_mapping)U " + " 				LEFT OUTER JOIN"
				+ " 				(SELECT id, author_id cf_author_id, data_type cf_data_type, field_type cf_field_type, icon_url cf_icon_url, "
				+ " 				name cf_name, notes cf_notes, units cf_units " + " 				FROM "
				+ " 				custom_fields) C " + " 				ON C.id = u_custom_field_id"
				+ " 			) UGM ON u_user_group_id = ugo_user_group_id " + " 			LEFT OUTER JOIN"
				+ " 			(SELECT o_observation_id, o_custom_field_value_id, o_created_on, o_last_modified, o_custom_field_id, "
				+ " 			o_user_group_id, o_value_date, o_value_numeric, o_value_string, cv_author_id, cv_icon_url, cv_notes, cv_value  "
				+ " 			FROM "
				+ " 				(SELECT author_id o_author_id, created_on o_created_on, custom_field_id o_custom_field_id, "
				+ " 				custom_field_value_id o_custom_field_value_id, last_modified o_last_modified, "
				+ " 				observation_id o_observation_id, user_group_id o_user_group_id, value_date o_value_date,"
				+ " 				value_numeric o_value_numeric, value_string o_value_string"
				+ " 				FROM " + " 				observation_custom_field WHERE observation_id = "
				+ observationId + "  )O " + " 				LEFT OUTER JOIN"
				+ " 				(SELECT id, author_id cv_author_id, custom_field_id cv_id, icon_url cv_icon_url, notes cv_notes, "
				+ " 				value AS cv_value" + " 				FROM "
				+ " 				custom_field_values) CV ON CV.id = O.o_custom_field_value_id" + " "
				+ " 			) OCF"
				+ " 			ON ugo_observation_id = o_observation_id AND ugo_user_group_id = o_user_group_id AND "
				+ " 			u_custom_field_id = o_custom_field_id"
				+ " 		) R ) R  GROUP BY  ugo_observation_id, ugo_user_group_id, u_custom_field_id, u_allowed_participation, u_deafult_value, "
				+ " 		u_display_order, cf_author_id, cf_data_type, cf_field_type, cf_icon_url, cf_name, cf_notes, cf_units"
				+ " 		)R GROUP BY ugo_observation_id, ugo_user_group_id)R GROUP BY ugo_observation_id, ugo_user_group_id) R GROUP BY ugo_observation_id) UGO"
				+ " 		INNER JOIN" + " 		(" + " 		SELECT observation_id, "
				+ " 		jsonb_agg( DISTINCT (to_jsonb(row_to_json((SELECT t FROM  (SELECT id, icon, name, webaddress)t)))))\\:\\:json user_group_observations"
				+ " 		FROM"
				+ " 		(SELECT user_group_id, observation_id FROM user_group_observations WHERE observation_id = "
				+ observationId + " ) UGO" + " 		INNER JOIN"
				+ " 		(SELECT id ,icon, name, webaddress FROM user_group ) U"
				+ " 		ON id = UGO.user_group_id GROUP BY observation_id"
				+ " 		) U ON U. observation_id =  UGO.observation_id ) UG ON UG.observation_id = O.id" + " "
				+ " LEFT OUTER JOIN" + " 		(" + " 		SELECT object_id AS observation_id,"
				+ " 		jsonb_agg( DISTINCT (to_jsonb(row_to_json(( SELECT t FROM (SELECT flag_id id, author_id, author_name, profile_pic, created_on, flag )t)))))\\:\\:json "
				+ " 		flags" + " 		FROM"
				+ " 		(SELECT id flag_id, object_id, author_id, notes, created_on, flag  "
				+ " 		FROM flag WHERE object_type = 'species.participation.Observation' AND object_id = "
				+ observationId + " ) F" + " 		LEFT OUTER JOIN"
				+ " 		(SELECT id, name author_name, profile_pic from suser ) U ON U.id = author_id GROUP BY object_id"
				+ " 		) F ON F.observation_id = O.id" + " " + " LEFT OUTER JOIN" + " 		("
				+ " 		SELECT tag_ref as observation_id, "
				+ " 		jsonb_agg(DISTINCT (to_jsonb(row_to_json((SELECT t FROM (SELECT tag_id id, tag_name AS name)t)))))\\:\\:json tags"
				+ " 		FROM" + " 		(SELECT id tag_id , name tag_name FROM tags)extended_tags"
				+ " 		INNER JOIN"
				+ " 		(SELECT id tl_id, tag_id tl_tag_id, tag_ref FROM tag_links where type = 'observation' AND tag_ref = "
				+ observationId + " )extended_tag_links" + " 		ON tag_id = tl_tag_id GROUP BY tag_ref"
				+ " 		) T ON T.observation_id = O.id" + " " + " LEFT OUTER JOIN" + " 		("
				+ " 		SELECT object_id as observation_id,"
				+ " 		jsonb_agg(DISTINCT(to_jsonb(row_to_json(( SELECT t FROM (SELECT id, author_id, author_name, profile_pic, created_on, notes, "
				+ " 		user_group_id, language_id, language_name)t )) )))\\:\\:json featured" + " 		FROM"
				+ " 		(SELECT id, author_id, created_on, notes, user_group_id," + " 		language_id, object_id "
				+ " 		FROM featured WHERE object_type = 'species.participation.Observation' AND object_id = "
				+ observationId + " ) F " + " 		LEFT OUTER JOIN"
				+ " 		(SELECT id u_id, name author_name, profile_pic FROM suser) U ON u_id = author_id"
				+ " 		LEFT OUTER JOIN"
				+ " 		(SELECT id l_id, name language_name, three_letter_code FROM language ) L ON l_id = language_id GROUP BY object_id"
				+ " 		) FE ON FE.observation_id = O.id" + " " + " LEFT OUTER JOIN" + " 		("
				+ " 		SELECT observation_fact_id observation_id, "
				+ " 		jsonb_agg(DISTINCT row_to_json(( SELECT t FROM (SELECT trait_id, description, field_id, trait_icon, name, is_participatory, units, 			trait_types,data_types, trait_instance->0 AS trait_value)t ))\\:\\:jsonb) facts "
				+ " 		FROM "
				+ " 		(SELECT  observation_fact_id, trait_id, description, field_id, trait_icon, name, is_participatory, units, trait_types,data_types,"
				+ " 		jsonb_agg(DISTINCT etrait_instance) trait_instance" + " 		FROM " + " 		("
				+ " 			SELECT observation_fact_id, trait_id, description, field_id, trait_icon, name, is_participatory, units, trait_types,data_types,"
				+ " 			row_to_json(( SELECT t FROM (SELECT fact_id fact_id, contributor_id, fact_value from_value, to_value, from_date, "
				+ " 			to_date, tv_id trait_value_id, tv_description description, tv_icon icon, value)t))\\:\\:jsonb etrait_instance"
				+ " 			FROM 	" + " 			("
				+ " 				(SELECT id fact_id, object_id observation_fact_id, contributor_id, "
				+ " 				trait_instance_id, trait_value_id, value fact_value, to_value, from_date, to_date"
				+ " 				FROM fact WHERE object_type = 'species.participation.Observation' and is_deleted = false AND "
				+ " 				object_id = " + observationId + " " + " 				) F  "
				+ " 				LEFT OUTER JOIN"
				+ " 				(SELECT id trait_id, description, field_id, icon trait_icon, name, is_participatory, units, trait_types, data_types FROM  trait) T"
				+ " 				ON  trait_instance_id = trait_id" + " 				LEFT OUTER JOIN"
				+ " 				(SELECT id tv_id, description tv_description, icon tv_icon, trait_instance_id, value  FROM trait_value) TV"
				+ " 				ON tv_id = trait_value_id" + " 			) F " + " 		)F	"
				+ " 		GROUP BY observation_fact_id, trait_id, description, field_id, "
				+ " 		trait_icon, name, is_participatory, units, trait_types,data_types ) F WHERE observation_fact_id = "
				+ observationId + "  GROUP BY 	" + " 		observation_fact_id)FA ON FA.observation_id = O.id";

		Session session = sessionFactory.openSession();

		System.out.println(query);
		Query<ObservationESDocument> qry = session.createNativeQuery(query, ObservationESDocument.class);
		ObservationESDocument result = qry.getSingleResult();
		return result;
	}

	@SuppressWarnings("unchecked")
	public List<Observation_resource> constructObservationResource(String observationId) {

		String qry = "SELECT observation_resource FROM " + "(" + "SELECT  observation_id , " + "jsonb_agg( DISTINCT "
				+ "to_jsonb((row_to_json((SELECT t FROM (SELECT resource_id id, description , file_name , type ,url, rating , upload_time , uploader_id, license_id) t )) "
				+ ")))\\:\\:json observation_resource " + "FROM "
				+ "(SELECT resource_id or_resource_id, observation_id FROM observation_resource ) EO " + "INNER JOIN "
				+ "(SELECT id resource_id, description , file_name , type ,url, rating , upload_time , uploader_id, license_id FROM resource ) extended_resource "
				+ "ON or_resource_id = resource_id GROUP BY observation_id" + ") obr WHERE observation_id = "
				+ observationId;

		Session session = sessionFactory.openSession();
		Query<Object[]> query = session.createNativeQuery(qry);
		Object[] result = query.getSingleResult();
		for (Object o : result) {
			System.out.println(o.toString());
		}
		return null;
	}

	public Tags constructTags(String observaitonId) {

		String qry = "SELECT * FROM ( " + "SELECT tag_ref as observation_id, "
				+ "jsonb_agg(DISTINCT (to_jsonb(row_to_json((SELECT t FROM (SELECT tag_id id, tag_name AS name)t)))))\\:\\:json tags "
				+ "FROM " + "(SELECT id tag_id , name tag_name FROM tags) extended_tags " + "INNER JOIN "
				+ "(SELECT id tl_id, tag_id tl_tag_id, tag_ref FROM tag_links where type = 'observation')extended_tag_links "
				+ "ON tag_id = tl_tag_id GROUP BY tag_ref " + ") T WHERE observation_id = " + observaitonId;

		Session session = sessionFactory.openSession();
		Query<Tags> query = session.createNativeQuery(qry, Tags.class);
		Tags result = query.getSingleResult();
		return result;
	}

	public Facts constructFacts(String observationId) {
		String qry = "SELECT * FROM " + "(" + "SELECT observation_fact_id observation_id, "
				+ "jsonb_agg(DISTINCT row_to_json(( SELECT t FROM (SELECT trait_id, description, field_id, trait_icon, name, is_participatory, units, trait_types,data_types, trait_instance->0 AS trait_value)t ))\\:\\:jsonb) facts "
				+ "FROM "
				+ "(SELECT  observation_fact_id, trait_id, description, field_id, trait_icon, name, is_participatory, units, trait_types,data_types, "
				+ "jsonb_agg(DISTINCT etrait_instance) trait_instance " + "FROM " + "("
				+ "	SELECT observation_fact_id, trait_id, description, field_id, trait_icon, name, is_participatory, units, trait_types,data_types, "
				+ "	row_to_json(( SELECT t FROM (SELECT fact_id fact_id, contributor_id, fact_value from_value, to_value, from_date, "
				+ "	to_date, tv_id trait_value_id, tv_description description, tv_icon icon, value)t))\\:\\:jsonb etrait_instance "
				+ "	FROM " + "	(" + "		(SELECT id fact_id, object_id observation_fact_id, contributor_id, "
				+ "		trait_instance_id, trait_value_id, value fact_value, to_value, from_date, to_date "
				+ "		FROM fact WHERE object_type = 'species.participation.Observation' and is_deleted = false "
				+ "		) F  " + "		LEFT OUTER JOIN "
				+ "		(SELECT id trait_id, description, field_id, icon trait_icon, name, is_participatory, units, trait_types, data_types FROM  trait) T "
				+ "		ON  trait_instance_id = trait_id " + "		LEFT OUTER JOIN "
				+ "		(SELECT id tv_id, description tv_description, icon tv_icon, trait_instance_id, value  FROM trait_value) TV "
				+ "		ON tv_id = trait_value_id " + "	) F " + ")F	"
				+ "GROUP BY observation_fact_id, trait_id, description, field_id, trait_icon, name, is_participatory, units, trait_types,data_types ) F GROUP BY "
				+ "observation_fact_id)FA WHERE observation_id = " + observationId;

		Session session = sessionFactory.openSession();
		Query<Facts> query = session.createNativeQuery(qry, Facts.class);
		Facts result = query.getSingleResult();
		return result;

	}

	public List<Flags> constructFlags(String observationId) {
		String qry = "";

		Session session = sessionFactory.openSession();
		Query<Flags> query = session.createNativeQuery(qry, Flags.class);
		List<Flags> result = query.getResultList();
		return result;
	}

}
