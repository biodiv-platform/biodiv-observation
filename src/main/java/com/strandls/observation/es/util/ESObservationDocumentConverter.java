package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.resource.pojo.ResourceData;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.user.pojo.UserIbp;

/**
 * Utility class to convert ES document structures to domain POJOs
 * Works with existing ObservationESDocument class and its inner classes
 *
 * @author Optimization Team
 */
public class ESObservationDocumentConverter {

    private static final Logger logger = LoggerFactory.getLogger(ESObservationDocumentConverter.class);
    private static final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Convert ObservationESDocument to Observation entity
     */
    public static Observation toObservation(ObservationESDocument esDoc) {
        if (esDoc == null) {
            return null;
        }

        Observation obs = new Observation();
        obs.setId(esDoc.getObservation_id());
        obs.setAuthorId(esDoc.getAuthor_id());
        obs.setGroupId(esDoc.getGroup_id());
        obs.setNotes(esDoc.getNotes());
        obs.setFlagCount(esDoc.getFlag_count() != null ? esDoc.getFlag_count().intValue() : 0);
        obs.setVisitCount(esDoc.getVisit_count() != null ? esDoc.getVisit_count().longValue() : 0L);
        obs.setIsLocked(esDoc.getIs_locked());
        obs.setGeoPrivacy(esDoc.getGeo_privacy());
        obs.setPlaceName(esDoc.getPlace_name());
        obs.setReverseGeocodedName(esDoc.getReverse_geocoded_name());
        obs.setLocationScale(esDoc.getLocation_scale());
        obs.setProtocol(esDoc.getProtocol());
        obs.setBasisOfRecord(esDoc.getBasis_of_record());
        obs.setLanguageId(esDoc.getLanguage_id() != null ? esDoc.getLanguage_id().longValue() : null);
        obs.setDataTableId(esDoc.getData_table_id());
        obs.setDatasetId(esDoc.getDataset_id());

        // Location
        if (esDoc.getLocation() != null) {
            obs.setLatitude(esDoc.getLocation().getLat());
            obs.setLongitude(esDoc.getLocation().getLon());
        }

        // Dates
        obs.setFromDate(esDoc.getFrom_date());
        obs.setToDate(esDoc.getTo_date());
        obs.setCreatedOn(esDoc.getCreated_on());
        obs.setLastRevised(esDoc.getLast_revised());

        // Max voted reco ID
        if (esDoc.getMax_voted_reco() != null) {
            obs.setMaxVotedRecoId(esDoc.getMax_voted_reco().getId());
        }

        // Checklist annotations
        if (esDoc.getChecklist_annotation() != null) {
            obs.setChecklistAnnotations(esDoc.getChecklist_annotation());
        }

        return obs;
    }

    /**
     * Convert ES observation resources to ResourceData list
     */
    public static List<ResourceData> toResourceDataList(List<Observation_resource> esResources) {
        if (esResources == null || esResources.isEmpty()) {
            return Collections.emptyList();
        }

        return esResources.stream().map(esr -> {
            // Create Resource object
            com.strandls.resource.pojo.Resource resource = new com.strandls.resource.pojo.Resource();
            resource.setId(esr.getId());
            resource.setFileName(esr.getFile_name());
            resource.setType(esr.getType());
            resource.setDescription(esr.getDescription());
            resource.setUrl(esr.getUrl());
            resource.setRating(esr.getRating());
            resource.setContributor(esr.getContributor());
            resource.setLicenseId(esr.getLicense_id() != null ? esr.getLicense_id().longValue() : null);
            resource.setUploaderId(esr.getUploader_id() != null ? esr.getUploader_id().longValue() : null);

            // Parse upload_time string to Date
            if (esr.getUpload_time() != null) {
                try {
                    resource.setUploadTime(new java.sql.Date(
                        new java.text.SimpleDateFormat("yyyy-MM-dd").parse(esr.getUpload_time()).getTime()
                    ));
                } catch (Exception e) {
                    logger.warn("Failed to parse upload_time: {}", esr.getUpload_time());
                }
            }

            // Create License object
            com.strandls.resource.pojo.License license = new com.strandls.resource.pojo.License();
            if (esr.getLicense_id() != null) {
                license.setId(esr.getLicense_id().longValue());
            }
            license.setName(esr.getLicense_name());
            license.setUrl(esr.getLicense_url());

            // Create ResourceData with nested objects
            ResourceData rd = new ResourceData();
            rd.setResource(resource);
            rd.setLicense(license);
            // UserIbp and Tags will be null/empty for now
            rd.setUserIbp(null);
            rd.setTags(Collections.emptyList());

            return rd;
        }).collect(Collectors.toList());
    }

    /**
     * Convert ES facts to FactValuePair list
     * Note: FactValuePair has a simpler structure than Facts
     */
    public static List<FactValuePair> toFactValuePairList(List<Facts> esFacts) {
        if (esFacts == null || esFacts.isEmpty()) {
            return Collections.emptyList();
        }

        List<FactValuePair> result = new ArrayList<>();

        for (Facts esf : esFacts) {
            if (esf.getTrait_value() != null && !esf.getTrait_value().isEmpty()) {
                // Create one FactValuePair for each trait_value
                for (Trait_value tv : esf.getTrait_value()) {
                    FactValuePair fvp = new FactValuePair();
                    fvp.setNameId(esf.getTrait_id());
                    fvp.setName(esf.getName());
                    fvp.setValueId(tv.getTrait_value_id());
                    fvp.setValue(tv.getValue());
                    fvp.setFromDate(tv.getFrom_date());
                    fvp.setToDate(tv.getTo_date());
                    fvp.setType(esf.getTrait_types());
                    fvp.setIsParticipatry(esf.getIs_participatory());
                    result.add(fvp);
                }
            } else {
                // No trait values, create a single FactValuePair with just the trait info
                FactValuePair fvp = new FactValuePair();
                fvp.setNameId(esf.getTrait_id());
                fvp.setName(esf.getName());
                fvp.setType(esf.getTrait_types());
                fvp.setIsParticipatry(esf.getIs_participatory());
                result.add(fvp);
            }
        }

        return result;
    }

    /**
     * Convert ES all reco votes to RecoIbp list
     */
    public static List<RecoIbp> toRecoIbpList(List<All_reco_vote> esRecoVotes) {
        if (esRecoVotes == null || esRecoVotes.isEmpty()) {
            return Collections.emptyList();
        }

        return esRecoVotes.stream().map(esr -> {
            RecoIbp reco = new RecoIbp();

            if (esr.getScientific_name() != null) {
                Scientific_name esName = esr.getScientific_name();
                reco.setScientificName(esName.getName());

                if (esName.getTaxon_detail() != null) {
                    reco.setTaxonId(esName.getTaxon_detail().getId());
                    reco.setScientificName(esName.getTaxon_detail().getScientific_name());
                }

                if (esName.getAccepted_name_id() != null) {
                    reco.setSpeciesId(esName.getAccepted_name_id());
                }
            }

            // Get common name from common_names list
            if (esr.getCommon_names() != null && !esr.getCommon_names().isEmpty()) {
                reco.setCommonName(esr.getCommon_names().get(0).getCommon_name());
            }

            // Get first author from authors_voted
            if (esr.getAuthors_voted() != null && !esr.getAuthors_voted().isEmpty()) {
                Authors_voted firstAuthor = esr.getAuthors_voted().get(0);
                UserIbp userIbp = new UserIbp();
                userIbp.setId(firstAuthor.getId());
                userIbp.setName(firstAuthor.getName());
                userIbp.setProfilePic(firstAuthor.getProfile_pic());
                reco.setUserIbp(userIbp);
            }

            return reco;
        }).collect(Collectors.toList());
    }

    /**
     * Get main recommendation from max_voted_reco
     */
    public static RecoIbp toMainRecoIbp(Max_voted_reco maxReco, List<All_reco_vote> allRecoVotes, Long observationId) {
        if (maxReco == null) {
            return null;
        }

        RecoIbp reco = new RecoIbp();
        reco.setScientificName(maxReco.getScientific_name());
        reco.setTaxonId(maxReco.getId());

        // Get common name from common_names list
        if (maxReco.getCommon_names() != null && !maxReco.getCommon_names().isEmpty()) {
            reco.setCommonName(maxReco.getCommon_names().get(0).getCommon_name());
        }

        // Try to find matching vote to get user info and species ID
        if (allRecoVotes != null) {
            allRecoVotes.stream()
                .filter(vote -> vote.getRecommendation_id() != null && vote.getRecommendation_id().equals(maxReco.getId()))
                .findFirst()
                .ifPresent(vote -> {
                    if (vote.getAuthors_voted() != null && !vote.getAuthors_voted().isEmpty()) {
                        Authors_voted firstAuthor = vote.getAuthors_voted().get(0);
                        UserIbp userIbp = new UserIbp();
                        userIbp.setId(firstAuthor.getId());
                        userIbp.setName(firstAuthor.getName());
                        userIbp.setProfilePic(firstAuthor.getProfile_pic());
                        reco.setUserIbp(userIbp);
                    }

                    if (vote.getScientific_name() != null && vote.getScientific_name().getAccepted_name_id() != null) {
                        reco.setSpeciesId(vote.getScientific_name().getAccepted_name_id());
                    }
                });
        }

        return reco;
    }

    /**
     * Aggregate all reco votes into suggestions (same logic as current implementation)
     */
    public static List<AllRecoSugguestions> aggregateRecoSuggestions(List<RecoIbp> allRecoVotes) {
        if (allRecoVotes == null || allRecoVotes.isEmpty()) {
            return Collections.emptyList();
        }

        List<AllRecoSugguestions> result = new ArrayList<>();

        for (RecoIbp reco : allRecoVotes) {
            boolean updated = false;

            for (AllRecoSugguestions suggestion : result) {
                if (reco.getTaxonId() != null && suggestion.getTaxonId() != null) {
                    if (reco.getTaxonId().equals(suggestion.getTaxonId())) {
                        // Update existing suggestion
                        if ((suggestion.getCommonName() == null || suggestion.getCommonName().trim().isEmpty())
                                && reco.getCommonName() != null && !reco.getCommonName().trim().isEmpty()) {
                            suggestion.setCommonName(reco.getCommonName());
                        }
                        if ((suggestion.getScientificName() == null || suggestion.getScientificName().trim().isEmpty())
                                && reco.getScientificName() != null && !reco.getScientificName().trim().isEmpty()) {
                            suggestion.setScientificName(reco.getScientificName());
                        }
                        if (suggestion.getSpeciesId() == null && reco.getSpeciesId() != null) {
                            suggestion.setSpeciesId(reco.getSpeciesId());
                        }
                        suggestion.getUserList().add(reco.getUserIbp());
                        updated = true;
                        break;
                    }
                }
            }

            if (!updated) {
                List<UserIbp> userList = new ArrayList<>();
                userList.add(reco.getUserIbp());
                AllRecoSugguestions suggestion = new AllRecoSugguestions(
                    reco.getCommonName(),
                    reco.getScientificName(),
                    reco.getTaxonId(),
                    reco.getSpeciesId(),
                    userList
                );
                result.add(suggestion);
            }
        }

        return result;
    }

    /**
     * Convert lists are already typed in ObservationESDocument, just return safely
     */
    public static <T> List<T> safeList(List<T> list) {
        return list != null ? list : Collections.emptyList();
    }
}
