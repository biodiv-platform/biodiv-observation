package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.text.SimpleDateFormat;
import java.time.Clock;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.strandls.naksha.pojo.ObservationLocationInfo;

public class GbifObservationESMapper {

	public ObservationESDocument mapToESDocument(Date date, String month, Double lat, Double lon, String placeName,
			Long recoId, Long taxonId, Long rank, Long speciesid, String taxonStatus,
			List<Map<String, String>> hierarchy, String scientificName, String cannonicalName, Long acceptedNameIds,
			String italisicedForm, String position, Long id, Date dateIdentified, String name, String state,
			String district, String tahsil, Long groupId, String groupName, String externalOriginalReferenceLink,
			String externalGbifReferenceLink,ObservationLocationInfo layerInfo) {

		ExternalObservationESDocument gbifObs = new ExternalObservationESDocument();
		Clock clock = Clock.systemUTC();
		Date createdOnDate = Date.from(clock.instant());

		gbifObs.setFrom_date(date);
		gbifObs.setCreated_on(createdOnDate);
		gbifObs.setObserved_in_month(month);
		gbifObs.setLocation(new Location(lat, lon));
		gbifObs.setObservation_id(id);
		gbifObs.setIs_external(true);
		gbifObs.setData_source("gbif.org");
		gbifObs.setExternal_original_reference_link(externalOriginalReferenceLink);
		gbifObs.setExternal_gbif_reference_link(externalGbifReferenceLink);
		gbifObs.setIs_checklist(false);
		gbifObs.setNo_media(1);
		gbifObs.setNo_of_audio(0);
		gbifObs.setNo_of_images(0);
		gbifObs.setNo_of_videos(0);
		gbifObs.setPlace_name(placeName);
		gbifObs.setLayer_info(layerInfo);
		gbifObs.setUnique_id_prefix("gbif");

		Max_voted_reco maxVotedReco = new Max_voted_reco();
		mapMaxvotedreco(maxVotedReco, recoId, taxonId, rank, speciesid, taxonStatus, hierarchy, scientificName);
		gbifObs.setMax_voted_reco(maxVotedReco);

		List<All_reco_vote> allRecoVote = new ArrayList<>();
		All_reco_vote reco = new All_reco_vote();

		String sId = null;
		if (speciesid != null) {
			sId = String.valueOf(speciesid);
		}

		mapAllRecoVote(reco, recoId, scientificName, cannonicalName, acceptedNameIds, taxonId, italisicedForm, rank,
				taxonStatus, position, sId, dateIdentified, name);
		allRecoVote.add(reco);
		gbifObs.setAll_reco_vote(allRecoVote);

		LocationInformation locationInfo = new LocationInformation();
		locationInfo.setState(state);
		locationInfo.setDistrict(district);
		locationInfo.setTahsil(tahsil);

		gbifObs.setLocation_information(locationInfo);
		gbifObs.setGroup_id(groupId);
		gbifObs.setGroup_name(groupName);
		gbifObs.setIs_locked(false);

		return (gbifObs);
	}

	private void mapAllRecoVote(All_reco_vote allRecoVote, Long recommendationId, String scientificName,
			String cannonicalName, Long acceptedNameId, Long taxonId, String italicisedForm, Long rank, String status,
			String position, String speciesId, Date dateIdentified, String name) {
		allRecoVote.setRecommendation_id(recommendationId);
		Scientific_name recoScientificName = new Scientific_name();
		Taxon_detail taxonDetails = new Taxon_detail();

		taxonDetails.setId(taxonId);
		taxonDetails.setItalicised_form(italicisedForm);
		taxonDetails.setPosition(position);
		taxonDetails.setRank(rank);

		taxonDetails.setSpecies_id(speciesId);

		taxonDetails.setStatus(status);
		taxonDetails.setCanonical_form(cannonicalName);
		taxonDetails.setScientific_name(scientificName);
		taxonDetails.setName(name);

		recoScientificName.setName(name);
		recoScientificName.setAccepted_name_id(acceptedNameId);
		recoScientificName.setTaxon_detail(taxonDetails);

		allRecoVote.setScientific_name(recoScientificName);
		allRecoVote.setLast_modified(dateIdentified);

	}

	private void mapMaxvotedreco(Max_voted_reco maxVotedeReco, Long recoId, Long taxonId, Long rank, Long speciesId,
			String taxonStatus, List<Map<String, String>> hierarchy, String scientificName) {

		maxVotedeReco.setId(recoId);
		maxVotedeReco.setRank(rank);
		maxVotedeReco.setSpecies_id(speciesId);
		maxVotedeReco.setTaxonstatus(taxonStatus);
		maxVotedeReco.setScientific_name(scientificName);

		List<Hierarchy> list = new ArrayList<>();
		if (hierarchy != null) {
			for (Map<String, String> node : hierarchy) {
				Hierarchy h = new Hierarchy();
				h.setNormalized_name(node.get("normalised_name"));
				h.setRank(Long.parseLong(node.get("rank")));
				h.setTaxon_id(Long.parseLong(node.get("taxon_id").toString()));
				list.add(h);
			}
		}

		maxVotedeReco.setHierarchy(list);
		if (rank != null) {
			maxVotedeReco.setRanktext(getRankText(rank));
		}

	}

	private String getRankText(Long rank) {

		if (rank == 0) {
			return ("Kingdom");
		} else if (rank == 1) {
			return ("Phylum");
		} else if (rank == 2) {
			return ("Class");
		} else if (rank == 3) {
			return ("Order");
		} else if (rank == 4) {
			return ("Superfamily");
		} else if (rank == 5) {
			return ("Family");
		} else if (rank == 6) {
			return ("Subfamily");
		} else if (rank == 7) {
			return ("Genus");
		} else if (rank == 8) {
			return ("Subgenus");
		} else if (rank == 9) {
			return ("Species");
		} else if (rank == 10) {
			return ("Infraspecies");
		} else {
			return null;
		}

	}

}
