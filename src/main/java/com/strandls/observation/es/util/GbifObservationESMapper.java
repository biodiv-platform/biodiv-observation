package com.strandls.observation.es.util;

import java.util.ArrayList;
import java.text.SimpleDateFormat;
import java.time.Clock;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class GbifObservationESMapper {

	public ObservationESDocument mapToESDocument(Date date, String month, double lat, double lon, Long recoId,
			Long taxonId, Long rank, Long speciesid, String taxonStatus, List<Map<String, String>> hierarchy,
			String scientificName, String cannonicalName, Long acceptedNameIds, String italisicedForm, String position,
			Long id, Date dateIdentified, String name, String state, String district, String tahsil, Long groupId,
			String groupName, String externalReferenceLink) {

		ObservationESDocument gbifObs = new ObservationESDocument();
		Clock clock = Clock.systemUTC();
		Date createdOnDate = Date.from(clock.instant());

		gbifObs.setFrom_date(date);
		gbifObs.setCreated_on(createdOnDate);
		gbifObs.setObserved_in_month(month);
		gbifObs.setLocation(new Location(lat, lon));
		gbifObs.setObservation_id(id);
		gbifObs.setExternal_reference_link(externalReferenceLink);
		gbifObs.setIs_external(true);
		gbifObs.setData_source("gbif.org");
		gbifObs.setIs_checklist(false);
		gbifObs.setNo_media(0);
		gbifObs.setNo_of_audio(0);
		gbifObs.setNo_of_images(0);
		gbifObs.setNo_of_videos(0);

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

	}

}
