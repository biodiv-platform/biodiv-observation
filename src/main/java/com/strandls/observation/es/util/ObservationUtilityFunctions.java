/**
 * 
 */
package com.strandls.observation.es.util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVWriter;
import com.strandls.observation.pojo.DownloadLog;

/**
 * @author ashish
 *
 */
public class ObservationUtilityFunctions {

	private final Logger logger = LoggerFactory.getLogger(ObservationUtilityFunctions.class);

	private final String[] csvCoreHeaders = { "catalogNumber", "createdBy", "placeName", "flagNotes",
			"noOfIdentifications", "geoPrivacy", "createdOn", "associatedMedia", "group_id", "dateAccuracy", "isLocked",
			"locationLat", "locationLon", "locationScale", "fromDate", "toDate", "rank", "scientificName", "commonName",
			"kingdom", "phylum", "class", "order", "superfamily", "family", "genus", "species" };
	private final Integer hierarchyDepth = 8;
	private final String csvFileDownloadPath = "/app/data/biodiv/data-archive/listpagecsv";
	private CSVWriter writer;

	public String getCsvFileNameDownloadPath() {

		Date date = new Date();
		String fileName = "obv_"+date.getTime()+".csv";
		String filePathName = csvFileDownloadPath + File.separator + fileName;
		File file = new File(filePathName);
		try {
			file.createNewFile();
		} catch (IOException e) {
			logger.error(e.getMessage());
		}
		return fileName;
	}

	public List<String[]> getCsvHeaders(List<String> customfields, List<String> taxonomic, List<String> spatial,
			List<String> traits, List<String> temporal, List<String> misc) {
		List<String[]> headers = new ArrayList<String[]>();
		List<String> optionalHeaders = getOptionalHeaders(customfields, taxonomic, spatial, traits, temporal, misc);
		List<String> header = Stream.concat(Arrays.asList(csvCoreHeaders).stream(), optionalHeaders.stream())
				.collect(Collectors.toList());
		headers.add(header.stream().toArray(String[]::new));
		return headers;
	}

	public DownloadLog createDownloadLogEntity(String filePath, Long authorId, String filterURL, String notes,
			Long offSet, String status, String type) {
		Timestamp timestamp = new Timestamp(System.currentTimeMillis());
		DownloadLog entity = new DownloadLog();
		entity.setAuthorId(authorId);
		entity.setFilePath(filePath);
		entity.setFilterUrl(filterURL);
		entity.setNotes(notes);
		entity.setOffsetParam(offSet);
		entity.setCreatedOn(timestamp);
		entity.setStatus(status);
		entity.setType(type);
		entity.setVersion(2L);
		return entity;

	}

	public void insertListToCSV(List<ObservationListElasticMapping> records, CSVWriter writer,
			List<String> customfields, List<String> taxonomic, List<String> spatial, List<String> traits,
			List<String> temporal, List<String> misc) {

		List<String[]> rowSets = new ArrayList<String[]>();
		for (ObservationListElasticMapping record : records) {
			List<String> row = new ArrayList<String>();
			row.add(record.getObservationId().toString());
			row.add(record.getUser().getName());
			row.add(record.getPlaceName());
			row.add(record.getFlags() != null ? fetchFlags(record.getFlags()) : null);
			row.add(record.getNoOfIdentification());
			row.add(record.getGeoPrivacy().toString());
			row.add(parseDate(record.getCreatedOn()));
			row.add(record.getReprImageUrl());
			row.add(record.getSpeciesGroup());
			row.add(record.getDateAccuracy());
			row.add(record.getIsLocked().toString());
			row.add(record.getLatitude().toString());
			row.add(record.getLongitude().toString());
			row.add(record.getLocationScale());
			row.add(parseDate(record.getFromDate()));
			row.add(parseDate(record.getToDate()));
			row.add(record.getMaxVotedReco() != null ? record.getMaxVotedReco().getRanktext() : null);
			row.add(record.getMaxVotedReco() != null ? record.getMaxVotedReco().getScientific_name() : null);
			row.add(record.getMaxVotedReco() != null ? fetchMaxVotedCommonName(record.getMaxVotedReco()) : null);
			row.addAll(record.getMaxVotedReco() != null
					? (record.getMaxVotedReco().getHierarchy() != null
							? getMaxVotedHierarchy(record.getMaxVotedReco().getHierarchy())
							: new ArrayList<String>(Collections.nCopies(hierarchyDepth, (String) null)))
					: new ArrayList<String>(Collections.nCopies(hierarchyDepth, (String) null)));
			List<String> optionalHeader = null;
			if (!customfields.isEmpty()) {
				optionalHeader = Arrays.asList(customfields.get(0).split(","));
				Collection<String> values = fetchCustomFieldForCsv(optionalHeader, record.getCustomFields());
				row.addAll(values);
			}

			if (!taxonomic.isEmpty()) {
				optionalHeader = Arrays.asList(taxonomic.get(0).split(","));
				Collection<String> values = fetchTaxonomicForCsv(optionalHeader, record.getAllRecoVotes(),
						record.getMaxVotedReco());
				row.addAll(values);
			}
			if (!spatial.isEmpty()) {
				optionalHeader = Arrays.asList(spatial.get(0).split(","));
				Collection<String> values = fetchSpatialForCsv(optionalHeader, record.getLocationInformation(),
						record.getReverseGeocodedName());
				row.addAll(values);
			}
			if (!traits.isEmpty()) {
				optionalHeader = Arrays.asList(traits.get(0).split(","));
				Collection<String> values = fetchTraitsForCsv(optionalHeader, record.getFacts());
				row.addAll(values);
			}
			if (!temporal.isEmpty()) {
				optionalHeader = Arrays.asList(temporal.get(0).split(","));
				Collection<String> values = fetchTemporalForCsv(optionalHeader, record);
				row.addAll(values);
			}
			if (!misc.isEmpty()) {
				optionalHeader = Arrays.asList(misc.get(0).split(","));
				Collection<String> values = fetchMiscForCsv(optionalHeader, record);
				row.addAll(values);
			}

			rowSets.add(row.stream().toArray(String[]::new));
		}
		writer.writeAll(rowSets);

	}

	public CSVWriter getCsvWriter(String fileName) {
		FileWriter outputfile = null;
		try {
			outputfile = new FileWriter(new File(fileName));
			writer = new CSVWriter(outputfile);
		} catch (IOException e) {
			logger.error("CSVWriter error logging - " + e.getMessage());
		}
		return writer;
	}

	public void writeIntoCSV(CSVWriter writer, List<String[]> data) {
		writer.writeAll(data);
	}

	public void closeWriter() {
		try {
			writer.close();
		} catch (IOException e) {
			logger.error("CSVWriter error logging - " + e.getMessage());
		}
	}

	public PublicationGrade GradeObservation(ObservationListElasticMapping observation) {
		PublicationGrade observationGrade = new PublicationGrade();
		observationGrade.setHasMediaEvidence((observation.getNoOfAudios() != 0 || observation.getNoOfImages() != 0
				|| observation.getNoOfVideos() != 0) ? true : false);

		observationGrade.setHasDateDefined(observation.getFromDate() != null ? true : false);
		observationGrade.setIsLocationDefined(
				(observation.getLatitude() != null || observation.getLongitude() != null) ? true : false);

		observationGrade.setHasfamilyRankOrLower(
				observation.getMaxVotedReco() != null ? (observation.getMaxVotedReco().getRank() >= 5 ? true : false)
						: false);
		observationGrade.setHasTaxonName(observation.getMaxVotedReco() != null
				? (observation.getMaxVotedReco().getScientific_name() != null ? true : false)
				: false);

		observationGrade.setIsIdValidated(
				observation.getIsLocked() == true || (observation.getRecoVoteCount() >= 2 ? true : false) ? true
						: false);

		observationGrade.setIsNotFlagged(observation.getFlagCount() == 0 ? true : false);
		observationGrade.setIsNativeObservation(observation.getDatasetTitle() != null
				? (observation.getDatasetTitle().toLowerCase().contains("gbif") ? false : true)
				: true);

		return observationGrade;
	}

	private String fetchMaxVotedCommonName(Max_voted_reco reco) {
		List<Common_names> names = reco.getCommon_names();
		String value = "";
		if (names != null) {
			for (Common_names name : names) {
				if (name != null)
					value += name.getCommon_name() + ":" + name.getLanguage_name() + " | ";
			}
			if (value.length() > 3)
				value = value.substring(0, value.length() - 3);
		}
		return value;
	}

	private List<String> getMaxVotedHierarchy(List<Hierarchy> hierarchy) {
		List<String> hierarchyValues = new ArrayList<String>(Collections.nCopies(hierarchyDepth, (String) null));
		for (Hierarchy h : hierarchy) {
			int rank = h.getRank().intValue();
			if (rank == 7) {
				rank -= 1;
			} else if (rank == 9) {
				rank -= 2;
			}
			if (rank >= 0 && rank <= 7)
				hierarchyValues.set(rank, h.getNormalized_name());
		}
		return hierarchyValues;
	}

	private List<String> getOptionalHeaders(List<String> customfields, List<String> taxonomic, List<String> spatial,
			List<String> traits, List<String> temporal, List<String> misc) {
		List<String> optionalHeader = new ArrayList<String>();
		if (!customfields.isEmpty()) {
			customfields = Arrays.asList(customfields.get(0).split(","));
			optionalHeader.addAll(customfields);
		}
		if (!taxonomic.isEmpty()) {
			taxonomic = Arrays.asList(taxonomic.get(0).split(","));
			optionalHeader.addAll(taxonomic);
		}
		if (!spatial.isEmpty()) {
			spatial = Arrays.asList(spatial.get(0).split(","));
			optionalHeader.addAll(spatial);
		}
		if (!traits.isEmpty()) {
			traits = Arrays.asList(traits.get(0).split(","));
			optionalHeader.addAll(traits);
		}
		if (!temporal.isEmpty()) {
			temporal = Arrays.asList(temporal.get(0).split(","));
			optionalHeader.addAll(temporal);
		}
		if (!misc.isEmpty()) {
			misc = Arrays.asList(misc.get(0).split(","));
			optionalHeader.addAll(misc);
		}
		return optionalHeader;
	}

	private Collection<String> fetchCustomFieldForCsv(List<String> customfields, List<Custom_fields> cf) {
		LinkedHashMap<String, String> map = createLinkedHashMap(customfields);
		if (cf != null) {
			for (Custom_fields customField : cf) {
				for (Custom_field field : customField.getCustom_field()) {
					String key = field.getCf_name();
					String keyValue = map.get(key);
					String fieldValue = getCustomFieldValue(field.getCustom_field_values(),
							field.getField_type().toLowerCase().trim());
					if (keyValue == null)
						map.replace(key, fieldValue);
					else
						map.replace(key, keyValue + " | " + fieldValue);
				}
			}
		}
		return map.values();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private String getCustomFieldValue(Custom_field_values values, String fieldType) {
		if (fieldType.equalsIgnoreCase("field text"))
			return values.getField_text_data();
		if (fieldType.equalsIgnoreCase("range"))
			return values.getMin_range() + "-" + values.getMax_range();
		if (fieldType.equalsIgnoreCase("single categorical"))
			return values.getSingle_categorical_data();
		if (fieldType.equalsIgnoreCase("multiple categorical"))
			return String.join(";", values.getMultiple_categorical_data() != null ? values.getMultiple_categorical_data() : new ArrayList());
		return null;

	}

	private Collection<String> fetchTaxonomicForCsv(List<String> taxonomic, List<All_reco_vote> allRecoVote,
			Max_voted_reco maxVotedReco) {
		LinkedHashMap<String, String> map = createLinkedHashMap(taxonomic);
		String[] taxonomicValues = { "previousIdentifications", "previousVernacularNames", "speciesPageId",
				"higherClassificationId" };
		if (allRecoVote != null) {
			for (All_reco_vote reco : allRecoVote) {
				String name = reco.getScientific_name() != null ? reco.getScientific_name().getName() : null;
				String recoId = reco.getRecommendation_id().toString();
				String keyValue = map.get(taxonomicValues[0]);
				if (keyValue == null)
					map.replace(taxonomicValues[0], recoId + "#" + name + " | ");
				else
					map.replace(taxonomicValues[0], keyValue + recoId + "#" + name + " | ");

				String commonNameValue = null;
				keyValue = map.get(taxonomicValues[1]);
				if (reco.getCommon_names() != null) {
					for (Common_names commonNames : reco.getCommon_names()) {
						if (commonNameValue == null)
							commonNameValue = commonNames.getCommon_name() + ":" + commonNames.getLanguage_name() + "_";
						else
							commonNameValue += commonNames.getCommon_name() + ":" + commonNames.getLanguage_name()
									+ "_";
					}
					if (keyValue == null)
						map.replace(taxonomicValues[1],
								recoId + "#" + commonNameValue.substring(0, commonNameValue.length() - 1) + " | ");
					else
						map.replace(taxonomicValues[1], keyValue + recoId + "#"
								+ commonNameValue.substring(0, commonNameValue.length() - 1) + " | ");
				}
			}
		}
		if (maxVotedReco != null) {
			map.replace(taxonomicValues[2],
					(maxVotedReco.getSpecies_id() != null ? maxVotedReco.getSpecies_id().toString() : null));
			List<Hierarchy> hierarchy = maxVotedReco.getHierarchy();
			if (hierarchy != null) {
				String value = "";
				for (Hierarchy level : hierarchy) {
					value += "name:" + level.getNormalized_name() + "#rank:" + level.getRank() + "#taxonID:"
							+ level.getTaxon_id() + " | ";
				}
				if (value.length() > 3) {
					map.replace(taxonomicValues[3], value.substring(0, value.length()-3));
				}
			}
		}
		return map.values();

	}

	private Collection<String> fetchSpatialForCsv(List<String> spatial, LocationInformation locationInformation,
			String reverseGeocodedName) {
		LinkedHashMap<String, String> map = createLinkedHashMap(spatial);
		String[] category = { "state", "district", "tahsil", "reverseGeocodedName" };
		if (locationInformation != null) {
			map.replace(category[0], locationInformation.getState());
			map.replace(category[1], locationInformation.getDistrict());
			map.replace(category[2], locationInformation.getTahsil());
		}
		if (reverseGeocodedName != null)
			map.replace(category[3], reverseGeocodedName);
		return map.values();
	}

	private Collection<String> fetchTraitsForCsv(List<String> traits, List<Facts> facts) {
		LinkedHashMap<String, String> map = createLinkedHashMap(traits);
		if (facts != null) {
			for (Facts fact : facts) {
				String traitName = fact.getName();
				String value = map.get(traitName);
				if (value == null)
					map.replace(traitName, fetchTraitValue(fact.getTrait_value()));
				else
					map.replace(traitName, value + " | " + fetchTraitValue(fact.getTrait_value()));
			}
		}
		return map.values();
	}

	private String fetchTraitValue(List<Trait_value> values) {
		String traitValues = null;
		if (values != null) {
			for (Trait_value traitValue : values) {
				if (traitValues == null)
					traitValues = traitValue.getValue();
				else
					traitValues += " | " + traitValue.getValue();
			}
		}
		return traitValues;
	}

	private Collection<String> fetchTemporalForCsv(List<String> temporal, ObservationListElasticMapping document) {
		LinkedHashMap<String, String> map = createLinkedHashMap(temporal);
		String[] temporalFields = {"observedInMonth", "lastRevised","toDate"};
		map.replace(temporalFields[0], document.getObservedInMonth());
		map.replace(temporalFields[1], document.getLastRevised());
		map.replace(temporalFields[2], document.getToDate());
		return map.values();
	}

	private Collection<String> fetchMiscForCsv(List<String> misc, ObservationListElasticMapping document) {
		LinkedHashMap<String, String> map = createLinkedHashMap(misc);
		String[] miscFields = { "datasetName", "containsMedia", "uploadProtocol", 
				"flagCount", "organismRemarks","annotations", "tags", 
				"userGroup","noOfImages","speciesGroup" };
		map.replace(miscFields[0], document.getDatasetTitle());
		map.replace(miscFields[1], document.getContainsMedia().toString());
		map.replace(miscFields[2], document.getUploadProtocol());
		map.replace(miscFields[3], document.getFlagCount().toString());
		map.replace(miscFields[4], document.getOrganismRemarks());
		map.replace(miscFields[5], document.getAnnotations());
		map.replace(miscFields[6], document.getTags() != null ? fetchTags(document.getTags()) : null);
		map.replace(miscFields[7], document.getSpeciesGroup());
		map.replace(miscFields[8], document.getNoOfImages().toString());
		map.replace(miscFields[9], document.getSpeciesGroup());
		return map.values();

	}

	private String fetchTags(List<Tags> tags) {
		String value = "";
		for (Tags tag : tags) {
			value += tag.getName()+" | ";
		}
		if (value.length() > 3)
			return value.substring(0, value.length() - 3);
		return null;
	}

	private String fetchFlags(List<Flags> flags) {
		String value = "";
		for (Flags flag : flags) {
			value += flag.getNotes() + " | ";
		}
		if (value.length() > 3)
			return value.substring(0, value.length() - 3);
		return value;
	}

	private LinkedHashMap<String, String> createLinkedHashMap(List<String> keys) {
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		for (String key : keys) {
			map.put(key, null);
		}
		return map;
	}
	
	private String parseDate(String date) {
        DateFormat originalFormat = new SimpleDateFormat("dd/MM/yyyy"); 
        DateFormat secondaryFormat = new SimpleDateFormat("yyyy-MM-dd");
        if(!(date == null)) {
	        if(date.contains("-") || date.contains("T")) {
	        	try {
	        		return originalFormat.format(new Date(secondaryFormat.parse(date).getTime())).toString();
				} catch (ParseException e) {
					logger.error("Date Parsing Error - "+e.getMessage());
				}
	        }
	        else
	        {
	        	return originalFormat.format(new Date(Long.parseLong(date))).toString();
	        }
        }
        return "";

	}
}
