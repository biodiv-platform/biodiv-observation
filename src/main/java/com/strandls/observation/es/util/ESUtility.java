package com.strandls.observation.es.util;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.esmodule.pojo.CustomFields;
import com.strandls.esmodule.pojo.FilterPanelData;
import com.strandls.esmodule.pojo.MapAndBoolQuery;
import com.strandls.esmodule.pojo.MapAndMatchPhraseQuery;
import com.strandls.esmodule.pojo.MapAndRangeQuery;
import com.strandls.esmodule.pojo.MapExistQuery;
import com.strandls.esmodule.pojo.MapGeoPoint;
import com.strandls.esmodule.pojo.MapOrBoolQuery;
import com.strandls.esmodule.pojo.MapOrMatchPhraseQuery;
import com.strandls.esmodule.pojo.MapOrRangeQuery;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchParams.SortTypeEnum;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.esmodule.pojo.Traits;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 */
public class ESUtility {

	private final Logger logger = LoggerFactory.getLogger(ESUtility.class);

	@Inject
	private EsServicesApi esService;

	private List<Object> cSTSOT(String str) {
		if (str == null || str.isBlank())
			return new ArrayList<>();

		String[] y = str.split(",");
		Set<Object> strSet1 = Arrays.stream(y).collect(Collectors.toSet());
		return new ArrayList<>(strSet1);
	}

	private boolean isParsableAsLong(final String s) {
		try {
			Long.valueOf(s);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	private List<Long> getListOfIds(String str) {
		if (str == null || str.isBlank())
			return new ArrayList<>();
		String[] y = str.split(",");
		List<Long> longIds = new ArrayList<>();
		for (String z : y) {
			if (isParsableAsLong(z)) {
				longIds.add(Long.parseLong(z));
			} else {
				longIds.add(0L);
			}
		}
		return longIds;
	}

	private MapAndBoolQuery assignBoolAndQuery(String key, List<Object> values) {
		MapAndBoolQuery andBool = new MapAndBoolQuery();
		andBool.setKey(key);
		andBool.setValues(values);
		return andBool;
	}

	private MapOrBoolQuery assOrBoolQuery(String key, List<Object> values) {
		MapOrBoolQuery orBool = new MapOrBoolQuery();
		orBool.setKey(key);
		orBool.setValues(values);
		return orBool;
	}

	private MapExistQuery assignExistsQuery(String key, Boolean values, String path) {
		MapExistQuery existQuery = new MapExistQuery();
		existQuery.setKey(key);
		existQuery.setExists(values);
		existQuery.setPath(path);
		return existQuery;
	}

	private MapAndMatchPhraseQuery assignAndMatchPhrase(String key, String value) {
		MapAndMatchPhraseQuery andMatchPhrase = new MapAndMatchPhraseQuery();
		andMatchPhrase.setKey(key);
		andMatchPhrase.setValue(value);
		return andMatchPhrase;
	}

	private MapOrMatchPhraseQuery assignOrMatchPhrase(String key, String value) {
		MapOrMatchPhraseQuery orMatchPhrase = new MapOrMatchPhraseQuery();
		orMatchPhrase.setKey(key);
		orMatchPhrase.setValue(value);
		return orMatchPhrase;
	}

	private MapAndRangeQuery assignAndRange(String key, Object start, Object end, String path) {
		MapAndRangeQuery andRange = new MapAndRangeQuery();
		andRange.setKey(key);
		andRange.setStart(start);
		andRange.setEnd(end);
		andRange.setPath(path);
		return andRange;
	}

	private MapOrRangeQuery assignOrRange(String key, Object start, Object end) {
		MapOrRangeQuery orRange = new MapOrRangeQuery();
		orRange.setKey(key);
		orRange.setStart(start);
		orRange.setEnd(end);
		return orRange;
	}

	public MapSearchQuery getMapSearchQuery(String sGroup, String taxon, String user, String userGroupList,
			String webaddress, String speciesName, String mediaFilter, String months, String isFlagged, String minDate,
			String maxDate, String validate, Map<String, List<String>> traitParams,
			Map<String, List<String>> customParams, String classificationid, MapSearchParams mapSearchParams,
			String maxvotedrecoid, String recoId, String createdOnMaxDate, String createdOnMinDate, String status,
			String taxonId, String recoName, String rank, String tahsil, String district, String state, String tags,
			String publicationGrade, String authorVoted, String dataSetName, String dataTableName, String geoEntity,
			String dataTableId) {

		List<MapAndBoolQuery> boolAndLists = new ArrayList<>();
		List<MapOrBoolQuery> boolOrLists = new ArrayList<>();
		List<MapOrRangeQuery> rangeOrLists = new ArrayList<>();
		List<MapAndRangeQuery> rangeAndLists = new ArrayList<>();
		List<MapExistQuery> andMapExistQueries = new ArrayList<>();
		List<MapAndMatchPhraseQuery> andMatchPhraseQueries = new ArrayList<>();
		List<MapOrMatchPhraseQuery> orMatchPhraseQueriesnew = new ArrayList<>();

		try {
			FilterPanelData esFilter = esService.getFilterLists(ObservationIndex.INDEX.getValue(),
					ObservationIndex.TYPE.getValue());

			// Species group
			List<Object> groupId = cSTSOT(sGroup);
			if (!groupId.isEmpty()) {
				boolAndLists.add(assignBoolAndQuery(ObservationIndex.SGROUP.getValue(), groupId));
			}

			// Taxon browser
			List<Object> taxonIds = cSTSOT(taxon);
			if (!taxonIds.isEmpty()) {
				boolAndLists.add(assignBoolAndQuery(ObservationIndex.PATH.getValue(), taxonIds));
			}

			// Name :- Reco name
			if (recoName != null) {
				orMatchPhraseQueriesnew
						.add(assignOrMatchPhrase(ObservationIndex.SCIENTIFICNAME.getValue(), recoName.toLowerCase()));
				orMatchPhraseQueriesnew
						.add(assignOrMatchPhrase(ObservationIndex.COMMONNAME.getValue(), recoName.toLowerCase()));
			}

			// Status
			List<Object> taxonStatus = cSTSOT(status);
			if (!taxonStatus.isEmpty()) {
				boolAndLists.add(assignBoolAndQuery(ObservationIndex.STATUS.getValue(), taxonStatus));
			}

			// Taxon Id / Status Exists checks
			List<Object> taxonIdsArray = cSTSOT(taxonId);
			if (!taxonIdsArray.isEmpty() && taxonIdsArray.size() < 2) {
				String first = (String) taxonIdsArray.get(0);
				if (first.equalsIgnoreCase("0")) {
					andMapExistQueries.add(assignExistsQuery(ObservationIndex.STATUS.getValue(), false, null));
				}
				if (first.equalsIgnoreCase("1")) {
					andMapExistQueries.add(assignExistsQuery(ObservationIndex.STATUS.getValue(), true, null));
				}
			}

			// Rank, Tahsil, District, State, Tags (Lowercase normalization)
			processLowercaseAndBool(boolAndLists, ObservationIndex.RANK.getValue(), rank);
			processLowercaseAndBool(boolAndLists, ObservationIndex.TAHSIL.getValue(), tahsil);
			processLowercaseAndBool(boolAndLists, ObservationIndex.DISTRICT.getValue(), district);
			processLowercaseAndBool(boolAndLists, ObservationIndex.STATE.getValue(), state);
			processLowercaseAndBool(boolAndLists, ObservationIndex.TAGS.getValue(), tags);

			// User Group
			List<Object> userGroupId = cSTSOT(userGroupList);
			if (!userGroupId.isEmpty()) {
				boolAndLists.add(assignBoolAndQuery(ObservationIndex.USERGROUPID.getValue(), userGroupId));
			}

			// Data Quality :- Identification (Using null for infinity)
			List<Object> speciesNames = cSTSOT(speciesName);
			if (!speciesNames.isEmpty() && speciesNames.size() < 2) {
				String first = (String) speciesNames.get(0);
				if (first.equalsIgnoreCase("UNIDENTIFIED")) {
					rangeAndLists.add(assignAndRange(ObservationIndex.NOOFIDENTIFICATION.getValue(), 0, 0, null));
				}
				if (first.equalsIgnoreCase("IDENTIFIED")) {
					rangeAndLists.add(assignAndRange(ObservationIndex.NOOFIDENTIFICATION.getValue(), 1, null, null));
				}
			}

			// Data Quality :- Flagged (Using null for infinity)
			List<Object> flagged = cSTSOT(isFlagged);
			if (!flagged.isEmpty() && flagged.size() < 2) {
				String first = (String) flagged.get(0);
				if (first.equalsIgnoreCase("1")) {
					rangeAndLists.add(assignAndRange(ObservationIndex.FLAGCOUNT.getValue(), first, null, null));
				}
				if (first.equalsIgnoreCase("0")) {
					rangeAndLists.add(assignAndRange(ObservationIndex.FLAGCOUNT.getValue(), first, first, null));
				}
			}

			// Data Quality :- Validate
			List<Object> validates = cSTSOT(validate);
			if (!validates.isEmpty() && validates.size() < 2) {
				String first = (String) validates.get(0);
				List<Object> data = new ArrayList<>();
				data.add(first.equalsIgnoreCase("validate") ? "true" : "false");
				boolAndLists.add(assignBoolAndQuery(ObservationIndex.ISLOCKED.getValue(), data));
			}

			// Author
			List<Object> authorId = cSTSOT(user);
			if (!authorId.isEmpty()) {
				boolAndLists.add(assignBoolAndQuery(ObservationIndex.AUTHORID.getValue(), authorId));
			}

			// Media type (Using null for infinity)
			List<Object> mediaFilters = cSTSOT(mediaFilter);
			if (!mediaFilters.isEmpty()) {
				for (Object f : mediaFilters) {
					rangeOrLists.add(assignOrRange((String) f, 1, null));
				}
			}

			// Dates (Observed On / Created On)
			handleDateRanges(rangeAndLists, ObservationIndex.FROMDATE.getValue(), minDate, maxDate);
			handleDateRanges(rangeAndLists, ObservationIndex.CREATEDON.getValue(), createdOnMinDate, createdOnMaxDate);

			// Seasonal Months
			List<Object> month = cSTSOT(months);
			if (!month.isEmpty()) {
				boolAndLists.add(assignBoolAndQuery(ObservationIndex.OBSERVATIONMONTH.getValue(), month));
			}

			// Traits Filter
			processTraits(traitParams, esFilter, boolAndLists, rangeAndLists);

			// Custom Field Filter
			processCustomFields(customParams, esFilter, boolAndLists, rangeAndLists, andMatchPhraseQueries);

			// General conditions
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.ISCHECKLIST.getValue(), cSTSOT("false")));

			if (webaddress != null && !webaddress.isEmpty()) {
				boolAndLists.add(assignBoolAndQuery("usergroupname", cSTSOT(webaddress)));
			}

			addSimpleAndBool(boolAndLists, ObservationIndex.MAXVTEDRECO.getValue(), maxvotedrecoid);
			addSimpleAndBool(boolAndLists, ObservationIndex.RECOID.getValue(), recoId);
			addSimpleAndBool(boolAndLists, ObservationIndex.AUTHORVOTED.getValue(), authorVoted);
			addSimpleAndBool(boolAndLists, ObservationIndex.PUBLICATIONGRADE.getValue(), publicationGrade);
			addSimpleAndBool(boolAndLists, ObservationIndex.GEOENTITY.getValue(), geoEntity);
			addSimpleAndBool(boolAndLists, ObservationIndex.DATATABLEID.getValue(), dataTableId);

			addOrMatchPhraseList(orMatchPhraseQueriesnew, ObservationIndex.DATASETNAME.getValue(), dataSetName);
			addOrMatchPhraseList(orMatchPhraseQueriesnew, ObservationIndex.DATATABLENAME.getValue(), dataTableName);

			// Combine all queries
			MapSearchQuery mapSearchQuery = new MapSearchQuery();
			mapSearchQuery.setAndBoolQueries(boolAndLists);
			mapSearchQuery.setOrBoolQueries(boolOrLists);
			mapSearchQuery.setAndRangeQueries(rangeAndLists);
			mapSearchQuery.setOrRangeQueries(rangeOrLists);
			mapSearchQuery.setAndExistQueries(andMapExistQueries);
			mapSearchQuery.setAndMatchPhraseQueries(andMatchPhraseQueries);
			mapSearchQuery.setOrMatchPhraseQueries(orMatchPhraseQueriesnew);
			mapSearchQuery.setSearchParams(mapSearchParams);

			return mapSearchQuery;

		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	private void processLowercaseAndBool(List<MapAndBoolQuery> list, String key, String rawValues) {
		List<Object> values = cSTSOT(rawValues);
		if (!values.isEmpty()) {
			List<Object> lowerCaseList = values.stream().map(o -> o.toString().toLowerCase())
					.collect(Collectors.toList());
			list.add(assignBoolAndQuery(key, lowerCaseList));
		}
	}

	private void addSimpleAndBool(List<MapAndBoolQuery> list, String key, String rawValues) {
		List<Object> values = cSTSOT(rawValues);
		if (!values.isEmpty()) {
			list.add(assignBoolAndQuery(key, values));
		}
	}

	private void addOrMatchPhraseList(List<MapOrMatchPhraseQuery> list, String key, String rawValues) {
		List<Object> values = cSTSOT(rawValues);
		values.forEach(item -> list.add(assignOrMatchPhrase(key, item.toString())));
	}

	private void handleDateRanges(List<MapAndRangeQuery> list, String key, String min, String max) {
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
		String today = sdf.format(new Date());
		if (min != null || max != null) {
			list.add(assignAndRange(key, (min != null ? min : today), (max != null ? max : today), null));
		}
	}

	private void processTraits(Map<String, List<String>> traitParams, FilterPanelData esFilter,
			List<MapAndBoolQuery> boolAndLists, List<MapAndRangeQuery> rangeAndLists) {
		for (Entry<String, List<String>> entry : traitParams.entrySet()) {
			try {
				String type = entry.getKey().split("\\.")[1];
				String key = entry.getKey().split("\\.")[0];
				String values = entry.getValue().get(0);

				if (type.equalsIgnoreCase("string")) {
					String traitId = key.split("_")[1];
					for (Traits t : esFilter.getTraits()) {
						if (t.getId().toString().equals(traitId)) {
							List<Object> traitValueList = cSTSOT(values).stream()
									.map(o -> t.getName() + "|" + traitId + "|" + o.toString())
									.collect(Collectors.toList());
							boolAndLists.add(
									assignBoolAndQuery(ObservationIndex.TRAITSAGGREGATION.getValue(), traitValueList));
							break;
						}
					}
				} else if (type.equalsIgnoreCase("season")) {
					String[] parts = values.split(",");
					if (parts.length >= 2) {
						rangeAndLists.add(assignAndRange("traits_season." + key, parts[0].replace('Z', ' '),
								parts[1].replace('Z', ' '), null));
					}
				} else if (type.equalsIgnoreCase("range")) {
					List<Long> range = getListOfIds(values);
					if (range.size() >= 2) {
						rangeAndLists.add(assignAndRange("traits." + key, range.get(0), range.get(1), null));
					}
				}
			} catch (Exception e) {
				logger.error("Trait error: {}", e.getMessage());
			}
		}
	}

	private void processCustomFields(Map<String, List<String>> customParams, FilterPanelData esFilter,
			List<MapAndBoolQuery> boolAndLists, List<MapAndRangeQuery> rangeAndLists,
			List<MapAndMatchPhraseQuery> andMatchPhraseQueries) {
		for (Entry<String, List<String>> entry : customParams.entrySet()) {
			try {
				String key = entry.getKey();
				String cfId = key.split("\\.")[0].split("_")[1];
				String fieldType = key.split("\\.")[1];

				for (CustomFields cf : esFilter.getCustomFields()) {
					if (cf.getId().toString().equals(cfId)) {
						String value = entry.getValue().get(0);
						if (fieldType.equalsIgnoreCase("field_content")
								|| fieldType.toLowerCase().contains("categorical")) {
							List<Object> valueObjects = cSTSOT(value);
							List<Object> valueList = new ArrayList<>();
							for (Object o : valueObjects) {
								valueList.add(cf.getId() + "|" + cf.getName() + "|" + cf.getFieldtype() + "|"
										+ cf.getDataType() + "|" + o.toString());
							}
							boolAndLists.add(
									assignBoolAndQuery(ObservationIndex.CUSTOMFIELDAGGREGATION.getValue(), valueList));
						} else if (fieldType.equalsIgnoreCase("field_text")) {
							String phrase = cfId + "|.*" + value.toLowerCase() + ".*";
							andMatchPhraseQueries
									.add(assignAndMatchPhrase(ObservationIndex.CUSTOMFIELDIDVALUE.getValue(), phrase));
						} else if (fieldType.equalsIgnoreCase("range")) {
							String[] range = value.split("-");
							boolAndLists
									.add(assignBoolAndQuery(ObservationIndex.CUSTOMFIELDID.getValue(), cSTSOT(cfId)));
							// Use null for unbounded sides
							rangeAndLists.add(assignAndRange(ObservationIndex.CUSTOMFIELDRANGEMINVALUE.getValue(),
									range[0], null, null));
							rangeAndLists.add(assignAndRange(ObservationIndex.CUSTOMFIELDRANGEMAXVALUE.getValue(), null,
									range[1], null));
						}
					}
				}
			} catch (Exception e) {
				logger.error("Custom field error: {}", e.getMessage());
			}
		}
	}

	public MapSearchQuery getSearchQueryResource(String resourcesUrl) {
		List<MapOrBoolQuery> boolOrLists = new ArrayList<>();
		boolOrLists.add(assOrBoolQuery(ObservationIndex.RESOURCE.getValue(), cSTSOT(resourcesUrl)));

		MapSearchQuery mapSearchQuery = new MapSearchQuery();
		MapSearchParams searchParams = new MapSearchParams();
		searchParams.setFrom(0);
		searchParams.setLimit(50);
		searchParams.setSortOn(ObservationIndex.CREATEDON.getValue());
		searchParams.setSortType(SortTypeEnum.DESC);

		mapSearchQuery.setSearchParams(searchParams);
		mapSearchQuery.setOrBoolQueries(boolOrLists);
		return mapSearchQuery;
	}

	public List<MapGeoPoint> polygonGenerator(String locationArray) {
		List<MapGeoPoint> polygon = new ArrayList<>();
		double[] points = Stream.of(locationArray.split(",")).mapToDouble(Double::parseDouble).toArray();
		for (int i = 0; i < points.length; i = i + 2) {
			MapGeoPoint geoPoint = new MapGeoPoint();
			geoPoint.setLat(points[i + 1]);
			geoPoint.setLon(points[i]);
			polygon.add(geoPoint);
		}
		return polygon;
	}

	public List<List<MapGeoPoint>> multiPolygonGenerator(String[] locationArray) {
		List<List<MapGeoPoint>> multipolygon = new ArrayList<>();
		for (String geoPoint : locationArray) {
			multipolygon.add(polygonGenerator(geoPoint));
		}
		return multipolygon;
	}
}