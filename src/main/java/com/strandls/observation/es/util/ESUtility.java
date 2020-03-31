/**
 * 
 */
package com.strandls.observation.es.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.esmodule.pojo.MapAndBoolQuery;
import com.strandls.esmodule.pojo.MapAndMatchPhraseQuery;
import com.strandls.esmodule.pojo.MapAndRangeQuery;
import com.strandls.esmodule.pojo.MapExistQuery;
import com.strandls.esmodule.pojo.MapOrBoolQuery;
import com.strandls.esmodule.pojo.MapOrMatchPhraseQuery;
import com.strandls.esmodule.pojo.MapOrRangeQuery;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchParams.SortTypeEnum;
import com.strandls.esmodule.pojo.MapSearchQuery;

/**
 * @author Abhishek Rudra
 *
 */
public class ESUtility {

	private final Logger logger = LoggerFactory.getLogger(ESUtility.class);

	private List<Object> cSTSOT(String str) {
		if (str == null || str == "" || str.isEmpty())
			return new ArrayList<Object>();

		String[] y = str.split(",");
		Set<Object> strSet1 = Arrays.stream(y).collect(Collectors.toSet());
		List<Object> strList = new ArrayList<Object>();
		strList.addAll(strSet1);
		return strList;

	}

	private boolean isParsableAsLong(final String s) {
		try {
			Long.valueOf(s);
			return true;
		} catch (NumberFormatException numberFormatException) {
			return false;
		}
	}

	private List<Long> getListOfIds(String str) {
		if (str == null || str == "" || str.isEmpty())
			return new ArrayList<Long>();
		String[] y = str.split(",");
		List<Long> LongIds = new ArrayList<>();
		for (String z : y) {
			if (isParsableAsLong(z)) {
				LongIds.add(Long.parseLong(z));
			} else {
				LongIds.add(0L);
			}
		}

		return LongIds;
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
			String maxvotedrecoid, String createdOnMaxDate, String createdOnMinDate, String status, String taxonId,
			String recoName, String rank, String tahsil, String district, String state, String tags) {

		List<MapAndBoolQuery> boolAndLists = new ArrayList<MapAndBoolQuery>();
		List<MapOrBoolQuery> boolOrLists = new ArrayList<MapOrBoolQuery>();
		List<MapOrRangeQuery> rangeOrLists = new ArrayList<MapOrRangeQuery>();
		List<MapAndRangeQuery> rangeAndLists = new ArrayList<MapAndRangeQuery>();
		List<MapExistQuery> andMapExistQueries = new ArrayList<MapExistQuery>();
		List<MapAndMatchPhraseQuery> andMatchPhraseQueries = new ArrayList<MapAndMatchPhraseQuery>();
		List<MapOrMatchPhraseQuery> orMatchPhraseQueriesnew = new ArrayList<MapOrMatchPhraseQuery>();

//		species group
		List<Object> groupId = cSTSOT(sGroup);
		if (!groupId.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.sGroup.getValue(), groupId));
		}

//		taxon browser
		List<Object> taxonIds = cSTSOT(taxon);
		if (!taxonIds.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.path.getValue(), taxonIds));
		}

//		name :- reco name
		if (recoName != null) {
			orMatchPhraseQueriesnew
					.add(assignOrMatchPhrase(ObservationIndex.scientificName.getValue(), recoName.toLowerCase()));
			orMatchPhraseQueriesnew
					.add(assignOrMatchPhrase(ObservationIndex.commonName.getValue(), recoName.toLowerCase()));
		}

//		name :- status
		List<Object> taxonStatus = cSTSOT(status);
		if (!taxonStatus.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.status.getValue(), taxonStatus));
		}

//		name :- taxon Id
		List<Object> taxonIdsArray = cSTSOT(taxonId);
		if (!taxonIdsArray.isEmpty()) {
			if (taxonIdsArray.size() < 2) {
				String first = (String) taxonIdsArray.toArray()[0];

				if (first.equalsIgnoreCase("0")) {
					andMapExistQueries.add(assignExistsQuery(ObservationIndex.status.getValue(), false, null));
				}
				if (first.equalsIgnoreCase("1")) {
					andMapExistQueries.add(assignExistsQuery(ObservationIndex.status.getValue(), true, null));
				}
			}

		}

//		Rank
		List<Object> rankList = cSTSOT(rank);
		if (!rankList.isEmpty()) {
			List<Object> lowerCaseList = new ArrayList<Object>();
			for (Object o : rankList) {
				String result = o.toString().toLowerCase();
				lowerCaseList.add(result);
			}
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.rank.getValue(), lowerCaseList));
		}

//		tahsil
		List<Object> tahsilList = cSTSOT(tahsil);
		if (!tahsilList.isEmpty()) {
			List<Object> lowerCaseList = new ArrayList<Object>();
			for (Object o : tahsilList) {
				String result = o.toString().toLowerCase();
				lowerCaseList.add(result);
			}
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.tahsil.getValue(), lowerCaseList));
		}

//		district
		List<Object> districtList = cSTSOT(district);
		if (!tahsilList.isEmpty()) {
			List<Object> lowerCaseList = new ArrayList<Object>();
			for (Object o : districtList) {
				String result = o.toString().toLowerCase();
				lowerCaseList.add(result);
			}
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.district.getValue(), lowerCaseList));
		}

//		state
		List<Object> stateList = cSTSOT(state);
		if (!stateList.isEmpty()) {
			List<Object> lowerCaseList = new ArrayList<Object>();
			for (Object o : stateList) {
				String result = o.toString().toLowerCase();
				lowerCaseList.add(result);
			}
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.state.getValue(), lowerCaseList));
		}

//		tags
		List<Object> tagsList = cSTSOT(tags);
		if (!tagsList.isEmpty()) {
			List<Object> lowerCaseList = new ArrayList<Object>();
			for (Object o : tagsList) {
				String result = o.toString().toLowerCase();
				lowerCaseList.add(result);
			}
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.tags.getValue(), lowerCaseList));

		}

//		user Group
		List<Object> userGroupId = cSTSOT(userGroupList);
		if (!userGroupId.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.userGroupId.getValue(), userGroupId));
		}

//		Data Quality :- Identification
		List<Object> speciesNames = cSTSOT(speciesName);
		if (!speciesNames.isEmpty()) {
			if (speciesNames.size() < 2) {
				String first = (String) speciesNames.toArray()[0];
				if (first.equalsIgnoreCase("UNIDENTIFED")) {
					rangeAndLists.add(assignAndRange(ObservationIndex.no_of_identifications.getValue(), 0, 0, null));
				}
				if (first.equalsIgnoreCase("IDENTIFED")) {
					rangeAndLists.add(
							assignAndRange(ObservationIndex.no_of_identifications.getValue(), 1, Long.MAX_VALUE, null));
				}
			}

		}

//		Data Quality:- Flagged
		List<Object> flagged = cSTSOT(isFlagged);
		if (!flagged.isEmpty()) {

			if (flagged.size() < 2) {
				String first = (String) flagged.toArray()[0];
				if (first.equalsIgnoreCase("1")) {
					rangeAndLists
							.add(assignAndRange(ObservationIndex.flagCount.getValue(), first, Long.MAX_VALUE, null));
				}
				if (first.equalsIgnoreCase("0")) {
					rangeAndLists.add(assignAndRange(ObservationIndex.flagCount.getValue(), first, first, null));
				}

			}
		}

//		Data Quality:- Validate
		List<Object> validates = cSTSOT(validate);
		if (!validates.isEmpty()) {
			if (validates.size() < 2) {
				List<Object> data = new ArrayList<Object>();
				String first = (String) validates.toArray()[0];
				if (first.equalsIgnoreCase("invalidate")) {
					data.add("false");
					boolAndLists.add(assignBoolAndQuery(ObservationIndex.isLocked.getValue(), data));
				}
				if (first.equalsIgnoreCase("validate")) {
					data.add("true");
					boolAndLists.add(assignBoolAndQuery(ObservationIndex.isLocked.getValue(), data));
				}
			}

		}

//		user
		List<Object> authorId = cSTSOT(user);
		if (!authorId.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.authorid.getValue(), authorId));
		}

//		media type
		List<Object> mediaFilters = cSTSOT(mediaFilter);
		if (!mediaFilters.isEmpty()) {
			// remove no media value
			for (Object filter : mediaFilters) {
				rangeOrLists.add(assignOrRange((String) filter, 1, Long.MAX_VALUE));
			}

		}

//		Observed On 
		String minDateValue = null;
		String maxDateValue = null;
		Date date = new Date();
		SimpleDateFormat out = new SimpleDateFormat("YYYY-MM-dd");
		try {
			if (minDate != null) {
				minDateValue = minDate;
			}
			if (maxDate != null) {
				maxDateValue = maxDate;
			}
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		if (minDateValue != null && maxDateValue != null) {
			rangeAndLists.add(assignAndRange(ObservationIndex.fromDate.getValue(), minDateValue, maxDateValue, null));
		}
		if (minDateValue != null && maxDateValue == null) {
			rangeAndLists
					.add(assignAndRange(ObservationIndex.fromDate.getValue(), minDateValue, out.format(date), null));
		}
		if (minDateValue == null && maxDateValue != null) {
			rangeAndLists
					.add(assignAndRange(ObservationIndex.fromDate.getValue(), out.format(date), maxDateValue, null));
		}

//		Created on
		String createdOnMaxDateValue = null;
		String createdOnMinDateValue = null;
		try {
			if (createdOnMinDate != null) {
				createdOnMinDateValue = createdOnMinDate;
			}
			if (createdOnMaxDate != null) {
				createdOnMaxDateValue = createdOnMaxDate;
			}
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		if (createdOnMinDateValue != null && createdOnMaxDateValue != null) {

			rangeAndLists.add(assignAndRange(ObservationIndex.createdOn.getValue(), createdOnMinDateValue,
					createdOnMaxDateValue, null));
		}
		if (createdOnMinDateValue != null && createdOnMaxDateValue == null) {
			rangeAndLists.add(assignAndRange(ObservationIndex.createdOn.getValue(), createdOnMinDateValue,
					out.format(date), null));
		}
		if (createdOnMinDateValue == null && createdOnMaxDateValue != null) {
			rangeAndLists.add(assignAndRange(ObservationIndex.createdOn.getValue(), out.format(date),
					createdOnMaxDateValue, null));
		}

//		seasonal
		List<Object> month = cSTSOT(months);
		if (!month.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.ObservedOnMonth.getValue(), month));

		}

//		Traits filter
		if (!traitParams.isEmpty()) {
			for (Map.Entry<String, List<String>> entry : traitParams.entrySet()) {
				try {
					String value = entry.getKey().split("\\.")[1];

					if (value.equalsIgnoreCase("string")) {
						String key = entry.getKey().split("\\.")[0];
						String Ids = entry.getValue().get(0);
						System.out.println(Ids);
						List<Object> traitIdList = new ArrayList<Object>();
						traitIdList.add(key.split("_")[1]);
						List<Object> listOfIds = cSTSOT(Ids);
						List<Object> lowerCaseList = new ArrayList<Object>();
						for (Object o : listOfIds) {
							String result = o.toString().toLowerCase();
							lowerCaseList.add(result);
						}
						boolAndLists.add(assignBoolAndQuery(ObservationIndex.traitId.getValue(), traitIdList));
						boolAndLists.add(assignBoolAndQuery(ObservationIndex.traitValue.getValue(), lowerCaseList));
					}

					if (value.equalsIgnoreCase("season")) {
						String key = entry.getKey().split("\\.")[0];
						String Ids = entry.getValue().get(0);
						System.out.println(Ids);
						String[] y = Ids.split(",");

						try {
							Date minSeasonDate = new SimpleDateFormat("yyyy-MM-dd").parse(y[0]);
							Date maxSeasonDate = new SimpleDateFormat("yyyy-MM-dd").parse(y[1]);
							Calendar cal = Calendar.getInstance();
							cal.setTime(minSeasonDate);
							int minMonth = cal.get(Calendar.MONTH);
							int minDay = cal.get(Calendar.DATE);
							cal.setTime(maxSeasonDate);
							int maxMonth = cal.get(Calendar.MONTH);
							int maxDay = cal.get(Calendar.DATE);
							if (minMonth == 0 && minDay == 1 && maxMonth == 11 && maxDay == 31) {

							} else {

								rangeAndLists.add(assignAndRange("traits_season." + key, y[0].replace('Z', ' '),
										y[1].replace('Z', ' '), null));
							}
						} catch (ParseException e) {
							logger.error(e.getMessage());
						}

						key = "traits." + key;

					}

					if (value.equalsIgnoreCase("color_hsl")) {

						String key = entry.getKey().split("\\.")[0];

						String Ids = entry.getValue().get(0);
						List<Long> listOfIds = getListOfIds(Ids);
						Long h;
						Long s;
						Long l;
						Long hMax;
						Long hMin;
						Long sMax;
						Long sMin;
						Long lMax;
						Long lMin;

						if (listOfIds.size() >= 3) {

							h = listOfIds.get(0);
							hMax = h + 5L;
							if (h - 5L < 0) {
								hMin = 0L;
							} else {
								hMin = h - 5L;
							}

							s = listOfIds.get(1);

							sMax = s + 5L;
							if (s - 5L < 0) {
								sMin = 0L;
							} else {
								sMin = s - 5L;
							}

							l = listOfIds.get(2);
							lMax = l + 5L;
							if (l - 5L < 0) {
								lMin = 0L;
							} else {
								lMin = l - 5L;
							}

							rangeAndLists
									.add(assignAndRange("traits_json." + key + ".h", hMin, hMax, "traits_json." + key));
							rangeAndLists
									.add(assignAndRange("traits_json." + key + ".s", sMin, sMax, "traits_json." + key));
							rangeAndLists
									.add(assignAndRange("traits_json." + key + ".l", lMin, lMax, "traits_json." + key));
						}
					}
					if (value.equalsIgnoreCase("range")) {
						String key = entry.getKey().split("\\.")[0];
						String Ids = entry.getValue().get(0);
						List<Long> listOfIds = getListOfIds(Ids);
						Long rMin;
						Long rMax;
						if (listOfIds.size() >= 2) {
							rMin = listOfIds.get(0);
							rMax = listOfIds.get(1);
							rangeAndLists.add(assignAndRange("traits." + key, rMin, rMax, null));
						}

					}

				} catch (ArrayIndexOutOfBoundsException e) {
					logger.error(e.getMessage());
				}

			}
		}

//		custom Field code to be implemented

		if (!customParams.isEmpty()) {

			for (Entry<String, List<String>> entry : customParams.entrySet()) {
				try {
					String key = entry.getKey();
					String cfId = key.split("\\.")[0].split("_")[1];
					List<Object> cfidObject = cSTSOT(cfId);
					String fieldType = key.split("\\.")[1];
					boolAndLists.add(assignBoolAndQuery(ObservationIndex.customFieldId.getValue(), cfidObject));
					if (fieldType.equalsIgnoreCase("field_text")) {
//						 match phrase
						String value = entry.getValue().get(0);
						andMatchPhraseQueries.add(assignAndMatchPhrase(ObservationIndex.customFieldTextValue.getValue(),
								value.toLowerCase()));

					}
					if (fieldType.equalsIgnoreCase("single_categorical")) {
//						 bool and query
						String value = entry.getValue().get(0);
						List<Object> valueObject = cSTSOT(value);
						boolAndLists.add(assignBoolAndQuery(
								ObservationIndex.customFieldSingleCategoricalValue.getValue(), valueObject));
					}
					if (fieldType.equalsIgnoreCase("multiple_categorical")) {
//						bool and query
						String value = entry.getValue().get(0);
						List<Object> valueObject = cSTSOT(value);
						boolAndLists.add(assignBoolAndQuery(
								ObservationIndex.customFieldMultipleCategoricalValue.getValue(), valueObject));

					}
					if (fieldType.equalsIgnoreCase("Range")) {
//						 range and query
						String value = entry.getValue().get(0);
						String values[] = value.split("-");
						rangeAndLists.add(assignAndRange(ObservationIndex.customFieldRangeMinValue.getValue(),
								values[0], Long.MAX_VALUE, null));
						rangeAndLists.add(assignAndRange(ObservationIndex.customFieldRangeMaxValue.getValue(),
								Long.MIN_VALUE, values[1], null));
					}
					if (fieldType.equalsIgnoreCase("field_content")) {
//						 exist query
						String value = entry.getValue().get(0);
						List<Object> valueObject = cSTSOT(value);
						if (valueObject.size() < 2) {
							String first = (String) valueObject.toArray()[0];
							if (first.equalsIgnoreCase("0")) {
								andMapExistQueries.add(assignExistsQuery(
										ObservationIndex.customFieldTextValue.getValue(), false, null));
							} else {
								andMapExistQueries.add(assignExistsQuery(
										ObservationIndex.customFieldTextValue.getValue(), true, null));
							}
						}
					}
					if (fieldType.equalsIgnoreCase("categorical_content")
							|| fieldType.equalsIgnoreCase("range_content")) {
//						exist query
						String value = entry.getValue().get(0);
						List<Object> valueObject = cSTSOT(value);
						if (valueObject.size() < 2) {
							String first = (String) valueObject.toArray()[0];
							if (first.equalsIgnoreCase("0")) {
								andMapExistQueries.add(assignExistsQuery(
										ObservationIndex.customFieldAggregation.getValue(), false, null));
							} else {
								andMapExistQueries.add(assignExistsQuery(
										ObservationIndex.customFieldAggregation.getValue(), true, null));
							}

						}

					}

				} catch (Exception e) {
					logger.error(e.getMessage());
				}
			}
		}

		/**
		 * General conditions
		 * 
		 * General condition
		 */

		String isCheckList = "false";
		List<Object> ischecklist = cSTSOT(isCheckList);
		if (!ischecklist.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery(ObservationIndex.isChecklist.getValue(), ischecklist));
		}

//		Unknown Checks		
		List<Object> userGroupName = cSTSOT(webaddress);
		if (!userGroupName.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery("usergroupname", userGroupName));

		}

//		unknown checks
		List<Object> maxvotedrecoids = cSTSOT(maxvotedrecoid);
		if (!maxvotedrecoids.isEmpty()) {
			boolAndLists.add(assignBoolAndQuery("maxvotedrecoid", maxvotedrecoids));
		}

		/**
		 * combine all the queries
		 * 
		 */
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
	}

	public MapSearchQuery getSearchQueryResource(String resourcesUrl) {
		List<MapOrBoolQuery> boolOrLists = new ArrayList<MapOrBoolQuery>();
		List<Object> values = cSTSOT(resourcesUrl);
		boolOrLists.add(assOrBoolQuery(ObservationIndex.resource.getValue(), values));
		MapSearchQuery mapSearchQuery = new MapSearchQuery();
		
		MapSearchParams searchParams = new MapSearchParams();
		MapSearchParams mapSearchParams = new MapSearchParams();
		mapSearchParams.setFrom(0);
		mapSearchParams.setLimit(50);
		mapSearchParams.setSortOn(ObservationIndex.createdOn.getValue());
		mapSearchParams.setSortType(SortTypeEnum.DESC);
		mapSearchParams.setMapBoundParams(null);
		
		mapSearchQuery.setSearchParams(searchParams);
		mapSearchQuery.setOrBoolQueries(boolOrLists);
		return mapSearchQuery;
	}
}
