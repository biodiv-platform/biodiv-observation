package com.strandls.observation.service.Impl;

import javax.inject.Inject;
import com.strandls.observation.dao.DataSetDAO;
import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.pojo.DataTable;
import com.strandls.observation.pojo.Dataset;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DataTableHelper {

	@Inject
	private DataSetDAO datasetDao;

	private final Logger logger = LoggerFactory.getLogger(DataTableHelper.class);

	public DataTable createDataTable(ObservationBulkDTO observationBulkData, Long userId, String jwtToken) {
		DataTable dataTable = new DataTable();
		dataTable.setAccessLicenseId(observationBulkData.getLicenseId());
		dataTable.setAccessRights(null);
		dataTable.setAgreeTerms(true);
		dataTable.setChecklistId(null);
		dataTable.setColumns(new ArrayList<String>().toString()); // ask
		dataTable.setCreatedOn(observationBulkData.getCreatedOn());
		dataTable.setCustomFields(new HashMap<String, String>().toString()); // ask
		dataTable.setDataTableType("OBSERVATIONS");
		dataTable.setDeleted(false);
		dataTable.setDescription(observationBulkData.getDescription());
		dataTable.setExternalId(null);
		dataTable.setExternalUrl(null);
		dataTable.setFeatureCount(0);
		dataTable.setFlagCount(0);

		// geo fields
		dataTable.setGeographicalCoverageGeoPrivacy(false);
		dataTable.setGeographicalCoverageLatitude(observationBulkData.getLatitude());
		dataTable.setGeographicalCoverageLongitude(observationBulkData.getLongitude());
		dataTable.setGeographicalCoverageLocationAccuracy(observationBulkData.getLocationAccuracy());
		dataTable.setGeographicalCoverageLocationScale(observationBulkData.getLocationScale());
		dataTable.setGeographicalCoveragePlaceName(observationBulkData.getObservedAt());

		GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);
		WKTReader wktRdr = new WKTReader(geofactory);
		if (!observationBulkData.getWktString().isEmpty()) {
			try {
				Geometry geoBoundary = wktRdr.read(observationBulkData.getWktString());
				dataTable.setGeographicalCoverageTopology(geoBoundary);
			} catch (ParseException e) {
				createPointTopology(geofactory, observationBulkData, dataTable);
				logger.error(e.getMessage());
			}

		} else {
			createPointTopology(geofactory, observationBulkData, dataTable);

		}

		Dataset dataset = datasetDao.findDataSetByTitle("standalone_dataset");
		Long datasetid = observationBulkData.getDataset() != null ? observationBulkData.getDataset() : dataset.getId();
		dataTable.setDatasetId(datasetid);
		dataTable.setImagesFileId(null);
		dataTable.setLanguageId(205L);
		dataTable.setLastRevised(observationBulkData.getCreatedOn());
		dataTable.setMethods(observationBulkData.getMethods());
		dataTable.setPartyAttributions(observationBulkData.getAttribution());
		dataTable.setPartyUploaderId(userId);
		dataTable.setPartyContributorId(observationBulkData.getContributors()); // only one contributor
		dataTable.setProject(observationBulkData.getProject());
		dataTable.setRating(0);
		dataTable.setSummary(observationBulkData.getSummary());
		dataTable.setTaxonomicCoverageGroupIds(observationBulkData.getSGroup());
		dataTable.setTemporalCoverageDateAccuracy(observationBulkData.getDateAccuracy());
		dataTable.setTemporalCoverageFromDate(observationBulkData.getObservedFromDate());
		dataTable.setTemporalCoverageToDate(observationBulkData.getObservedToDate());
		dataTable.setTitle(observationBulkData.getTitle());
		dataTable.setTraitValueFileId(null);
		dataTable.setuFileId(1L); // uFile table id
		dataTable.setVersion(2L);
		dataTable.setViaCode(null);
		dataTable.setViaId(null);
		dataTable.setUploadLogId(null);
		dataTable.setUploaderId(userId);
		dataTable.setBasisOfData(observationBulkData.getBasisOfData());

		System.out.println("\n***** DataTable Prepared *****\n");
		System.out.println(dataTable.toString());
		return dataTable;
	}

	private void createPointTopology(GeometryFactory geofactory, ObservationBulkDTO observationBulkData,
			DataTable dataTable) {

		DecimalFormat df = new DecimalFormat("#.####");
		df.setRoundingMode(RoundingMode.HALF_EVEN);
		double latitude = Double.parseDouble(df.format(observationBulkData.getLatitude()));
		double longitude = Double.parseDouble(df.format(observationBulkData.getLongitude()));
		Coordinate c = new Coordinate(longitude, latitude);
		Geometry topology = geofactory.createPoint(c);
		dataTable.setGeographicalCoverageTopology(topology);
	}
}
