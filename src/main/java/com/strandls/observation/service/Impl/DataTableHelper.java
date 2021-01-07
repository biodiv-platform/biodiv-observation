package com.strandls.observation.service.Impl;

import com.strandls.observation.dto.ObservationBulkDTO;
import com.strandls.observation.pojo.DataTable;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;

public class DataTableHelper {

    public DataTable createDataTable(ObservationBulkDTO observationBulkData, Long userId) {
        DataTable dataTable = new DataTable();
        dataTable.setAccessLicenseId(observationBulkData.getLicenseId());
        dataTable.setAccessRights(null);
        dataTable.setAgreeTerms(true);
        dataTable.setChecklistId(null);
        dataTable.setColumns(new ArrayList<String>().toString()); // ask
        dataTable.setCreatedOn(observationBulkData.getCreatedOn());
        dataTable.setCustomFields(new HashMap<String, String>().toString()); // ask
        dataTable.setDatasetId(observationBulkData.getDataset());
        dataTable.setDataTableType("OBSERVATIONS");
        dataTable.setDeleted(false);
        dataTable.setDescription(observationBulkData.getDescription());
        dataTable.setExternalId(null);
        dataTable.setExternalUrl(null);
        dataTable.setFeatureCount(0);
        dataTable.setFlagCount(0);

        // geo fields
        dataTable.setGeographicalCoverageGeoPrivacy(observationBulkData.getHidePreciseLocation());
        dataTable.setGeographicalCoverageLatitude(observationBulkData.getLatitude());
        dataTable.setGeographicalCoverageLongitude(observationBulkData.getLongitude());
        dataTable.setGeographicalCoverageLocationAccuracy(observationBulkData.getLocationAccuracy());
        dataTable.setGeographicalCoverageLocationScale(observationBulkData.getLocationScale());
        dataTable.setGeographicalCoveragePlaceName(observationBulkData.getObservedAt());

        GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);
        DecimalFormat df = new DecimalFormat("#.####");
        df.setRoundingMode(RoundingMode.HALF_EVEN);
        double latitude = Double.parseDouble(df.format(observationBulkData.getLatitude()));
        double longitude = Double.parseDouble(df.format(observationBulkData.getLongitude()));
        Coordinate c = new Coordinate(longitude, latitude);
        Geometry topology = geofactory.createPoint(c);
        dataTable.setGeographicalCoverageTopology(topology);

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
}
