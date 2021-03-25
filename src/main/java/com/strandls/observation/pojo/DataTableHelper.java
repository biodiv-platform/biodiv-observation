package com.strandls.observation.pojo;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;

import com.strandls.observation.dto.BulkObservationDTO;
import com.strandls.resource.pojo.UFile;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;

public class DataTableHelper {
	
	public DataTable createDataTable(BulkObservationDTO boDTO, Long userId, UFile uFile) {
		DataTable dataTable = new DataTable();
		dataTable.setAccessLicenseId(boDTO.getLicenseId());
		dataTable.setAccessRights(null);
		dataTable.setAgreeTerms(true);
		dataTable.setChecklistId(null);
		dataTable.setColumns(new ArrayList<String>().toString()); // ask
		dataTable.setCreatedOn(boDTO.getCreatedOn());
		dataTable.setCustomFields(new HashMap<String, String>().toString()); // ask
		dataTable.setDatasetId(boDTO.getDataset());
		dataTable.setDataTableType("OBSERVATIONS");
		dataTable.setDeleted(false);
		dataTable.setDescription(boDTO.getDescription());
		dataTable.setExternalId(null);
		dataTable.setExternalUrl(null);
		dataTable.setFeatureCount(0);
		dataTable.setFlagCount(0);
		
		// geo fields
		dataTable.setGeographicalCoverageGeoPrivacy(boDTO.getHidePreciseLocation());
		dataTable.setGeographicalCoverageLatitude(boDTO.getLatitude());
		dataTable.setGeographicalCoverageLongitude(boDTO.getLongitude());
		dataTable.setGeographicalCoverageLocationAccuracy(boDTO.getLocationAccuracy());
		dataTable.setGeographicalCoverageLocationScale(boDTO.getLocationScale());
		dataTable.setGeographicalCoveragePlaceName(boDTO.getObservedAt());
		
		GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);
		DecimalFormat df = new DecimalFormat("#.####");
		df.setRoundingMode(RoundingMode.HALF_EVEN);
		double latitude = Double.parseDouble(df.format(boDTO.getLatitude()));
		double longitude = Double.parseDouble(df.format(boDTO.getLongitude()));
		Coordinate c = new Coordinate(longitude, latitude);
		Geometry topology = geofactory.createPoint(c);
		dataTable.setGeographicalCoverageTopology(topology);
		
		dataTable.setImagesFileId(null);
		dataTable.setLanguageId(205L);
		dataTable.setLastRevised(boDTO.getCreatedOn());
		dataTable.setMethods(boDTO.getMethods());
		dataTable.setPartyAttributions(boDTO.getAttribution());
		dataTable.setPartyUploaderId(userId);
		dataTable.setPartyContributorId(boDTO.getContributors()); // only one contributor
		dataTable.setProject(boDTO.getProject());
		dataTable.setRating(0);
		dataTable.setSummary(boDTO.getSummary());
		dataTable.setTaxonomicCoverageGroupIds(boDTO.getSGroup());
		dataTable.setTemporalCoverageDateAccuracy(boDTO.getDateAccuracy());
		dataTable.setTemporalCoverageFromDate(boDTO.getObservedFromDate());
		dataTable.setTemporalCoverageToDate(boDTO.getObservedToDate());
		dataTable.setTitle(boDTO.getTitle());
		dataTable.setTraitValueFileId(null);
		dataTable.setuFileId(uFile.getId()); // uFile table id
		dataTable.setVersion(2L);
		dataTable.setViaCode(null);
		dataTable.setViaId(null);
		dataTable.setUploadLogId(null);
		dataTable.setUploaderId(userId);
		dataTable.setBasisOfData(boDTO.getBasisOfData());

		System.out.println("\n***** DataTable Prepared *****\n");
		System.out.println(dataTable.toString());
		return dataTable;
	}

}
