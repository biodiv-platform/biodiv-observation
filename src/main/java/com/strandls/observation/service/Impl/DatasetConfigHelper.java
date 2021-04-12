package com.strandls.observation.service.Impl;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.observation.dao.DataSetDAO;
import com.strandls.observation.pojo.Dataset;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;

public class DatasetConfigHelper {

	private final Logger logger = LoggerFactory.getLogger(DatasetConfigHelper.class);

	@Inject
	private DataSetDAO datasetDao;

	public void configureDataSetDefault(String title) {
		try {
			Dataset res = datasetDao.findDataSetByTitle(title);

			DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
			Date date = dateFormat.parse("1970-01-01 05:30:00");

			if (res == null) {

				String topleft = "";
				String bottomright = "";
				InputStream in = Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("config.properties");

				Properties properties = new Properties();
				try {
					properties.load(in);
				} catch (IOException e) {
					logger.error(e.getMessage());
				}

				topleft = properties.getProperty("topLeft");
				bottomright = properties.getProperty("bottomRight");
				try {
					in.close();
				} catch (IOException e) {
					logger.error(e.getMessage());
				}

				String point1[] = topleft.split(",");
				String point2[] = bottomright.split(",");

				Coordinate[] cords = new Coordinate[] {
						new Coordinate(Double.parseDouble(point1[0]), Double.parseDouble(point1[1])),
						new Coordinate(Double.parseDouble(point2[0]), Double.parseDouble(point1[1])),
						new Coordinate(Double.parseDouble(point2[0]), Double.parseDouble(point2[1])),
						new Coordinate(Double.parseDouble(point1[0]), Double.parseDouble(point2[1])),
						new Coordinate(Double.parseDouble(point1[0]), Double.parseDouble(point1[1])), };

				GeometryFactory geofactory = new GeometryFactory(new PrecisionModel(), 4326);

				Geometry topology = geofactory.createPolygon(geofactory.createLinearRing(cords));

				Dataset standaloneDataset = new Dataset();
				standaloneDataset.setTitle(title);
				standaloneDataset.setRating(0);
				standaloneDataset.setFeatureCount(0);
				standaloneDataset.setFeatureCount(0);

				standaloneDataset.setPartyContributorId(1);
				standaloneDataset.setPartyUploaderId(1);
				standaloneDataset.setDeleted(false);
				standaloneDataset.setAccessLicenseId(822);
				standaloneDataset.setCreatedOn(new Timestamp(new Date().getTime()));
				standaloneDataset.setLastRevised(new Timestamp(new Date().getTime()));

				standaloneDataset.setTaxonomicCoverageGroupIds("839");
				standaloneDataset.setTemporalCoverageDateAccuracy("Unknown");
				standaloneDataset.setTemporalCoverageFromDate(new Timestamp(date.getTime()));
				standaloneDataset.setTemporalCoverageToDate(new Timestamp(date.getTime()));
				// geographical data
				standaloneDataset.setGeographicalCoverageGeoPrivacy(false);
				standaloneDataset.setGeographicalCoverageLatitude(0);
				standaloneDataset.setGeographicalCoverageLongitude(0);
				standaloneDataset.setGeographicalCoverageTopology(topology);
				standaloneDataset.setGeographicalCoverageLocationScale("APPROXIMATE");

				datasetDao.save(standaloneDataset);

			}

		} catch (Exception e) {
			e.printStackTrace();
			logger.error(e.getMessage());
		}

	}

}
