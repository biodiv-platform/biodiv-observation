package com.strandls.observation.es.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.opencsv.CSVWriter;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.observation.dao.ObservationDownloadLogDAO;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.service.MailService;
import com.strandls.observation.service.ObservationListService;
import com.strandls.user.ApiException;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.DownloadLogData;

public class ObservationListCSVThread implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ObservationListCSVThread.class);
	private final String modulePath = "/data-archive/listpagecsv";
	private final String basePath = "/app/data/biodiv";

	private ESUtility esUtility;
	private ObservationListService observationListService;
	private ObservationDownloadLogDAO downloadLogDao;
	private List<String> customfields;
	private List<String> taxonomic;
	private List<String> spatial;
	private List<String> traits;
	private List<String> temporal;
	private List<String> misc;

	private String sGroup;
	private String taxon;
	private String user;
	private String userGroupList;
	private String webaddress;
	private String speciesName;
	private String mediaFilter;
	private String months;
	private String isFlagged;
	private String minDate;
	private String maxDate;
	private String validate;
	private Map<String, List<String>> traitParams;
	private Map<String, List<String>> customParams;
	private String classificationid;
	private MapSearchParams mapSearchParams;
	private String maxvotedrecoid; // --
	private String createdOnMaxDate;
	private String createdOnMinDate;
	private String status;
	private String taxonId;
	private String recoName;
	private String rank;
	private String tahsil;
	private String district;
	private String state;
	private String tags;
	private String publicationGrade;

	private String index;
	private String type;
	private String geoAggregationField;
	private Integer geoAggegationPrecision;
	private Boolean onlyFilteredAggregation;
	private String termsAggregationField;

	private String authorId;
	private String notes;
	private String url;
	private String dataSetName;
	private String dataTableName;
	private String dataTableId;
	private MailService mailService;
	private UserServiceApi userServiceApi;
	private ObjectMapper objectMapper;
	private MapSearchQuery mapSearchQuery;
	private String geoShapeFilterField;

	public ObservationListCSVThread() {
		super();
	}

	public ObservationListCSVThread(ESUtility esUtility, ObservationListService observationListService,
			ObservationDownloadLogDAO downloadLogDao, List<String> customfields, List<String> taxonomic,
			List<String> spatial, List<String> traits, List<String> temporal, List<String> misc, String sGroup,
			String taxon, String user, String userGroupList, String webaddress, String speciesName, String mediaFilter,
			String months, String isFlagged, String minDate, String maxDate, String validate,
			Map<String, List<String>> traitParams, Map<String, List<String>> customParams, String classificationid,
			MapSearchParams mapSearchParams, String maxvotedrecoid, String createdOnMaxDate, String createdOnMinDate,
			String status, String taxonId, String recoName, String rank, String tahsil, String district, String state,
			String tags, String publicationGrade, String index, String type, String geoAggregationField,
			Integer geoAggegationPrecision, Boolean onlyFilteredAggregation, String termsAggregationField,
			String authorId, String notes, String url, String dataSetName, String dataTableName,
			MailService mailService, UserServiceApi userServiceApi, ObjectMapper objectMapper,
			MapSearchQuery mapSearchQuery, String geoShapeFilterField, String dataTableId) {
		super();
		this.esUtility = esUtility;
		this.observationListService = observationListService;
		this.downloadLogDao = downloadLogDao;
		this.customfields = customfields;
		this.taxonomic = taxonomic;
		this.spatial = spatial;
		this.traits = traits;
		this.temporal = temporal;
		this.misc = misc;
		this.sGroup = sGroup;
		this.taxon = taxon;
		this.user = user;
		this.userGroupList = userGroupList;
		this.webaddress = webaddress;
		this.speciesName = speciesName;
		this.mediaFilter = mediaFilter;
		this.months = months;
		this.isFlagged = isFlagged;
		this.minDate = minDate;
		this.maxDate = maxDate;
		this.validate = validate;
		this.traitParams = traitParams;
		this.customParams = customParams;
		this.classificationid = classificationid;
		this.mapSearchParams = mapSearchParams;
		this.maxvotedrecoid = maxvotedrecoid;
		this.createdOnMaxDate = createdOnMaxDate;
		this.createdOnMinDate = createdOnMinDate;
		this.status = status;
		this.taxonId = taxonId;
		this.recoName = recoName;
		this.rank = rank;
		this.tahsil = tahsil;
		this.district = district;
		this.state = state;
		this.tags = tags;
		this.publicationGrade = publicationGrade;
		this.index = index;
		this.type = type;
		this.geoAggregationField = geoAggregationField;
		this.geoAggegationPrecision = geoAggegationPrecision;
		this.onlyFilteredAggregation = onlyFilteredAggregation;
		this.termsAggregationField = termsAggregationField;
		this.authorId = authorId;
		System.out.println("\n\n***** Author Id: " + authorId + " *****\n\n");
		this.notes = notes;
		this.url = url;
		this.dataSetName = dataSetName;
		this.dataTableName = dataTableName;
		this.mailService = mailService;
		this.userServiceApi = userServiceApi;
		this.objectMapper = objectMapper;
		this.mapSearchQuery = mapSearchQuery;
		this.geoShapeFilterField = geoShapeFilterField;
		this.dataTableId = dataTableId;
	}

	@Override
	public void run() {
		DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
		LocalDateTime now = LocalDateTime.now();
		logger.info("Observation List Download Request Received : RequestId = " + authorId + dtf.format(now));
		ObservationUtilityFunctions obUtil = new ObservationUtilityFunctions();
		String fileName = obUtil.getCsvFileNameDownloadPath(false);
		String filePath = basePath + modulePath + File.separator + fileName;
		CSVWriter writer = obUtil.getCsvWriter(filePath);
		obUtil.writeIntoCSV(writer, obUtil.getCsvHeaders(customfields, taxonomic, spatial, traits, temporal, misc));
		Integer max = 10000;
		Integer offset = 0;
		Integer epochSize = 0;
		String fileGenerationStatus = "Pending";
		String fileType = "CSV";
		DownloadLog entity = obUtil.createDownloadLogEntity(null, Long.parseLong(authorId), url, notes, 0L,
				fileGenerationStatus, fileType);
		try {
			fileGenerationStatus = "SUCCESS";
			do {
				mapSearchParams.setFrom(offset);
				mapSearchParams.setLimit(max);

				MapSearchQuery searchQuery = mapSearchQuery != null ? mapSearchQuery
						: esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList, webaddress, speciesName,
								mediaFilter, months, isFlagged, minDate, maxDate, validate, traitParams, customParams,
								classificationid, mapSearchParams, maxvotedrecoid, null, createdOnMaxDate,
								createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state, tags,
								publicationGrade, null, dataSetName, dataTableName, null, dataTableId);

				List<ObservationListElasticMapping> epochSet = observationListService.getObservationListCsv(index, type,
						searchQuery, geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation,
						termsAggregationField, geoShapeFilterField);

				epochSize = epochSet.size();
				offset = offset + max;
				obUtil.insertListToCSV(epochSet, writer, customfields, taxonomic, spatial, traits, temporal, misc,
						objectMapper);
				logger.info(
						"Observation List Download RequestId = " + authorId + dtf.format(now) + "@ offset = " + offset);
			} while (epochSize >= max);
			entity.setFilePath(filePath);
			entity.setStatus(fileGenerationStatus);
			mailService.sendMail(authorId, fileName, "observation");
		} catch (Exception e) {
			logger.error("file generation failed @ " + filePath + " due to - " + e.getMessage());
			fileGenerationStatus = "FAILED";
			entity.setStatus(fileGenerationStatus);
		} finally {
			obUtil.closeWriter();
			entity.setStatus(fileGenerationStatus);
			DownloadLogData data = new DownloadLogData();
			data.setFilePath(modulePath + File.separator + fileName);
			data.setFileType(fileType);
			data.setFilterUrl(entity.getFilterUrl());
			data.setStatus(fileGenerationStatus);
			data.setNotes(notes);
			data.setSourcetype("Observations");
			try {
				userServiceApi.logDocumentDownload(data);
			} catch (ApiException e) {
				logger.error(e.getMessage());
			}
		}
		if (fileGenerationStatus.equalsIgnoreCase("failed")) {
			try {
				Files.deleteIfExists(Paths.get(filePath));
			} catch (IOException e) {
				logger.error(e.getMessage());
			}
		}
	}

}
