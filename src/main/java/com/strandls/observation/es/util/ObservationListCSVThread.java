package com.strandls.observation.es.util;

import java.util.List;
import java.util.Map;

import com.opencsv.CSVWriter;
import com.strandls.esmodule.pojo.MapSearchParams;
import com.strandls.esmodule.pojo.MapSearchQuery;
import com.strandls.observation.dao.ObservationDownloadLogDAO;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.service.MailService;
import com.strandls.observation.service.ObservationListService;

public class ObservationListCSVThread implements Runnable {
	

	private ESUtility esUtility;
	private ObservationListService observationListService;
	private ObservationDownloadLogDAO downloadLogDao;
	private List<String>customfields;
	private List<String>taxonomic;
	private List<String>spatial;
	private List<String>traits;
	private List<String>temporal;
	private List<String>misc;
	
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
	private String maxvotedrecoid; //--
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
	private MailService mailService;
	
	
	public ObservationListCSVThread() {
		super();
		// TODO Auto-generated constructor stub
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
			String authorId, String notes, String url, MailService mailService) {
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
		this.notes = notes;
		this.url = url;
		this.mailService = mailService;
	}



	@Override
	public void run() {
		ObservationUtilityFunctions obUtil = new ObservationUtilityFunctions();
		String filePath = obUtil.getCsvFileNameDownloadPath();
		CSVWriter writer = obUtil.getCsvWriter(filePath);
		obUtil.writeIntoCSV(writer, obUtil.getCsvHeaders(customfields, taxonomic, spatial, traits, temporal, misc));
		Integer max = 500;
		Integer offset = 0;
		Integer epochSize = 0;
		String fileGenerationStatus = "Pending";
		String fileType = "CSV";
		DownloadLog entity = obUtil.createDownloadLogEntity(null,Long.parseLong(authorId), url,
				notes, 0L, fileGenerationStatus,fileType);
		downloadLogDao.save(entity);
		do {
			mapSearchParams.setFrom(offset);
			mapSearchParams.setLimit(max);

			MapSearchQuery mapSearchQuery = esUtility.getMapSearchQuery(sGroup, taxon, user, userGroupList,
					webaddress, speciesName, mediaFilter, months, isFlagged, minDate, maxDate, validate,
					traitParams, customParams, classificationid, mapSearchParams, maxvotedrecoid, createdOnMaxDate,
					createdOnMinDate, status, taxonId, recoName, rank, tahsil, district, state, tags,
					publicationGrade);

			List<ObservationListElasticMapping> epochSet = observationListService.getObservationListCsv(index, type,
					mapSearchQuery, geoAggregationField, geoAggegationPrecision, onlyFilteredAggregation,
					termsAggregationField);

			epochSize = epochSet.size();
			offset = offset + max;
			obUtil.insertListToCSV(epochSet, writer, customfields, taxonomic, spatial, traits, temporal, misc);

		} while (epochSize >= max);
		obUtil.closeWriter();
		fileGenerationStatus = "SUCCESS";
		entity.setFilePath(filePath);
		entity.setStatus(fileGenerationStatus);
		downloadLogDao.update(entity);
		mailService.sendMail(authorId);		
		mailService.sendMail("1111");
		System.out.println("Successful operation");

	}

}
