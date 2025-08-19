package com.strandls.observation.service.Impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.strandls.activity.pojo.UserGroupMailData;
import com.strandls.traits.pojo.MailData;
import com.strandls.traits.pojo.ObservationMailData;

public class MailMetaDataConverter {

	public MailData traitMetaData(com.strandls.activity.pojo.MailData mailData) {
		MailData metaData = new MailData();
		ObservationMailData observationMeta = new ObservationMailData();
		observationMeta.setAuthorId(mailData.getObservationData().getAuthorId());
		observationMeta.setCommonName(mailData.getObservationData().getCommonName());
		observationMeta.setIconURl(mailData.getObservationData().getIconURl());
		observationMeta.setLocation(mailData.getObservationData().getLocation());
		observationMeta.setObservationId(mailData.getObservationData().getObservationId());
		Date odt = mailData.getObservationData().getObservedOn();
		observationMeta.setObservedOn(odt == null ? null : Date.from(odt.toInstant()));
		observationMeta.setScientificName(mailData.getObservationData().getScientificName());
		List<com.strandls.traits.pojo.UserGroupMailData> userGroupMeta = new ArrayList<com.strandls.traits.pojo.UserGroupMailData>();
		for (UserGroupMailData userGroupData : mailData.getUserGroupData()) {
			com.strandls.traits.pojo.UserGroupMailData ugMeta = new com.strandls.traits.pojo.UserGroupMailData();
			ugMeta.setIcon(userGroupData.getIcon());
			ugMeta.setId(userGroupData.getId());
			ugMeta.setName(userGroupData.getName());
			ugMeta.setWebAddress(userGroupData.getWebAddress());
			userGroupMeta.add(ugMeta);
		}
		metaData.setObservationData(observationMeta);
		metaData.setUserGroupData(userGroupMeta);
		return metaData;
	}

	public com.strandls.userGroup.pojo.MailData userGroupMetadata(com.strandls.activity.pojo.MailData mailData) {
		com.strandls.userGroup.pojo.MailData metaData = new com.strandls.userGroup.pojo.MailData();
		com.strandls.userGroup.pojo.ObservationMailData observationMeta = new com.strandls.userGroup.pojo.ObservationMailData();
		observationMeta.setAuthorId(mailData.getObservationData().getAuthorId());
		observationMeta.setCommonName(mailData.getObservationData().getCommonName());
		observationMeta.setIconURl(mailData.getObservationData().getIconURl());
		observationMeta.setLocation(mailData.getObservationData().getLocation());
		observationMeta.setObservationId(mailData.getObservationData().getObservationId());
		Date odt = mailData.getObservationData().getObservedOn();
		observationMeta.setObservedOn(odt == null ? null : Date.from(odt.toInstant()));
		observationMeta.setScientificName(mailData.getObservationData().getScientificName());
		List<com.strandls.userGroup.pojo.UserGroupMailData> userGroupMeta = new ArrayList<com.strandls.userGroup.pojo.UserGroupMailData>();
		for (UserGroupMailData userGroupData : mailData.getUserGroupData()) {
			com.strandls.userGroup.pojo.UserGroupMailData ugMeta = new com.strandls.userGroup.pojo.UserGroupMailData();
			ugMeta.setIcon(userGroupData.getIcon());
			ugMeta.setId(userGroupData.getId());
			ugMeta.setName(userGroupData.getName());
			ugMeta.setWebAddress(userGroupData.getWebAddress());
			userGroupMeta.add(ugMeta);
		}
		metaData.setObservationData(observationMeta);
		metaData.setUserGroupData(userGroupMeta);
		return metaData;

	}

	public com.strandls.utility.pojo.MailData utilityMetaData(com.strandls.activity.pojo.MailData mailData) {
		com.strandls.utility.pojo.MailData metaData = new com.strandls.utility.pojo.MailData();
		com.strandls.utility.pojo.ObservationMailData observationMeta = new com.strandls.utility.pojo.ObservationMailData();
		observationMeta.setAuthorId(mailData.getObservationData().getAuthorId());
		observationMeta.setCommonName(mailData.getObservationData().getCommonName());
		observationMeta.setIconURl(mailData.getObservationData().getIconURl());
		observationMeta.setLocation(mailData.getObservationData().getLocation());
		observationMeta.setObservationId(mailData.getObservationData().getObservationId());
		Date odt = mailData.getObservationData().getObservedOn();
		observationMeta.setObservedOn(odt == null ? null : Date.from(odt.toInstant()));
		observationMeta.setScientificName(mailData.getObservationData().getScientificName());
		List<com.strandls.utility.pojo.UserGroupMailData> userGroupMeta = new ArrayList<com.strandls.utility.pojo.UserGroupMailData>();
		for (UserGroupMailData userGroupData : mailData.getUserGroupData()) {
			com.strandls.utility.pojo.UserGroupMailData ugMeta = new com.strandls.utility.pojo.UserGroupMailData();
			ugMeta.setIcon(userGroupData.getIcon());
			ugMeta.setId(userGroupData.getId());
			ugMeta.setName(userGroupData.getName());
			ugMeta.setWebAddress(userGroupData.getWebAddress());
			userGroupMeta.add(ugMeta);
		}
		metaData.setObservationData(observationMeta);
		metaData.setUserGroupData(userGroupMeta);
		return metaData;
	}

}
