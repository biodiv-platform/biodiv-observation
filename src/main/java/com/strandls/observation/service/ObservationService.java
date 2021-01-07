/**
 * 
 */
package com.strandls.observation.service;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import javax.servlet.http.HttpServletRequest;

import com.strandls.observation.dto.ObservationBulkDTO;
import org.pac4j.core.profile.CommonProfile;

import com.strandls.activity.pojo.Activity;
import com.strandls.activity.pojo.CommentLoggingData;
import com.strandls.activity.pojo.MailData;
import com.strandls.observation.es.util.ObservationListElasticMapping;
import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.pojo.ListPagePermissions;
import com.strandls.observation.pojo.MaxVotedRecoPermission;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ObservationCreateUGContext;
import com.strandls.observation.pojo.ObservationUGContextCreatePageData;
import com.strandls.observation.pojo.ObservationUpdateData;
import com.strandls.observation.pojo.ObservationUserPageInfo;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowData;
import com.strandls.resource.pojo.ResourceRating;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.FactValuePair;
import com.strandls.traits.pojo.TraitsValue;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.user.pojo.Follow;
import com.strandls.userGroup.pojo.CustomFieldFactsInsert;
import com.strandls.userGroup.pojo.CustomFieldObservationData;
import com.strandls.userGroup.pojo.CustomFieldValues;
import com.strandls.userGroup.pojo.Featured;
import com.strandls.userGroup.pojo.FeaturedCreate;
import com.strandls.userGroup.pojo.UserGroupIbp;
import com.strandls.utility.pojo.FlagIbp;
import com.strandls.utility.pojo.FlagShow;
import com.strandls.utility.pojo.Language;
import com.strandls.utility.pojo.Tags;
import com.strandls.utility.pojo.TagsMapping;

/**
 * @author Abhishek Rudra
 *
 */
public interface ObservationService {

	public ShowData findById(Long id);

	public ShowData createObservation(HttpServletRequest request, ObservationCreate observationData);

	public ObservationUpdateData getObservationEditPageData(CommonProfile profile, Long observationId) throws Exception;

	public ShowData editObservaitonCore(HttpServletRequest request, CommonProfile profile, Long observationId,
			ObservationUpdateData observationUpdate) throws Exception;

	public String removeObservation(HttpServletRequest request, CommonProfile profile, Long userId, Long observationId);

	public Long updateSGroup(HttpServletRequest request, Long observationId, Long sGroupId);

	public Long updateMaxVotedReco(Long observationId, Long maxVotedReco);

	public List<Tags> updateTags(HttpServletRequest request, TagsMapping tagsMapping);

	public List<FactValuePair> updateTraits(HttpServletRequest request, String observationId, String traitId,
			List<Long> valueList);

	public List<UserGroupIbp> updateUserGroup(HttpServletRequest request, String observationId,
			List<Long> userGroupList);

	public List<SpeciesGroup> getAllSpeciesGroup();

	public List<Language> getLanguages(Boolean isDirty);

	public List<Featured> createFeatured(HttpServletRequest request, FeaturedCreate featuredCreate);

	public List<Featured> unFeatured(HttpServletRequest request, String observaitonId, List<Long> userGroupList);

	public List<TraitsValue> getTraitsValue(HttpServletRequest request, String traitId);

	public List<TraitsValuePair> getTraitList(String speciesId);

	public ObservationUserPermission getUserPermissions(HttpServletRequest request, CommonProfile profile,
			String observationId, Long userId, String taxonList) throws Exception;

	public List<Tags> getTagsSugguestions(String phrase);

	public List<UserGroupIbp> getUsersGroupList(HttpServletRequest request, CommonProfile profile);

	public List<AllRecoSugguestions> aggregateAllRecoSuggestions(List<RecoIbp> allRecoVote);

	public List<FlagShow> createFlag(HttpServletRequest request, Long observationId, FlagIbp flagIbp);

	public List<FlagShow> unFlag(HttpServletRequest request, Long observationId, String flagId);

	public Follow followRequest(HttpServletRequest request, Long observationId);

	public Follow unFollowRequest(HttpServletRequest request, Long observationId);

	public Long getObservationAuthor(Long observationId);

	public void applyFilterObservationPosting(String userGroupId);

	public void applyFilterObservationRemoving(String userGroupId);

	public void applyGeoPrivacyObservaiton();

	public List<CustomFieldObservationData> addUpdateCustomFieldData(HttpServletRequest request,
			CustomFieldFactsInsert factsCreateData);

	public List<CustomFieldValues> getCustomFieldOptions(HttpServletRequest request, String observationId,
			String userGroupId, String cfId);

	public void produceToRabbitMQ(String observationId, String updateType);

	public ObservationUGContextCreatePageData getUGContextObservationCreateDetails(HttpServletRequest request,
			Long userGroupId);

	public ShowData creteObservationUGContext(HttpServletRequest request,
			ObservationCreateUGContext observationUGContext);

	public Boolean updateLastRevised(Long observationId);

	public List<MaxVotedRecoPermission> listMaxRecoVotePermissions(HttpServletRequest request, CommonProfile profile,
			Map<Long, Long> observationTaxonId);

	public ListPagePermissions getListPagePermissions(HttpServletRequest request, CommonProfile profile,
			Long observationId, String taxonList);

	public MailData generateMailData(Long observationId);

	public Activity addObservationComment(HttpServletRequest request, CommentLoggingData comment);

	public Boolean updateGalleryResourceRating(HttpServletRequest request, Long observationId,
			ResourceRating resourceRating);

	public ObservationListElasticMapping getObservationPublicationGrade(String index, String type,
			String observationId);

	public List<DownloadLog> fetchDownloadLog(List<Long> authorId, String fileType, Integer offSet, Integer limit);

	public void bgfilterRule(HttpServletRequest request, Long observationId);

	public String forceUpdateIndexField(String index, String type, String field, String value, Long dataTableId);

	public ObservationUserPageInfo observationUploadInfo(Long userId, Long sGroupId, Boolean hasMedia, Long offset);

	public ObservationUserPageInfo observationIdentifiedInfo(Long userId, Long sGroupId, Boolean hasMedia, Long offset);

	public void observationBulkUpload(HttpServletRequest request, ObservationBulkDTO observationBulkData) throws InterruptedException, ExecutionException;

}
