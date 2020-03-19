/**
 * 
 */
package com.strandls.observation.service;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.pac4j.core.profile.CommonProfile;

import com.strandls.observation.pojo.AllRecoSugguestions;
import com.strandls.observation.pojo.MaxVotedRecoPermission;
import com.strandls.observation.pojo.ObservationCreate;
import com.strandls.observation.pojo.ObservationCreateUGContext;
import com.strandls.observation.pojo.ObservationUGContextCreatePageData;
import com.strandls.observation.pojo.ObservationUpdateData;
import com.strandls.observation.pojo.ObservationUserPermission;
import com.strandls.observation.pojo.RecoIbp;
import com.strandls.observation.pojo.ShowData;
import com.strandls.observation.pojo.observationMailData;
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

	public ShowData editObservaitonCore(CommonProfile profile, Long observationId,
			ObservationUpdateData observationUpdate) throws Exception;

	public String removeObservation(CommonProfile profile, Long userId, Long observationId);

	public Long updateSGroup(Long observationId, Long sGroupId);

	public Long updateMaxVotedReco(Long observationId, Long maxVotedReco);

	public List<Tags> updateTags(TagsMapping tagsMapping);

	public List<FactValuePair> updateTraits(String observationId, String traitId, List<Long> valueList);

	public List<UserGroupIbp> updateUserGroup(String observationId, List<Long> userGroupList);

	public List<SpeciesGroup> getAllSpeciesGroup();

	public List<Language> getLanguages(Boolean isDirty);

	public List<Featured> createFeatured(FeaturedCreate featuredCreate);

	public List<Featured> unFeatured(String observaitonId, List<Long> userGroupList);

	public List<TraitsValue> getTraitsValue(String traitId);

	public List<TraitsValuePair> getTraitList(String speciesId);

	public ObservationUserPermission getUserPermissions(CommonProfile profile, String observationId, Long userId,
			String taxonList) throws Exception;

	public List<Tags> getTagsSugguestions(String phrase);

	public List<UserGroupIbp> getUsersGroupList(CommonProfile profile);

	public List<AllRecoSugguestions> aggregateAllRecoSuggestions(List<RecoIbp> allRecoVote);

	public List<FlagShow> createFlag(Long observationId, FlagIbp flagIbp);

	public List<FlagShow> unFlag(Long observationId, String flagId);

	public Follow followRequest(Long observationId);

	public Follow unFollowRequest(Long observationId);

	public Long getObservationAuthor(Long observationId);

	public void applyFilterObservation(String userGroupIds);

	public void applyGeoPrivacyObservaiton();

	public List<CustomFieldObservationData> addUpdateCustomFieldData(CustomFieldFactsInsert factsCreateData);

	public List<CustomFieldValues> getCustomFieldOptions(String observationId, String userGroupId, String cfId);

	public void produceToRabbitMQ(String observationId, String updateType);

	public ObservationUGContextCreatePageData getUGContextObservationCreateDetails(Long userGroupId);

	public ShowData creteObservationUGContext(HttpServletRequest request,
			ObservationCreateUGContext observationUGContext);

	public observationMailData getMailData(Long obvId);

	public Boolean updateLastRevised(Long observationId);

	public List<MaxVotedRecoPermission> listMaxRecoVotePermissions(CommonProfile profile,
			Map<Long, Long> observationTaxonId);

}
