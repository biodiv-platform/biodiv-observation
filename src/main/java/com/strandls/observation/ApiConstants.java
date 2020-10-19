/**
 * 
 */
package com.strandls.observation;

/**
 * @author Abhishek Rudra
 *
 */
public class ApiConstants {

	// versioning
	public static final String V1 = "/v1";
	// ---------- paths ---------------

//	<----- Observation controller------>
	public static final String OBSERVATION = "/observation";
	public static final String SHOW = "/show";
	public static final String PING = "/ping";
	public static final String BULK = "/bulk";
	public static final String CREATE = "/create";
	public static final String SPECIESGROUP = "/speciesgroup";
	public static final String AGREE = "/agree";
	public static final String UPDATE = "/update";
	public static final String EDIT = "/edit";
	public static final String TAGS = "/tags";
	public static final String TRAITS = "/traits";
	public static final String USERGROUP = "/usergroup";
	public static final String ALL = "/all";
	public static final String SPECIES = "/species";
	public static final String LANGUAGE = "/language";
	public static final String FEATURED = "/featured";
	public static final String UNFEATURED = "/unfeatured";
	public static final String PERMISSIONS = "/permissions";
	public static final String AUTOCOMPLETE = "/autocomplete";
	public static final String DELETE = "/delete";
	public static final String FLAG = "/flag";
	public static final String UNFLAG = "/unflag";
	public static final String FOLLOW = "/follow";
	public static final String UNFOLLOW = "/unfollow";
	public static final String AUTHOR = "/author";
	public static final String APPLYFILTER = "/applyFilter";
	public static final String APPLYGEOPRIVACY = "/applyGeoPrivacy";
	public static final String CUSTOMFIELD = "/customField";
	public static final String OPTIONS = "/options";
	public static final String INSERT = "/insert";
	public static final String PRODUCE = "/produce";
	public static final String CREATEOBSERVATION = "/createObservation";
	public static final String UGCONTEXT = "/ugContext";
	public static final String MAIL = "/mail";
	public static final String LASTREVISED = "/lastrevised";
	public static final String MAXVOTEDRECO = "/maxVotedReco";
	public static final String ADD = "/add";
	public static final String COMMENT = "/comment";
	public static final String FIND = "/find";
	public static final String MINIMAL = "/minimal";
	public static final String RATING = "/rating";
	public static final String RESOURCE = "/resource";
	public static final String POSTING = "/posting";
	public static final String REMOVING = "/removing";

	public static final String PUBLICATIONGRADE = "/publicationgrade";
	public static final String INDEXFIELDUPDATE = "/indexfieldupdate";
	public static final String USERINFO = "/userinfo";

//	<-------------RECO Controller------------->
	public static final String RECO = "/reco";
	public static final String RECOVOTE = "/recovote";
	public static final String IBP = "/ibp";
	public static final String CANONICAL = "/canonical";
	public static final String REMOVE = "/remove";
	public static final String VALIDATE = "/validate";
	public static final String UNLOCK = "/unlock";
	public static final String RECALCULATE = "/recalculate";

	// Observation List Controller
	public static final String LIST = "/list";
	public static final String AGGREGATE = "/aggregate";
	public static final String LISTCSV = "/listcsv";
	public static final String LISTDOWNLOAD = "/listdownload";
	
}
