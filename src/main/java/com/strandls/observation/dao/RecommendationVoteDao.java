/**
 * 
 */
package com.strandls.observation.dao;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.pojo.RecommendationVote;
import com.strandls.observation.util.AbstractDAO;

/**
 * @author Abhishek Rudra
 *
 */
public class RecommendationVoteDao extends AbstractDAO<RecommendationVote, Long> {

	private final Logger logger = LoggerFactory.getLogger(RecommendationVote.class);

	/**
	 * @param sessionFactory
	 */
	@Inject
	protected RecommendationVoteDao(SessionFactory sessionFactory) {
		super(sessionFactory);
	}

	@Override
	public RecommendationVote findById(Long id) {
		Session session = sessionFactory.openSession();
		RecommendationVote entity = null;
		try {
			entity = session.get(RecommendationVote.class, id);
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return entity;
	}

	@SuppressWarnings("unchecked")
	public List<RecommendationVote> findByRecommendationId(Long obvId, Long recoId) {

		String qry = "from RecommendationVote rv where rv.observationId = :obvId " + "and recommendationId = :recoId";
		Session session = sessionFactory.openSession();
		List<RecommendationVote> result = null;
		try {
			Query<RecommendationVote> query = session.createQuery(qry);
			query.setParameter("obvId", obvId);
			query.setParameter("recoId", recoId);

			result = query.getResultList();

		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	public int findRecoVoteCount(Long obvId) {
		String qry = "from RecommendationVote where observationId = :obvId";
		Session session = sessionFactory.openSession();
		Integer recoVoteCount = 0;
		try {
			Query<RecommendationVote> query = session.createQuery(qry);
			query.setParameter("obvId", obvId);
			recoVoteCount = query.getResultList().size();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return recoVoteCount;
	}

	@SuppressWarnings("unchecked")
	public List<RecommendationVote> findRecoVoteOnObservation(Long obvId) {
		String qry = "from RecommendationVote where observationId = :obvId";
		Session session = sessionFactory.openSession();
		List<RecommendationVote> recoVoteList = new ArrayList<RecommendationVote>();
		try {
			Query<RecommendationVote> query = session.createQuery(qry);
			query.setParameter("obvId", obvId);
			recoVoteList = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return recoVoteList;
	}

	@SuppressWarnings("unchecked")
	public RecommendationVote findRecoVoteIdByRecoId(Long observaitonId, Long userId, Long scientificNameId,
			Long commonNameId) {
		String qry = "from RecommendationVote where observationId = :observationId ";
		if (userId != null)
			qry = qry.concat("and authorId = :userId ");

		if (scientificNameId != null && commonNameId != null)
			qry = qry.concat("and recommendationId = :scientificNameId and commonNameRecoId = :commonNameId");
		else if (scientificNameId != null) {
			qry = qry.concat("and recommendationId = :scientificNameId");
		} else if (commonNameId != null) {
			qry = qry.concat("and recommendationId = :commonNameId and commonNameRecoId = :commonNameId");
		}

		RecommendationVote result = null;
		Session session = sessionFactory.openSession();
		try {
			Query<RecommendationVote> query = session.createQuery(qry);
			query.setParameter("observationId", observaitonId);
			if (userId != null)
				query.setParameter("userId", userId);
			if (scientificNameId != null)
				query.setParameter("scientificNameId", scientificNameId);
			if (commonNameId != null)
				query.setParameter("commonNameId", commonNameId);

			query.setMaxResults(1);
			result = query.getSingleResult();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	public Map<Long, Long> getUniqueRecoVoteByUser(Long userId, Long sGroup, Boolean hasMedia, Long offset) {

		String qry = "select rv.recommendation_id, count(rv.id) from recommendation_vote rv "
				+ "inner join observation o on rv.observation_id = o.id where rv.author_id = " + userId;
		if (sGroup != null)
			qry = qry + " and o.group_id = " + sGroup;
		if (hasMedia)
			qry = qry + " and (o.no_of_audio > 0 or o.no_of_videos > 0 or o.no_of_images > 0 ) ";

		qry = qry + " and o.is_deleted = false ";

		qry = qry + " group by rv.recommendation_id order by count(rv.id) desc limit 10 offset " + offset;

		String qry1 = "select count(distinct(rv.recommendation_id)) from recommendation_vote rv "
				+ "inner join observation o on rv.observation_id = o.id where rv.author_id = " + userId;
		if (sGroup != null)
			qry1 = qry1 + " and o.group_id = " + sGroup;
		if (hasMedia)
			qry1 = qry1 + " and (o.no_of_audio > 0 or o.no_of_videos > 0 or o.no_of_images > 0 ) ";

		qry1 = qry1 + " and o.is_deleted = false ";

		Session session = sessionFactory.openSession();

		Map<Long, Long> result = new LinkedHashMap<Long, Long>();

		List<Object[]> objectList = null;

		try {
			Query<Object[]> query = session.createNativeQuery(qry);

			objectList = query.getResultList();

			for (Object object[] : objectList) {
				result.put(Long.parseLong(object[0].toString()), Long.parseLong(object[1].toString()));
			}

			Query<Object> query2 = session.createNativeQuery(qry1);
			Object obj = query2.getSingleResult();
			result.put(null, Long.parseLong(obj.toString()));

		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;

	}

	@SuppressWarnings("unchecked")
	public List<RecommendationVote> findByRecoIdList(List<Long> recoIdList) {
		String qry = "from RecommendationVote where recommendationId in :recoIdList ";
		Session session = sessionFactory.openSession();
		List<RecommendationVote> result = null;
		try {
			Query<RecommendationVote> query = session.createQuery(qry);
			query.setParameter("recoIdList", recoIdList);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

	
	@SuppressWarnings("unchecked")
	public List<RecommendationVote> findCommonNameMatchByRecoIdList(List<Long> recoIdList) {
		String qry = "from RecommendationVote where commonNameRecoId in :recoIdList ";
		Session session = sessionFactory.openSession();
		List<RecommendationVote> result = null;
		try {
			Query<RecommendationVote> query = session.createQuery(qry);
			query.setParameter("recoIdList", recoIdList);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}
	
}
