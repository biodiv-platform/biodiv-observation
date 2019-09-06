/**
 * 
 */
package com.strandls.observation.dao;

import java.util.List;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
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
			// TODO: handle exception
		} finally {
			session.close();
		}
		return recoVoteCount;
	}

}
