/**
 * 
 */
package com.strandls.observation.dao;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
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

}
