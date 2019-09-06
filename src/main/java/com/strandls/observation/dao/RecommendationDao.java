/**
 * 
 */
package com.strandls.observation.dao;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.util.AbstractDAO;

/**
 * @author Abhishek Rudra
 *
 */
public class RecommendationDao extends AbstractDAO<Recommendation, Long> {

	private final Logger logger = LoggerFactory.getLogger(RecommendationDao.class);

	/**
	 * @param sessionFactory
	 */
	@Inject
	protected RecommendationDao(SessionFactory sessionFactory) {
		super(sessionFactory);
	}

	@Override
	public Recommendation findById(Long id) {
		Session session = sessionFactory.openSession();
		Recommendation entity = null;
		try {
			entity = session.get(Recommendation.class, id);
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return entity;
	}

}
