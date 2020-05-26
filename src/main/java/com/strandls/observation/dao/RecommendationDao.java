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

import javax.inject.Inject;

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

	@SuppressWarnings("unchecked")
	public Recommendation findRecoByTaxonId(Long taxonId, Boolean isScientific) {

		String qry = "from Recommendation where taxonConceptId = :taxonId and isScientificName = :isScientific";
		Session session = sessionFactory.openSession();
		Recommendation result = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("taxonId", taxonId);
			query.setParameter("isScientific", isScientific);
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
	public Recommendation findByCommonName(String name, Long languageId) {
		String qry = "from Recommendation where name = :name and isScientificName = false and languageId = :languageId";
		Session session = sessionFactory.openSession();
		Recommendation result = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("name", name);
			query.setParameter("languageId", languageId);
			result = query.getSingleResult();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	public List<Recommendation> findByCanonicalName(String canonicalName) {

		String qry = "from Recommendation where canonicalName = :canonicalName and isScientificName = true";
		Session session = sessionFactory.openSession();
		List<Recommendation> resultList = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("canonicalName", canonicalName);
			resultList = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return resultList;
	}

	@SuppressWarnings("unchecked")
	public List<Recommendation> findAllScientificName() {

		String qry = "from Recommendation where isScientificName = true";
		Session session = sessionFactory.openSession();
		List<Recommendation> resultList = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			resultList = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return resultList;
	}

	@SuppressWarnings("unchecked")
	public List<Recommendation> findByRecoName(String name, Boolean isScientific) {
		String qry = "from Recommendation where name = :name and isScientificName = :isScientific";
		Session session = sessionFactory.openSession();
		List<Recommendation> result = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("name", name);
			query.setParameter("isScientific", isScientific);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;

	}

}
