/**
 * 
 */
package com.strandls.observation.dao;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.pojo.Recommendation;
import com.strandls.observation.util.AbstractDAO;

import jakarta.inject.Inject;

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
	public List<Recommendation> findByIdList(List<Long> idList) {
		String qry = "from Recommendation where id in :idList";
		Session session = sessionFactory.openSession();
		List<Recommendation> result = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("idList", idList);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

	/**
	 * Same as {@link #findByIdList(List)} but runs on a caller-provided session
	 * instead of opening its own, so callers that need to combine this with
	 * other queries can do so on a single pooled connection.
	 */
	@SuppressWarnings("unchecked")
	public List<Recommendation> findByIdList(Session session, List<Long> idList) {
		String qry = "from Recommendation where id in :idList";
		List<Recommendation> result = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("idList", idList);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
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

	@SuppressWarnings("unchecked")
	public List<Recommendation> findByTaxonIds(List<Long> taxonIds) {
		String qry = "from Recommendation where isScientificName = true and taxonConceptId in :taxonIds";
		Session session = sessionFactory.openSession();
		List<Recommendation> result = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("taxonIds", taxonIds);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	public List<Recommendation> findByAcceptedNameIds(List<Long> acceptedIds) {
		String qry = "from Recommendation where isScientificName = true and acceptedNameId in :acceptedIds";
		Session session = sessionFactory.openSession();
		List<Recommendation> result = null;
		try {
			Query<Recommendation> query = session.createQuery(qry);
			query.setParameter("acceptedIds", acceptedIds);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

	public List<Recommendation> updateAll(List<Recommendation> recos) {
		if (recos == null || recos.isEmpty()) {
			return Collections.emptyList();
		}

		Session session = sessionFactory.openSession();
		Transaction tx = null;
		try {
			tx = session.beginTransaction();

			List<Recommendation> saved = new ArrayList<>(recos.size());
			for (int i = 0; i < recos.size(); i++) {
				saved.add(session.merge(recos.get(i)));

				// Flush + clear every 50 rows to avoid OOM on large batches
				if (i % 50 == 0) {
					session.flush();
					session.clear();
				}
			}

			session.flush();
			tx.commit();
			return saved;

		} catch (Exception e) {
			if (tx != null && tx.isActive()) {
				tx.rollback();
			}
			logger.error("Failed to batch update recommendations: {}", e.getMessage(), e);
			throw new RuntimeException("updateAll failed", e);
		} finally {
			session.close();
		}
	}

}
