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

import com.strandls.observation.pojo.Observation;
import com.strandls.observation.util.AbstractDAO;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationDAO extends AbstractDAO<Observation, Long> {

	private static final Logger logger = LoggerFactory.getLogger(ObservationDAO.class);

	@Inject
	protected ObservationDAO(SessionFactory sessionFactory) {
		super(sessionFactory);
	}

	@Override
	public Observation findById(Long id) {
		Session session = sessionFactory.openSession();
		Observation entity = null;
		try {
			entity = session.get(Observation.class, id);
		} catch (Exception e) {
			logger.info(e.getMessage());
			logger.error(e.toString());
		} finally {
			session.close();
		}
		return entity;
	}

	@SuppressWarnings("unchecked")
	public List<Observation> fetchInBatch(int startPoint) {
		List<Observation> result = null;
		Session session = sessionFactory.openSession();
		String qry = "from Observation where isDeleted = false and geoPrivacy = false and maxVotedRecoId is not NULL  order by id";
		try {
			Query<Observation> query = session.createQuery(qry);
			query.setMaxResults(20000);
			query.setFirstResult(startPoint);
			result = query.getResultList();

		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	public List<Observation> fecthByListOfIds(List<Long> observationList) {
		String qry = "from Observation where isDeleted = false and id IN :ids";
		Session session = sessionFactory.openSession();
		List<Observation> result = null;
		try {
			Query<Observation> query = session.createQuery(qry);
			query.setParameter("ids", observationList);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	public List<Observation> fetchInBatchRecoCalculate(int startPoint) {
		List<Observation> result = null;
		Session session = sessionFactory.openSession();
		String qry = "from Observation where isDeleted = false and noOfIdentifications = 0 and maxVotedRecoId is not NULL order by id";
		try {
			Query<Observation> query = session.createQuery(qry);
			query.setMaxResults(5000);
			query.setFirstResult(startPoint);
			result = query.getResultList();

		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return result;
	}

}
