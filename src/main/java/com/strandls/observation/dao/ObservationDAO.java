/**
 * 
 */
package com.strandls.observation.dao;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.query.Query;
import org.hibernate.type.LongType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.pojo.Observation;
import com.strandls.observation.util.AbstractDAO;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationDAO extends AbstractDAO<Observation, Long> {

	private final Logger logger = LoggerFactory.getLogger(ObservationDAO.class);

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
	public Long findTotalObservation() {
		Session session = sessionFactory.openSession();
		String qry = "select count(id) from observation where is_deleted = false";
		Long total = null;
		try {
			Query<Long> query = session.createNativeQuery(qry).addScalar("id", LongType.INSTANCE);
			total = query.getSingleResult();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return total;
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
		} finally {
			session.close();
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	public List<Long> fetchObservationIdsList(Long startPoint) {
		Session session = sessionFactory.openSession();
		String qry = "select id from observation where is_deleted = false offset :startPoint limit 30000";
		List<Long> result = null;
		try {
			Query<Long> query = session.createNativeQuery(qry).addScalar("id", LongType.INSTANCE);
			query.setParameter("startPoint", startPoint);
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
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

	@SuppressWarnings({ "unchecked", "deprecation" })
	public List<Object[]> getValuesOfColumnsBasedOnFilter(List<String> projectedColumns, Map<String, Object> filterOn) {
		Session session = sessionFactory.openSession();
		Criteria criteria = session.createCriteria(Observation.class);
		ProjectionList projectionList = Projections.projectionList();
		for (String projectedColumn : projectedColumns) {
			projectionList.add(Projections.property(projectedColumn));
		}
		criteria.add(Restrictions.allEq(filterOn));
		criteria.setProjection(projectionList);
		List<Object[]> queryData = criteria.list();
		session.close();
		return queryData;
	}

	@SuppressWarnings("unchecked")
	public List<Observation> fetchByDataTableId(List<Long> dataTableId, Integer limit, Integer offset) {
		String qry = "from Observation where isDeleted = false and dataTableId IN :ids";
		Session session = sessionFactory.openSession();
		List<Observation> result = null;
		try {
			Query<Observation> query = session.createQuery(qry);
			query.setParameter("ids", dataTableId);
			query.setFirstResult(offset);
			if (limit != null) {
				query.setMaxResults(limit);
			}
			result = query.getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}

		return result;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public List<Observation> getObservationCountForDatatable(Long datatableId) {
		Session session = sessionFactory.openSession();
		List<Observation> observationList = new ArrayList<Observation>();
		String hql = "from Observation o where o.dataTableId = :datatable";
		try {
			Query query = session.createQuery(hql);
			query.setParameter("datatable", datatableId);
			observationList = query.list();
		} catch (Exception ex) {
			logger.error(ex.getMessage());
		} finally {
			session.close();
		}
		return observationList;
	}

}
