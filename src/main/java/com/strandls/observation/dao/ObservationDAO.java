/**
 * 
 */
package com.strandls.observation.dao;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

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

	@SuppressWarnings({ "unchecked" })
	public Long getObservationCountForDatatable(String datatableId) {

		Session session = sessionFactory.openSession();
		String qry = "select count(*) from observation where is_deleted = false and data_table_id = :datatableId";
		Long total = null;
		try {
			Query<Long> query = session.createNativeQuery(qry).addScalar("count", LongType.INSTANCE);
			;
			query.setParameter("datatableId", Integer.parseInt(datatableId));
			total = query.getSingleResult();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return total;
	}

	public List<Observation> getObservationList(Integer offset, Integer max, String authorId) {
		List<Observation> observationList = new ArrayList<>();
		try (Session session = sessionFactory.openSession()) {
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Observation> cq = cb.createQuery(Observation.class);
			Root<Observation> root = cq.from(Observation.class);
			cq.select(root);
			if (authorId != null && !authorId.isEmpty()) {
				cq.where(cb.equal(root.get("authorId"), Long.parseLong(authorId)));
			}
			cq.orderBy(cb.desc(root.get("createdOn")));
			Query<Observation> query = session.createQuery(cq);
			if (offset != null) {
				query.setFirstResult(offset);
			}
			if (max != null) {
				query.setMaxResults(max);
			}
			observationList = query.getResultList();
		} catch (Exception ex) {
			logger.error("Error fetching observation list", ex);
		}
		return observationList;
	}

	@SuppressWarnings("unchecked")
	public Long findTotalObservationByAuthorID(String authorId) {

		try (Session session = sessionFactory.openSession()) {
			String qry = "SELECT COUNT(id) FROM observation";
			Query<Number> query;

			if (authorId != null && !authorId.isEmpty()) {
				qry += " WHERE author_id = :authorId";
				query = session.createNativeQuery(qry);
				query.setParameter("authorId", Long.parseLong(authorId));
			} else {
				query = session.createNativeQuery(qry);
			}
			return query.getSingleResult().longValue();
		}
	}

}
