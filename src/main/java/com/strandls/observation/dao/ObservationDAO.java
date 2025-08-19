package com.strandls.observation.dao;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.pojo.Observation;
import com.strandls.observation.util.AbstractDAO;

import jakarta.inject.Inject;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

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
			Query<Long> query = session.createNativeQuery(qry).addScalar("id", Long.class);
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
			Query<Observation> query = session.createQuery(qry, Observation.class);
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
			Query<Observation> query = session.createQuery(qry, Observation.class);
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
			Query<Long> query = session.createNativeQuery(qry).addScalar("id", Long.class);
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
			Query<Observation> query = session.createQuery(qry, Observation.class); // Specify result class for type
																					// safety
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

	// Refactored method using JPA Criteria API
	public List<Object[]> getValuesOfColumnsBasedOnFilter(List<String> projectedColumns, Map<String, Object> filterOn) {
		Session session = sessionFactory.openSession();
		List<Object[]> queryData = null;
		try {
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Object[]> cq = cb.createQuery(Object[].class);
			Root<Observation> root = cq.from(Observation.class);

			// Projections
			List<jakarta.persistence.criteria.Selection<?>> selections = new ArrayList<>();
			for (String projectedColumn : projectedColumns) {
				selections.add(root.get(projectedColumn));
			}
			cq.multiselect(selections.toArray(new jakarta.persistence.criteria.Selection[0]));

			// Restrictions
			List<Predicate> predicates = new ArrayList<>();
			Set<Map.Entry<String, Object>> entrySet = filterOn.entrySet();
			for (Map.Entry<String, Object> entry : entrySet) {
				predicates.add(cb.equal(root.get(entry.getKey()), entry.getValue()));
			}
			cq.where(cb.and(predicates.toArray(new Predicate[0])));

			// Execute query
			queryData = session.createQuery(cq).getResultList();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return queryData;
	}

	@SuppressWarnings("unchecked")
	public List<Observation> fetchByDataTableId(List<Long> dataTableId, Integer limit, Integer offset) {
		String qry = "from Observation where isDeleted = false and dataTableId IN :ids";
		Session session = sessionFactory.openSession();
		List<Observation> result = null;
		try {
			Query<Observation> query = session.createQuery(qry, Observation.class); // Specify result class for type
																					// safety
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
		String qry = "select count(*) as count from observation where is_deleted = false and data_table_id = :datatableId";
		Long total = null;
		try {
			// In Hibernate 6, addScalar takes the alias and the class type
			Query<Long> query = session.createNativeQuery(qry).addScalar("count", Long.class);
			query.setParameter("datatableId", Integer.parseInt(datatableId));
			total = query.getSingleResult();
		} catch (Exception e) {
			logger.error(e.getMessage());
		} finally {
			session.close();
		}
		return total;
	}

}