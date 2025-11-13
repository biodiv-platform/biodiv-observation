package com.strandls.observation.util;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.criterion.CriteriaSpecification;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;

public abstract class AbstractDAO<T, K extends Serializable> {

	protected SessionFactory sessionFactory;

	protected Class<? extends T> daoType;

	@SuppressWarnings("unchecked")
	protected AbstractDAO(SessionFactory sessionFactory) {
		daoType = (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
		this.sessionFactory = sessionFactory;
	}

	public T save(T entity) {
		Session session = sessionFactory.openSession();
		Transaction tx = null;
		try {
			tx = session.beginTransaction();
			session.save(entity);
			tx.commit();
		} catch (Exception e) {
			if (tx != null)
				tx.rollback();
			throw e;
		} finally {
			session.close();
		}
		return entity;
	}

	public T update(T entity) {
		Session session = sessionFactory.openSession();
		Transaction tx = null;
		try {
			tx = session.beginTransaction();
			session.update(entity);
			tx.commit();
		} catch (Exception e) {
			if (tx != null)
				tx.rollback();
			throw e;
		} finally {
			session.close();
		}
		return entity;
	}

	public T delete(T entity) {
		Session session = sessionFactory.openSession();
		Transaction tx = null;
		try {
			tx = session.beginTransaction();
			session.delete(entity);
			tx.commit();
		} catch (Exception e) {
			if (tx != null)
				tx.rollback();
			throw e;
		} finally {
			session.close();
		}
		return entity;
	}

	public abstract T findById(K id);

	@SuppressWarnings({ "unchecked", "deprecation" })
	public List<T> findAll() {
		List<T> entities = null;
		Session session = sessionFactory.openSession();
		try {
			Criteria criteria = session.createCriteria(daoType);
			entities = criteria.setResultTransformer(CriteriaSpecification.DISTINCT_ROOT_ENTITY).list();
		} catch (Exception e) {
			throw e;
		} finally {
			session.close();
		}

		return entities;
	}

	@SuppressWarnings({ "unchecked", "deprecation" })
	public List<T> findAll(int limit, int offset) {
		List<T> entities = null;
		Session session = sessionFactory.openSession();
		try {
			Criteria criteria = session.createCriteria(daoType)
					.setResultTransformer(CriteriaSpecification.DISTINCT_ROOT_ENTITY);
			entities = criteria.setFirstResult(offset).setMaxResults(limit).list();
		} catch (Exception e) {
			throw e;
		} finally {
			session.close();
		}

		return entities;
	}

	@SuppressWarnings({ "unchecked", "deprecation" })
	public List<T> fetchFilteredRecordsWithCriteria(String attribute1, String attribute2, List<Long> value1,
			Object value2, String orderBy, Integer offSet, Integer limit) {
		Session session = sessionFactory.openSession();
		try {
			Criteria criteria = session.createCriteria(daoType.getName());
			if (attribute1 != null) {
				criteria.add(Restrictions.in(attribute1, value1));
			}
			if (attribute2 != null)
				criteria.add(Restrictions.eq(attribute2, value2));
			if (orderBy != null)
				criteria.addOrder(Order.desc(orderBy));
			if (offSet != -1)
				criteria.setFirstResult(offSet);
			if (limit != -1)
				criteria.setMaxResults(limit);
			List<T> resultList = criteria.list();
			return resultList;
		} catch (Exception e) {
		} finally {
			session.close();
		}
		return null;
	}

	@SuppressWarnings({ "unchecked", "deprecation" })
	public List<T> fetchFilteredRecords(Map<String, Object> equalsFilters, List<String> notNullFields, String orderBy,
			Integer offset, Integer limit) {
		Session session = sessionFactory.openSession();
		try {
			Criteria criteria = session.createCriteria(daoType.getName());

			// Add equality filters
			if (equalsFilters != null) {
				for (Map.Entry<String, Object> entry : equalsFilters.entrySet()) {
					criteria.add(Restrictions.eq(entry.getKey(), entry.getValue()));
				}
			}

			// Add IS NOT NULL filters
			if (notNullFields != null) {
				for (String field : notNullFields) {
					criteria.add(Restrictions.isNotNull(field));
				}
			}

			// Ordering, pagination
			if (orderBy != null)
				criteria.addOrder(Order.desc(orderBy));
			if (offset != -1)
				criteria.setFirstResult(offset);
			if (limit != -1)
				criteria.setMaxResults(limit);

			return criteria.list();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			session.close();
		}
		return null;
	}

}
