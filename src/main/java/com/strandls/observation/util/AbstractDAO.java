package com.strandls.observation.util;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.query.Query;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

public abstract class AbstractDAO<T, K extends Serializable> {

	protected final SessionFactory sessionFactory;
	protected final Class<T> daoType;

	@SuppressWarnings("unchecked")
	protected AbstractDAO(SessionFactory sessionFactory) {
		this.sessionFactory = sessionFactory;
		this.daoType = (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}

	public T save(T entity) {
		return executeInTransaction(session -> {
			session.save(entity);
			return entity;
		});
	}

	public T update(T entity) {
		return executeInTransaction(session -> {
			session.update(entity);
			return entity;
		});
	}

	public T delete(T entity) {
		return executeInTransaction(session -> {
			session.delete(entity);
			return entity;
		});
	}

	public abstract T findById(K id);

	public List<T> findAll() {
		try (Session session = sessionFactory.openSession()) {
			String hql = "FROM " + daoType.getSimpleName();
			return session.createQuery(hql, daoType).list();
		}
	}

	public List<T> findAll(int limit, int offset) {
		try (Session session = sessionFactory.openSession()) {
			String hql = "FROM " + daoType.getSimpleName();
			return session.createQuery(hql, daoType).setFirstResult(offset).setMaxResults(limit).list();
		}
	}

	public List<T> fetchFilteredRecordsWithCriteria(String attribute1, String attribute2, List<Long> value1,
			Object value2, String orderBy, Integer offSet, Integer limit) {
		Session session = sessionFactory.openSession();
		List<T> resultList = null;
		try {
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<T> cq = cb.createQuery(daoType);
			Root<T> root;
			root = cq.from(daoType);
			cq.select(root);

			List<Predicate> predicates = new ArrayList<>();

			if (attribute1 != null && value1 != null && !value1.isEmpty()) {
				predicates.add(root.get(attribute1).in(value1));
			}

			if (attribute2 != null && value2 != null) {
				predicates.add(cb.equal(root.get(attribute2), value2));
			}

			if (!predicates.isEmpty()) {
				cq.where(cb.and(predicates.toArray(new Predicate[0])));
			}

			if (orderBy != null) {
				// Assuming descending order as in original code; use cb.asc() for ascending
				cq.orderBy(cb.desc(root.get(orderBy)));
			}

			Query<T> query = session.createQuery(cq);

			if (offSet != null && offSet != -1) {
				query.setFirstResult(offSet);
			}
			if (limit != null && limit != -1) {
				query.setMaxResults(limit);
			}

			resultList = query.getResultList();

		} catch (Exception e) {
		} finally {
			session.close();
		}
		return resultList;
	}

	// Generic transaction execution wrapper
	protected <R> R executeInTransaction(HibernateTransaction<R> action) {
		Transaction tx = null;
		try (Session session = sessionFactory.openSession()) {
			tx = session.beginTransaction();
			R result = action.execute(session);
			tx.commit();
			return result;
		} catch (RuntimeException e) {
			if (tx != null)
				tx.rollback();
			throw e;
		}
	}

	@FunctionalInterface
	public interface HibernateTransaction<R> {
		R execute(Session session);
	}
}