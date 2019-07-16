package com.strandls.observation.util;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.NoResultException;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.query.Query;

public abstract class AbstractDAO<T, K extends Serializable> {
	
	protected SessionFactory sessionFactory;

	protected Class<? extends T> daoType;

	@SuppressWarnings("unchecked")
	protected AbstractDAO(SessionFactory sessionFactory){
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
		Session session = sessionFactory.openSession();
		Criteria criteria = session.createCriteria(daoType);
		List<T> entities = criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY).list();
		return entities;
	}

	@SuppressWarnings({ "unchecked", "deprecation" })
	public List<T> findAll(int limit, int offset) {
		Session session = sessionFactory.openSession();
		Criteria criteria = session.createCriteria(daoType).setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		List<T> entities = criteria.setFirstResult(offset).setMaxResults(limit).list();
		return entities;
	}

	// TODO:improve this to do dynamic finder on any property
	@SuppressWarnings("unchecked")
	public T findByPropertyWithCondition(String property, String value, String condition) {
		String queryStr = "" + "from " + daoType.getSimpleName() + " t " + "where t." + property + " " + condition
				+ " :value";
		Session session = sessionFactory.openSession();
		Query<T> query = session.createQuery(queryStr);
		query.setParameter("value", value);

		T entity = null;
		try {
			entity = (T) query.getSingleResult();
		} catch (NoResultException e) {
			throw e;
		}
		session.close();
		return entity;

	}

	@SuppressWarnings("unchecked")
	public List<T> getByPropertyWithCondtion(String property, Object value, String condition, int limit, int offset) {
		String queryStr = "" + "from " + daoType.getSimpleName() + " t " + "where t." + property + " " + condition
				+ " :value" + " order by id";
		Session session = sessionFactory.openSession();
		Query<T> query = session.createQuery(queryStr);
		query.setParameter("value", value);

		List<T> resultList = new ArrayList<T>();
		try {
			if (limit > 0 && offset >= 0)
				query = query.setFirstResult(offset).setMaxResults(limit);
			resultList = query.getResultList();

		} catch (NoResultException e) {
			throw e;
		}
		session.close();
		return resultList;
	}

}
