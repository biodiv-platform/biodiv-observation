package com.strandls.observation.dao;

import javax.inject.Inject;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.pojo.Dataset;
import com.strandls.observation.util.AbstractDAO;

/**	
 * 
 * @author vishnu
 *
 */
public class DataSetDAO extends AbstractDAO<Dataset, Long> {

	private static final Logger logger = LoggerFactory.getLogger(DataSetDAO.class);

	@Inject
	protected DataSetDAO(SessionFactory sessionFactory) {
		super(sessionFactory);
	}

	@Override
	public Dataset findById(Long id) {
		Session session = sessionFactory.openSession();
		Dataset entity = null;
		try {
			entity = session.get(Dataset.class, id);
		} catch (Exception e) {
			logger.info(e.getMessage());
			logger.error(e.toString());
		} finally {
			session.close();
		}
		return entity;
	}

	@SuppressWarnings("unchecked")
	public Dataset findDataSetByTitle(String title) {
		Session session = sessionFactory.openSession();
		String qry = "select * from dataset where is_deleted = false and title =" + title;
		Dataset result = null;
		try {
			Query<Dataset> query = session.createNativeQuery(qry);
			result = query.getSingleResult();
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return result;
	}

}
