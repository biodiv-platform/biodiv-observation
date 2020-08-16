package com.strandls.observation.dao;

import javax.inject.Inject;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.observation.pojo.DataTable;
import com.strandls.observation.util.AbstractDAO;

public class DataTableDAO extends AbstractDAO<DataTable, Long> {
	
	private static final Logger logger = LoggerFactory.getLogger(DataTableDAO.class);

	@Inject
	protected DataTableDAO(SessionFactory sessionFactory) {
		super(sessionFactory);
	}

	@Override
	public DataTable findById(Long id) {
		Session session = sessionFactory.openSession();
		DataTable entity = null;
		try {
			entity = session.get(DataTable.class, id);
		} catch (Exception e) {
			logger.info(e.getMessage());
			logger.error(e.toString());
		} finally {
			session.close();
		}
		return entity;
	}

}
