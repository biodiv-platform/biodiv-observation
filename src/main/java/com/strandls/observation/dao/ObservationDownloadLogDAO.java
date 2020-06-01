/**
 * 
 */
package com.strandls.observation.dao;

import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;
import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.util.AbstractDAO;

/**
 * @author ashish
 *
 */
public class ObservationDownloadLogDAO extends AbstractDAO<DownloadLog, Long>{

	private static final Logger logger = LoggerFactory.getLogger(ObservationDownloadLogDAO.class);
	
	@Inject
	protected ObservationDownloadLogDAO(SessionFactory sessionFactory) {
		super(sessionFactory);
	}

	@Override
	public DownloadLog findById(Long id) {
		// TODO Auto-generated method stub
		return null;
	}

}
