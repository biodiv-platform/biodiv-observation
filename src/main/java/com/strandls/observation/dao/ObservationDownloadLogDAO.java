/**
 * 
 */
package com.strandls.observation.dao;

import javax.inject.Inject;

import org.hibernate.SessionFactory;

import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.util.AbstractDAO;

/**
 * @author ashish
 *
 */
public class ObservationDownloadLogDAO extends AbstractDAO<DownloadLog, Long>{

	
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
