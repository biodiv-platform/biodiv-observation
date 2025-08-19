/**
 * 
 */
package com.strandls.observation.dao;

import org.hibernate.SessionFactory;

import com.strandls.observation.pojo.DownloadLog;
import com.strandls.observation.util.AbstractDAO;

import jakarta.inject.Inject;

/**
 * @author ashish
 *
 */
public class ObservationDownloadLogDAO extends AbstractDAO<DownloadLog, Long> {

	@Inject
	protected ObservationDownloadLogDAO(SessionFactory sessionFactory) {
		super(sessionFactory);
	}

	@Override
	public DownloadLog findById(Long id) {
		return null;
	}

}
