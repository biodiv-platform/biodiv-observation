/**
 * 
 */
package com.strandls.observation.dao;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationDAOModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(ObservationDAO.class).in(Scopes.SINGLETON);
		bind(RecommendationDao.class).in(Scopes.SINGLETON);
		bind(RecommendationVoteDao.class).in(Scopes.SINGLETON);
		bind(ObservationDownloadLogDAO.class).in(Scopes.SINGLETON);
		bind(DataTableDAO.class).in(Scopes.SINGLETON);
		bind(DataSetDAO.class).in(Scopes.SINGLETON);
	}

}
