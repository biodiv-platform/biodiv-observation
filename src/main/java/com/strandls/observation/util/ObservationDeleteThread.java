package com.strandls.observation.util;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.strandls.esmodule.ApiException;
import com.strandls.observation.pojo.Observation;
import com.strandls.observation.service.Impl.ObservationServiceImpl;

/**
 * Temporary used for delete bulk observation, remove onve es-mod get bulk
 * delete
 * 
 * @author vishnu
 *
 */
public class ObservationDeleteThread implements Runnable {
	private final Logger logger = LoggerFactory.getLogger(ObservationDeleteThread.class);
	private final List<Observation> observationList;
	private final ObservationServiceImpl observationImpl;
	private final HttpServletRequest request;

	public ObservationDeleteThread(List<Observation> observationList, ObservationServiceImpl observationImpl,
			HttpServletRequest request) {
		this.observationImpl = observationImpl;
		this.observationList = observationList;
		this.request = request;
	}

	@Override
	public void run() {
		observationList.forEach((item) -> {
			try {
				observationImpl.deleteObservation(request, item, false);
			} catch (ApiException e) {
				logger.error(e.getMessage());
			}
		});
	}

}
