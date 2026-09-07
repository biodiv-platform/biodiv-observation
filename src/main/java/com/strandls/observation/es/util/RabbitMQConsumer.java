/**
 * 
 */
package com.strandls.observation.es.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.DeliverCallback;
import com.rabbitmq.client.Recoverable;
import com.rabbitmq.client.RecoveryListener;
import com.strandls.esmodule.pojo.TaxonomyUpdateData;
import com.strandls.observation.service.Impl.RecommendationServiceImpl;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class RabbitMQConsumer {

	private final Logger logger = LoggerFactory.getLogger(RabbitMQConsumer.class);

	private final static String OBSERVATION_QUEUE = "observationQueue";
	private static final String TAXONOMY_QUEUE = "taxonomyQueue";

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private RecommendationServiceImpl recoService;

	@Inject
	private Connection connection;

	// Dedicated to consuming only, never touched by publisher code, so it is
	// safe for basicConsume's own dispatch thread(s) to own exclusively.
	private Channel consumerChannel;

	private final ObjectMapper objectMapper = new ObjectMapper();

	private synchronized Channel getConsumerChannel() throws Exception {
		if (consumerChannel == null || !consumerChannel.isOpen()) {
			consumerChannel = connection.createChannel();
		}
		return consumerChannel;
	}

	/**
	 * Subscribes both consumers and, since topology recovery is disabled on
	 * the shared {@link Connection} (see {@link com.strandls.observation.RabbitMqConnection}),
	 * re-subscribes them itself whenever the connection recovers from a drop -
	 * the broker forgets consumer registrations on disconnect, so this is a
	 * plain re-subscribe rather than a duplicate.
	 */
	public void startConsuming() throws Exception {
		elasticUpdate();
		listenToTaxonomyEvents();
		if (connection instanceof Recoverable) {
			((Recoverable) connection).addRecoveryListener(new RecoveryListener() {
				@Override
				public void handleRecovery(Recoverable recoverable) {
					try {
						elasticUpdate();
						listenToTaxonomyEvents();
						logger.info("Re-subscribed RabbitMQ consumers after connection recovery");
					} catch (Exception e) {
						logger.error("Failed to re-subscribe RabbitMQ consumers after recovery", e);
					}
				}

				@Override
				public void handleRecoveryStarted(Recoverable recoverable) {
				}
			});
		}
	}

	public void elasticUpdate() throws Exception {
		DeliverCallback deliverCallback = (consumerTag, delivery) -> {
			String message = new String(delivery.getBody(), "UTF-8");
			BasicProperties properties = delivery.getProperties();
			String updateType = properties.getType();
			System.out.println("----[RABBIT MQ CONSUMER]---");
			System.out.println("consuming observation Id :" + message);
			System.out.println("Updating :" + updateType);

			ESUpdateThread updateThread = new ESUpdateThread(esUpdate, message);
			Thread thread = new Thread(updateThread);
			thread.start();

		};
		getConsumerChannel().basicConsume(OBSERVATION_QUEUE, true, deliverCallback, consumerTag -> {
		});
	}

	public void listenToTaxonomyEvents() throws Exception {
		DeliverCallback deliverCallback = (consumerTag, delivery) -> {
			String message = new String(delivery.getBody(), "UTF-8");
			System.out.println("----[OBSERVATION TAXONOMY EVENT]----");
			System.out.println("Received: " + message);
			TaxonomyUpdateData event = objectMapper.readValue(message, TaxonomyUpdateData.class);
			recoService.handleTaxonByName(event);

		};

		getConsumerChannel().basicConsume(TAXONOMY_QUEUE, true, deliverCallback, consumerTag -> {
		});
	}

}
