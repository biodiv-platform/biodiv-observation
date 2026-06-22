/**
 * 
 */
package com.strandls.observation.es.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.DeliverCallback;
import com.strandls.esmodule.pojo.TaxonomyUpdateData;
import com.strandls.observation.service.Impl.RecommendationServiceImpl;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class RabbitMQConsumer {

	private final static String OBSERVATION_QUEUE = "observationQueue";
	private static final String TAXONOMY_QUEUE = "taxonomyQueue";

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private RecommendationServiceImpl recoService;

	@Inject
	private Channel channel;

	private final ObjectMapper objectMapper = new ObjectMapper();

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
		channel.basicConsume(OBSERVATION_QUEUE, true, deliverCallback, consumerTag -> {
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

		channel.basicConsume(TAXONOMY_QUEUE, true, deliverCallback, consumerTag -> {
		});
	}

}
