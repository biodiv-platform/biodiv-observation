/**
 * 
 */
package com.strandls.observation.es.util;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.DeliverCallback;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class RabbitMQConsumer {

	private final static String OBSERVATION_QUEUE = "observationQueue";

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private Channel channel;

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

}
