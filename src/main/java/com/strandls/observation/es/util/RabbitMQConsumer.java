/**
 * 
 */
package com.strandls.observation.es.util;

import com.google.inject.Inject;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.DeliverCallback;

/**
 * @author Abhishek Rudra
 *
 */
public class RabbitMQConsumer {

	private final static String QUEUE_ELASTIC = "elastic";

	@Inject
	private ESUpdate esUpdate;

	@Inject
	private Channel channel;

	public void elasticUpdate() throws Exception {
		DeliverCallback deliverCallback = (consumerTag, delivery) -> {
			String message = new String(delivery.getBody(), "UTF-8");
			BasicProperties properties = delivery.getProperties();
			String updateType = properties.getType();
			System.out.println("Updating :" + updateType);
			esUpdate.updateESInstance(message);

		};
		channel.basicConsume(QUEUE_ELASTIC, true, deliverCallback, consumerTag -> {
		});
	}

}
