/**
 * 
 */
package com.strandls.observation.es.util;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;

import jakarta.inject.Inject;

/**
 * @author Abhishek Rudra
 *
 */
public class RabbitMQProducer {

	private static final String EXCHANGE_BIODIV = "biodiv";

	@Inject
	private Channel channel;

	public void setMessage(final String routingKey, String message, String updateType) throws Exception {
	    String threadName = Thread.currentThread().getName();
	    long start = System.nanoTime();
	    System.out.println("[PUBLISH START] thread=" + threadName + " obsId=" + message + " t=" + start);

	    BasicProperties properties = new BasicProperties(null, null, null, null, null, null, null, null, null, null,
	            updateType, null, null, null);
	    channel.basicPublish(EXCHANGE_BIODIV, routingKey, properties, message.getBytes("UTF-8"));

	    long end = System.nanoTime();
	    System.out.println("[PUBLISH END]   thread=" + threadName + " obsId=" + message + " durationNs=" + (end - start));
	}

}
