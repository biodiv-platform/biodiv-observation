/**
 * 
 */
package com.strandls.observation;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.concurrent.TimeoutException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.strandls.observation.util.PropertyFileUtil;

/**
 * @author Abhishek Rudra
 *
 */
public class RabbitMqConnection {

	private final Logger logger = LoggerFactory.getLogger(RabbitMqConnection.class);

	private final String QUEUE_ELASTIC = "elastic";
	private final String ROUTING_ELASTIC = "esmodule";

	public static final String EXCHANGE_BIODIV;
	public static final String MAIL_QUEUE;
	public static final String MAIL_ROUTING_KEY;

	static {
		Properties properties = PropertyFileUtil.fetchProperty("config.properties");
		EXCHANGE_BIODIV = properties.getProperty("rabbitmq_exchange");
		MAIL_QUEUE = properties.getProperty("rabbitmq_queue");
		MAIL_ROUTING_KEY = properties.getProperty("rabbitmq_routingKey");
	}

	public Channel setRabbitMQConnetion() throws IOException, TimeoutException {

		InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

		Properties properties = new Properties();
		try {
			properties.load(in);
		} catch (IOException e) {
			logger.error(e.getMessage());
		}

		String rabbitmqHost = properties.getProperty("rabbitmq_host");
		Integer rabbitmqPort = Integer.parseInt(properties.getProperty("rabbitmq_port"));
		String rabbitmqUsername = properties.getProperty("rabbitmq_username");
		String rabbitmqPassword = properties.getProperty("rabbitmq_password");
		in.close();

		ConnectionFactory factory = new ConnectionFactory();
		factory.setHost(rabbitmqHost);
		factory.setPort(rabbitmqPort);
		factory.setUsername(rabbitmqUsername);
		factory.setPassword(rabbitmqPassword);
		Connection connection = factory.newConnection();
		Channel channel = connection.createChannel();
		channel.exchangeDeclare(EXCHANGE_BIODIV, "direct");
		channel.queueDeclare(QUEUE_ELASTIC, false, false, false, null);
		channel.queueBind(QUEUE_ELASTIC, EXCHANGE_BIODIV, ROUTING_ELASTIC);
		channel.queueDeclare(MAIL_QUEUE, false, false, false, null);
		channel.queueBind(MAIL_QUEUE, EXCHANGE_BIODIV, MAIL_ROUTING_KEY);

		return channel;

	}
}
