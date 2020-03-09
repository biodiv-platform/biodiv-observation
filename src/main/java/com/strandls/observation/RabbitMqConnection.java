/**
 * 
 */
package com.strandls.observation;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.concurrent.TimeoutException;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

/**
 * @author Abhishek Rudra
 *
 */
public class RabbitMqConnection {

	private final static String EXCHANGE_BIODIV = "biodiv";
	private final static String QUEUE_ELASTIC = "elastic";
	private final static String ROUTING_ELASTIC = "esmodule";

	public Channel setRabbitMQConnetion() throws IOException, TimeoutException {

		InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

		Properties properties = new Properties();
		try {
			properties.load(in);
		} catch (IOException e) {
			e.printStackTrace();
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

		return channel;

	}
}
