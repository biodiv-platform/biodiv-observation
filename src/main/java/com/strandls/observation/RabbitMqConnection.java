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
 * Owns the single, long-lived RabbitMQ {@link Connection} for this
 * application. Callers should not ask this class for a {@link Channel} to
 * hold on to; instead get the connection via {@link #connect()} and obtain
 * channels through {@link RabbitChannelProvider}, which hands out one channel
 * per thread as RabbitMQ's client requires.
 *
 * @author Abhishek Rudra
 *
 */
public class RabbitMqConnection {

	private final Logger logger = LoggerFactory.getLogger(RabbitMqConnection.class);

	private final static String OBSERVATION_QUEUE = "observationQueue";
	private final static String ROUTING_OBSERVATION = "observation";

	public static final String TAXONOMY_QUEUE = "taxonomyQueue";
	public static final String TAXONOMY_ROUTING_KEY = "taxonomy.updated";

	public final static String EXCHANGE_BIODIV;

	public static final String MAIL_QUEUE;
	public static final String MAIL_ROUTING_KEY;

	private static final int MAX_CONNECT_ATTEMPTS = 5;
	private static final long INITIAL_BACKOFF_MILLIS = 2000L;
	private static final long MAX_BACKOFF_MILLIS = 30000L;

	static {
		Properties properties = PropertyFileUtil.fetchProperty("config.properties");
		EXCHANGE_BIODIV = properties.getProperty("rabbitmq_exchange");
		MAIL_QUEUE = properties.getProperty("rabbitmq_queue");
		MAIL_ROUTING_KEY = properties.getProperty("rabbitmq_routingKey");
	}

	/**
	 * Opens the single application-wide connection, retrying with backoff if
	 * the broker isn't reachable yet, and declares the exchange/queue
	 * topology once. The returned connection has automatic recovery enabled,
	 * so it will reconnect (and re-declare topology) on its own if the
	 * network drops later.
	 */
	public Connection connect() throws IOException, TimeoutException {

		ConnectionFactory factory = buildConnectionFactory();

		long backoff = INITIAL_BACKOFF_MILLIS;
		for (int attempt = 1; attempt <= MAX_CONNECT_ATTEMPTS; attempt++) {
			try {
				Connection connection = factory.newConnection("biodiv-observation");
				connection.addShutdownListener(cause -> {
					if (!cause.isInitiatedByApplication()) {
						logger.error("RabbitMQ connection to {}:{} closed unexpectedly: {}", factory.getHost(),
								factory.getPort(), cause.getMessage());
					}
				});
				logger.info("Connected to RabbitMQ at {}:{} (attempt {}/{})", factory.getHost(), factory.getPort(),
						attempt, MAX_CONNECT_ATTEMPTS);
				declareTopology(connection);
				return connection;
			} catch (IOException | TimeoutException e) {
				if (attempt == MAX_CONNECT_ATTEMPTS) {
					logger.error("Could not connect to RabbitMQ at {}:{} after {} attempts", factory.getHost(),
							factory.getPort(), MAX_CONNECT_ATTEMPTS);
					throw e;
				}
				logger.warn("RabbitMQ connection attempt {}/{} failed ({}); retrying in {} ms", attempt,
						MAX_CONNECT_ATTEMPTS, e.getMessage(), backoff);
				sleep(backoff);
				backoff = Math.min(backoff * 2, MAX_BACKOFF_MILLIS);
			}
		}

		// Unreachable: the loop above always either returns or throws on the last attempt.
		throw new IOException("Failed to connect to RabbitMQ after " + MAX_CONNECT_ATTEMPTS + " attempts");
	}

	private ConnectionFactory buildConnectionFactory() throws IOException {

		InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");

		Properties properties = new Properties();
		try {
			properties.load(in);
		} finally {
			in.close();
		}

		String rabbitmqHost = properties.getProperty("rabbitmq_host");
		Integer rabbitmqPort = Integer.parseInt(properties.getProperty("rabbitmq_port"));
		String rabbitmqUsername = properties.getProperty("rabbitmq_username");
		String rabbitmqPassword = properties.getProperty("rabbitmq_password");

		ConnectionFactory factory = new ConnectionFactory();
		factory.setHost(rabbitmqHost);
		factory.setPort(rabbitmqPort);
		factory.setUsername(rabbitmqUsername);
		factory.setPassword(rabbitmqPassword);

		// Explicit even though these are the client defaults: reconnect and
		// re-declare exchanges/queues/bindings/consumers automatically if the
		// connection drops after startup.
		factory.setAutomaticRecoveryEnabled(true);
		factory.setTopologyRecoveryEnabled(true);
		factory.setNetworkRecoveryInterval(5000);
		factory.setConnectionTimeout(10000);
		factory.setRequestedHeartbeat(30);

		return factory;
	}

	private void declareTopology(Connection connection) throws IOException {
		try (Channel setupChannel = connection.createChannel()) {
			setupChannel.exchangeDeclare(EXCHANGE_BIODIV, "direct");
			setupChannel.queueDeclare(OBSERVATION_QUEUE, false, false, false, null);
			setupChannel.queueBind(OBSERVATION_QUEUE, EXCHANGE_BIODIV, ROUTING_OBSERVATION);
			setupChannel.queueDeclare(MAIL_QUEUE, false, false, false, null);
			setupChannel.queueBind(MAIL_QUEUE, EXCHANGE_BIODIV, MAIL_ROUTING_KEY);
			setupChannel.queueDeclare(TAXONOMY_QUEUE, false, false, false, null);
			setupChannel.queueBind(TAXONOMY_QUEUE, EXCHANGE_BIODIV, TAXONOMY_ROUTING_KEY);
		} catch (TimeoutException e) {
			throw new IOException("Timed out declaring RabbitMQ topology", e);
		}
	}

	private void sleep(long millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}
}
