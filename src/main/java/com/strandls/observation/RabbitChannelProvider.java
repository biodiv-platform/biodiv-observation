/**
 *
 */
package com.strandls.observation;

import java.io.IOException;
import java.io.UncheckedIOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;

import jakarta.inject.Inject;

/**
 * Hands out one RabbitMQ {@link Channel} per calling thread, opened lazily
 * from the shared {@link Connection} and reused for the lifetime of that
 * thread.
 *
 * RabbitMQ's Java client documents that a Channel must not be used
 * concurrently by more than one thread - doing so can interleave frames from
 * different calls on the wire and cause the broker to close the connection.
 * Callers must always publish via {@code get()} rather than caching the
 * returned Channel themselves, since a cached reference would defeat the
 * per-thread isolation this class provides.
 */
public class RabbitChannelProvider {

	private final Logger logger = LoggerFactory.getLogger(RabbitChannelProvider.class);

	private final Connection connection;

	private final ThreadLocal<Channel> threadLocalChannel = new ThreadLocal<>();

	@Inject
	public RabbitChannelProvider(Connection connection) {
		this.connection = connection;
	}

	public Channel get() {
		Channel channel = threadLocalChannel.get();
		if (channel == null || !channel.isOpen()) {
			try {
				channel = connection.createChannel();
				threadLocalChannel.set(channel);
				logger.debug("Opened RabbitMQ channel for thread {}", Thread.currentThread().getName());
			} catch (IOException e) {
				throw new UncheckedIOException(
						"Failed to open RabbitMQ channel for thread " + Thread.currentThread().getName(), e);
			}
		}
		return channel;
	}
}
