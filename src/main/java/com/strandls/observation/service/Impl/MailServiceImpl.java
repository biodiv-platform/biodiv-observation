package com.strandls.observation.service.Impl;

import java.util.HashMap;
import java.util.Map;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.rabbitmq.client.Channel;
import com.strandls.mail_utility.model.EnumModel.FIELDS;
import com.strandls.mail_utility.model.EnumModel.MAIL_TYPE;
import com.strandls.mail_utility.producer.RabbitMQProducer;
import com.strandls.mail_utility.util.JsonUtil;
import com.strandls.observation.RabbitMqConnection;
import com.strandls.observation.service.MailService;
import com.strandls.observation.util.PropertyFileUtil;

public class MailServiceImpl implements MailService {
	
	private static final Logger logger = LoggerFactory.getLogger(MailServiceImpl.class);
	
	@Inject
	Channel channel;

	@Override
	public void sendMail() {
		Map<String, Object> data = new HashMap<String, Object>();
		data.put(FIELDS.TYPE.getAction(), MAIL_TYPE.DOWNLOAD_MAIL.getAction());
//		data.put(FIELDS.TO.getAction(), new String[] { user.getEmail() });
		Map<String, Object> model = new HashMap<String, Object>();

		data.put(FIELDS.DATA.getAction(), JsonUtil.unflattenJSON(model));
		RabbitMQProducer producer = new RabbitMQProducer(channel);
		try {
			producer.produceMail(RabbitMqConnection.MAIL_EXCHANGE, RabbitMqConnection.MAIL_ROUTING_KEY, null,
					JsonUtil.mapToJSON(data));
			String admins = PropertyFileUtil.fetchProperty("config.properties", "mail_bcc");
			data.put(FIELDS.TO.getAction(), admins.split(","));
			producer.produceMail(RabbitMqConnection.MAIL_EXCHANGE, RabbitMqConnection.MAIL_ROUTING_KEY, null,
					JsonUtil.mapToJSON(data));
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
	}

}
