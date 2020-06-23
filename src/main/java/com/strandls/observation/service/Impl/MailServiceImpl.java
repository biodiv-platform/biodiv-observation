package com.strandls.observation.service.Impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.rabbitmq.client.Channel;
import com.strandls.mail_utility.model.EnumModel.DOWNLOAD_MAIL;
import com.strandls.mail_utility.model.EnumModel.FIELDS;
import com.strandls.mail_utility.model.EnumModel.MAIL_TYPE;
import com.strandls.mail_utility.producer.RabbitMQProducer;
import com.strandls.mail_utility.util.JsonUtil;
import com.strandls.observation.RabbitMqConnection;
import com.strandls.observation.service.MailService;
import com.strandls.observation.util.PropertyFileUtil;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.user.pojo.User;

public class MailServiceImpl implements MailService {
	
	private static final Logger logger = LoggerFactory.getLogger(MailServiceImpl.class);
	
	@Inject
	Channel channel;
	
	@Inject
	UserServiceApi userServiceApi;
	
	@Override
	public void sendMail(String authorId, String fileName, String type) {
		try {
			User user = userServiceApi.getUser(authorId);
		
		Properties properties = PropertyFileUtil.fetchProperty("config.properties");
		Map<String, Object> data = new HashMap<String, Object>();
		data.put(FIELDS.TYPE.getAction(), MAIL_TYPE.DOWNLOAD_MAIL.getAction());
		data.put(FIELDS.TO.getAction(), user.getEmail());
		Map<String, Object> model = new HashMap<String, Object>();
		model.put(DOWNLOAD_MAIL.SERVER_URL.getAction(), properties.getProperty("serverUrl"));
		model.put(DOWNLOAD_MAIL.SITENAME.getAction(), properties.getProperty("siteName"));
		model.put(DOWNLOAD_MAIL.USER_DATA.getAction(), user);
		model.put(DOWNLOAD_MAIL.DOWNLOAD_TYPE.getAction(), type);
		model.put(DOWNLOAD_MAIL.DOWNLOAD_FILE.getAction(), fileName);



		data.put(FIELDS.DATA.getAction(), JsonUtil.unflattenJSON(model));
		RabbitMQProducer producer = new RabbitMQProducer(channel);
		if (user.getEmail() != null && !user.getEmail().isEmpty()
				&& !user.getEmail().contains("@ibp.org") && user.getSendNotification() != null
				&& user.getSendNotification()) {
			producer.produceMail(RabbitMqConnection.EXCHANGE_BIODIV, RabbitMqConnection.MAIL_ROUTING_KEY, null,
					JsonUtil.mapToJSON(data));
		}
			String admins = PropertyFileUtil.fetchProperty("config.properties", "mail_bcc");
			data.put(FIELDS.TO.getAction(), admins.split(","));
			producer.produceMail(RabbitMqConnection.EXCHANGE_BIODIV, RabbitMqConnection.MAIL_ROUTING_KEY, null,
					JsonUtil.mapToJSON(data));
	}
		 catch (Exception e) {
			logger.error(e.getMessage());
		}
	}

}
