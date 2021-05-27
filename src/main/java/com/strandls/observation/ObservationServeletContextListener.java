/**
 * 
 */
package com.strandls.observation;

import java.io.File;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletContextEvent;

import org.glassfish.jersey.servlet.ServletContainer;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Scopes;
import com.google.inject.servlet.GuiceServletContextListener;
import com.google.inject.servlet.ServletModule;
import com.rabbitmq.client.Channel;
import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.file.api.UploadApi;
import com.strandls.mail_utility.producer.RabbitMQProducer;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.observation.contorller.ObservationControllerModule;
import com.strandls.observation.dao.ObservationDAOModule;
import com.strandls.observation.es.util.ESUtilModule;
import com.strandls.observation.es.util.RabbitMQConsumer;
import com.strandls.observation.service.Impl.DatasetConfigHelper;
import com.strandls.observation.service.Impl.ObservationServiceModule;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.taxonomy.controllers.SpeciesServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyTreeServicesApi;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.controller.CustomFieldServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.utility.controller.UtilityServiceApi;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServeletContextListener extends GuiceServletContextListener {

	private final Logger logger = LoggerFactory.getLogger(ObservationServeletContextListener.class);

	@Override
	protected Injector getInjector() {

		Injector injector = Guice.createInjector(new ServletModule() {
			@Override
			protected void configureServlets() {

				Configuration configuration = new Configuration();

				try {
					for (Class<?> cls : getEntityClassesFromPackage("com")) {
						configuration.addAnnotatedClass(cls);
					}
				} catch (ClassNotFoundException | IOException | URISyntaxException e) {
					logger.error(e.getMessage());
				}

				configuration = configuration.configure();
				SessionFactory sessionFactory = configuration.buildSessionFactory();

//				Rabbit MQ initialization
				RabbitMqConnection rabbitConnetion = new RabbitMqConnection();
				Channel channel = null;
				try {
					channel = rabbitConnetion.setRabbitMQConnetion();
				} catch (Exception e) {
					logger.error(e.getMessage());
				}

				bind(Channel.class).toInstance(channel);
				RabbitMQProducer producer = new RabbitMQProducer(channel);
				Map<String, String> props = new HashMap<String, String>();
				props.put("javax.ws.rs.Application", ApplicationConfig.class.getName());
				props.put("jersey.config.server.provider.packages", "com");
				props.put("jersey.config.server.wadl.disableWadl", "true");

				ObjectMapper objectMapper = new ObjectMapper();
				bind(ObjectMapper.class).toInstance(objectMapper);

				bind(SessionFactory.class).toInstance(sessionFactory);
				bind(TraitsServiceApi.class).in(Scopes.SINGLETON);
				bind(RabbitMQProducer.class).toInstance(producer);
				bind(ResourceServicesApi.class).in(Scopes.SINGLETON);
				bind(TaxonomyServicesApi.class).in(Scopes.SINGLETON);
				bind(SpeciesServicesApi.class).in(Scopes.SINGLETON);
				bind(TaxonomyTreeServicesApi.class).in(Scopes.SINGLETON);
				bind(UserGroupSerivceApi.class).in(Scopes.SINGLETON);
				bind(CustomFieldServiceApi.class).in(Scopes.SINGLETON);
				bind(LayerServiceApi.class).in(Scopes.SINGLETON);
				bind(EsServicesApi.class).in(Scopes.SINGLETON);
				bind(UtilityServiceApi.class).in(Scopes.SINGLETON);
				bind(UserServiceApi.class).in(Scopes.SINGLETON);
				bind(ActivitySerivceApi.class).in(Scopes.SINGLETON);
				bind(UploadApi.class).in(Scopes.SINGLETON);
				bind(Headers.class).in(Scopes.SINGLETON);
				bind(ServletContainer.class).in(Scopes.SINGLETON);
				bind(RabbitMQProducer.class).toInstance(producer);
				serve("/api/*").with(ServletContainer.class, props);

			}
		}, new ObservationControllerModule(), new ObservationDAOModule(), new ObservationServiceModule(),
				new ESUtilModule());

		try {
			injector.getInstance(RabbitMQConsumer.class).elasticUpdate();
			injector.getInstance(DatasetConfigHelper.class).createStandaloneRecord("standalone_dataset");
		} catch (Exception e) {
			logger.error(e.getMessage());
		}

		return injector;

	}

	protected List<Class<?>> getEntityClassesFromPackage(String packageName)
			throws URISyntaxException, IOException, ClassNotFoundException {

		List<String> classNames = getClassNamesFromPackage(packageName);
		List<Class<?>> classes = new ArrayList<Class<?>>();
		for (String className : classNames) {
			// logger.info(className);
			Class<?> cls = Class.forName(className);
			Annotation[] annotations = cls.getAnnotations();

			for (Annotation annotation : annotations) {
				if (annotation instanceof javax.persistence.Entity) {
					System.out.println("Mapping entity :" + cls.getCanonicalName());
					classes.add(cls);
				}
			}
		}

		return classes;
	}

	private static ArrayList<String> getClassNamesFromPackage(final String packageName)
			throws URISyntaxException, IOException {

		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
		ArrayList<String> names = new ArrayList<String>();
		URL packageURL = classLoader.getResource(packageName);

		URI uri = new URI(packageURL.toString());
		File folder = new File(uri.getPath());

		Files.find(Paths.get(folder.getAbsolutePath()), 999, (p, bfa) -> bfa.isRegularFile()).forEach(file -> {
			String name = file.toFile().getAbsolutePath().replaceAll(folder.getAbsolutePath() + File.separatorChar, "")
					.replace(File.separatorChar, '.');
			if (name.indexOf('.') != -1) {
				name = packageName + '.' + name.substring(0, name.lastIndexOf('.'));
				names.add(name);
			}
		});

		return names;
	}

	@Override
	public void contextDestroyed(ServletContextEvent servletContextEvent) {

		Injector injector = (Injector) servletContextEvent.getServletContext().getAttribute(Injector.class.getName());

		SessionFactory sessionFactory = injector.getInstance(SessionFactory.class);
		sessionFactory.close();

		Channel channel = injector.getInstance(Channel.class);
		try {
			channel.getConnection().close();
		} catch (IOException e) {
			logger.error(e.getMessage());
		}

		super.contextDestroyed(servletContextEvent);
		// ... First close any background tasks which may be using the DB ...
		// ... Then close any DB connection pools ...

		// Now deregister JDBC drivers in this context's ClassLoader:
		// Get the webapp's ClassLoader
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		// Loop through all drivers
		Enumeration<Driver> drivers = DriverManager.getDrivers();
		while (drivers.hasMoreElements()) {
			Driver driver = drivers.nextElement();
			if (driver.getClass().getClassLoader() == cl) {
				// This driver was registered by the webapp's ClassLoader, so deregister it:
				try {
					logger.info("Deregistering JDBC driver {}", driver);
					DriverManager.deregisterDriver(driver);
				} catch (SQLException ex) {
					logger.error("Error deregistering JDBC driver {}", driver, ex);
				}
			} else {
				// driver was not registered by the webapp's ClassLoader and may be in use
				// elsewhere
				logger.trace("Not deregistering JDBC driver {} as it does not belong to this webapp's ClassLoader",
						driver);
			}
		}

	}
}
