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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Scopes;
import com.google.inject.servlet.GuiceServletContextListener;
import com.strandls.activity.controller.ActivitySerivceApi;
import com.strandls.authentication_utility.filter.FilterModule;
import com.strandls.esmodule.controllers.EsServicesApi;
import com.strandls.naksha.controller.LayerServiceApi;
import com.strandls.observation.contorller.ObservationControllerModule;
import com.strandls.observation.dao.ObservationDAOModule;
import com.strandls.observation.service.Impl.ObservationServiceModule;
import com.strandls.resource.controllers.ResourceServicesApi;
import com.strandls.taxonomy.controllers.TaxonomyServicesApi;
import com.strandls.traits.controller.TraitsServiceApi;
import com.strandls.user.controller.UserServiceApi;
import com.strandls.userGroup.controller.UserGroupSerivceApi;
import com.strandls.utility.controller.UtilityServiceApi;
import com.sun.jersey.guice.JerseyServletModule;
import com.sun.jersey.guice.spi.container.servlet.GuiceContainer;

/**
 * @author Abhishek Rudra
 *
 */
public class ObservationServeletContextListener extends GuiceServletContextListener {

	private static final Logger logger = LoggerFactory.getLogger(ObservationServeletContextListener.class);

	@Override
	protected Injector getInjector() {

		Injector injector = Guice.createInjector(new JerseyServletModule() {
			@Override
			protected void configureServlets() {

				Configuration configuration = new Configuration();

				try {
					for (Class<?> cls : getEntityClassesFromPackage("com")) {
						configuration.addAnnotatedClass(cls);
					}
				} catch (ClassNotFoundException | IOException | URISyntaxException e) {
					e.printStackTrace();
					logger.error(e.getMessage());
				}

				configuration = configuration.configure();
				SessionFactory sessionFactory = configuration.buildSessionFactory();

				Map<String, String> props = new HashMap<String, String>();
				props.put("javax.ws.rs.Application", ApplicationConfig.class.getName());
				props.put("jersey.config.server.wadl.disableWadl", "true");

				ObjectMapper objectMapper = new ObjectMapper();
				bind(ObjectMapper.class).toInstance(objectMapper);

				bind(SessionFactory.class).toInstance(sessionFactory);
				bind(TraitsServiceApi.class).in(Scopes.SINGLETON);
				bind(ResourceServicesApi.class).in(Scopes.SINGLETON);
				bind(TaxonomyServicesApi.class).in(Scopes.SINGLETON);
				bind(UserGroupSerivceApi.class).in(Scopes.SINGLETON);
				bind(LayerServiceApi.class).in(Scopes.SINGLETON);
				bind(EsServicesApi.class).in(Scopes.SINGLETON);
				bind(UtilityServiceApi.class).in(Scopes.SINGLETON);
				bind(UserServiceApi.class).in(Scopes.SINGLETON);
				bind(ActivitySerivceApi.class).in(Scopes.SINGLETON);
				serve("/api/*").with(GuiceContainer.class, props);
				filter("/*").through(SwaggerFilter.class);

			}
		}, new ObservationControllerModule(), new FilterModule(), new ObservationDAOModule(),
				new ObservationServiceModule());

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
}
