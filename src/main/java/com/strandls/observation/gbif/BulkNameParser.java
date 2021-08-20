package com.strandls.observation.gbif;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.strandls.utility.pojo.ParsedName;

public class BulkNameParser {
	private Logger logger = LoggerFactory.getLogger(BulkNameParser.class);

	private CloseableHttpClient httpClient = HttpClients.createDefault();

	public List<ParsedName> findParsedNames(List<String> scienntificNames) {
		ObjectMapper objectMapper = new ObjectMapper()
				  .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

		List<ParsedName> finalResult = Arrays.asList(new ParsedName[scienntificNames.size()]);
		System.out.println(scienntificNames.size());

		Map<String, List<Integer>> indexOf = new HashMap<>();

		for (int index = 0; index < scienntificNames.size(); index++) {
			if (!indexOf.containsKey(scienntificNames.get(index))) {
				List<Integer> indexValue = new ArrayList<>();
				indexValue.add(index);
				indexOf.put(scienntificNames.get(index), indexValue);
			} else {
				List<Integer> indices = indexOf.get(scienntificNames.get(index));
				indices.add(index);
				indexOf.put(scienntificNames.get(index), indices);
			}
		}

		String names = StringUtils.join(scienntificNames, "|");
		URIBuilder builder = new URIBuilder();
		builder.setScheme("http").setHost("localhost:9091").setPath("/api").setParameter("q", names);

		List<ParsedName> parsedName = null;

		URI uri = null;
		try {
			uri = builder.build();
			System.out.println(uri);
			HttpGet request = new HttpGet(uri);

			try (CloseableHttpResponse response = httpClient.execute(request)) {
				System.out.println(response.getStatusLine().toString());

				HttpEntity entity = response.getEntity();
				Header headers = entity.getContentType();
				System.out.println(headers);

				if (entity != null) {
					// return it as a String
					String result = EntityUtils.toString(entity);
					parsedName = Arrays.asList(objectMapper.readValue(result, ParsedName[].class));
					// System.out.println(entity.getClass());
				}

			} catch (Exception e) {
				logger.error(e.getMessage());
			}

		} catch (URISyntaxException e1) {
			logger.error(e1.getMessage());
		}

		for (ParsedName name : parsedName) {
			// finalResult.set(indexOf.get(name.getVerbatim()), name);
			List<Integer> indicesToFill = indexOf.get(name.getVerbatim());

			for (Integer j : indicesToFill) {
				finalResult.set(j, name);
			}
		}

		return finalResult;
	}

}
