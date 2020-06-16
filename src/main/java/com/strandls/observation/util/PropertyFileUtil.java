package com.strandls.observation.util;

import java.util.Properties;

public class PropertyFileUtil {
	
	public static Properties fetchProperty(String fileName) {
        Properties properties = new Properties();
        try {
            ClassLoader classLoader = PropertyFileUtil.class.getClassLoader();
            properties.load(classLoader.getResourceAsStream(fileName));
        } catch (Exception e) {
        	properties = null;
            e.printStackTrace();
        }
        return properties;
    }

    public static String fetchProperty(String fileName, String propertyName) {
        Properties properties = new Properties();
        String result = "";
        try {
            ClassLoader classLoader = PropertyFileUtil.class.getClassLoader();
            properties.load(classLoader.getResourceAsStream(fileName));
            result = properties.getProperty(propertyName);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
        }
		return result;
    }
}