package com.mdtlabs.coreplatform.commonservice.common;


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;

/**
 * <p>
 * Used to fetch the frequent changes messages like error / success messages
 * from the application.properties file.
 * </p>
 * 
 * @author Karthick Murugesan created on Jan 11, 2024
 *
 */
public class MessageValidator {

	private static MessageValidator messageValidator;
	public static final String SUCCESS_PROPERTIES = "/success_messages.properties";
	public static final String ERROR_PROPERTIES = "/error_messages.properties";
	public static final String INSTRUCTION_TXT = "/high_risk_pregnancy_criteria_instruction.txt";

	/**
	 * <p>
	 * Message validator singleton.
	 * </p>
	 * <p>
	 * Creating singleton for message validator.
	 * </p>
	 *
	 * @return returns the message validator.
	 */
	public static synchronized MessageValidator getInstance() {
		if (messageValidator == null) {
			messageValidator = new MessageValidator();
		}
		return messageValidator;
	}

	/**
	 * <p>
	 * To get property file with respect to type
	 * </p>
	 *
	 * @param type success or error type is passed
	 * @return property file name
	 */
	private String getPropertyFileByType(String type) {
		String propFileName = ERROR_PROPERTIES;
		if (type.equals(Constants.SUCCESS)) {
			propFileName = SUCCESS_PROPERTIES;
		} else if(type.equalsIgnoreCase(Constants.INSTRUCTIONS)) {
			propFileName = INSTRUCTION_TXT;
		}
		return propFileName;
	}

	/**
	 * <p>
	 * Get Message from SUCCESS_ERROR_PROPERTIES file based on the message key.
	 * </p>
	 *
	 * @param messageKey message key is passed in this attribute.
	 * @return returns the constructed message.
	 */
	public String getMessage(String messageKey, String type) {
		final Properties configProp = new Properties();
		final String propFileName = getPropertyFileByType(type);
		try {
			InputStream in = getClass().getResourceAsStream(propFileName);
			configProp.load(in);

		} catch (IOException ioe) {
			Logger.logError(ioe.getMessage(), ioe);
		}
		return configProp.getProperty(messageKey);
	}

	/**
	 * <p>
	 * Get Message from SUCCESS_ERROR_PROPERTIES file based on the message key.
	 * </p>
	 *
	 * @param messageKey code or message key is passed in this attribute.
	 * @param arg        value to be appended with the chat message is passed in
	 *                   this attribute.
	 * @return returns the message constructed as response for chat.
	 */
	public String getMessage(String messageKey, String type, String... arg) {
		final Properties configProp = new Properties();
		String arguments = null;
		try {
			InputStream in = getClass().getResourceAsStream(getPropertyFileByType(type));
			configProp.load(in);
			arguments = MessageFormat.format(configProp.getProperty(messageKey), arg);
		} catch (IOException ioe) {
			Logger.logError(ioe.getMessage(), ioe);
		}
		return arguments;
	}

	/**
	 * <p>
	 * Get Message from SUCCESS_ERROR_PROPERTIES file based on the message key.
	 * </p>
	 *
	 * @param messageKey code or message key is passed in this attribute.
	 * @param arg        value to be appended with the chat message is passed in
	 *                   this attribute.
	 * @return returns the message constructed as response for chat.
	 */
	public String getMessage(String messageKey, String type, List<String> arg) {
		final Properties configProp = new Properties();
		String arguments = null;
		String[] stringArg = arg.toArray(new String[0]);
		try {
			InputStream in = getClass().getResourceAsStream(getPropertyFileByType(type));
			configProp.load(in);
			arguments = MessageFormat.format(configProp.getProperty(messageKey), stringArg);
		} catch (IOException ioe) {
			Logger.logError(ioe.getMessage(), ioe);
		}
		return arguments;
	}

	/**
	 * <p>
	 * Getting Error Message With the Arguments.
	 * </p>
	 *
	 * @param messageKey message key is passed in this attribute.
	 * @param arg        value to be appended with the error message is passed in
	 *                   this attribute.
	 * @return returns the constructed message.
	 */
	public String getMessage(String messageKey, String type, String arg) {
		final Properties configProp = new Properties();
		String arguments = null;
		final String propFileName = getPropertyFileByType(type);
		try {
			InputStream in = getClass().getResourceAsStream(propFileName);
			configProp.load(in);
			arguments = MessageFormat.format(configProp.getProperty(messageKey), arg);
		} catch (IOException ioe) {
			Logger.logError(ioe.getMessage(), ioe);
		}
		return arguments;
	}

	/**
	 * <p>
	 * Retrieves a list of messages from a properties file based on the type.
	 * </p>
	 *
	 * @param type The type of messages to retrieve (e.g., "success" or "error").
	 * @return A list of messages from the properties file.
	 */
	public List<String> getMessage(String type) {
		final String propFileName = getPropertyFileByType(type);
		List<String> instructions = new ArrayList<>();
		try {
			InputStream in = getClass().getResourceAsStream(propFileName);
			BufferedReader reader = new BufferedReader(new InputStreamReader(in));
			String line;
			while ((line = reader.readLine()) != null) {
				instructions.add(line);
			}
			reader.close();
		} catch (IOException ioe) {
			Logger.logError(ioe.getMessage(), ioe);
		}
		return instructions;
	}

}
