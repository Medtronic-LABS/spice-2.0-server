package com.mdtlabs.coreplatform.commonservice.common.exception;

import org.springframework.http.HttpStatus;

import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;

import java.time.ZonedDateTime;

/**
 * <p>
 * This service class contain all the business logic for exception resolver
 * module and perform all the user operation here.
 * </p>
 * 
 * @author Karthick Murugesan created on Jan 11, 2024
 */
public class ExceptionResolverImpl implements ExceptionResolver {

	/**
	 * {@inheritDoc}
	 */
	public ErrorMessage resolveError(final HttpStatus statusCode, final String msg) {
		Logger.logInfo(ErrorConstants.RESOLVER_ERROR + msg);
		return ErrorMessage.builder()
				.dateTime(ZonedDateTime.now().toInstant().toEpochMilli())
				.errorCode(statusCode.value())
				.message(msg)
				.exception(msg)
				.status(Boolean.FALSE)
				.build();
	}
}
