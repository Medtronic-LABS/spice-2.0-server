package com.mdtlabs.coreplatform.commonservice.common.exception;

import org.springframework.http.HttpStatus;

/**
 * <p>
 * This an interface class for exception resolver you can implement this with
 * any class.
 * </p>
 * 
 * @author Karthick Murugesan created on Jan 11, 2024
 */
public interface ExceptionResolver {

	/**
	 * <p>
	 * This method is used to construct error resolver
	 * </p>
	 *
	 * @param statusCode - status code of error
	 * @param msg        - error trace
	 * @return ErrorMessage - error message builder
	 */
	ErrorMessage resolveError(final HttpStatus statusCode, final String msg);
}
