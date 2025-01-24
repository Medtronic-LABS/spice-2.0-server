package com.mdtlabs.coreplatform.commonservice.common.message;

/**
 * <p>
 * This enum represents the success codes used in the application.
 * Each success code is associated with a message in the application's property file (application.property),
 * which is located in the resource folder.
 * </p>
 *
 * <p>
 * The success codes are used to fetch the corresponding success messages from the property file.
 * </p>
 *
 * @author Sharveshkumar created on Apr 02, 2024
 *
 */
public enum SuccessCode {

	HEALTH_CHECK(9000),
	PRESCRIPTION_SAVE(1011);

	private int key;

	SuccessCode(int key) {
		this.key = key;
	}

	public int getKey() {
		return this.key;
	}
}
