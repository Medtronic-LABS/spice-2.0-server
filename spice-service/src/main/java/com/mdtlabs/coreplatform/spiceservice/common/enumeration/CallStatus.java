package com.mdtlabs.coreplatform.spiceservice.common.enumeration;

/**
 * Enumeration representing the possible outcomes of a call.
 * <p>
 * This enumeration defines the possible statuses of a call within the system, which include:
 * <ul>
 *     <li>SUCCESSFUL - Indicates that the call was completed successfully.</li>
 *     <li>UNSUCCESSFUL - Indicates that the call did not achieve its intended outcome.</li>
 * </ul>
 * These statuses can be used to track the outcome of calls and facilitate appropriate follow-up actions or reporting.
 * </p>
 *
 * @author Yogeshwaran Mohan
 * Created on Mar 26, 2024.
 */
public enum CallStatus {
    SUCCESSFUL, UNSUCCESSFUL
}
