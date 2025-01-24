package com.mdtlabs.coreplatform.spiceservice.common.enumeration;

/**
 * Enumeration representing the reasons for unsuccessful calls within the system.
 * <p>
 * This enumeration defines the specific reasons why a call might be marked as unsuccessful, facilitating targeted follow-up actions and reporting. The reasons include:
 * <ul>
 *     <li>UNREACHABLE - Indicates that the call recipient could not be reached.</li>
 *     <li>WRONG_NUMBER - Indicates that the call was made to an incorrect number.</li>
 * </ul>
 * These reasons help in understanding the nature of unsuccessful calls and in planning subsequent communication strategies.
 * </p>
 *
 * @author Yogeshwaran Mohan
 * Created on Mar 26, 2024.
 */
public enum UnsuccessfulCallReason {
    UNREACHABLE, WRONG_NUMBER
}
