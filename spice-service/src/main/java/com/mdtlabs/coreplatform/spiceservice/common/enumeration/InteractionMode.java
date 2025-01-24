package com.mdtlabs.coreplatform.spiceservice.common.enumeration;

/**
 * Enumeration representing the modes of interaction for follow-up processes.
 * <p>
 * This enumeration defines the various modes of interaction that can occur during follow-up processes within the system. The modes include:
 * <ul>
 *     <li>ASSESSMENT - Represents an initial assessment or evaluation interaction.</li>
 *     <li>MEDICAL_REVIEW - Indicates a medical review or consultation.</li>
 *     <li>FOLLOW_UP - Denotes a subsequent follow-up interaction after the initial assessment or medical review.</li>
 * </ul>
 * These interaction modes are crucial for categorizing the nature of each interaction, facilitating targeted responses and actions based on the specific mode.
 * </p>
 *
 * @author Praveen
 * Created on July 11, 2024.
 */
public enum InteractionMode {
    ASSESSMENT, MEDICAL_REVIEW, FOLLOW_UP
}
