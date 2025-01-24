package com.mdtlabs.coreplatform.spiceservice.common.enumeration;

/**
 * Enumeration representing the different types of appointments that can be scheduled or referred within the system.
 * <p>
 * This enumeration defines the types of appointments that are recognized by the system, including:
 * <ul>
 *     <li>HH_VISIT - Represents a household visit.</li>
 *     <li>REFERRED - Indicates that the patient has been referred to another service or specialist.</li>
 *     <li>MEDICAL_REVIEW - Denotes a medical review appointment.</li>
 * </ul>
 * </p>
 * <p>
 * These appointment types are used throughout the system to categorize and manage appointments, enabling specific
 * workflows and actions based on the appointment type.
 * </p>
 *
 * @author Yogeshwaran Mohan
 * Created on Mar 26, 2024.
 */
public enum AppointmentType {
    // for community followup
    HH_VISIT, REFERRED, HH_MAPPING, MEDICAL_REVIEW,
    // for non community followup
    NON_COMMUNITY_MEDICAL_REVIEW, LOST_TO_FOLLOW_UP, ASSESSMENT, SCREENED
}
