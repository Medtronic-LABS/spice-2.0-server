package com.mdtlabs.coreplatform.offlineservice.common.enumeration;

/**
 * <p>
 * This is a Enumeration for Appointment Type.
 * </p>
 *
 * @author Yogeshwaran Mohan Created on Mar 26, 2024.
 */
public enum AppointmentType {
    // for community followup
    HH_VISIT, REFERRED, HH_MAPPING, MEDICAL_REVIEW,
    // for non community followup
    NON_COMMUNITY_MEDICAL_REVIEW, LOST_TO_FOLLOW_UP, ASSESSMENT, SCREENED
}
