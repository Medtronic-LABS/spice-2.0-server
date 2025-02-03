package com.mdtlabs.coreplatform.fhirmapper.common;

import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

/**
 * <p>
 * To define the common static parameter used all over the application.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Feb 05, 2024
 */
public class FhirIdentifierConstants {

    public static final String BASE_URL = FhirUtils.getFhirIdentifierUrl();
    public static final String PATIENT_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "patient-id");
    public static final String MOTHER_PATIENT_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "mother-patient-id");
    public static final String VILLAGE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "village-id");
    public static final String OTHER_VILLAGE_NAME_URL = StringUtil.concatString(BASE_URL, "other-village-name");
    public static final String OBSERVATION_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "observation-type");
    public static final String VISIT_NUMBER_SYSTEM_URL = StringUtil.concatString(BASE_URL, "visit-count");
    public static final String ENCOUNTER_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "encounter-type");
    public static final String VITALS_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "vitals-type");
    public static final String TYPE_SYSTEM_URL =  StringUtil.concatString(BASE_URL, "type");
    public static final String PRESCRIPTION_STATUS_SYSTEM_URL = StringUtil.concatString(BASE_URL, "prescription-status");
    public static final String INVESTIGATION_STATUS_SYSTEM_URL = StringUtil.concatString(BASE_URL, "investigation-status");
    public static final String FOLLOW_UP_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "follow-up-id");
    public static final String MEDICATION_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "medication-id");
    public static final String RELATIONSHIP_SYSTEM_URL = StringUtil.concatString(BASE_URL, "relationship-system");
    public static final String PATIENT_STATUS_SYSTEM_URL = StringUtil.concatString(BASE_URL, "patient-status");
    public static final String PATIENT_CURRENT_STATUS_SYSTEM_URL = StringUtil.concatString(BASE_URL, "patient-current-status");
    public static final String PATIENT_CURRENT_STATUS_CLOSED_REASON = StringUtil.concatString(BASE_URL, "closed-reason");
    public static final String HOUSEHOLD_NO_SYSTEM_URL = StringUtil.concatString(BASE_URL, "household-number");
    public static final String HOUSEHOLD_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "household-id");
    public static final String TICKET_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "ticket-type");
    public static final String TICKET_CATEGORY_SYSTEM_URL = StringUtil.concatString(BASE_URL, "category");
    public static final String FHIR_YES_NO_CODE = "https://fhir.loinc.org/ValueSet/LL3044-6,observation";
    public static final String CONDITION_SYSTEM_URL = StringUtil.concatString(BASE_URL, "condition");
    public static final String LAB_TEST_KEY_NAME_URL = StringUtil.concatString(BASE_URL, "lab-test-name");
    public static final String CONDITION_DISEASE_TYPE_URL = StringUtil.concatString(BASE_URL, "disease-type");
    public static final String ORGANIZATION_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "organization-id");
    public static final String COUNTRY_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "country-id");
    public static final String PHONE_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "phone");
    public static final String PHONE_CATEGORY_SYSTEM_URL = StringUtil.concatString(BASE_URL, "phone-category");
    public static final String MODIFIED_DATE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "modified-time");

    //Extension URL
    public static final String IS_PREGNANT_EXTENSION_URL = StringUtil.concatString(BASE_URL, "fhir/StructureDefinition/pregnant-status");

    public static final String CONDITION_DISEASE_URL = StringUtil.concatString(BASE_URL, "diseaseId");
    public static final String IDENTITY_TYPE_SYSTEM_URL =  StringUtil.concatString(BASE_URL, "identity-type");
    public static final String SCREENED_LANDMARK_SYSTEM_URL =  StringUtil.concatString(BASE_URL, "screened-landmark");
    public static final String IDENTITY_VALUE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "identity-value");
    public static final String VIRTUAL_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "virtual-id");
    public static final String PROGRAM_ID_SYSTEM_URL = StringUtil.concatString(BASE_URL, "program-id");
    public static final String CARE_PLAN_SYSTEM_URL = StringUtil.concatString(BASE_URL, "careplan-type");
    public static final String APPOINTMENT_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "appointment-type");
    public static final String CONDITION_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL, "condition-type");
    public static final String MEDICATION_STATEMENT_TYPE_SYSTEM_URL = StringUtil.concatString(BASE_URL,
            "medicationstatement-type");
    public static final String NCD_MEDICAL_REVIEW_STATUS_SYSTEM_URL = StringUtil.concatString(BASE_URL, "ncd",
            "-medicalreview-status");
    public static final String IDENTITY_TYPE_NATIONAL_ID =  StringUtil.concatString(BASE_URL, "national-id");
    public static final String MENTAL_HEALTH_SYSTEM_URL = StringUtil.concatString(BASE_URL, "mental-health");
    public static final String HIV_SYSTEM_URL = StringUtil.concatString(BASE_URL, "hiv");
    public static final String PATIENT_REFERRAL_STATUS_SYSTEM_URL = StringUtil.concatString(BASE_URL, "is-patient",
            "-referred");
    public static final String PATIENT_RISK_LEVEL_URL = StringUtil.concatString(BASE_URL, "risk-level");
    public static final String PREGNANT_SYSTEM_URL = StringUtil.concatString(BASE_URL, "pregnant");
    public static final String IS_REVIEW_SYSTEM_URL = StringUtil.concatString(BASE_URL,"is-review");

    //confirm-diagnosis
    public static final String PATIENT_DIAGNOSIS_NCD_IDENTIFIER_SYSTEM_URL = StringUtil.concatString(BASE_URL, "patient-diagnosis-ncd");
    public static final String PATIENT_DIAGNOSIS_MENTAL_HEALTH_IDENTIFIER_URL = StringUtil.concatString(BASE_URL, "patient-diagnosis-mental-health");
    public static final String PATIENT_DIAGNOSIS_HIV_IDENTIFIER_URL = StringUtil.concatString(BASE_URL, "patient-diagnosis-hiv");
    public static final String PATIENT_DIAGNOSIS_PREGNANCY_IDENTIFIER_URL = StringUtil.concatString(BASE_URL, "patient-diagnosis-pregnancy");
    public static final String PATIENT_DIAGNOSIS_OTHER_IDENTIFIER_URL = StringUtil.concatString(BASE_URL, "patient-diagnosis-other");
    public static final String PATIENT_STATUS_DIAGNOSIS_SYSTEM_URL = StringUtil.concatString(BASE_URL, "patient-diagnosis-status");


    public static final String PATIENT_DELETE_REASON_IDENTIFIER_URL = StringUtil.concatString(BASE_URL, "patient-delete-reason");

    public static final Map<String, String> CONFIRM_DIAGNOSIS_IDENTIFIER_MAP = Map.of(Constants.NCD, PATIENT_DIAGNOSIS_NCD_IDENTIFIER_SYSTEM_URL,
            Constants.MENTAL_HEALTH, PATIENT_DIAGNOSIS_MENTAL_HEALTH_IDENTIFIER_URL, Constants.MATERNAL_HEALTH, PATIENT_DIAGNOSIS_PREGNANCY_IDENTIFIER_URL,
            Constants.HIV.toUpperCase(), PATIENT_DIAGNOSIS_HIV_IDENTIFIER_URL,
            Constants.OTHER, PATIENT_DIAGNOSIS_OTHER_IDENTIFIER_URL);
    // dispense identifiers
    public static final String PRESCRIBED_ID_URL = StringUtil.concatString(BASE_URL, "prescription-id");
    public static final String PRESCRIBED_DAYS_URL = StringUtil.concatString(BASE_URL, "prescribed-days");
    public static final String PRESCRIPTION_ALL_READY_FILLED_DAYS_URL = StringUtil.concatString(BASE_URL, "already-filled-days");
    public static final String PRESCRIPTION_FILLED_DAYS_URL = StringUtil.concatString(BASE_URL, "prescription-filled-days");

    private FhirIdentifierConstants() {

    }
    public static final String META_COMORBIDITY_ID = StringUtil.concatString(BASE_URL, "comorbidity-id");
    public static final String META_COMPLICATION_ID = StringUtil.concatString(BASE_URL, "complication-id");
    public static final String META_CURRENT_MEDICATION_ID = StringUtil.concatString(BASE_URL, "current-medication-id");
}
