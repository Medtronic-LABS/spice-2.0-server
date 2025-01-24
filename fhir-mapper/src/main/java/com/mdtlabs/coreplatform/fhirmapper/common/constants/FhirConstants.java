package com.mdtlabs.coreplatform.fhirmapper.common.constants;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;

/**
 * <p>
 *   This FHIR constants used for fhir mapping purpose
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
public class FhirConstants {
    public static final String PATIENT_IDENTIFIER_URL = "urn:uuid:patient";
    public static final String ENCOUNTER_IDENTIFIER_URL = "urn:uuid:encounter";
    public static final String CONDITION_IDENTIFIER_URL = "urn:uuid:condition";
    public static final String RELATED_PERSON_IDENTIFIER_URL = "urn:uuid:related-person";
    public static final String BLOOD_PRESSURE_IDENTIFIER_URL = "urn:uuid:bp-log";
    public static final String PREGNANCY_IDENTIFIER_URL = "urn:uuid:pregnancy";
    public static final String BLOOD_SUGAR_IDENTIFIER_URL = "urn:uuid:glucose-log";
    public static final String HEIGHT_IDENTIFIER_URL = "urn:uuid:height";
    public static final String WEIGHT_IDENTIFIER_URL = "urn:uuid:weight";
    public static final String BMI_IDENTIFIER_URL = "urn:uuid:bmi";
    public static final String TEMPERATURE_IDENTIFIER_URL = "urn:uuid:temperature";
    public static final String SUICIDE_SCREENER_IDENTIFIER_URL = "urn:uuid:suicide-screener";
    public static final String SUBSTANCE_ABUSE_IDENTIFIER_URL = "urn:uuid:substance-abuse";
    public static final String REFERRAL_TICKER_IDENTIFIER_URL = "urn:uuid:referral-ticket";
    public static final String REGULAR_SMOKER_IDENTIFIER_URL = "urn:uuid:regular-smoker";
    public static final String VITAL_SIGNS_IDENTIFIER_URL = "urn:uuid:vital-signs";
    public static final String MENTAL_HEALTH_OBSERVATION_IDENTIFIER_URL = "urn:uuid:mental-health-observation";
    public static final String RED_RISK_OBSERVATION_IDENTIFIER_URL = "urn:uuid:red-risk-observation";
    public static final String LOCATION_IDENTIFIER_URL = "urn:uuid:location";
    public static final String QUESTIONNAIRERESPONSE_IDENTIFIER_URL = "urn:uuid:questionnaire-response";
    public static final String SYMPTOM_IDENTIFIER_URL = "urn:uuid:symptom";
    public static final String COMPLIANCE_IDENTIFIER_URL = "urn:uuid:compliance";
    public static final String PATIENT_STATUS_DIABETES_IDENTIFIER_URL = "urn:uuid:patient-status-diabetes";
    public static final String PATIENT_STATUS_HYPERTENSION_IDENTIFIER_URL = "urn:uuid:patient-status-hypertension";
    public static final String PATIENT_DIAGNOSIS_IDENTIFIER_URL = "urn:uuid:patient-diagnosis";
    public static final String PHQ4_QUESTIONNAIRERESPONSE_IDENTIFIER_URL = "urn:uuid:phq4-questionnaire-response";
    public static final String PHQ9_QUESTIONNAIRERESPONSE_IDENTIFIER_URL = "urn:uuid:phq9-questionnaire-response";
    public static final String GAD7_QUESTIONNAIRERESPONSE_IDENTIFIER_URL = "urn:uuid:gad7-questionnaire-response";
    public static final String HIV_QUESTIONNAIRERESPONSE_IDENTIFIER_URL = "urn:uuid:hiv-questionnaire-response";


    // resource type
    public static final String PATIENT = "Patient";
    public static final String PATIENT_ID = "Patient/%s";
    public static final String LOCATION_ID = "Location/%s";
    public static final String ENCOUNTER = "Encounter";
    public static final String ORGANIZATION_ID = "Organization/%s";
    public static final String ENCOUNTER_ID = "Encounter/%s";
    public static final String OBSERVATION = "Observation";
    public static final String QUESTIONNAIRE_RESPONSE = "QuestionnaireResponse";
    public static final String RELATED_PERSON = "RelatedPerson";
    public static final String RELATED_PERSON_ID = "RelatedPerson/%s";
    public static final String PRACTITIONER_ID = "Practitioner/%s";
    public static final String OBSERVATION_ID = "Observation/%s";
    public static final String MEDICATION_REQUEST_ID = "MedicationRequest/%s";
    public static final String CONDITION_ID = "Condition/%s";
    public static final String SERVICE_REQUEST_ID = "ServiceRequest/%s";
    public static final String QUESTIONNAIRE_RESPONSE_ID = "QuestionnaireResponse/%s";
    public static final String LOCATION = "Location";
    public static final String NATIONAL_ID = "national-id";
    public static final String BLOOD_PRESSURE = "Blood Pressure";
    public static final String BLOOD_GLUCOSE = "Blood Glucose";
    public static final String PREGNANCY_ANC = "Pregnancy Anc";
    public static final String PREGNANCY = "Pregnancy";
    public static final String VITAL_SIGNS = "Vital Signs";
    public static final String VITAL_SIGNS_IDENTIFIER = "vitalsigns";
    public static final String VITAL_RISK_DETAILS = "Vital Risk Details";
    public static final String SYMPTOM = "Symptom";
    public static final String COMPLIANCE = "Compliance";
    public static final String CONDITION = "Condition";
    public static final String PATIENT_SYMPTOM = "Patient Symptom";
    public static final String ORGANIZATION = "Organization";

    //ORG Urls
    public static final String UNIT_SOFT_MEASURE_ORG_URL = "http://unitsofmeasure.org";

    public static final String HEIGHT = "Height";
    public static final String WEIGHT = "Weight";
    public static final String REGULAR_SMOKER = "Regular Smoker";
    public static final String BMI = "BMI";

    public static final String FBS = "fbs";
    public static final String RBS = "rbs";

    public static final String CELSIUS_CODE = "℃";
    public static final String CM_CODE = "cm";
    public static final String KG_CODE = "kg";
    public static final String KG_PER_M2_CODE = "kg/m^2";
    public static final String BLOOD_PRESSURE_UNIT = "mmHg";
    public static final String BLOOD_PRESSURE_UNIT_CODE = "mm[Hg]";
    public static final String PULSE_UNIT = "BPM";
    public static final String STATUS = "status";
    public static final String TYPE = "type";
    public static final String SCREENED = "SCREENED";
    public static final String SCREENING = "screening";
    public static final String ENROLLED = "ENROLLED";
    public static final String ASSESSED = "ASSESSED";
    public static final String MEDICAL_REVIEWED = "MEDICAL_REVIEWED";
    public static final String CVD_RISK_LEVEL = "cvdRiskLevel";
    public static final String CVD_RISK_SCORE_DISPLAY = "cvdRiskScoreDisplay";
    public static final String CVD_RISK_SCORE = "cvdRiskScore";
    public static final String PHQ4_RISK_LEVEL = "phq4RiskLevel";
    public static final String PHQ9_RISK_LEVEL = "phq9RiskLevel";
    public static final String GAD7_RISK_LEVEL = "gad7RiskLevel";
    public static final String PHQ4_SCORE = "phq4Score";
    public static final String PHQ9_SCORE = "phq9Score";
    public static final String GAD7_SCORE = "gad7Score";
    public static final String PHQ4_FIRST_SCORE = "phq4FirstScore";
    public static final String PHQ4_SECOND_SCORE = "phq4SecondScore";
    public static final String RISK_LEVEL = "riskLevel";
    public static final String RISK_MESSAGE = "riskMessage";
    public static final String PREGNANCY_IDENTIFIER = "pregnancy";

    // gender
    public static final List<String> GENDER_LIST = List.of("FEMALE", "MALE");

    public static final String TEMPERATURE = "Temperature";
    public static final String CONFIRMED = "confirmed";
    public static final String ACTIVE = "active";

    public static final String HTN_CONDITION = "htn";
    public static final String DBM_CONDITION = "dbm";
    public static final String MENTAL_HEALTH_CONDITION = "mentalHealth";
    public static final String SUBSTANCE_DISORDER_CONDITION = "substanceDisorder";
    public static final String MENTAL_HEALTH = "Mental Health";
    public static final String DEPRESSION = "depression";
    public static final String ANXIETY = "anxiety";
    public static final String GAD7 = "GAD7";
    public static final String PHQ9 = "PHQ9";

    public static final Map<String, String> CONFIRM_DIAGNOSIS_TYPE_MAP = Map.of(Constants.NCD, Constants.NCD,
            Constants.MENTAL_HEALTH, Constants.MENTAL_HEALTH, Constants.MATERNAL_HEALTH, Constants.MATERNAL_HEALTH,
            Constants.HIV.toUpperCase(), Constants.HIV.toUpperCase(),
            Constants.OTHER, Constants.OTHER);
    public static final Map<String, List<String>> CONFIRM_DIAGNOSIS_CATEGORY = Map.of(Constants.NCD, List.of(Constants.DIABETES, Constants.HYPERTENSION),
            Constants.HIV.toUpperCase(), List.of(Constants.HIV.toUpperCase()),
            Constants.MENTAL_HEALTH, List.of(Constants.MENTAL_HEALTH, Constants.SUBSTANCE_DISORDER), Constants.MATERNAL_HEALTH, List.of(Constants.PREGNANCY_TYPE));

    public static final String PREGNANCY_DETAILS = "pregnancy-details";

    // patient status
    public static final String PATIENT_STATUS = "patient-status";

    private FhirConstants() {

    }
}
