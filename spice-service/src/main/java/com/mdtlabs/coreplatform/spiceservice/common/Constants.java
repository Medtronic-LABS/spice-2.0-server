package com.mdtlabs.coreplatform.spiceservice.common;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;

/**
 * It contains constants which are related to the spice service.
 */
@Getter
@Setter
public class Constants {

    public static final String FOLLOW_UP_DUE_VISIT = "FollowUpDueVisit";
    public static final String FOLLOW_UP_DUE_CALL = "FollowUpDueCall";
    public static final String FOLLOW_UP_COND_CALL = "FollowUpCondCall";
    public static final String START_DATE = "startDate";
    public static final String END_DATE = "endDate";
    public static final Integer FOUR = 4;
    public static final String STRING_ONE = "1";
    public static final Long LONG_ONE = 1L;
    public static final String APPOINTMENT_TYPES = "appointmentTypes";

    public static final String FORWARD_SLASH = "/";
    public static final String BROWSER = "browser";

    public Constants() {
    }

    // form types.
    public static final String GENERAL_SCREENING = "general_screening";
    public static final String HOUSEHOLD_REGISTER = "household_registration";
    public static final String HOUSEHOLD_MEMBER_REGISTER = "household_member_registration";

    // clinical workflow types.
    public static final String CBS = "CBS";
    public static final String NAME = "name";
    public static final String ICCM = "ICCM";
    public static final String OTHER_SYMPTOMS = "OTHER_SYMPTOMS";
    public static final String TOTAL_COUNT = "totalCount";
    public static final String PATIENT_LIST = "patientList";
    public static final String PATIENT_STATUS = "patient_status";
    public static final String WEIGHT = "Weight";
    public static final String VITAL_INFORMATION = "VitalInformation";
    public static final String SYSTOLIC = "systolic";
    public static final String DIASTOLIC = "diastolic";
    public static final String PULSE = "pulse";
    public static final String BP = "Bp";
    public static final String NCD = "NCD";
    public static final String RMNCH = "RMNCH";
    public static final String TB = "TB";
    public static final List<String> DEFAULT_FORMS = List.of(GENERAL_SCREENING, HOUSEHOLD_REGISTER, HOUSEHOLD_MEMBER_REGISTER);
    public static final String SYMPTOMS = "symptom";
    public static final String SYMPTOMS_BY_CATEGORY = "symptomByCategory";
    public static final String FEVER = "Fever";
    public static final String DIARRHOEA = "Diarrhoea";
    public static final String COUGH = "Cough";
    public static final String HH_VISIT = "HH Visit";
    public static final String RMNCH_VISIT_COMPLETED = "VISIT COMPLETED";
    public static final String RMNCH_VISIT = "RMNCH_VISIT";


    public static final String SUCCESSFUL = "SUCCESSFUL";
    public static final String UN_SUCCESSFUL = "UNSUCCESSFUL";
    public static final String UN_ANSWERED = "UN_ANSWERED";
    public static final String WRONG_NUMBER = "WRONG_NUMBER";

    public static final Boolean BOOLEAN_TRUE = Boolean.TRUE;
    public static final Boolean BOOLEAN_FALSE = Boolean.FALSE;

    public static final int ZERO = 0;
    public static final int ONE = 1;
    public static final int TWO = 2;
    public static final int SEVEN = 7;
    public static final String EMPTY_SPACE = " ";
    public static final int OFFLINE_RESOURCE_LIST_SKIP = 0;
    public static final int OFFLINE_RESOURCE_LIST_LIMIT = 1000;
    public static final String COMMA = ",";
    public static final String EMPTY_STRING = "";
    public static final String HYPHEN = "-";

    public static final String PRESENTING_COMPLAINTS_ICCM = "presenting_complaints_iccm";
    public static final String PRESENTING_COMPLAINTS = "presenting_complaints";
    public static final String PRESENTING_COMPLAINTS_PNC_MOTHER = "presenting_complaints_pnc_mother";
    public static final String PRESENTING_COMPLAINTS_PNC_BABY = "presenting_complaints_pnc_baby";
    public static final String PRESENTING_COMPLAINTS_ANC = "presenting_complaints_anc";
    public static final String SYSTEMIC_EXAMINATION_ABOVE_5Y = "systemic_examination_above5Y";
    public static final String SYSTEMIC_EXAMINATION = "systemic_examination";
    public static final String SYSTEMIC_EXAMINATION_PNC_MOTHER = "systemic_examination_pnc_mother";
    public static final String SYSTEMIC_EXAMINATION_PNC_BABY = "systemic_examination_pnc_baby";
    public static final String OBSTETRIC_EXAMINATION_ANC = "obstetric_examination_anc";
    public static final String OBSTETRIC_EXAMINATION = "obstetric_examination";
    public static final String PREGNANCY_HISTORY = "pregnancy_history";
    public static final String COST = "cost";
    public static final String BLOOD_GROUP = "blood_group";
    public static final String DISEASE = "disease";

    public static final String DIAGNOSIS = "diagnosis";
    public static final String MEDICAL_SUPPLIES = "medical_supplies";
    public static final String COUNSELLED_ON = "counselled_on";
    public static final String EXAMINATION_UNDER_2M = "examination_under_2M";
    public static final String EXAMINATION = "examination";
    public static final String EXAMINATION_UNDER_5Y = "examination_under_5Y";
    public static final String UNDER_TWO_MONTHS = "UNDER_TWO_MONTHS";
    public static final String UNDER_FIVE_YEARS = "UNDER_FIVE_YEARS";
    public static final String ABOVE_FIVE_YEARS = "ABOVE_FIVE_YEARS";
    public static final String PNC_MOTHER_REVIEW = "PNC_MOTHER_REVIEW";
    public static final String PNC_CHILD_REVIEW = "PNC_CHILD_REVIEW";
    public static final String MOTHER_DELIVERY_REVIEW = "MOTHER_DELIVERY_REVIEW";
    public static final String ANC_REVIEW = "ANC_REVIEW";
    public static final String ANC = "ANC";
    public static final String PNC = "PNC";
    public static final String PNC_MOTHER = "PNC_MOTHER";
    public static final String PNC_NEONATE = "PNC_NEONATE";
    public static final String CHILDHOOD = "Childhood";
    public static final String RECOVERED = "Recovered";
    public static final String REFERRED = "Referred";
    public static final String ON_TREATMENT = "OnTreatment";
    public static final String MEDICAL_REVIEW = "medicalReview";
    public static final String ASSESSMENT = "assessment";
    public static final String CHILDHOOD_VISIT = "CHILDHOOD_VISIT";
    public static final String DISTRICT_ID = "districtId";
    public static final String DELIVERY_TYPE = "delivery_type";
    public static final String DELIVERY_BY = "delivery_by";
    public static final String DELIVERY_AT = "delivery_at";
    public static final String DELIVERY_STATUS = "delivery_status";
    public static final String CONDITION_OF_MOTHER = "condition_of_mother";
    public static final String RISK_FACTORS = "risk_factors";
    public static final String META = "meta";
    public static final String MOTHER_DELIVERY_STATUS = "mother_delivery_status";
    public static final String NEONATE_OUTCOME = "neonate_outcome";
    public static final String PNC_NEONATE_OUTCOME = "pnc_neonate_outcome";
    public static final String PRESCRIPTION_STATUS = "Prescription Created SuccessFully";
    public static final String SIGNATURE_FILE_NAME = "prescsign.jpeg";
    public static final String DOSAGE_FREQUENCY = "dosage_frequency";
    public static final String MUAC = "muac";
    public static final String MUAC_UPPER_CASE = "MUAC";
    public static final String MUAC_RED = "Red";
    public static final String MUAC_YELLOW = "Yellow";
    public static final String MUAC_GREEN = "Green";
    public static final String SIGNATURE = "signature";
    public static final String PRESCRIPTION_REQUEST = "prescriptionRequest";
    public static final String VILLAGE_IDS = "villageIds";
    public static final String USER_IDS = "userIds";
    public static final String IS_COMPLETED = "isCompleted";
    public static final String HOUSEHOLD_VISIT_DATE = "householdVisitDate";
    public static final String REFERRAL_DATE = "referralDate";
    public static final String LAST_SYNC_TIME = "lastSyncTime";
    public static final String CURRENT_SYNC_TIME = "currentSyncTime";
    public static final String MEDICAL_REVIEW_DATE = "medicalReviewDate";
    public static final String IMMUNISATION_STATUS = "immunisation_status";
    public static final String MEDICATION_FREQUENCY = "medication_frequency";
    public static final String RMNCH_VISIT_REASON = "%s Visit %s";
    public static final int PNC_VISIT_CLOSE_DAYS = 15;
    public static final int CHILD_VISIT_CLOSE_DAYS = 456;
    public static final Map<String, String> RMNCH_VISIT_MAPPING = Map.of(ANC, ANC, PNC_MOTHER, PNC,
            PNC_NEONATE, PNC, CHILDHOOD_VISIT, CHILDHOOD);
    public static final String FOLLOW_UP = "FollowUp";
    public static final String AWS_ENVIRONMENT_EKS = "eks";
    public static final String CHILD_VISIT = "CHILD_VISIT";
    //Medical Review
    public static final String ABOVE_5_GENERAL_MEDICAL_REVIEW = "ABOVE_FIVE_YEARS";
    public static final String PREGNANCY_ANC_MEDICAL_REVIEW = "ANC_REVIEW";
    public static final String PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW = "MOTHER_DELIVERY_REVIEW";
    public static final String LABOUR_DETAILS = "MOTHER_LABOUR_DETAILS";
    public static final String PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW = "NEONATE_BIRTH_REVIEW";
    public static final String PNC_MOTHER_MEDICAL_REVIEW = "PNC_MOTHER_REVIEW";
    public static final String PNC_CHILD_MEDICAL_REVIEW = "PNC_CHILD_REVIEW";
    public static final String PNC_VISIT = "PNC Visit";
    public static final String STATE_OF_PERINEUM = "state_of_perineum";
    public static final String UNASSIGNED = "Unassigned";
    public static final String OTHER = "Other";

    public static final String META_NUTRITION_LIFESTYLE = "NutritionLifestyle";
    public static final String META_DOSAGE_FORM = "DosageForm";
    public static final String META_SYMPTOMS = "Symptoms";
    public static final String META_MEDICAL_COMPLIANCES = "MedicalCompliance";
    public static final String META_DIAGNOSIS = "Diagnosis";
    public static final String META_REASONS = "Reasons";
    public static final String META_COMORBIDITIES = "Comorbidity";
    public static final String META_COMPLAINTS = "Complaints";
    public static final String META_CURRENT_MEDICATION = "CurrentMedication";
    public static final String META_COMPLICATIONS = "Complication";
    public static final String META_PHYSICAL_EXAMINATION = "PhysicalExamination";
    public static final String META_LIFESTYLE = "LifestyleQuestions";
    public static final String META_LIFESTYLE_ANSWERS = "LifestyleAnswers";
    public static final String META_FREQUENCY = "Frequency";
    public static final String META_FREQUENCY_TYPE = "FrequencyType";
    public static final String META_DOSAGE_FREQUENCY = "DosageFrequency";
    public static final String META_MODEL_QUESTIONS = "ModelQuestions";
    public static final String META_SIDE_MENU = "SideMenu";
    public static final String META_MESSAGE = "Messages";
    public static final String META_UNIT = "Unit";
    public static final String META_RISK_ALGORITHM = "RiskAlgorithm";


    public static final String META_DEFAULT = "English - India";

    public static final String DEFAULT_CULTURE_VALUE = "English - India";
    public static final String NCD_META = "ncdMeta";

    public static final String TYPE = "type";
    public static final String QUESTIONS = "questions";

    public static final String PATIENT_ID_TYPE_PATTERN = "national-id|passport|birth-certificate";
    public static final String CLIENT_REGISTRY_NATIONAL_ID = "national-id";
    public static final String CLIENT_REGISTRY_PASSPORT = "passport";
    public static final String CLIENT_REGISTRY_BIRTH_CERTIFICATE = "birth-certificate";
    public static final String VALUE = "value";

    public static final List<Map<String, String>> CLIENT_IDENTITY_TYPES = List.of(
            Map.of(NAME, "National ID", VALUE, CLIENT_REGISTRY_NATIONAL_ID),
            Map.of(NAME, "Passport Number", VALUE, CLIENT_REGISTRY_PASSPORT),
            Map.of(NAME, "Birth Certificate", VALUE, CLIENT_REGISTRY_BIRTH_CERTIFICATE)
    );

    public static final String WORKFLOW_SCREENING = "Screening";
    public static final String WORKFLOW_ENROLLMENT = "Enrollment";
    public static final String WORKFLOW_ASSESSMENT = "Assessment";
    public static final String SCREEN_TYPES = "screenTypes";
    public static final String EMPTY = "";
    public static final String FORM_LAYOUT = "formLayout";
    public static final String FAMILY = "family";
    public static final String PARENT_FAMILY = "parentFamily";
    public static final String ID = "id";
    public static final String CALL_REGISTER_ID = "callRegisterId";
    public static final String SCREENING = "screening";
    public static final String ENROLLMENT = "enrollment";
    public static final String MODULE = "Module";
    public static final String INPUT_FORM = "Input_form";
    public static final String CONSENT_FORM = "Consent_form";
    public static final String VIEW_SCREENS = "viewScreens";
    public static final String FORM_INPUT = "formInput";
    public static final String FORM = "form";
    public static final String UPDATE_VIRTUAL_ID = "update_virtual_id";
    public static final String INPUT_TENANT_ID = "in_tenant_id";
    public static final int MINUS_ONE = -1;
    public static final String FORM_NAME_COUNTRY = "country";

    public static final String PROVISIONAL_DM = "DM";
    public static final String BG_DEFAULT_FREQUENCY = "Every 1 month";
    public static final String HBA1C_DEFAULT_FREQUENCY = "Every 3 months";
    public static final String BG_DEFAULT_FREQUENCY_PERIOD = "month";
    public static final Integer BG_DEFAULT_FRQUENCY_DURATION = 1;
    public static final String FREQUENCY_HBA1C_CHECK = "HbA1c Check";

    public static final String EVERY_ONE_WEEK = "Every 1 week";
    public static final String EVERY_TWO_WEEKS = "Every 2 weeks";
    public static final String EVERY_THREE_WEEKS = "Every 3 weeks";
    public static final String EVERY_ONE_MONTH = "Every 1 month";
    public static final String EVERY_TWO_MONTHS = "Every 2 months";
    public static final String EVERY_THREE_MONTHS = "Every 3 months";
    public static final String DEFAULT = "default";
    public static final String NEW = "NEW";
    public static final String ROLE_RED_RISK_USER = "RED_RISK_USER";
    public static final String TEMPLATE_TYPE_RED_RISK = "RED_RISK";


    public static final Map<String, List<Integer>> PREGNANCY_TREATMENT_PLANS = Map.of(EVERY_ONE_WEEK,
            List.of(15, 16, 17, 18, 19, 23, 24, 25, 26, 27, 29, 30, 31, 35, 36, 37, 38, 39, 40),
            EVERY_TWO_WEEKS, List.of(14, 22, 28, 34),
            EVERY_THREE_WEEKS, List.of(13, 21, 33),
            EVERY_ONE_MONTH, List.of(9, 10, 11, 12, 20, 32),
            EVERY_TWO_MONTHS, List.of(5, 6, 7, 8),
            EVERY_THREE_MONTHS, List.of(4));
    public static final String HIGH = "High";
    public static final int BP_THRESHOLD_SYSTOLIC = 140;
    public static final int BP_THRESHOLD_DIASTOLIC = 90;
    public static final String HIGHER_MODERATE = "Higher Moderate";
    public static final String LOW = "Low";
    public static final String MODERATE = "Moderate";
    public static final String CATEGORY_TWO = "category2";
    public static final String CATEGORY_THREE = "category3";
    public static final String CATEGORY_FOUR = "category4";
    public static final String CATEGORY_FIVE = "category5";

    public static final String STRING_TWO = "2";
    public static final String STRING_THREE = "3";
    public static final String STRING_FOUR = "4";
    public static final String STRING_FIVE = "5";

    public static final String CATEGORY_TWO_COUNT = "category2count";
    public static final String CATEGORY_THREE_COUNT = "category3count";
    public static final String CATEGORY_FOUR_COUNT = "category4count";
    public static final String CATEGORY_FIVE_COUNT = "category5count";

    public static final String RBS = "rbs";
    public static final String FBS = "fbs";
    public static final String BOTH_MODERATE = "Both Moderate";
    public static final String BOTH_HIGHER_MODERATE = "Both Higher Moderate";
    public static final String GLUCOSE_MODERATE = "Glucose Moderate";
    public static final String CULTURE_VALUE_MESSAGE = "Messages";
    public static final String ENROLLED = "ENROLLED";
    public static final String NO_SYMPTOMS = "No symptoms";
    public static final String AFRICA = "AFRICA";
    public static final String NCD_REGISTER ="NCD Register";
    public static final String EMPOWER_HEALTH_NCD = "Empower Health NCD";
    public static final String NON_COMMUNITY = "NON_COMMUNITY";
    public static final String COMMUNITY = "COMMUNITY";

    //bp and bg types
    public static final String ASSESSMENT_TYPE_SCREENING = "screening";
    public static final String ASSESSMENT_TYPE_ASSESSMENT = "assessment";
    public static final String ASSESSMENT_TYPE_MEDICAL_REVIEW = "medicalReview";

    // others
    public static final String REGISTER = "registration";
    public static final String MY_PATIENTS = "my_patients";
    public static final String NUTRITION_LIFESTYLE = "nutritionLifestyle";
    public static final String DISPENSE = "dispense";
    public static final String INVESTIGATION = "investigation";
    private List<Symptom> symptoms = new ArrayList<>();
    private List<Symptom> ncdSymptoms = new ArrayList<>();
    public static final String HYPERTENSION = "Hypertension";
    public static final String RED_RISK = "RED_RISK";
    public static final String LOWER_MODERATE = "Lower Moderate";
    public static final String DIABETES = "Diabetes";
    public static final List<String> RED_RISK_ORDER = new ArrayList<>();
    static {
        RED_RISK_ORDER.add(LOW);
        RED_RISK_ORDER.add(LOWER_MODERATE);
        RED_RISK_ORDER.add(MODERATE);
        RED_RISK_ORDER.add(HIGHER_MODERATE);
        RED_RISK_ORDER.add(HIGH);
    }
    public static final String RELATED_PERSON = "RelatedPerson";
    public static final String PATIENT = "Patient";
    public static final String INSTRUCTIONS = "instructions";
    public static final String SCREENED = "SCREENED";

    public static final String ASSIGNED = "Assigned";
    public static final Long LONG_ZERO = 0L;
    //e-sign
    public static final String SCREENING_REQUEST = "screeningRequest";
    public static final String REGISTRATION_REQUEST = "registrationRequest";
    public static final String SIGNATURE_FILE = "signatureFile";
    public static final String SIGNATURE_DATE_FORMAT = "yyyyMMddHHmmssSSS";
    public static final String SCREENING_FORMAT = "screeningsign.jpeg";
    public static final String SCREENING_FOLDER = "screening/";
    public static final String ENROLLMENT_FOLDER = "enrollment/";
    public static final String ENROLLMENT_FORMAT = "enrollmentsign.jpeg";
    public static final String WFA = "wfa";
    public static final String WFH = "wfh";
    public static final String MALE = "male";
    public static final int NEGATIVE_TWO = -2;
    public static final int THREE = 3;
    public static final int NEGATIVE_THREE = -3;
    public static final int TEN = 10;
    public static final Double DOUBLE_TEN = 10.0;
    public static final int FORTY_FIVE = 45;
    public static final int SIXTY = 60;
    public static final int ONE_HUNDRED = 100;
    public static final Double ONE_HUNDRED_IN_DOUBLE = 100.0;
    public static final int ONE_HUNDRED_TWENTY = 120;
    public static final String FEMALE = "female";

    // Dashboard sort fields
    public static final String DASHBOARD_MONTH = "month";
    public static final String DASHBOARD_YESTERDAY = "yesterday";
    public static final String DASHBOARD_WEEK = "week";
    public static final String FOLLOWUP_DAILY = "daily";
    public static final String FOLLOWUP_WEEKLY = "weekly";
    public static final String FOLLOWUP_MONTHLY = "monthly";

    // Call register constants
    public static final String REFERRED_SITE_ID = "referredSiteId";

    // others
    public static final String PATIENT_IDS = "patientIds";
    public static final String RETRY_ATTEMPTS = "retryAttempts";
    public static final String CALL_REGISTER_COUNT = "callRegisterCount";
    public static final String DATE_FORMAT_WITHOUT_MILLISECOND = "yyyy-MM-dd'T'HH:mm:ss";
    public static final String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";
    public static final String MEDICAL_REVIEW_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ssX";
    public static final String LOST_TO_FOLLOW_UP = "LOST_TO_FOLLOW_UP";
    public static final String ASSESSMENT_COMPLETED = "ASSESSMENT_COMPLETED";
    public static final String MINIMAL = "Minimal";
    public static final String MILD = "Mild";
    public static final String MODERATELY_SEVERE = "Moderately severe";
    public static final String SEVERE = "Severe";
    public static final String NORMAL = "Normal";
    public static final String PREGNANCY = "pregnancy";
}
