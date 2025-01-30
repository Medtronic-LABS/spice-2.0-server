package com.mdtlabs.coreplatform.fhirmapper.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirCodeDTO;

/**
 * <p>
 * To define the common static parameter used all over the application.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Feb 05, 2024
 */
public class Constants extends ca.uhn.fhir.rest.api.Constants {

    // Fhir
    public static final String FHIR_BASE_URL = "urn:uuid:";
    public static final String PATIENT_VITAL_INFORMATION = "PatientVitals";
    public static final String REFERENCE_PATIENT_ID = "referencePatientId";
    public static final String FORWARD_SLASH = "/";
    public static final String COMMA = ",";
    public static final String COMMA_OTHER = ", Other";
    public static final String VERTICAL_BAR = "|";
    public static final String HIGHFIN = "-";
    public static final int ZERO = 0;
    public static final double ZERO_DOUBLE = 0.0;
    public static final int TWO = 2;
    public static final String ZERO_STRING = "0";
    public static final Long ZERO_LONG = 0L;
    public static final int SIXTY = 60;
    public static final long SEVEN = 7;
    public static final long SIXTEEN = 16;
    public static final int THREE = 3;
    public static final int FOUR = 4;
    public static final String CREATED = "Created";
    public static final String DAYS = "days";
    public static final String ID = "id";
    public static final String MEMBER_ID = "memberId";
    public static final String TT_DOSE = "ttDose";
    public static final String RISK_FACTORS = "riskFactors";
    public static final int INT_SEVEN = 7;

    //BirthHistory
    public static final String BIRTHWEIGHT = "birthWeight";
    public static final String BIRTHWEIGHT_CATEGORY = "birthWeightCategory";
    public static final String BIRTH_HISTORY = "birthHistory";
    public static final String GESTATIONAL_AGE_CATEGORY = "gestationalAgeCategory";

    //HouseHold
    public static final String OWNED_HAND_WASHING_FACILITY_WITH_SOAP = "ownedHandWashingFacilityWithSoap";
    public static final String OWNED_TREATED_BED_NET = "ownedTreatedBedNet";
    public static final String OWNED_AN_IMPROVED_LATRINE = "ownedAnImprovedLatrine";
    public static final String BED_NET_COUNT = "bedNetCount";

    //Encounter Names
    //Assessment Types
    public static final String ICCM = "ICCM";
    public static final String OTHER_SYMPTOMS = "OTHER_SYMPTOMS";
    public static final String RMNCH = "RMNCH";
    public static final String ANC = "ANC";
    public static final String PNC = "PNC";
    public static final String PNC_MOTHER = "PNC_MOTHER";
    public static final String PNC_NEONATE = "PNC_NEONATE";
    public static final String CHILDHOOD_VISIT = "CHILDHOOD_VISIT";
    public static final String ICCM_UNDER_2M = "UNDER_TWO_MONTHS";
    public static final String ICCM_ABOVE_2M_5Y = "UNDER_FIVE_YEARS";
    //Medical Review
    public static final String ABOVE_5_GENERAL_MEDICAL_REVIEW = "ABOVE_FIVE_YEARS";
    public static final String PREGNANCY_ANC_MEDICAL_REVIEW = "ANC_REVIEW";
    public static final String PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW = "MOTHER_DELIVERY_REVIEW";
    public static final String LABOUR_DETAILS = "MOTHER_LABOUR_DETAILS";
    public static final String PREGNANCY_ANC_NEONATE_MEDICAL_REVIEW = "NEONATE_BIRTH_REVIEW";
    public static final String PNC_MOTHER_MEDICAL_REVIEW = "PNC_MOTHER_REVIEW";
    public static final String PNC_CHILD_MEDICAL_REVIEW = "PNC_CHILD_REVIEW";

    //General Signs
    public static final String GENERAL_DANGER_SIGNS = "generalDangerSigns";
    public static final String VOMITING = "vomitingEverything";
    public static final String BREASTFEED = "breastfeed";
    public static final String UNUSUAL_SLEEPY = "unusualSleepy";
    public static final String CONVULSION = "convulsionPastFewDays";

    //nutritional Status
    public static final String NUTRITIONAL_STATUS = "nutritionalStatus";
    public static final String OEDEMA_OF_BOTH_FEET = "oedemaOfBothFeet";
    public static final String MUAC_CODE = "MUAC";

    public static final String FEVER = "fever";
    public static final String DIARRHOEA = "diarrhoea";
    public static final String COUGH = "cough";

    //Diarrhoea
    public static final String BLOODY_DIARRHOEA = "bloodyDiarrhoea";
    public static final String ZINC = "zinc";
    public static final String ORS = "ORS";
    public static final String JELLY_WATER = "JellyWater";
    public static final String SSS = "sss";

    //Fever
    public static final String RDT_TEST = "RDTTest";
    public static final String ACT = "ACT";
    public static final String TEMPERATURE = "temperature";

    //Cough
    public static final String CHEST_DRAWING = "chestIndrawing";
    public static final String AMOXICILLIN = "amoxicillin";
    public static final String BREATH_PER_MINUTE = "breathsPerMinute";

    //Medication Dispense status
    public static final String UNKNOWN = "UNKNOWN";
    public static final String COMPLETED = "COMPLETED";
    public static final String DISPENSED = "DISPENSED";

    //ANC
    public static final String TAKES_FANCIDAR_TABLETS = "takesFancidarTablets";
    public static final String LAST_MENSTRUAL_PERIOD = "lastMenstrualPeriod";
    public static final String ESTIMATED_DELIVERY_DATE = "estimatedDeliveryDate";
    public static final String IS_MALE_PARTNER_PRESENT = "isMalePartnerPresent";
    public static final String SLEEPS_UNDER_BED_NET = "sleepsUnderBedNet";
    public static final String EATS_MORE_THAN_BEFORE = "eatsMoreThanBefore";
    public static final String TAKES_IRON_FLOAT_TABLETS = "takesIronFolateTablets"; //Need to add medication Dispense
    public static final String PRIORITY_PREGNANCY = "priorityPregnancy";
    public static final String MISCARRIAGE = "miscarriageOrStillbirth";
    public static final String PLACE_OF_DELIVERY = "placeOfDelivery";
    public static final String OTHER_PLACE_OF_DELIVERY = "otherPlaceOfDelivery";
    public static final String EATS_4GROUP_IRON_VITA_RICH_FOODS = "eats4GroupIronAndVitaminARichFoods";
    public static final String GESTATIONAL_AGE = "gestationalAge";
    public static final String BIRTH_PLAN_MADE = "birthPlanMade";
    public static final String DEATH_OF_MOTHER = "deathOfMother";


    //PNC
    public static final String NO_OF_NEONATES = "noOfNeonates";
    public static final String NEONATE_PATIENT_ID = "neonatePatientId";
    public static final String DATE_OF_DELIVERY = "dateOfDelivery";
    public static final String PNC_CREATED_DATE = "pncCreatedDate";
    public static final String CHLORHEXIDINE = "chlorhexidine";
    public static final String FATHER_PRESENT = "fatherPresent";
    public static final String EXCLUSIVELY_BREASTFEEDING = "exclusivelyBreastfeeding";
    public static final String DEATH_OF_NEW_BORN = "deathOfNewBorn";
    public static final String LOW_BIRTH_WEIGHT = "lowBirthWeight";
    public static final String NEWBORN_REFERRED_TO_SBCU = "newBornReferredToSBCU";
    public static final String PLANNED_VISIT_DATE = "plannedVisitDate";
    public static final String ACTUAL_VISIT_DATE = "actualVisitDate";
    public static final String PENTA_OPV_GIVEN = "pentaOpvGiven";
    public static final String MEASLES_ONE_GIVEN = "measles1Given";
    public static final String YELLOW_FEVER_VACCINE_GIVEN = "yellowFeverVacineGiven";
    public static final String MEASLES_TWO_GIVEN = "measles2Given";
    public static final String POST_REFERRAL_FOLLOWUP_DONE = "postReferralFollowUpDone";

    //childHood
    public static final String MUAC = "MUAC";
    public static final String MUAC_CM = "muacInCentimeter";
    public static final String DEATH_OF_BABY = "deathOfBaby";
    public static final String FED_FROM_4FOOD_GROUPS = "fedFrom4FoodGroups";
    public static final String MOTHER_OR_PARTNER_USING_FAMILY_PLANNING = "motherOrPartnerUsingFamilyPlanning";
    public static final String TAKING_MINIMUM_MEALS_PER_DAY = "takingMinimumMealsPerDay";

    //Summary
    public static final String NO_OF_DAYS = "noOfDays";
    public static final String NO_OF_DAYS_OF_FEVER = "noOfDaysOfFever";
    public static final String NO_OF_DAYS_OF_DIARRHOEA = "noOfDaysOfDiarrhoea";
    public static final String NO_OF_DAYS_OF_COUGH = "noOfDaysOfCough";

    public static final String SIGNS = "signs";
    public static final String SIGNS_DISPLAY_NAME = "Signs";
    public static final String IS_TAKEN_TO_CLINIC = "isTakenToClinic";
    public static final String NEAREST_PHU_REFERRED_TO = "nearestPHUReferredTo";
    public static final String NEXT_VISIT_DATE = "nextVisitDate";

    //Meta Data Types
    public static final String META_UNDER_TWO_MONTHS = "UnderTwoMonths";
    public static final String META_UNDER_FIVE_YEARS = "UnderFiveYears";
    public static final String META_ABOVE_FIVE_YEARS = "AboveFiveYears";
    public static final String META_PNC_MOTHER_MEDICAL_REVIEW = "PNC-Mother";

    //Extension Types
    public static final String EXTENSION_EDUCATION_URL = "http://hl7.org/fhir/StructureDefinition/patientEducationLevel" ;
    public static final String EXTENSION_ENROLLMENT_URL = "http://hl7.org/fhir/StructureDefinition/enrollmentAt";
    public static final String EXTENSION_OCCUPATION_URL = "http://hl7.org/fhir/StructureDefinition/patientOccupation" ;

    //ABOVE FIVE YEARS
    public static final String CLINICAL_NOTES = "clinicalNotes";
    public static final String PRESENTING_COMPLAINTS = "presentingComplaints";
    public static final String SYSTEMIC_EXAMINATIONS = "systemicExaminations";
    public static final String PHYSICAL_EXAMINATIONS = "physicalExaminations";
    public static final String MEDICAL_SUPPLIES = "medicalSupplies";

    //ZERO TO TWO MONTHS Medical Review
    public static final String ACTIVITY = "activity";
    public static final String PULSE = "pulse";
    public static final String BMI = "bmi";
    public static final String FUNDAL_HEIGHT = "fundalHeight";
    public static final String GENDER = "gender";
    public static final String NEONATE_OUTCOME = "neonateOutcome";
    public static final String HEIGHT = "height";
    public static final String WEIGHT = "weight";
    public static final String WAZ = "WAZ";
    public static final String WHZ = "WHZ";
    public static final String SYSTOLIC = "systolic";
    public static final String MMHG = "mmHg";
    public static final String BPM = "BPM";
    public static final String CMS = "cms";
    public static final String CM = "cm";
    public static final String MIN = "min";
    public static final String KGM2 = "kg/m2";
    public static final String KG = "kg";
    public static final String DIASTOLIC = "diastolic";
    public static final String YELLOW_SKIN_OR_FACE_LESS_THAN_24_HRS = "yellowSkinOrFaceLessThan24hrs";
    public static final String YELLOW_PALMS_AND_SOLES = "yellowPalmsAndSoles";
    public static final String JAUNDICE_APPEARING = "jaundiceAppearing";
    public static final String NO_JAUNDICE = "noJaundice";
    public static final String SOLES_NOT_YELLOW = "solesNotYellow";
    public static final String STOPED_FEEDING = "stoppedFeeding";
    public static final String CONVULSIONS = "convulsions";
    public static final String SEVERE_CHEST_INDRAWING = "severeChestIndrawing";
    public static final String MOVES_WHEN_STIMULATED = "movesWhenStimulated";
    public static final String MOVE_ONLY_WHEN_STIMULATED_OR_NO_MOVEMENT_WHEN_STIMULATED =
            "moveWhenStimulatedOrNoMovementOnStimulation";
    public static final String LOW_BODY_TEMPERATURE = "lowBodyTemperature";
    public static final String UNBILICUS_RED_OR_DRAINING_PUS = "umbilicusRedOrDrainingPus";
    public static final String SKIN_PUSTULES = "skinPustules";
    public static final String TIME_PERIOD = "timePeriod";
    public static final String BLOOD_IN_STOOL = "bloodInStool";
    public static final String NO_MOVEMENT_ON_STIMILATION = "noMovementOnStimulation";
    public static final String RESTLESS_OR_IRRITABLE = "restlessOrIrritable";
    public static final String SUNKEN_EYES = "sunkenEyes";
    public static final String SKIN_PINCH = "skinPinch";
    public static final String POSITIVE_VIROLOGICAL_TEST_INFANT = "hasPositiveVirologicalTestForInfant";
    public static final String IS_MOTHER_POSITIVE_AND_CHILD_NEGATIVE = "isMotherPositiveAndChildNegative";
    public static final String POSITIVE_ANTIBODY_FOR_INFANT = "hasPositiveAntibodyTestForInfant";
    public static final String IS_MOTHER_POSITIVE_AND_INFANT_NOT_TESTED = "isMotherPositiveAndInfantNotTested";
    public static final String NEGATIVE_MOTHER_AND_CHILD = "hasNegativeTestForMotherOrChild";
    public static final String ANY_BREAST_FEEDING_DIFFICULTY = "anyBreastfeedingDifficulty";
    public static final String LESS_THAN_8_BREASTFEEDS_IN_24_HRS = "lessThan8BreastfeedsIn24hrs";
    public static final String SWITCHING_BREAST_FREQUENTLY = "switchingBreastFrequently";
    public static final String NOT_INCREASING_BF_DURING_ILLNESS = "notIncreasingBFDuringIllness";
    public static final String RECEIVES_OTHER_FOOD_OR_DRINKS = "receivesOtherFoodsOrDrinks";
    public static final String MOUTH_ULCER_OR_THRUSH = "mouthUlcersOrThrush";
    public static final String UNDER_WEIGHT = "underweight";
    public static final String POSITIONING = "positioning";
    public static final String ATTACHMENT = "attachment";
    public static final String SUCKLING = "suckling";
    public static final String NO_FEEDING_PROBLEM = "noFeedingProblem";
    public static final String INAPPROPRIATE_REPLACEMENT_FEEDS = "inappropriateReplacementFeeds";
    public static final String INSUFFICENT_REPLACEMENT_FEEDS = "insufficientReplacementFeeds";
    public static final String INCORRECTLY_PREPARED_MILK = "incorrectlyPreparedMilk";
    public static final String USE_OF_FEEDING_BOTTLE = "useOfFeedingBottle";
    public static final String HIV_MOTHER_MIXING_FEEDS = "HIVMotherMixingFeeds";
    public static final String BOTTLE_FEEDING = "bottleFeeding";
    public static final String LOW_WEIGHT_FOR_AGE = "lowWeightForAge";
    public static final String THRUSH = "thrush";
    public static final String IMMUNISATION_STATUS = "immunisationStatus";
    public static final String RESPIRATION_RATE = "respirationRate";
    public static final String VIT_A_FOR_MOTHER = "vitAForMother";
    public static final String ALBENDAZOLE = "albendazole";
    public static final String BREAST_FEEDING = "breastFeeding";
    public static final String OBSERVATION_JAUNDICE = "jaundice";
    public static final String OBSERVATION_VERY_RARE_DISEASE = "verySevereDisease";
    public static final String OBSERVATION_DIARRHOEA = "diarrhoea";
    public static final String OBSERVATION_BREAST_FEEDING_PROBLEM = "breastfeedingProblem";
    public static final String OBSERVATION_NON_BREAST_FEEDING_PROBLEM = "nonBreastfeedingProblem";
    public static final String OBSERVATION_HIV_INFECTION = "hivInfection";
    public static final String OBSERVATION_CLINICAL_SUMMARY = "clinicalSummaryAndSigns";
    public static final String OBSERVATION_JAUNDICE_DISPLAY_NAME = "Jaundice";
    public static final String OBSERVATION_VERY_RARE_DISEASE_DISPLAY_NAME = "Very Severe Disease (PSBI)";
    public static final String OBSERVATION_DIARRHOEA_DISPLAY_NAME = "Diarrhoea";
    public static final String OBSERVATION_BREAST_FEEDING_PROBLEM_DISPLAY_NAME = "Feeding Problem (For all Breastfeeding)";
    public static final String OBSERVATION_NON_BREAST_FEEDING_PROBLEM_DISPLAY_NAME = "Feeding Problem (For all Non-Breastfeeding)";
    public static final String OBSERVATION_HIV_INFECTION_DISPLAY_NAME = "Check for HIV Infection";
    public static final String OBSERVATION_COUGH = "cough";
    public static final String OBSERVATION_COUGH_DISPLAY_NAME = "Cough / Difficult Breathing";
    public static final String OBSERVATION_FEVER = "fever";
    public static final String OBSERVATION_FEVER_DISPLAY_NAME = "Fever";
    public static final String OBSERVATION_ANAEMIA = "anemia";
    public static final String OBSERVATION_EAR_PROBLEM = "earProblem";
    public static final String OBSERVATION_EAR_PROBLEM_DISPLAY_NAME = "Ear Problem";
    public static final String OBSERVATION_GENERAL_DANGER_SIGNS = "generalDangerSigns";
    public static final String OBSERVATION_GENERAL_DANGER_SIGNS_DISPLAY_NAME = "General Danger Signs";
    public static final String OBSERVATION_FEVER_SIGNS = "feverSigns";
    public static final String OBSERVATION_DIARRHOEA_SIGNS = "diarrhoeaSigns";
    public static final String OBSERVATION_ANAEMIA_SIGNS = "anemiaSigns";
    public static final String OBSERVATION_HIV_RDT = "hivRDT";
    public static final String OBSERVATION_ANAEMIA_DISPLAY_NAME = "Malnutrition / Anemia";
    public static final String OBSERVATION_HIV_RDT_DISPLAY_NAME = "HIV / AIDS  RDT";
    public static final String UNABLE_TO_DRINK_OR_BREASTFEED = "unableToDrinkOrBreastfeed";
    public static final String VOMITING_EVERYTHING = "vomitingEverything";
    public static final String HISTORY_OF_CONVULSION = "historyOfConvulsion";
    public static final String CONVULSING_NOW = "convulsingNow";
    public static final String LETHARGIC_OR_UNCONSCIOUS = "lethargicOrUnconscious";
    public static final String COUGH_OR_DIFFICULT_BREATHING = "coughOrDifficultBreathing";
    public static final String CHEST_INDRAWING = "chestIndrawing";
    public static final String STRIDOR = "stridor";
    public static final String HAS_FEVER = "hasFever";
    public static final String HAS_DIARRHOEA = "diarrhoea";
    public static final String IS_MOTHER_HAS_FEVER = "isMotherHasFever";
    public static final String MICROSCOPY_RESULT = "microscopyResult";
    public static final String HAS_EAR_PAIN = "hasEarPain";
    public static final String EAR_DISCHARGE = "earDischarge";
    public static final String APPETITE_TEST = "appetiteTest";
    public static final String MOTHER = "mother";
    public static final String CHILD = "child";
    public static final List<String> UNDER_2_MONTHS_EXAMINATION = List.of(OBSERVATION_JAUNDICE, OBSERVATION_VERY_RARE_DISEASE, OBSERVATION_DIARRHOEA,
            OBSERVATION_BREAST_FEEDING_PROBLEM, OBSERVATION_NON_BREAST_FEEDING_PROBLEM, OBSERVATION_HIV_INFECTION);
    public static final List<String> UNDER_5_YEARS_EXAMINATION = List.of(OBSERVATION_GENERAL_DANGER_SIGNS, OBSERVATION_COUGH, OBSERVATION_DIARRHOEA,
            OBSERVATION_FEVER, OBSERVATION_EAR_PROBLEM, OBSERVATION_ANAEMIA, OBSERVATION_HIV_RDT);
    public static final Map<String, String> UNDER_2_MONTHS_EXAMINATION_DISPLAY_NAMES_MAP = Map.of(OBSERVATION_JAUNDICE,
            OBSERVATION_JAUNDICE_DISPLAY_NAME, OBSERVATION_VERY_RARE_DISEASE,
            OBSERVATION_VERY_RARE_DISEASE_DISPLAY_NAME, OBSERVATION_DIARRHOEA, OBSERVATION_DIARRHOEA_DISPLAY_NAME,
            OBSERVATION_BREAST_FEEDING_PROBLEM, OBSERVATION_BREAST_FEEDING_PROBLEM_DISPLAY_NAME,
            OBSERVATION_NON_BREAST_FEEDING_PROBLEM, OBSERVATION_NON_BREAST_FEEDING_PROBLEM_DISPLAY_NAME,
            OBSERVATION_HIV_INFECTION, OBSERVATION_HIV_INFECTION_DISPLAY_NAME);
    public static final Map<String, String> UNDER_5_YEARS_EXAMINATION_DISPLAY_NAMES = Map.of(
            OBSERVATION_GENERAL_DANGER_SIGNS, OBSERVATION_GENERAL_DANGER_SIGNS_DISPLAY_NAME, OBSERVATION_COUGH,
            OBSERVATION_COUGH_DISPLAY_NAME, OBSERVATION_DIARRHOEA, OBSERVATION_DIARRHOEA_DISPLAY_NAME,
            OBSERVATION_FEVER, OBSERVATION_FEVER_DISPLAY_NAME, OBSERVATION_EAR_PROBLEM,
            OBSERVATION_EAR_PROBLEM_DISPLAY_NAME, OBSERVATION_ANAEMIA, OBSERVATION_ANAEMIA_DISPLAY_NAME,
            OBSERVATION_HIV_RDT, OBSERVATION_HIV_RDT_DISPLAY_NAME);
    public static final Map<String, String> UNDER_5_SIGNS = Map.of(OBSERVATION_ANAEMIA_SIGNS, OBSERVATION_ANAEMIA, OBSERVATION_FEVER_SIGNS, OBSERVATION_FEVER, OBSERVATION_DIARRHOEA_SIGNS, OBSERVATION_DIARRHOEA);


    public static final String APGAR_SCORE_ONE_MINUTE = "apgarScoreOneMinute";
    public static final String APGAR_SCORE_FIVE_MINUTE = "apgarScoreFiveMinute";
    public static final String APGAR_SCORE_TEN_MINUTE = "apgarScoreTenMinute";
    public static final String OBSTETRIC_EXAMINATION = "obstetricExaminations";
    public static final String PREGNANCY_HISTORY = "pregnancyHistory";
    public static final String BP = "bp";
    public static final String FETAL_HEART_RATE = "fetalHeartRate";
    public static final String DOES_MOTHER_HAVE_A_DELIVERY_KIT = "doesMotherHaveADeliveryKit";

    public static final String DATE_AND_TIME_OF_LABOUR_ONSET = "dateAndTimeOfLabourOnset";
    public static final String DATE_AND_TIME_OF_DELIVERY = "dateAndTimeOfDelivery";
    public static final String DELIVERY_TYPE = "deliveryType";
    public static final String DELIVERY_BY = "deliveryBy";
    public static final String DELIVERY_AT = "deliveryAt";
    public static final String DELIVERY_AT_OTHER = "deliveryAtOther";
    public static final String DELIVERY_STATUS = "deliveryStatus";
    public static final String DELIVERY_BY_OTHER = "deliveryByOther";
    public static final String OTHERS_SPECIFY = "Others (Specify)";
    public static final String SPECIFY = "Specify";
    public static final String GRIMACE = "grimace";
    public static final String APPEARANCE = "appearance";
    public static final String RESPIRATION = "respiration";
    public static final String NO_OF_FETUS = "noOfFetus";
    public static final String GENERAL_CONDITION_OF_MOTHER = "generalConditionOfMother";
    public static final String STATE_OF_THE_PERINEUM = "stateOfThePerineum";
    public static final String TEAR = "tear";
    public static final String STATE_OF_BABY = "stateOfBaby";
    public static final String GRAVIDA = "gravida";
    public static final String PARITY = "parity";
    public static final String PATIENT_BLOOD_GROUP = "patientBloodGroup";

    public static final String MOTHER_ALIVE = "motherAlive";
    public static final String CHILD_ALIVE = "childAlive";
    public static final String PNC_MOTHER_SYSTEMIC_EXAMINATION = "pncMotherSystemicExamination";
    public static final String PNC_CHILD_PHYSICAL_EXAMINATIONS = "pncMotherPhysicalExaminations";
    public static final String BREAST_CONDITION = "breastCondition";
    public static final String HAVE_BREATHING_PROBLEMS = "haveBreathingProblems";
    public static final String INVOLUTIONS_OF_THE_UTERUS = "involutionsOfTheUterus";
    public static final String BREAST_CONDITION_NOTES = "breastConditionCondition";
    public static final String INVOLUTIONS_OF_THE_UTERUS_NOTES = "involutionsOfTheUterusCondition";
    public static final String CONGENITAL_DETECT = "congenitalDetect";
    public static final String CORD_EXAMINATION = "cordExamination";
    public static final String EXCLUSIVE_BREAST_FEEDING = "exclusiveBreastFeeding";

    //Patient Status
    public static final String ON_TREATMENT = "OnTreatment";
    public static final String ON_TREATMENT_DISPLAY = "On Treatment";
    public static final String RECOVERED = "Recovered";
    public static final String REFERRED = "Referred";
    public static final String PREGNANCY = "Pregnant";
    public static final String POSTPARTUM = "Postpartum";
    public static final String POSTPARTUM_DISPLAYNAME = "Post Partum";
    public static final String POSTNATAL = "Postnatal";
    public static final String POST_NATAL = "Post Natal";
    public static final String NEONATE = "Neonate";
    public static final String LACTATING = "Lactating";
    public static final List<String> PREGNANCY_STATUS = List.of(Constants.PREGNANCY, Constants.POSTPARTUM,
            Constants.LACTATING, Constants.POSTPARTUM_DISPLAYNAME);
    public static final List<String> PATIENT_STATUS_LIST = List.of(Constants.ON_TREATMENT, Constants.RECOVERED,
            Constants.REFERRED, Constants.ON_TREATMENT_DISPLAY);
    public static final Map<String, String> PATIENT_STATUS_DISPLAY_NAMES = Map.of(Constants.ON_TREATMENT.toLowerCase(),
            Constants.ON_TREATMENT_DISPLAY, Constants.RECOVERED.toLowerCase(), Constants.RECOVERED,
            Constants.REFERRED.toLowerCase(), Constants.REFERRED, Constants.PREGNANCY.toLowerCase(),
            Constants.PREGNANCY, Constants.POSTPARTUM.toLowerCase(), Constants.POSTPARTUM_DISPLAYNAME,
            Constants.LACTATING.toLowerCase(), Constants.LACTATING);

    public static final String DATE = "date";
    public static final String ACTIVE = "active";
    public static final String PRELIMINARY = "preliminary";
    public static final String STRING_FINAL = "final";
    public static final String AMENDED = "amended";
    public static final String DRAFT = "draft";
    public static final String META = "meta";
    public static final String POST = "POST";
    public static final List<String> SKIP_STATUS = List.of(Constants.PREGNANCY, Constants.POSTPARTUM,
            Constants.LACTATING, Constants.POSTNATAL, Constants.RECOVERED);
    public static final String PHONE_FHIR = "PHONE";
    public static final String FHIR_CODE_SYSTEM_V3_ROLE_CODE = "http://terminology.hl7.org/CodeSystem/v3-RoleCode";


    public static final String SUMMARY = "summary";
    public static final String ATTRIBUTED_TO = "attributed-to";
    public static final String CLIENT = "client";
    public static final String RESPONSE = "response";
    public static final String URGENT = "urgent";
    public static final String ROUTINE = "routine";
    public static final String ON_HOLD = "on-hold";
    public static final String CHILDHOOD = "Childhood";
    public static final String STATUS = "status";
    public static final String ASSESSMENT_TYPE = "assessmentType";
    public static final String ASSESSMENT_CATEGORY = "assessmentCategory";
    public static final String IS_EDITABLE = "is_editable";
    public static final String QUESTION_MARK = "?";
    public static final String AND = "&";
    public static final String LOCATION = "location";
    public static final String AUTHORIZATION = "Authorization";
    public static final String TODAY = "today";
    public static final String YESTERDAY = "yesterday";
    public static final String THIS_WEEK = "week";
    public static final String THIS_MONTH = "month";
    public static final String TOMORROW = "tomorrow";
    public static final String FHIR_SEARCH_DATE_FORMAT = "yyyy-MM-dd";
    public static final int ONE = 1;
    public static final int NEGATIVE_ONE = -1;
    public static final int FIVE = 5;
    public static final int TEN = 10;
    public static final String NOT_APPLICABLE = "N/A";
    public static final String YES_CODE = "Y";
    public static final String NO_CODE = "N";
    public static final String YES = "YES";
    public static final String NO = "NO";
    public static final String NA = "NA";
    public static final String TEMPERATURE_UNIT = "°C";
    public static final String TEMPERATURE_SYSTEM = "http://unitsofmeasure.org";
    public static final String TEMPERATURE_CODE = "Cel";
    public static final String NOTES = "Notes";
    public static final String HAS_COUGH = "hasCough";
    public static final String COUGH_LASTED_THAN_2_WEEKS = "cough_lasted_than_2_weeks";
    public static final String WEIGHT_LOSS = "weightLoss";
    public static final String FHIR_HISTORY_ID_REGEX = "/_history";
    public static final String TB = "TB";
    protected static final Map<String, List<FhirCodeDTO>> CODES = new HashMap<>();
    public static final String CLINIC = "Clinic";
    public static final String LOINC_URL = "http://loinc.org";
    public static final String EMPTY_SPACE = " ";
    public static final String EMPTY_STRING = "";
    public static final String META_CODE_DETAILS_REDIS_KEY = "metaCodeDetails";
    public static final String PATIENT_LIST = "patientList";
    public static final String TOTAL_COUNT = "totalCount";
    public static final String IDENTIFIER = "identifier";
    public static final String SYMPTOMS = "Symptoms";
    public static final String SYMPTOM = "symptom";
    public static final String COMPLETED_SMALLER_CASE = "completed";
    public static final String PERSONAL = "personal";
    public static final String MEDICAL_REVIEW = "medicalReview";
    public static final String ASSESSMENT = "assessment";
    public static final String DISPENSE = "dispense";
    public static final String REGISTRATION = "registration";
    public static final String FAMILY_MEMBER = "family member";
    public static final String FRIEND = "friend";
    public static final String MALE = "male";
    public static final String FEMALE = "female";
    public static final String NON_BINARY = "non-binary";
    public static final String BREATHING = "BREATHING";
    public static final String NATIONAL_ID = "nationalId";
    public static final String PATIENT_UPDATION_TYPE = "FROM_UPDATE_PATIENT";
    public static final String PATIENT_CREATION_TYPE = "FROM_CREATE_PATIENT";

    //VITAL OBSERVATION
    public static final String ANC_VISIT_NUMBER = "ancVisitNumber";
    public static final String PNC_VISIT_NUMBER = "pncVisitNumber";
    public static final String CHILDHOOD_VISIT_NUMBER = "childhoodVisitNumber";
    public static final String NEONATE_DEATH_RECORDED_BY_PHU = "isNeonateDeathRecordedByPHU";
    public static final List<String> OBSERVATION_INTEGER_FIELDS = List.of(ANC_VISIT_NUMBER, PNC_VISIT_NUMBER,
            CHILDHOOD_VISIT_NUMBER
            , NO_OF_NEONATES, PREGNANCY_ANC_MEDICAL_REVIEW, PNC_MOTHER_MEDICAL_REVIEW, PNC_CHILD_MEDICAL_REVIEW, GESTATIONAL_AGE);
    public static final List<String> OBSERVATION_DATE_FIELDS = List.of(LAST_MENSTRUAL_PERIOD, ESTIMATED_DELIVERY_DATE
            , DATE_OF_DELIVERY, PNC_CREATED_DATE);
    public static final List<String> OBSERVATION_BOOLEAN_FIELDS = List.of(NEONATE_DEATH_RECORDED_BY_PHU);
    public static final List<String> OBSERVATION_STRING_FIELDS = List.of(NEONATE_PATIENT_ID, NEONATE_OUTCOME);
    public static final int MAX_LIMIT = 99999;

    //Query Params
    public static final String OBSERVATION = "Observation";
    public static final String PERFORMER_PARAM = "performer=RelatedPerson/%s";
    public static final String DATE_PARAM_ASC = "_sort=-date";
    public static final String COUNT_PARAM = "_count=%s";
    public static final String OFFSET_PARAM = "_getpagesoffset=%s";
    public static final String CODE_PARAM = "code=%s";
    public static final String TOTAL_COUNT_PARAM = "_summary=count";
    public static final String IDENTIFIER_PARAM = "identifier=%s";
    public static final String ENCOUNTER_PARAM = "encounter=%s";
    public static final String COMPONENT_CODE_TEXT_PARAM = "component-code:text=%s";

    public static final String PATIENT_LIST_PARAMS = "ServiceRequest?_include=ServiceRequest" +
            ":subject&priority=urgent&_count=%s&_getpagesoffset=%s&requisition=%s&subject.active=true&_sort=-subject";
    public static final String PATIENT_TOTAL_PARAMS = "ServiceRequest?priority=urgent&_elements=subject" +
            "&_count=99999&requisition=%s&subject.active=true";
    public static final String PROVENANCE_INCLUDES_RELATED_PERSON_AND_ENCOUNTER = "Provenance?agent=%s&agent-role:text=POST&recorded=ge%s&recorded=le%s&_include=Provenance:target:RelatedPerson&_include=Provenance:target:Encounter&_include=Provenance:target:Observation&_include=Provenance:target:MedicationDispense&_count=999999";
    public static final String PROVENANCE_INCLUDES_DIAGNOSTIC_REPORT = "Provenance?agent=%s&agent-role:text=PUT&recorded=ge%s&recorded=le%s&_include=Provenance:target:DiagnosticReport&_count=999999";
    public static final String PREGNANT_SEARCH_PARAM = "&subject.pregnancy-extension-search=NO,NA";
    public static final String PREGNANT_PATIENT_LIST_PARAMS = "RelatedPerson?_count=%s&_getpagesoffset=%s&pregnancy-extension-search=YES";
    public static final String PREGNANT_TOTAL_PARAMS = "RelatedPerson?_count=0&pregnancy-extension-search=YES&active=true";
    public static final String PATIENT_SUBJECT_IDENTIFIER = "&subject.identifier=%s";
    public static final String PATIENT_IDENTIFIER = "&identifier=%s";
    public static final String PATIENT_STATUS_FILTER_PARAM = "&status=%s";
    public static final String PATIENT_ACTIVE_STATUS = "&active=true";
    public static final String PATIENT_REFERENCE_FILTER_PARAM = "&_id:not=%s";
    public static final String PATIENT_NAME_SEARCH_PARAM = "&subject.name=%s";
    public static final String PATIENT_PHONE_SEARCH_PARAM = "&subject.phone-search=%s";
    public static final String PATIENT_IDENTIFIER_SEARCH_PARAM = "&subject.patient-id-search=%s";
    public static final String PATIENT_VISIT_DATE_FILTER = "&occurrence=ge%s&occurrence=le%s";
    public static final Map<String, String> PATIENT_STATUS_FHIR_FILTER_MAPPING = Map.of(Constants.REFERRED, Constants.ACTIVE,
            Constants.ON_TREATMENT, Constants.ON_HOLD);
    public static final String PATIENT_VISIT_DATE_FILTER_PARAM = "&_has:ServiceRequest:subject:occurrence=%s";
    public static final String RELATED_PERSON_LIST_PARAMS = "RelatedPerson?_count=%s&_getpagesoffset=%s&active=true";
    public static final String PAGINATE_PARAMS = "_count=%s&_getpagesoffset=%s";
    public static final String LAST_UPDATED_DESC_SORT = "&_sort=-_lastUpdated";
    public static final String FILTER_PARAM = "_filter=";
    public static final String IDENTIFIER_EQ_FILTER = "identifier eq %s|%s";
    public static final String IDENTIFIER_NE_FILTER = "identifier ne %s|%s";
    public static final String OR = "or";
    public static final String AND_STRING = "and";
    public static final String ORGANIZATION_FILTER = "organization eq %s";
    public static final String HOUSEHOLD_MEMBER_WITH_PATIENT_VITALS_LIST_PARAMS = "Observation?performer.identifier=%s&" +
            "identifier=%s&status=%s&_getpagesoffset=%s&_count=%s&_elements=performer&_sort=-performer";
    public static final String RELATED_PERSON_QUERY = "RelatedPerson?identifier=%s&identifier=%s&active=true&_sort=-_lastUpdated&_count=1&_include=RelatedPerson:patient";
    public static final String MENTAL_HEALTH_OBSERVATION_QUERY = "Observation?identifier=%s&performer=%s&_sort=-_lastUpdated&_count=1&_include=Observation:has-member";
    public static final String GET_RED_RISK_DETAILS = "Observation?identifier=%s&performer=RelatedPerson/%s&_sort=-_lastUpdated";
    public static final String PRESCRIPTION_DISPENSE_ENCOUNTER_LIST_QUERY = "Encounter?_id=%s&subject=Patient/%s&identifier=%s&_sort=-_lastUpdated";
    public static final String INVESTIGATION_ENCOUNTER_LIST_QUERY = "Encounter?_id=%s&subject=Patient/%s&identifier=%s&_sort=-_lastUpdated";
    public static final String PRESCRIPTION_DISPENSE_HISTORY = "MedicationDispense?context=%s&_sort=-_id";
    public static final String PRESCRIPTION_PRESCRIBED_HISTORY = "MedicationRequest?encounter=%s&_sort=-_id";
    public static final String INVESTIGATION_BY_ENCOUNTER = "DiagnosticReport?encounter=%s&_sort=-issued&_include" +
            "=DiagnosticReport:performer";
    public static final String STATUS_NOT_CANCELLED = "&status:not=cancelled";
    public static final String GET_LATEST_MEDICATION_REQUEST_PERFORMER_BY_PATIENT = "MedicationRequest?subject=%s&_sort=-authoredon&_count=1&_include=MedicationRequest:intended-performer";
    public static final String SEARCH_RELATED_PERSON_DETAILS_USING_ENCOUNTER = "Encounter?participant=Participant/%s&_sort=-_lastUpdated&_count=1&_include=Encounter:subject&_include=Encounter:participant&_include=Encounter:location&_revinclude=Observation:encounter&_revinclude=QuestionnaireResponse:encounter";
    public static final String SEARCH_RELATED_PERSON_DETAILS_USING_ENCOUNTER_TYPE = "Encounter?identifier=type|%s&participant=Participant/%s&_sort=-_lastUpdated&_count=1&_include=Encounter:subject&_include=Encounter:participant&_include=Encounter:location&_revinclude=Observation:encounter&_revinclude=QuestionnaireResponse:encounter";
    public static final String SEARCH_RELATED_PERSON_DETAILS = "RelatedPerson?identifier=%s&_sort=-_lastUpdated&_getpagesoffset=%s&_count=%s&_revinclude=Observation:performer&_count=1&_revinclude=QuestionnaireResponse:source&_revinclude=Patient:link";
    public static final String IDENTIFIER_NOT_PARAM = "&identifier:not=%s|%s";
    public static final String PATIENT_LIST_PARAMS_TOTAL_COUNT = "Patient?identifier=%s|%s&_count=%s&_has:ServiceRequest:subject:intent=order";

    public static final String SEARCH_VITAL_SIGNS_BY_RELATED_PERSON = "Observation?identifier=%s|%s&performer=RelatedPerson/%s&_sort=-_lastUpdated&_count=1&_include=Observation:derived-from&_include=Observation:subject";
    public static final String SEARCH_VITAL_SIGNS_BY_RELATED_PERSONS = "Observation?identifier=%s|%s&performer=RelatedPerson/%s&_sort=-_lastUpdated&_include=Observation:derived-from";
    public static final String SEARCH_VITAL_SIGNS = "Observation?_id=%s&_include=Observation:derived-from&_include=Observation:subject";
    public static final String SEARCH_RELATED_PERSON_VITAL_SIGNS = "Observation?identifier=%s|%s&performer=RelatedPerson/%s&_sort=-_lastUpdated";
    public static final String SEARCH_RELATED_PERSON = "RelatedPerson?active=true&_id=%s&_sort=-_lastUpdated&_count=1&_revinclude=Patient:link";
    public static final String SEARCH_RELATED_PERSONS = "RelatedPerson?identifier=%s&_sort=-_lastUpdated&_count=%s&_getpagesoffset=%s";
    public static final String SEARCH_RELATED_PERSON_DETAILS_NAME = "RelatedPerson?name:contains=%s&_sort=-_lastUpdated&_count=%s&_getpagesoffset=%s";
    public static final String SEARCH_RELATED_PERSON_DETAILS_NO = "RelatedPerson?phone-search=%s&_sort=-_lastUpdated&&_count=%s&_getpagesoffset=%s";
    public static final String PATIENT_PARAMS_NAME = "&name:contains=%s";
    public static final String PATIENT_PARAMS_TELECOM = "&phone-search=%s";
    public static final String PATIENT_PARAMS_PATIENT_ID = "&patient-id-search=%s";
    public static final String GET_GROUP_BY_MEMBER_ID_PARAM = "Group?characteristic:text=%s";
    public static final String HOUSEHOLD_LIST_PARAMS = "Group?identifier=%s&_getpagesoffset=%s&_count=%s&_sort=-_lastUpdated";
    public static final String PATIENT_BASIC_DETAILS_PARAMS = "Observation?identifier=%s|%s&code:text=%s&performer=%s" +
            "&status=preliminary";
    public static final String PATIENT_BASIC_DETAILS_PARAMS_USE_PATIENT = "Observation?identifier=%s|%s&code:text=%s&subject=%s" +
            "&status=preliminary";
    public static final String UPDATE_ENCOUNTER_PARAMS = "Encounter?identifier=%s|%s&status=in-progress&subject=%s";
    public static final String SERVICE_REQUEST_QUERY = "ServiceRequest?_count=9999&_sort=-_id&subject=%s";
    public static final String SERVICE_REQUEST_QUERY_PERFORMER = "ServiceRequest?_sort=-_id&performer=%s&priority=urgent";
    public static final String SERVICE_REQUEST_STATUS_NOT_COMPLETE_QUERY = "&status=on-hold,active";
    public static final String SERVICE_REQUEST_STATUS_ENCOUNTER_TYPE_QUERY = "&identifier=%s|%s";
    public static final String IDENTIFIER_QUERY = "&identifier=%s";
    public static final String SERVICE_REQUEST_STATUS_REQUISITION_QUERY = "&requisition=%s|%s";
    public static final String HOUSEHOLD_MEMBER_LIST_PARAMS = "RelatedPerson?identifier=%s&_getpagesoffset=%s&_count=%s&_sort=-_lastUpdated";
    public static final String GET_HOUSEHOLD_MEMBER_PARAMS = "RelatedPerson?identifier=%s|%s&_sort=-_id";
    public static final String GET_HOUSEHOLD_PARAMS = "Group?_id=%s&_count=%s";
    public static final String GET_ENCOUNTER_ID = "Encounter?_id=%s";
    public static final String GET_MEMBER_ID = "RelatedPerson?_id=%s";
    public static final String GET_PATIENT_COVERAGE = "Coverage?beneficiary=Patient/%s";
    public static final String GET_PATIENT_APPOINTMENT = "Appointment?actor=RelatedPerson/%s&identifier=%s|%s&_sort=-_lastUpdated";
    public static final String GET_LATEST_OBSERVATION = "Observation?code:text=%s&performer=%s&_sort=-date&_count=1";
    public static final String GET_LATEST_ENCOUNTER = "Encounter?_sort=-_lastUpdated&identifier=%s|%s&subject=%s&_count=1";
    public static final String GET_MEDICAL_DISPENSE_ENCOUNTER_BY_PATIENT = "MedicationDispense?subject=%s" +
            "&_sort=-whenhandedover&_include=MedicationDispense:context&_count=1";
    public static final String GET_PATIENT_BY_ID = "Patient?identifier=%s|%s";
    public static final String GET_ACTIVE_STATUS_SERVICE_REQUEST = "ServiceRequest?performer=%s&status=%s&_count=0&identifier=%s|%s";
    public static final String GET_PATIENT_BY_PATIENT_REFERENCE = "Patient?_id=%s";
    public static final String GET_PATIENT_BY_MEMBER_ID = "Patient?link=%s";
    public static final String INCLUDE_SUB_RESOURCES = "&_include=ServiceRequest:requester&_include=ServiceRequest:performer&_include=ServiceRequest:encounter&_include=ServiceRequest:subject&_revinclude=Provenance:target";
    public static final String ID_WHERE = "&_id=%s";
    public static final String IDNTIFIER_WHERE = "&identifier=%s|%s";
    public static final String PRIORITY_QUERY_PARAMS = "&priority=%s";
    public static final String IDNTIFIER_NOT = "&identifier:not=%s|%s";
    public static final String REV_INCLUDE_ENCOUNTER_OBSERVATION = "&_revinclude=Observation:encounter&_count=9999" +
            "&_include=Encounter:diagnosis&_include=Encounter:subject";
    public static final String GET_LOCATION_PARAMS = "Location?_id=%s&_count=%s";
    public static final String HOUSEHOLD_MEMBER_LIST = "RelatedPerson?_id=%s&_count=%s";
    public static final String DEVICE_LIST = "Device?_id=%s&_count=%s";
    public static final String PRACTIIONER_PARAMS = "Practitioner?_id=%s";
    public static final String ORGANIZATION_PARAMS = "Organization?_id=%s";
    public static final String COVERAGE_BY_PATIENT_PARAMS = "Coverage?patient=%s";
    public static final String PATIENT_VITAL_OBSERVATION_LIST_PARAMS = "Observation?identifier=%s|%s&performer=%s" +
            "&status=preliminary&_count=9999";
    public static final String PREVIOUS_SERVICE_REQUEST = "ServiceRequest?identifier=%s|%s&priority=urgent" +
            "&requisition=%s|%s&performer=%s";
    public static final String GET_SERVICE_REQUEST_BY_PERFORMER = "ServiceRequest?performer=%s&status=%s&_sort=-_lastUpdated";
    public static final String LAST_SYNC_QUERY_PARAMS = "_lastUpdated=gt%s";
    public static final String CURRENT_SYNC_QUERY_PARAMS = "_lastUpdated=le%s";
    public static final String GET_PRESCRIPTION_BY_PATIENT_AND_ENCOUNTER = "MedicationRequest?subject=%s&_sort" +
            "=authoredon";
    public static final String ACTIVE_PRESCRIPTION_LIST_PARAMS = "MedicationRequest?subject=%s&status=active,completed&_count=9999&_sort=-_id";
    public static final String ACTIVE_PRESCRIPTION_LIST_USING_RELATED_PERSON = "MedicationRequest?requester=%s&status=active,completed&_count=9999&_sort=-_id";
    public static final String PRESCRIPTION_LIST_PARAMS = "MedicationRequest?subject=%s&status=%s&_count=9999&_sort=-_id";
    public static final String INVESTIGATION_LIST_PARAMS = "DiagnosticReport?identifier=%s|&subject=%s&status=%s&_count=9999&_sort=-_id";
    public static final String PROJECTION_PARAMS = "_elements=%s";
    public static final String GET_PRESCRIPTION_PARAMS = "MedicationRequest?_id=%s";
    public static final String CONDITION_QUERY_VERIFICATION_STATUS_WITH_IDENTIFIER = "Condition?subject=Patient/%s" +
            "&verification-status:code=confirmed&identifier=%s&_count=9999";
    public static final String CONDITION_QUERY_VERIFICATION_STATUS = "Condition?subject=Patient/%s" +
            "&verification-status:code=confirmed&_count=9999";
    public static final String PROVISIONAL_DIAGNOSIS_QUERY = "Condition?asserter=RelatedPerson/%s" +
            "&verification-status:text=provisional&_count=9999";
    public static final String DIAGNOSTIC_REPORT_QUERY = "DiagnosticReport?_id=%s";
    public static final String DIAGNOSTIC_REPORT_QUERY_WITH_ENCOUNTER = "DiagnosticReport?encounter=%s&status:not=cancelled";
    public static final String DIAGNOSTIC_REPORT_QUERY_WITH_STATUS = "DiagnosticReport?identifier=%s|&subject=Patient" +
            "/%s&status=%s&_count=9999&_include=DiagnosticReport:results-interpreter&_include=DiagnosticReport" +
            ":performer&_include=DiagnosticReport:result&_sort=-issued";
    public static final String OBSERVATION_QUERY_WITH_ID = "Observation?_id=%s";
    public static final String KEY_ENCOUNTER_ID = "encounterId";
    public static final String KEY_CHILD_ENCOUNTER_ID = "childEncounterId";
    public static final String KEY_CHILD_PATIENT_REFERENCE = "childPatientReference";
    public static final String KEY_PATIENT_REFERENCE = "patientReference";
    public static final String CONDITION_CLINCAL_STATUS_SYSTEM = "http://terminology.hl7.org/CodeSystem/condition-clinical";
    public static final String CLINCAL_STATUS_CODE_ACTIVE = "active";
    public static final String CLINCAL_STATUS_DISPLAY_ACTIVE = "Active";
    public static final String CLINCAL_STATUS_CODE_INACTIVE = "inactive";
    public static final String CLINCAL_STATUS_DISPLAY_INACTIVE = "Inactive";
    public static final String TYPE = "type";
    public static final String GET_ENCOUNTER_BY_ID_QUERY = "Encounter?_id=%s";
    public static final String INCLUDE_PATIENT_ORGANIZATION = "&_include=Patient:organization";


    public static final String GET_PRESCRIPTION_VISIT_ENCOUNTER_QUERY = "Encounter?_sort=date&subject=Patient/%s&identifier=%s";
    public static final String GET_PRESCRIPTION_ENCOUNTER_QUERY = "Encounter?_sort=-date&subject=Patient/%s&identifier=%s&_count=9999";
    public static final String GET_INVESTIGATION_ASC_ENCOUNTER_QUERY = "Encounter?_sort=date&subject=Patient/%s&identifier=%s&_count=9999";
    public static final String GET_INVESTIGATION_ENCOUNTER_QUERY = "Encounter?_sort=-date&subject=Patient/%s&identifier=%s&_count=9999";
    public static final String MEDICATION_REQUEST_HISTORY = "MedicationRequest/%s/_history?_count=9999";
    public static final String INVESTIGATION_HISTORY = "DiagnosticReport/%s/_history?_count=9999";
    public static final String FHIR_RESOURCE_ENCOUNTER = "Encounter";
    public static final String FHIR_RESOURCE_PATIENT = "Patient";
    public static final String FHIR_RESOURCE_MEDICATION_REQUEST = "MedicationRequest";
    public static final String FHIR_RESOURCE_MEDICATION_DISPENSE = "MedicationDispense";
    public static final String FETCH_LATEST_OBSERVATION_QUERY = "Observation?code:text=%s&status=%s&performer" +
            "=RelatedPerson/%s&_sort=-date&_count=1";
    public static final String GET_PSYCHOLOGY_NOTES_BY_USER_AND_RELATED_PERSON_ID = "Provenance?agent=Practitioner/%s" +
            "&_include=Provenance:target:Observation&target:Observation.performer=RelatedPerson/%s&target:Observation.identifier=%s";
    public static final String GET_PSYCHOLOGY_BY_IDENTIFIER_TYPE_AND_RELATED_PERSON_ID = "Observation?identifier=%s,%s&performer=%s&_include=Observation:performer&_count=99999";
    public static final String GET_OBSERVATION_BY_DERIVED_FROM = "Observation?derived-from=%s";
    public static final String GET_PRACTITIONER_BY_OBSERVATION_ID = "Provenance?target=Observation/%s&_include=Provenance:agent";
    public static final String GET_OBSERVATION_BY_IDENTIFIER = "Observation?status=%s&identifier=%s&subject=%s";
    public static final String CODE_VALUE_PARAM = "value-quantity:missing=false";
    public static final String FHIR_RESOURCE_MEMBER = "RelatedPerson?";
    public static final String NAME_AND_TELECOM_PARAM = "name:contains=%s&name:contains=%s&telecom=%s&active=true";
    public static final String ACTIVE_TRUE = "&active=true";
    public static final String GET_ENCOUNTER_BY_MEMBER_ID = "Encounter?identifier=%s|%s&participant=%s";

    // condition query
    public static final String PATIENT_STATUS_CONDITION_QUERY = "Condition?subject=Patient/%s&identifier=%s";
    public static final String PREGNANCY_CONDITION_QUERY = "Condition?identifier=%s&asserter=RelatedPerson/%s";
    public static final String CONDITION_QUERY = "Condition?subject=Patient/%s&identifier=diagnosis&identifier=%s&clinical-status:code=active&_count=9999";
    public static final String CONDITION_QUERY_WITHOUT_TYPE = "Condition?subject=Patient/%s&identifier=diagnosis" +
            "&clinical-status:code=active&_count=9999";

    public static final String PRESCRIBED = "Prescribed";
    public static final String PRESCRIPTION_DISPENSED = "Dispensed";
    public static final String PRESCRIPTION = "Prescription";
    public static final String INVESTIGATED = "Investigated";
    public static final String EVERYTHING = "$everything";
    public static final String MUAC_UPPER_CASE = "MUAC";

    public static final String RMNCH_VISIT = "RMNCH_VISIT";
    public static final String RMNCH_VISIT_REASON = "%s Visit %s";
    public static final Map<String, String> RMNCH_VISIT_MAPPING = Map.of(ANC, ANC, PNC_MOTHER, PNC,
            PNC_NEONATE, PNC, CHILDHOOD_VISIT, CHILDHOOD);
    public static final String THREE_DIGIT_FORMAT_SPECIFIER = "%03d";
    public static final String FOUR_DIGIT_FORMAT_SPECIFIER = "%04d";
    public static final String OTHER_NOTES = "otherNotes";
    public static final String LATE_PRETERM = "Late Preterm";
    public static final String MODERATE_PRETERM = "Moderate Preterm";
    public static final String VERY_PRETERM = "Very Preterm";
    public static final String EXTREMELY_PRETERM = "Extremely Preterm";
    public static final String EARLY_TERM = "Early Term";
    public static final String FULL_TERM = "Full Term";
    public static final String LATE_TERM = "Late Term";
    public static final String POST_TERM = "Post Term";
    public static final String LBW = "Low Birth Weight";
    public static final String VLBW = "Very Low Birth Weight";
    public static final String ELBW = "Extremely Low Birth Weight";
    public static final String NBW = "Normal Birth Weight";
    public static final String HBW = "High Birth Weight";
    public static final String FHIR_RESOURCE_DIAGNOSTIC_REPORT = "DiagnosticReport";
    public static final String FHIR_RESOURCE_OBSERVATION = "Observation";
    public static final String FHIR_RESOURCE_PRACTITIONER = "Practitioner";

    //AUTO close reasons
    public static final int PNC_VISIT_CLOSE_DAYS = 15;
    public static final int CHILD_VISIT_CLOSE_DAYS = 456;
    public static final String AUTO_CLOSE_CHILHOOD_REASON = "Auto Close(Child visit More than 456 Days)";
    public static final String AUTO_CLOSE_PNC_MOTHER_REASON = "Auto Close(PNC More than 15 Days)";
    public static final String AUTO_CLOSE_PNC_CHILD_REASON = "Auto Close(PNC More than 15 Days)";
    public static final String AUTO_CLOSE_ANC_REASON = "Auto Close(Due to PNC visit start)";
    public static final String AUTO_CLOSE_PNC_REASON = "Auto Close(Due to ANC visit start)";
    public static final String AUTO_CLOSE_LABOUR_REASON = "Auto Close(Due to Labour visit start)";
    public static final String AUTO_REFERRAL_TICKET = "Ticket created by our system(AutoReferral).";
    public static final String AUTO_CLOSED_TICKET = "Ticket closed by our system(AutoClose).";
    public static final String RMNCH_VISIT_COMPLETED = "VISIT COMPLETED";
    public static final String AUTO_REFERRAL = "AutoReferral";
    public static final String AUTO_CLOSE = "AutoClose";
    public static final String PATIENT_DEAD = "Patient Dead";
    public static final String PNC_VISIT = "PNC Visit";
    public static final List<String> AUTO_CLOSE_REASONS = new ArrayList<>(
            List.of(AUTO_CLOSE_CHILHOOD_REASON, AUTO_CLOSE_PNC_MOTHER_REASON, AUTO_CLOSE_PNC_CHILD_REASON,
                    AUTO_CLOSE_ANC_REASON, AUTO_CLOSE_PNC_REASON, AUTO_CLOSE_LABOUR_REASON, RMNCH_VISIT_COMPLETED, PATIENT_DEAD));

    public static final String STRING_NAME = "String";
    public static final String DATE_NAME = "Date";
    public static final String CODE_NAME = "Code";
    public static final String QUANTITY_NAME = "Quantity";
    public static final String BOOLEAN_NAME = "Boolean";
    public static final String INTEGER_NAME = "Integer";
    public static final String DECIMAL_NAME = "Decimal";
    public static final String FINAL_STATUS = "final";
    public static final String REGISTERED_STATUS = "registered";
    public static final String PRACTITIONER = "Practitioner";
    public static final String HOUSEHOLD_COUNT = "Group?_has:Provenance:target:agent=%s&identifier=%s&_has:Provenance:target:recorded=ge%s&_has:Provenance:target:recorded=le%s&_count=0&_sort=_id";
    public static final String PATIENT_COUNT = "Patient?_has:Provenance:target:agent=%s&identifier=%s&_has:Provenance:target:recorded=ge%s&_has:Provenance:target:recorded=le%s&_count=0&_sort=_id";
    public static final String ICCM_RMNCH_COUNT = "Encounter?_has:Provenance:target:agent=%s&patient.identifier=%s&identifier=%s&_has:Provenance:target:recorded=ge%s&_has:Provenance:target:recorded=le%s&_count=0&_sort=_id";
    public static final String SERVICE_REQUEST_COUNT = "ServiceRequest?_has:Provenance:target:agent=%s&patient.identifier=%s&identifier=%s&_has:Provenance:target:recorded=ge%s&_has:Provenance:target:recorded=le%s&_count=0&_sort=_id";
    public static final String SERVICE_REQUEST_BY_PATIENT_ID_QUERY = "ServiceRequest?subject=%s&status=active&_sort=-_lastUpdated";
    public static final String SERVICE_REQUEST_COUNT_WORSENED = "ServiceRequest?_has:Provenance:target:agent=%s&patient.identifier=%s&identifier=%s&identifier=%s&_has:Provenance:target:recorded=ge%s&_has:Provenance:target:recorded=le%s&_count=0&_sort=_id";
    public static final String HOUSEHOLD = "Household";
    public static final String HOUSEHOLD_MEMBER = "HouseholdMember";
    public static final String PATIENT = "Patient";
    public static final String WORSENED = "Worsened";
    public static final String PATIENT_STATUS = "PatientStatus";
    public static final String FOLLOW_UP_COND_VISIT = "FOllowUpCondVisit";

    //Performance Constants
    public static final List<String> ASSESSMENTS = List.of(ANC, PNC_MOTHER, PNC_NEONATE, CHILDHOOD_VISIT,
            OTHER_SYMPTOMS, ICCM);
    public static final List<String> PATIENT_STATUS_CATEGORY = List.of(ICCM, RMNCH, CHILDHOOD);
    public static final List<String> RMNCH_ASSESSMENTS = List.of(ANC, PNC_MOTHER, PNC_NEONATE, CHILDHOOD_VISIT);
    public static final int PERFORMANCE_SEARCH_LIMIT_VALUE = 499;
    public static final String PERFORMANCE_QUERY_ENCOUNTER = "Provenance?agent=%s&_include=Provenance:target&target:Encounter.identifier=%s&agent-role=TRANS";
    public static final String PERFORMANCE_QUERY_LIMIT_SKIP = "&_count=%s&_getpagesoffset=%s";
    public static final String PERFORMANCE_QUERY_GROUP = "Provenance?agent=%s&_include=Provenance:target&target:Group.identifier=%s&agent-role=TRANS";
    public static final String PERFORMANCE_QUERY_HOUSEHOLD_MEMBER = "Provenance?agent=%s&_include=Provenance:target&target:RelatedPerson.identifier=%s&agent-role=TRANS";
    public static final String PERFORMANCE_QUERY_SERVICE_REQUEST = "Provenance?agent=%s&_include=Provenance:target" +
            "&target:ServiceRequest.performer.identifier=%s&agent-role=TRANS";
    public static final String PERFORMANCE_QUERY_DATE_FILTER = "&recorded=ge%s&recorded=le%s";
    public static final String PERFORMANCE_QUERY_RELATED_PERSON = "RelatedPerson?_id=%s&_count=999";
    public static final List<String> SKIP_STATUS_CATEGORY = new ArrayList<>(
            List.of(Constants.RMNCH_VISIT, Constants.CHILD_VISIT));
    public static final String CHILD_VISIT = "CHILD_VISIT";
    public static final String ABOVE_FIVE_YEARS = "AboveFiveYears";
    public static final String NCD = "NCD";
    public static final String MENTAL_HEALTH = "Mental Health";
    public static final String MATERNAL_HEALTH = "Maternal Health";

    public static final String ENROLLED = "ENROLLED";
    public static final String ENROLLMENT = "enrollment";
    public static final String SCREENING_SEARCH_PARAM = "Observation?code:text=%s&performer=%s&_sort=-_lastUpdated";
    public static final String DOOR_TO_DOOR = "Door to Door";
    public static final String CAMP = "camp";

    public static final Map<String, String>  FREQUNCY_TYPE_VALUE_MAP = new HashMap<>();
    public static final String FREQUENCY_BP_CHECK = "BP Check";
    public static final String FREQUENCY_BG_CHECK = "BG Check";
    public static final String FREQUENCY_MEDICAL_REVIEW = "Medical Review";
    public static final String FREQUENCY_HBA1C_CHECK = "HbA1c Check";
    public static final String FREQUENCY_CHO_CHECK = "CHO Check";
    public static final String NOT_ENROLLED = "NOT_ENROLLED";
    public static final String PERIOD = ".";
    public static final String EQUAL_SYMBOL = "=";
    public static final String HYPHEN = "-";
    public static final String RED_RISK = "Red Risk";
    public static final String SP_RISK_LEVEL = "risk-level";
    public static final String SP_CVD_RISK = "cvd-risk";
    public static final String SP_BLOOD_GLUCOSE = "blood-glucose";
    public static final String SP_AVERAGE_SYSTOLIC = "average-systolic";
    public static final String SP_NEXT_BP_ASSESSMENT_DATE = "next-bp-assessment-date";
    public static final String SP_NEXT_MEDICAL_REVIEW_DATE = "next-medical-review-date";
    public static final String SP_NATIONAL_ID = "national-id";
    public static final String SP_VIRTUAL_ID = "virtual-id";
    public static final String SP_NAME_NATIONAL_ID = "name-national-id-search";
    public static final String SP_NATIONAL_ID_VIRTUAL_ID_PHONE = "national-id-virtual-id-phone-search";
    public static final String PARAMQUALIFIER_STRING_CONTAINS = ":contains";
    public static final String PARAM_HAS = "_has";
    public static final String PARAM_COUNT = "_count";
    public static final String PARAM_SEARCH_TOTAL_MODE = "_total";
    public static final String PARAM_LASTUPDATED = "_lastUpdated";
    public static final String SP_NAME_NATIONAL_ID_VIRTUAL_ID_PHONE = "name-national-id-virtual-id-phone-search";
    public static final String SP_PHONE = "phone-search";
    public static final String SP_RELATED_PERSON = "related-person";
    public static final String SP_PATIENT_STATUS = "patient-status";
    public static final String SP_PATIENT_REFERRED = "is-patient-referred";
    public static final String SP_LIST_PATIENT_REGISTRATION = "list-patient-type-registration";
    public static final String SP_ORGANIZATION = "organization";
    public static final String SP_NUTRITION_LIFESTYLE_STATUS = "nutrition-lifestyle-status";
    public static final String SP_REGISTERED_ISSUED = "registered-issued";
    public static final String SP_ACTIVE_AUTHORED_ON = "active-authored-on";
    public static final String FILTER_RED_RISK = "&risk-level=High";
    public static final String FILTER_CVD_RISK_LEVEL = "&_has:Observation:patient:cvd-risk=";
    public static final String FILTER_ASSESSMENT_DATE = "&_has:Appointment:patient:next-bp-assessment-date=";
    public static final String FILTER_MEDICAL_REVIEW_DATE = "&_has:Appointment:patient:next-medical-review-date=";
    public static final String SORT_HIGH_LOW_BG = "&_sort=-blood-glucose,-_lastUpdated";
    public static final String SORT_HIGH_LOW_BP = "&_sort=-average-systolic,-_lastUpdated";
    public static final String SORT_ASSESSMENT_DATE = "&_sort=-next-bp-assessment-date,-_lastUpdated";
    public static final String SORT_MEDICAL_REVIEW_DATE = "&_sort=-next-medical-review-date,-_lastUpdated";
    public static final String SORT_RED_RISK = "&_sort=risk-level,-_lastUpdated";
    public static final String SORT_RED_RISK_PATIENT_LINK = "&_sort=patient.risk-level,-_lastUpdated";
    public static final String INCLUDE_APPOINTMENT_PATIENT = "&_include=Appointment:patient";
    public static final String INCLUDE_OBSERVATION_PATIENT = "&_include=Observation:patient";
    public static final String SP_VALIDITY_PERIOD = "validity-period";
    public static final String ACCURATE = "accurate";
    public static final String CO = "co";
    public static final String NE = "ne";
    public static final String EQ = "eq";
    public static final String COLON = ":";
    public static final String SEARCH_TEXT_FILL_UP = "<<<search-text>>>";
    public static final String ROLE_NUTRITIONIST = "NUTRITIONIST";
    public static final String CLINICIAN_NOTE = "clinicianNote";
    public static final String OTHER_NOTE = "otherNote";
    public static final String NUTRITION_LIFESTYLE = "nutritionLifestyle";
    public static final String LIFESTYLE_ASSESSMENT = "lifestyleAssessment";
    public static final String INVESTIGATION = "investigation";
    public static final String STRING_FORMAT_SPECIFIER = "%s";
    public static final String NOT = "not";
    public static final String INITIAL_REVIEWED = "initialReviewed";

    public static final String CONFIRMED = "confirmed";
    public static final String UNCONFIRMED = "unconfirmed";
    public static final String PROVISIONAL = "provisional";
    public static final String SUBSTANCE_DISORDER = "Substance Disorder";
    public static final String PREGNANCY_TYPE = "Pregnancy";
    public static final String OTHER = "Other";
    public static final String OTHER_LOWER_CASE = "other";
    public static final String HYPERTENSION_DIAGNOSIS_VALUE = "hypertension";
    public static final String DOUBLE_QUOTE = "\"";
    public static final String COUNSELOR = "Counselor";
    public static final String START_DATE = "startDate";
    public static final String END_DATE = "endDate";
    public static final String PATIENT_STATUS_TYPE = "patient-status";
    public static final String PRESCRIPTION_FILLED_DAYS = "prescription-filled-days";
    public static final String NO_SYMPTOMS = "No symptoms";
    public static final String MENTAL_HEALTH_STATUS = "Mental Health Status";
    public static final String OFFLINE_ASSESSMENT = "offline_assessment";
    public static final String OBSERVATION_ID = "ObservationId";

    static {
        FREQUNCY_TYPE_VALUE_MAP.put(FREQUENCY_BP_CHECK, HYPERTENSION_DIAGNOSIS_VALUE);
        FREQUNCY_TYPE_VALUE_MAP.put(FREQUENCY_HBA1C_CHECK, "hba1c");
        FREQUNCY_TYPE_VALUE_MAP.put(FREQUENCY_MEDICAL_REVIEW, MEDICAL_REVIEW);
        FREQUNCY_TYPE_VALUE_MAP.put(FREQUENCY_BG_CHECK, "diabetes");
        FREQUNCY_TYPE_VALUE_MAP.put(FREQUENCY_CHO_CHECK, "prenancyanc");


    }

    public static final List<String> FREQUENCIES = List.of(FREQUENCY_BP_CHECK, FREQUENCY_BG_CHECK, FREQUENCY_HBA1C_CHECK, FREQUENCY_MEDICAL_REVIEW, FREQUENCY_CHO_CHECK);
    public static final String BG_PROVISIONAL_FREQUENCY_NAME = "Physician Approval Pending";
    public static final String MEDICAL_REVIEW_FREQUENCY = "Medical Review Frequency";
    public static final String BP_CHECK_FREQUENCY = "Blood Pressure Check Frequency";
    public static final String BG_CHECK_FREQUENCY = "Blood Glucose Check Frequency";
    public static final String HBA1C_CHECK_FREQUENCY = "HbA1c Check Frequency";
    public static final String CHO_CHECK_FREQUENCY = "CHO Check Frequency";

    public static final String EVERY_ONE_WEEK = "Every 1 week";
    public static final String EVERY_TWO_WEEKS = "Every 2 weeks";
    public static final String EVERY_THREE_WEEKS = "Every 3 weeks";
    public static final String EVERY_ONE_MONTH = "Every 1 month";
    public static final String EVERY_TWO_MONTHS = "Every 2 months";
    public static final String EVERY_THREE_MONTHS = "Every 3 months";
    public static final String TREATMENT_PLAN_DEFAULT = "default";

    public static final String PROVISIONAL_DM = "DM";
    public static final String BG_DEFAULT_FREQUENCY = "Every 1 month";
    public static final String HBA1C_DEFAULT_FREQUENCY = "Every 3 months";
    public static final String BG_DEFAULT_FREQUENCY_PERIOD = "mo";
    public static final Integer BG_DEFAULT_FRQUENCY_DURATION = 1;
    public static final String PATIENT_TREATMENT_PLAN_TITLE = "Patient Treatment plan";
    public static final String HOME = "home";
    public static final String MOBILE = "mobile";
    public static final String TEMP = "temp";

    //Patient status
    public static final String KNOWN_PATIENT = "Known Patient";
    public static final String DIABETES = "Diabetes";
    public static final String HYPERTENSION = "Hypertension";
    public static final String PATIENT_STATUS_FIELD = "Hypertension,Diabetes,Substance Disorder,Mental Health Status";


    public static final Map<String, List<Integer>> PREGNANCY_TREATMENT_PLANS = Map.of(EVERY_ONE_WEEK,
            List.of(15, 16, 17, 18, 19, 23, 24, 25, 26, 27, 29, 30, 31, 35, 36, 37, 38, 39, 40),
            EVERY_TWO_WEEKS, List.of(14, 22, 28, 34),
            EVERY_THREE_WEEKS, List.of(13, 21, 33),
            EVERY_ONE_MONTH, List.of(9, 10, 11, 12, 20, 32),
            EVERY_TWO_MONTHS, List.of(5, 6, 7, 8),
            EVERY_THREE_MONTHS, List.of(4));


    public static final String MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE = "medicalreviewvisit";
    public static final String KEY_ENCOUNTER_REFERENCE = "encounterReference";
    public static final String NCD_MEDICAL_REVIEWED = "medicalReviewed";
    public static final String NCD_MEDICAL_REVIEW_ENCOUNTER_TYPE = "ncdmedicalreview";

    public static final String KNOWN_DIABETES_PATIENT = "known";
    public static final String HIGH = "High";
    public static final String LOW = "Low";
    public static final String MODERATE = "Moderate";
    public static final String HIGHER_MODERATE = "Higher Moderate";
    public static final String BOTH_MODERATE = "Both Moderate";
    public static final String BOTH_HIGHER_MODERATE = "Both Higher Moderate";
    public static final String GLUCOSE_MODERATE = "Glucose Moderate";
    public static final String COLOR_CODE_HIGH = "#8B0000";
    public static final String COLOR_CODE_LOW = "#B1CD77";
    public static final String COLOR_CODE_MODERATE = "#E4CC76";
    public static final String COLOR_CODE_HIGHER_MODERATE = "#E4A476";
    public static final Map<String, String> RISK_COLOR_CODE = Map.of(LOW, COLOR_CODE_LOW, MODERATE, COLOR_CODE_MODERATE,
            HIGHER_MODERATE, COLOR_CODE_HIGHER_MODERATE, HIGH, COLOR_CODE_HIGH);

    public static final String GENDER_MALE = "Male";
    public static final String GENDER_FEMALE = "Female";

    public static final String DIABETES_UNCONTROLLED_OR_POORLY_CONTROLLED = "Diabetes: Uncontrolled/Poorly controlled";
    public static final String DIABETES_WELL_CONTROLLED = "Diabetes: Well-controlled";
    public static final String PRE_DIABETES = "Pre-Diabetes";
    public static final String VITAL_SIGNS = "vitalsigns";


    // threshold limits
    public static final int BP_THRESHOLD_SYSTOLIC = 140;
    public static final int BP_THRESHOLD_DIASTOLIC = 90;
    public static final int FBS_MMOL_L = 7;
    public static final int RBS_MMOL_L = 11;
    public static final int FBS_MG_DL = 126;
    public static final int RBS_MG_DL = 198;

    // units & metrics
    public static final String RBS = "rbs";
    public static final String FBS = "fbs";
    public static final String UNIT = "unit";
    public static final String GLUCOSE_UNIT_MG_DL = "mg/dL";
    public static final String GLUCOSE_UNIT_MMOL_L = "mmol/L";

    public static final String PHQ4 = "PHQ4";
    public static final String PHQ9 = "PHQ9";
    public static final String GAD7 = "GAD7";
    public static final String HIV = "hiv";

    public static final String VALUE = "value";
    public static final String NAME = "name";
    public static final String DISPLAY_VALUE = "displayValue";
    public static final String SIGNATURE = "signature";
    public static final String ASSESSMENT_TYPE_SCREENING = "screening";
    public static final String ASSESSMENT_TYPE_ASSESSMENT = "assessment";
    public static final String ASSESSMENT_TYPE_MEDICAL_REVIEW = "medicalReview";
    public static final String RISK_LEVEL = "riskLevel";
    public static final String PATIENT_TREATMENT_PLAN = "patienttreatementplan";
    public static final String PSYCHOLOGY = "psychology";
    public static final String PSYCHOLOGY_NOTES = "psychology-notes";
    public static final String PSYCHOLOGY_ASSESSMENT = "psychologyAssessment";
    public static final String IS_PATIENT_REFERRED = "is-patient-referred";

    // request type
    public static final String NON_COMMUNITY ="NON_COMMUNITY";
    public static final String DIAGNOSIS_REPORT_QUERY_WITH_ID = "DiagnosticReport?_id=%s&_include=DiagnosticReport:results-interpreter";
    public static final String ROLE_LAB_TECHNICIAN = "LAB_TECHNICIAN";

    public static final String INVESTIGATION_LIST_BY_NAME = "DiagnosticReport?subject=%s&code:text=%s&status=%s&_include=DiagnosticReport:result&_include=DiagnosticReport:results-interpreter&_include=DiagnosticReport:performer&_sort=-issued";
    public static final String HBA1C="HbA1c";
    public static final String LIPID_PROFILE="Lipid Profile";
    public static final String RENAL_FUNCTION_TEST = "Renal Function Test";
    public static final String DATE_FORMAT_YYYY_MM_DD = "yyyy-MM-dd";
    public static final Map<String,Integer> INVESTIGATION_INTENSIFICATION_CRITERIA = Map.of(Constants.HBA1C, 90, Constants.LIPID_PROFILE, 365, Constants.RENAL_FUNCTION_TEST, 365);
    public static final String INVESTIGATION_COUNT_BY_PATIENT = "DiagnosticReport?status=%s&subject=%s&identifier=%s&_summary=count";



    public static final String TODAY_START_DATE = "todayStartDate";
    public static final String TODAY_END_DATE = "todayEndDate";
    public static final String TOMORROW_START_DATE = "tomorrowStartDate";
    public static final String TOMORROW_END_DATE = "tomorrowEndDate";
    public static final String YESTERDAY_START_DATE = "yesterdayStartDate";
    public static final String YESTERDAY_END_DATE = "yesterdayEndDate";

    public static final String ZONED_UTC_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSxxx";
    public static final String TIMEZONE_UTC = "UTC";
    public static final String MEDICAL_REVIEW_START_DATE = "medicalReviewStartDate";
    public static final String MEDICAL_REVIEW_END_DATE = "medicalReviewEndDate";
    public static final String ASSESSMENT_START_DATE = "assessmentStartDate";
    public static final String ASSESSMENT_END_DATE = "assessmentEndDate";
    public static final String LAB_TEST_REFERRED_START_DATE = "labTestReferredStartDate";
    public static final String LAB_TEST_REFERRED_END_DATE = "labTestReferredEndDate";
    public static final String PRESCRIPTION_REFERRED_START_DATE = "prescriptionReferredStartDate";
    public static final String PRESCRIPTION_REFERRED_END_DATE = "prescriptionReferredEndDate";
    public static final String CATEGORY_DM = "DM";
    public static final String PATIENT_STATUS_IDENTIFIER = "patient-status";

    public static final String COUNT_PRESCRIPTION_DAYS_COMPLETED = "prescriptionDaysCompletedCount";
    public static final String COUNT_NON_REVIEWED_TEST = "nonReviewedTestCount";
    public static final String COUNT_NUTRITION_LIFESTYLE_REVIED = "nutritionLifestyleReviewedCount";
    public static final String COUNT_PSYCHOLOGY_REVIEWED = "psychologicalCount";

    public static final String LIFESTYLE_REVIEW_STATUS = "lifeStyleReviewStatus";
    public static final String PSYCHOLOGICAL_REVIEW_STATUS = "psychologicalReviewStatus";
    public static final String FORM_NAME_COUNTRY = "country";
    public static final String UPDATE_VIRTUAL_ID = "update_virtual_id";
    public static final String INPUT_TENANT_ID = "in_tenant_id";
    public static final int MINUS_ONE = -1;


    // other
    public static final String MT_PATIENTS = "my_patients";
    public static final String RELATED_PERSON_QUERY_BASED_ON_VILLAGE = "RelatedPerson?identifier=%s";
    public static final String LAST_UPDATED_GE_PARAM = "&_lastUpdated=ge%s";
    public static final String LAST_UPDATED_LT_PARAM = "&_lastUpdated=lt%s&_sort=-_lastUpdated&_revinclude=Patient:link";

    //Medical review history
    public static final String GET_LATEST_ENCOUNTER_BY_PERIOD = "Encounter?subject=%s&identifier=%s&identifier=%s&_sort=date&_count=999999999";
    public static final String GET_LATEST_ENCOUNTER_BY_IDENTIFIER = "Encounter?subject=%s&identifier=%s&_sort=-date&_count=99999999";
    public static final String GET_ENCOUNTER_BY_PART_OF = "Encounter?part-of=%s&_sort=part-of&status:not=cancelled";
    public static final String OBSERVATION_REV_INCLUDE_ENCOUNTER_IDS = GET_ENCOUNTER_ID + "&_revinclude=Observation:encounter&_count=9999999";
    public static final String REV_INCLUDE_ENCOUNTER_IDS = GET_ENCOUNTER_ID + "&_revinclude=%s:encounter&_revinclude=%s:encounter&_revinclude=%s:encounter&_count=9999999";
    public static final String USER_PRACTIONER = "Practitioner/";
    public static final String RELATED_PERSON_QUERY_ID = "RelatedPerson?_id=%s";
    public static final String PATIENT_IDS = "patientIds";

    public static final String MEDICAL_REVIEW_APPOINTMENT_TYPE = "medicalreview";
    public static final String QUESTIONNAIRE_RESPONSE_ID = "QuestionnaireResponse?_id=%s&identifier=%s";
    public static final String QUESTIONNAIRE_RESPONSE_BY_MEMBER_ID = "QuestionnaireResponse?source=%s&identifier=%s&_sort=-authored&_count=1";



    public static final String OBSERVATION_BLOOD_PRESSURE = "bloodPressure";
    public static final String OBSERVATION_BLOOD_GLUCOSE = "bloodGlucose";
    public static final String OBSERVATION_WEIGHT = "weight";
    public static final String OBSERVATION_HEIGHT = "height";
    public static final String OBSERVATION_BMI = "bmi";
    public static final String OBSERVATION_RISK_LEVEL = "redRiskDetails";
    public static final String OBSERVATION_SUICIDE_SCREENER = "suicideScreener";
    public static final String OBSERVATION_SUBSTANCE_ABUSE = "substanceAbuse";
    public static final String OBSERVATION_REGULAR_SMOKER = "regularSmoker";
    public static final String OBSERVATION_PREGNANCY = "pregnancy";
    public static final String OBSERVATION_MENTAL_HEALTH = "mentalHealth";
    public static final String OBSERVATION_VITAL_SIGNS = "vitalsigns";
    public static final String OBSERVATION_TEMPERATURE = "temperature";
    public static final String OBSERVATION_PSYCHOLOGY_ASSESSMENT = "psychologyAssessment";
    public static final String OBSERVATION_COMPLIANCE = "compliance";
    public static final String OBSERVATION_PSYCHOLOGY_NOTES = "psychologyNotes";
    public static final String OBSERVATION_NUTRITION_LIFESTYLE = "nutritionLifestyle";
    public static final String OBSERVATION_LIFESTYLE = "lifestyle";
    public static final String OBSERVATION_COMPLAINTS = "complaints";
    public static final String OBSERVATION_CLINICAL_NOTES = "clinicalNotes";
    public static final String OBSERVATION_PHYSICAL_EXAMINATION = "physicalExaminations";
    public static final String OBSERVATION_CURRENT_MEDICATION = "currentMedication";
    public static final String OBSERVATION_PATIENT_SYMPTOM = "patientSymptom";
    public static final String WORD_SPACE_REGEX = "([a-z])([A-Z])";
    public static final String ENCOUNTER_TYPE_MENTAL_HEALTH = "mentalhealth";
    public static final String PARAM_SORT_ASC = "_sort:asc";
    public static final String PARAM_SORT_DESC = "_sort:desc";

    private Constants() {

    }
}