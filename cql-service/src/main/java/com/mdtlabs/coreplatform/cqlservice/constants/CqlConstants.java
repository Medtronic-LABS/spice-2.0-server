package com.mdtlabs.coreplatform.cqlservice.constants;

import java.util.HashSet;
import java.util.Set;

/**
 * <p>
 * The CqlConstants contains the constants that required for the cql module.
 * </p>
 *
 * @author Vishwaeaswarean M created on May 20, 2024
 */
public class CqlConstants {

    public static final String PATIENT_VITALS = "PatientVitals";

    private CqlConstants() {
    }

    public static final String SIGNS = "Signs";

    public static final String PATIENT = "Patient";

    public static final String CQL_EXTENSION = ".cql";

    public static final String ENCOUNTER = "encounter";

    public static final String ANC_CONTACT = "Should Proceed with ANC contact";

    public static final String ANC_REFERRAL = "Should Proceed with ANC contact OR Referral";

    public static final String ANC_FIRST_CONTACT_DATE = "ANC First Contact Date";

    public static final String ANC_SECOND_CONTACT_DATE = "ANC Second Contact Date";

    public static final String ANC_THIRD_CONTACT_DATE = "ANC Third Contact Date";

    public static final String ANC_FOURTH_CONTACT_DATE = "ANC Fourth Contact Date";

    public static final String ANC_FIFTH_CONTACT_DATE = "ANC Fifth Contact Date";

    public static final String ANC_SIXTH_CONTACT_DATE = "ANC Sixth Contact Date";

    public static final String ANC_SEVENTH_CONTACT_DATE = "ANC Seventh Contact Date";

    public static final String ANC_EIGHTH_CONTACT_DATE = "ANC Eighth Contact Date";

    public static final String ANC_ANTICIPATED_DELIVERY_DATE = "ANC Anticipated Delivery Date";

    public static final String GESTATIONAL_AGE = "Gestational age (GA) based on LMP";

    public static final String ESTIMATED_DUE_DATE = "Estimated due date (EDD) based on last menstrual period (LMP)";

    public static final String ESTIMATED_DATE_OF_CONCEPTION = "Estimated Date of Conception";

    public static final String ANC_FIRST_VISIT_DATE = "ANC First Visit Date";

    public static final String ANC_SECOND_VISIT_DATE = "ANC Second Visit Date";

    public static final String ANC_THIRD_VISIT_DATE = "ANC Third Visit Date";

    public static final String ANC_FOURTH_VISIT_DATE = "ANC Fourth Visit Date";

    public static final String ANC_FIFTH_VISIT_DATE = "ANC Fifth Visit Date";

    public static final String ANC_SIXTH_VISIT_DATE = "ANC Sixth Visit Date";

    public static final String ANC_SEVENTH_VISIT_DATE = "ANC Seventh Visit Date";

    public static final String ANC_EIGHTH_VISIT_DATE = "ANC Eighth Visit Date";

    public static final String ANC_GESTATIONAL_AGE_GA = "Gestational age (GA)";

    public static final String ANC_ESTIMATED_DELIVERY_DATE = "Estimated Delivery Date";

    public static final String ANC_ESTIMATED_DATE_OF_CONCEPTION = "Estimated Date of Conception";

    public static final String FHIR_HISTORY_ID_REGEX = "/_history";

    public static final String HL7_URL = "http://hl7.org/fhir";

    public static final String PATIENT_IDENTIFIER = "patient-id";

    public static final String VILLAGE_IDENTIFIER = "village";

    public static final String PATIENT_FHIR_ID = "patient-fhir-id";

    public static final String MEMBER_IDENTIFIER = "member-id";

    public static final String ANC_DT_01 = "ANCDT01";

    public static final String ANC_S_01 = "ANCS01";

    public static final String ENCOUNTER_SLASH = "Encounter/";

    public static final String EVERYTHING_URL = "/$everything";

    public static final Set<String> ANC_CONTACT_DATA = new HashSet<>(Set.of(PATIENT, ANC_CONTACT, ANC_REFERRAL,
            ANC_FIRST_CONTACT_DATE, ANC_SECOND_CONTACT_DATE, ANC_THIRD_CONTACT_DATE, ANC_FOURTH_CONTACT_DATE,
            ANC_FIFTH_CONTACT_DATE, ANC_SIXTH_CONTACT_DATE, ANC_SEVENTH_CONTACT_DATE, ANC_EIGHTH_CONTACT_DATE,
            ANC_ANTICIPATED_DELIVERY_DATE, GESTATIONAL_AGE, ESTIMATED_DATE_OF_CONCEPTION, ESTIMATED_DUE_DATE));

}
