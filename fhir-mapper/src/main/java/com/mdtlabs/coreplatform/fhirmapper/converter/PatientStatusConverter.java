package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.StringType;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

/**
 * <p>
 *      This class used to convert the patient condition resource.
 * </p>
 *
 * @author - Gopinath
 */
@Component
public class PatientStatusConverter {

    private final FhirUtils fhirUtils;

    public PatientStatusConverter(FhirUtils fhirUtils) {
        this.fhirUtils = fhirUtils;
    }

    /**
     * <p>
     *     Used to create the diabetes patient status in condition resource.
     * <p/>
     *
     * @param patientStatusDto - Patient status details
     * @param condition - Patient condition resource
     * @return - {@link Condition} - Saved patient condition details.
     */
    public Condition createDiabetesStatus(PatientStatusDTO patientStatusDto, Condition condition) {
        if (Objects.nonNull(patientStatusDto.getNcdPatientStatus()) && Objects.nonNull(patientStatusDto.getNcdPatientStatus().getDiabetesStatus())) {
            condition = Objects.nonNull(condition) ? condition : new Condition();
            condition.setVerificationStatus(fhirUtils.setCodes(patientStatusDto.getNcdPatientStatus().getDiabetesStatus()));
            condition.setOnset(new StringType(patientStatusDto.getNcdPatientStatus().getDiabetesYearOfDiagnosis()));
            if (patientStatusDto.getNcdPatientStatus().getDiabetesStatus().equals(Constants.KNOWN_PATIENT)) {
                condition.setSeverity(fhirUtils.setCodes(patientStatusDto.getNcdPatientStatus().getDiabetesControlledType()));    
            }
            if (Objects.nonNull(patientStatusDto.getNcdPatientStatus().getDiabetesDiagnosis())) {
                condition.setCode(fhirUtils.setCodes(patientStatusDto.getNcdPatientStatus().getDiabetesDiagnosis()));
            }
            condition.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_DIAGNOSIS_SYSTEM_URL).setValue(Constants.DIABETES);
        }
        return condition;
    }

    /**
     * <p>
     *     Used to create the hypertension patient status in condition resource.
     * <p/>
     *
     * @param patientStatusDto - Patient status details
     * @param condition - Patient condition resource
     * @return - {@link Condition} - Saved patient condition details.
     */
    public Condition createHypertensionStatus(PatientStatusDTO patientStatusDto, Condition condition) {
        if (Objects.nonNull(patientStatusDto.getNcdPatientStatus()) && Objects.nonNull(patientStatusDto.getNcdPatientStatus().getHypertensionStatus())) {
            condition = Objects.nonNull(condition) ? condition : new Condition();
            condition.setVerificationStatus(fhirUtils.setCodes(patientStatusDto.getNcdPatientStatus().getHypertensionStatus()));
            condition.setOnset(new StringType(patientStatusDto.getNcdPatientStatus().getHypertensionYearOfDiagnosis()));
            condition.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_DIAGNOSIS_SYSTEM_URL).setValue(Constants.HYPERTENSION);
            condition.setCode(fhirUtils.setCodes(Constants.HYPERTENSION));
        }
        return condition;
    }

    /**
     * <p>
     *     Used set the patient reference in condition resource.
     * <p/>
     *
     * @param condition - Patient condition resource
     * @param patientId - Reference id for patient
     */
    public void setReference(Condition condition, String patientId, String patientVisitId, String memberReference) {
        if (Objects.nonNull(patientId)) {
            condition.setSubject(new Reference(
                    String.format(FhirConstants.PATIENT_ID, patientId)));
        } else {
            condition.setSubject(new Reference(FhirConstants.PATIENT_IDENTIFIER_URL));
        }
        if (Objects.nonNull(memberReference)) {
            condition.setAsserter(new Reference(
                    String.format(FhirConstants.RELATED_PERSON_ID, memberReference)));
        }
        if (Objects.nonNull(patientVisitId)) {
            condition.setEncounter(new Reference(
                    String.format(FhirConstants.ENCOUNTER_ID, patientVisitId)));
        }
    }

    /**
     * <p>
     * The function `createConfirmedNCDStatus` creates a Condition object with specific codes based on the
     * patient's NCD status.
     * </p>
     *
     * @param patientStatusDto {@link PatientStatusDTO} It contains information about the patient's status, specifically related
     *                         to non-communicable diseases (NCDs) such as diabetes and hypertension.
     * @param condition        {@link Condition} It represents a existing condition.
     * @return {@link Condition} returns the updated condition resource.
     */
    public Condition createConfirmedNCDStatus(PatientStatusDTO patientStatusDto, Condition condition) {
        condition = Objects.nonNull(condition) ? condition : new Condition();
        condition.setVerificationStatus(fhirUtils.setCodes(Constants.CONFIRMED));
        String diabetesType = patientStatusDto.getNcdPatientStatus().getDiabetesDiagnosis();

        List<CodeableConcept> codeableConcepts = new ArrayList<>();

        if (patientStatusDto.getNcdPatientStatus().getDiabetesStatus().equals(Constants.KNOWN_PATIENT)) {
            codeableConcepts.add(fhirUtils.createCodeableConcept(diabetesType));
        }
        if (patientStatusDto.getNcdPatientStatus().getHypertensionStatus().equals(Constants.KNOWN_PATIENT)) {
            codeableConcepts.add(fhirUtils.createCodeableConcept(Constants.HYPERTENSION_DIAGNOSIS_VALUE));
        }
        condition.setCategory(codeableConcepts);

        if (!condition.hasIdentifier()) {
            condition.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_DIAGNOSIS_NCD_IDENTIFIER_SYSTEM_URL).setValue(Constants.NCD);
        }
        return condition;
    }

    /**
     * <p>
     * This function updates a Condition object with confirmed diagnosis information based on the input parameters.
     * </p>
     *
     * @param confirmDiagnosisDTO {@link ConfirmDiagnosisDTO} It contains information related to confirming a diagnosis.
     * @param condition           {@link Condition} It represents a medical condition object that needs to be updated with confirmed diagnosis information.
     * @param type                {@link String} It is used to specify the type of diagnosis being confirmed.
     * @param diagnoses           {@link List} This contains the list of diagnoses to be added to the `Condition` object.
     * @return {@link Condition} It returns a `Condition` object.
     */
    public Condition updateConfirmedDiagnosis(ConfirmDiagnosisDTO confirmDiagnosisDTO, Condition condition, String type, List<String> diagnoses) {
        condition = Objects.nonNull(condition) ? condition : new Condition();
        condition.setVerificationStatus(fhirUtils.setCodes(Constants.CONFIRMED));
        List<CodeableConcept> codeableConcepts = new ArrayList<>();
        if (Objects.nonNull(diagnoses)) {
            diagnoses.forEach(
                    diagnosis -> codeableConcepts.add(fhirUtils.createCodeableConcept(diagnosis))
            );
        }
        condition.setCategory(codeableConcepts);

        if (!condition.hasIdentifier()) {
            condition.addIdentifier().setSystem(FhirIdentifierConstants.CONFIRM_DIAGNOSIS_IDENTIFIER_MAP.get(type))
                    .setValue(FhirConstants.CONFIRM_DIAGNOSIS_TYPE_MAP.get(type));
        }

        if (type.equals(Constants.OTHER) && Objects.nonNull(confirmDiagnosisDTO.getDiagnosisNotes())) {
            Annotation note = new Annotation()
                    .setText(confirmDiagnosisDTO.getDiagnosisNotes())
                    .setTime(new Date());

            if (condition.hasNote()) {
                condition.getNote().getFirst().setText(note.getText()).setTime(note.getTime());
            } else {
                condition.addNote(note);
            }
        } else if (type.equals(Constants.OTHER) && Objects.isNull(confirmDiagnosisDTO.getDiagnosisNotes())) {
            condition.setNote(new ArrayList<>());
        }
        return condition;
    }

    /**
     * <p>
     * The function  creates a Condition object with specific codes based on the
     * patient's Mental health status.
     * </p>
     *
     * @param mentalHealthStatus {@link MentalHealthStatus} It contains information about the patient's status
     * @param condition        {@link Condition} It represents a existing condition.
     * @param value the identifier value is given
     */
    public void setMentalHealthStatus(Condition condition, MentalHealthStatus mentalHealthStatus, String value) {
        condition.setVerificationStatus(fhirUtils.setCodes(mentalHealthStatus.getStatus()));
        condition.setOnset(new StringType(mentalHealthStatus.getYearOfDiagnosis()));

        if (mentalHealthStatus.getStatus().equals(Constants.KNOWN_PATIENT) && Objects.nonNull(mentalHealthStatus.getMentalHealthDisorder())) {
            List<CodeableConcept> codeableConcepts = new ArrayList<>();
            for (String disorder : mentalHealthStatus.getMentalHealthDisorder()) {
                codeableConcepts.add(fhirUtils.createCodeableConcept(disorder));
            }
            condition.setCategory(codeableConcepts);
        }

        if (Objects.equals(Constants.MENTAL_HEALTH_STATUS, value)) {
            condition.setCode(fhirUtils.createCodeableConcept(MetaCodeConstants.MENTAL_HEALTH_KEY));
        } else {
            condition.setCode(fhirUtils.createCodeableConcept(MetaCodeConstants.SUBSTANCE_ABUSE_KEY));
        }

        if (Objects.nonNull(mentalHealthStatus.getComments())) {
            Annotation note = new Annotation()
                    .setText(mentalHealthStatus.getComments())
                    .setTime(new Date());

            if (condition.hasNote()) {
                condition.getNote().getFirst().setText(note.getText()).setTime(note.getTime());
            } else {
                condition.addNote(note);
            }
        }
        condition.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_DIAGNOSIS_SYSTEM_URL).setValue(value);
    }

    /**
     * <p>
     * The function  creates a Condition object with specific codes based on the
     * patient's Mental health status.
     * </p>
     *
     * @param patientStatusDto {@link PatientStatusDTO} It contains information about the patient's status
     * @param condition        {@link Condition} It represents a existing condition.
     * @return {@link Condition} returns the updated condition resource.
     */
    public Condition createConfirmedMentalHealthStatus(PatientStatusDTO patientStatusDto, Condition condition) {
        condition = Objects.nonNull(condition) ? condition : new Condition();
        condition.setVerificationStatus(fhirUtils.setCodes(Constants.CONFIRMED));
        List<CodeableConcept> codeableConcepts = new ArrayList<>();

        if (patientStatusDto.getMentalHealthStatus().getStatus().equals(Constants.KNOWN_PATIENT)) {
            for (String disorder : patientStatusDto.getMentalHealthStatus().getMentalHealthDisorder()) {
                codeableConcepts.add(fhirUtils.createCodeableConcept(disorder));
            }
        }
        if (patientStatusDto.getSubstanceUseStatus().getStatus().equals(Constants.KNOWN_PATIENT)) {
            for (String disorder : patientStatusDto.getSubstanceUseStatus().getMentalHealthDisorder()) {
                codeableConcepts.add(fhirUtils.createCodeableConcept(disorder));
            }
        }
        condition.setCategory(codeableConcepts);

        if (!condition.hasIdentifier()) {
            condition.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_DIAGNOSIS_MENTAL_HEALTH_IDENTIFIER_URL).setValue(Constants.MENTAL_HEALTH);
        }
        return condition;
    }
}
