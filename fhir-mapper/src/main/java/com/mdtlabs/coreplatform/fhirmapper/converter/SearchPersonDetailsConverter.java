package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Appointment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Coverage;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriberDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

/**
 * <p>
 * Converts FHIR Observation entity to Search person details DTO.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-23
 */
@Component
public class SearchPersonDetailsConverter {
    private final SpiceConverter spiceConverter;

    private final FhirUtils fhirUtils;

    private final PregnancyConverter pregnancyConverter;

    private RestApiUtil restApiUtil;

    @Autowired
    public SearchPersonDetailsConverter(SpiceConverter spiceConverter, PregnancyConverter pregnancyConverter,
                                        FhirUtils fhirUtils, RestApiUtil restApiUtil) {
        this.spiceConverter = spiceConverter;
        this.pregnancyConverter = pregnancyConverter;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
    }

    /**
     * <p>
     * Set blood pressure log details to search person details DTO using FHIR Observation entity.
     * </p>
     *
     * @param patientDetailsDTO  The patient details DTO
     * @param observation        The FHIR Observation entity.
     *
     */
    private void setBpLogDetails(PatientDetailsDTO patientDetailsDTO,
                                 Observation observation) {
        if (Objects.nonNull(observation)) {
            BpLogDTO bpLogDTO = new BpLogDTO();
            spiceConverter.setBpLogDTO(observation, bpLogDTO);
            patientDetailsDTO.setBpLogDetails(bpLogDTO.getBpLogDetails());
            patientDetailsDTO.setCvdRiskScore(bpLogDTO.getCvdRiskScore());
            patientDetailsDTO.setCvdRiskLevel(bpLogDTO.getCvdRiskLevel());
            patientDetailsDTO.setCvdRiskScoreDisplay(bpLogDTO.getCvdRiskScoreDisplay());
            patientDetailsDTO.setIsHtnDiagnosis(bpLogDTO.getIsBeforeHtnDiagnosis());
            if (Objects.nonNull(bpLogDTO.getAvgSystolic())
                    && Objects.nonNull(bpLogDTO.getAvgDiastolic())) {
                patientDetailsDTO.setAvgSystolic(bpLogDTO.getAvgSystolic());
                patientDetailsDTO.setAvgDiastolic(bpLogDTO.getAvgDiastolic());
                String avgBloodPressure = ((int) bpLogDTO.getAvgSystolic())
                        + Constants.FORWARD_SLASH + ((int) bpLogDTO.getAvgDiastolic());
                patientDetailsDTO.setAvgBloodPressure(avgBloodPressure);
            }
        }
    }

    /**
     * <p>
     * Set blood glucose log details to search person details DTO using FHIR Observation entity.
     * </p>
     *
     * @param patientDetailsDTO  The patient details DTO
     * @param observation        The FHIR Observation entity.
     *
     */
    private void setGlucoseLogDetails(PatientDetailsDTO patientDetailsDTO,
                                      Observation observation) {
        if (Objects.nonNull(observation)) {
            GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
            spiceConverter.setGlucoseLogDTO(observation, glucoseLogDTO);
            patientDetailsDTO.setGlucoseUnit(glucoseLogDTO.getGlucoseUnit());
            patientDetailsDTO.setGlucoseType(glucoseLogDTO.getGlucoseType());
            if (Objects.nonNull(glucoseLogDTO.getGlucoseValue())) {
                patientDetailsDTO.setGlucoseValue(glucoseLogDTO.getGlucoseValue());
                patientDetailsDTO.setGlucoseDateTime(glucoseLogDTO.getGlucoseDateTime());
            }
            patientDetailsDTO.setHba1c(glucoseLogDTO.getHba1c());
            patientDetailsDTO.setHba1cUnit(glucoseLogDTO.getHba1cUnit());
            patientDetailsDTO.setHba1cDateTime(glucoseLogDTO.getHba1cDateTime());
            patientDetailsDTO.setIsDiabetesDiagnosis(glucoseLogDTO.getIsBeforeDiabetesDiagnosis());
            patientDetailsDTO.setLastMealTime(glucoseLogDTO.getLastMealTime());
            patientDetailsDTO.setDiabetes(glucoseLogDTO.getDiabetes());
            patientDetailsDTO.setDiabetesOtherSymptoms(glucoseLogDTO.getDiabetesOtherSymptoms());
        }
    }

    /**
     * <p>
     * Set pregnancy anc details to search person details DTO
     * using FHIR Observation entity.
     * </p>
     *
     * @param patientDetailsDTO  The search patient details DTO
     * @param observation        The FHIR Observation entity.
     *
     */
    private void setPregnancyDetails(PatientDetailsDTO patientDetailsDTO,
                                        Observation observation) {
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();

        if (Objects.nonNull(observation) && Observation.ObservationStatus.PRELIMINARY.equals(observation.getStatus())) {
            pregnancyConverter.convertObservationToPregnancyDetails(pregnancyDetailsDTO, observation);
            patientDetailsDTO.setIsPregnant(pregnancyDetailsDTO.getIsPregnant());
            pregnancyDetailsDTO.setIsDangerSymptoms(patientDetailsDTO.getRedRiskPatient());
            patientDetailsDTO.setPregnancyDetails(pregnancyDetailsDTO);
        }
    }

    /**
     * <p>
     * Checks whether the observation is taken last 24 hours or not
     * </p>
     * @param observation  The FHIR Observation entity
     * @return             True if the observation is taken within 24 hours
     *
     */
    private boolean isEffectiveDateTimeWithinLast24Hours(Observation observation) {
        if (observation.hasEffectiveDateTimeType()) {
            Date effectiveDateTime = observation.getEffectiveDateTimeType().getValue();
            Instant effectiveInstant = effectiveDateTime.toInstant();
            Instant now = Instant.now();
            // Calculate the time 24 hours ago
            Instant twentyFourHoursAgo = now.minusSeconds(3600 * 24L);
            // Check if the effective time is within the last 24 hours
            return effectiveInstant.isAfter(twentyFourHoursAgo);
        }
        return false;
    }

    /**
     * <p>
     * Construct search person details like bp log, glucose log, bio details, bio metrics,
     * pregnancy details and symptoms using patient details entity
     * </p>
     *
     * @param searchPersonDetailsDTO   The search patient details
     * @param type                     The type of patient search
     *
     */
    public PatientDetailsDTO constructSearchPersonDetails(SearchPersonDetailsDTO searchPersonDetailsDTO,
                                                          String type) {
        fhirUtils.initiateCodesMap();
        PatientDetailsDTO patientDetailsDTO = new PatientDetailsDTO();
        BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();
        BioDataDTO bioDataDTO = new BioDataDTO();
        boolean isAssessment = type.equalsIgnoreCase(Constants.ASSESSMENT);
        boolean isDispense = type.equalsIgnoreCase(Constants.DISPENSE);
        spiceConverter.setRegularSmokerDetails(searchPersonDetailsDTO.getRegularSmokerObservation(),
                bioMetricsDTO);
        spiceConverter.setBmiDetails(searchPersonDetailsDTO.getBmiObservation(), bioMetricsDTO);
        spiceConverter.setHeightDetails(searchPersonDetailsDTO.getHeightObservation(), bioMetricsDTO);
        spiceConverter.setWeightDetails(searchPersonDetailsDTO.getWeightObservation(), bioMetricsDTO);

        if (Objects.nonNull(searchPersonDetailsDTO.getRelatedPerson())) {
            spiceConverter.setBioDataDetails(searchPersonDetailsDTO.getRelatedPerson(),
                    bioMetricsDTO, bioDataDTO);
            this.setSearchPersonBioDataDetails(patientDetailsDTO, bioMetricsDTO, bioDataDTO);
            this.setSearchPersonBioMetricsDetails(patientDetailsDTO, bioMetricsDTO);
            if (!Constants.OFFLINE_ASSESSMENT.equals(type)) {
                this.setNextMedicalReviewDate(patientDetailsDTO, searchPersonDetailsDTO.getRelatedPerson());
            }
            patientDetailsDTO.setId(searchPersonDetailsDTO.getRelatedPerson().getIdPart());
            patientDetailsDTO.setPatientId(fhirUtils.getIdFromReference(searchPersonDetailsDTO.getRelatedPerson().getPatient().getReference()));
        }
        setScreenedLandmark(patientDetailsDTO, searchPersonDetailsDTO.getVitalSignsObservation());
        if (Objects.nonNull(searchPersonDetailsDTO.getPatient())) {
            searchPersonDetailsDTO.getPatient().getAddress().forEach(address -> {
                if (!Objects.isNull(address.getUse()) && address.getUse().equals(Address.AddressUse.TEMP)) {
                    patientDetailsDTO.setLandmark(address.getText());
                }
            });
        }

        if (Objects.nonNull(searchPersonDetailsDTO.getPatient())) {
            searchPersonDetailsDTO.getPatient().getIdentifier()
                    .forEach(identifier -> {
                        if (FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL.equals(identifier.getSystem())) {
                            patientDetailsDTO.setProgramId(identifier.getValue());
                        }
                        if (FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID.equals(identifier.getSystem())) {
                            patientDetailsDTO.setNationalId(identifier.getValue());
                        }
                    });
            patientDetailsDTO.setPatientId(searchPersonDetailsDTO.getPatient().getIdPart());
            searchPersonDetailsDTO.getPatient().getIdentifier().stream()
                    .filter(identifier -> FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL
                            .equals(identifier.getSystem()))
                    .map(Identifier::getValue)
                    .findFirst()
                    .ifPresent(patientDetailsDTO::setPatientStatus);
            searchPersonDetailsDTO.getPatient().getIdentifier().stream()
                    .filter(identifier -> FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL
                            .equals(identifier.getSystem()))
                    .map(Identifier::getValue)
                    .findFirst()
                    .ifPresent(patientDetailsDTO::setProgramId);
            setEnrolledDate(searchPersonDetailsDTO.getPatient(), patientDetailsDTO);
            setPatientOccupation(searchPersonDetailsDTO.getPatient(), patientDetailsDTO);
            if (!Constants.OFFLINE_ASSESSMENT.equals(type)) {
                setPatientHealthInsuranceDetails(searchPersonDetailsDTO.getPatient(), patientDetailsDTO);
            }
        } else {
            searchPersonDetailsDTO.getRelatedPerson().getIdentifier().stream()
                    .filter(identifier -> FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL
                            .equals(identifier.getSystem()))
                    .map(Identifier::getValue)
                    .findFirst()
                    .ifPresent(patientDetailsDTO::setPatientStatus);
        }
        if (isDispense) {
            setLastRefillDetails(patientDetailsDTO, searchPersonDetailsDTO.getPatient());
            return patientDetailsDTO;
        }
        this.setSuicideDetails(patientDetailsDTO, searchPersonDetailsDTO.getSuicideScreenerObservation());
        this.setSubstanceDetails(patientDetailsDTO, searchPersonDetailsDTO.getSubstanceAbuseObservation());
        this.setMentalHealthDetails(patientDetailsDTO, searchPersonDetailsDTO.getMentalHealthObservation());
        this.setRedRiskDetails(patientDetailsDTO, searchPersonDetailsDTO.getRedRiskObservation());
        this.setPregnancyDetails(patientDetailsDTO, searchPersonDetailsDTO.getPregnancyObservation());

        if (isAssessment) {
            if (Objects.nonNull(searchPersonDetailsDTO.getBpObservation())
                    && isEffectiveDateTimeWithinLast24Hours(searchPersonDetailsDTO.getBpObservation())) {
                this.setBpLogDetails(patientDetailsDTO, searchPersonDetailsDTO.getBpObservation());
            } else {
                this.setBpLogDetails(patientDetailsDTO, searchPersonDetailsDTO.getBpObservation());
                patientDetailsDTO.setBpLogDetails(null);
                patientDetailsDTO.setIsHtnDiagnosis(null);
                patientDetailsDTO.setAvgSystolic(null);
                patientDetailsDTO.setAvgDiastolic(null);
                patientDetailsDTO.setAvgBloodPressure(null);
            }
            if (Objects.nonNull(searchPersonDetailsDTO.getBgObservation())
                    && isEffectiveDateTimeWithinLast24Hours(searchPersonDetailsDTO.getBgObservation())) {
                this.setGlucoseLogDetails(patientDetailsDTO, searchPersonDetailsDTO.getBgObservation());
            }
        } else {
            this.setGlucoseLogDetails(patientDetailsDTO, searchPersonDetailsDTO.getBgObservation());
            this.setBpLogDetails(patientDetailsDTO, searchPersonDetailsDTO.getBpObservation());
        }
        return patientDetailsDTO;
    }

    /**
     * <p>
     * Set patient next medical review date using fhir appointment
     * </p>
     *
     * @param relatedPerson       The FHIR RelatedPerson entity.
     * @param patientDetailsDTO   The search patient details DTO
     *
     */
    private void setNextMedicalReviewDate(PatientDetailsDTO patientDetailsDTO,
                                          RelatedPerson relatedPerson) {
        String url = String.format(Constants.GET_PATIENT_APPOINTMENT, relatedPerson.getIdPart(), FhirIdentifierConstants.APPOINTMENT_TYPE_SYSTEM_URL, Constants.MEDICAL_REVIEW_APPOINTMENT_TYPE);
        Bundle appointmentBundle = restApiUtil.getBatchRequest(url);
        Appointment appointment = null;

        if (Objects.nonNull(appointmentBundle)
                && !appointmentBundle.getEntry().isEmpty()) {
            appointment = (Appointment) appointmentBundle.getEntry().getFirst().getResource();
        }
        if (Objects.nonNull(appointment)) {
            patientDetailsDTO.setNextMedicalReviewDate(appointment.getStart());
        }
    }

    /**
     * <p>
     * Get and Set patient health insurance details using
     * Fhir Coverage entity
     * </p>
     *
     * @param patient             The FHIR Patient entity.
     * @param patientDetailsDTO   The search patient details DTO
     *
     */
    private void setPatientHealthInsuranceDetails(Patient patient, PatientDetailsDTO patientDetailsDTO) {
        String url = String.format(Constants.GET_PATIENT_COVERAGE, patient.getIdPart());
        Bundle coverageBundle = restApiUtil.getBatchRequest(url);
        Coverage coverage = null;

        if (Objects.nonNull(coverageBundle)
                && !coverageBundle.getEntry().isEmpty()) {
            coverage = (Coverage) coverageBundle.getEntry().getFirst().getResource();
        }
        if (Objects.nonNull(coverage)) {
            setPatientCoverageDetails(coverage, patientDetailsDTO);
        }
    }

    /**
     * <p>
     * Set patient insurance details using FHir Coverage resource
     * </p>
     *
     * @param coverage            The FHIR Coverage entity.
     * @param patientDetailsDTO   The search patient details DTO
     *
     */
    private void setPatientCoverageDetails(Coverage coverage,
                                           PatientDetailsDTO patientDetailsDTO) {
        if (Objects.equals(Coverage.CoverageStatus.ACTIVE, coverage.getStatus())) {
            patientDetailsDTO.setInsuranceStatus(Boolean.TRUE);
            if (Objects.nonNull(coverage.getSubscriberId())) {
                patientDetailsDTO.setInsuranceId(coverage.getSubscriberId());
            }
            if (Objects.nonNull(coverage.getType())
                    && Objects.nonNull(coverage.getType().getText())) {
                String insuranceType = coverage.getType().getText();
                patientDetailsDTO.setInsuranceType(insuranceType);
                if (insuranceType.contains(MetaCodeConstants.OTHER + Constants.HIGHFIN)) {
                    patientDetailsDTO.setInsuranceType(MetaCodeConstants.OTHER);
                    patientDetailsDTO.setOtherInsurance(insuranceType.split(Constants.HIGHFIN)[Constants.ONE]);
                }
            }
        } else {
            patientDetailsDTO.setInsuranceStatus(Boolean.FALSE);
        }
    }

    /**
     * <p>
     * Set patient occupation details using patient extension
     * </p>
     *
     * @param patient             The FHIR Patient entity.
     * @param patientDetailsDTO   The search patient details DTO
     *
     */
    private void setPatientOccupation(Patient patient, PatientDetailsDTO patientDetailsDTO) {
        patient.getExtension().stream()
                .filter(ext -> Constants.EXTENSION_OCCUPATION_URL.equals(ext.getUrl()))
                .map(Extension::getValue)
                .forEach(value -> {
                    if (value instanceof CodeableConcept codeableConcept) {
                        patientDetailsDTO.setOccupation(codeableConcept.getText());
                    }
                });
    }

    /**
     * <p>
     * Set patient enrolled date in patient details using patient extension
     * </p>
     *
     * @param patient             The FHIR Patient entity.
     * @param patientDetailsDTO   The search patient details DTO
     *
     */
    private void setEnrolledDate(Patient patient, PatientDetailsDTO patientDetailsDTO) {
        if (Objects.nonNull(patientDetailsDTO.getProgramId())) {
            patient.getExtension().stream()
                    .filter(ext -> Constants.EXTENSION_ENROLLMENT_URL.equals(ext.getUrl()))
                    .map(Extension::getValue)
                    .forEach(value -> {
                        if (value instanceof DateTimeType dateTimeValue) {
                            patientDetailsDTO.setEnrollmentAt(dateTimeValue.getValueAsString());
                        }
                    });
        }
    }

    /**
     * <p>
     *   Sets patient last refill details in patient details DTO
     * </p>
     *
     * @param patientDetailsDTO  The PatientDetailsDTO entity
     * @param patient            FHIR Patient entity
     */
    private void setLastRefillDetails(PatientDetailsDTO patientDetailsDTO, Patient patient) {
        String latestDispenseUrl = String.format(Constants.GET_MEDICAL_DISPENSE_ENCOUNTER_BY_PATIENT,
                ResourceType.Patient.name().concat(Constants.FORWARD_SLASH).concat(patient.getIdPart()));
        Bundle latestDispenseBundle = restApiUtil.getBatchRequest(latestDispenseUrl);
        PrescriberDetailsDTO prescriberDetailsDTO = null;

        if (Objects.nonNull(latestDispenseBundle) && !latestDispenseBundle.getEntry().isEmpty()) {
            Encounter encounter = latestDispenseBundle.getEntry().stream().filter(entry -> ResourceType.Encounter
                    .equals(entry.getResource().getResourceType()))
                    .map(entry -> (Encounter) entry.getResource()).findFirst().orElse(new Encounter());
            prescriberDetailsDTO = new PrescriberDetailsDTO();
            prescriberDetailsDTO.setLastRefillVisitId(encounter.getIdPart());
            prescriberDetailsDTO.setLastRefillDate(encounter.getPeriod() != null ? encounter.getPeriod().getStart() : null);
        }
        prescriberDetailsDTO = getPrescriberDetails(prescriberDetailsDTO, patient);
        patientDetailsDTO.setPrescribedDetails(prescriberDetailsDTO);
    }

    /**
     * <p>
     *   Get prescriber details of a particular patient
     * </p>
     *
     * @param prescriberDetailsDTO  The PrescriberDetailsDTO entity
     * @param patient               FHIR Patient entity
     *
     * @return The PrescriberDetailsDTO entity
     */
    private PrescriberDetailsDTO getPrescriberDetails(PrescriberDetailsDTO prescriberDetailsDTO, Patient patient) {
        String url = String.format(Constants.GET_LATEST_MEDICATION_REQUEST_PERFORMER_BY_PATIENT,
                patient.getIdPart());
        Bundle responseBundle = restApiUtil.getBatchRequest(url);
        for (Bundle.BundleEntryComponent component: responseBundle.getEntry()) {
            if (component.getResource() instanceof Practitioner practitioner) {
                prescriberDetailsDTO = Objects.isNull(prescriberDetailsDTO)
                        ? new PrescriberDetailsDTO() : prescriberDetailsDTO;
                if (!practitioner.getName().isEmpty()) {
                    prescriberDetailsDTO.setFirstName(practitioner.getName()
                            .getFirst().getGiven().getFirst().getValue());
                    prescriberDetailsDTO.setLastName(practitioner.getName()
                            .getFirst().getFamily());
                }
                practitioner.getTelecom().stream()
                        .filter(contactPoint -> ContactPoint.ContactPointSystem.PHONE.getDisplay()
                                .equals(contactPoint.getSystem().getDisplay()))
                        .map(ContactPoint::getValue)
                        .findFirst()
                        .ifPresent(prescriberDetailsDTO::setPhoneNumber);
            }
        }
        return prescriberDetailsDTO;
    }

    /**
     * <p>
     * Set search person screened landmark using FHIR Observation entity.
     * </p>
     *
     * @param patientDetailsDTO  The search patient details DTO
     * @param observation        The FHIR Observation entity.
     *
     */
    private void setScreenedLandmark(PatientDetailsDTO patientDetailsDTO, Observation observation) {
        if (Objects.nonNull(observation) && !observation.getIdentifier().isEmpty()) {
            observation.getIdentifier().stream()
                    .filter(identifier -> FhirIdentifierConstants.SCREENED_LANDMARK_SYSTEM_URL.equals(identifier.getSystem()))
                    .map(Identifier::getValue)
                    .findFirst()
                    .ifPresent(patientDetailsDTO::setLandmark);
        }
    }

    /**
     * <p>
     * Set search person bio metrics details using FHIR Observation entity.
     * </p>
     *
     * @param patientDetailsDTO  The search patient details DTO
     * @param bioMetricsDTO      The biometrics DTO.
     * @param bioDataDTO         The bio data DTO.
     *
     */
    private void setSearchPersonBioDataDetails(PatientDetailsDTO patientDetailsDTO,
                                               BioMetricsDTO bioMetricsDTO, BioDataDTO bioDataDTO) {

        patientDetailsDTO.setGender(bioMetricsDTO.getGender().toLowerCase());
        patientDetailsDTO.setAge(bioMetricsDTO.getAge());
        patientDetailsDTO.setDateOfBirth(bioMetricsDTO.getDateOfBirth());
        patientDetailsDTO.setFirstName(bioDataDTO.getFirstName());
        patientDetailsDTO.setLastName(bioDataDTO.getLastName());
        if (Objects.nonNull(bioDataDTO.getMiddleName())) {
            patientDetailsDTO.setName(bioDataDTO.getFirstName() + Constants.EMPTY_SPACE +
                    bioDataDTO.getMiddleName() + Constants.EMPTY_SPACE + bioDataDTO.getLastName());
            patientDetailsDTO.setMiddleName(bioDataDTO.getMiddleName());
        } else {
            patientDetailsDTO.setName(bioDataDTO.getFirstName() +
                    Constants.EMPTY_SPACE + bioDataDTO.getLastName());
        }
        patientDetailsDTO.setIdentityType(bioDataDTO.getIdentityType());
        patientDetailsDTO.setIdentityValue(bioDataDTO.getIdentityValue());
        if (Objects.nonNull(bioDataDTO.getVillage().getId())) {
            patientDetailsDTO.setVillageId(bioDataDTO.getVillage().getId().toString());
        }
        if (Objects.nonNull(bioDataDTO.getVillage().getName())) {
            patientDetailsDTO.setVillage(bioDataDTO.getVillage().getName());
        }
        patientDetailsDTO.setPhoneNumber(bioDataDTO.getPhoneNumber());
        patientDetailsDTO.setPhoneNumberCategory(bioDataDTO.getPhoneNumberCategory());
    }

    /**
     * <p>
     * Set search person bio metrics details using FHIR Observation entity.
     * </p>
     *
     * @param patientDetailsResponseDTO  The search person details DTO
     * @param bioMetricsDTO           The biometrics DTO.
     *
     */
    private void setSearchPersonBioMetricsDetails(PatientDetailsDTO patientDetailsResponseDTO,
                                                  BioMetricsDTO bioMetricsDTO) {
        patientDetailsResponseDTO.setBmi(bioMetricsDTO.getBmi());
        patientDetailsResponseDTO.setBmiCategory(bioMetricsDTO.getBmiCategory());
        patientDetailsResponseDTO.setHeight(bioMetricsDTO.getHeight());
        patientDetailsResponseDTO.setIsPregnant(bioMetricsDTO.getIsPregnant());
        patientDetailsResponseDTO.setWeight(bioMetricsDTO.getWeight());
        patientDetailsResponseDTO.setIsRegularSmoker(bioMetricsDTO.getIsRegularSmoker());
    }

    /**
     * <p>
     * Set risk details to search person details dto using FHIR
     * Observation entity.
     * </p>
     *
     * @param patientDetailsDTO  The search patient details DTO
     * @param observation        The FHIR Observation entity.
     *
     */
    private void setMentalHealthDetails(PatientDetailsDTO patientDetailsDTO,
                                Observation observation) {
        if (Objects.nonNull(observation)) {
            Map<String, String> riskDetails = new HashMap<>();
            spiceConverter.setRiskDetails(observation, riskDetails);
            patientDetailsDTO.setPhq4RiskLevel(riskDetails.get(FhirConstants.PHQ4_RISK_LEVEL));
            patientDetailsDTO.setPhq4score(riskDetails.get(FhirConstants.PHQ4_SCORE));
            patientDetailsDTO.setPhq4FirstScore(riskDetails.get(FhirConstants.PHQ4_FIRST_SCORE));
            patientDetailsDTO.setPhq4SecondScore(riskDetails.get(FhirConstants.PHQ4_SECOND_SCORE));
            patientDetailsDTO.setPhq9Score(riskDetails.get(FhirConstants.PHQ9_SCORE));
            patientDetailsDTO.setGad7Score(riskDetails.get(FhirConstants.GAD7_SCORE));
            patientDetailsDTO.setPhq9RiskLevel(riskDetails.get(FhirConstants.PHQ9_RISK_LEVEL));
            patientDetailsDTO.setGad7RiskLevel(riskDetails.get(FhirConstants.GAD7_RISK_LEVEL));
        }
    }

    /**
     * <p>
     * Set patient red risk details using FHIR
     * Observation entity.
     * </p>
     *
     * @param patientDetailsDTO  The search patient details DTO
     * @param observation        The FHIR Observation entity.
     *
     */
    private void setRedRiskDetails(PatientDetailsDTO patientDetailsDTO,
                                        Observation observation) {
        if (Objects.nonNull(observation)) {
            if (Objects.nonNull(observation.getValueStringType())) {
                String riskLevel = observation.getValueStringType().getValue();
                patientDetailsDTO.setRiskLevel(riskLevel);
                if (Objects.equals(Constants.HIGH, riskLevel)) {
                    patientDetailsDTO.setRedRiskPatient(Boolean.TRUE);
                }
            }
            if (!observation.getComponent().isEmpty() &&
                Objects.nonNull(observation.getComponent().getFirst().getValueStringType())) {
                patientDetailsDTO.setRiskMessage(observation.getComponent().getFirst()
                        .getValueStringType().getValue());
            }
        }
        patientDetailsDTO.setRedRiskPatient(Objects.nonNull(patientDetailsDTO.getRedRiskPatient()));
    }

    /**
     * <p>
     * Set substance details to search person details dto using FHIR
     * Observation entity.
     * </p>
     *
     * @param patientDetailsDTO       The search patient details DTO
     * @param observation             The FHIR Observation entity.
     *
     */
    private void setSubstanceDetails(PatientDetailsDTO patientDetailsDTO,
                                     Observation observation) {
        if (Objects.nonNull(observation)) {
            Map<String, String> suicideDetails = new HashMap<>();
            spiceConverter.setSubstanceDetails(observation, suicideDetails);

            if (!observation.getCode().getCoding().isEmpty()) {
                patientDetailsDTO.setCageAid(observation.getValueStringType().getValueAsString());
            }
        }
    }

    /**
     * <p>
     * Set suicide details to search person details dto using FHIR
     * Observation entity.
     * </p>
     *
     * @param patientDetailsResponseDTO  The search person details DTO
     * @param observation             The FHIR Observation entity.
     *
     */
    private void setSuicideDetails(PatientDetailsDTO patientDetailsResponseDTO,
                                   Observation observation) {
        if (Objects.nonNull(observation)) {
            Map<String, String> suicideDetails = new HashMap<>();
            spiceConverter.setSuicideScreenerDetails(observation, suicideDetails);
            String suicidalIdeation = suicideDetails.get(MetaCodeConstants.HAVE_YOU_THOUGHT_OF_ENDING_YOUR_LIFE_OR_NOT_WORTH_LIVING);
            if (!suicideDetails.isEmpty() && Objects.nonNull(suicidalIdeation)
                    && suicidalIdeation.contains(Constants.NO.toLowerCase())) {
                patientDetailsResponseDTO.setSuicidalIdeation(Constants.NO.toLowerCase());
            } else {
                patientDetailsResponseDTO.setSuicidalIdeation(Constants.YES.toLowerCase());
            }
        }
    }

    /**
     * Converts a FHIR RelatedPerson entity to a PatientDetailsDTO.
     *
     * @param relatedPerson The FHIR RelatedPerson entity to convert.
     * @param patientDTO The PatientDetailsDTO to populate with patient details.
     * @return The populated PatientDetailsDTO.
     */
    public PatientDetailsDTO convertToPatientDetails(RelatedPerson relatedPerson, PatientDetailsDTO patientDTO) {
        if (Objects.nonNull(relatedPerson.getName())
                && Objects.nonNull(relatedPerson.getName().getFirst())
                && Objects.nonNull(relatedPerson.getName().getFirst().getGiven())
                && !relatedPerson.getName().getFirst().getGiven().isEmpty()
                && Objects.nonNull(relatedPerson.getName().getFirst().getGiven().getFirst())
                && Objects.nonNull(relatedPerson.getName().getFirst().getGiven().getLast())) {
            patientDTO.setName(StringUtil.concatString(relatedPerson.getName().getFirst().getGiven().getFirst().asStringValue(),
                    Constants.EMPTY_SPACE,
                    relatedPerson.getName().getFirst().getGiven().getLast().asStringValue()));
        }
        patientDTO.setIsActive(relatedPerson.getActive());
        if (Objects.nonNull(relatedPerson.getId())) {
            patientDTO.setId(relatedPerson.getIdPart());
        }
        if (Objects.nonNull(relatedPerson.getGender())) {
            patientDTO.setGender(relatedPerson.getGender().toCode());
        }
        if (Objects.nonNull(relatedPerson.getBirthDate())) {
            BioMetricsDTO bioMetricsDTO = new BioMetricsDTO();
            bioMetricsDTO.setDateOfBirth(relatedPerson.getBirthDate());
            patientDTO.setDateOfBirth(bioMetricsDTO.getDateOfBirth());
            patientDTO.setAge(bioMetricsDTO.getAge());
        }
        if (Objects.nonNull(relatedPerson.getIdentifier())) {
            relatedPerson.getIdentifier().forEach(identifier -> {
                if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)) {
                    patientDTO.setPatientStatus(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.IDENTITY_TYPE_NATIONAL_ID)) {
                    patientDTO.setIdentityType(Constants.NATIONAL_ID);
                    patientDTO.setIdentityValue(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_REFERRAL_STATUS_SYSTEM_URL)) {
                    patientDTO.setIsReferred(Constants.YES.equals(identifier.getValue()));
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VIRTUAL_ID_SYSTEM_URL)) {
                    patientDTO.setProgramId(identifier.getValue());
                } else if (identifier.getSystem().equals(FhirIdentifierConstants.VILLAGE_SYSTEM_URL)) {
                    patientDTO.setVillageId(identifier.getValue());
                }  else if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_RISK_LEVEL_URL)
                        && !Constants.NA.equals(identifier.getValue())) {
                    patientDTO.setRiskLevel(identifier.getValue());
                }
            });
        }
        if (Objects.nonNull(relatedPerson.getAddress())
                && !relatedPerson.getAddress().isEmpty()
                && Objects.nonNull(relatedPerson.getAddress().getFirst())
                && Objects.nonNull(relatedPerson.getAddress().getFirst().getText())) {
            patientDTO.setVillage(relatedPerson.getAddress().getFirst().getText());
        }
        if (Objects.nonNull(relatedPerson.getTelecom())
                && !relatedPerson.getTelecom().isEmpty()
                && Objects.nonNull(relatedPerson.getTelecom().getFirst())
                && Objects.nonNull(relatedPerson.getTelecom().getFirst().getValue())) {
            patientDTO.setPhoneNumber(relatedPerson.getTelecom().getFirst().getValue());
        }
        if (Objects.nonNull(relatedPerson.getPatient())) {
            patientDTO.setPatientId(fhirUtils.getIdFromReference(relatedPerson.getPatient().getReference()));
        }
        return patientDTO;
    }
}
