package com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.impl;

import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

/**
 * <p>
 * This class is a service class to perform operation on Patient visit.
 * </p>
 *
 * @author Karthick M created on Aug 19, 2024
 */
@Service
public class PatientVisitServiceImpl implements PatientVisitService {

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final EncounterConverter encounterConverter;

    @Autowired
    public PatientVisitServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil, EncounterConverter encounterConverter) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.encounterConverter = encounterConverter;
    }

    private static final String VISIT_ENCOUNTER = "Encounter?participant=RelatedPerson/%s&identifier=%s|%s&date=ge%s&date=le%s";
    private static final String GET_ENCOUNTER_BY_ID = "Encounter?_id=%s";
    private static final String IS_INITIAL_REVIEWED = "Encounter?participant=RelatedPerson/%s&identifier=%s|%s";


    /**
     * {@inheritDoc}
     */
    public Map<String, Object> createPatientVisit(PatientVisitDTO patientVisit) {

        Bundle bundle = restApiUtil.getBatchRequest(String.format(VISIT_ENCOUNTER, patientVisit.getMemberReference(),
                FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL, Constants.MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE, DateUtil.getStartOfDay().toInstant().toString(), DateUtil.getEndOfDay().toInstant().toString()));

        if (bundle.getEntry().isEmpty()) {
            Bundle transcationBundle = new Bundle().setType(BundleType.TRANSACTION);
            String uuid = fhirUtils.getUniqueId();
            String id = StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                    Constants.FORWARD_SLASH,
                    Constants.FHIR_BASE_URL,
                    uuid);

            Encounter encounter = encounterConverter.createEncounter(patientVisit.getPatientReference(), patientVisit.getMemberReference(),
                    FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL, Constants.MEDICAL_REVIEW_VISIT_ENCOUNTER_TYPE, patientVisit.getProvenance());

            fhirUtils.setBundle(id,
                    StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                    Bundle.HTTPVerb.POST,
                    encounter,
                    transcationBundle,
                    patientVisit.getProvenance());
            ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(transcationBundle));
            Map<String, List<String>> fhirResponse = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
            return Map.of(Constants.KEY_ENCOUNTER_REFERENCE, fhirResponse.get(String.valueOf(ResourceType.Encounter)).getFirst(),
                    Constants.INITIAL_REVIEWED, getPatientMedicalReviewStatus(patientVisit.getMemberReference()));

        } else {
            Encounter encounter = (Encounter) bundle.getEntry().getFirst().getResource();
            return Map.of(Constants.KEY_ENCOUNTER_REFERENCE, encounter.getIdPart(),
                    Constants.INITIAL_REVIEWED, getPatientMedicalReviewStatus(patientVisit.getMemberReference()));
        }
    }

    /**
     * {@inheritDoc}
     */
    public Encounter updatePatientVisitStatus(String encounterId, boolean isPrescription, boolean isMedicalReview, boolean isInvestication, String patientReference) {
        Bundle bundle = restApiUtil.getBatchRequest(String.format(GET_ENCOUNTER_BY_ID, encounterId));
        Encounter encounter = null;
        if (!bundle.isEmpty()) {
            encounter = (Encounter) bundle.getEntry().getFirst().getResource();

            if (isPrescription && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL))) {
                encounter.addIdentifier().setSystem(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL).setValue(Constants.PRESCRIBED);
            }
            if (isInvestication && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.INVESTIGATION_STATUS_SYSTEM_URL))) {
                encounter.addIdentifier().setSystem(FhirIdentifierConstants.INVESTIGATION_STATUS_SYSTEM_URL).setValue(Constants.INVESTIGATED);
            }
            if (isMedicalReview && encounter.getIdentifier().stream().noneMatch(identifier -> identifier.getSystem().equals(FhirIdentifierConstants.NCD_MEDICAL_REVIEW_STATUS_SYSTEM_URL))) {
                encounter.addIdentifier().setSystem(FhirIdentifierConstants.NCD_MEDICAL_REVIEW_STATUS_SYSTEM_URL).setValue(Constants.NCD_MEDICAL_REVIEWED);
            }
            encounter.setSubject(fhirUtils.getReferenceUrl(ResourceType.Patient, patientReference));
        }
        return encounter;
    }

    /**
     * {@inheritDoc}
     */
    public boolean getPatientMedicalReviewStatus(String memberReference) {
        Bundle bundle = restApiUtil.getBatchRequest(String.format(IS_INITIAL_REVIEWED, memberReference, FhirIdentifierConstants.NCD_MEDICAL_REVIEW_STATUS_SYSTEM_URL, Constants.NCD_MEDICAL_REVIEWED));
        return !bundle.getEntry().isEmpty();
    }
}
