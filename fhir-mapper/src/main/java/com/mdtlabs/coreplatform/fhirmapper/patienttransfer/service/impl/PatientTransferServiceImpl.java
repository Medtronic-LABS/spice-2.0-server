package com.mdtlabs.coreplatform.fhirmapper.patienttransfer.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patienttransfer.service.PatientTransferService;

/**
 * <p>
 * This service class contain all the business logic and perform all the
 * operation here.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 07, 2024
 */
@Service
public class PatientTransferServiceImpl implements PatientTransferService {

    private final PatientService patientService;

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    public PatientTransferServiceImpl(PatientService patientService, FhirUtils fhirUtils, RestApiUtil restApiUtil) {
        this.patientService = patientService;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> validatePatientTransfer(RequestDTO requestDTO) {
        String patientFhirId = requestDTO.getPatientReference();
        if (Objects.isNull(patientFhirId)) {
            throw new DataNotAcceptableException(1256);
        }
        Bundle bundle = patientService.getPatientDetailsByPatientReference(patientFhirId);
        List<Bundle.BundleEntryComponent> entries = bundle.getEntry();
        Patient patient = null;
        if (entries.isEmpty()) {
            throw new DataNotAcceptableException(1252);
        } else {
            patient = (Patient) bundle.getEntry().getFirst().getResource();
        }
        if (Objects.isNull(patient)) {
            throw new DataNotAcceptableException(1252);
        }
        patient.getIdentifier().stream().forEach(identifier -> {
            if (!Constants.ENROLLED.equals(identifier.getValue()) && Constants.PATIENT_STATUS_IDENTIFIER.equals(identifier.getSystem())) {
                throw new DataNotAcceptableException(16006);
            }
        });
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updatePatientRecords(RequestDTO requestDTO) {
        String patientFhirId = requestDTO.getPatientReference();
        if (Objects.isNull(patientFhirId)) {
            throw new DataNotAcceptableException(1256);
        }
        Bundle patientBundle = patientService.getPatientDetailsByPatientReference(patientFhirId);
        Patient patient = (Patient) patientBundle.getEntry().getFirst().getResource();
        patient.setManagingOrganization(new
                Reference(String.format(FhirConstants.ORGANIZATION_ID,
                requestDTO.getTransferSite())));
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        fhirUtils.setBundleUsingId(patient.getIdPart(),
                Constants.EMPTY_SPACE,
                Bundle.HTTPVerb.PUT,
                patient,
                bundle,
                requestDTO.getProvenance());
        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));

        if (Objects.nonNull(patient.getLink())
                && !patient.getLink().isEmpty()
                && Objects.nonNull(patient.getLink().getFirst())
                && Objects.nonNull(patient.getLink().getFirst().getOther())
                && Objects.nonNull(patient.getLink().getFirst().getOther().getReference())) {
            String memberId = fhirUtils.getIdFromReference(
                    patient.getLink().getFirst().getOther().getReference());
            Bundle responseBundle = restApiUtil.getBatchRequest(String.format(Constants.RELATED_PERSON_QUERY_ID, memberId).concat(Constants.PATIENT_ACTIVE_STATUS));
            RelatedPerson relatedPerson = (RelatedPerson) responseBundle.getEntry().getFirst().getResource();
            Bundle transactionBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
            Map<String, Identifier> identifierMap = new HashMap<>();
            List<Identifier> identifiers = relatedPerson.getIdentifier();
            identifiers.forEach(identifier -> identifierMap.put(identifier.getSystem(), identifier));
            if (Objects.nonNull(requestDTO.getTransferSite())) {
                if (identifierMap.containsKey(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL)) {
                    identifierMap.get(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL).setValue(requestDTO.getTransferSite());
                } else {
                    identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL)
                            .setValue(requestDTO.getTransferSite()));
                }
            }
            relatedPerson.setIdentifier(identifiers);
            fhirUtils.setBundleUsingId(memberId,
                    Constants.EMPTY_SPACE,
                    Bundle.HTTPVerb.PUT,
                    relatedPerson,
                    transactionBundle,
                    requestDTO.getProvenance());
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(transactionBundle));
        } else {
            throw new DataNotAcceptableException(2004);
        }
    }
}
