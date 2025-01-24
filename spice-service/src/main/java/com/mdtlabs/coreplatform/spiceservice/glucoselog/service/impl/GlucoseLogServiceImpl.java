package com.mdtlabs.coreplatform.spiceservice.glucoselog.service.impl;


import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.glucoselog.service.GlucoseLogService;
import com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.PatientTreatmentPlanService;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.SymptomRepository;

/**
 * This class implements the GlucoseLogService interface and contains actual
 * business logic to perform operations on GlucoseLog entity.
 *
 * @since  Aug 28, 2024
 * @author Tamilmani
 */
@Service
public class GlucoseLogServiceImpl implements GlucoseLogService {

    private final PatientTreatmentPlanService patientTreatmentPlanService;

    private final SymptomRepository symptomRepository;

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final FollowUpService followUpService;

    @Autowired
    public GlucoseLogServiceImpl(PatientTreatmentPlanService patientTreatmentPlanService,
                                 FhirServiceApiInterface fhirServiceApiInterface, SymptomRepository symptomRepository,
                                 FollowUpService followUpService) {
        this.patientTreatmentPlanService = patientTreatmentPlanService;
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.symptomRepository = symptomRepository;
        this.followUpService = followUpService;
    }

    /**
     * {@inheritDoc}
     */
    @Transactional
	public GlucoseLogDTO addGlucoseLog(GlucoseLogDTO glucoseLogDTO) {
        Logger.logInfo("In GlucoseLogServiceImpl, create new bg log information");
        AssessmentDTO patientAssessmentDTO = fhirServiceApiInterface.assessmentCreate(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                constructAssessmentDTO(glucoseLogDTO));
        // TODO nextBpAssessmentDate calculation based on the treatment plan
        if (Objects.nonNull(patientAssessmentDTO.getNextBgAssessmentDate())) {
            createCallRegister(patientAssessmentDTO);
        }
		return glucoseLogDTO;
	}

    /**
     * Construct the assessment details using bp log details
     *
     * @param glucoseLogDTO bg log details
     * @return AssessmentDTO
     */
    private AssessmentDTO constructAssessmentDTO(GlucoseLogDTO glucoseLogDTO) {
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setMemberReference(glucoseLogDTO.getRelatedPersonFhirId());
        assessmentDTO.setAssessmentType(Constants.NON_COMMUNITY);
        assessmentDTO.setType(glucoseLogDTO.getType());
        assessmentDTO.setPatientReference(glucoseLogDTO.getPatientId());
        assessmentDTO.setAssessmentTakenOn(glucoseLogDTO.getBgTakenOn());
        assessmentDTO.setAssessmentOrganizationId(glucoseLogDTO.getAssessmentOrganizationId());
        assessmentDTO.setGlucoseLog(glucoseLogDTO);
        assessmentDTO.setEncounter(constructEncounterDetailDTO(glucoseLogDTO));
        assessmentDTO.setAssessmentType(Constants.NON_COMMUNITY);
        assessmentDTO.setBioData(glucoseLogDTO.getBioData());
        assessmentDTO.setBioMetrics(glucoseLogDTO.getBioMetrics());
        return assessmentDTO;
    }


    /**
     * Construct the encounter details using bp log details
     *
     * @param glucoseLogDTO bp log details
     * @return EncounterDetailsDTO
     */
    private EncounterDetailsDTO constructEncounterDetailDTO(GlucoseLogDTO glucoseLogDTO) {
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setProvenance(glucoseLogDTO.getProvenance());
        return encounterDetailsDTO;
    }

    /**
     * {@inheritDoc}
     */
    private void validateGlucoseLog(GlucoseLogDTO glucoseLogDTO) {
        if (Objects.isNull(glucoseLogDTO)) {
            throw new BadRequestException(1000);
        }
        if (Objects.isNull(glucoseLogDTO.getGlucoseValue()) || Objects.isNull(glucoseLogDTO.getGlucoseType())
                || Objects.isNull(glucoseLogDTO.getGlucoseDateTime()) || Objects.isNull(glucoseLogDTO.getGlucoseUnit())
                || Objects.isNull(glucoseLogDTO.getLastMealTime())) {
            throw new BadRequestException(7005);
        }
    }

    /**
     * {@inheritDoc}
     */
    public PatientGlucoseLogDTO getPatientGlucoseLogsWithSymptoms(RequestDTO requestData) {
        Logger.logInfo("In GlucoseLogServiceImpl, get patient bg log values with symptoms");
        if (Objects.isNull(requestData.getMemberId())) {
            throw new DataNotFoundException(3000);
        }
        PatientGlucoseLogDTO patientGlucoseLogDTO = fhirServiceApiInterface.getGlucoseList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestData);
        List<String> symptoms = new ArrayList<>();
        if (Objects.nonNull(patientGlucoseLogDTO.getLatestGlucoseLog()) && Objects.nonNull(patientGlucoseLogDTO.getLatestGlucoseLog().getSymptoms())) {
            List<Symptom> bgSymptoms = symptomRepository.findByNameInAndType(patientGlucoseLogDTO.getLatestGlucoseLog().getSymptoms(), Constants.DIABETES);
            bgSymptoms.forEach(symptom -> {
                symptoms.add(symptom.getName());
            });
            patientGlucoseLogDTO.getLatestGlucoseLog().setSymptoms(symptoms);;
        }
        return patientGlucoseLogDTO;
    }

    /**
     * <p>
     * This function Creates and updates patients registers for the patient
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} request that contains the patient related details
     */
    private void createCallRegister(AssessmentDTO assessmentDTO) {
        CallRegister callRegister = new CallRegister();
        callRegister.setMemberId(assessmentDTO.getMemberReference());
        callRegister.setPatientId(Objects.nonNull(assessmentDTO.getPatientId())
                ? assessmentDTO.getPatientId() : assessmentDTO.getPatientReference());
        callRegister.setIsInitiated(Boolean.FALSE);
        callRegister.setType(AppointmentType.ASSESSMENT);
        callRegister.setIsWrongNumber(Boolean.FALSE);
        callRegister.setVillageId(assessmentDTO.getVillageId());
        callRegister.setNextBGAssessmentTime(assessmentDTO.getNextBgAssessmentDate());
        callRegister.setReferredSiteId(assessmentDTO.getAssessmentOrganizationId());
        followUpService.addCallRegister(callRegister, Boolean.TRUE);
    }
}
