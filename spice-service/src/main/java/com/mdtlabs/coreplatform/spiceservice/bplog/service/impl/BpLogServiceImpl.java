package com.mdtlabs.coreplatform.spiceservice.bplog.service.impl;


import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.bplog.service.BpLogService;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.PatientTreatmentPlanService;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.SymptomRepository;

/**
 * <p>
 * This class implements the BpLogService interface and contains actual business
 * logic to perform operations on BpLog entity.
 * </p>
 *
 */
@Service
public class BpLogServiceImpl implements BpLogService {

	private final SymptomRepository symptomRepository;

	private final FhirServiceApiInterface fhirServiceApiInterface;

	private final FollowUpService followUpService;


	@Autowired
	public BpLogServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, SymptomRepository symptomRepository,
                            FollowUpService followUpService) {
		this.fhirServiceApiInterface = fhirServiceApiInterface;
		this.symptomRepository = symptomRepository;
        this.followUpService = followUpService;
	}

	/**
	 * {@inheritDoc}
	 */
	@Transactional
	public BpLogDTO addBpLog(BpLogDTO bpLogDTO) {
		Logger.logInfo("In BpLogServiceImpl, create new bp log information");
		validateBpLogDTO(bpLogDTO);
		bpLogDTO.setType(Constants.MEDICAL_REVIEW);
		AssessmentDTO patientAssessmentDTO = fhirServiceApiInterface.assessmentCreate(CommonUtil.getAuthToken(), CommonUtil.getClient(),
				constructAssessmentDTO(bpLogDTO));
		if (Objects.nonNull(patientAssessmentDTO.getNextBpAssessmentDate())) {
			createCallRegister(patientAssessmentDTO);
		}
		return bpLogDTO;
	}

	/**
	 * Validate the bp log details
	 *
	 * @param bpLogDTO bpLogDTO to validate the details
	 */
	private void validateBpLogDTO(BpLogDTO bpLogDTO) {
		if (Objects.isNull(bpLogDTO.getRelatedPersonFhirId())) {
			throw new DataNotFoundException(3000);
		}
		if (Objects.isNull(bpLogDTO.getAssessmentOrganizationId())) {
			throw new DataNotFoundException(3002);
		}
	}

	/**
	 * Construct the assessment details using bp log details
	 *
	 * @param bpLogDTO bp log details
	 * @return AssessmentDTO
	 */
	private AssessmentDTO constructAssessmentDTO(BpLogDTO bpLogDTO) {
		AssessmentDTO assessmentDTO = new AssessmentDTO();
		assessmentDTO.setMemberReference(bpLogDTO.getRelatedPersonFhirId());
		assessmentDTO.setPatientReference(bpLogDTO.getPatientId());
		assessmentDTO.setAssessmentType(Constants.NON_COMMUNITY);
		assessmentDTO.setType(bpLogDTO.getType());
		assessmentDTO.setAssessmentTakenOn(bpLogDTO.getBpTakenOn());
		assessmentDTO.setAssessmentOrganizationId(bpLogDTO.getAssessmentOrganizationId());
		assessmentDTO.setCvdRiskLevel(bpLogDTO.getCvdRiskLevel());
		assessmentDTO.setCvdRiskScore(bpLogDTO.getCvdRiskScore());
		assessmentDTO.setCvdRiskScoreDisplay(bpLogDTO.getCvdRiskScoreDisplay());
		assessmentDTO.setBpLog(bpLogDTO);
		assessmentDTO.setBioMetrics(bpLogDTO.getBioMetrics());
		assessmentDTO.setEncounter(constructEncounterDetailDTO(bpLogDTO));
		assessmentDTO.setAssessmentType(Constants.NON_COMMUNITY);
		assessmentDTO.setTemperature(bpLogDTO.getTemperature());
		assessmentDTO.setBioData(bpLogDTO.getBioData());
		return assessmentDTO;
	}

	/**
	 * Construct the encounter details using bp log details
	 *
	 * @param bpLogDTO bp log details
	 * @return EncounterDetailsDTO
	 */
	private EncounterDetailsDTO constructEncounterDetailDTO(BpLogDTO bpLogDTO) {
		EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
		encounterDetailsDTO.setProvenance(bpLogDTO.getProvenance());
		return encounterDetailsDTO;
	}

	/**
	 * {@inheritDoc}
	 */
	public PatientBpLogsDTO getPatientBpLogsWithSymptoms(RequestDTO requestData) {
		Logger.logInfo("In BpLogServiceImpl, get patient bp log value with symptoms");
		if (Objects.isNull(requestData.getMemberId())) {
			throw new DataNotFoundException(3000);
		}
		PatientBpLogsDTO patientBpLogsDTO = fhirServiceApiInterface.getBpLogList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestData);
		List<String> symptoms = new ArrayList<>();
		if(Objects.nonNull(patientBpLogsDTO) && Objects.nonNull(patientBpLogsDTO.getLatestBpLog()) &&
				Objects.nonNull(patientBpLogsDTO.getLatestBpLog().getSymptoms())) {
			List<Symptom> bpSymptoms = symptomRepository.findByNameInAndType(patientBpLogsDTO.getLatestBpLog().getSymptoms(), Constants.HYPERTENSION);
			bpSymptoms.forEach(symptom ->
				symptoms.add(symptom.getName()));
			patientBpLogsDTO.getLatestBpLog().setSymptoms(symptoms);
		}
		return patientBpLogsDTO;
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
		callRegister.setNextBPAssessmentDate(assessmentDTO.getNextBpAssessmentDate());
		callRegister.setReferredSiteId(assessmentDTO.getAssessmentOrganizationId());
		followUpService.addCallRegister(callRegister, Boolean.TRUE);
	}
}
