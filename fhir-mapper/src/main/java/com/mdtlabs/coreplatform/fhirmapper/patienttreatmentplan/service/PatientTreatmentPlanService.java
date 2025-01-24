package com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service;

import java.util.Date;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CarePlan;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanResponseDTO;

/**
 * <p>
 * This interface is a service to perform operation on Patient treatmentplan.
 * </p>
 *
 * @author Karthick M created on Aug 14, 2024
 */
public interface PatientTreatmentPlanService {
    
    /**
	 * This function Creates and updates patients treatment plan details.
	 * 
	 * @param treatmentplan request
	 * @return TreatmentPlanResponseDTO 
	 */
    TreatmentPlanResponseDTO createProvisionalPlan(TreatmentPlanDTO treatmentplan, Bundle bundle);

    /**
	 * This method is used to get patient treatment plan details.
	 *
	 * @param RequestDTO - request
	 * @return PatientTreatmentPlan - entity
	 */
	TreatmentPlanResponseDTO getPatientTreatmentPlanDetails(RequestDTO request);

    /**
	 * Updates the patient treatment plan data.
	 *
	 * @param patientTreatmentPlan - entity
	 * @return boolean - true or false
	 */
	void updateTreatmentPlanData(TreatmentPlanDTO treatmentPlan);

	/**
	 * <p>
	 * Creates or updates an appointment for a patient.
	 * </p>
	 *
	 * @param assessmentType The type of assessment being conducted.
	 * @param nextVisitDate The date of the next visit.
	 * @param memberRef The reference to the member.
	 * @param patientRef The reference to the patient.
	 * @param transactionBundle The transaction bundle containing the appointment details.
	 * @param provenance The provenance information for the appointment.
	 */
	void createOrUpdateAppointment(String assessmentType, Date nextVisitDate, String memberRef, String patientRef,
			Bundle transactionBundle, ProvenanceDTO provenance, boolean isProvisional);


	/**
	 * Gets Care plan resource for patient.
	 *  
	 * @param memeberReference
	 * @return CarePlan
	 */
	public CarePlan getCarePlanForPatient(String memeberReference);

	/**
	 * Updates next visit date for patient.
	 * 
	 * @param memberReference
	 * @param patientReference
	 * @param appointmentType
	 * @param carePlan
	 * @param provenance
	 */
	public Date updateNextVisitDateForPatient(String memberReference,String patientReference, String appointmentType, CarePlan carePlan, ProvenanceDTO provenance, Date assessmentDate);

}
