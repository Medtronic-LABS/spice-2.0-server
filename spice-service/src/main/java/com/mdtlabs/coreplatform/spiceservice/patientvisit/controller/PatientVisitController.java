package com.mdtlabs.coreplatform.spiceservice.patientvisit.controller;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patientvisit.service.PatientVisitService;

/**
 * <p>
 * This class is a controller class to perform operation on Patient Visit entity.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 09, 2023
 */
@RestController
@RequestMapping("/patientvisit")
public class PatientVisitController {

    private final PatientVisitService patientVisitService;

    @Autowired
    public PatientVisitController(PatientVisitService patientVisitService) {
        this.patientVisitService = patientVisitService;
    }

    /**
     * <p>
     * This method is used to add a new patient visit.
     * </p>
     *
     * @param patientVisitDto - request dto
     * @return patientVisit Entity.
     */
    @PostMapping("/create")
    public SuccessResponse<Map<String, Object>> addPatientVisit(@RequestBody PatientVisitDTO
        patientVisitDto) {
        return new SuccessResponse<>(SuccessCode.PATIENT_VISIT_SAVE,
            patientVisitService.createPatientVisit(patientVisitDto), HttpStatus.CREATED);
    }


}
