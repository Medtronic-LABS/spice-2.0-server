package com.mdtlabs.coreplatform.fhirmapper.patientvisit.controller;

import java.util.Map;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

/**
 * <p>
 * This class is a controller class to perform operation on Patient visit. 
 * </p>
 *
 * @author Karthick M created on sep 02, 2024
 */
@RestController
@RequestMapping("/patientvisit")
public class PatientVisitController {

    private PatientVisitService patientVisitService;

    public PatientVisitController(PatientVisitService patientVisitService) {
        this.patientVisitService = patientVisitService;
    }
    
    /**
     * Creates or updates patient visit.
     * 
     * @param patientVisit
     * @return Map<String, String>
     */
    @PostMapping("/create")
    public Map<String, Object> createOrUpdatePatientVisit(@RequestBody PatientVisitDTO patientVisit) {
        return patientVisitService.createPatientVisit(patientVisit);
    }
}
