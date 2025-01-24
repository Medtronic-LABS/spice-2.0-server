package com.mdtlabs.coreplatform.spiceservice.patientnutritionlifestyle.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.patientnutritionlifestyle.service.PatientNutritionLifestyleService;

/**
 * <p>
 *     This is a service class for PatientNutritionLifestyle entity.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
@Service
public class PatientNutritionLifestyleServiceImpl implements PatientNutritionLifestyleService {

    private final FhirServiceApiInterface fhirServiceApiInterface;


    @Autowired
    public PatientNutritionLifestyleServiceImpl(FhirServiceApiInterface fhirServiceApiInterface) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public PatientNutritionLifestyle addPatientNutritionLifestyle(PatientNutritionLifestyle patientNutritionLifestyle) {
        return fhirServiceApiInterface.addPatientNutritionLifestyle(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), patientNutritionLifestyle);
    }

    /**
     * {@inheritDoc}
     */
    public List<PatientNutritionLifestyle> getPatientNutritionLifeStyleList(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getPatientNutritionLifeStyleList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }


    /**
     * {@inheritDoc}
     */
    public PatientNutritionLifestyleUpdateDTO updatePatientNutritionLifestyle(PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles) {
        return fhirServiceApiInterface.updatePatientNutritionLifestyle(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientNutritionLifestyles);
    }

    /**
     * {@inheritDoc}
     */
    public PatientNutritionLifestyle removePatientNutritionLifestyle(PatientNutritionLifestyle patientNutritionLifestyle) {
        return fhirServiceApiInterface.deletePatientNutritionLifestyle(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientNutritionLifestyle);
    }
}
