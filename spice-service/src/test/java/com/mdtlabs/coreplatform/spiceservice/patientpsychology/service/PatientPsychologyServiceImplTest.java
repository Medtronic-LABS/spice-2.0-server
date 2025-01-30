package com.mdtlabs.coreplatform.spiceservice.patientpsychology.service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.spiceservice.patientpsychology.service.impl.PatientPsychologyServiceImpl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientPsychologyServiceImplTest {
    @InjectMocks
    private PatientPsychologyServiceImpl patientPsychologyService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Test
    void removePsychologyDataById(){
        TestDataProvider.init();
        PsychologyDTO request = new PsychologyDTO();
        when(fhirServiceApiInterface.removePsychologyData(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(request);
        PsychologyDTO response = patientPsychologyService.removePsychologyDataById(request);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void updatePsychologyData(){
        TestDataProvider.init();
        PsychologyDTO request = new PsychologyDTO();
        when(fhirServiceApiInterface.updatePsychology(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(request);
        PsychologyDTO response = patientPsychologyService.updatePsychologyData(request);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void savePsychologyData() {
        TestDataProvider.init();
        PsychologyDTO request = new PsychologyDTO();
        when(fhirServiceApiInterface.savePatientPsychology(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(request);
        PsychologyDTO response = patientPsychologyService.savePsychologyData(request);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPsychologyDataByUserIdAndRelatedPersonId() {
        TestDataProvider.init();
        PsychologyDTO request = new PsychologyDTO();
        when(fhirServiceApiInterface.getPatientPsychology(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(List.of(request));
        List<PsychologyDTO> response = patientPsychologyService.getPsychologyDataByUserIdAndRelatedPersonId(request);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }
}
