package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientConverterTest {

    @Mock
    CommonConverter commonConverter;

    @Mock
    FhirUtils fhirUtils;

    @InjectMocks
    PatientConverter patientConverter;

    @Test
    void createPatientTest() {
        TestDataProvider.init();
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        BioDataDTO bioDataDTO = TestDataProvider.getBioData();
        Patient patient = null;

        // when
        TestDataProvider.getStaticMock();
        when(commonConverter.getFhirPhoneNumberCategory(anyString())).thenReturn(ContactPoint.ContactPointUse.TEMP);
        when(commonConverter.createHumanName(bioDataDTO.getFirstName(),
                bioDataDTO.getMiddleName(), bioDataDTO.getLastName(), null)).thenReturn(new HumanName());

        Patient response = patientConverter.createPatient(patient, bioDataDTO, bioMetricsDTO, new Date());
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void createPatientTestWithNullDob() {
        TestDataProvider.init();
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        BioDataDTO bioDataDTO = TestDataProvider.getBioData();
        Patient patient = null;

        // set
        bioMetricsDTO.setDateOfBirth(null);
        bioMetricsDTO.setGender(TestConstants.TEXT);

        // when
        TestDataProvider.getStaticMock();
        when(commonConverter.getFhirPhoneNumberCategory(anyString())).thenReturn(ContactPoint.ContactPointUse.TEMP);
        when(commonConverter.createHumanName(bioDataDTO.getFirstName(),
                bioDataDTO.getMiddleName(), bioDataDTO.getLastName(), null)).thenReturn(new HumanName());

        Patient response = patientConverter.createPatient(patient, bioDataDTO, bioMetricsDTO, new Date());
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void convertToPatientDetails() {
        Patient patient = TestDataProvider.getPatient();

        PatientDetailsDTO patientDTO = new PatientDetailsDTO();

        when(fhirUtils.getIdFromReference(any())).thenReturn("1234");
        PatientDetailsDTO result = patientConverter.convertToPatientDetails(patient, patientDTO);
        Assertions.assertNull(result.getFirstName());
        Assertions.assertNull(result.getLastName());
        Assertions.assertTrue(result.getIsActive());
        Assertions.assertEquals(TestConstants.ONE_TWO_THREE_FOUR, result.getId());
        Assertions.assertEquals(TestConstants.MALE, result.getGender().toLowerCase());
    }

    @Test
    void mapPatient() {
        //given
        Patient patient = TestDataProvider.getPatient();
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDTO();
        String mapType = TestConstants.BLANK_STRING;
        HumanName humanName = new HumanName();
        humanName.setText(StringUtil.concatString(request.getBioData().getFirstName(), request.getBioData().getLastName()));

        //when
        when(commonConverter.createHumanName(request.getBioData().getFirstName(),
                request.getBioData().getMiddleName(), request.getBioData().getLastName(),
                request.getBioData().getInitial())).thenReturn(humanName);

        //then
        patientConverter.mapPatient(patient, request, mapType);
        Assertions.assertEquals(Boolean.TRUE, patient.getActive());
    }

    @Test
    void mapRelatedPerson() {
        //given
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDTO();
        String mapType = TestConstants.BLANK_STRING;
        HumanName humanName = new HumanName();
        humanName.setText(StringUtil.concatString(request.getBioData().getFirstName(), request.getBioData().getLastName()));

        //when
        when(commonConverter.createHumanName(request.getBioData().getFirstName(),
                request.getBioData().getMiddleName(), request.getBioData().getLastName(),
                request.getBioData().getInitial())).thenReturn(humanName);

        //then
        patientConverter.mapRelatedPerson(relatedPerson, request, mapType);
        Assertions.assertEquals(Boolean.TRUE, relatedPerson.getActive());
    }

    @Test
    void convertToEnrollmentResponseDTO() {
        //given
        Patient patient = TestDataProvider.getPatient();
        Extension extension = new Extension(Constants.EXTENSION_EDUCATION_URL);
        List<Extension> extensions = new ArrayList<>();
        extensions.add(extension);
        patient.setExtension(extensions);
        CodeableConcept educationLevel = new CodeableConcept();
        educationLevel.addCoding(new Coding(Constants.EXTENSION_EDUCATION_URL, "bachelor", "Bachelor's Degree"));
        extension.setValue(educationLevel);
        EnrollmentResponseDTO enrollmentResponseDTO = new EnrollmentResponseDTO();

        //then
        patientConverter.convertToEnrollmentResponseDTO(patient, enrollmentResponseDTO);
        Assertions.assertNotNull(patient.getTelecom());
    }

    @Test
    void createPatientFromRelatedPerson() {
        //given
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();

        //then
        Patient response = patientConverter.createPatientFromRelatedPerson(relatedPerson);
        Assertions.assertNotNull(response);
    }

    @Test
    void convertToPatientDetailsPatientDTO() {
        //given
        Patient patient = TestDataProvider.getPatient();
        PatientDetailsDTO patientDetailsDTO = new PatientDetailsDTO();

        //then
        PatientDetailsDTO result = patientConverter.convertToPatientDetails(patient, patientDetailsDTO);
        Assertions.assertNotNull(result);
    }
}
