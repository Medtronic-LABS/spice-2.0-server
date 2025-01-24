package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioMetricsDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RelatedPersonConverterTest {
    @Mock
    CommonConverter commonConverter;

    @InjectMocks
    RelatedPersonConverter relatedPersonConverter;

    @Test
    void createRelatedPersonTest() {
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        BioDataDTO bioDataDTO = TestDataProvider.getBioData();
        RelatedPerson relatedPerson = new RelatedPerson();
        List<Identifier> identifiers = new ArrayList<>();
        Identifier patientId = new Identifier();
        patientId.setSystem("http://hospital.org/patients").setValue("12345");
        identifiers.add(patientId);
        Identifier nationalId = new Identifier();
        nationalId.setSystem("http://national.gov/ids").setValue("A123456789");
        identifiers.add(nationalId);
        relatedPerson.setIdentifier(identifiers);

        // when
        when(commonConverter.getFhirPhoneNumberCategory(anyString())).thenReturn(ContactPoint.ContactPointUse.TEMP);
        when(commonConverter.createHumanName(bioDataDTO.getFirstName(),
                bioDataDTO.getMiddleName(), bioDataDTO.getLastName(), null)).thenReturn(new HumanName());
        RelatedPerson response = relatedPersonConverter.createRelatedPerson(relatedPerson, bioDataDTO, bioMetricsDTO, new Date(),
                TestConstants.TEXT, TestConstants.TEXT, TestConstants.TEXT);
        Assertions.assertNotNull(response);
    }

    @Test
    void createRelatedPersonTestWithNullDob() {
        BioMetricsDTO bioMetricsDTO = TestDataProvider.getBioMetricsDTO();
        BioDataDTO bioDataDTO = TestDataProvider.getBioData();
        RelatedPerson relatedPerson = null;

        // set
        bioMetricsDTO.setDateOfBirth(null);
        bioMetricsDTO.setGender(TestConstants.TEXT);

        // when
        when(commonConverter.getFhirPhoneNumberCategory(anyString())).thenReturn(ContactPoint.ContactPointUse.TEMP);
        when(commonConverter.createHumanName(bioDataDTO.getFirstName(),
                bioDataDTO.getMiddleName(), bioDataDTO.getLastName(), null)).thenReturn(new HumanName());
        RelatedPerson response = relatedPersonConverter.createRelatedPerson(relatedPerson, bioDataDTO, bioMetricsDTO, new Date(),
               TestConstants.TEXT, TestConstants.TEXT, TestConstants.TEXT);
        Assertions.assertNotNull(response);
    }
}
