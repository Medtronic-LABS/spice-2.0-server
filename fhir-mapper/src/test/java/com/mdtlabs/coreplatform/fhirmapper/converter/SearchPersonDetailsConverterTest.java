package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.List;

import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.StringType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SearchPersonDetailsConverterTest {

    @InjectMocks
    private SearchPersonDetailsConverter searchPersonDetailsConverter;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private SpiceConverter spiceConverter;

    @Mock
    private RestApiUtil restApiUtil;

    @Test
    void constructSearchPersonDetailsIsAssessment() {
        //given
        SearchPersonDetailsDTO searchPersonDetailsDTO = new SearchPersonDetailsDTO();
        searchPersonDetailsDTO.setRelatedPerson(null);
        searchPersonDetailsDTO.setPatient(TestDataProvider.getPatient());
        String type = "assessment";

        //then
        PatientDetailsDTO response = searchPersonDetailsConverter.constructSearchPersonDetails(searchPersonDetailsDTO, type);
        Assertions.assertNotNull(response);
    }

//    @Test
    void constructSearchPersonDetails() {
        //given
        SearchPersonDetailsDTO searchPersonDetailsDTO = new SearchPersonDetailsDTO();
        searchPersonDetailsDTO.setRelatedPerson(TestDataProvider.getRelatedPerson());
        String type = "assessment";
        searchPersonDetailsDTO.setPatient(TestDataProvider.getPatient());

        //then
        PatientDetailsDTO response = searchPersonDetailsConverter.constructSearchPersonDetails(searchPersonDetailsDTO, type);
        Assertions.assertNotNull(response);
    }

    @Test
    void constructSearchPersonDetailsIsDispense() {
        //given
        SearchPersonDetailsDTO searchPersonDetailsDTO = new SearchPersonDetailsDTO();
        searchPersonDetailsDTO.setRelatedPerson(null);
        String type = "isDispense";
        searchPersonDetailsDTO.setPatient(TestDataProvider.getPatient());

        //then
        PatientDetailsDTO response = searchPersonDetailsConverter.constructSearchPersonDetails(searchPersonDetailsDTO, type);
        Assertions.assertNotNull(response);
    }

    @Test
    void convertToPatientDetails() {
        //given
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        List<StringType> theGiven = new ArrayList<>();
        theGiven.add(new StringType("John"));
        theGiven.add(new StringType("Doe"));
        relatedPerson.getName().getFirst().setGiven(theGiven);
        relatedPerson.getAddress().getFirst().setText("Village");
        PatientDetailsDTO patientDTO = new PatientDetailsDTO();

        //then
        searchPersonDetailsConverter.convertToPatientDetails(relatedPerson, patientDTO);
        Assertions.assertEquals(patientDTO.getIsActive(), relatedPerson.getActive());
    }
}