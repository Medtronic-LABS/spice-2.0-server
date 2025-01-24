package com.mdtlabs.coreplatform.fhirmapper.mapper;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.Dosage;
import org.hl7.fhir.r4.model.Group;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationDispense;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Timing;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FhirMapperTest {

    @Mock
    private FhirUtils fhirUtils;
    @InjectMocks
    private FhirMapper underTest;

    @Test
    void setGroup() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        CodeableConcept handWashingSoap = new CodeableConcept();
        handWashingSoap.setText(TestConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP);
        handWashingSoap.setCoding(List.of(new Coding(TestConstants.BLANK_STRING, TestConstants.BLANK_STRING, TestConstants.BLANK_STRING)));
        when(fhirUtils.setCodes(TestConstants.OWNED_HAND_WASHING_FACILITY_WITH_SOAP)).thenReturn(handWashingSoap);
        CodeableConcept improvedLatrine = new CodeableConcept();
        improvedLatrine.setText(TestConstants.OWNED_AN_IMPROVED_LATRINE);
        improvedLatrine.setCoding(List.of(new Coding(TestConstants.BLANK_STRING, TestConstants.BLANK_STRING, TestConstants.BLANK_STRING)));
        when(fhirUtils.setCodes(TestConstants.OWNED_AN_IMPROVED_LATRINE)).thenReturn(improvedLatrine);
        CodeableConcept treatedBedNet = new CodeableConcept();
        treatedBedNet.setText(TestConstants.OWNED_TREATED_BED_NET);
        treatedBedNet.setCoding(List.of(new Coding(TestConstants.BLANK_STRING, TestConstants.BLANK_STRING, TestConstants.BLANK_STRING)));
        when(fhirUtils.setCodes(TestConstants.OWNED_TREATED_BED_NET)).thenReturn(treatedBedNet);
        CodeableConcept bedNetCount = new CodeableConcept();
        bedNetCount.setText(TestConstants.BED_NET_COUNT);
        when(fhirUtils.setCodes(TestConstants.BED_NET_COUNT)).thenReturn(bedNetCount);
        Group group = underTest.setGroup(householdDTO, new Group());
        Assertions.assertEquals(householdDTO.getName(), group.getName());
        Assertions.assertEquals(householdDTO.getNoOfPeople(), group.getQuantity());
        householdDTO.setHouseholdNo(TestConstants.ONE);
        Group updatedGroup = underTest.setGroup(householdDTO, group);
        Assertions.assertEquals(householdDTO.getHouseholdNo().toString(), updatedGroup.getIdentifier().getFirst().getValue());
    }


    @Test
    void setLocation() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        Location location = underTest.setLocation(new Location(), householdDTO);
        Assertions.assertNotNull(location);
    }

    @Test
    void setRelatedPerson() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        RelatedPerson relatedPerson = underTest.setRelatedPerson(householdMemberDTO, new RelatedPerson());
        Assertions.assertNotNull(relatedPerson);
        householdMemberDTO.setGender(null);
        householdMemberDTO.setDateOfBirth(new Date());
        householdMemberDTO.setName(null);
        householdMemberDTO.setPatientReference(null);
        householdMemberDTO.setVillage(null);
        RelatedPerson nullRelatedPerson = underTest.setRelatedPerson(householdMemberDTO, relatedPerson);
        Assertions.assertNotNull(nullRelatedPerson);
    }

    @Test
    void setGroupCharacteristicComponent() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        List<Group.GroupCharacteristicComponent> groupCharacteristicComponents = new ArrayList<>();
        Group.GroupCharacteristicComponent groupCharacteristicComponent = new Group.GroupCharacteristicComponent();
        groupCharacteristicComponent.getCode().setText(TestConstants.TEXT);
        groupCharacteristicComponents.add(groupCharacteristicComponent);
        underTest.setGroupCharacteristicComponent(groupCharacteristicComponents, TestConstants.URL, householdMemberDTO);
        Assertions.assertNotNull(groupCharacteristicComponents);
    }

    @Test
    void setDeviceEntity() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        Device device = underTest.setDeviceEntity(householdDTO, new Device());
        householdDTO.setHeadPhoneNumber(TestConstants.PHONE_NUMBER);
        Device trueDevice = underTest.setDeviceEntity(householdDTO, device);
        Assertions.assertNotNull(device);
        Assertions.assertNotNull(trueDevice);
    }


    @Test
    void toHousehold() {
        Group group = TestDataProvider.getGroup();
        HouseholdDTO householdDTO = underTest.toHousehold(group, new HouseholdDTO());
        Assertions.assertNotNull(householdDTO);
    }

    @Test
    void toPatient() {
        Patient patient = TestDataProvider.getPatient();
        PatientDTO patientDTO = underTest.toPatient(patient, new PatientDTO());
        patient.setId(TestConstants.BLANK_STRING);
        patient.setGender(null);
        patient.setBirthDate(null);
        PatientDTO nullPatientDTO = underTest.toPatient(patient, new PatientDTO());
        Assertions.assertNotNull(patientDTO);
        Assertions.assertNotNull(nullPatientDTO);
    }

    @Test
    void relatedPersonToPatient() {
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        PatientDTO patientDTO = underTest.relatedPersonToPatient(relatedPerson, new PatientDTO());
        HumanName humanName = new HumanName();
        humanName.setText(null);
        relatedPerson.setName(List.of(humanName));
        relatedPerson.setId(TestConstants.BLANK_STRING);
        relatedPerson.setBirthDate(null);
        relatedPerson.setGender(null);
        ContactPoint telecom = new ContactPoint();
        relatedPerson.addTelecom(telecom);
        relatedPerson.setRelationship(null);
        relatedPerson.setIdentifier(null);
        Address address = new Address();
        address.setCity(null);
        relatedPerson.setAddress(List.of(address));
        PatientDTO nullPatientDTO = underTest.relatedPersonToPatient(relatedPerson, new PatientDTO());
        Assertions.assertNotNull(patientDTO);
        Assertions.assertNotNull(nullPatientDTO);
    }

    @Test
    void toHouseholdMember() {
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        HouseholdMemberDTO householdMemberDTO = underTest.toHouseholdMember(relatedPerson);
        relatedPerson.setRelationship(null);
        relatedPerson.setIdentifier(null);
        relatedPerson.setAddress(null);
        HouseholdMemberDTO nullHouseholdMemberDTO = underTest.toHouseholdMember(relatedPerson);
        Assertions.assertNotNull(householdMemberDTO);
        Assertions.assertNotNull(nullHouseholdMemberDTO);
    }

    @Test
    void toHouseholdMembers() {
        Bundle bundle = new Bundle();
        bundle.addEntry().setResource(TestDataProvider.getRelatedPerson());
        List<HouseholdMemberDTO> householdMemberDTOs = underTest.toHouseholdMembers(bundle);
        Assertions.assertNotNull(householdMemberDTOs);
    }

    @Test
    void mapHouseholdDevice() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        Device device = TestDataProvider.getDevice();
        underTest.mapHousehold(householdDTO, device);
        Assertions.assertNotNull(device);
        Assertions.assertEquals(householdDTO.getHeadPhoneNumber(), device.getIdentifier().getFirst().getValue());
    }

    @Test
    void mapHouseholdLocation() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        Location location = TestDataProvider.getLocation();
        underTest.mapHousehold(householdDTO, location);
        Assertions.assertNotNull(location);
        Assertions.assertEquals(householdDTO.getVillage(), location.getAddress().getCity());
    }

    @Test
    void mapHouseholdGroup() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        Group group = TestDataProvider.getGroup();
        underTest.mapHousehold(householdDTO, group);
        Assertions.assertNotNull(group);
        Assertions.assertEquals(householdDTO.getName(), group.getName());
    }

    @Test
    void mapPrescriptionDTO() {
        //given
        Identifier identifier = new Identifier();
        Reference reference = new Reference(TestConstants.PATIENT_REFERENCE);
        MedicationRequest.MedicationRequestDispenseRequestComponent component = new MedicationRequest.MedicationRequestDispenseRequestComponent();
        Dosage dosage = new Dosage();
        Timing timing = new Timing();
        Timing.TimingRepeatComponent timingRepeatComponent = new Timing.TimingRepeatComponent();
        MedicationRequest medicationRequest = TestDataProvider.getMedicationRequest();
//        medicationRequest.setDispenseRequest()
        identifier.setSystem(FhirIdentifierConstants.MEDICATION_ID_SYSTEM_URL);
        identifier.setValue(TestConstants.TWO_STR);
        medicationRequest.setIdentifier(List.of(identifier));
        medicationRequest.setStatus(MedicationRequest.MedicationRequestStatus.ACTIVE);
        reference.setReference("Reference");
        reference.setDisplay(TestConstants.TEXT);
        medicationRequest.setEncounter(reference);
        medicationRequest.setMedication(reference);
        component.setValidityPeriod(new Period());
        medicationRequest.setDispenseRequest(component);
        timingRepeatComponent.setPeriod(1L);
        timing.setRepeat(timingRepeatComponent);
        dosage.setTiming(timing);
        dosage.setText(TestConstants.TEXT);
        medicationRequest.setDosageInstruction(List.of(dosage));
        medicationRequest.getDispenseRequest().getValidityPeriod().setEnd(new Date());
        medicationRequest.getDispenseRequest().getValidityPeriod().setStart(new Date());

        //when
        when(fhirUtils.getIdFromReference(medicationRequest.getEncounter().getReference())).thenReturn(TestConstants.PATIENT_REFERENCE);

        //then
        PrescriptionDTO response = underTest.mapPrescriptionDTO(medicationRequest);
        Assertions.assertNotNull(response);

        //given
        Annotation annotation = new Annotation();
        annotation.setText(TestConstants.TEXT);
        annotation.setTime(new Date());
        medicationRequest.setNote(List.of(annotation));

        //when
        when(fhirUtils.getIdFromReference(medicationRequest.getEncounter().getReference())).thenReturn(TestConstants.PATIENT_REFERENCE);

        //then
        response = underTest.mapPrescriptionDTO(medicationRequest);
        Assertions.assertNotNull(response);
    }

    @Test
    void mapDispensePrescriptionDTO() {
        //given
        MedicationDispense medicationDispense = new MedicationDispense();
        List<Identifier> theIdentifier = new ArrayList<>();
        Identifier identifier1 = new Identifier().setSystem("nullmedication-id").setValue("12345");
        Identifier identifier2 = new Identifier().setSystem("nullprescription-id").setValue("98765");
        Identifier identifier3 = new Identifier().setSystem("nullprescription-filled-days").setValue("98765");
        Identifier identifier4 = new Identifier().setSystem("already-filled-days").setValue("98765");
        theIdentifier.add(identifier1);
        theIdentifier.add(identifier2);
        theIdentifier.add(identifier3);
        theIdentifier.add(identifier4);
        medicationDispense.setIdentifier(theIdentifier);
        List<Annotation> theNote = new ArrayList<>();
        theNote.add(new Annotation());
        theNote.add(new Annotation());
        medicationDispense.setNote(theNote);
        List<Dosage> theDosageInstruction = new ArrayList<>();
        Dosage dosage = new Dosage();
        dosage.setTiming(new Timing().setRepeat(new Timing.TimingRepeatComponent().setPeriod(TestConstants.ONE)));
        dosage.setPatientInstruction(TestConstants.TWO_STR);
        dosage.setAdditionalInstruction(new ArrayList<>());
        List<Dosage.DosageDoseAndRateComponent> theDoseAndRate = new ArrayList<>();
        theDoseAndRate.add(new Dosage.DosageDoseAndRateComponent());
        dosage.setDoseAndRate(theDoseAndRate);
        theDosageInstruction.add(dosage);
        medicationDispense.setDosageInstruction(theDosageInstruction);

        //then
        PrescriptionDTO result = underTest.mapDispensePrescriptionDTO(medicationDispense);
        Assertions.assertNotNull(result);
    }

}