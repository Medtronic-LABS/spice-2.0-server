package com.mdtlabs.coreplatform.spiceservice.staticdata.service;

import java.util.*;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.*;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.model.*;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.*;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.impl.StaticDataServiceImpl;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class StaticDataServiceTest {

    @InjectMocks
    private StaticDataServiceImpl staticDataService;

    @Mock
    private FormMetaRepository formMetaRepository;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    @Mock
    private UserServiceApiInterface userServiceApiInterface;

    @Mock
    private MenuRepository menuRepository;

    @Mock
    private SymptomRepository symptomRepository;

    @Mock
    private PresentingComplaintsRepository presentingComplaintsRepository;

    @Mock
    private SystemicExaminationRepository systemicExaminationRepository;

    @Mock
    private DiseaseCategoryRepository diseaseCategoryRepository;

    @Mock
    private ExaminationRepository examinationRepository;

    @Mock
    private ObstetricExaminationRepository obstetricExaminationRepository;

    @Mock
    private MetaRepository metaRepository;

    @Mock
    private FrequencyTypeRepository frequencyTypeRepository;
    @Mock
    private ComorbidityRepository comorbidityRepository;
    @Mock
    private ComplaintsRepository complaintsRepository;
    @Mock
    private ComplicationRepository complicationRepository;
    @Mock
    private CultureRepository cultureRepository;
    @Mock
    private CurrentMedicationRepository currentMedicationRepository;
    @Mock
    private DosageFormRepository dosageFormRepository;
    @Mock
    private FrequencyRepository frequencyRepository;
    @Mock
    private LifestyleRepository lifestyleRepository;
    @Mock
    private MedicalComplianceRepository medicalComplianceRepository;
    @Mock
    private ModelQuestionsRepository modelQuestionsRepository;
    @Mock
    private NutritionLifestyleRepository nutritionLifestyleRepository;
    @Mock
    private PhysicalExaminationRepository physicalExaminationRepository;
    @Mock
    private ReasonRepository reasonRepository;
    @Mock
    private RiskAlgorithmRepository riskAlgorithmRepository;
    @Mock
    private UnitRepository unitRepository;
    @Mock
    private DiagnosisRepository diagnosisRepository;
    @Mock
    private RedisTemplate<String, Map<String, List<MetaDataDTO>>> redisTemplate;
    @Mock
    private DosageFrequencyRepository dosageFrequencyRepository;
    @Mock
    private FormMetaService formMetaService;
    @Mock
    private MessageRepository messageRepository;



    private StaticMetaDataResponseDTO staticData;
    private Map<String, List<MetaDataDTO>> metaDatas;


    @BeforeEach
    public void setup() {
        ReflectionTestUtils.setField(staticDataService, "retryAttempts", 7);
        TestDataProvider.init();
        staticData = new StaticMetaDataResponseDTO();
        metaDatas = new HashMap<>();
        metaDatas.put(Constants.META, List.of(new MetaDataDTO(Constants.MEDICAL_SUPPLIES), new MetaDataDTO(Constants.COUNSELLED_ON),
                new MetaDataDTO(Constants.PREGNANCY_HISTORY), new MetaDataDTO(Constants.COST), new MetaDataDTO(Constants.BLOOD_GROUP),
                new MetaDataDTO(Constants.PATIENT_STATUS), new MetaDataDTO(Constants.DELIVERY_AT), new MetaDataDTO(Constants.DELIVERY_BY),
                new MetaDataDTO(Constants.DELIVERY_STATUS), new MetaDataDTO(Constants.DELIVERY_TYPE), new MetaDataDTO(Constants.MOTHER_DELIVERY_STATUS),
                new MetaDataDTO(Constants.RISK_FACTORS), new MetaDataDTO(Constants.CONDITION_OF_MOTHER), new MetaDataDTO(Constants.NEONATE_OUTCOME),
                new MetaDataDTO(Constants.IMMUNISATION_STATUS), new MetaDataDTO(Constants.MUAC)));
    }

    @Test
    void testSetMedicalSupplies() {
        String meta = Constants.MEDICAL_SUPPLIES;
        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.MEDICAL_SUPPLIES, staticData.getMedicalSupplies().getFirst().getCategory());
    }

    @Test
    void testSetCounselledOn() {
        String meta = Constants.COUNSELLED_ON;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.COUNSELLED_ON, staticData.getCounselledOn().getFirst().getCategory());
    }

    @Test
    void testSetRiskFactors() {
        String meta = Constants.RISK_FACTORS;
        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.RISK_FACTORS, staticData.getRiskFactors().getFirst().getCategory());
    }

    @Test
    void testSetCost() {
        String meta = Constants.COST;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.COST, staticData.getCost().getFirst().getCategory());
    }

    @Test
    void testSetBloodGroups() {
        String meta = Constants.BLOOD_GROUP;
        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.BLOOD_GROUP, staticData.getBloodGroup().getFirst().getCategory());
    }

    @Test
    void testSetPregnancyHistories() {
        String meta = Constants.PREGNANCY_HISTORY;
        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.PREGNANCY_HISTORY, staticData.getPregnancyHistories().getFirst().getCategory());
    }

    @Test
    void testSetPatientStatus() {
        String meta = Constants.PATIENT_STATUS;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.PATIENT_STATUS, staticData.getPatientStatus().getFirst().getCategory());
    }

    @Test
    void testSetDeliveryAt() {
        String meta = Constants.DELIVERY_AT;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.DELIVERY_AT, staticData.getDeliveryAt().getFirst().getCategory());
    }

    @Test
    void testSetDeliveryBy() {
        String meta = Constants.DELIVERY_BY;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.DELIVERY_BY, staticData.getDeliveryBy().getFirst().getCategory());
    }

    @Test
    void testSetDeliveryStatus() {
        String meta = Constants.DELIVERY_STATUS;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.DELIVERY_STATUS, staticData.getDeliveryStatus().getFirst().getCategory());
    }

    @Test
    void testSetMotherDeliveryStatus() {
        String meta = Constants.MOTHER_DELIVERY_STATUS;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.MOTHER_DELIVERY_STATUS, staticData.getMotherDeliveryStatus().getFirst().getCategory());
    }

    @Test
    void testSetConditionOfMother() {
        String meta = Constants.CONDITION_OF_MOTHER;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.CONDITION_OF_MOTHER, staticData.getConditionOfMother().getFirst().getCategory());
    }

    @Test
    void testSetNeonate() {
        String meta = Constants.NEONATE_OUTCOME;

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.NEONATE_OUTCOME, staticData.getNeonateOutcome().getFirst().getCategory());
    }

    @Test
    void testSetImmunisationStatus() {
        String meta = Constants.IMMUNISATION_STATUS;
        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.IMMUNISATION_STATUS, staticData.getImmunisationStatus().getFirst().getCategory());
    }

    @Test
    void testSetMuac() {
        String meta = Constants.MUAC;
        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.MUAC, staticData.getMuac().getFirst().getCategory());
    }

    @Test
    void testSetDeliveryType() {
        String meta = Constants.DELIVERY_TYPE;
        MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setCategory("Delivery Type 1");

        metaDatas.put(Constants.DELIVERY_TYPE, Arrays.asList(metaDataDTO));

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        assertEquals(Constants.DELIVERY_TYPE, staticData.getDeliveryType().getFirst().getCategory());
    }

    @Test
    void testSetUnknownMeta() {
        String meta = "UNKNOWN_META";

        staticDataService.setOtherMetas(staticData, meta, metaDatas);

        // Assert that no changes were made to staticData for an unknown meta key
        assertEquals(null, staticData.getMedicalSupplies());
        assertEquals(null, staticData.getCounselledOn());
    }

    @AfterEach
    public void wrapUp() {
        TestDataProvider.cleanUp();
    }


    @Test
    void getUserStaticData() {
        TestDataProvider.getStaticMock();
        StaticUserDataResponseDTO mockResponse = TestDataProvider.getStaticUserDataResponseDTO();
        HealthFacilityDTO healthFacilityDTO = TestDataProvider.getHealthFacilityDTO();
        healthFacilityDTO.setLinkedVillages(List.of(TestDataProvider.getVillage()));
        healthFacilityDTO.setCustomizedWorkflows(TestDataProvider.getCustomizedWorkflows());
        healthFacilityDTO.setClinicalWorkflows(List.of(TestDataProvider.getClinicalWorkflow()));
        healthFacilityDTO.setTenantId(TestConstants.ONE);
        List<HealthFacilityDTO> healthFacilityDTOs = List.of(healthFacilityDTO);
        UserResponseDTO userResponseDTO = TestDataProvider.getUserResponseDTO();
        userResponseDTO.setVillages(List.of(TestDataProvider.getVillage()));
        ResponseEntity<UserResponseDTO> userResponse = new ResponseEntity<UserResponseDTO>(userResponseDTO, HttpStatus.OK);
        Menu menu = TestDataProvider.getMenu();
        Culture culture = new Culture();
        culture.setAppTypes(List.of(""));
        List<Culture> cultures = List.of(culture);
        List<Program> programs = List.of(new Program());
        mockResponse.setCultures(cultures);
        mockResponse.setDefaultHealthFacility(healthFacilityDTO);
        RegionCustomizationDTO regionCustomizationDTO = new RegionCustomizationDTO();
        regionCustomizationDTO.setCategory(Constants.CONSENT_FORM);
        regionCustomizationDTO.setType("");
        regionCustomizationDTO.setFormInput("");


        Menu menu1 = new Menu();
        menu1.setMenus(new ArrayList<>(Arrays.asList(new HashMap<>(), new HashMap<>())));
        when(menuRepository.findByRoleNameInAndIsActiveTrueAndIsDeletedFalse(any())).thenReturn(List.of(menu1));

        when(adminServiceApiInterface.getHealthFacilitiesByTenants(
                CommonUtil.getAuthToken(), "mob",
                UserContextHolder.getUserDto().getOrganizationIds().stream().toList())).thenReturn(healthFacilityDTOs);
        when(adminServiceApiInterface.getHealthFacilitiesByChiefdom(CommonUtil.getAuthToken(), UserContextHolder.getUserDto().getClient(), mockResponse.getDefaultHealthFacility().getChiefdom().getId())).thenReturn(healthFacilityDTOs);
        when(cultureRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(cultures);
        when(userServiceApiInterface.getUserVillages(CommonUtil.getAuthToken(), UserContextHolder.getUserDto().getClient(), UserContextHolder.getUserDto().getId())).thenReturn(userResponse);
        when(menuRepository.getMenuByRole(UserContextHolder.getUserDto().getRoles().get(0).getName(), null)).thenReturn(List.of(menu));
        when(adminServiceApiInterface.getPrograms(CommonUtil.getAuthToken(), CommonUtil.getClient(), healthFacilityDTOs.stream().map(HealthFacilityDTO::getId).toList())).thenReturn(programs);
        when(adminServiceApiInterface.getRegionCustomizationsByCategory(CommonUtil.getAuthToken(),
                UserContextHolder.getUserDto().getClient(), Constants.CONSENT_FORM)).thenReturn(new ArrayList<>(Arrays.asList(regionCustomizationDTO)));

        when(menuRepository.findByRoleNameAndIsActiveTrueAndIsDeletedFalse(UserContextHolder.getUserDto().getRoles().get(0).getName())).thenReturn(menu);
        StaticUserDataResponseDTO actualResponse = staticDataService.getUserStaticData();
        Assertions.assertNotNull(actualResponse);
    }

    @Test
    void testGetWorkflowFormDataForStaticData() {
        TestDataProvider.getStaticMock();
        List<FormMeta> formMetas = new ArrayList<>();
        formMetas.add(new FormMeta());
        List<Long> ids = List.of(1L);
        TestDataProvider.getStaticMock();
        Culture culture = new Culture();
        Long cultureId = 1l;
        ClinicalWorkflow clinicalWorkflow = new ClinicalWorkflow();
        clinicalWorkflow.setWorkflowName("sample");

        Map<String, List<MetaDataDTO>> valuesMap = new HashMap<>();
        valuesMap.put("key", List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.SYMPTOMS, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.DIAGNOSIS, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.PRESENTING_COMPLAINTS, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.SYSTEMIC_EXAMINATION, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.EXAMINATION, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.DISEASE, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.SYMPTOMS_BY_CATEGORY, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.OBSTETRIC_EXAMINATION, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.META, List.of(TestDataProvider.getMetaDataDTO()));
        MetaDataDTO metaDataDTO = TestDataProvider.getMetaDataDTO();

        metaDataDTO.setWorkflow(clinicalWorkflow.getWorkflowName());
        valuesMap.put(Constants.META_MODEL_QUESTIONS, new ArrayList<>(Arrays.asList(metaDataDTO)));

        ValueOperations operations = mock(ValueOperations.class);
        when(redisTemplate.opsForValue()).thenReturn(operations);
        when(operations.get(Constants.META)).thenReturn(valuesMap);
        HealthFacility healthFacility = TestDataProvider.getHealthFacility();

        healthFacility.setClinicalWorkflows(new ArrayList<>(Arrays.asList(clinicalWorkflow)));
        List<CountryCustomization> countryCustomizations = TestDataProvider.getCountryCustomizations();
        List<WorkflowCustomization> workflowCustomizations = TestDataProvider.getWorkflowCustomizations();
        SearchRequestDTO request = new SearchRequestDTO();

        when(adminServiceApiInterface.getUnSelectedClinicalWorkFlows(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                healthFacility.getClinicalWorkflows().stream().map(BaseEntity::getId).toList())).thenReturn(List.of(clinicalWorkflow));
        when(cultureRepository.findByNameIgnoreCase(Constants.DEFAULT_CULTURE_VALUE)).thenReturn(culture);
        when(adminServiceApiInterface.getHealthFacilitiy(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), UserSelectedTenantContextHolder.get())).thenReturn(healthFacility);
        MetaDTO response = staticDataService.getWorkflowFormDataForStaticData(request);
        Assertions.assertNotNull(response);

        healthFacility.setCustomizedWorkflows(new ArrayList<>());
        when(adminServiceApiInterface.getHealthFacilitiy(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), UserSelectedTenantContextHolder.get())).thenReturn(healthFacility);
        when(adminServiceApiInterface.getCountryCustomizations(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), cultureId)).thenReturn(countryCustomizations);
        response = staticDataService.getWorkflowFormDataForStaticData(request);
        Assertions.assertNotNull(response);

        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setDistrictId(healthFacility.getDistrict().getId());
        requestDTO.setWorkflowIds(List.of(2l));
        healthFacility.setCustomizedWorkflows(TestDataProvider.getCustomizedWorkflows());
        when(adminServiceApiInterface.getHealthFacilitiy(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), UserSelectedTenantContextHolder.get())).thenReturn(healthFacility);
        when(adminServiceApiInterface.getCountryCustomizations(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), cultureId)).thenReturn(countryCustomizations);
        when(adminServiceApiInterface.getWorkflowCustomization(eq(CommonUtil.getAuthToken()), any(), any())).thenReturn(workflowCustomizations);
        response = staticDataService.getWorkflowFormDataForStaticData(request);
        Assertions.assertNotNull(response);

        healthFacility.setCustomizedWorkflows(new ArrayList<>());
        when(adminServiceApiInterface.getHealthFacilitiy(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), UserSelectedTenantContextHolder.get())).thenReturn(healthFacility);
        response = staticDataService.getWorkflowFormDataForStaticData(request);
        Assertions.assertNotNull(response);

        request.setNonNcdWorkflowEnabled(true);
        String[] stringArray = new String[] {};
        when(formMetaRepository.getFormMetaByClinicalWorkflowIds(ids, Constants.DEFAULT_FORMS, stringArray)).thenReturn(formMetas);
        when(adminServiceApiInterface.getHealthFacilitiy(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), UserSelectedTenantContextHolder.get())).thenReturn(healthFacility);
        response = staticDataService.getWorkflowFormDataForStaticData(request);
        Assertions.assertNotNull(response);

        formMetas = new ArrayList<>();
        when(formMetaRepository.getFormMetaByClinicalWorkflowIds(ids, Constants.DEFAULT_FORMS, stringArray)).thenReturn(formMetas);
        response = staticDataService.getWorkflowFormDataForStaticData(request);
        Assertions.assertNotNull(response);

        formMetas.add(new FormMeta());
        when(formMetaRepository.getFormMetaByClinicalWorkflowIds(ids, Constants.DEFAULT_FORMS, stringArray)).thenReturn(formMetas);
        response = staticDataService.getWorkflowFormDataForStaticData(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getFormDataByFormTypeAndWorkflow() {
        FormMeta formMeta = new FormMeta();
        FormMetaDTO formMetaDTO = new FormMetaDTO();
        when(formMetaRepository.getFormMetaByFormTypeAndClinicalWorkflowName(any(), any())).thenReturn(formMeta);
        when(modelMapper.map(any(), any())).thenReturn(formMetaDTO);
        FormMetaDTO formMetaDTO1 = staticDataService.getFormDataByFormTypeAndWorkflow("", "");
        Assertions.assertNotNull(formMetaDTO1);
        when(formMetaRepository.getFormMetaByFormTypeAndClinicalWorkflowName(any(), any())).thenReturn(null);
        FormMetaDTO formMetaDTO2 = staticDataService.getFormDataByFormTypeAndWorkflow("", "");
        assertNull(formMetaDTO2);
    }

    @Test
    void getFormDataByFormType() {
        FormMeta formMeta = new FormMeta();
        FormMetaDTO formMetaDTO = new FormMetaDTO();
        when(formMetaRepository.getFormMetaByFormType(any())).thenReturn(formMeta);
        when(modelMapper.map(any(), any())).thenReturn(formMetaDTO);
        FormMetaDTO formMetaDTO1 = staticDataService.getFormDataByFormType("");
        Assertions.assertNotNull(formMetaDTO1);
        when(formMetaRepository.getFormMetaByFormType(any())).thenReturn(null);
        FormMetaDTO formMetaDTO2 = staticDataService.getFormDataByFormType("");
        assertNull(formMetaDTO2);
    }

    @Test
    void getMetadata() {
        TestDataProvider.getStaticMock();
        List<Symptom> symptoms = List.of(new Symptom());
        Map<String, List<MetaDataDTO>> valuesMap = new HashMap<>();
        valuesMap.put("key", List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.SYMPTOMS, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.DIAGNOSIS, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.PRESENTING_COMPLAINTS, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.SYSTEMIC_EXAMINATION, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.EXAMINATION, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.OBSTETRIC_EXAMINATION, List.of(TestDataProvider.getMetaDataDTO()));
        valuesMap.put(Constants.META, List.of(TestDataProvider.getMetaDataDTO()));
        List<PresentingComplaints> presentingComplaints = List.of(new PresentingComplaints());
        List<SystemicExaminations> systemicExaminations = List.of(new SystemicExaminations());
        List<DiseaseCategory> diseaseCategories = List.of(new DiseaseCategory());
        List<ObstetricExamination> obstetricExaminations = List.of(new ObstetricExamination());
        List<Examination> examinations = List.of(new Examination());
        ValueOperations operations = mock(ValueOperations.class);
        List<Message> messages = new ArrayList<>();
        when(redisTemplate.opsForValue()).thenReturn(operations);
        when(operations.get(Constants.META)).thenReturn(valuesMap);
        when(symptomRepository.findAll()).thenReturn(symptoms);
        when(presentingComplaintsRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(any())).thenReturn(presentingComplaints);
        when(systemicExaminationRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(any())).thenReturn(systemicExaminations);
        when(diseaseCategoryRepository.findAllByIsActiveTrueAndIsDeletedFalse()).thenReturn(diseaseCategories);
        when(obstetricExaminationRepository.findAllByIsActiveTrueAndIsDeletedFalse()).thenReturn(obstetricExaminations);
        when(examinationRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(any())).thenReturn(examinations);
        when(symptomRepository.findByCategoryAndIsDeletedFalseAndIsActiveTrue(any())).thenReturn(symptoms);
        when(modelMapper.map(presentingComplaints, new TypeToken<List<PresentingComplaintsDTO>>() {}.getType()))
                .thenReturn(List.of(new PresentingComplaintsDTO()));
        when(modelMapper.map(diseaseCategories, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new DiseaseCategoryDTO()));
        when(modelMapper.map(systemicExaminations, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new SystemicExaminationDTO()));
        when(modelMapper.map(examinations, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new ExaminationDTO()));
        when(modelMapper.map(obstetricExaminations, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new ObstetricExaminationDTO()));

        when(redisTemplate.opsForValue()).thenReturn(operations);
        when(operations.get(Constants.META)).thenReturn(null);
        List<Symptom> symptomList = List.of(new Symptom());
        when(symptomRepository.findAll()).thenReturn(symptomList);
        when(modelMapper.map(symptomList, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(List.of(TestDataProvider.getMetaDataDTO()));
        List<PresentingComplaints> presentingCompliants = List.of(new PresentingComplaints());
        when(presentingComplaintsRepository.findAll()).thenReturn(presentingCompliants);
        when(modelMapper.map(presentingCompliants, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(presentingCompliants);
        List<SystemicExaminations> systemicExamination = List.of(new SystemicExaminations());
        when(systemicExaminationRepository.findAll()).thenReturn(systemicExamination);
        when(modelMapper.map(systemicExamination, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(systemicExamination);
        List<DiseaseCategory> diseaseCategory = List.of(new DiseaseCategory());
        when(diseaseCategoryRepository.findAllByIsActiveTrueAndIsDeletedFalse()).thenReturn(diseaseCategory);
        when(modelMapper.map(diseaseCategory, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(diseaseCategory);
        List<Examination> examination = List.of(new Examination());
        when(examinationRepository.findAll()).thenReturn(examination);
        when(modelMapper.map(examination, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(examination);
        List<ObstetricExamination> obstetricExaminationsList = List.of(new ObstetricExamination());
        when(obstetricExaminationRepository.findAllByIsActiveTrueAndIsDeletedFalse()).thenReturn(obstetricExaminationsList);
        when(modelMapper.map(obstetricExaminationsList, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(obstetricExaminationsList);
        List<Meta> metaList = List.of(new Meta());
        when(metaRepository.findAll()).thenReturn(metaList);
        when(modelMapper.map(metaList, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(metaList);
        List<DosageFrequency> dosageFrequencyList = List.of(new DosageFrequency());
        when(dosageFrequencyRepository.findAll()).thenReturn(dosageFrequencyList);
        when(modelMapper.map(dosageFrequencyList, new TypeToken<List<MetaDataDTO>>() {
        }.getType())).thenReturn(dosageFrequencyList);
        when(messageRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(messages);

        StaticMetaDataResponseDTO staticMetaDataResponseDTO = staticDataService.getNonNCDMetaData(List.of(TestConstants.SYMPTOMS,
                TestConstants.PRESENTING_COMPLAINTS_ICCM,
                TestConstants.SYSTEMIC_EXAMINATION_ABOVE_5Y,
                TestConstants.SYSTEMIC_EXAMINATION_UNDER_5Y,
                TestConstants.DIAGNOSIS,
                Constants.EXAMINATION,
                Constants.OBSTETRIC_EXAMINATION,
                Constants.DOSAGE_FREQUENCY,
                Constants.SYMPTOMS_BY_CATEGORY,
                TestConstants.EXAMINATION_UNDER_5Y,
                Constants.PRESENTING_COMPLAINTS,
                Constants.SYSTEMIC_EXAMINATION,
                Constants.DISEASE,
                TestConstants.SYSTEMIC_EXAMINATION_PNC_MOTHER,
                TestConstants.SYSTEMIC_EXAMINATION_PNC_BABY,
                TestConstants.PRESENTING_COMPLAINTS_ANC,
                TestConstants.PRESENTING_COMPLAINTS_PNC_BABY,
                TestConstants.PRESENTING_COMPLAINTS_PNC_MOTHER,
                TestConstants.OBSTETRIC_EXAMINATION_ANC,
                TestConstants.PREGNANCY_HISTORY), Constants.ANC);
        Assertions.assertNotNull(staticMetaDataResponseDTO);
        staticMetaDataResponseDTO = staticDataService.getMetadata(List.of(TestConstants.SYMPTOMS,
                Constants.SYMPTOMS_BY_CATEGORY,
                Constants.PRESENTING_COMPLAINTS,
                Constants.SYSTEMIC_EXAMINATION,
                Constants.MEDICAL_SUPPLIES,
                Constants.COUNSELLED_ON,
                Constants.PREGNANCY_HISTORY,
                Constants.COST,
                Constants.BLOOD_GROUP,
                Constants.PATIENT_STATUS,
                Constants.DELIVERY_AT,
                Constants.DELIVERY_BY,
                Constants.DELIVERY_STATUS,
                Constants.DELIVERY_TYPE,
                Constants.MOTHER_DELIVERY_STATUS,
                Constants.RISK_FACTORS,
                Constants.CONDITION_OF_MOTHER,
                Constants.NEONATE_OUTCOME,
                Constants.IMMUNISATION_STATUS,
                Constants.MUAC, Constants.ANC));
        Assertions.assertNotNull(staticMetaDataResponseDTO);
        staticMetaDataResponseDTO = staticDataService.getMetadata(List.of(TestConstants.SYMPTOMS,
                Constants.SYMPTOMS_BY_CATEGORY, Constants.ANC));
        Assertions.assertNotNull(staticMetaDataResponseDTO);

    }

    @Test
    void shouldReturnMobileUsersAndFacilities() {
        // Given
        TestDataProvider.getStaticMock();
        List<UserResponseDTO> mobileUsers = Collections.singletonList(new UserResponseDTO());
        List<HealthFacilityDTO> healthFacilities = Collections.singletonList(new HealthFacilityDTO());

        when(userServiceApiInterface.getAllMobileUsers(any(), any())).thenReturn(mobileUsers);
        when(adminServiceApiInterface.getAllHealthFacilities(any(), any())).thenReturn(healthFacilities);

        // When
        StaticUserDataResponseDTO result = staticDataService.getMobileUsersAndFacilities();

        // Then
        Assertions.assertNotNull(result);
        assertEquals(mobileUsers, result.getMobileUsers());
        assertEquals(healthFacilities, result.getHealthFacilities());
    }

    @Test
    void shouldReturnEmptyListsWhenNoMobileUsersAndFacilities() {

        TestDataProvider.getStaticMock();

        when(userServiceApiInterface.getAllMobileUsers(any(), any())).thenReturn(Collections.emptyList());
        when(adminServiceApiInterface.getAllHealthFacilities(any(), any())).thenReturn(Collections.emptyList());

        // When
        StaticUserDataResponseDTO result = staticDataService.getMobileUsersAndFacilities();

        // Then
        Assertions.assertNotNull(result);
        assertEquals(Collections.emptyList(), result.getMobileUsers());
        assertEquals(Collections.emptyList(), result.getHealthFacilities());
    }

    @Test
    void getAllDiagnosis() {
        //given
        List<DiseaseCategory> diseaseCategories = List.of(TestDataProvider.getDiseaseCategory());

        //when
        when(diseaseCategoryRepository.findAllByIsActiveTrueAndIsDeletedFalse()).thenReturn(diseaseCategories);
        when(modelMapper.map(diseaseCategories, new TypeToken<List<DiseaseCategoryDTO>>() {
        }.getType())).thenReturn(diseaseCategories);
        //then
        List<DiseaseCategoryDTO> response = staticDataService.getAllDiagnosis();
        Assertions.assertNotNull(response);
    }

    @Test
    void testGetMetaData() {
        TestDataProvider.getStaticMock();
        List<String> metaNames = List.of(Constants.META_DOSAGE_FORM, Constants.META_NUTRITION_LIFESTYLE, Constants.META_MEDICAL_COMPLIANCES, Constants.META_DIAGNOSIS, Constants.META_REASONS, Constants.META_UNIT, Constants.META_DOSAGE_FREQUENCY, Constants.META_RISK_ALGORITHM);
        Map<String, List<MetaDataDTO>> valuesMap = TestDataProvider.getSaticDataCache();
        MetaDataDTO metaDataDTO = TestDataProvider.getMetaDataDTO();
        metaDataDTO.setCvdRiskAlgorithm(new HashMap<>());
        metaDataDTO.setCountryId(1L);
        valuesMap.put(Constants.META_RISK_ALGORITHM, new ArrayList<>(Arrays.asList(metaDataDTO)));

        List<PresentingComplaints> presentingComplaints = List.of(new PresentingComplaints());
        List<SystemicExaminations> systemicExaminations = List.of(new SystemicExaminations());
        List<DiseaseCategory> diseaseCategories = List.of(new DiseaseCategory());
        List<ObstetricExamination> obstetricExaminations = List.of(new ObstetricExamination());
        List<Examination> examinations = List.of(new Examination());
        List<DosageForm> dosageForms = new ArrayList<>();
        dosageForms.add(TestDataProvider.getDosageForm());
        List<NutritionLifestyle> nutritionLifestyles = new ArrayList<>();
        nutritionLifestyles.add(TestDataProvider.getNutritionLifestyle());
        List<Symptom> symptoms = new ArrayList<>();
        symptoms.add(TestDataProvider.getSymptom());
        List<MedicalCompliance> medicalCompliances = new ArrayList<>();
        medicalCompliances.add(TestDataProvider.getMedicalCompliance());
        List<Diagnosis> diagnosisList = new ArrayList<>();
        diagnosisList.add(TestDataProvider.getDiagnosis());
        List<Reason> reasons = new ArrayList<>();
        reasons.add(TestDataProvider.getReason());
        List<DosageFrequency> dosageFrequencies = new ArrayList<>();
        dosageFrequencies.add(TestDataProvider.getDosageFrequency());
        List<ModelQuestions> modelQuestionsList = new ArrayList<>();
        modelQuestionsList.add(TestDataProvider.getModelQuestions());
        List<Comorbidity> comorbidities = new ArrayList<>();
        comorbidities.add(TestDataProvider.getComorbidity());
        List<Complaints> complaintsList = new ArrayList<>();
        complaintsList.add(TestDataProvider.getComplaints());
        List<Complication> complications = new ArrayList<>();
        complications.add(TestDataProvider.getComplication());
        List<PhysicalExamination> physicalExaminations = new ArrayList<>();
        physicalExaminations.add(TestDataProvider.getPhysicalExamination());
        List<Lifestyle> lifestyles = new ArrayList<>();
        lifestyles.add(TestDataProvider.getLifestyle());
        List<Frequency> frequencies = new ArrayList<>();
        frequencies.add(TestDataProvider.getFrequency());
        List<FrequencyType> frequencyTypes = List.of(TestDataProvider.getFrequencyType());
        List<RiskAlgorithm> riskAlgorithms = new ArrayList<>();
        riskAlgorithms.add(TestDataProvider.getMetaRiskAlgorithm());
        List<Message> messages = new ArrayList<>();
        messages.add(new Message());

        List<CurrentMedication> currentMedications = new ArrayList<>();
        List<Unit> units = new ArrayList<>();
        ValueOperations valueOperations = mock(ValueOperations.class);

        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(Constants.META)).thenReturn(valuesMap);
        when(comorbidityRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(comorbidities);
        when(complaintsRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(complaintsList);
        when(complicationRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(complications);
        when(currentMedicationRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(currentMedications);
        when(dosageFormRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(dosageForms);
        when(dosageFrequencyRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(dosageFrequencies);
        when(frequencyRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(frequencies);
        when(frequencyTypeRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(frequencyTypes);
        when(lifestyleRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(lifestyles);
        when(modelQuestionsRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(modelQuestionsList);
        when(nutritionLifestyleRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(nutritionLifestyles);
        when(physicalExaminationRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(physicalExaminations);
        when(reasonRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(reasons);
        when(unitRepository.findByNameNotLike(Constants.OTHER)).thenReturn(units);
        when(riskAlgorithmRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(riskAlgorithms);
        when(medicalComplianceRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(medicalCompliances);
        when(symptomRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(symptoms);
        when(diagnosisRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(diagnosisList);

        when(symptomRepository.findAll()).thenReturn(symptoms);
        when(presentingComplaintsRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(any())).thenReturn(presentingComplaints);
        when(systemicExaminationRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(any())).thenReturn(systemicExaminations);
        when(diseaseCategoryRepository.findAllByIsActiveTrueAndIsDeletedFalse()).thenReturn(diseaseCategories);
        when(obstetricExaminationRepository.findAllByIsActiveTrueAndIsDeletedFalse()).thenReturn(obstetricExaminations);
        when(examinationRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(any())).thenReturn(examinations);
        when(symptomRepository.findByCategoryAndIsDeletedFalseAndIsActiveTrue(any())).thenReturn(symptoms);
        when(modelMapper.map(presentingComplaints, new TypeToken<List<PresentingComplaintsDTO>>() {}.getType()))
                .thenReturn(List.of(new PresentingComplaintsDTO()));
        when(modelMapper.map(diseaseCategories, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new DiseaseCategoryDTO()));
        when(modelMapper.map(systemicExaminations, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new SystemicExaminationDTO()));
        when(modelMapper.map(examinations, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new ExaminationDTO()));
        when(modelMapper.map(obstetricExaminations, new TypeToken<List<PresentingComplaintsDTO>>() {
        }.getType()))
                .thenReturn(List.of(new ObstetricExaminationDTO()));

        StaticMetaDataResponseDTO response = staticDataService.getMetadata(metaNames);
        Assertions.assertNotNull(response);

        when(valueOperations.get(Constants.META)).thenReturn(valuesMap);
        response = staticDataService.getMetadata(metaNames);
        Assertions.assertNotNull(response);
    }

    @Test
    void testGetNCDMedicalReviewStaticData() {
        TestDataProvider.getStaticMock();
        Map<String, List<MetaDataDTO>> valuesMap = TestDataProvider.getSaticDataCache();

        ValueOperations valueOperations = mock(ValueOperations.class);

        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(Constants.META)).thenReturn(valuesMap);

        MedicalReviewStaticDataDTO response = staticDataService.getNCDMedicalReviewStaticData();
        Assertions.assertNotNull(response);
    }

    @Test
    void getMetaFormData() {
        //given
        FormMetaUi formMetaUi = new FormMetaUi();
        formMetaUi.setFormName(Constants.FORM);
        formMetaUi.setId(TestConstants.ONE);

        //when
        when(formMetaService.getMetaForms(Constants.FORM)).thenReturn(formMetaUi);

        //then
        MetaFormDTO response = staticDataService.getMetaFormData(Constants.FORM);
        assertEquals(TestConstants.ONE, response.getId());
        Assertions.assertNotNull(response);
    }

    @Test
    void getMetaFormDataWithNull() {
        //when
        when(formMetaService.getMetaForms(Constants.FORM)).thenReturn(null);

        //then
        MetaFormDTO response = staticDataService.getMetaFormData(Constants.FORM);
        assertNull(response);
    }

    @Test
    void getMenu() {
        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        searchRequestDTO.setCountryId(1L);
        searchRequestDTO.setRoleName("sample");
        Menu menu = TestDataProvider.getMenu();
        menu.setCountryId(searchRequestDTO.getCountryId());
        MenuDTO expectedMenuDTO = TestDataProvider.getMenuDTO();

        //when
        TestDataProvider.getStaticMock();
        when(menuRepository.getMenuByRole(searchRequestDTO.getRoleName(), searchRequestDTO.getCountryId())).thenReturn(List.of(menu));
        when(modelMapper.map(eq(menu), eq(MenuDTO.class))).thenReturn(expectedMenuDTO);

        //then
        MenuDTO actualMenuDTO = staticDataService.getMenu(searchRequestDTO);

        Assertions.assertNotNull(actualMenuDTO);
        //assertEquals(expectedMenuDTO, actualMenuDTO);
    }

    @Test
    void getMenuWithNull() {
        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        searchRequestDTO.setCountryId(1L);

        //when
        TestDataProvider.getStaticMock();
        when(menuRepository.getMenuByRole(anyString(), anyLong())).thenReturn(Collections.emptyList());

        //then
        MenuDTO actualMenuDTO = staticDataService.getMenu(searchRequestDTO);
        Assertions.assertNotNull(actualMenuDTO);
    }

    @Test
    void testFindCulture() {
        Culture culture = new Culture();
        culture.setName("sample");
        when(cultureRepository.findByNameIgnoreCase("sample")).thenReturn(culture);
        Culture result = staticDataService.findCulture("sample");
        assertEquals("sample", result.getName());
    }

    @Test
    void getLifestyles() {
        TestDataProvider.getStaticMock();
        Map<String, List<MetaDataDTO>> valuesMap = TestDataProvider.getSaticDataCache();

        ValueOperations valueOperations = mock(ValueOperations.class);

        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(Constants.META)).thenReturn(valuesMap);
        List<MetaDataDTO> response = staticDataService.getLifestyles();
        Assertions.assertNotNull(response);
    }

    @Test
    void getAllFrequencies() {
        List<Frequency> frequencies = new ArrayList<>();
        when(frequencyRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(frequencies);
        List<Frequency> result = staticDataService.getAllFrequencies();
        Assertions.assertNotNull(result);
    }

    @Test
    void getSymptoms() {
        List<Symptom> symptoms = new ArrayList<>();
        when(symptomRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(symptoms);
        List<Symptom> result = staticDataService.getSymptoms();
        Assertions.assertNotNull(result);
    }

    @Test
    void getMessageMetaData(){
        TestDataProvider.getStaticMock();
        Map<String, List<MetaDataDTO>> valuesMap = TestDataProvider.getSaticDataCache();

        ValueOperations valueOperations = mock(ValueOperations.class);

        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get(Constants.META)).thenReturn(valuesMap);
        List<MetaDataDTO> response = staticDataService.getMessageMetaData();
        Assertions.assertNotNull(response);
    }


}
