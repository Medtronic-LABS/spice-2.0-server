package com.mdtlabs.coreplatform.spiceservice.staticdata.service.impl;

import java.util.*;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.json.JSONArray;
import org.json.JSONObject;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.AppTypesContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.*;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.*;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MetaDTO.NCDFromDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.*;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.*;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.FormMetaService;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;

/**
 * <p>
 * This service implements the static data service and is responsible for
 * implementing business logic needed to get meta data related to the service.
 * </p>
 */
@Service
public class StaticDataServiceImpl implements StaticDataService {

    private ModelMapper modelMapper = new ModelMapper();

    private final FormMetaRepository formMetaRepository;

    private final AdminServiceApiInterface adminServiceApiInterface;

    private final UserServiceApiInterface userServiceApiInterface;

    private final MenuRepository menuRepository;

    private final MetaRepository metaRepository;

    private final SymptomRepository symptomRepository;

    private final PresentingComplaintsRepository presentingComplaintsRepository;

    private final SystemicExaminationRepository systemicExaminationRepository;

    private final DiseaseCategoryRepository diseaseCategoryRepository;

    private final DiagnosisRepository diagnosisRepository;

    private final ExaminationRepository examinationRepository;

    private final RedisTemplate<String, Map<String, List<MetaDataDTO>>> redisTemplate;

    private final ObstetricExaminationRepository obstetricExaminationRepository;

    private final DosageFrequencyRepository dosageFrequencyRepository;

    private final FrequencyTypeRepository frequencyTypeRepository;

    private final ComorbidityRepository comorbidityRepository;

    private final ComplaintsRepository complaintsRepository;

    private final ComplicationRepository complicationRepository;

    private final CultureRepository cultureRepository;

    private final CurrentMedicationRepository currentMedicationRepository;

    private final DosageFormRepository dosageFormRepository;

    private final FrequencyRepository frequencyRepository;

    private final LifestyleRepository lifestyleRepository;

    private final MedicalComplianceRepository medicalComplianceRepository;

    private final ModelQuestionsRepository modelQuestionsRepository;

    private final NutritionLifestyleRepository nutritionLifestyleRepository;

    private final PhysicalExaminationRepository physicalExaminationRepository;

    private final ReasonRepository reasonRepository;

    private final RiskAlgorithmRepository riskAlgorithmRepository;

    private final UnitRepository unitRepository;

    private final FormMetaService formMetaService;

    private final MessageRepository messageRepository;

    @Autowired
    public StaticDataServiceImpl(ComorbidityRepository comorbidityRepository,
                                 ComplaintsRepository complaintsRepository,
                                 ComplicationRepository complicationRepository,
                                 CultureRepository cultureRepository,
                                 CurrentMedicationRepository currentMedicationRespository,
                                 DiagnosisRepository diagnosisRepository,
                                 DosageFormRepository dosageFormRepository,
                                 DosageFrequencyRepository dosageFrequencyRepository,
                                 FrequencyRepository frequencyRepository,
                                 LifestyleRepository lifestyleRepository,
                                 MedicalComplianceRepository medicalComplianceRepository,
                                 ModelQuestionsRepository modelQuestionsRepository,
                                 NutritionLifestyleRepository nutritionLifestyleRepository,
                                 PhysicalExaminationRepository physicalExaminationRepository,
                                 ReasonRepository reasonRepository,
                                 RiskAlgorithmRepository riskAlgorithmRepository,
                                 SymptomRepository symptomRepository,
                                 UnitRepository unitRepository,
                                 FrequencyTypeRepository frequencyTypeRepository,
                                 AdminServiceApiInterface adminServiceApiInterface,
                                 FormMetaRepository formMetaRepository,
                                 UserServiceApiInterface userServiceApiInterface,
                                 MenuRepository menuRepository,
                                 PresentingComplaintsRepository presentingComplaintsRepository,
                                 SystemicExaminationRepository systemicExaminationRepository,
                                 DiseaseCategoryRepository diseaseCategoryRepository,
                                 ExaminationRepository examinationRepository,
                                 RedisTemplate<String, Map<String, List<MetaDataDTO>>> redisTemplate,
                                 ObstetricExaminationRepository obstetricExaminationRepository,
                                 MetaRepository metaRepository, FormMetaService formMetaService,
                                 MessageRepository messageRepository) {
        this.formMetaRepository = formMetaRepository;
        this.userServiceApiInterface = userServiceApiInterface;
        this.menuRepository = menuRepository;
        this.presentingComplaintsRepository = presentingComplaintsRepository;
        this.systemicExaminationRepository = systemicExaminationRepository;
        this.diseaseCategoryRepository = diseaseCategoryRepository;
        this.examinationRepository = examinationRepository;
        this.redisTemplate = redisTemplate;
        this.obstetricExaminationRepository = obstetricExaminationRepository;
        this.comorbidityRepository = comorbidityRepository;
        this.complaintsRepository = complaintsRepository;
        this.complicationRepository = complicationRepository;
        this.cultureRepository = cultureRepository;
        this.currentMedicationRepository = currentMedicationRespository;
        this.diagnosisRepository = diagnosisRepository;
        this.dosageFormRepository = dosageFormRepository;
        this.dosageFrequencyRepository = dosageFrequencyRepository;
        this.frequencyRepository = frequencyRepository;
        this.lifestyleRepository = lifestyleRepository;
        this.medicalComplianceRepository = medicalComplianceRepository;
        this.modelQuestionsRepository = modelQuestionsRepository;
        this.nutritionLifestyleRepository = nutritionLifestyleRepository;
        this.physicalExaminationRepository = physicalExaminationRepository;
        this.reasonRepository = reasonRepository;
        this.riskAlgorithmRepository = riskAlgorithmRepository;
        this.symptomRepository = symptomRepository;
        this.unitRepository = unitRepository;
        this.frequencyTypeRepository = frequencyTypeRepository;
        this.adminServiceApiInterface = adminServiceApiInterface;
        this.metaRepository = metaRepository;
        this.formMetaService = formMetaService;
        this.messageRepository = messageRepository;
    }

    @Value("${app.smart-anc}")
    private Boolean isSmartAnc;

    @Value("${app.app-version}")
    private String appVersion;

    @Value("${app.retry-call-attempts:5}")
    private Integer retryAttempts;

    @Value("${app.version-check-exempt-users}")
    private String versionCheckExemptUsers;

    /**
     * {@inheritDoc}
     */
    public StaticUserDataResponseDTO getUserStaticData() {
        StaticUserDataResponseDTO response = new StaticUserDataResponseDTO();
        UserContextDTO userDTO = UserContextHolder.getUserDto();
        Long tenantId = UserSelectedTenantContextHolder.get();

        List<HealthFacilityDTO> tenantHealthFacilities = adminServiceApiInterface.getHealthFacilitiesByTenants(
                CommonUtil.getAuthToken(), userDTO.getClient(),
                UserContextHolder.getUserDto().getOrganizationIds().stream().toList());

        List<HealthFacilityDTO> workflowHealthFacilities = adminServiceApiInterface.getHealthFacilitiesByTenants(
                CommonUtil.getAuthToken(), userDTO.getClient(),
                List.of(UserSelectedTenantContextHolder.get()));
        Set<Long> workflowIds = new HashSet<>();
        workflowHealthFacilities.forEach(healthFacility -> {
            workflowIds.addAll(healthFacility.getClinicalWorkflows().stream().map(BaseEntity::getId).toList());
            if (Objects.nonNull(healthFacility.getCustomizedWorkflows())) {
                workflowIds.addAll(healthFacility.getCustomizedWorkflows().stream().map(BaseEntity::getId).toList());
            }
        });
        Set<VillageDTO> villages = new HashSet<>();
        List<Culture> cultures = cultureRepository.findByIsDeletedFalseAndIsActiveTrue();
        response.setCultures(cultures.stream().filter(culture -> AppTypesContextHolder.get().stream().anyMatch(culture.getAppTypes()::contains)).toList());
        response.setIdentityTypes(Constants.CLIENT_IDENTITY_TYPES);
        tenantHealthFacilities.forEach(healthFacility -> {
            if (tenantId.equals(healthFacility.getTenantId())) {
                response.setDefaultHealthFacility(healthFacility);
            }
            villages.addAll(healthFacility.getLinkedVillages());
        });
        if (Objects.isNull(response.getDefaultHealthFacility())) {
            throw new SpiceValidation(1009);
        }
        response.setConsentForm(getConsentForm());
        response.setDistricts(adminServiceApiInterface.getDistrictsByCountryId(CommonUtil.getAuthToken(), tenantId, userDTO.getCountry().getId()));
        response.setChiefdoms(adminServiceApiInterface.getChiefdomsByCountryId(CommonUtil.getAuthToken(), tenantId, userDTO.getCountry().getId()));
        response.setPrograms(adminServiceApiInterface.getPrograms(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                tenantHealthFacilities.stream().map(HealthFacilityDTO::getId).toList()));
        response.setWorkflowIds(workflowIds);
        response.setUserHealthFacilities(tenantHealthFacilities);
        List<HealthFacilityDTO> healthFacilities = adminServiceApiInterface.getHealthFacilitiesByChiefdom(
                CommonUtil.getAuthToken(), userDTO.getClient(),
                response.getDefaultHealthFacility().getChiefdom().getId());
        response.setNearestHealthFacilities(healthFacilities);
        UserResponseDTO userResponse = userServiceApiInterface
                .getUserVillages(CommonUtil.getAuthToken(), userDTO.getClient(),
                        userDTO.getId())
                .getBody();
        List<Long> villageIds = userResponse.getVillages().isEmpty()
                ? villages.stream().map(VillageDTO::getId).toList()
                : userResponse.getVillages().stream().map(VillageDTO::getId).toList();
        if (Constants.COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            response.setVillages(adminServiceApiInterface.getVillageByIds(CommonUtil.getAuthToken(),
                    userDTO.getClient(), villageIds));
            if (userResponse.getVillages().isEmpty()) {
                userResponse.setVillages(response.getVillages());
            }
        } else {
            response.setVillages(adminServiceApiInterface.getVillagesByCountryId(CommonUtil.getAuthToken(),
                    userDTO.getClient(), userDTO.getCountry().getId()));
        }
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        response.setFrequency(modelMapper.map(metaRepository.findByCategoryAndIsDeletedFalseAndIsActiveTrue(Constants.MEDICATION_FREQUENCY), new TypeToken<List<FrequencyDTO>>() {
        }.getType()));
        response.setMenu(getUserMenus(userDTO.getRoles().stream().toList()));
        response.setUserProfile(userResponse);
        response.setSmartAncEnabled(isSmartAnc);
        response.setAppTypes(AppTypesContextHolder.get());
        response.setRemainingAttemptsCount(retryAttempts);
        return response;
    }

    /**
     * Get User Menus
     *
     * @param roles List of Roles
     * @return Menu
     */
    private Menu getUserMenus(List<RoleDTO> roles) {
        List<String> roleNames = roles.stream().map(RoleDTO::getName).toList();
        List<Menu> menus = menuRepository.findByRoleNameInAndIsActiveTrueAndIsDeletedFalse(roleNames);
        Menu menu = (!Objects.isNull(menus) && !menus.isEmpty()) ? menus.getFirst() : null;
        if (!Objects.isNull(menu)) {
            List<Map<String, Object>> mergedMenus = new ArrayList<>();
            Set<Object> menuNames = new HashSet<>();
            menus.forEach(value ->
                value.getMenus().forEach(menuItem -> {
                    if (menuNames.add(menuItem.get(Constants.NAME))) {
                        mergedMenus.add(menuItem);
                    }
                }));
            menu.setMenus(mergedMenus);
        }
        return menu;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public FormMetaDTO getFormDataByFormTypeAndWorkflow(String workflowName, String formType) {
        FormMeta formMeta = formMetaRepository.getFormMetaByFormTypeAndClinicalWorkflowName(workflowName, formType);
        if (null == formMeta) {
            return null;
        }
        return modelMapper.map(formMeta, FormMetaDTO.class);
    }

    /**
     * {@inheritDoc}
     */
    public FormMetaDTO getFormDataByFormType(String formType) {
        FormMeta formMeta = formMetaRepository.getFormMetaByFormType(formType);
        if (null == formMeta) {
            return null;
        }
        return modelMapper.map(formMeta, FormMetaDTO.class);
    }

    /**
     * Get Consent Form
     *
     * @return Map of Consent Form
     */
    private Map<String, String> getConsentForm() {
        UserContextDTO userDTO = UserContextHolder.getUserDto();
        Map<String, String> consentForm = new HashMap<>();
        List<RegionCustomizationDTO> regionCustomizations = adminServiceApiInterface.getRegionCustomizationsByCategory(CommonUtil.getAuthToken(),
                userDTO.getClient(), Constants.CONSENT_FORM);
        if (!Objects.isNull(regionCustomizations) && !regionCustomizations.isEmpty()) {
            for (RegionCustomizationDTO customization : regionCustomizations) {
                if (Constants.CONSENT_FORM.equals(customization.getCategory())) {
                    consentForm.put(customization.getType(), customization.getFormInput());
                }
            }
        }
        return consentForm;
    }

    /**
     * {@inheritDoc}
     */
    public MetaDTO getWorkflowFormDataForStaticData(SearchRequestDTO request) {
        ModelMapper mapper = new ModelMapper();
        HealthFacility healthFacility = adminServiceApiInterface.getHealthFacilitiy(CommonUtil.getAuthToken(),
                UserSelectedTenantContextHolder.get(), UserSelectedTenantContextHolder.get());
        MetaDTO response = getNCDForms(healthFacility, UserContextHolder.getUserDto().getCulture().getId());
        List<FormMeta> formMetas = formMetaRepository.getFormMetaByClinicalWorkflowIds(request.getWorkflowIds(),
                Constants.DEFAULT_FORMS, AppTypesContextHolder.get().toArray(new String[]{}));
        List<FormMetaDTO> responseForms = new ArrayList<>();
        if (!Objects.isNull(formMetas) && !formMetas.isEmpty()) {
            formMetas.forEach(formMeta -> responseForms.add(mapper.map(formMeta, FormMetaDTO.class)));
        }
        response.setFormData(responseForms);
        List<ClinicalWorkflow> tools = adminServiceApiInterface.getClinicalWorkflows(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request.getWorkflowIds());
        response.setClinicalTools(tools);
        return response;
    }

    /**
     * Gets a country wise NCD meta forms for screening, enrollment and assessment based on health facility and culture.
     *
     * @param healthFacility user's healthfacility
     * @param cultureId user's culture values
     * @return MetaDTO
     */
    private MetaDTO getNCDForms(HealthFacility healthFacility, Long cultureId) {
        MetaDTO response = new MetaDTO();
        cultureId = (Objects.isNull(cultureId) || Constants.ZERO == cultureId)
                ? cultureRepository.findByNameIgnoreCase(Constants.DEFAULT_CULTURE_VALUE).getId()
                : cultureId;

        List<ClinicalWorkflow> selectedClinicalWorkflows = healthFacility.getClinicalWorkflows();
        List<ClinicalWorkflow> customizedWorkflows = Objects.isNull(healthFacility.getCustomizedWorkflows()) ?
                new ArrayList<>() : healthFacility.getCustomizedWorkflows();
        Map<String, String> consentForms = new HashMap<>();
            SearchRequestDTO request = new SearchRequestDTO();
            Map<Long, ClinicalWorkflow> customizedWorkflowMap = new HashMap<>();
            customizedWorkflows.forEach(workflow ->
                customizedWorkflowMap.put(workflow.getId(), workflow));
            request.setWorkflowIds(customizedWorkflows.isEmpty() ? null : customizedWorkflows.stream().map(BaseEntity::getId).toList());
            request.setDistrictId(healthFacility.getDistrict().getId());

            List<WorkflowCustomization> workflowCustomizations = adminServiceApiInterface.getWorkflowCustomization(
                    CommonUtil.getAuthToken(),
                    UserSelectedTenantContextHolder.get(), request);
            List<Map<String, Object>> customizedModules = new ArrayList<>();
            for (WorkflowCustomization workflowCustomization : workflowCustomizations) {
                if (Constants.MODULE.equals(workflowCustomization.getType())) {
                    if (customizedWorkflowMap.keySet().contains(workflowCustomization.getClinicalWorkflowId())) {
                        Map<String, Object> customForm = new HashMap<>();
                        customForm.put(Constants.VIEW_SCREENS, customizedWorkflowMap
                                .get(workflowCustomization.getClinicalWorkflowId()).getViewScreens());
                        customForm.put(Constants.FORM_INPUT, workflowCustomization.getFormInput());
                        customForm.put(Constants.ID, workflowCustomization.getClinicalWorkflowId());
                        customizedModules.add(customForm);
                    }
                } else {
                    consentForms.put(workflowCustomization.getType(), workflowCustomization.getFormInput());
                }
            }
            response.setCustomizedWorkflow(customizedModules);
        List<ClinicalWorkflow> unSelectedClinicalWorkflows = adminServiceApiInterface.getUnSelectedClinicalWorkFlows(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                selectedClinicalWorkflows.stream().map(BaseEntity::getId).toList());

        List<String> unselectedWorkflowNames = unSelectedClinicalWorkflows.stream().map(ClinicalWorkflow::getWorkflowName).toList();

        List<CountryCustomization> countryCustomizations = adminServiceApiInterface
                .getCountryCustomizations(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), cultureId);
        NCDFromDTO assessment = new NCDFromDTO();
        NCDFromDTO enrollment = new NCDFromDTO();
        NCDFromDTO screening = new NCDFromDTO();

        if (!Objects.isNull(countryCustomizations) && !countryCustomizations.isEmpty()) {
            for (CountryCustomization customization : countryCustomizations) {
                if (Constants.INPUT_FORM.equals(customization.getCategory())) {
                    constructInputFormJSON(unselectedWorkflowNames, customization);
                } else {
                    customization.setFormInput(consentForms.keySet().contains(customization.getType())
                            ? consentForms.get(customization.getType())
                            : customization.getFormInput());
                }
                setScreening(assessment, enrollment, screening, customization);
            }
        }
        response.setEnrollment(enrollment);
        response.setAssessment(assessment);
        response.setScreening(screening);
        response.setModelQuestions(getModelQuestions(UserContextHolder.getUserDto().getCountry().getId(),
                selectedClinicalWorkflows.stream().map(ClinicalWorkflow::getWorkflowName).toList()));
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public StaticMetaDataResponseDTO getMetadata(List<String> metaNames) {
        StaticMetaDataResponseDTO response = new StaticMetaDataResponseDTO();
        Map<String, List<MetaDataDTO>> dataMap = setMetaDateCache();
        for (String metaName : metaNames) {
            switch (metaName) {
                case Constants.META_DOSAGE_FORM:
                    response.setDosageForm(dataMap.get(Constants.META_DOSAGE_FORM));
                    break;
                case Constants.META_NUTRITION_LIFESTYLE:
                    response.setNutritionLifestyle(dataMap.get(Constants.META_NUTRITION_LIFESTYLE));
                    break;
                case Constants.META_SYMPTOMS:
                    response.setSymptoms(dataMap.get(Constants.META_SYMPTOMS));
                    List<MetaDataDTO> symptoms = dataMap.get(Constants.SYMPTOMS).stream()
                            .filter(metaData -> (Objects.isNull(metaData.getAppTypes())
                                    || AppTypesContextHolder.get().stream().anyMatch(metaData.getAppTypes()::contains))).toList();
                    response.setSymptoms(symptoms);
                    break;
                case Constants.META_MEDICAL_COMPLIANCES:
                    response.setMedicalCompliances(dataMap.get(Constants.META_MEDICAL_COMPLIANCES));
                    break;
                case Constants.META_DIAGNOSIS:
                    response.setDiagnosis(dataMap.get(Constants.META_DIAGNOSIS));
                    break;
                case Constants.META_REASONS:
                    response.setReasons(dataMap.get(Constants.META_REASONS));
                    break;
                case Constants.META_UNIT:
                    response.setUnits(dataMap.get(Constants.META_UNIT));
                    break;
                case Constants.META_DOSAGE_FREQUENCY:
                    response.setDosageFrequencies(dataMap.get(Constants.META_DOSAGE_FREQUENCY));
                    break;
                case Constants.META_RISK_ALGORITHM:
                    response.setCvdRiskAlgorithms(dataMap.get(Constants.META_RISK_ALGORITHM).stream()
                            .filter(risk -> Objects.equals(risk.getCountryId(),
                                    UserContextHolder.getUserDto().getCountry().getId()))
                            .findFirst().get().getCvdRiskAlgorithm());
                    break;

                default:
                    break;
            }
        }
        return response;
    }

    /**
     * Set Other Meta values
     *
     * @param staticData StaticMetaDataResponseDTO
     * @param meta       Name of the meta
     */
    public void setOtherMetas(StaticMetaDataResponseDTO staticData, String meta, Map<String, List<MetaDataDTO>> metaDatas) {
        switch (meta) {
            case Constants.MEDICAL_SUPPLIES:
                staticData.setMedicalSupplies(getOtherMetaDetails(Constants.MEDICAL_SUPPLIES, metaDatas));
                break;
            case Constants.COUNSELLED_ON:
                staticData.setCounselledOn(getOtherMetaDetails(Constants.COUNSELLED_ON, metaDatas));
                break;
            case Constants.PREGNANCY_HISTORY:
                staticData.setPregnancyHistories(getOtherMetaDetails(Constants.PREGNANCY_HISTORY, metaDatas));
                break;
            case Constants.COST:
                staticData.setCost(getOtherMetaDetails(Constants.COST, metaDatas));
                break;
            case Constants.BLOOD_GROUP:
                staticData.setBloodGroup(getOtherMetaDetails(Constants.BLOOD_GROUP, metaDatas));
                break;
            case Constants.PATIENT_STATUS:
                staticData.setPatientStatus(getOtherMetaDetails(Constants.PATIENT_STATUS, metaDatas));
                break;
            case Constants.DELIVERY_AT:
                staticData.setDeliveryAt(getOtherMetaDetails(Constants.DELIVERY_AT, metaDatas));
                break;
            case Constants.DELIVERY_BY:
                staticData.setDeliveryBy(getOtherMetaDetails(Constants.DELIVERY_BY, metaDatas));
                break;
            case Constants.DELIVERY_STATUS:
                staticData.setDeliveryStatus(getOtherMetaDetails(Constants.DELIVERY_STATUS, metaDatas));
                break;
            case Constants.DELIVERY_TYPE:
                staticData.setDeliveryType(getOtherMetaDetails(Constants.DELIVERY_TYPE, metaDatas));
                break;
            case Constants.MOTHER_DELIVERY_STATUS:
                staticData.setMotherDeliveryStatus(getOtherMetaDetails(Constants.MOTHER_DELIVERY_STATUS, metaDatas));
                break;
            case Constants.RISK_FACTORS:
                staticData.setRiskFactors(getOtherMetaDetails(Constants.RISK_FACTORS, metaDatas));
                break;
            case Constants.CONDITION_OF_MOTHER:
                staticData.setConditionOfMother(getOtherMetaDetails(Constants.CONDITION_OF_MOTHER, metaDatas));
                break;
            case Constants.NEONATE_OUTCOME:
                staticData.setNeonateOutcome(getOtherMetaDetails(Constants.NEONATE_OUTCOME, metaDatas));
                break;
            case Constants.IMMUNISATION_STATUS:
                staticData.setImmunisationStatus(getOtherMetaDetails(Constants.IMMUNISATION_STATUS, metaDatas));
                break;
            case Constants.MUAC:
                staticData.setMuac(getOtherMetaDetails(Constants.MUAC, metaDatas));
                break;
            case Constants.STATE_OF_PERINEUM:
                staticData.setStateOfPerineum(getOtherMetaDetails(Constants.STATE_OF_PERINEUM, metaDatas));
                break;
            case Constants.PNC_NEONATE_OUTCOME:
                staticData.setPncNeonateOutcome(getOtherMetaDetails(Constants.PNC_NEONATE_OUTCOME, metaDatas));
                break;
            default:
                break;
        }
    }

    /**
     * {@inheritDoc}
     */
    public StaticMetaDataResponseDTO getNonNCDMetaData(List<String> metaNames, String type) {
        StaticMetaDataResponseDTO staticData = new StaticMetaDataResponseDTO();
        Map<String, List<MetaDataDTO>> metaDatas = setMetaDateCache();

        for (String meta : metaNames) {
            switch (meta) {
                case Constants.SYMPTOMS_BY_CATEGORY:
                    List<MetaDataDTO> symptomsByCategory = metaDatas.get(Constants.SYMPTOMS).stream()
                            .filter(metaData -> type.equals(metaData.getCategory())).toList();
                    staticData.setSymptoms(modelMapper.map(symptomsByCategory, new TypeToken<List<Symptom>>() {
                    }.getType()));
                    break;
                case Constants.PRESENTING_COMPLAINTS:
                    setPresentingComplaints(staticData, type, metaDatas);
                    break;
                case Constants.SYSTEMIC_EXAMINATION:
                    setSystemicExaminations(staticData, type, metaDatas);
                    break;
                case Constants.DISEASE:
                    List<MetaDataDTO> diseaseCategories = metaDatas.get(Constants.DISEASE).stream()
                            .filter(metaData -> type.equals(metaData.getType())).toList();
                    staticData.setDiseaseCategories(diseaseCategories);
                    break;
                case Constants.EXAMINATION:
                    List<MetaDataDTO> examinationMeta = metaDatas.get(Constants.EXAMINATION).stream()
                            .filter(metaData -> type.equals(metaData.getType())).toList();
                    staticData.setExaminations(examinationMeta);
                    break;
                case Constants.OBSTETRIC_EXAMINATION:
                    List<MetaDataDTO> physicalExaminations = metaDatas.get(Constants.OBSTETRIC_EXAMINATION).stream()
                            .filter(metaData -> type.equals(metaData.getType())).toList();
                    staticData.setObstetricExaminations(physicalExaminations);
                    break;
                default:
                    setOtherMetas(staticData, meta, metaDatas);
                    break;
            }
        }
        return staticData;

    }

    /**
     * Set Presenting Complaints in StaticResponse
     *
     * @param staticData StaticData Object
     * @param type       Type
     */
    private void setPresentingComplaints(StaticMetaDataResponseDTO staticData, String type,
            Map<String, List<MetaDataDTO>> metaDatas) {
        List<MetaDataDTO> presentingComplaintsByCategory = metaDatas.get(Constants.PRESENTING_COMPLAINTS).stream()
                .filter(metaData -> type.equals(metaData.getType())).toList();
        staticData.setPresentingComplaints(presentingComplaintsByCategory);
    }

    /**
     * Set SystemicExaminations in StaticResponse
     *
     * @param staticData StaticData Object
     * @param type       Type
     */
    private void setSystemicExaminations(StaticMetaDataResponseDTO staticData, String type,
            Map<String, List<MetaDataDTO>> metaDatas) {
        List<MetaDataDTO> systemicExaminationByCategory = metaDatas.get(Constants.SYSTEMIC_EXAMINATION).stream()
                .filter(metaData -> type.equals(metaData.getType())).toList();
        staticData.setSystemicExaminations(systemicExaminationByCategory);
    }

    /**
     * Get Meta Details Based on Category
     *
     * @param category Category
     * @return List of Meta
     */
    private List<MetaDataDTO> getOtherMetaDetails(String category, Map<String, List<MetaDataDTO>> metaDatas) {
        return metaDatas.get(Constants.META).stream()
                .filter(metaData -> category.equals(metaData.getCategory())
                        && (Objects.isNull(metaData.getAppTypes())
                        || AppTypesContextHolder.get().stream().anyMatch(metaData.getAppTypes()::contains)))
                .toList();
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, List<MetaDataDTO>> setMetaDateCache() {

        Map<String, List<MetaDataDTO>> valuesMap = redisTemplate.opsForValue().get(Constants.META);
        if (Objects.isNull(valuesMap)) {
            modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
            valuesMap = new HashMap<>();

            //set Symptoms
            List<Symptom> symptoms = symptomRepository.findAllByIsDeletedFalseAndIsActiveTrue();
            valuesMap.put(Constants.SYMPTOMS, modelMapper.map(symptoms, new TypeToken<List<MetaDataDTO>>() {
            }.getType()));

            //set PresentingComplaints
            List<PresentingComplaints> presentingCompliants = presentingComplaintsRepository.findAllByIsActiveTrueAndIsDeletedFalse();
            valuesMap.put(Constants.PRESENTING_COMPLAINTS,
                    modelMapper.map(presentingCompliants, new TypeToken<List<MetaDataDTO>>() {
                    }.getType()));

            //set SystemicExaminations
            List<SystemicExaminations> systemicExamination = systemicExaminationRepository.findAllByIsActiveTrueAndIsDeletedFalse();
            valuesMap.put(Constants.SYSTEMIC_EXAMINATION,
                    modelMapper.map(systemicExamination, new TypeToken<List<MetaDataDTO>>() {
                    }.getType()));

            // set DiseaseCategory
            List<DiseaseCategory> diseaseCategory = diseaseCategoryRepository.findAllByIsActiveTrueAndIsDeletedFalse();
            valuesMap.put(Constants.DISEASE, modelMapper.map(diseaseCategory, new TypeToken<List<MetaDataDTO>>() {
            }.getType()));
            //set Examination
            List<Examination> examination = examinationRepository.findAllByIsActiveTrueAndIsDeletedFalse();
            valuesMap.put(Constants.EXAMINATION, modelMapper.map(examination, new TypeToken<List<MetaDataDTO>>() {
            }.getType()));

            // set obstetricExaminations
            List<ObstetricExamination> obstetricExaminations = obstetricExaminationRepository
                    .findAllByIsActiveTrueAndIsDeletedFalse();
            valuesMap.put(Constants.OBSTETRIC_EXAMINATION,
                    modelMapper.map(obstetricExaminations, new TypeToken<List<MetaDataDTO>>() {
                    }.getType()));

            //set otherMetas
            List<Meta> metaList = metaRepository.findAllByIsActiveTrueAndIsDeletedFalse();
            valuesMap.put(Constants.META, modelMapper.map(metaList, new TypeToken<List<MetaDataDTO>>() {
            }.getType()));

            setNcdMetaDataCache(valuesMap);

            //set dosage frequencies
            List<DosageFrequency> dosageFrequencyList = dosageFrequencyRepository.findAllByIsActiveTrueAndIsDeletedFalse();
            valuesMap.put(Constants.DOSAGE_FREQUENCY, modelMapper.map(dosageFrequencyList, new TypeToken<List<MetaDataDTO>>() {
            }.getType()));

            //set messages
            List<Message> messages = messageRepository.findByIsDeletedFalseAndIsActiveTrue();
            valuesMap.put(Constants.META_MESSAGE, modelMapper.map(messages, new TypeToken<List<MetaDataDTO>>() {
            }.getType()));

            redisTemplate.opsForValue().set(Constants.META, valuesMap);
        }
        return valuesMap;
    }

    /**
     * {@inheritDoc}
     */
    public StaticUserDataResponseDTO getMobileUsersAndFacilities() {
        StaticUserDataResponseDTO response = new StaticUserDataResponseDTO();
        UserContextDTO userDTO = UserContextHolder.getUserDto();
        List<UserResponseDTO> mobileUsers = userServiceApiInterface.getAllMobileUsers(CommonUtil.getAuthToken(),
                userDTO.getClient());
        List<HealthFacilityDTO> healthFacilities = adminServiceApiInterface.getAllHealthFacilities(
                CommonUtil.getAuthToken(),
                userDTO.getClient());
        response.setMobileUsers(mobileUsers);
        response.setHealthFacilities(healthFacilities);
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public List<DiseaseCategoryDTO> getAllDiagnosis() {
        List<DiseaseCategory> diseaseCategories = diseaseCategoryRepository.findAllByIsActiveTrueAndIsDeletedFalse();
        return Objects.nonNull(diseaseCategories)
                ? modelMapper.map(diseaseCategories, new TypeToken<List<DiseaseCategoryDTO>>() {
                }.getType())
                : new ArrayList<>();
    }

    /**
     * Sets all meta datas into redis.
     *
     * @param metas redistemplate
     * @return Map<String, List<MetaDataDTO>>
     */
    private Map<String, List<MetaDataDTO>> setNcdMetaDataCache(Map<String, List<MetaDataDTO>> metas) {
        List<Comorbidity> comorbidities = comorbidityRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_COMORBIDITIES, modelMapper.map(comorbidities, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<Complaints> complaints = complaintsRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_COMPLAINTS, modelMapper.map(complaints, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<Complication> complications = complicationRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_COMPLICATIONS, modelMapper.map(complications, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<CurrentMedication> currentMedications = currentMedicationRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_CURRENT_MEDICATION,
                modelMapper.map(currentMedications, new TypeToken<List<MetaDataDTO>>() {
                }.getType()));

        List<DosageForm> dosageForms = dosageFormRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_DOSAGE_FORM, modelMapper.map(dosageForms, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<DosageFrequency> dosageFrequencies = dosageFrequencyRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_DOSAGE_FREQUENCY,
                modelMapper.map(dosageFrequencies, new TypeToken<List<MetaDataDTO>>() {
                }.getType()));

        List<Frequency> frequencies = frequencyRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_FREQUENCY, modelMapper.map(frequencies, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<FrequencyType> frequencyTypes = frequencyTypeRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_FREQUENCY_TYPE, modelMapper.map(frequencyTypes, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<Lifestyle> lifestyles = lifestyleRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_LIFESTYLE, modelMapper.map(lifestyles, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<ModelQuestions> modelQuestions = modelQuestionsRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_MODEL_QUESTIONS, modelMapper.map(modelQuestions, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<NutritionLifestyle> nutritionLifestyles = nutritionLifestyleRepository
                .findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_NUTRITION_LIFESTYLE,
                modelMapper.map(nutritionLifestyles, new TypeToken<List<MetaDataDTO>>() {
                }.getType()));

        List<PhysicalExamination> physicalExaminations = physicalExaminationRepository
                .findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_PHYSICAL_EXAMINATION,
                modelMapper.map(physicalExaminations, new TypeToken<List<MetaDataDTO>>() {
                }.getType()));

        List<Reason> reasons = reasonRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_REASONS, modelMapper.map(reasons, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<Unit> units = unitRepository.findByNameNotLike(Constants.OTHER);
        metas.put(Constants.META_UNIT, modelMapper.map(units, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<RiskAlgorithm> riskAlgorithms = riskAlgorithmRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_RISK_ALGORITHM, modelMapper.map(riskAlgorithms, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<MedicalCompliance> medicalCompliances = medicalComplianceRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_MEDICAL_COMPLIANCES,
                modelMapper.map(medicalCompliances, new TypeToken<List<MetaDataDTO>>() {
                }.getType()));

        List<Symptom> symptoms = symptomRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_SYMPTOMS, modelMapper.map(symptoms, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));

        List<Diagnosis> diagnosis = diagnosisRepository.findByIsDeletedFalseAndIsActiveTrue();
        metas.put(Constants.META_DIAGNOSIS, modelMapper.map(diagnosis, new TypeToken<List<MetaDataDTO>>() {
        }.getType()));
        return metas;
    }

    /**
     * <p>
     * Gets mental health meta data for a particular country.
     * </p>
     *
     * @param countryId country ID of the user.
     * @return Collection on mental health meta data.
     */
    public List<Map<String, String>> getModelQuestions(Long countryId,
            List<String> clinicalWorkflows) {
        List<Map<String, String>> response = new ArrayList<>();
        Map<String, List<MetaDataDTO>> dataMap = setMetaDateCache();
        List<MetaDataDTO> modelQuestions = dataMap.get(Constants.META_MODEL_QUESTIONS);
        if (!Objects.isNull(modelQuestions) && !modelQuestions.isEmpty()) {
            modelQuestions = modelQuestions.stream()
                    .filter(question -> (countryId.equals(question.getCountryId())
                            && clinicalWorkflows.contains(question.getWorkflow())))
                    .sorted(Comparator.comparing(MetaDataDTO::getDisplayOrder))
                    .toList();
            if (Objects.isNull(modelQuestions) || modelQuestions.isEmpty()) {
                modelQuestions = dataMap.get(Constants.META_MODEL_QUESTIONS).stream()
                        .filter(MetaDataDTO::isDefault).toList();
            }
            Map<String, List<MetaDataDTO>> questions = new HashMap<>();
            for (MetaDataDTO question : modelQuestions) {
                if (!questions.containsKey(question.getType())) {
                    questions.put(question.getType(), new ArrayList<>());
                }
                questions.get(question.getType()).add(question);
            }

            ObjectMapper objectMapper = new ObjectMapper();
            for (List<MetaDataDTO> question : questions.values()) {
                try {
                    response.add(Map.of(Constants.TYPE, question.get(Constants.ZERO).getType(), Constants.QUESTIONS,
                            objectMapper.writeValueAsString(question)));
                } catch (JsonProcessingException e) {
                    Logger.logError(" Error while construct JSON object : " + e.getMessage());
                    throw new Validation(1001);
                }
            }
        }
        return response;
    }

    /**
     * Construct input JSON for the region customization.
     *
     * @param unselectedWorkflowNames Unselected workflows.
     * @param customization           Region customizations.
     */
    private void constructInputFormJSON(List<String> unselectedWorkflowNames, CountryCustomization customization) {
        if (!unselectedWorkflowNames.isEmpty()) {
            try {
                JSONObject json = new JSONObject(customization.getFormInput());
                JSONArray formLayoutArray = json.optJSONArray(Constants.FORM_LAYOUT);
                JSONArray updatedFormLayoutArray = new JSONArray();
                for (int i = 0; i < formLayoutArray.length(); i++) { // List<Map<String,>>
                    JSONObject explrObject = formLayoutArray.getJSONObject(i);
                    if (!(unselectedWorkflowNames.contains(explrObject.optString(Constants.ID, Constants.EMPTY))
                    || unselectedWorkflowNames.contains(explrObject.optString(Constants.FAMILY, Constants.EMPTY))
                    || unselectedWorkflowNames.contains(explrObject.optString(Constants.PARENT_FAMILY, Constants.EMPTY)))) {
                        updatedFormLayoutArray.put(explrObject);
                    }
                }
                json.put(Constants.FORM_LAYOUT, updatedFormLayoutArray);
                customization.setFormInput(json.toString());
            } catch (Exception e) {
                Logger.logError(" Error while construct JSON object : " + e.getMessage());
                throw new Validation(1001);
            }
        }
    }

    /**
     * <p>
     * Sets screening map based on cusotmization.
     * </p>
     *
     * @param assessment    - assessment map
     * @param enrollment    - enrollment map
     * @param screening     - screening map
     * @param customization - entity
     */
    private void setScreening(NCDFromDTO assessment, NCDFromDTO enrollment,
            NCDFromDTO screening, CountryCustomization customization) {
        if (customization.getType().equalsIgnoreCase(Constants.SCREENING)) {
            screening.setForms(customization);
        }
        if (customization.getType().equalsIgnoreCase(Constants.ENROLLMENT)) {
            enrollment.setForms(customization);
        }
        if (customization.getType().equalsIgnoreCase(Constants.ASSESSMENT)) {
            assessment.setForms(customization);
        }
    }

    /**
     * {@inheritDoc}
     */
    public MedicalReviewStaticDataDTO getNCDMedicalReviewStaticData() {
        Map<String, List<MetaDataDTO>> dataMap = setMetaDateCache();
        MedicalReviewStaticDataDTO response = new MedicalReviewStaticDataDTO();
        response.setComorbidity(dataMap.get(Constants.META_COMORBIDITIES));
        response.setComplaints(dataMap.get(Constants.META_COMPLAINTS));
        response.setCurrentMedication(dataMap.get(Constants.META_CURRENT_MEDICATION));
        response.setComplications(dataMap.get(Constants.META_COMPLICATIONS));
        response.setPhysicalExamination(dataMap.get(Constants.META_PHYSICAL_EXAMINATION));
        List<MetaDataDTO> lifestyles = dataMap.get(Constants.META_LIFESTYLE);
        response.setLifestyle(lifestyles);
        response.setFrequencies(dataMap.get(Constants.META_FREQUENCY));
        response.setFrequencyTypes(dataMap.get(Constants.META_FREQUENCY_TYPE));
        response.setNutritionLifestyles(dataMap.get(Constants.META_NUTRITION_LIFESTYLE));
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public boolean checkAppVersion(String appVersionReq) {
        return (!Objects.isNull(versionCheckExemptUsers) &&
                versionCheckExemptUsers.contains(UserContextHolder.getUserDto().getUsername())) ? Boolean.TRUE :
                (appVersion.equals(appVersionReq));
    }

    /**
     * {@inheritDoc}
     */
    public MetaFormDTO getMetaFormData(String form){
            FormMetaUi obj = formMetaService.getMetaForms(form);
            if (null == obj) {
                return null;
            }
            return new MetaFormDTO(obj.getId(), obj.getFormName(),
                    obj.getComponents());
    }

    /**
     * {@inheritDoc}
     */
    public MenuDTO getMenu(SearchRequestDTO searchRequestDTO) {
        if (Objects.nonNull(searchRequestDTO.getRoleName()) && !searchRequestDTO.getRoleName().isEmpty()) {
            return modelMapper.map(getMenuByRoleName(searchRequestDTO.getRoleName(), null,
                    searchRequestDTO.getAppTypes()), MenuDTO.class);
        }
        return new MenuDTO();
    }

    /**
     * <p>
     * Retrieves the menu for a given role name and country ID.
     * </p>
     *
     * @param roleName  {@link String} the name of the role for which the menu is to be retrieved
     * @param countryId {@link Long} the ID of the country for which the menu is to be retrieved
     * @return the menu corresponding to the given role name and country ID, or a default menu if no specific menu is found
     */
    private Menu getMenuByRoleName(String roleName, Long countryId, List<String> appTypes) {
        List<Menu> menus = menuRepository.getMenuByRole(roleName, countryId);
        return menus.stream()
                .findFirst()
                        .orElse(new Menu());
    }

    /**
     * {@inheritDoc}
     */
    public List<Symptom> getSymptoms() {
        return symptomRepository.findByIsDeletedFalseAndIsActiveTrue();
    }

    /**
     * {@inheritDoc}
     */
    public List<MetaDataDTO> getMessageMetaData() {
        return setMetaDateCache().get(Constants.META_MESSAGE);
    }


    /**
     * {@inheritDoc}
     */
    public Culture findCulture(String name) {
        return cultureRepository.findByNameIgnoreCase(name);
    }

    /**
     * {@inheritDoc}
     */
    public List<Frequency> getAllFrequencies() {
        return frequencyRepository.findByIsDeletedFalseAndIsActiveTrue();
    }

    /**
     * {@inheritDoc}
     */
    public List<MetaDataDTO> getLifestyles() {
        Map<String, List<MetaDataDTO>> dataMap = setMetaDateCache();
        return dataMap.get(Constants.META_LIFESTYLE);
    }

}
