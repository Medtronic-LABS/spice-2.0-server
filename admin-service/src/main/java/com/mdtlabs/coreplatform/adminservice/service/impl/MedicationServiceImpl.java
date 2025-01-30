package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.mdtlabs.coreplatform.adminservice.model.dto.ClassificationDTO;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.model.dto.Code;
import com.mdtlabs.coreplatform.adminservice.model.dto.CategoryDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.Category;
import com.mdtlabs.coreplatform.adminservice.model.dto.MedicationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.DosageForm;
import com.mdtlabs.coreplatform.adminservice.model.entity.Medication;
import com.mdtlabs.coreplatform.adminservice.repository.ClassificationRepository;
import com.mdtlabs.coreplatform.adminservice.repository.DosageFormRepository;
import com.mdtlabs.coreplatform.adminservice.repository.MedicationRepository;
import com.mdtlabs.coreplatform.adminservice.service.MedicationService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

/**
 * <p>
 * This service class contain all the business logic for medication module and
 * perform all the medication operation here.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
@Service
public class MedicationServiceImpl implements MedicationService {

    private final MedicationRepository medicationRepository;

    private final ClassificationRepository classificationRepository;

    private final DosageFormRepository dosageFormRepository;

    private final ModelMapper mapper = new ModelMapper();

    public MedicationServiceImpl(MedicationRepository medicationRepository, ClassificationRepository classificationRepository, DosageFormRepository dosageFormRepository) {
        this.medicationRepository = medicationRepository;
        this.classificationRepository = classificationRepository;
        this.dosageFormRepository = dosageFormRepository;
    }

    /**
     * {@inheritDoc}
     */
    public List<Medication> createMedication(List<MedicationDTO> request) {
        Set<MedicationDTO> medicationSet = new HashSet<>(request);
        if (request.size() != medicationSet.size()) {
            throw new DataNotAcceptableException(12013);
        }
        List<String> medicationNames = new ArrayList<>();
        medicationSet.forEach(medication -> {
            try {
                validateMedication(medication);
            } catch (DataNotAcceptableException | DataConflictException e) {
                medicationNames.add(medication.getName());
                Logger.logError(e.getMessage(), e);
            }
        });
        if (Constants.ZERO < medicationNames.size()) {
            String failedMedications = String.join(",", medicationNames);
            throw new DataConflictException(12009, failedMedications);
        }
        List<Medication> medications = convertMedicationDTOToMedication(request);
        return medicationRepository.saveAll(medications);
    }

    /**
     * {@inheritDoc}
     */
    public List<MedicationDTO> getMedicationsByIds(List<Long> ids) {
        List<Medication> medications = medicationRepository.getAllMedicationsByIds(ids);
        return this.convertMedicationToMedicationDTO(medications);
    }

    /**
     * {@inheritDoc}
     */
    public Medication updateMedication(MedicationDTO medication) {
        validateMedication(medication);
        Medication existingMedication = medicationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(
                medication.getId());

        if (Objects.isNull(existingMedication)) {
            throw new DataNotFoundException(12008);
        }
        if (!(existingMedication.getName()).equals(medication.getName())) {
            throw new DataNotAcceptableException(12015);
        }
        if (!Objects.equals((existingMedication.getCountryId()), (medication.getCountryId()))) {
            throw new DataNotAcceptableException(12010);
        }
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        return medicationRepository.save(convertMedication(medication));
    }

    /**
     * {@inheritDoc }
     */
    public Boolean validateMedication(MedicationDTO medication) {
        if (Objects.isNull(medication.getClassificationId()) && Objects.isNull(medication.getBrandId())
                && Objects.isNull(medication.getDosageFormId()) && Objects.isNull(medication.getName())
                && Objects.isNull(medication.getCountryId()) && Objects.isNull(medication.getCodeDetails())) {
            throw new DataNotAcceptableException(12014);
        }
        Medication medicationCountryDetail = medicationRepository.getMedicationByMandatoryFields(
                medication.getClassificationId(), medication.getBrandId(), medication.getDosageFormId(),
                medication.getCountryId(), medication.getName());

        if (!Objects.isNull(medicationCountryDetail)
                && !Objects.equals(medication.getId(), medicationCountryDetail.getId())) {
            throw new DataConflictException(12009, medication.getName());
        }
        return Objects.isNull(medicationCountryDetail)
                || (Objects.equals(medication.getId(), medicationCountryDetail.getId()));
    }

    /**
     * {@inheritDoc}
     */
    public Medication getMedicationById(SearchRequestDTO requestDto) {
        if (Objects.isNull(requestDto.getId())) {
            throw new DataNotAcceptableException(12012);
        }
        Medication medication = medicationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDto.getId());
        if (Objects.isNull(medication)) {
            throw new DataNotFoundException(12008);
        }
        return medication;
    }

    /**
     * {@inheritDoc}
     */
    public Medication getMedicationByName(SearchRequestDTO requestDto) {
        if (StringUtils.isEmpty(requestDto.getName())) {
            throw new DataNotAcceptableException(12019);
        }
        Medication medication = medicationRepository.findByNameAndIsDeletedFalseAndIsActiveTrue(requestDto.getName());
        if (Objects.isNull(medication)) {
            throw new DataNotFoundException(12016);
        }
        return medication;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<MedicationDTO> getAllMedications(SearchRequestDTO requestObject) {
        ResponseListDTO<MedicationDTO> response = new ResponseListDTO<>();
        if (Objects.isNull(requestObject.getCountryId())) {
            throw new DataNotAcceptableException(1001);
        }
        String searchTerm = requestObject.getSearchTerm();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.REGEX_SEARCH_PATTERN)) {
            return response;
        }
        Pageable pageable = Pagination.setPagination(requestObject.getSkip(), requestObject.getLimit(), Constants.UPDATED_AT,
                false);
        Page<Medication> medications = medicationRepository.getAllMedications(searchTerm,
                requestObject.getCountryId(), pageable);
        if (!Objects.isNull(medications) && !medications.isEmpty()) {
            response = new ResponseListDTO<>(convertMedicationToMedicationDTO(medications.stream().toList()), medications.getTotalElements());
        }

        return response;
    }

    /**
     * {@inheritDoc}
     */
    public Boolean deleteMedicationById(SearchRequestDTO requestDto) {
        Medication medication = getMedicationById(requestDto);
        medication.setDeleted(Constants.BOOLEAN_TRUE);
        medication.setActive(Constants.BOOLEAN_FALSE);
        return (null != medicationRepository.save(medication));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Medication getOtherMedication(long countryId) {
        return medicationRepository.getOtherMedication(countryId, Constants.OTHER,
                Constants.OTHER, Constants.OTHER, Constants.OTHER);
    }

    /**
     * {@inheritDoc}
     */
    public List<ClassificationDTO> getClassifications(Long countryId) {
        return mapper.map(classificationRepository.getClassifications(countryId),
                new TypeToken<List<ClassificationDTO>>() {}.getType());
    }

    /**
     * {@inheritDoc}
     */
    public List<DosageForm> getDosageForms() {
        return dosageFormRepository.findAllByIsActiveTrueAndIsDeletedFalse();
    }

    /**
     * {@inheritDoc }
     */
    @Override
    public ResponseListDTO<MedicationDTO> searchMedicationByName(SearchRequestDTO requestDTO) {
        String searchTerm = requestDTO.getSearchTerm();
        if (Objects.isNull(searchTerm) || searchTerm.isEmpty()) {
            throw new DataNotAcceptableException(18008);
        }
        List<Medication> medications = medicationRepository.searchMedications(searchTerm, UserContextHolder.getUserDto().getCountry().getId());
        List<MedicationDTO> medicationDTOList = convertMedicationToMedicationDTO(medications);
        return new ResponseListDTO<>(medicationDTOList, Long.valueOf(medicationDTOList.size()));
    }

    /**
     * Converts a list of {@link MedicationDTO} objects into a list of {@link Medication} entities.
     * <p>
     * This method iterates over each {@link MedicationDTO} in the provided list, converting each one into a
     * {@link Medication} entity by calling the {@code convertMedication} method. The converted {@link Medication}
     * entities are then collected into a list that is returned.
     * </p>
     *
     * @param medicationDTOs A list of {@link MedicationDTO} objects to be converted.
     * @return A list of {@link Medication} entities corresponding to the input list of {@link MedicationDTO} objects.
     */
    private List<Medication> convertMedicationDTOToMedication(List<MedicationDTO> medicationDTOs) {
        List<Medication> medications = new ArrayList<>();
        medicationDTOs.forEach(medicationDTO ->
            medications.add(convertMedication(medicationDTO)));
        return medications;
    }

    /**
     * Converts a single {@link MedicationDTO} object into a {@link Medication} entity.
     * <p>
     * This method maps the properties of a {@link MedicationDTO} object to a new {@link Medication} entity.
     * It includes direct mappings for simple properties and also handles the conversion of complex properties
     * such as {@code codes}, which is converted by calling the {@code setCodes} method.
     * </p>
     *
     * @param medicationDTO The {@link MedicationDTO} object to be converted.
     * @return A {@link Medication} entity with properties populated from the provided {@link MedicationDTO} object.
     */
    private Medication convertMedication(MedicationDTO medicationDTO) {
        Medication medication = new Medication();
        medication.setId(medicationDTO.getId());
        medication.setBrandId(medicationDTO.getBrandId());
        medication.setClassificationId(medicationDTO.getClassificationId());
        medication.setName(medicationDTO.getName());
        medication.setDosageFormId(medicationDTO.getDosageFormId());
        medication.setCountryId(medicationDTO.getCountryId());
        medication.setDosageFormName(medicationDTO.getDosageFormName());
        medication.setBrandName(medicationDTO.getBrandName());
        medication.setClassificationName(medicationDTO.getClassificationName());
        medication.setCodes(setCodes(medicationDTO.getCodeDetails(), medicationDTO.getName()));
        if (Objects.nonNull(medicationDTO.getCategory())) {
            Category category = new Category();
            category.setId(medicationDTO.getCategory().getId());
            category.setName(medicationDTO.getCategory().getName());
            medication.setCategory(category);
        }
        return medication;
    }

    /**
     * Splits a given code detail's code string into individual codes, creates a list of {@link Medication.Code} objects,
     * and sets the system URL and display name for each code.
     * <p>
     * This method takes a {@link Code} object, which contains a comma-separated string of codes and a URL,
     * and a medication name. It splits the string into individual codes, then for each code, it creates a new
     * {@link Medication.Code} object, sets the code, system URL, and display name, and adds it to a list.
     * </p>
     *
     * @param codeDetails The {@link Code} object containing the comma-separated codes and the system URL.
     * @param name        The name of the medication to set as the display name for each code.
     * @return A list of {@link Medication.Code} objects with the code, system URL, and display name set.
     */
    private List<Medication.Code> setCodes(Code codeDetails, String name) {
        List<Medication.Code> codeList = new ArrayList<>();
        List<String> codes = Arrays.asList(codeDetails.getCode().split(","));
        codes.stream().forEach(code -> {
            Medication.Code medicationCode = new Medication.Code();
            medicationCode.setCode(code);
            medicationCode.setSystem(codeDetails.getUrl());
            medicationCode.setDisplay(name);
            codeList.add(medicationCode);
        });
        return codeList;
    }

    /**
     * Converts a list of {@link Medication} entities into a list of {@link MedicationDTO} objects.
     * <p>
     * This method iterates over each {@link Medication} entity in the provided list, creating a new {@link MedicationDTO}
     * object for each, and populating it with the corresponding properties from the {@link Medication} entity. It also
     * converts the {@link Medication.Code} objects associated with each {@link Medication} into a {@link Code}
     * object and sets it in the {@link MedicationDTO}. The resulting list of {@link MedicationDTO}s is then returned.
     * </p>
     *
     * @param medications A list of {@link Medication} entities to be converted.
     * @return A list of {@link MedicationDTO}s corresponding to the input list of {@link Medication} entities.
     */
    private List<MedicationDTO> convertMedicationToMedicationDTO(List<Medication> medications) {
        List<MedicationDTO> medicationDTOs = new ArrayList<>();
        medications.stream().forEach(medication -> {
            MedicationDTO medicationDTO = new MedicationDTO();
            medicationDTO.setId(medication.getId());
            medicationDTO.setBrandId(medication.getBrandId());
            medicationDTO.setClassificationId(medication.getClassificationId());
            medicationDTO.setName(medication.getName());
            medicationDTO.setDosageFormId(medication.getDosageFormId());
            medicationDTO.setCountryId(medication.getCountryId());
            medicationDTO.setDosageFormName(medication.getDosageFormName());
            medicationDTO.setBrandName(medication.getBrandName());
            medicationDTO.setClassificationName(medication.getClassificationName());
            medicationDTO.setCodeDetails((Objects.nonNull(medication.getCodes()) && !medication.getCodes().isEmpty())
                    ? setMedicationCodeDetails(medication.getCodes()) : null);
            if (Objects.nonNull(medication.getCategory())) {
                CategoryDTO categoryDTO = new CategoryDTO();
                categoryDTO.setId(medication.getCategory().getId());
                categoryDTO.setName(medication.getCategory().getName());
                categoryDTO.setDisplayOrder(medication.getCategory().getDisplayOrder());
                medicationDTO.setCategory(categoryDTO);
            }
            medicationDTOs.add(medicationDTO);
        });
        return medicationDTOs;
    }

    /**
     * Sets the medication code details for a given list of medication codes.
     * <p>
     * This method aggregates all codes from a list of {@link Medication.Code} objects into a single {@link Code} object.
     * It concatenates all codes into a comma-separated string and sets the system URL from the first code in the list, assuming all codes
     * share the same system URL. This method is useful for preparing medication code information in a format suitable for data transfer objects.
     * </p>
     *
     * @param codes A list of {@link Code} objects representing the codes to be aggregated.
     * @return A {@link Code} object containing the aggregated code details.
     */
    private Code setMedicationCodeDetails(List<Medication.Code> codes) {
        Code codeDetails = new Code();
        codeDetails.setCode(codes.stream()
                .map(Medication.Code::getCode)
                .collect(Collectors.joining(", ")));
        codeDetails.setUrl(codes.get(0).getSystem());
        return codeDetails;
    }
}
