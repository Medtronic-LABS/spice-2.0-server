package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.AdminConstants;
import com.mdtlabs.coreplatform.adminservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.model.dto.Code;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTestCustomization;
import com.mdtlabs.coreplatform.adminservice.repository.LabTestCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.service.LabTestCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

/**
 * <p>
 * This class implements the operations that can be performed on LabTestCustomization.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jun 20, 2024
 */
@Service
public class LabTestCustomizationServiceImpl implements LabTestCustomizationService {

    private final LabTestCustomizationRepository labTestCustomizationRepository;

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final ModelMapper mapper = new ModelMapper();

    public LabTestCustomizationServiceImpl(LabTestCustomizationRepository labTestCustomizationRepository, FhirServiceApiInterface fhirServiceApiInterface) {
        this.labTestCustomizationRepository = labTestCustomizationRepository;
        this.fhirServiceApiInterface = fhirServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public LabTestCustomizationDTO createLabTestCustomization(LabTestCustomizationDTO labTestCustomizationDTO) {
        if (Objects.nonNull(labTestCustomizationRepository
                .findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                        labTestCustomizationDTO.getTestName(), labTestCustomizationDTO.getCountryId()))) {
            throw new DataNotAcceptableException(12009, String.valueOf(labTestCustomizationDTO.getUniqueName()));
        }
        LabTestCustomization labTestCustomization = mapper.map(labTestCustomizationDTO, LabTestCustomization.class);
        labTestCustomization.setCodes(setCodes(labTestCustomizationDTO.getCodeDetails(), labTestCustomizationDTO.getTestName()));
        LabTestCustomizationDTO response = mapper.map(labTestCustomizationRepository.save(labTestCustomization), LabTestCustomizationDTO.class);
        response.setCodeDetails(setCodeDetails(labTestCustomization.getCodes()));
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public LabTestCustomizationDTO getLabTestCustomization(SearchRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getName())) {
            throw new DataNotFoundException(1011);
        }
        LabTestCustomizationDTO response = null;
        Long countryId = Objects.nonNull(requestDTO.getCountryId())
                ? requestDTO.getCountryId()
                : UserContextHolder.getUserDto().getCountry().getId();
        LabTestCustomization labTestCustomization =
                labTestCustomizationRepository.findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getName(), countryId);
        if (Objects.nonNull(labTestCustomization)) {
             response = mapper.map(labTestCustomization, LabTestCustomizationDTO.class);
            response.setCodeDetails(setCodeDetails(labTestCustomization.getCodes()));
        } else {
            response = new LabTestCustomizationDTO();
            response.setFormInput(String.format(AdminConstants.DEFAULT_FORM_INPUT, requestDTO.getName(), requestDTO.getName()));
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public LabTestCustomizationDTO getLabTestCustomizationByUniqueName(SearchRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getName())) {
            throw new DataNotFoundException(1004);
        }
        LabTestCustomizationDTO response = null;
        Long countryId = Objects.nonNull(requestDTO.getCountryId())
                ? requestDTO.getCountryId()
                : UserContextHolder.getUserDto().getCountry().getId();
        LabTestCustomization labTestCustomization =
                labTestCustomizationRepository.findByUniqueNameAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                        requestDTO.getName(), countryId);
        if (Objects.nonNull(labTestCustomization)) {
            response = mapper.map(labTestCustomization, LabTestCustomizationDTO.class);
            response.setCodeDetails(setCodeDetails(labTestCustomization.getCodes()));
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<LabTestCustomizationDTO> listLabTestCustomization(SearchRequestDTO requestDTO) {
        if (Objects.equals(requestDTO.getLimit(), Constants.ZERO)) {
            requestDTO.setLimit(10);
        }
        Long countryId = (Objects.nonNull(requestDTO.getCountryId()))
                ? requestDTO.getCountryId() : UserContextHolder.getUserDto().getCountry().getId();
        ResponseListDTO<LabTestCustomizationDTO> response = new ResponseListDTO<>();
        Pageable pageable = Pagination.setPagination(requestDTO.getSkip(),
                requestDTO.getLimit(),
                Constants.UPDATED_AT,
                Constants.BOOLEAN_FALSE);
        Page<LabTestCustomization> labTestCustomizations =
                labTestCustomizationRepository.findAllByName(requestDTO.getSearchTerm(), countryId, pageable);
        if (!Objects.isNull(labTestCustomizations) && !labTestCustomizations.isEmpty()) {
            List<LabTestCustomizationDTO> labTestCustomizationDTOS = new ArrayList<>();
            labTestCustomizations.stream().forEach(labTestCustomization -> {
                LabTestCustomizationDTO labTestCustomizationDTO = mapper.map(labTestCustomization, LabTestCustomizationDTO.class);
                labTestCustomizationDTO.setCodeDetails(setCodeDetails(labTestCustomization.getCodes()));
                labTestCustomizationDTOS.add(labTestCustomizationDTO);
            });
            response.setData(labTestCustomizationDTOS);
            response.setTotalCount(labTestCustomizations.getTotalElements());
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public LabTestCustomizationDTO updateLabTestCustomization(LabTestCustomizationDTO labTestCustomizationDTO) {
        if (Objects.isNull(labTestCustomizationDTO.getId())) {
            throw new DataNotFoundException(1002);
        }
        LabTestCustomization existingNamedLabTest = labTestCustomizationRepository
                .findByTestNameIgnoreCaseAndIdNotAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                        labTestCustomizationDTO.getTestName(), labTestCustomizationDTO.getId(),
                        labTestCustomizationDTO.getCountryId());
        if (Objects.nonNull(existingNamedLabTest)) {
            throw new DataConflictException(1012, existingNamedLabTest.getTestName());
        }
        LabTestCustomization existingLabTestCustomization =
                labTestCustomizationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(labTestCustomizationDTO.getId());
        if (Objects.isNull(existingLabTestCustomization)) {
            throw new DataNotAcceptableException(12009, String.valueOf(labTestCustomizationDTO.getId()));
        }
        LabTestCustomization labTestCustomization = mapper.map(labTestCustomizationDTO, LabTestCustomization.class);
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        labTestCustomization.setCodes(Objects.nonNull(labTestCustomizationDTO.getCodeDetails())
                ? setCodes(labTestCustomizationDTO.getCodeDetails(), labTestCustomizationDTO.getTestName())
                : existingLabTestCustomization.getCodes());
        mapper.map(labTestCustomization, existingLabTestCustomization);
        LabTestCustomizationDTO response = mapper.map(labTestCustomizationRepository.save(existingLabTestCustomization),
                LabTestCustomizationDTO.class);
        response.setCodeDetails(setCodeDetails(existingLabTestCustomization.getCodes()));
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public void deleteLabTestCustomization(SearchRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getId())) {
            throw new DataNotFoundException(1002);
        }
        LabTestCustomization labTestCustomization =
                labTestCustomizationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId());
        if (Objects.isNull(labTestCustomization)) {
            throw new DataNotFoundException(1010);
        }
        labTestCustomization.setDeleted(true);
        labTestCustomization.setActive(false);
        labTestCustomizationRepository.save(labTestCustomization);
    }

    /**
     *
     * To set Codes in labTestCustomization from labTestCustomization code details.
     *
     * @param codeDetails labTestCustomization code details.
     * @param name labTestCustomization name.
     * @return labTestCustomization code details.
     */
    private LabTestCustomization.Code setCodes(Code codeDetails, String name) {
        if (Objects.nonNull(codeDetails)) {
            LabTestCustomization.Code code = new LabTestCustomization.Code();
            code.setCode(codeDetails.getCode());
            code.setSystem(codeDetails.getUrl());
            code.setDisplay(name);
            return code;
        }
        return null;
    }

    /**
     *
     * To set code details in labTestCustomizationDTO from codeDetails of labTestCustomization.
     *
     * @param codes codeDetails of labTestCustomization.
     * @return codeDetails of labTestCustomization.
     */
    private Code setCodeDetails(LabTestCustomization.Code codes) {
        if (Objects.nonNull(codes)) {
            Code code = new Code();
            code.setCode(codes.getCode());
            code.setUrl(codes.getSystem());
            return code;
        }
        return null;
    }

    /**
     * To valiadte the name of the labTest.
     *
     * @param requestDTO request with the name of the labTest
     * @return boolean
     */
    public Boolean validateLabTestCustomization(SearchRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getName()) && Objects.isNull(requestDTO.getCountryId())) {
            throw new SpiceValidation(1011);
        }
        LabTestCustomization labTest = labTestCustomizationRepository
                .findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getName(), requestDTO.getCountryId());
        if (Objects.nonNull(labTest)) {
            throw new SpiceValidation(1012, labTest.getTestName());
        }
        return Boolean.TRUE;
    }

}
