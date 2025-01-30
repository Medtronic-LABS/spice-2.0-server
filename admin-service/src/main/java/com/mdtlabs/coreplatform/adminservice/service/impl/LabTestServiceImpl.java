package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;
import java.util.Objects;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTest;
import com.mdtlabs.coreplatform.adminservice.repository.LabTestRepository;
import com.mdtlabs.coreplatform.adminservice.service.LabTestService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

/**
 * <p>
 * This service class contain all the business logic for labTest module and
 * perform all the labTest operation here.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
@Service
public class LabTestServiceImpl implements LabTestService {

    private final LabTestRepository labTestRepository;

    private final ModelMapper mapper = new ModelMapper();

    public LabTestServiceImpl(LabTestRepository labTestRepository) {
        this.labTestRepository = labTestRepository;
    }

    /**
     * {@inheritDoc}
     */
    public LabTest createLabTest(LabTestDTO labTest) {
        if (Objects.isNull(labTest)) {
            Logger.logError(ErrorConstants.REQUEST_NOT_EMPTY);
            throw new BadRequestException(1003);
        }
        if (Objects.isNull(labTest.getTenantId())) {
            Logger.logError(ErrorConstants.TENANT_ID_NOT_NULL);
            throw new DataNotAcceptableException(1005);
        }
        validateLabTest(labTest);

        return labTestRepository.save(mapper.map(labTest, LabTest.class));
    }

    /**
     * {@inheritDoc}
     */
    public Boolean validateLabTest(LabTestDTO labTest) {
        LabTest labTestCountryDetail = labTestRepository.findByCountryIdAndNameAndIsDeletedAndTenantId(labTest.getCountryId(),
                labTest.getName(), false, labTest.getTenantId());
        if (!Objects.isNull(labTestCountryDetail) && (Objects.isNull(labTest.getId()) || !Objects.equals(labTest.getId(), labTestCountryDetail.getId()))) {
            Logger.logError(labTest.getName() + " already exist(s) in the regional database.");
            throw new DataConflictException(18000, labTest.getName());
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<LabTestDTO> getAllLabTests(SearchRequestDTO requestDto) {
        Page<LabTest> labTests;
        ResponseListDTO<LabTestDTO> response = new ResponseListDTO<>();
        String searchTerm = requestDto.getSearchTerm();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        Pageable pageable = Pagination.setPagination(requestDto.getSkip(), requestDto.getLimit(),
                Constants.UPDATED_AT, false);
        labTests = labTestRepository.getAllLabTests(searchTerm, requestDto.getCountryId(),
                requestDto.getTenantId(), pageable);

        if (!labTests.isEmpty()) {
            response.setData(mapper.map(labTests.stream().toList(), new TypeToken<List<LabTestDTO>>() {
            }.getType()));
            response.setTotalCount(labTests.getTotalElements());
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public boolean removeLabTest(SearchRequestDTO requestDto) {
        Logger.logDebug("Remove the labtest meta data for id - " + requestDto.getId());
        LabTest labTest = labTestRepository.findByIdAndIsDeletedAndTenantId(requestDto.getId(), Constants.BOOLEAN_FALSE,
                requestDto.getTenantId());
        if (Objects.isNull(labTest)) {
            Logger.logError(ErrorConstants.LABTEST_NOT_FOUND + requestDto.getId());
            throw new DataNotFoundException(18013, requestDto.getId().toString());
        }
        labTest.setDeleted(Constants.BOOLEAN_TRUE);
        labTest.setActive(Constants.BOOLEAN_FALSE);
        return !Objects.isNull(labTestRepository.save(labTest));
    }

    /**
     * {@inheritDoc}
     */
    public LabTest updateLabTest(LabTestDTO labTest) {
        if (Objects.isNull(labTest) || Objects.isNull(labTest.getId())) {
            Logger.logError(ErrorConstants.REQUEST_NOT_EMPTY);
            throw new BadRequestException(1003);
        }
        LabTest existingLabTest = getLabTestById(labTest.getId(), labTest.getTenantId());
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        mapper.map(labTest, existingLabTest);
        return labTestRepository.save(existingLabTest);
    }

    /**
     * {@inheritDoc}
     */
    public LabTest getLabTestById(SearchRequestDTO requestDto) {
        if (Objects.isNull(requestDto.getId()) || Objects.isNull(requestDto.getTenantId())) {
            Logger.logError("Invalid or Empty LabTest ID.");
            throw new DataNotAcceptableException(18009);
        }
        return getLabTestById(requestDto.getId(), requestDto.getTenantId());
    }

    private LabTest getLabTestById(Long id, Long tenantId) {
        LabTest labTest = labTestRepository.findByIdAndIsDeletedAndTenantId(id,
                Constants.BOOLEAN_FALSE, tenantId);
        if (Objects.isNull(labTest)) {
            Logger.logError(ErrorConstants.LABTEST_NOT_FOUND + id);
            throw new DataNotFoundException(18013, id.toString());
        }
        return labTest;
    }
}
