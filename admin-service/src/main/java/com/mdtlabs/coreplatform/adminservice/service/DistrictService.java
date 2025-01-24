package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;

/**
 * <p>
 * DistrictService is defining an interface called "DistrictService" which contains several
 * methods for managing user districts.
 * </p>
 *
 * @author Gopinath created on July 15, 2024
 */
public interface DistrictService {

    /**
     * <p>
     * This method is used to retrieve a list of districts based on a search request.
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                         to get the list of districts is given
     * @return {@link ResponseListDTO} The response list DTO containing retrieved list of districts
     * and total count is returned
     */
    public ResponseListDTO<DistrictDTO> getDistricts(SearchRequestDTO searchRequestDto);

    /**
     * <p>
     * This method is used to get list of district details with child organization counts.
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of districts is given
     * @return {@link ResponseListDTO} The response list DTO containing retrieved list of districts
     * and total count is returned
     */
    public ResponseListDTO<DistrictListDTO> getDistrictList(SearchRequestDTO searchRequestDto);

    /**
     * <p>
     * This method is used to get list of district details based on given request
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                         to get the list of district details is given
     * @return {@link SearchRequestDTO} The retrieved list of district details for given request is returned
     */
    public DistrictDTO getDistrictDetails(SearchRequestDTO searchRequestDto);

    /**
     * <p>
     * This method is used to create a new district using the accountWorkflowDto data.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The district workflow dto that contains
     *                           necessary information to create district is given
     * @return {@link District} The district which is created for given district details is returned
     */
    public District createDistrict(DistrictRequestDTO districtRequestDTO);

    /**
     * <p>
     * This method is used to update a existing district using the accountWorkflowDto data.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The district workflow dto that contains
     *                           necessary information to update district is given
     * @return {@link District} The district which is updated for given district details is returned
     */
    public District updateDistrict(DistrictRequestDTO districtRequestDTO);

    /**
     * <p>
     * This method is used to activate an district based on the provided request DTO.
     * </p>
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The request contains necessary information to activate or
     *                   deactivate the list of districts is given
     * @return {@link Boolean} A boolean indicating whether the district is activated or Deactivated is returned
     */
    public Boolean activateOrDeactivateDistrict(DistrictRequestDTO districtRequestDTO);

    /**
     * <p>
     * This method is used to retrieve a list of deactivated districts based on a search request.
     * </p>
     *
     * @param searchRequestDto {@link SearchRequestDTO} The search request contains necessary information
     *                         to get the list of districts is given
     * @return {@link ResponseListDTO} The response list DTO containing retrieved list of districts
     * and total count is returned
     */
    public ResponseListDTO<DistrictDTO> getDeactivatedDistricts(SearchRequestDTO searchRequestDto);


    /**
     * <p>
     * Gets District count by country ids and its active status
     * </p>
     *
     * @param ids      {@link List}     - Country ids for which the district count to be retrieved is given
     * @param isActive {@link Boolean} - Active status of the districts that need to be counted is given
     * @return List {@link List} - The district count based on country id is retrieved
     */
    List<Map<String, Object>> getDistrictCountByCountryIds(List<Long> ids, Boolean isActive);
}
