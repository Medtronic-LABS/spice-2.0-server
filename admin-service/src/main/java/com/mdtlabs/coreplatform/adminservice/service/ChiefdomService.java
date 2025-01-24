package com.mdtlabs.coreplatform.adminservice.service;

import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * The ChiefdomService is a interface that defines CRUD related to to Chiefdom entity.
 * </p>
 *
 * @author Divya S
 */
public interface ChiefdomService {

    /**
     * <p>
     * This method is used to get a list of map containing district IDs with corresponding count of chiefdoms that
     * is searched using the given district IDs.
     * </p>
     *
     * @param districtIds {@link List} The list of district IDs associated with the chiefdoms that
     *                   are being searched is given
     * @return {@link Map} A map containing key as district IDs and value as count of chiefdoms
     * for the corresponding district IDs provided is returned
     */
    List<Map<String, Object>> getChiefdomCountByDistrictIds(List<Long> districtIds, Boolean isActive);

    /**
     * <p>
     * This method is used to activate or deactivate an chiefdom by its ID.
     * </p>
     *
     * @param countryId  {@link Long} The districtId used to get the district based on the country id
     * @param districtId  {@link Long} The accountId for which the chiefdom is being searched is given
     * @param isActive {@link Boolean} The boolean value indicating whether the chiefdom is active or not is given
     */
    List<Long> activateOrDeactivateChiefdoms(Long countryId, Long districtId, boolean isActive);

    /**
     * <p>
     * Gets Chiefdom count by country ids and its active status
     * </p>
     *
     * @param countryIds {@link List}     - Country ids for which the chiefdom count to be retrieved is given
     * @param isActive {@link Boolean} - Active status of the chiefdoms that need to be counted is given
     * @return List {@link List} - The chiefdom count based on country id is retrieved
     */
    List<Map<String, Object>> getChiefdomCountByCountryIds(List<Long> countryIds, Boolean isActive);

    /**
     * <p>
     * This method is used to get list of  chiefdoms and count as ResponseListDTO using the given request.
     * </p>
     *
     * @param requestDto {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of chiefdom DTOs is given
     * @return {@link ResponseListDTO} The retrieved list of chiefdom DTOs and total count for the
     * given search request is returned
     */
    public ResponseListDTO<ChiefdomDTO> getAllChiefdoms(SearchRequestDTO requestDto);

    /**
     * <p>
     * This method is used to get list of chiefdoms and count as ResponseListDTO using the given request.
     * </p>
     *
     * @param requestDto {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of chiefdom DTOs is given
     * @return {@link ResponseListDTO} The retrieved list of chiefdoms DTOs and total count for the
     * given search request is returned
     */
    public ResponseListDTO<ChiefdomDTO> getChiefdomList(ChiefdomRequestDTO requestDto);

    /**
     * <p>
     * This method is used to update a existing chiefdom using the chiefdom Dto.
     * </p>
     *
     * @param chiefdom {@link Chiefdom} The chiefdom dto that contains necessary information
     *                      to update chiefdom is given
     * @return {@link Chiefdom} The chiefdom is updated using the provided chiefdom
     * details and returned
     */
    public Chiefdom updateChiefdom(Chiefdom chiefdom);

    /**
     * <p>
     * This method is used to update a existing chiefdom using the chiefdom Dto.
     * </p>
     *
     * @param chiefdom {@link Chiefdom} The chiefdom dto that contains necessary information
     *                      to update chiefdom is given
     * @param villages {@link List} The villages that need to be added to the chiefdom is given
     * @return {@link Chiefdom} The chiefdom is updated using the provided chiefdom
     * details and returned
     */
    public Chiefdom createChiefdom(Chiefdom chiefdom, List<Village> villages);
    
    /**
     * <p>
     * This method is used to get list of chiefdom details based on given request
     * </p>
     *
     * @param searchRequestDTO {@link SearchRequestDTO} The search request contains necessary information
     *                   to get the list of chiefdom details is given
     * @return {@link ChiefdomDTO} The retrieved list of chiefdom details
     * for the given request is returned
     */
    public ChiefdomDTO getChiefdomDetails(SearchRequestDTO searchRequestDTO);
}
