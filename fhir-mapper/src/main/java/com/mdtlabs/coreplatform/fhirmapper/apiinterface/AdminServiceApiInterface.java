package com.mdtlabs.coreplatform.fhirmapper.apiinterface;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicationDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

import java.util.List;

/**
 * Interface for admin-service communication.
 * <p>
 * This interface facilitates the interaction with the admin-service, specifically for operations
 * related to health facilities. It defines methods for fetching health facility details based on
 * FHIR ID. The {@link FeignClient} annotation is used to specify the service name and URL, which
 * are dynamically resolved from the application's configuration.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jul 18, 2024
 */
@FeignClient(name = "admin-service", url = "${app.admin-service}")
public interface AdminServiceApiInterface {

    /**
     * Fetches health facility details by FHIR ID.
     * <p>
     * Sends a POST request to the admin-service to retrieve details of a health facility identified
     * by its FHIR ID. Requires authorization and client headers for authentication and context.
     * </p>
     *
     * @param authorization The authorization token.
     * @param client        The client identifier.
     * @param requestDTO    The request containing the FHIR ID.
     * @return The {@link HealthFacilityRequestDTO} containing health facility details.
     */
    @PostMapping("/healthfacility/details-by-fhir-id")
    HealthFacilityRequestDTO getHealthFacilityByFhirId(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                       @RequestHeader(Constants.CLIENT) String client,
                                                       @RequestBody RequestDTO requestDTO);

    /**
     * <p>
     *  Sends a POST request to the admin-service to retrieve details of a labTest Customization details
     *  by its name. Requires authorization and client headers for authentication and context.
     * </p>
     *
     * @param authorization The authorization token
     * @param client The client identifier
     * @param requestDTO The request containing the name.
     * @return The {@link LabTestCustomizationDTO} containing labTestCustomization details.
     */
    @PostMapping("lab-test-customization/search")
    LabTestCustomizationDTO getLabTestCustomizationByName(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                          @RequestHeader(Constants.CLIENT) String client,
                                                          @RequestBody SearchRequestDTO requestDTO);

    /**
     * <p>
     *     This method used to get village sequence details based on the villageId.
     * </p>
     *
     * @param authorization The authorization token
     * @param client The client identifier
     * @param requestDTO The request containing villageId.
     * @return Long village member sequence details.
     */
    @PostMapping("/village/member-sequence")
    Long getMemberSequenceByVillageId(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                            @RequestHeader(Constants.CLIENT) String client,
                                            @RequestBody SearchRequestDTO requestDTO);

    /**
     * <p>
     *     This method used to get village household sequence details based on the villageId.
     * </p>
     *
     * @param authorization The authorization token
     * @param client The client identifier
     * @param requestDTO The request containing village id.
     * @return Long village household sequence details.
     */
    @PostMapping("/village/household-sequence")
    Long getHouseholdSequenceByVillageId(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                               @RequestHeader(Constants.CLIENT) String client,
                               @RequestBody SearchRequestDTO requestDTO);

    /**
     * <p>
     *     This method used to get village details based on the villageId.
     * </p>
     *
     * @param authorization The authorization token
     * @param client The client identifier
     * @param requestDTO The request containing village id.
     * @return Long village household sequence details.
     */
    @PostMapping("/village")
    VillageDTO getVillageDetailsByVillageId(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                         @RequestHeader(Constants.CLIENT) String client,
                                         @RequestBody SearchRequestDTO requestDTO);

    /**
     * <p>
     *  Sends a POST request to the admin-service to retrieve details of a medication Customization details
     *  by its id. Requires authorization and client headers for authentication and context.
     * </p>
     *
     * @param authorization The authorization token
     * @param client The client identifier
     * @param ids The request containing the medication ids.
     * @return The {@link LabTestCustomizationDTO} containing labTestCustomization details.
     */
    @PostMapping("medication/search-by-ids")
    List<MedicationDTO> getAllMedicationByIds(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                              @RequestHeader(Constants.CLIENT) String client,
                                              @RequestBody List<Long> ids);

}
