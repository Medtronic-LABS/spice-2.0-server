package com.mdtlabs.coreplatform.fhirmapper.apiinterface;

import java.util.List;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;

import feign.FeignException;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FrequencyDTO;

/**
 * Interface for spice-service communication.
 * <p>
 * This interface facilitates the interaction with the spice-service, specifically for operations
 * related to health facilities. It defines methods for fetching health facility details based on
 * FHIR ID. The {@link FeignClient} annotation is used to specify the service name and URL, which
 * are dynamically resolved from the application's configuration.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jul 18, 2024
 */
@FeignClient(name = "spice-service", url = "${app.spice-service}")
public interface SpiceServiceApiInterface {

    @PostMapping("/static-data/frequencies")
    List<FrequencyDTO> getFrequencies(@RequestHeader(Constants.AUTHORIZATION) String authorization,
            @RequestHeader(Constants.CLIENT) String client);

    /**
     * Creates a Member Link notification to map with household
     *
     * @param token auth token
     * @param client auth client
     * @param householdMemberDTO HouseholdMemberDTO DTO
     * @throws FeignException
     */
    @PostMapping("/household-member-link/create")
    public void createHouseholdMemberLink(@RequestHeader("Authorization") String token,
            @RequestHeader("client") String client, @RequestBody HouseholdMemberDTO householdMemberDTO)
            throws FeignException;

}
