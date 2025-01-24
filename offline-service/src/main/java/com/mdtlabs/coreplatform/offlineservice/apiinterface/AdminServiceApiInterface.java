package com.mdtlabs.coreplatform.offlineservice.apiinterface;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.offlineservice.common.dto.HealthFacilityDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import java.util.List;

/**
 * <p>
 * AdminApiInterface is a admin service feign. The methods are used to perform various actions
 * such as creating Health facility, Updating Health facility, Deleting Health facility and Creating Country.
 * </p>
 *
 * @author Praveen created on Sep 26, 2024
 */
@FeignClient(name = "admin-service", url = "${app.admin-service}")
public interface AdminServiceApiInterface {

    /**
     * <p>
     * List facility based on organization ID.
     * This method takes an authorization token, cookie, client, and a RequestDTO as parameters
     * </p>
     *
     * @param authorization The authorization token of the user.
     * @param client        The client from which the request is made.
     * @param tenantIds     List of tenant Ids
     * @return A ResponseEntity containing the created VillageDTO.
     */
    @PostMapping("/healthfacility/tenants-list")
    List<HealthFacilityDTO> getFacilityByTenants(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                   @RequestHeader(Constants.CLIENT) String client,
                                                   @RequestBody List<Long> tenantIds);
}
