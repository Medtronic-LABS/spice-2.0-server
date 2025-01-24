package com.mdtlabs.coreplatform.offlineservice.apiinterface;

import java.util.List;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.offlineservice.common.dto.AncResultDTO;
import com.mdtlabs.coreplatform.offlineservice.common.dto.RequestDTO;


/**
 * <p>
 * This interface is used to access cql service APIs.
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024
 */
@FeignClient(name = "cql-service", url = "${app.cql-service}")
public interface CqlApiInterface {

    /**
     * <p>
     * This method is used to retrieve the ANC results for patients based on the request data provided.
     * It is a POST mapping which accepts a RequestDTO as a request body and two headers: token and client.
     * </p>
     *
     * @param token The authorization token for the request.
     * @param client The client information for the request.
     * @param requestDTO The request body containing the data required to retrieve the ANC results.
     * @return A list of AncResultDTOs representing the ANC results for the specified conditions.
     */
    @PostMapping("/cql/anc-result/list")
    List<AncResultDTO> getAncResultByVillages(
            @RequestHeader(Constants.AUTHORIZATION_HEADER) String token,
            @RequestHeader(Constants.HEADER_CLIENT) String client,
            @RequestBody RequestDTO requestDTO);
}
