package com.mdtlabs.coreplatform.userservice.apiinterface;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;

/**
 * <p>
 * AdminApiInterface is a admin service feign. The methods are used to perform various actions
 * such as creating Health facility, Updating Health facility, Deleting Health facility and Creating Country.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
@FeignClient(name = "admin-service", url = "${app.admin-service}")
public interface AdminServiceApiInterface {

    /**
     * <p>
     * Creates a health facility.
     * This method takes an authorization token, cookie, client, and a HealthFacilityRequestDTO as parameters and
     * creates a health facility.
     * </p>
     *
     * @param authorization The authorization token of the user.
     * @param cookie        The authorization cookie of the user.
     * @param client        The client from which the request is made.
     * @param request       The HealthFacilityRequestDTO containing the details of the health facility to be created.
     * @return A ResponseEntity containing the created HealthFacilityRequestDTO.
     */
    @PostMapping("/healthfacility/create")
    ResponseEntity<HealthFacility> createHealthFacility(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                        @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                        @RequestHeader(Constants.CLIENT) String client,
                                                        @RequestBody HealthFacilityRequestDTO request);

    /**
     * <p>
     * Creates a Country.
     * This method takes an authorization token, cookie, client, and a CountryRequestDTO as parameters and creates a country.
     * </p>
     *
     * @param authorization The authorization token of the user.
     * @param cookie        The authorization cookie of the user.
     * @param client        The client from which the request is made.
     * @param request       The CountryRequestDTO containing the details of the country to be created.
     * @return A ResponseEntity containing the created Country.
     */
    @PostMapping("/country/create")
    ResponseEntity<Country> createCountry(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                          @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                          @RequestHeader(Constants.CLIENT) String client,
                                          @RequestBody CountryRequestDTO request);

    /**
     * <p>
     * Updates a HealthFacility.
     * This method takes an authorization token, cookie, client, and a HealthFacilityRequestDTO as parameters and
     * updates a health facility.
     * </p>
     *
     * @param authorization The authorization token of the user.
     * @param cookie        The authorization cookie of the user.
     * @param client        The client from which the request is made.
     * @param request       The HealthFacilityRequestDTO containing the updated details of the health facility.
     * @return A ResponseEntity containing the updated HealthFacility.
     */
    @PutMapping("/healthfacility/update")
    ResponseEntity<HealthFacility> updateHealthFacility(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                        @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                        @RequestHeader(Constants.CLIENT) String client,
                                                        @RequestBody HealthFacilityRequestDTO request);

    /**
     * <p>
     * Deletes a HealthFacility.
     * This method takes an authorization token, cookie, client, and a SearchRequestDTO as parameters and deletes a health
     * facility.
     * </p>
     *
     * @param authorization The authorization token of the user.
     * @param cookie        The authorization cookie of the user.
     * @param client        The client from which the request is made.
     * @param request       The SearchRequestDTO containing the details of the health facility to be deleted.
     * @return A ResponseEntity containing a message about the operation result.
     */
    @PostMapping("/healthfacility/delete")
    ResponseEntity<String> deleteHealthFacility(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                                @RequestHeader(value = Constants.COOKIE_HEADER, required = false) String cookie,
                                                @RequestHeader(Constants.CLIENT) String client,
                                                @RequestBody SearchRequestDTO request);

    /**
     * <p>
     * This method is used to create a new district with the given district details and authorization and
     * tenant id headers.
     * </p>
     *
     * @param authorization {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                      authenticate the user making the request
     * @param client        {@link String} The client name used to authenticate the user based on the type of user(Mobile or Admin)
     * @param districtRequest {@link DistrictRequestDTO} The data required to create a new district is given.
     * @return {@link District} The created district is returned
     */
    @PostMapping("/district/create")
    ResponseEntity<District> createDistrict(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                        @RequestHeader(Constants.CLIENT) String client,
                                        @RequestBody DistrictRequestDTO districtRequest);

    /**
     * <p>
     * This method is used to create a new operating unit with the provided data and authorization token.
     * </p>
     *
     * @param authorization       {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                            authenticate the user making the request
     * @param client              {@link String} The client name used to authenticate the user based on the type of user(Mobile or Admin)
     * @param chiefdomRequestDTO {@link ChiefdomRequestDTO}  The data required to create a new operating unit is given.
     * @return {@link Chiefdom} The created operating unit is returned
     */
    @PostMapping("/chiefdom/create")
    ResponseEntity<Chiefdom> createChiefdom(@RequestHeader(Constants.AUTHORIZATION) String authorization,
                                              @RequestHeader(Constants.CLIENT) String client,
                                              @RequestBody ChiefdomRequestDTO chiefdomRequestDTO);
}
