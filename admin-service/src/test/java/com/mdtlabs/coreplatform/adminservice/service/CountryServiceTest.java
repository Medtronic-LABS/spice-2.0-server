package com.mdtlabs.coreplatform.adminservice.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.repository.CountryRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.CountryServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;

@ExtendWith(MockitoExtension.class)
class CountryServiceTest {

    @InjectMocks
    CountryServiceImpl countryService;

    @Mock
    CountryRepository countryRepository;

    @Mock
    UserServiceApiInterface userServiceApiInterface;

    @Test
    void createCountry() {
        CountryRequestDTO requestDTO = TestDataProvider.getCountryRequestDTO();
        List<Country> countries = new ArrayList<>();
        when(countryRepository.findByPhoneNumberCodeOrNameIgnoreCaseAndIsActiveTrueAndIsDeletedFalse(requestDTO.getPhoneNumberCode(), requestDTO.getName().strip())).thenReturn(countries);

        Country response = countryService.createCountry(requestDTO);
        assertNotNull(response);
        countries = List.of(TestDataProvider.getCountry());
        when(countryRepository.findByPhoneNumberCodeOrNameIgnoreCaseAndIsActiveTrueAndIsDeletedFalse(requestDTO.getPhoneNumberCode(), requestDTO.getName().strip())).thenReturn(countries);
        assertThrows(DataConflictException.class, () -> countryService.createCountry(requestDTO));
    }

    @Test 
    void addRegionAdmin() {
        UserRequestDTO requestDTO = TestDataProvider.getUserRequestDTO();
        ResponseEntity<UserResponseDTO> responseEntity = new ResponseEntity<>(new UserResponseDTO(), HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userServiceApiInterface.addAdmin(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), requestDTO)).thenReturn(responseEntity);

        UserResponseDTO responseDTO = countryService.addRegionAdmin(requestDTO);
        assertNotNull(responseDTO);
        TestDataProvider.cleanUp();
    }

    @Test 
    void updateRegionAdmin() {
        UserRequestDTO requestDTO = TestDataProvider.getUserRequestDTO();
        ResponseEntity<UserResponseDTO> responseEntity = new ResponseEntity<>(new UserResponseDTO(), HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userServiceApiInterface.updateAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(),
                requestDTO)).thenReturn(responseEntity);

        UserResponseDTO responseDTO =countryService.updateRegionAdmin(requestDTO);
        assertNotNull(responseDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void testDeleteAdmin () {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        ResponseEntity<UserResponseDTO> responseEntity = new ResponseEntity<>(new UserResponseDTO(), HttpStatus.CREATED);
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        when(userServiceApiInterface.removeAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(),
                requestDTO)).thenReturn(responseEntity);

        UserResponseDTO responseDTO =countryService.removeAdmin(requestDTO);
        assertNotNull(responseDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void getCountryDetails() {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        Country country = TestDataProvider.getCountry();
        when(countryRepository.getCountryById(requestDTO.getId(), requestDTO.getTenantId(), false, true)).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> countryService.getCountryDetails(requestDTO));

        when(countryRepository.getCountryById(requestDTO.getId(), requestDTO.getTenantId(), false, true)).thenReturn(country);
        CountryDTO response = countryService.getCountryDetails(requestDTO);
        assertNotNull(response);

        requestDTO.setId(null);
        assertThrows(BadRequestException.class, () -> countryService.getCountryDetails(requestDTO));
        requestDTO.setId(TestConstants.ONE);
        requestDTO.setTenantId(null);
        assertThrows(DataNotFoundException.class, () -> countryService.getCountryDetails(requestDTO));
    }

    @Test
    void updateCountry() {
        CountryRequestDTO request = TestDataProvider.getCountryRequestDTO();
        Country country = TestDataProvider.getCountry();

        when(countryRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getId())).thenReturn(null);
        assertThrows(DataNotFoundException.class, () -> countryService.updateCountry(request));

        when(countryRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getId())).thenReturn(country);
        when(countryRepository.save(country)).thenReturn(country);

        Country response = countryService.updateCountry(request);
        assertNotNull(response);
    }

    @Test
    void getCountries() {
        //given
        Page<Country> countries = new PageImpl<>(List.of(TestDataProvider.getCountry()));
        Pageable pageable = mock(Pageable.class);

        //when
        when(countryRepository.searchCountries(Constants.SEARCH_TERM, pageable)).thenReturn(countries);

        //then
        Page<Country> response = countryService.getCountries(Constants.SEARCH_TERM, pageable);
        assertNotNull(response);
        assertEquals(countries, response);
    }
}
