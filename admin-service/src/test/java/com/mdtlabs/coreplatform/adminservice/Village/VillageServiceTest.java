package com.mdtlabs.coreplatform.adminservice.Village;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.commons.compress.utils.IOUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.adminservice.AdminConstants;
import com.mdtlabs.coreplatform.adminservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.adminservice.repository.ChiefdomRepository;
import com.mdtlabs.coreplatform.adminservice.repository.DistrictRepository;
import com.mdtlabs.coreplatform.adminservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.VillageServiceImpl;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;

/**
 * <p>
 * The VillageServiceTest class is a JUnit test class that tests the functionality of the
 * VillageService class, specifically the uploadFile method.
 * </p>
 *
 * @author Divya S
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class VillageServiceTest {

    @InjectMocks
    private VillageServiceImpl villageService;

    @Mock
    private VillageRepository villageRepository;

    @Mock
    private ChiefdomRepository chiefdomRepository;

    @Mock
    private DistrictRepository districtRepository;

    @Mock
    private OrganizationRepository organizationRepository;

    @Mock
    private RedisTemplate<String, Map<Long, List<Long>>> redisTemplate;

    @Mock
    private ModelMapper mapper;

    @Test
    void testUploadVillage() throws IOException {
        //given
        File file = new File("src/test/resources/input.xlsx");
        FileInputStream input = new FileInputStream(file);
        MultipartFile multipartFile = new MockMultipartFile("file",
                file.getName(), "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", IOUtils.toByteArray(input));

        Map<String, Object> map = new HashMap<>();
        map.put(AdminConstants.KEY_COUNTRY_ID, 1L);
        map.put(AdminConstants.KEY_DISTRICT_ID, 1L);
        map.put(AdminConstants.KEY_CHIEFDOM_ID, 1L);
        map.put(AdminConstants.KEY_VILLAGE_ID, 1L);
        map.put(AdminConstants.KEY_COUNTRY_TENANT_ID, 1L);
        map.put(AdminConstants.KEY_CHIEFDOM_TENANT_ID, 3L);
        map.put(AdminConstants.KEY_DISTRICT_TENANT_ID, 2L);
        map.put(AdminConstants.KEY_COUNTRY_NAME, "Sierra Leone");
        map.put(AdminConstants.KEY_DISTRICT_NAME, "Northern Region");
        map.put(AdminConstants.KEY_CHIEFDOM_NAME, "Bombali");
        map.put(AdminConstants.KEY_VILLAGE_NAME, "Bolgatanga Regional Hospital");
        List<Map<String, Object>> regionDetails = List.of(map);
        List<String> appTypes = List.of("COMMUNITY");

        when(villageRepository.getRegionDetails()).thenReturn(regionDetails);

        Country country = new Country("Seirra leone");
        District savedDistrict = TestDataProvider.getDistrict();
        savedDistrict.setCountryId(country.getId());
        Chiefdom savedChiefdom = TestDataProvider.getChiefdom();
        savedChiefdom.setCountryId(country.getId());
        savedChiefdom.setDistrictId(savedDistrict.getId());
        
        Village savedVillage = TestDataProvider.getVillage();
        savedVillage.setCountryId(country.getId());
        savedVillage.setDistrictId(savedDistrict.getId());
        savedVillage.setChiefdomId(savedChiefdom.getId());
        Village village = new Village();
        village.setName("Bolgatanga Regional Hospital");
        village.setCode("2");
        village.setCountryId(country.getId());
        village.setDistrictId(savedDistrict.getId());
        village.setChiefdomId(savedChiefdom.getId());
        Organization organization = new Organization( "district","Northern Region", 1l);
        
        //when
        when(organizationRepository.save(any(Organization.class))).thenReturn(organization);
        when(districtRepository.save(any(District.class))).thenReturn(savedDistrict);
        when(chiefdomRepository.save(any(Chiefdom.class))).thenReturn(savedChiefdom);
        when(villageRepository.save(any(Village.class))).thenReturn(village);

        //then
        villageService.uploadRegionFile(multipartFile, appTypes);
        Assertions.assertNotNull(file);
    }

    @Test
    void testUploadVillageWithExistingVillages() throws IOException {
        //given
        File file = new File("src/test/resources/input.xlsx");
        FileInputStream input = new FileInputStream(file);
        MultipartFile multipartFile = new MockMultipartFile("file",
                file.getName(), "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", IOUtils.toByteArray(input));

        Map<String, Object> map = new HashMap<>();
        map.put(AdminConstants.KEY_COUNTRY_ID, 1L);
        map.put(AdminConstants.KEY_DISTRICT_ID, 1L);
        map.put(AdminConstants.KEY_CHIEFDOM_ID, 1L);
        map.put(AdminConstants.KEY_VILLAGE_ID, 1L);
        map.put(AdminConstants.KEY_COUNTRY_TENANT_ID, 1L);
        map.put(AdminConstants.KEY_CHIEFDOM_TENANT_ID, 3L);
        map.put(AdminConstants.KEY_DISTRICT_TENANT_ID, 2L);
        map.put(AdminConstants.KEY_COUNTRY_NAME, "Sierra Leone");
        map.put(AdminConstants.KEY_DISTRICT_NAME, "Northern Region");
        map.put(AdminConstants.KEY_CHIEFDOM_NAME, "Bombali");
        map.put(AdminConstants.KEY_VILLAGE_NAME, "Bolgatanga Regional Hospital");
        List<Map<String, Object>> regionDetails = List.of(map);
        List<String> appTypes = List.of("COMMUNITY");

        when(villageRepository.getRegionDetails()).thenReturn(regionDetails);

        Country country = new Country("Seirra leone");
        District savedDistrict = TestDataProvider.getDistrict();
        savedDistrict.setCountryId(country.getId());
        Chiefdom savedChiefdom = TestDataProvider.getChiefdom();
        savedChiefdom.setCountryId(country.getId());
        savedChiefdom.setDistrictId(savedDistrict.getId());
        
        Village savedVillage = TestDataProvider.getVillage();
        savedVillage.setCountryId(country.getId());
        savedVillage.setDistrictId(savedDistrict.getId());
        savedVillage.setChiefdomId(savedChiefdom.getId());
        Village village = new Village();
        village.setName("Bolgatanga Regional Hospital");
        village.setCode("2");
        village.setCountryId(country.getId());
        village.setDistrictId(savedDistrict.getId());
        village.setChiefdomId(savedChiefdom.getId());
        village.setId(1L);
        Organization organization = new Organization( "district","Northern Region", 1l);


        //when
        when(organizationRepository.save(any(Organization.class))).thenReturn(organization);
        when(districtRepository.save(any(District.class))).thenReturn(savedDistrict);
        when(chiefdomRepository.save(any(Chiefdom.class))).thenReturn(savedChiefdom);
        when(villageRepository.save(any(Village.class))).thenReturn(village);


        //then
        villageService.uploadRegionFile(multipartFile, appTypes);
        Assertions.assertNotNull(file);
    }

    @Test
    void testDownloadFile() {
        Long countryId = 1l;

        Map<String, Object> map = new HashMap<>();
        map.put(AdminConstants.KEY_COUNTRY_ID, 1L);
        map.put(AdminConstants.KEY_DISTRICT_ID, 1L);
        map.put(AdminConstants.KEY_CHIEFDOM_ID, 1L);
        map.put(AdminConstants.KEY_VILLAGE_ID, 1L);
        map.put(AdminConstants.KEY_COUNTRY_TENANT_ID, 1L);
        map.put(AdminConstants.KEY_CHIEFDOM_TENANT_ID, 3L);
        map.put(AdminConstants.KEY_DISTRICT_TENANT_ID, 2L);
        map.put(AdminConstants.KEY_COUNTRY_NAME, "Sierra Leone");
        map.put(AdminConstants.KEY_DISTRICT_NAME, "Northern Region");
        map.put(AdminConstants.KEY_CHIEFDOM_NAME, "Bombali");
        map.put(AdminConstants.KEY_VILLAGE_NAME, "Bolgatanga Regional Hospital");
        List<Map<String, Object>> regionDetails = List.of(map);
        List<String> appTypes = List.of("COMMUNITY");

        //when
        when(villageRepository.getRegionDetails(countryId)).thenReturn(regionDetails);

        //then
        byte[] actualFile = villageService.downloadRegionFile(countryId, appTypes);

        assertNotNull(actualFile);
    }

    @Test
    void getVillageDetailsByVillageId() {
        //given
        Long id = 1l;
        Village village = TestDataProvider.getVillage();
        VillageDTO villageDTO = TestDataProvider.getVillageDTO();
        villageDTO.setCode(null);

        //when
        when(villageRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id)).thenReturn(village);
        when(mapper.map(village, VillageDTO.class)).thenReturn(villageDTO);

        //then
        VillageDTO response = villageService.getVillageDetailsByVillageId(id);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(villageDTO, response);
    }

    @Test
    void addVillagesToChiefdom() {
        Chiefdom chiefdom = TestDataProvider.getChiefdom();
        Village village = TestDataProvider.getVillage();
        List<Village> existingVillages = List.of(village);
        Map<String, Village> metaVillages = new HashMap<>();
        metaVillages.put(village.getName(), village);
        when(villageRepository.getVillages(chiefdom.getCountryId(), chiefdom.getDistrictId(), chiefdom.getId(), null)).thenReturn(existingVillages);
        villageService.addVillagesToChiefdom(chiefdom,existingVillages);
        verify(villageRepository).getVillages(chiefdom.getCountryId(), chiefdom.getDistrictId(), chiefdom.getId(), null);
        assertTrue(metaVillages.containsKey(village.getName()));
        assertEquals(village, metaVillages.get(village.getName()));
    }

    @Test
    void getMemberSequenceByVillageId() {
        Long id = 1L;
        when(villageRepository.getMemberSequenceByVillageId(id)).thenReturn(id);
        Long actualSequence = villageService.getMemberSequenceByVillageId(id);
        verify(villageRepository).getMemberSequenceByVillageId(id);
        assertEquals(id, actualSequence);
    }

    @Test
    void getHouseholdSequenceByVillageId() {
        Long id = 1L;
        when(villageRepository.getHouseholdSequenceByVillageId(id)).thenReturn(id);
        Long actualSequence = villageService.getHouseholdSequenceByVillageId(id);
        assertEquals(id, actualSequence);
        verify(villageRepository).getHouseholdSequenceByVillageId(id);
    }


}
