package com.mdtlabs.coreplatform.commonservice.common;

import java.lang.reflect.Field;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import static org.mockito.Mockito.mockStatic;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import org.mockito.MockedStatic;
import org.modelmapper.ModelMapper;
import org.springframework.core.annotation.AnnotationUtils;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import org.springframework.mock.web.MockMultipartFile;

/**
 * <p>
 * The TestDataProvider class provides methods for initializing and mocking various objects and entities used in unit
 * tests.
 * </p>
 *
 * @author John Kennedy created on Jul 2, 2024
 */
public class TestDataProvider {

    public static ModelMapper modelMapper = new ModelMapper();
    private static MockedStatic<CommonUtil> commonUtil;
    private static MockedStatic<UserSelectedTenantContextHolder> userSelectedTenantContextHolder;
    private static MockedStatic<UserTenantsContextHolder> userTenantsContextHolder;
    private static MockedStatic<UserContextHolder> userContextHolder;
    private static MockedStatic<MessageFormat> messageFormat;
    private static MockedStatic<AnnotationUtils> annotationUtils;

    public static void init() {
        commonUtil = mockStatic(CommonUtil.class);
        userSelectedTenantContextHolder = mockStatic(UserSelectedTenantContextHolder.class);
        userTenantsContextHolder = mockStatic(UserTenantsContextHolder.class);
        userContextHolder = mockStatic(UserContextHolder.class);
        messageFormat = mockStatic(MessageFormat.class);
        annotationUtils = mockStatic(AnnotationUtils.class);
    }

    public static void getStaticMock() {
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(TestConstants.ONE);
        userContextDTO.setIsSuperUser(Boolean.FALSE);
        userContextDTO.setTenantId(TestConstants.ONE);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn("BearerTest");
        userSelectedTenantContextHolder.when(UserSelectedTenantContextHolder::get).thenReturn(1L);
        userTenantsContextHolder.when(UserTenantsContextHolder::get).thenReturn(List.of(1L));
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);
    }

    public static void getMessageValidatorMock() {
        messageFormat.when(() -> MessageFormat.format("Invalid token: {0}, {1}", TestConstants.ARGUMENT, TestConstants.MESSAGE)).thenReturn("message");
        messageFormat.when(() -> MessageFormat.format("Invalid token: {0}", TestConstants.ARGUMENT)).thenReturn("message");
    }

    public static void cleanUp() {
        commonUtil.close();
        userSelectedTenantContextHolder.close();
        userTenantsContextHolder.close();
        userContextHolder.close();
        messageFormat.close();
        annotationUtils.close();
    }

    public static Collection<Object> setUp(Class<?> injectClass, String field, Object mockedObject,
                                           Collection<Object> collection) throws NoSuchFieldException, IllegalAccessException {
        Field mapper = injectClass.getDeclaredField(field);
        mapper.setAccessible(true);
        mapper.set(mockedObject, collection);
        return collection;
    }

    public static BaseEntity getBaseEntity() {
        return new BaseEntity();
    }

    public static TenantBaseEntity getTenantBaseEntity() {
        return new TenantBaseEntity();
    }

    public static UserDTO getUserDTO() {
        return modelMapper.map(getUser(), UserDTO.class);
    }

    public static User getUser() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordCount(Constants.ONE);
        user.setForgetPasswordTime(new Date());
        user.setInvalidLoginAttempts(Constants.ONE);
        user.setTenantId(TestConstants.FIVE);
        return user;
    }

    public static Role getRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName(TestConstants.ROLE_NAME);
        role.setLevel(1000L);
        return role;
    }

    public static Organization getOrganization() {
        Organization organization = new Organization();
        organization.setId(TestConstants.ONE);
        organization.setFormName(Constants.FORM_NAME);
        organization.setFormDataId(TestConstants.ONE);
        organization.setName(Constants.ORGANIZATION_REDIS_KEY);
        organization.setSequence(TestConstants.ZERO);
        organization.setParentOrganizationId(TestConstants.ONE);
        organization.setActive(Boolean.TRUE);
        organization.setDeleted(Boolean.FALSE);
        organization.setCreatedBy(TestConstants.ONE);
        organization.setUpdatedBy(TestConstants.ONE);
        organization.setCreatedAt(new Date());
        organization.setUpdatedAt(new Date());
        return organization;
    }

    public static List<Organization> getOrganizationList() {
        List<Organization> organizations = new ArrayList<>();
        Organization organization = new Organization();
        organization.setId(TestConstants.ONE);
        organization.setFormDataId(TestConstants.ONE);
        organization.setFormName("country");
        organizations.add(organization);

        organization = new Organization();
        organization.setId(TestConstants.TWO);
        organization.setFormDataId(TestConstants.ONE);
        organization.setFormName("district");
        organization.setParentOrganizationId(TestConstants.ONE);
        organizations.add(organization);

        organization = new Organization();
        organization.setId(TestConstants.THREE);
        organization.setFormDataId(TestConstants.ONE);
        organization.setFormName("chiefdom");
        organization.setParentOrganizationId(TestConstants.TWO);
        organizations.add(organization);

        organization = new Organization();
        organization.setId(TestConstants.FOUR);
        organization.setFormDataId(TestConstants.ONE);
        organization.setFormName("healthfacility");
        organization.setParentOrganizationId(TestConstants.THREE);
        organizations.add(organization);

        return organizations;
    }

    public static MockMultipartFile getMockedSignatureFile() {
        return new MockMultipartFile(
                "signatureFiles",
                "signature.jpeg",
                "text/plain",
                "Sample Signature Data".getBytes()
        );
    }
}
