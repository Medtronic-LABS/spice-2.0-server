package com.mdtlabs.coreplatform.commonservice.common.aspects;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import org.aspectj.lang.JoinPoint;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class TenantPermissionValidationTest {

    @InjectMocks
    private TenantPermissionValidation tenantPermissionValidation;

    @Mock
    private JoinPoint joinPoint;

    @Test
    void validateAspectWhereSelectedTenantIdIsNull() {
        //given
        UserSelectedTenantContextHolder.set(null);
        UserDTO userDTO =  new UserDTO();

        //then
        Assertions.assertThrows(Validation.class,
                () -> tenantPermissionValidation.validateAspect(joinPoint, userDTO));
    }

    @Test
    void validateAspectWhereSelectedTenantIdNotInTenantList() {
        //given
        UserSelectedTenantContextHolder.set(1L);
        UserTenantsContextHolder.set(List.of(2L, 3L, 4L));
        UserDTO userDTO = new UserDTO();

        //then
        Assertions.assertThrows(Validation.class,
                () -> tenantPermissionValidation.validateAspect(joinPoint, userDTO));
    }

    @Test
    void validateAspectWhereSelectedTenantIdInTenantList() {
        //given
        UserSelectedTenantContextHolder.set(1L);
        UserTenantsContextHolder.set(List.of(1L, 2L, 3L));
        UserDTO userDTO = new UserDTO();

        //then
        Assertions.assertDoesNotThrow(() -> tenantPermissionValidation.validateAspect(joinPoint, userDTO));
    }
}