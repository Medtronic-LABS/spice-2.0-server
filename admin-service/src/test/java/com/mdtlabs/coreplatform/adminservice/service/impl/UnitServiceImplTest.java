package com.mdtlabs.coreplatform.adminservice.service.impl;

import com.mdtlabs.coreplatform.adminservice.repository.UnitRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class UnitServiceImplTest {

    @Mock
    UnitRepository unitRepository;

    @InjectMocks
    UnitServiceImpl unitService;

    @Test
    public void testGetUnitsByType() {
        unitService.getUnitsByType("S");
        verify(unitRepository, times(1)).findByTypeAndIsActiveTrueAndIsDeletedFalse("S");
    }
}