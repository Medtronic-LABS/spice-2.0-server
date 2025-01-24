package com.mdtlabs.coreplatform.spiceservice.metadata.service;


import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.repository.GenericRepository;
import com.mdtlabs.coreplatform.spiceservice.metadata.service.impl.TimezoneServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class TimezoneServiceImplTest {

    @InjectMocks
    private TimezoneServiceImpl timezoneService;

    @Mock
    private GenericRepository<Timezone> genericRepository;

    @Test
    void testLine() {
        //when
        when(genericRepository.findById(anyLong())).thenReturn(Optional.of(new Timezone()));

        //then
        timezoneService.findById(1L);
    }
}
