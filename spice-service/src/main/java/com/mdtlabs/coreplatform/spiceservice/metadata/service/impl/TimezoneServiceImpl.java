package com.mdtlabs.coreplatform.spiceservice.metadata.service.impl;

import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.repository.GenericRepository;
import com.mdtlabs.coreplatform.commonservice.common.service.impl.GenericServiceImpl;
import com.mdtlabs.coreplatform.spiceservice.metadata.service.TimezoneService;

/**
 * <p>
 * This service class contain all the business logic for timezone module and perform
 * all the user operation here.
 * </p>
 *
 * @author VigneshKumar created on Jun 30, 2022
 */
@Service
public class TimezoneServiceImpl extends GenericServiceImpl<Timezone> implements TimezoneService {

    public TimezoneServiceImpl(GenericRepository<Timezone> genericRepository) {
        super(genericRepository);
    }
}
