package com.mdtlabs.coreplatform.spiceservice.metadata.controller;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.controller.GenericController;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.service.GenericService;


/**
 * <p>
 * Timezone Controller used to perform any action in the timezone module like read and
 * write.
 * </p>
 *
 * @author Divya created on Jul 18, 2024
 */
@RestController
@RequestMapping("/timezone")
public class TimezoneController extends GenericController<Timezone> {

    public TimezoneController(GenericService<Timezone> genericService) {
        super(genericService);
    }
}
