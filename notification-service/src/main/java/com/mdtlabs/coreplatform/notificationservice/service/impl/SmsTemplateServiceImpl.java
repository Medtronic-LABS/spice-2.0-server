package com.mdtlabs.coreplatform.notificationservice.service.impl;

import java.util.List;

import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.notificationservice.repository.SmsTemplateRepository;
import com.mdtlabs.coreplatform.notificationservice.service.SmsTemplateService;

/**
 * <p>
 * SmsTemplateServiceImpl class implements the SmsTemplateService interface and provides methods to retrieve SMS templates
 * from a repository.
 * </p>
 *
 * @author Prabu created on Mar 23, 2023
 */
@Service
public class SmsTemplateServiceImpl implements SmsTemplateService {

    private final SmsTemplateRepository smsTemplateRepository;

    public SmsTemplateServiceImpl(SmsTemplateRepository smsTemplateRepository) {
        this.smsTemplateRepository = smsTemplateRepository;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SMSTemplate getSmsTemplateValues(String templateType) {
        return smsTemplateRepository.findByTypeAndIsActiveTrueAndIsDeletedFalse(templateType.toUpperCase());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SMSTemplate> getSMSTemplates(List<String> list) {
        return smsTemplateRepository.getSMSTemplates(null);
    }

}
