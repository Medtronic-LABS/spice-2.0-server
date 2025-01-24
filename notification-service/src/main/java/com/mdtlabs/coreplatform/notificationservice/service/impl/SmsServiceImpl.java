package com.mdtlabs.coreplatform.notificationservice.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.OutBoundSMS;
import com.mdtlabs.coreplatform.notificationservice.repository.OutBoundSmsRepository;
import com.mdtlabs.coreplatform.notificationservice.service.SmsService;

/**
 * <p>
 * SmsServiceImpl class that implements an interface for saving and retrieving SMS messages using a repository.
 * </p>
 *
 * @author JeyahariniTA created on Nov 03, 2022
 */
@Service
public class SmsServiceImpl implements SmsService {


    private final OutBoundSmsRepository outBoundSmsRepository;

    ModelMapper modelMapper = new ModelMapper();

    @Value("${app.sms-retry-attempts}")
    private int smsRetryAttempts;

    public SmsServiceImpl(OutBoundSmsRepository outBoundSmsRepository) {
        this.outBoundSmsRepository = outBoundSmsRepository;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean saveOutBoundSms(List<SmsDTO> smsDtoList) {

		List<OutBoundSMS> outBoundSmsList = new ArrayList<>();
		for (SmsDTO smsDto : smsDtoList) {
			OutBoundSMS outBoundSms = new OutBoundSMS();
			if (!Objects.isNull(smsDto.getBody())) {
				outBoundSms.setBody(smsDto.getBody());
			}
			if (!Objects.isNull(smsDto.getToPhoneNo())) {
				outBoundSms.setPhoneNumber(smsDto.getToPhoneNo());
			}
			if (!Objects.isNull(smsDto.getUserName())) {
				outBoundSms.setUserName(smsDto.getUserName());
			}
			if (!Objects.isNull(smsDto.getNotificationId())) {
				outBoundSms.setNotificationId(smsDto.getNotificationId());
			}
			if (!Objects.isNull(smsDto.getTenantId())) {
				outBoundSms.setTenantId(smsDto.getTenantId());
			}
			if (!Objects.isNull(smsDto.getFormDataId())) {
				outBoundSms.setFormDataId(smsDto.getFormDataId());
			}
			outBoundSmsList.add(outBoundSms);
		}
		outBoundSmsRepository.saveAll(outBoundSmsList);
		return true;

	}

    /**
     * {@inheritDoc}
     */
    public List<OutBoundSMS> getOutBoundSms() {
        return outBoundSmsRepository.getAllSms(smsRetryAttempts);
    }

    /**
     * {@inheritDoc}
     */
    public List<OutBoundSMS> updateOutBoundSms(List<OutBoundSMS> outBoundSms) {
        return outBoundSmsRepository.saveAll(outBoundSms);
    }
}
