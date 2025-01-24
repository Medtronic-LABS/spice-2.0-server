package com.mdtlabs.coreplatform.spiceservice.staticdata.service.impl;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.spiceservice.common.model.FormMetaUi;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.FormMetaUiRepository;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.FormMetaService;
import com.mdtlabs.coreplatform.spiceservice.common.CommonUtil;


/**
 * <p>
 * This service class contain all the business logic for FormMetaService module and perform
 * all the user operation here.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Aug 8, 2022
 */
@Service
public class FormMetaServiceImpl implements FormMetaService {

    private final FormMetaUiRepository formMetaUiRepository;

    @Autowired
    public FormMetaServiceImpl(FormMetaUiRepository formMetaUiRepository) {
        this.formMetaUiRepository = formMetaUiRepository;
    }

    /**
     * {@inheritDoc}
     */
    public List<FormMetaUi> getMetaForms() {
        if (CommonUtil.getCommonListsInstance().getFormMetaUis().isEmpty()) {
            CommonUtil.getCommonListsInstance().setFormMetaUis(formMetaUiRepository.findByIsDeletedFalseAndIsActiveTrue());
        }
        return CommonUtil.getCommonListsInstance().getFormMetaUis();
    }

    /**
     * {@inheritDoc}
     */
    public FormMetaUi getMetaForms(String formName) {
        if (CommonUtil.getCommonListsInstance().getFormMetaUis().isEmpty()) {
            CommonUtil.getCommonListsInstance().setFormMetaUis(formMetaUiRepository.findByIsDeletedFalseAndIsActiveTrue());
        }
        Optional<FormMetaUi> formMetaUiOptional = CommonUtil.getCommonListsInstance().getFormMetaUis().stream().filter(obj -> formName.equals(obj.getFormName())).findFirst();
        return formMetaUiOptional.isPresent()? formMetaUiOptional.get() : null;
    }
}
