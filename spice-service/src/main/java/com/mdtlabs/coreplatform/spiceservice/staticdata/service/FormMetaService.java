package com.mdtlabs.coreplatform.spiceservice.staticdata.service;

import java.util.List;

import com.mdtlabs.coreplatform.spiceservice.common.model.FormMetaUi;

/**
 * <p>
 * This an interface class for FormMetaUi module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Aug 08, 2024
 */
public interface FormMetaService {

    /**
     * Gets a all meta forms
     *
     * @return list of metaforms
     */
    public List<FormMetaUi> getMetaForms();

    /**
     * Gets a meta forms by formname
     *
     * @param formName
     * @return list of metaforms
     */
    public FormMetaUi getMetaForms(String formName);

}
