package com.mdtlabs.coreplatform.spiceservice.staticdata.service.impl;

import com.mdtlabs.coreplatform.spiceservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.common.model.FormMetaUi;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.FormMetaUiRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class FormMetaServiceImplTest {

    @Mock
    private FormMetaUiRepository formMetaUiRepository;

    @InjectMocks
    private  FormMetaServiceImpl formMetaService;

    @Test
    public void testGetMetaForms() {
        formMetaService.getMetaForms();
        verify(formMetaUiRepository, times(1)).findByIsDeletedFalseAndIsActiveTrue();
    }

    @Test
    public void testGetMetaFormsWithValue() {
        List<FormMetaUi> formMetaUi = List.of(new FormMetaUi());
        CommonUtil.getCommonListsInstance().setFormMetaUis(formMetaUi);
        List<FormMetaUi> metaForms = formMetaService.getMetaForms();
        verify(formMetaUiRepository, times(0)).findByIsDeletedFalseAndIsActiveTrue();
        assertEquals(metaForms, formMetaUi);
    }

    @Test
    public void testGetMetaFormsArgs() {
        FormMetaUi formMetaUi = new FormMetaUi();
        formMetaUi.setFormName("s");
        when(formMetaUiRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(List.of(formMetaUi));
        FormMetaUi metaForm = formMetaService.getMetaForms("s");
        verify(formMetaUiRepository, times(1)).findByIsDeletedFalseAndIsActiveTrue();
        assertEquals(metaForm, formMetaUi);
    }

}