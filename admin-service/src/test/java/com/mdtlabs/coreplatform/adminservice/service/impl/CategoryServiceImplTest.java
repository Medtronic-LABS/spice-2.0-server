package com.mdtlabs.coreplatform.adminservice.service.impl;

import com.mdtlabs.coreplatform.adminservice.repository.CategoryRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class CategoryServiceImplTest {

    @Mock
    private CategoryRepository categoryRepository;

    @InjectMocks
    private CategoryServiceImpl categoryService;

    @Test
    void testGetCategories() {
        categoryService.getCategories();
        verify(categoryRepository).findByIsDeletedFalseAndIsActiveTrueOrderByDisplayOrder();
    }

}