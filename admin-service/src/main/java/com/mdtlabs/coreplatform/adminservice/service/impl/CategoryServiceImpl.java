package com.mdtlabs.coreplatform.adminservice.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.model.entity.Category;
import com.mdtlabs.coreplatform.adminservice.repository.CategoryRepository;
import com.mdtlabs.coreplatform.adminservice.service.CategoryService;

import java.util.List;

/**
 * <p>
 * This service class contain all the business logic for category module for medication.
 * </p>
 *
 * @author Gokul created on Oct 17, 2024
 */
@Service
public class CategoryServiceImpl implements CategoryService {

    private final CategoryRepository categoryRepository;

    @Autowired
    public CategoryServiceImpl(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    /**
     * {@inheritDoc}
     */
    public List<Category> getCategories() {
        return categoryRepository.findByIsDeletedFalseAndIsActiveTrueOrderByDisplayOrder();
    }
}
