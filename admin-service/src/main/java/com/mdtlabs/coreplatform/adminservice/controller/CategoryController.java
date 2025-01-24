package com.mdtlabs.coreplatform.adminservice.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.model.entity.Category;
import com.mdtlabs.coreplatform.adminservice.service.CategoryService;

import java.util.List;

/**
 * <p>
 *   A controller used to perform api operations related to Category module
 * </p>
 *
 * @author Gokul created on Oct 17, 2024
 */
@RestController
@RequestMapping(value = "/category")
public class CategoryController {

    private final CategoryService categoryService;

    @Autowired
    public CategoryController(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    /**
     * <p>
     *   Get medication category details
     * </p>
     * @return - List of categories
     */
    @GetMapping("/list")
    public List<Category> getCategories() {
        return categoryService.getCategories();
    }
}

