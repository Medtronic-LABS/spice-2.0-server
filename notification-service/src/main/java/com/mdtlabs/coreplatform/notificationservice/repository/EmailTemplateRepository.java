package com.mdtlabs.coreplatform.notificationservice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.EmailTemplate;

/**
 * <p>
 * EmailTemplateRepository is a Java interface defining a repository for the EmailTemplate entity.
 * It extends two other repository interfaces, JpaRepository and PagingAndSortingRepository,
 * which provide basic CRUD operations and pagination/sorting functionality respectively.
 * </p>
 *
 * @author Prabu created on Sep 16, 2022
 */
@Repository
public interface EmailTemplateRepository
        extends JpaRepository<EmailTemplate, Long>, PagingAndSortingRepository<EmailTemplate, Long> {

    public static final String GET_TEMPLATE_BY_TYPE = "select emailTemplate from EmailTemplate "
            + "as emailTemplate where emailTemplate.type = :type and emailTemplate.appType = :appType and "
            + "emailTemplate.isActive = true and emailTemplate.isDeleted = false";

    /**
     * <p>
     * This method used to get an email template based on its type and application type.
     * </p>
     *
     * @param type    {@link String} The type of email template for which the email template is being retrieved is given
     * @param appType {@link String} The type of application for which the email template is being retrieved is given
     * @return {@link EmailTemplate} The email template for the given type and app type is retrieved from the database
     * and returned
     */
    @Query(value = GET_TEMPLATE_BY_TYPE)
    public EmailTemplate getTemplate(@Param(Constants.TYPE) String type, @Param(Constants.APP_TYPE) String appType);

}
