package com.mdtlabs.coreplatform.commonservice.common.aspects;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.annotations.TenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.contexts.AppTypesContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.repository.HealthFacilityRepository;


/**
 * <p>
 * This class is the business logic implementation for validation annotation
 * used under @ConfigureAppType annotation.
 * </p>
 *
 * @author Vishwaeaswaran created on Oct 01, 2024
 */
@Aspect
@Component
public class AppTypeConfiguration {

    private HealthFacilityRepository healthFacilityRepository;

    @Autowired
    public AppTypeConfiguration(HealthFacilityRepository healthFacilityRepository){
        this.healthFacilityRepository = healthFacilityRepository;
    }

    /**
     * <p>
     * This is a Java aspect that sets the app type in context holder.
     * </p>
     *
     * @param joinPoint {@link JoinPoint} object represents the execution of a method in Spring AOP is given.
     */
    @Before("@annotation(com.mdtlabs.coreplatform.commonservice.common.annotations.ConfigureAppType)")
    public void validateAspect(JoinPoint joinPoint) {
        Long selectedTenantId = UserSelectedTenantContextHolder.get();
        HealthFacility healthFacility =
                healthFacilityRepository.findByTenantIdAndIsDeletedFalseAndIsActiveTrue(selectedTenantId);
        Set<String> appTypes = new HashSet<>();
        if (Objects.nonNull(healthFacility)) {
            healthFacility.getClinicalWorkflows()
                    .forEach(clinicalWorkflow -> appTypes.addAll(clinicalWorkflow.getAppTypes()));
            if (Constants.ONE == appTypes.size()) {
                SelectedAppTypeContextHolder.set(appTypes.iterator().next());
            } else if (Constants.ONE < appTypes.size()) {
                SelectedAppTypeContextHolder.set(Constants.APP_TYPE_NON_COMMUNITY);
            }
        }
        AppTypesContextHolder.set(new ArrayList<>(appTypes));
    }
}