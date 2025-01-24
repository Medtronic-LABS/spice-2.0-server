package com.mdtlabs.coreplatform.authservice.common;

import java.util.Date;
import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserToken;

public class TestDataProvider {

    public static Role getRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName(TestConstants.ROLE_NAME);
        role.setLevel(1000l);
        return role;
    }

    public static User getUser() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setPassword(TestConstants.PASSWORD);
        user.setFirstName(TestConstants.FIRST_NAME);
        user.setLastName(TestConstants.LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordCount(Constants.ONE);
        user.setForgetPasswordTime(new Date());
        user.setInvalidLoginAttempts(Constants.ONE);
        user.setTenantId(TestConstants.ONE);
        user.setCountryCode(TestConstants.COUNTRY_CODE);
        user.setRoles(Set.of(getRole()));
        user.getSuiteAccess().add(Constants.CLIENT_ADMIN);
        user.getSuiteAccess().add(Constants.CLIENT_CFR);
        user.getSuiteAccess().add(Constants.CLIENT_SPICE_MOBILE);
        user.setTimezone(new Timezone());
        return user;
    }
    
    public static UserToken getUserToken() {
        UserToken userToken = new UserToken();
        userToken.setId(1L);
        userToken.setUserId(1L);
        userToken.setAuthToken(TestConstants.TOKEN);
        return userToken;
    }

}
