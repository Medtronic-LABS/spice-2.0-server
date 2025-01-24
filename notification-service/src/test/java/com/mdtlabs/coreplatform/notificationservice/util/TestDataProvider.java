package com.mdtlabs.coreplatform.notificationservice.util;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.NotificationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OutBoundEmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.*;

import org.apache.commons.lang3.StringUtils;
import org.springframework.cloud.client.ServiceInstance;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.EmailDTO;

public class TestDataProvider {

    public static Role getRole() {
        Role role = new Role();
        role.setId(TestConstants.ONE);
        role.setName(Constants.ROLE_NAME_CLAIM);
        role.setLevel(TestConstants.ONE);
        return role;
    }

    public static Organization getOrganization() {
        Organization organization = new Organization();
        organization.setId(TestConstants.ONE);
        organization.setFormName(Constants.FORM_NAME);
        organization.setFormDataId(TestConstants.ONE);
        organization.setName(TestConstants.COUNTRY_NAME);
        organization.setSequence(TestConstants.ZERO);
        organization.setParentOrganizationId(TestConstants.FIVE);
        organization.setFormDataId(TestConstants.ONE);
        organization.setActive(Boolean.TRUE);
        return organization;
    }

    public static Set<Organization> getSetOrganizations() {
        Set<Organization> setOrganizations = new HashSet<>();
        setOrganizations.add(getOrganization());
        return setOrganizations;
    }

    public static User getUser() {
        User user = new User();
        user.setId(TestConstants.ONE);
        user.setPassword(Constants.PASSWORD_ENCODER);
        user.setFirstName(TestConstants.USER_FIRST_NAME);
        user.setLastName(TestConstants.USER_LAST_NAME);
        user.setPhoneNumber(TestConstants.PHONE_NUMBER);
        user.setUsername(TestConstants.USER_NAME);
        user.setForgetPasswordCount(Constants.ONE);
        user.setForgetPasswordTime(new Date());
        user.setInvalidLoginAttempts(Constants.ONE);
        user.setTenantId(TestConstants.FIVE);
        user.setCountryCode(TestConstants.COUNTRY_CODE_VALUE);
        user.setRoles(Set.of(getRole()));
        user.setOrganizations(getSetOrganizations());
        return user;
    }
    public static EmailDTO getEmailDTO() {
        EmailDTO emailDTO = new EmailDTO();
        emailDTO.setBcc(Constants.EMAIL);
        emailDTO.setTo(getUser().getUsername());
        emailDTO.setFormDataId(getUser().getId().toString());
        emailDTO.setFormName(Constants.EMAIL_FORM_USER);
        emailDTO.setSubject(Constants.SUCCESS);
        emailDTO.setToMails(TestConstants.USER_NAME);
        emailDTO.setBody(Constants.EMAIL);
        return emailDTO;
    }

    public static OutBoundEmail getOutBoundEmail() {
        OutBoundEmail outBoundEmail = new OutBoundEmail();
        outBoundEmail.setId(TestConstants.ONE);
        outBoundEmail.setTenantId(TestConstants.ONE);
        outBoundEmail.setFormDataId(TestConstants.USER_NAME);
        outBoundEmail.setFormName(Constants.EMAIL_FORM_USER);
        outBoundEmail.setBcc(Constants.EMAIL_FORM_USER);
        outBoundEmail.setBody(Constants.EMAIL);
        outBoundEmail.setRetryAttempts(Constants.TWO);
        outBoundEmail.setTo(TestConstants.USER_NAME);
        outBoundEmail.setCc(Constants.EMAIL);
        outBoundEmail.setSubject(Constants.EMAIL);
        return outBoundEmail;
    }

    public static List<OutBoundEmail> getOutBoundEmails() {
        OutBoundEmail outBoundEmail = getOutBoundEmail();
        return List.of(outBoundEmail);
    }

    public static EmailTemplate getEmailTemplate() {
        EmailTemplate emailTemplate = new EmailTemplate();
        emailTemplate.setId(TestConstants.ONE);
        emailTemplate.setType(Constants.TYPE);
        return emailTemplate;
    }

    public static Properties getProperties() {
        Properties properties = new Properties();
        properties.put(Constants.MAIL_SMTP_AUTH, Constants.TRUE);
        properties.put(Constants.MAIL_SMTP_STARTTLS_ENABLE, Constants.TRUE);
        properties.put(Constants.MAIL_SMTP_HOST, "host");
        properties.put(Constants.MAIL_SMTP_PORT, "port");
        properties.put(Constants.MAIL_SMTP_SSL_PROTOCALS, Constants.SMTP_SSL_PROTOCAL);
        return properties;
    }

    public static Authenticator getAuthenticator() {
        return new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication("user", "password");
            }
        };
    }

    public static Session getSession() {
        return null;
    }

    public static MimeMessage getMimeMessage() {
        Session session = getSession();
        MimeMessage mimeMessage = new MimeMessage(session);
        try {
            mimeMessage.addHeader(Constants.CONTENT_TYPE, Constants.TEXT_HTML_CHARSET);
            mimeMessage.addHeader(Constants.FORMAT, Constants.FLOWED);
            mimeMessage.addHeader(Constants.CONTENT_TRANSFER_ENCODING, Constants.ENCODING);
            mimeMessage.setFrom(TestConstants.USER_NAME);
            mimeMessage.setReplyTo(InternetAddress.parse(TestConstants.USER_NAME, false));
            mimeMessage.setSubject(getEmailDTO().getSubject(), Constants.UTF_8);
            mimeMessage.setContent(getEmailDTO().getBody(), Constants.TEXT_HTML_CHARSET);
            mimeMessage.setSentDate(new Date());
            mimeMessage.setRecipients(Message.RecipientType.TO, InternetAddress
                    .parse(StringUtils.isNotBlank(getEmailDTO().getTo())
                            ? getEmailDTO().getTo() : Constants.EMPTY, false));
            mimeMessage.setRecipients(Message.RecipientType.CC, InternetAddress
                    .parse(StringUtils.isNotBlank(getEmailDTO().getCc())
                            ? getEmailDTO().getCc() : Constants.EMPTY, false));
            mimeMessage.setRecipients(Message.RecipientType.BCC, InternetAddress
                    .parse(StringUtils.isNotBlank(getEmailDTO().getBcc())
                            ? getEmailDTO().getBcc() : Constants.EMPTY, false));
        } catch (Exception exception) {
            return mimeMessage;
        }
        return mimeMessage;
    }

    public static List<ServiceInstance> getServiceInstances() {
        ServiceInstance serviceInstance = getServiceInstance();
        return List.of(serviceInstance);
    }

    private static ServiceInstance getServiceInstance() {
        return new ServiceInstance() {
            @Override
            public String getServiceId() {
                return null;
            }

            @Override
            public String getHost() {
                return null;
            }

            @Override
            public int getPort() {
                return 0;
            }

            @Override
            public boolean isSecure() {
                return false;
            }

            @Override
            public URI getUri() {
                try {
                    return new URI(Constants.APP_URL_EMAIL);
                } catch (URISyntaxException e) {
                    throw new RuntimeException(e);
                }
            }

            @Override
            public Map<String, String> getMetadata() {
                return null;
            }
        };
    }

    public static OutBoundSMS getOutBoundSMS() {
        return new OutBoundSMS();
    }


    public static OutBoundEmailDTO getOutBoundEmailDTO() {
        return new OutBoundEmailDTO();
    }


    public static NotificationDTO getNotificationDTO() {
        return new NotificationDTO();
    }

    public static Notification getNotification() {
        return new Notification();
    }

    public static SmsDTO getSmsDTO() {
        return new SmsDTO();
    }

    public static SMSTemplate getSmsTemplate() {
        return new SMSTemplate();
    }

    public static List<OutBoundSMS> getOutboundSmsList() {
        List<OutBoundSMS> smsList = new ArrayList<>();
        smsList.add(getOutBoundSMS());
        return smsList;
    }
}
