package com.mdtlabs.coreplatform.notificationservice.scheduler;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.EmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.OutBoundEmail;
import com.mdtlabs.coreplatform.notificationservice.service.EmailService;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;

/**
 * <p>
 * EmailScheduler class schedules and sends out emails based on a cron
 * expression and updates the status of the sent emails.
 * </p>
 *
 * @author Prabu Created on 23 Mar 2023
 */
@EnableScheduling
@Configuration
@ConditionalOnProperty(name = "spring.enable.scheduling.email")
public class EmailScheduler {

    private final EmailService emailService;

    @Value("${app.mail-send}")
    private String enableEmailNotification;

    public EmailScheduler(EmailService emailService) {
        this.emailService = emailService;
    }

    /**
     * <p>
     * This method is used to send out emails based on a cron expression and to update the status of the sent emails.
     * </p>
     */
    @Scheduled(cron = "${scheduler.cron.email}")
    @SchedulerLock(name = "TaskScheduler_email",
            lockAtLeastFor = "${app.shedlock.email.start}", lockAtMostFor = "${app.shedlock.email.stop}")
    public void scheduleTaskUsingCronExpression() {
        if (Boolean.parseBoolean(enableEmailNotification)) {
            List<OutBoundEmail> mails = emailService.getOutBoundEmails();
            List<OutBoundEmail> emailsListToUpdate = new ArrayList<>();
            if (!mails.isEmpty()) {
                for (OutBoundEmail outBoundEmail : mails) {
                    EmailDTO mail = new EmailDTO(outBoundEmail.getTo(), outBoundEmail.getCc(), outBoundEmail.getBcc(),
                            outBoundEmail.getSubject(), outBoundEmail.getBody());
                    boolean isSended = emailService.sendEmailNotification(mail);
                    if (isSended) {
                        outBoundEmail.setProcessed(isSended);
                    } else {
                        outBoundEmail.setRetryAttempts(outBoundEmail.getRetryAttempts() + 1);
                    }
                    emailsListToUpdate.add(outBoundEmail);
                }
                emailService.updateOutBoundEmails(emailsListToUpdate);
            }
        }
    }
}
