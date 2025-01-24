package com.mdtlabs.coreplatform.notificationservice.common;

import java.util.Properties;

import javax.mail.Authenticator;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.MimeMessage;

import static org.mockito.Mockito.mockStatic;

import com.twilio.Twilio;
import org.mockito.MockedStatic;
import org.mockito.stubbing.Answer;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.notificationservice.util.TestDataProvider;

public class TestCommonMethods {

    private static MockedStatic<Session> session;
    private static MockedStatic<Transport> transport;
    private static MockedStatic<Twilio> twilio;

    public static void init() {
        session = mockStatic(Session.class);
        transport = mockStatic(Transport.class);
    }

    public static void getStaticMock() {
        MimeMessage mimeMessage = TestDataProvider.getMimeMessage();
        Properties properties = TestDataProvider.getProperties();
        Authenticator authenticator = TestDataProvider.getAuthenticator();
        session.when(() -> Session.getInstance(properties, authenticator)).thenReturn(null);
        transport.when(() -> Transport.send(mimeMessage)).thenAnswer((Answer<Void>) invocation -> null);
    }

    public static void cleanUp() {
        session.close();
        transport.close();
    }

    public static void initTwi() {
        twilio = mockStatic(Twilio.class);
    }

    public static void getStaticMockTwi() {
        twilio.when(() -> Twilio.init(Constants.STRING_THREE, Constants.TOKEN))
                .thenAnswer((Answer<Void>) invocation -> null);
    }

    public static void cleanUpTwi() {
        twilio.close();
    }
}
