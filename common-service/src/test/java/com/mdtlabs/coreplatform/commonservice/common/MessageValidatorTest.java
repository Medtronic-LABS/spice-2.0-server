package com.mdtlabs.coreplatform.commonservice.common;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MessageValidatorTest {

    @InjectMocks
    private MessageValidator messageValidator;

    @Test
    void getMessage() {
        //then
        String actualMessage = messageValidator.getMessage(TestConstants.STATUS_CODE, Constants.ERROR);
        assertNotNull(actualMessage);
        assertEquals(TestConstants.INVALID_TOKEN, actualMessage);
    }

    @Test
    void testGetMessageWithArgs() {
        //then
        TestDataProvider.init();
        TestDataProvider.getMessageValidatorMock();
        String actualMessage = messageValidator.getMessage(TestConstants.STATUS_CODE,
                Constants.ERROR, TestConstants.ARGUMENT, TestConstants.MESSAGE);
        TestDataProvider.cleanUp();
        assertNotNull(actualMessage);
        assertEquals(TestConstants.MESSAGE, actualMessage);
    }

    @Test
    void testGetMessage() {
        //then
        TestDataProvider.init();
        TestDataProvider.getMessageValidatorMock();
        String actualMessage = messageValidator.getMessage(TestConstants.STATUS_CODE,
                Constants.ERROR, List.of(TestConstants.ARGUMENT, TestConstants.MESSAGE));
        TestDataProvider.cleanUp();
        assertNotNull(actualMessage);
        assertEquals(TestConstants.MESSAGE, actualMessage);
    }

    @Test
    void testGetMessageWithArg() {
        //then
        TestDataProvider.init();
        TestDataProvider.getMessageValidatorMock();
        String actualMessage = messageValidator.getMessage(TestConstants.ERROR_STATUS_CODE, Constants.ERROR, TestConstants.ARGUMENT);
        TestDataProvider.cleanUp();
        assertNotNull(actualMessage);
        assertEquals(TestConstants.MESSAGE, actualMessage);
    }

    @Test
    void getMessages() {
        //then
        List<String> response = messageValidator.getMessage(TestConstants.ERROR_PROPERTIES);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMessagesWhenTypeIsSuccess() {
        //then
        String response = messageValidator.getMessage(TestConstants.ERROR_PROPERTIES, Constants.SUCCESS);
        Assertions.assertNull(response);
    }

    @Test
    void getMessagesWhenTypeIsInstruction() {
        //then
        assertThrows(NullPointerException.class, () -> messageValidator.getMessage(TestConstants.ERROR_PROPERTIES, Constants.INSTRUCTIONS));
    }

    @Test
    void messageValidator(){
        MessageValidator message = MessageValidator.getInstance();
        assertNotNull(message);
    }
}