package com.mdtlabs.coreplatform.commonservice.common.controller;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.service.impl.GenericServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class GenericControllerTest<T extends BaseEntity> {

    @InjectMocks
    GenericController<User> genericController;

    @Mock
    GenericServiceImpl<User> genericService;

    private User getUser(){
        User user =new User();
        user.setId(1L);
        return user;
    }

    @Test
    void save() {
        //given
        ResponseEntity<User> response = new ResponseEntity<>(getUser(), HttpStatus.OK);

        //when
        when(genericService.save(getUser())).thenReturn(getUser());

        //then
        ResponseEntity<Object> expectedResponse = genericController.save(getUser());
        assertEquals(response, expectedResponse);
    }

    @Test
    void saveWhenThrowsException() {
        //given
        ResponseEntity<User> response = new ResponseEntity<>(getUser(), HttpStatus.INTERNAL_SERVER_ERROR);

        //when
        doThrow(NullPointerException.class).when(genericService).save(getUser());

        //then
        ResponseEntity<Object> expectedResponse = genericController.save(getUser());
        assertEquals(response.getStatusCode(), expectedResponse.getStatusCode());
    }

    @Test
    void findAll() {
        //given
        ResponseEntity<List<User>> response = new ResponseEntity<>(List.of(getUser()), HttpStatus.OK);

        //when
        when(genericService.findAll()).thenReturn(List.of(getUser()));

        //then
        ResponseEntity<List<User>> expectedResponse = genericController.findAll();
        assertEquals(response, expectedResponse);
    }

    @Test
    void findAllWhenThrowsException() {
        //given
        ResponseEntity<List<User>> response = new ResponseEntity<>(List.of(getUser()), HttpStatus.INTERNAL_SERVER_ERROR);

        //when
        doThrow(NullPointerException.class).when(genericService).findAll();

        //then
        ResponseEntity<List<User>> expectedResponse = genericController.findAll();
        assertEquals(response.getStatusCode(), expectedResponse.getStatusCode());
    }

    @Test
    void findById() {
        //given
        ResponseEntity<User> response = new ResponseEntity<>(getUser(), HttpStatus.OK);

        //when
        when(genericService.findById(1L)).thenReturn(getUser());

        //then
        ResponseEntity<User> expectedResponse = genericController.findById(1L);
        assertEquals(response, expectedResponse);
    }

    @Test
    void findByIdWhenUserIsNull() {
        //given
        ResponseEntity<User> response = new ResponseEntity<>(getUser(), HttpStatus.INTERNAL_SERVER_ERROR);

        //when
        when(genericService.findById(1L)).thenReturn(null);

        //then
        ResponseEntity<User> expectedResponse = genericController.findById(1L);
        assertEquals(response.getStatusCode(), expectedResponse.getStatusCode());
    }

    @Test
    void findByIdWhenThrowsException() {
        //given
        ResponseEntity<User> response = new ResponseEntity<>(getUser(), HttpStatus.INTERNAL_SERVER_ERROR);

        //when
        doThrow(NullPointerException.class).when(genericService).findById(1L);

        //then
        ResponseEntity<User> expectedResponse = genericController.findById(1L);
        assertEquals(response.getStatusCode(), expectedResponse.getStatusCode());
    }
}