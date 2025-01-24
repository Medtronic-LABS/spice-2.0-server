package com.mdtlabs.coreplatform.commonservice.common.service.impl;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.repository.GenericRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Sort;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class GenericServiceTest<T extends BaseEntity> {

    @InjectMocks
    GenericServiceImpl<T> genericService;

    @Mock
    GenericRepository<User> genericRepository;

    @Test
    void findAll() {
        //given
        List<User> response = new ArrayList<>();

        //when
        when(genericRepository.findAll()).thenReturn(response);

        //then
        List<T> all = genericService.findAll();
        assertEquals(response, all);
    }

    @Test
    void findAllThrowsException() {
        //then
        doThrow(NullPointerException.class).when(genericRepository).findAll();
        assertThrows(Exception.class, () -> genericService.findAll());
    }

    @Test
    void findById() {
        //given
        User user = new User();
        user.setId(1L);

        //when
        when(genericRepository.findById(1L)).thenReturn(Optional.of(user));

        //then
        T entity = genericService.findById(1L);
        assertEquals(user, entity);
    }

    @Test
    void findByIdWhenEmpty() {
        //when
        when(genericRepository.findById(1L)).thenReturn(Optional.empty());

        //then
        T entity = genericService.findById(1L);
        assertNull(entity);

    }

    @Test
    void findByIdWhenThrowsException() {
        //when
        when(genericRepository.findById(1L)).thenReturn(null);

        //then
        assertThrows(Exception.class, () -> genericService.findById(1L));
    }

    @Test
    void save() {
        //given
        User user = new User();
        user.setId(1L);

        //when
        when(genericRepository.save(user)).thenReturn(user);

        //then
        T entity = genericService.save((T) user);
        assertEquals(user, entity);
    }

    @Test
    void saveThrowsException(){
        //given
        User user = new User();
        user.setId(1L);

        //then
        doThrow(NullPointerException.class).when(genericRepository).save(user);
        assertThrows(Exception.class, () -> genericService.save((T) user));
    }

    @Test
    void testFindAll() {
        //given
        Sort sort = Sort.by("id").ascending();
        User user = new User();
        user.setId(1L);
        List<User> users = Arrays.asList(user);

        //when
        when(genericRepository.findAll(sort)).thenReturn(users);

        //then
        List<T> expectedUsers = genericService.findAll(sort);
        assertEquals(users, expectedUsers);
    }

    @Test
    void TestFindAllThrowsException() {
        //given
        Sort sort = Sort.by("id").ascending();

        //then
        doThrow(NullPointerException.class).when(genericRepository).findAll(sort);
        assertThrows(Exception.class, () -> genericService.findAll(sort));
    }
}