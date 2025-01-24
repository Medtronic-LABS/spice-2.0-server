package com.mdtlabs.coreplatform.commonservice.common.util;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.mdtlabs.coreplatform.commonservice.common.Constants;


/**
 * <p>
 * The PaginationTest class contains parameterized tests for testing the Pagination class.
 * </p>
 *
 */
class PaginationTest {

    @ParameterizedTest
    @CsvSource({"10,10", "0,10", "1,1"})
    void setPagination(int limit, int expectedLimit) {
        //then
        Pageable pageable = Pagination.setPagination(Constants.ZERO, limit);
        assertEquals(expectedLimit, pageable.getPageSize());
        assertEquals(Constants.ZERO, pageable.getPageNumber());
    }

    @ParameterizedTest
    @CsvSource({"10,10", "0,10", "1,1"})
    void testSetPaginationWithLimit(int limit, int expectedLimit) {
        //then
        Pageable pageable = Pagination.setPagination(Constants.ZERO, limit, new ArrayList<>());
        assertEquals(expectedLimit, pageable.getPageSize());
        assertEquals(Constants.ZERO, pageable.getPageNumber());
    }

    @ParameterizedTest
    @CsvSource({"10,10", "0,10", "1,1"})
    void testSetPagination(int limit, int expectedLimit) {
        Sort sort = Sort.by(Constants.UPDATED_AT).ascending();
        //then
        Pageable pageable = Pagination.setPagination(Constants.ZERO, limit, sort);
        assertEquals(expectedLimit, pageable.getPageSize());
        assertEquals(Constants.ZERO, pageable.getPageNumber());
        assertEquals(sort, pageable.getSort());
    }

    @ParameterizedTest
    @CsvSource({"10,10,0,0,true", "0,10,10,1,false", "1,1,20,20,false"})
    void testSetPaginationWithSort(int limit, int expectedLimit, int skip, int expectedSkip, boolean sortByAscending) {
        //then
        Pageable pageable = Pagination.setPagination(skip, limit, Constants.UPDATED_AT, sortByAscending);
        assertEquals(expectedLimit, pageable.getPageSize());
        assertEquals(expectedSkip, pageable.getPageNumber());
    }
}