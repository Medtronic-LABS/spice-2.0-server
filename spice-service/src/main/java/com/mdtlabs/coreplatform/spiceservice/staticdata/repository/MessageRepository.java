package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Message;


@Repository
public interface MessageRepository extends JpaRepository<Message, Long> {

    List<Message> findByIsDeletedFalseAndIsActiveTrue();
}
