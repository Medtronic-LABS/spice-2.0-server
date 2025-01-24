package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.fhirmapper.common.RelationshipConstants;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RelationshipAlgorithmTest {

    @InjectMocks
    RelationshipAlgorithm relationshipAlgorithm;

    @Test
    void getChildRelationshipFromMother() {

        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.GRANDPARENT_LOWER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.BROTHER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.SISTER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.SON));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.MOTHER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.FATHER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.HUSBAND));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.HOUSEHOLD_HEAD));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.PARTNER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.SPOUSE));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.DAUGHTER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(null));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.CHILD));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.GRANDCHILD));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.AUNTY));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.BROTHER_SISTER));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.GRANDPARENT));
        Assertions.assertNotNull(relationshipAlgorithm.getChildRelationshipFromMother(RelationshipConstants.SON_DAUGHTER));
    }
}