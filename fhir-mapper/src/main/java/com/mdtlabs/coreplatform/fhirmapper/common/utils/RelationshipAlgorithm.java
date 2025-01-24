package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import java.util.Objects;

import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.fhirmapper.common.RelationshipConstants;

/**
 * Provides functionality to predict familial relationships based on a given mother's relationship to the household head.
 * <p>
 * This class contains methods to predict a child's relationship to the household head based on the mother's relationship.
 * It utilizes predefined constants for relationship types and supports a range of familial relationships.
 * </p>
 *
 * @author Nandhakumar created on Jun 09, 2024
 */
@Component
public class RelationshipAlgorithm {

    /**
     * Predicts the child's relationship to the household head based on the mother's relationship.
     * <p>
     * This method takes the mother's relationship to the household head as input and predicts the child's relationship
     * accordingly. It covers various familial relationships, including grandparent, aunt/uncle, and sibling relationships,
     * by checking the mother's relationship against a set of predefined constants.
     * </p>
     *
     * @param motherRelation The mother's relationship to the household head as a String.
     * @return The predicted child's relationship to the household head as a String.
     */
    public String getChildRelationshipFromMother(String motherRelation) {
        if (Objects.isNull(motherRelation)) {
            return RelationshipConstants.SON_DAUGHTER;
        }
        String relation = RelationshipConstants.CHILD;
        motherRelation = motherRelation.toLowerCase();
        if (containsAny(motherRelation, RelationshipConstants.GRANDPARENT_LOWER)) {
            relation = RelationshipConstants.GRANDCHILD;
        } else if (containsAny(motherRelation, RelationshipConstants.BROTHER, RelationshipConstants.SISTER)) {
            relation = RelationshipConstants.AUNTY;
        } else if (containsAny(motherRelation, RelationshipConstants.FATHER, RelationshipConstants.MOTHER)) {
            relation = RelationshipConstants.GRANDPARENT;
        } else if (containsAny(motherRelation, RelationshipConstants.SON, RelationshipConstants.DAUGHTER)) {
            relation = RelationshipConstants.BROTHER_SISTER;
        } else if (containsAny(motherRelation, RelationshipConstants.SPOUSE, RelationshipConstants.PARTNER,
                RelationshipConstants.HOUSEHOLD_HEAD, RelationshipConstants.HUSBAND)) {
            relation = RelationshipConstants.SON_DAUGHTER;
        }

        return relation;
    }

    /**
     * Checks if the source string contains any of the target strings.
     * <p>
     * This utility method checks if the given source string contains any of the provided target strings.
     * It is case-insensitive and returns true if at least one target string is found within the source.
     * </p>
     *
     * @param source  The source string to check.
     * @param targets An array of target strings to look for within the source string.
     * @return True if the source contains any of the target strings, false otherwise.
     */
    private boolean containsAny(String source, String... targets) {
        for (String target : targets) {
            if (source.contains(target.toLowerCase())) {
                return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

}
