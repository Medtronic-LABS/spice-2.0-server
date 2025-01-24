INSERT INTO
    public.form_meta (
        form_input,
        form_type,
        clinical_workflow_id,
        is_active,
        is_deleted,
        created_by,
        updated_by,
        app_types
    )
VALUES
    (
        '{
  "formLayout": [
    {
      "familyOrder": 0,
      "id": "bioData",
      "title": "Bio Data",
      "viewType": "CardView"
    },
    {
      "errorMessage": "Name is required and must be length of 1 to 100",
      "family": "bioData",
      "fieldName": "Name",
      "hint": "Enter Name",
      "id": "name",
      "onlyAlphabets": true,
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "maxLength": 100,
      "minLength": 1,
      "orderId": 1,
      "title": "Name",
      "viewType": "EditText",
      "visibility": "visible"
    },
    {
      "condition": [
        {
          "eq": "Other Family Member (specify)",
          "targetId": "otherFamilyMember",
          "visibility": "visible"
        }
      ],
      "defaultValue": null,
      "errorMessage": "Please select an relationship",
      "family": "bioData",
      "fieldName": "Relationship to Household",
      "id": "household_head_relationship",
      "isEditable": true,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "optionsList": [
        {
          "id": "HouseholdHead",
          "name": "Household Head"
        },
        {
          "id": "Wife / Husband",
          "name": "Wife / Husband"
        },
        {
          "id": "Son / Daughter",
          "name": "Son / Daughter"
        },
        {
          "id": "Father / Mother",
          "name": "Father / Mother"
        },
        {
          "id": "Brother / Sister",
          "name": "Brother / Sister"
        },
        {
          "id": "Grandchild",
          "name": "Grandchild"
        },
        {
          "id": "Grandparent",
          "name": "Grandparent"
        },
        {
          "id": "Other Family Member (specify)",
          "name": "Other Family Member (specify)"
        }
      ],
      "title": "Relationship to Household",
      "viewType": "Spinner",
      "orderId": 2,
      "visibility": "visible"
    },
    {
      "errorMessage": "Please enter a relationship",
      "family": "bioData",
      "fieldName": "Specify Other Relationship",
      "hint": "Enter Other Relationship",
      "id": "otherFamilyMember",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "orderId": 5,
      "isSummary": false,
      "title": "Specify Other Relationship",
      "viewType": "EditText",
      "visibility": "gone"
    },
    {
      "condition": [],
      "defaultValue": null,
      "errorMessage": "Please select a household location",
      "family": "bioData",
      "fieldName": "Village Name",
      "id": "village_id",
      "isEditable": true,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "optionsList": [],
      "title": "Household Location",
      "viewType": "Spinner",
      "localDataCache": "village_id",
      "visibility": "gone"
    },
    {
      "errorMessage": "Mobile Number is required and must be of length 1 to 8",
      "family": "bioData",
      "fieldName": "Mobile Number",
      "hint": "Enter Mobile Number",
      "id": "phone_number",
      "inputType": 3,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "maxLength": 8,
      "minLength": 1,
      "orderId": 3,
      "title": "Mobile Number",
      "viewType": "EditText",
      "visibility": "visible",
      "contentLength": 8,
      "startsWith": [
        "2",
        "3",
        "7",
        "8",
        "9"
      ]
    },
    {
      "condition": [],
      "defaultValue": null,
      "errorMessage": "Please select a Mobile number category",
      "family": "bioData",
      "fieldName": "Mobile Number Category",
      "id": "phone_number_category",
      "isEditable": true,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "optionsList": [
        {
          "id": "Personal",
          "name": "Personal"
        },
        {
          "id": "Family Member",
          "name": "Family Member"
        },
        {
          "id": "Friend",
          "name": "Friend"
        }
      ],
      "title": "Mobile Number Category",
      "orderId": 4,
      "viewType": "Spinner",
      "visibility": "visible"
    },
    {
      "disableFutureDate": true,
      "family": "bioData",
      "fieldName": "Age",
      "id": "date_of_birth",
      "isEnabled": true,
      "maxAge": 130,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "title": "Age",
      "viewType": "Age",
      "orderId": 5,
      "visibility": "visible"
    },
    {
      "errorMessage": "Please select a valid option",
      "condition": [
        {
          "eq": "Female",
          "targetId": "isPregnant",
          "visibility": "visible"
        }
      ],
      "family": "bioData",
      "fieldName": "Gender",
      "id": "gender",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 6,
      "optionsList": [
        {
          "name": "Male",
          "id": "male"
        },
        {
          "name": "Female",
          "id": "female"
        }
      ],
      "title": "Gender",
      "viewType": "SingleSelectionView",
      "visibility": "visible"
    },
    {
      "errorMessage": "Please select a valid option",
      "family": "bioData",
      "fieldName": "Are you pregnant?",
      "id": "isPregnant",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 7,
      "optionsList": [
        {
          "name": "Yes",
          "id": "yes"
        },
        {
          "name": "No",
          "id": "no"
        }
      ],
      "title": "Are you pregnant?",
      "viewType": "SingleSelectionView",
      "visibility": "gone"
    }
  ],
  "time": 12624563247
}
',
        'household_member_registration',
        NULL,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    ),
    (
        '{
  "formLayout": [
    {
      "familyOrder": 0,
      "id": "generalDangerSigns",
      "title": "General Danger Sign",
      "viewType": "CardView"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "generalDangerSigns",
      "fieldName": "Unconscious or unusually sleepy",
      "id": "isUnusualSleepy",
      "isEnabled": true,
      "isMandatory": true,
      "titleSummary": "General Danger Signs",
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 1,
      "readOnly": true,
      "title": "Unconscious or unusually sleepy",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Unconscious/unusually sleepy)",
      "visibility": "visible"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "generalDangerSigns",
      "fieldName": "Convulsions now or in the past few days",
      "id": "isConvulsionPastFewDays",
      "titleSummary": "General Danger Signs",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 2,
      "readOnly": true,
      "title": "Convulsions now or in the past few days",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Convulsions)",
      "visibility": "visible"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "generalDangerSigns",
      "fieldName": "Vomiting everything",
      "titleSummary": "General Danger Signs",
      "id": "isVomiting",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 3,
      "readOnly": true,
      "title": "Vomiting everything",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Vomiting)",
      "visibility": "visible"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "generalDangerSigns",
      "fieldName": "Not able to breastfeed or drink",
      "titleSummary": "General Danger Signs",
      "id": "isBreastfeed",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 4,
      "readOnly": true,
      "title": "Not able to breastfeed or drink",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Not able to breastfeed/drink)",
      "visibility": "visible"
    },
    {
      "familyOrder": 1,
      "id": "nutritionalStatusDetails",
      "title": "Nutritional Status",
      "viewType": "CardView"
    },
    {
      "condition": [],
      "defaultValue": null,
      "errorMessage": "Please select a valid option",
      "family": "nutritionalStatusDetails",
      "fieldName": "MUAC",
      "titleSummary": "MUAC",
      "id": "muacCode",
      "isSummary": true,
      "isEditable": true,
      "isEnabled": true,
      "isNeededDefault": true,
      "orderId": 1,
      "optionsList": [
        {
          "id": "Green",
          "name": "Green"
        },
        {
          "id": "Yellow",
          "name": "Yellow"
        },
        {
          "id": "Red",
          "name": "Red"
        }
      ],
      "title": "MUAC",
      "viewType": "Spinner",
      "isInfo": "visible",
      "infoTitle": "Job Aid (MUAC)",
      "visibility": "visible"
    },
    {
      "family": "nutritionalStatusDetails",
      "id": "muacStatus",
      "title": "MUAC",
      "backgroundColor": "#FDF4F0",
      "visibility": "visible",
      "viewType": "InformationLabel"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "nutritionalStatusDetails",
      "fieldName": "Oedema of both feet (SAM)",
      "titleSummary": "Oedema",
      "id": "hasOedemaOfBothFeet",
      "isEnabled": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 2,
      "readOnly": true,
      "title": "Oedema of both feet (SAM)",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Oedema)",
      "visibility": "visible"
    },
    {
      "familyOrder": 2,
      "id": "cough",
      "title": "Cough",
      "viewType": "CardView"
    },
    {
      "condition": [
        {
          "eq": "Yes",
          "targetId": "noOfDaysOfCough",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "chestInDrawing",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "breathPerMinute",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "amoxicillin",
          "visibility": "gone"
        },
        {
          "eq": "No",
          "targetId": "amoxicillinStatus",
          "visibility": "gone"
        }
      ],
      "errorMessage": "Please select a valid option",
      "family": "cough",
      "fieldName": "Cough or Difficult Breathing",
      "id": "hasCough",
      "titleSummary": "Cough",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 1,
      "readOnly": true,
      "title": "Cough or Difficult Breathing",
      "viewType": "SingleSelectionView",
      "isInfo": "gone",
      "visibility": "visible"
    },
    {
      "family": "cough",
      "fieldName": "Number of days of cough",
      "titleSummary": "Number of days of cough",
      "hint": "Enter Days",
      "id": "noOfDaysOfCough",
      "inputType": 2,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 2,
      "title": "Number of days of cough",
      "viewType": "NoOfDaysView",
      "maxLength": 2,
      "visibility": "gone",
      "informationVisibility": "invisible",
      "isInfo": "gone",
      "noOfDays": 20,
      "information": "≥ 21 Days"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "cough",
      "fieldName": "Chest indrawing",
      "titleSummary": "Chest indrawing",
      "id": "chestInDrawing",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 3,
      "readOnly": true,
      "title": "Chest indrawing",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Chest indrawing)",
      "visibility": "gone"
    },
    {
      "errorMessage": "Breath rate is required",
      "family": "cough",
      "fieldName": "Breaths per minute",
      "hint": "Breath Rate",
      "titleSummary": "Breathing Rate",
      "id": "breathPerMinute",
      "onlyAlphabets": true,
      "inputType": 3,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 4,
      "title": "Breaths per minute",
      "viewType": "NoOfDaysView",
      "visibility": "gone",
      "informationVisibility": "invisible",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Breaths per minute)",
      "noOfDays": 50,
      "maxLength": 2,
      "information": "Fast Breathing"
    },
    {
      "family": "cough",
      "id": "amoxicillinStatus",
      "title": "Recommended Dose",
      "backgroundColor": "#FDF4F0",
      "visibility": "gone",
      "viewType": "InformationLabel"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "cough",
      "fieldName": "Amoxicillin",
      "id": "amoxicillin",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "NA",
          "name": "Not Applicable"
        },
        {
          "id": "Dispensed",
          "name": "Dispensed"
        }
      ],
      "optionType": "boolean",
      "orderId": 3,
      "readOnly": true,
      "title": "Amoxicillin",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Amoxicillin)",
      "visibility": "gone",
      "dosageListItems": [
        {
          "tableId": 1,
          "columnName1": "DAY",
          "columnName2": "MORNING",
          "columnName3": "NIGHT",
          "title": "Treatment for Pneumonia Amoxicillin 250mg tab",
          "dosageFrequency": [
            {
              "minMonth": 2,
              "maxMonth": 12,
              "monthLabel": "2 months up to 12 months",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                },
                {
                  "day": "4",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                },
                {
                  "day": "5",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 12,
              "maxMonth": 36,
              "monthLabel": "12 months up to 3 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                },
                {
                  "day": "2",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                },
                {
                  "day": "3",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                },
                {
                  "day": "4",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                },
                {
                  "day": "5",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                }
              ]
            },
            {
              "minMonth": 36,
              "maxMonth": 60,
              "monthLabel": "3 years up to 5 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                },
                {
                  "day": "2",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                },
                {
                  "day": "3",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                },
                {
                  "day": "4",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                },
                {
                  "day": "5",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                }
              ]
            }
          ],
          "descriptionTitle": "Method of giving Amoxicillin tablets",
          "descriptionList": [
            "Demonstrate by giving the first dose. Teach the caregiver how to prepare the medicine",
            "Give the rest of the medicine to the caregiver to administer to the child at home",
            "Instruct the caregiver to give medicine twice per day for the full 5 days, even if the child feels better"
          ]
        }
      ]
    },
    {
      "familyOrder": 3,
      "id": "fever",
      "title": "Fever",
      "viewType": "CardView"
    },
    {
      "condition": [
        {
          "eq": "Yes",
          "targetId": "noOfDaysOfFever",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "rdtTest",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "act",
          "visibility": "gone"
        },
        {
          "eq": "No",
          "targetId": "actStatus",
          "visibility": "gone"
        }
      ],
      "errorMessage": "Please select a valid option",
      "family": "fever",
      "titleSummary": "Fever",
      "fieldName": "Fever, History of Fever or warm body",
      "id": "hasFever",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 1,
      "readOnly": true,
      "title": "Fever, History of Fever or warm body",
      "viewType": "SingleSelectionView",
      "isInfo": "gone",
      "visibility": "visible"
    },
    {
      "fieldName": "Temperature",
      "visibility": "gone",
      "maxValue": 48,
      "errorMessage": "",
      "title": "Temperature (in °C)",
      "titleSummary": "Temperature (in °C)",
      "condition": [],
      "minValue": 1,
      "maxLength": 4,
      "hint": "Temperature",
      "isEnabled": true,
      "isEnrollment": true,
      "viewType": "EditText",
      "inputType": 8192,
      "id": "temperature",
      "orderId": 2,
      "isSummary": true,
      "family": "fever"
    },
    {
      "errorMessage": "No of Days is required",
      "family": "fever",
      "fieldName": "Number of days of fever",
      "titleSummary": "Number of days of fever",
      "hint": "Enter Days",
      "id": "noOfDaysOfFever",
      "onlyAlphabets": true,
      "inputType": 2,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 3,
      "title": "Number of days of fever",
      "viewType": "NoOfDaysView",
      "visibility": "gone",
      "informationVisibility": "invisible",
      "isInfo": "gone",
      "noOfDays": 6,
      "maxLength": 2,
      "information": "≥ 7 Days"
    },
    {
      "condition": [
        {
          "eq": "+ve",
          "targetId": "act",
          "visibility": "visible"
        },
        {
          "eq": "+ve",
          "targetId": "actStatus",
          "visibility": "visible"
        }
      ],
      "errorMessage": "Please select a valid option",
      "family": "fever",
      "fieldName": "RDT - Test(+/-)",
      "titleSummary": "RDT",
      "id": "rdtTest",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "+ve",
          "name": "+ve"
        },
        {
          "id": "-ve",
          "name": "-ve"
        },
        {
          "id": "NA",
          "name": "Not Applicable"
        }
      ],
      "optionType": "boolean",
      "orderId": 4,
      "readOnly": true,
      "title": "RDT - Test(+/-)",
      "viewType": "SingleSelectionView",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "family": "fever",
      "id": "actStatus",
      "title": "Recommended Dose",
      "backgroundColor": "#FDF4F0",
      "visibility": "gone",
      "viewType": "InformationLabel"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "fever",
      "fieldName": "ACT",
      "id": "act",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "NA",
          "name": "Not Applicable"
        },
        {
          "id": "Dispensed",
          "name": "Dispensed"
        }
      ],
      "optionType": "boolean",
      "orderId": 5,
      "readOnly": true,
      "title": "ACT",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (ACT)",
      "visibility": "gone",
      "dosageListItems": [
        {
          "tableId": 1,
          "columnName1": "DAY",
          "columnName2": "MORNING",
          "columnName3": "NIGHT",
          "title": "Treatment of Malaria: AM - LF or AS - AQ",
          "dosageFrequency": [
            {
              "minMonth": 0,
              "maxMonth": 6,
              "monthLabel": "<6 months",
              "warning": "Do not provide AM - LF for <6 months old child"
            },
            {
              "minMonth": 6,
              "maxMonth": 36,
              "monthLabel": "6 months - 3 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet \n(0hr)",
                  "night": "1 Tablet \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 36,
              "maxMonth": 96,
              "monthLabel": "3 - 8 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "2 Tablets \n(0hr)",
                  "night": "2 Tablets \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                },
                {
                  "day": "3",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                }
              ]
            },
            {
              "minMonth": 96,
              "maxMonth": 168,
              "monthLabel": "9 - 14 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "3 Tablets \n(0hr)",
                  "night": "3 Tablets  \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                },
                {
                  "day": "3",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                }
              ]
            },
            {
              "minMonth": 168,
              "maxMonth": 1440,
              "monthLabel": "14 years and above",
              "routine": [
                {
                  "day": "1",
                  "morning": "4 Tablets \n(0hr)",
                  "night": "4 Tablets \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "4 Tablets",
                  "night": "4 Tablets"
                },
                {
                  "day": "3",
                  "morning": "4 Tablets",
                  "night": "4 Tablets"
                }
              ]
            }
          ],
          "descriptionTitle": "Simplified 3 - Day ACT Dose Regimen: Artesunate - Amodiaquine (AS - AQ)"
        },
        {
          "tableId": 2,
          "columnName1": "DAY",
          "columnName2": "DOSAGE PER DAY",
          "title": "Treatment of Malaria: AM - LF or AS - AQ",
          "dosageFrequency": [
            {
              "minMonth": 0,
              "maxMonth": 2,
              "monthLabel": "<2 months",
              "warning": "Do not provide AS - AQ for <2 months old child"
            },
            {
              "minMonth": 2,
              "maxMonth": 11,
              "monthLabel": "2 - 11 months: Pink packet",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 12,
              "maxMonth": 60,
              "monthLabel": "1 - 5 years: Purple packet",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 61,
              "maxMonth": 156,
              "monthLabel": "6 - 13 years: White packet",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 156,
              "maxMonth": 1440,
              "monthLabel": "14 years and above: White packet",
              "routine": [
                {
                  "day": "1",
                  "morning": "2 Tablets"
                },
                {
                  "day": "2",
                  "morning": "2 Tablets"
                },
                {
                  "day": "3",
                  "morning": "2 Tablets"
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "familyOrder": 4,
      "id": "diarrhoea",
      "title": "Diarrhoea",
      "viewType": "CardView"
    },
    {
      "condition": [
        {
          "eq": "Yes",
          "targetId": "numberOfDaysDiarrhoea",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "isBloodyDiarrhoea",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "diarrhoeaSigns",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "orsDispensedStatus",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "jellyWaterDispensedStatus",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "sssDispensedStatus",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "zincDispensedStatus",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "orsStatus",
          "visibility": "gone"
        },
        {
          "eq": "Yes",
          "targetId": "orsStatus",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "zincStatus",
          "visibility": "gone"
        },
        {
          "eq": "Yes",
          "targetId": "zincStatus",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "otherSigns",
          "visibility": "gone"
        }
      ],
      "errorMessage": "Please select a valid option",
      "family": "diarrhoea",
      "fieldName": "Diarhoea",
      "titleSummary": "Diarrhoea",
      "id": "hasDiarrhoea",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 1,
      "readOnly": true,
      "title": "Diarrhoea",
      "viewType": "SingleSelectionView",
      "isInfo": "gone",
      "visibility": "visible"
    },
    {
      "errorMessage": "No of Days is required",
      "family": "diarrhoea",
      "fieldName": "Number of days of diarrhoea",
      "titleSummary": "Number of days of diarrhoea",
      "hint": "Enter Days",
      "id": "numberOfDaysDiarrhoea",
      "onlyAlphabets": true,
      "inputType": 2,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 2,
      "title": "Number of days of Diarrhoea",
      "viewType": "NoOfDaysView",
      "visibility": "gone",
      "maxLength": 2,
      "informationVisibility": "invisible",
      "isInfo": "gone",
      "noOfDays": 13,
      "information": "≥ 14 Days"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "diarrhoea",
      "fieldName": "BloodyDiarrhoea",
      "id": "isBloodyDiarrhoea",
      "titleSummary": "Bloody Diarrhoea",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 3,
      "readOnly": true,
      "title": "Bloody Diarrhoea",
      "viewType": "SingleSelectionView",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "condition": [
        {
          "eq": "Other",
          "targetId": "otherSigns",
          "visibility": "visible"
        }
      ],
      "errorMessage": "Please select a valid sign",
      "family": "diarrhoea",
      "fieldName": "Signs",
      "titleSummary": "Signs",
      "id": "diarrhoeaSigns",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "maxLength": 100,
      "minLength": 1,
      "orderId": 4,
      "isSummary": true,
      "title": "Signs",
      "viewType": "DialogCheckbox",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "errorMessage": "Please enter a valid sign",
      "family": "diarrhoea",
      "fieldName": "Other Signs",
      "hint": "Enter Other Sign",
      "id": "otherSigns",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "orderId": 5,
      "isSummary": false,
      "title": "Other Signs",
      "viewType": "EditText",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "family": "diarrhoea",
      "id": "orsStatus",
      "title": "Recommended Dose",
      "backgroundColor": "#FDF4F0",
      "visibility": "gone",
      "viewType": "InformationLabel"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "diarrhoea",
      "fieldName": "ORS",
      "id": "orsDispensedStatus",
      "isEnabled": true,
      "isMandatory": false,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "NA",
          "name": "Not Applicable"
        },
        {
          "id": "Dispensed",
          "name": "Dispensed"
        }
      ],
      "optionType": "boolean",
      "orderId": 6,
      "readOnly": true,
      "title": "ORS",
      "viewType": "SingleSelectionView",
      "visibility": "gone",
      "isInfo": "visible",
      "infoTitle": "Job Aid (ORS)",
      "instruction": true,
      "dosageListItems": [
        {
          "tableId": 1,
          "title": "ORS Mixing Instruction",
          "dosageFrequency": [],
          "descriptionTitle": "Method of mixing and giving ORS for Diarrhoea",
          "descriptionList": [
            "Use safe water",
            "Wash hands with soap and water",
            "Take 1 liter (3 coke boiles full) of water",
            "Mix a packet of ORS in the water",
            "Give it to the child using a glass or a spoon after each loose stool, and as much as the child needs.",
            "Discard any remaining mixture after 24 hours and mix a new one."
          ]
        }
      ]
    },
    {
      "family": "diarrhoea",
      "id": "zincStatus",
      "title": "Recommended Dose",
      "backgroundColor": "#FDF4F0",
      "visibility": "gone",
      "viewType": "InformationLabel"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "diarrhoea",
      "fieldName": "Zinc",
      "id": "zincDispensedStatus",
      "isEnabled": true,
      "isMandatory": false,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "NA",
          "name": "Not Applicable"
        },
        {
          "id": "Dispensed",
          "name": "Dispensed"
        }
      ],
      "optionType": "boolean",
      "orderId": 3,
      "readOnly": true,
      "title": "Zinc",
      "viewType": "SingleSelectionView",
      "visibility": "gone",
      "isInfo": "visible",
      "infoTitle": "Job Aid (Zinc)",
      "instruction": true,
      "dosageListItems": [
        {
          "tableId": 1,
          "columnName1": "DAY",
          "columnName2": "DOSAGE PER DAY",
          "columnName3": "NIGHT",
          "title": "Regimen of Zinc Tablets",
          "dosageFrequency": [
            {
              "minMonth": 2,
              "maxMonth": 6,
              "monthLabel": "2 - 6 months",
              "routine": [
                {
                  "day": "1",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "4",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "5",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "6",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "7",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "8",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "9",
                  "morning": "1/2 Tablet"
                },
                {
                  "day": "10",
                  "morning": "1/2 Tablet"
                }
              ]
            },
            {
              "minMonth": 6,
              "maxMonth": 60,
              "monthLabel": "6 months to 5 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet"
                },
                {
                  "day": "4",
                  "morning": "1 Tablet"
                },
                {
                  "day": "5",
                  "morning": "1 Tablet"
                },
                {
                  "day": "6",
                  "morning": "1 Tablet"
                },
                {
                  "day": "7",
                  "morning": "1 Tablet"
                },
                {
                  "day": "8",
                  "morning": "1 Tablet"
                },
                {
                  "day": "9",
                  "morning": "1 Tablet"
                },
                {
                  "day": "10",
                  "morning": "1 Tablet"
                }
              ]
            }
          ],
          "descriptionTitle": "Method of giving zinc tablets",
          "descriptionList": [
            "Take a small amount of water in a teaspoon",
            "Put zinc tablets according to the age of the child",
            "Let the tablet dissolve",
            "Give all the dissolved zinc to the tablet"
          ]
        }
      ]
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "diarrhoea",
      "fieldName": "Jelly Water",
      "id": "jellyWaterDispensedStatus",
      "isEnabled": true,
      "isMandatory": false,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "NA",
          "name": "Not Applicable"
        },
        {
          "id": "Dispensed",
          "name": "Dispensed"
        }
      ],
      "optionType": "boolean",
      "orderId": 6,
      "readOnly": true,
      "title": "Jelly Water",
      "viewType": "SingleSelectionView",
      "visibility": "gone",
      "isInfo": "gone",
      "instruction": true,
      "dosageListItems": [
        {
          "tableId": 1,
          "title": "ORS Mixing Instruction",
          "dosageFrequency": [],
          "descriptionTitle": "Method of mixing and giving ORS for Diarrhoea",
          "descriptionList": [
            "Use safe water",
            "Wash hands with soap and water",
            "Take 1 liter (3 coke boiles full) of water",
            "Mix a packet of ORS in the water",
            "Give it to the child using a glass or a spoon after each loose stool, and as much as the child needs.",
            "Discard any remaining mixture after 24 hours and mix a new one."
          ]
        }
      ]
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "diarrhoea",
      "fieldName": "SSS",
      "id": "sssDispensedStatus",
      "isEnabled": true,
      "isMandatory": false,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "NA",
          "name": "Not Applicable"
        },
        {
          "id": "Dispensed",
          "name": "Dispensed"
        }
      ],
      "optionType": "boolean",
      "orderId": 6,
      "readOnly": true,
      "title": "Sugar and Salt Solution",
      "viewType": "SingleSelectionView",
      "visibility": "gone",
      "isInfo": "gone",
      "instruction": true,
      "dosageListItems": [
        {
          "tableId": 1,
          "title": "ORS Mixing Instruction",
          "dosageFrequency": [],
          "descriptionTitle": "Method of mixing and giving ORS for Diarrhoea",
          "descriptionList": [
            "Use safe water",
            "Wash hands with soap and water",
            "Take 1 liter (3 coke boiles full) of water",
            "Mix a packet of ORS in the water",
            "Give it to the child using a glass or a spoon after each loose stool, and as much as the child needs.",
            "Discard any remaining mixture after 24 hours and mix a new one."
          ]
        }
      ]
    }
  ],
  "time": 1658993682722
}',
        'iccm',
        2,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    ),
    (
        '{"formLayout":[{"familyOrder":0,"id":"pncNeonatal","orderId":1,"title":"PNC Visit - Neonate","viewType":"CardView"},{"id":"pncNeonateSigns","family":"pncNeonatal","fieldName":"pncNeonateSigns","orderId":2,"title":"Is the newborn baby experiencing any of these danger signs?","viewType":"DialogCheckbox","isMandatory":false,"isSummary":true,"titleSummary":"Danger Signs in newborn baby","hint":"Select Symptoms","localDataCache":"pncNeonateSigns","errorMessage":"Please select a valid option","visibility":"visible","condition":[{"eq":"Other","targetId":"otherPncNeonateSigns","visibility":"visible"}]},{"errorMessage":"Please enter a valid sign","family":"pncNeonatal","fieldName":"Other Signs","hint":"Enter Other Sign","id":"otherPncNeonateSigns","inputType":96,"isEnabled":true,"isMandatory":true,"isNeededDefault":true,"isNotDefault":false,"orderId":5,"isSummary":false,"title":"Other Signs","viewType":"EditText","isInfo":"gone","visibility":"gone"},{"family":"pncNeonatal","fieldName":"newbornReferredToSBCU","id":"newbornReferredToSBCU","condition":[],"orderId":4,"errorMessage":"Please select a valid option","isEnabled":true,"isMandatory":true,"isNeededDefault":true,"isSummary":true,"isBooleanAnswer":true,"optionsList":[{"id":true,"name":"Yes"},{"id":false,"name":"No"}],"optionType":"boolean","readOnly":true,"viewType":"SingleSelectionView","title":"Newborn referred to Special Baby Care Unit (SBCU)","visibility":"visible"},{"family":"pncNeonatal","fieldName":"lowBirthWeight","id":"lowBirthWeight","condition":[],"orderId":4,"errorMessage":"Please select a valid option","isEnabled":true,"isMandatory":true,"isNeededDefault":true,"isSummary":true,"isBooleanAnswer":true,"optionsList":[{"id":true,"name":"Yes"},{"id":false,"name":"No"}],"optionType":"boolean","readOnly":true,"viewType":"SingleSelectionView","title":"Low birth weight","visibility":"visible"},{"family":"pncNeonatal","fieldName":"deathOfNewborn","id":"deathOfNewborn","condition":[],"orderId":4,"errorMessage":"Please select a valid option","isEnabled":true,"isMandatory":true,"isNeededDefault":true,"isSummary":true,"isBooleanAnswer":true,"optionsList":[{"id":true,"name":"Yes"},{"id":false,"name":"No"}],"optionType":"boolean","readOnly":true,"viewType":"SingleSelectionView","title":"Death of newborn","visibility":"visible"}],"time":1658993682722}',
        'pncNeonatal',
        4,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    ),
    (
        '{
  "formLayout": [
    {
      "familyOrder": 0,
      "id": "householdInformation",
      "title": "Household Information",
      "viewType": "CardView"
    },
    {
      "errorMessage": "Household Name is required and must be length of 1 to 100",
      "family": "householdInformation",
      "fieldName": "Household Name",
      "hint": "Enter Household Name",
      "id": "householdName",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "maxLength": 100,
      "minLength": 1,
      "orderId": 1,
      "title": "Household Name",
      "viewType": "EditText",
      "visibility": "visible"
    },
    {
      "condition": [],
      "defaultValue": null,
      "errorMessage": "Please select a village",
      "family": "householdInformation",
      "fieldName": "Village Name",
      "id": "village_id",
      "isEditable": true,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "optionsList": [],
      "title": "Household Location",
      "viewType": "Spinner",
      "localDataCache": "village_id",
      "visibility": "visible"
    },
    {
      "family": "householdInformation",
      "fieldName": "Landmark",
      "hint": "Enter Landmark",
      "id": "landmark",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": false,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "maxLength": 256,
      "minLength": 4,
      "orderId": 2,
      "title": "Landmark",
      "viewType": "EditTextArea",
      "visibility": "visible"
    },
    {
      "errorMessage": "Household Head Mobile No is required and must be length of 1 to 8 digits",
      "family": "householdInformation",
      "fieldName": "Household Head Mobile No",
      "hint": "Enter Household Head Mobile No",
      "id": "head_phone_number",
      "inputType": 3,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "maxLength": 8,
      "minLength": 1,
      "orderId": 3,
      "title": "Household Head Mobile No",
      "viewType": "EditText",
      "visibility": "visible",
      "contentLength": 8,
      "startsWith": [
        "2",
        "3",
        "7",
        "8",
        "9"
      ]
    },
    {
      "condition": [],
      "defaultValue": null,
      "errorMessage": "Please select a Mobile number category",
      "family": "householdInformation",
      "fieldName": "Mobile Number Category",
      "id": "phone_number_category",
      "isEditable": true,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "optionsList": [
        {
          "id": "Personal",
          "name": "Personal"
        },
        {
          "id": "Family Member",
          "name": "Family Member"
        },
        {
          "id": "Friend",
          "name": "Friend"
        }
      ],
      "title": "Mobile Number Category",
      "orderId": 4,
      "viewType": "Spinner",
      "visibility": "visible"
    },
    {
      "family": "householdInformation",
      "fieldName": "No. of People in Household",
      "hint": "Enter Number",
      "id": "no_of_people",
      "inputType": 3,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 4,
      "maxLength": 2,
      "title": "No.of People in Household",
      "viewType": "EditText",
      "visibility": "visible"
    },
    {
      "errorMessage": "Please select a valid option",
      "family": "householdInformation",
      "fieldName": "Owns an improved latrine?",
      "id": "is_owned_an_improved_latrine",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 5,
      "optionsList": [
        {
          "name": "Yes",
          "id": "yes"
        },
        {
          "name": "No",
          "id": "no"
        }
      ],
      "title": "Owns an improved latrine?",
      "viewType": "SingleSelectionView",
      "visibility": "visible"
    },
    {
      "errorMessage": "Please select a valid option",
      "family": "householdInformation",
      "fieldName": "Owns hand washing facility with soap?",
      "id": "is_owned_hand_washing_facility_with_soap",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 6,
      "optionsList": [
        {
          "name": "Yes",
          "id": "yes"
        },
        {
          "name": "No",
          "id": "no"
        }
      ],
      "title": "Owns hand washing facility with soap?",
      "viewType": "SingleSelectionView",
      "visibility": "visible"
    },
    {
      "errorMessage": "Please select a valid option",
      "family": "householdInformation",
      "fieldName": "Owns a treated bed net?",
      "id": "is_owned_a_treated_bed_net",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 7,
      "optionsList": [
        {
          "name": "Yes",
          "id": "yes"
        },
        {
          "name": "No",
          "id": "no"
        }
      ],
      "title": "Owns a treated bed net?",
      "viewType": "SingleSelectionView",
      "visibility": "visible",
      "condition": [
        {
          "eq": "Yes",
          "targetId": "bed_net_count",
          "visibility": "visible"
        }
      ]
    },
    {
      "family": "householdInformation",
      "fieldName": "How many bed nets?",
      "hint": "Enter number of bed nets",
      "id": "bed_net_count",
      "inputType": 3,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "orderId": 2,
      "maxLength": 1,
      "title": "How many bed nets are setup?",
      "viewType": "EditText",
      "visibility": "gone"
    }
  ],
  "time": 12624563247
}',
        'household_registration',
        NULL,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    ),
    (
        '{
  "formLayout": [
    {
      "familyOrder": 1,
      "id": "signsAndSymptoms",
      "title": "Signs & Symptoms",
      "viewType": "CardView"
    },
    {
      "condition": [
        {
          "eq": "Other",
          "targetId": "otherConcerningSymptoms",
          "visibility": "visible"
        }
      ],
      "errorMessage": "Please select a signs",
      "family": "signsAndSymptoms",
      "fieldName": "Signs",
      "id": "otherSymptoms",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "maxLength": 100,
      "minLength": 1,
      "orderId": 1,
      "hint": "Select Symptoms",
      "isSummary": true,
      "title": "Are you experiencing any concerning symptoms?",
      "viewType": "DialogCheckbox",
      "titleSummary": "General Danger Signs",
      "isInfo": "gone",
      "visibility": "visible"
    },
    {
      "errorMessage": "Please enter a valid sign",
      "family": "signsAndSymptoms",
      "fieldName": "Other Concerning Symptoms",
      "hint": "Enter Other Symptoms",
      "id": "otherConcerningSymptoms",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "orderId": 2,
      "isSummary": false,
      "title": "Other Concerning Symptoms",
      "viewType": "EditText",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "familyOrder": 2,
      "id": "fever",
      "title": "Fever",
      "viewType": "CardView"
    },
    {
      "condition": [
        {
          "eq": "Yes",
          "targetId": "noOfDaysOfFever",
          "visibility": "visible"
        },
        {
          "eq": "Yes",
          "targetId": "rdtTest",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "act",
          "visibility": "gone"
        },
        {
          "eq": "No",
          "targetId": "actStatus",
          "visibility": "gone"
        }
      ],
      "errorMessage": "Please select a valid option",
      "family": "fever",
      "fieldName": "Fever, History of Fever or Hot body",
      "id": "hasFever",
      "titleSummary": "Fever",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isBooleanAnswer": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "orderId": 1,
      "readOnly": true,
      "title": "Fever, History of Fever or warm body",
      "viewType": "SingleSelectionView",
      "visibility": "visible"
    },
    {
      "fieldName": "Temperature",
      "visibility": "gone",
      "maxValue": 48,
      "errorMessage": "",
      "title": "Temperature (in °C)",
      "condition": [],
      "minValue": 1,
      "hint": "Temperature",
      "isEnabled": true,
      "isEnrollment": true,
      "viewType": "EditText",
      "inputType": 8192,
      "maxLength": 4,
      "id": "temperature",
      "isSummary": true,
      "family": "fever",
      "titleSummary": "Temperature (in °C)"
    },
    {
      "condition": [],
      "errorMessage": "No of Days is required",
      "family": "fever",
      "fieldName": "Number of days of fever",
      "hint": "Enter Days",
      "id": "noOfDaysOfFever",
      "onlyAlphabets": true,
      "inputType": 2,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "isSummary": true,
      "maxLength": 2,
      "minLength": 1,
      "orderId": 2,
      "title": "Number of days of fever",
      "titleSummary": "Number of days of fever",
      "viewType": "NoOfDaysView",
      "visibility": "gone",
      "noOfDays": 6,
      "information": "≥ 7 days",
      "informationVisibility": "invisible"
    },
    {
      "condition": [
        {
          "eq": "+ve",
          "targetId": "act",
          "visibility": "visible"
        },
        {
          "eq": "+ve",
          "targetId": "actStatus",
          "visibility": "visible"
        }
      ],
      "errorMessage": "Please select a valid option",
      "family": "fever",
      "fieldName": "RDT-Test (+/-)",
      "id": "rdtTest",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "+ve",
          "name": "+ve"
        },
        {
          "id": "-ve",
          "name": "-ve"
        },
        {
          "id": "NA",
          "name": "Not Applicable"
        }
      ],
      "optionType": "boolean",
      "orderId": 3,
      "readOnly": true,
      "title": "RDT-Test (+/-)",
      "titleSummary": "RDT",
      "viewType": "SingleSelectionView",
      "visibility": "gone"
    },
    {
      "family": "fever",
      "id": "actStatus",
      "title": "Recommended Dose",
      "backgroundColor": "#FDF4F0",
      "visibility": "gone",
      "viewType": "InformationLabel"
    },
    {
      "condition": [],
      "errorMessage": "Please select a valid option",
      "family": "fever",
      "fieldName": "ACT",
      "id": "act",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": "NA",
          "name": "Not Applicable"
        },
        {
          "id": "Dispensed",
          "name": "Dispensed"
        }
      ],
      "optionType": "boolean",
      "orderId": 4,
      "readOnly": true,
      "title": "ACT",
      "titleSummary": "ACT",
      "viewType": "SingleSelectionView",
      "isInfo": "visible",
      "infoTitle": "Job Aid (ACT)",
      "visibility": "gone",
      "dosageListItems": [
        {
          "tableId": 1,
          "columnName1": "DAY",
          "columnName2": "MORNING",
          "columnName3": "NIGHT",
          "title": "Treatment of Malaria: AM - LF or AS - AQ",
          "dosageFrequency": [
            {
              "minMonth": 0,
              "maxMonth": 6,
              "monthLabel": "<6 months",
              "warning": "Do not provide AM - LF for <6 months old child"
            },
            {
              "minMonth": 6,
              "maxMonth": 36,
              "monthLabel": "6 months - 3 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet \n(0hr)",
                  "night": "1 Tablet \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet",
                  "night": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 36,
              "maxMonth": 96,
              "monthLabel": "3 - 8 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "2 Tablets \n(0hr)",
                  "night": "2 Tablets \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                },
                {
                  "day": "3",
                  "morning": "2 Tablets",
                  "night": "2 Tablets"
                }
              ]
            },
            {
              "minMonth": 96,
              "maxMonth": 168,
              "monthLabel": "9 - 14 years",
              "routine": [
                {
                  "day": "1",
                  "morning": "3 Tablets \n(0hr)",
                  "night": "3 Tablets  \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                },
                {
                  "day": "3",
                  "morning": "3 Tablets",
                  "night": "3 Tablets"
                }
              ]
            },
            {
              "minMonth": 168,
              "maxMonth": 1440,
              "monthLabel": "14 years and above",
              "routine": [
                {
                  "day": "1",
                  "morning": "4 Tablets \n(0hr)",
                  "night": "4 Tablets \n(8hrs)"
                },
                {
                  "day": "2",
                  "morning": "4 Tablets",
                  "night": "4 Tablets"
                },
                {
                  "day": "3",
                  "morning": "4 Tablets",
                  "night": "4 Tablets"
                }
              ]
            }
          ],
          "descriptionTitle": "Simplified 3 - Day ACT Dose Regimen: Artesunate - Amodiaquine (AS - AQ)"
        },
        {
          "tableId": 2,
          "columnName1": "DAY",
          "columnName2": "DOSAGE PER DAY",
          "title": "Treatment of Malaria: AM - LF or AS - AQ",
          "dosageFrequency": [
            {
              "minMonth": 0,
              "maxMonth": 2,
              "monthLabel": "<2 months",
              "warning": "Do not provide AS - AQ for <2 months old child"
            },
            {
              "minMonth": 2,
              "maxMonth": 11,
              "monthLabel": "2 - 11 months: Pink packet",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 12,
              "maxMonth": 60,
              "monthLabel": "1 - 5 years: Purple packer",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 61,
              "maxMonth": 156,
              "monthLabel": "6 - 13 years: White packet",
              "routine": [
                {
                  "day": "1",
                  "morning": "1 Tablet"
                },
                {
                  "day": "2",
                  "morning": "1 Tablet"
                },
                {
                  "day": "3",
                  "morning": "1 Tablet"
                }
              ]
            },
            {
              "minMonth": 156,
              "maxMonth": 1440,
              "monthLabel": "14 years and above: White packet",
              "routine": [
                {
                  "day": "1",
                  "morning": "2 Tablets"
                },
                {
                  "day": "2",
                  "morning": "2 Tablets"
                },
                {
                  "day": "3",
                  "morning": "2 Tablets"
                }
              ]
            }
          ]
        }
      ]
    }
  ],
  "time": 1658993682722
}',
        'other_symptoms',
        3,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    ),
    (
        '{
  "formLayout": [
    {
      "familyOrder": 0,
      "id": "pncChild",
      "orderId": 1,
      "title": "Child Details",
      "viewType": "CardView"
    },
    {
      "id": "plannedVisitDate",
      "family": "pncChild",
      "fieldName": "plannedVisitDate",
      "menstrualPeriod": true,
      "orderId": 2,
      "title": "Planned Visit Date",
      "viewType": "DatePicker",
      "isSummary": true,
      "disableFutureDate": true,
      "hint": "Select Date",
      "errorMessage": "Please select a valid option",
      "visibility": "visible"
    },
    {
      "id": "actualVisitDate",
      "family": "pncChild",
      "fieldName": "actualVisitDate",
      "menstrualPeriod": true,
      "orderId": 3,
      "title": "Actual Visit Date",
      "viewType": "DatePicker",
      "isSummary": true,
      "disableFutureDate": true,
      "hint": "Select Date",
      "errorMessage": "Please select a valid option",
      "visibility": "visible"
    },
    {
      "id": "childhoodVisitSigns",
      "fieldName": "childhoodVisitSigns",
      "family": "pncChild",
      "orderId": 4,
      "titleSummary": "Danger Signs in newborn baby",
      "viewType": "DialogCheckbox",
      "localDataCache": "childhoodVisitSigns",
      "isMandatory": false,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "title": "Is the newborn baby experiencing any of these danger signs?",
      "visibility": "visible",
      "isSummary": true,
      "condition": [
        {
          "eq": "Other",
          "targetId": "otherChildhoodVisitSigns",
          "visibility": "visible"
        }
      ]
    },
    {
      "errorMessage": "Please enter a valid sign",
      "family": "pncChild",
      "fieldName": "Other Signs",
      "hint": "Enter Other Sign",
      "id": "otherChildhoodVisitSigns",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "orderId": 5,
      "isSummary": false,
      "title": "Other Signs",
      "viewType": "EditText",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "family": "pncChild",
      "fieldName": "fatherPresent",
      "id": "fatherPresent",
      "condition": [],
      "orderId": 6,
      "isBooleanAnswer": true,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Father present?",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "exclusivelyBreastfeeding",
      "id": "exclusivelyBreastfeeding",
      "condition": [],
      "orderId": 7,
      "isBooleanAnswer": true,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Exclusively breastfeeding?",

"titleSummary": "Danger Signs in mother",      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "sleepsUnderBedNet",
      "id": "sleepsUnderBedNet",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 8,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Sleep inside the bednet?",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "muac",
      "id": "muac",
      "viewType": "Spinner",
      "orderId": 9,
      "isSummary": true,
      "isEnabled": true,
      "title": "MUAC",
      "isInfo": "visible",
      "infoTitle": "Job Aid (MUAC)",
      "optionsList": [
        {
          "id": "Green",
          "name": "Green"
        },
        {
          "id": "Yellow",
          "name": "Yellow"
        },
        {
          "id": "Red",
          "name": "Red"
        }
      ],
      "errorMessage": "Please select a valid option"
    },
    {
      "family": "pncChild",
      "id": "muacStatus",
      "title": "MUAC",
      "backgroundColor": "#FDF4F0",
      "visibility": "visible",
      "viewType": "InformationLabel"
    },
    {
      "family": "pncChild",
      "fieldName": "takingMinimumMealsPerDay",
      "id": "takingMinimumMealsPerDay",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 10,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Taking minimum meals per day",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "fedFrom4FoodGroups",
      "id": "fedFrom4FoodGroups",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 11,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Fed from 4 food groups",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "pentaOpvGiven",
      "id": "pentaOpvGiven",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 12,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Penta/OPV 1, 2, 3 given",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "measles1Given",
      "id": "measles1Given",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 13,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Measles 1 given",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "yellowFeverVacineGiven",
      "id": "yellowFeverVacineGiven",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 14,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Yellow fever vaccine given",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "measles2Given",
      "id": "measles2Given",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 15,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Measles 2 given",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "postReferralFollowUpDone ",
      "id": "postReferralFollowUpDone",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 16,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Post referral followup done ",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "motherOrPartnerUsingFamilyPlanning",
      "id": "motherOrPartnerUsingFamilyPlanning",
      "condition": [],
      "orderId": 17,
      "isBooleanAnswer": true,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Mother / Partner using family planning",
      "visibility": "visible"
    },
    {
      "family": "pncChild",
      "fieldName": "deathOfBaby",
      "id": "deathOfBaby",
      "condition": [],
      "orderId": 18,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isBooleanAnswer": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "viewType": "SingleSelectionView",
      "title": "Death of baby",
      "visibility": "visible"
    }
  ],
  "time": 1658993682722
}',
        'pncChild',
        4,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    ),
    (
        '{
  "formLayout": [
    {
      "familyOrder": 0,
      "id": "pncMother",
      "orderId": 1,
      "title": "PNC Visit - Mother",
      "viewType": "CardView"
    },
    {
      "id": "dateOfDelivery",
      "family": "pncMother",
      "fieldName": "dateOfDelivery",
      "orderId": 2,
      "title": "Date of Delivery",
      "viewType": "DatePicker",
      "isMandatory": true,
      "disableFutureDate": true,
      "hint": "Select Date",
      "minDays": 14,
      "errorMessage": "Please select a valid option",
      "visibility": "visible"
    },
    {
      "family": "pncMother",
      "fieldName": "noOfNeonates",
      "id": "noOfNeonates",
      "viewType": "EditText",
      "orderId": 6,
      "inputType": 3,
      "hint": "Enter number",
      "contentLength": 1,
      "titleSummary": "No of Neonate",
      "isEnabled": true,
      "isMandatory": true,
      "title": "No of Neonate",
      "errorMessage": "Please enter a valid option"
    },
    {
      "id": "pncMotherSigns",
      "family": "pncMother",
      "fieldName": "pncMotherSigns",
      "orderId": 2,
      "title": "Is the expectant mother experiencing any of these danger signs?",
      "viewType": "DialogCheckbox",
      "isMandatory": false,
      "isSummary": true,
      "titleSummary": "Danger Signs in mother",
      "hint": "Select Symptoms",
      "localDataCache": "pncMotherSigns",
      "errorMessage": "Please select a valid option",
      "visibility": "visible",
      "condition": [
        {
          "eq": "Other",
          "targetId": "otherPncMotherSigns",
          "visibility": "visible"
        }
      ]
    },
    {
      "errorMessage": "Please enter a valid sign",
      "family": "pncMother",
      "fieldName": "Other Signs",
      "hint": "Enter Other Sign",
      "id": "otherPncMotherSigns",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "orderId": 5,
      "isSummary": false,
      "title": "Other Signs",
      "viewType": "EditText",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "family": "pncMother",
      "fieldName": "fatherPresent",
      "id": "fatherPresent",
      "condition": [

      ],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "isBooleanAnswer": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Father present?",
      "visibility": "visible"
    },
    {
      "family": "pncMother",
      "fieldName": "exclusivelyBreastfeeding",
      "id": "exclusivelyBreastfeeding",
      "condition": [

      ],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isBooleanAnswer": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Exclusively breastfeeding?",
      "visibility": "visible"
    },
    {
      "family": "pncMother",
      "fieldName": "sleepsUnderBedNet",
      "id": "sleepsUnderBedNet",
      "condition": [

      ],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isBooleanAnswer": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Sleep inside the bednet?",
      "visibility": "visible"
    },
    {
      "family": "pncMother",
      "fieldName": "chlorhexidine",
      "id": "chlorhexidine",
      "condition": [

      ],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isBooleanAnswer": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Chlorhexidine",
      "visibility": "visible"
    }
  ],
  "time": 1658993682722
}',
        'pncMother',
        4,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    ),
    (
        '{
  "formLayout": [
    {
      "familyOrder": 0,
      "id": "anc",
      "orderId": 1,
      "title": "ANC",
      "viewType": "CardView"
    },
    {
      "family": "anc",
      "fieldName": "deathOfMother",
      "id": "deathOfMother",
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isBooleanAnswer": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "condition": [
        {
          "eq": "No",
          "targetId": "lastMenstrualPeriod",
          "visibility": "gone"
        },
        {
          "eq": "No",
          "targetId": "ancSigns",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "otherAncSigns",
          "visibility": "gone"
        },
        {
          "eq": "No",
          "targetId": "isMalePartnerPresent",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "sleepsUnderBedNet",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "eatsMoreThanBefore",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "eats4GroupIronVitARichFoods",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "takesIronFloatTablets",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "takesFancidarTablets",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "priorityPregnancy",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "miscarriage",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "birthPlanMade",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "placeOfDelivery",
          "visibility": "visible"
        },
        {
          "eq": "No",
          "targetId": "otherPlaceOfDelivery",
          "visibility": "gone"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Death of mother",
      "visibility": "visible"
    },
    {
      "id": "lastMenstrualPeriod",
      "family": "anc",
      "fieldName": "lastMenstrualPeriod",
      "orderId": 2,
      "disableFutureDate": true,
      "title": "Last Menstrual Period",
      "viewType": "DatePicker",
      "isMandatory": true,
      "isSummary": true,
      "hint": "Select Date",
      "errorMessage": "Please select a valid option",
      "visibility": "gone",
      "menstrualPeriod": true
    },
    {
      "id": "ancSigns",
      "fieldName": "ancSigns",
      "family": "anc",
      "orderId": 3,
      "isSummary": true,
      "titleSummary": "Danger signs in mother",
      "viewType": "DialogCheckbox",
      "localDataCache": "motherDangerSign",
      "isMandatory": false,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "title": "Is the expectant mother experiencing any of these danger signs?",
      "visibility": "gone",
      "condition": [
        {
          "eq": "Other",
          "targetId": "otherAncSigns",
          "visibility": "visible"
        }
      ]
    },
    {
      "errorMessage": "Please enter a valid sign",
      "family": "anc",
      "fieldName": "Other Signs",
      "hint": "Enter Other Sign",
      "id": "otherAncSigns",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "orderId": 5,
      "isSummary": false,
      "title": "Other Signs",
      "viewType": "EditText",
      "isInfo": "gone",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "isMalePartnerPresent",
      "id": "isMalePartnerPresent",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Male Partner Present?",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "sleepsUnderBedNet",
      "id": "sleepsUnderBedNet",
      "condition": [],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isBooleanAnswer": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Sleep inside the bednet?",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "eatsMoreThanBefore",
      "id": "eatsMoreThanBefore",
      "condition": [],
      "orderId": 4,
      "isBooleanAnswer": true,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Eats more than before?",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "eats4GroupIronVitARichFoods",
      "id": "eats4GroupIronVitARichFoods",
      "condition": [],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "titleSummary": "4 food groups+  iron-rich & vit-A rich foods?",
      "isNeededDefault": true,
      "isSummary": true,
      "isBooleanAnswer": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Eats from 4 food groups plus iron-rich and vitamin-A rich foods?",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "takesIronFloatTablets",
      "id": "takesIronFloatTablets",
      "condition": [],
      "isBooleanAnswer": true,
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Takes iron-folate tablets",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "takesFancidarTablets",
      "id": "takesFancidarTablets",
      "condition": [],
      "orderId": 4,
      "isBooleanAnswer": true,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Takes Fancidar tablets",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "priorityPregnancy",
      "id": "priorityPregnancy",
      "condition": [],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isBooleanAnswer": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Priority pregnancy",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "miscarriage",
      "id": "miscarriage",
      "condition": [],
      "orderId": 4,
      "isBooleanAnswer": true,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Miscarriage/Stillbirth",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "birthPlanMade",
      "id": "birthPlanMade",
      "condition": [],
      "orderId": 4,
      "errorMessage": "Please select a valid option",
      "isEnabled": true,
      "isMandatory": true,
      "isBooleanAnswer": true,
      "isNeededDefault": true,
      "isSummary": true,
      "optionsList": [
        {
          "id": true,
          "name": "Yes"
        },
        {
          "id": false,
          "name": "No"
        }
      ],
      "optionType": "boolean",
      "readOnly": true,
      "viewType": "SingleSelectionView",
      "title": "Birth plan made",
      "visibility": "gone"
    },
    {
      "family": "anc",
      "fieldName": "placeOfDelivery",
      "id": "placeOfDelivery",
      "viewType": "Spinner",
      "condition": [
        {
          "eq": "Others",
          "targetId": "otherPlaceOfDelivery",
          "visibility": "visible"
        }
      ],
      "orderId": 6,
      "isSummary": true,
      "titleSummary": "Place of delivery",
      "localDataCache": "placeOfDelivery",
      "isEnabled": true,
      "isMandatory": false,
      "title": "Place of delivery (Intended per birth plan)",
      "errorMessage": "Please select a valid option",
      "visibility": "gone"
    },
    {
      "errorMessage": "Please enter a valid place of delivery",
      "family": "anc",
      "fieldName": "otherPlaceOfDelivery",
      "hint": "Enter Other Place of delivery",
      "id": "otherPlaceOfDelivery",
      "inputType": 96,
      "isEnabled": true,
      "isMandatory": true,
      "isNeededDefault": true,
      "isNotDefault": false,
      "orderId": 5,
      "isSummary": false,
      "title": "Other Place of delivery",
      "viewType": "EditText",
      "isInfo": "gone",
      "visibility": "gone"
    }
  ],
  "time": 1658993682722
}',
        'anc',
        4,
        true,
        false,
        0,
        0,
        '{COMMUNITY}'
    );