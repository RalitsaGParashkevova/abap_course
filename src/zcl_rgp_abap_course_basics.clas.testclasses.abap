*"* use this source file for your ABAP unit test classes
CLASS ltcl_ DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_rgp_abap_course_basics. " Class Under Test
    METHODS setup.

*Test methods calculator
    METHODS test_calculator_input_valid FOR TESTING.

    METHODS test_calculator_addition FOR TESTING.
    METHODS test_calculator_subtraction FOR TESTING.
    METHODS test_calculator_multiplication FOR TESTING.
    METHODS test_calculator_division FOR TESTING.

*Test methods date_parsing
    METHODS test_date_parsing_numeric FOR TESTING.
    METHODS test_date_parsing_aplhab_month FOR TESTING.
    METHODS test_date_parsing_inval_input FOR TESTING.
    METHODS test_date_parsing_day_numeric FOR TESTING.
    METHODS test_date_parsing_num_month FOR TESTING.
    METHODS test_date_parsing_invalid_year FOR TESTING.
    METHODS test_date_parsing_invalid_day FOR TESTING.
    METHODS test_date_parsing_inval_month FOR TESTING.
    METHODS test_date_parsing_leap_year FOR TESTING.
    METHODS test_date_parsing_no_leap_year FOR TESTING.

* Test methods scrabble_score
    METHODS test_scrabble_score_val_input FOR TESTING.
    METHODS test_scrabble_score_empty_inpt FOR TESTING.
    METHODS test_scrabble_score_num_input FOR TESTING.


*Test method fizz_buzz
    METHODS test_fizz_buzz FOR TESTING.

    METHODS test_hello_world FOR TESTING.



    METHODS test_task7_agency_booking_jpy FOR TESTING.
    METHODS test_task7_price_higher_usd FOR TESTING.
    METHODS test_task7_delete_non_eur_sort FOR TESTING.
    METHODS test_task7_first_travel_ids FOR TESTING.

    METHODS test_task8_agency_booking_jpy FOR TESTING.
    METHODS test_task8_price_higher_usd FOR TESTING.
    METHODS test_task8_first_travel_ids FOR TESTING.

ENDCLASS.


CLASS ltcl_ IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_rgp_abap_course_basics( ).
  ENDMETHOD.



* Calculator tests implementation
  METHOD test_calculator_addition.
    DATA lv_result TYPE i.
    lv_result = cut->zif_abap_course_basics~calculator(
    iv_first_number = 5
    iv_second_number = 3
    iv_operator = '+' ).

    cl_abap_unit_assert=>assert_equals( exp = 8 act = lv_result ).
  ENDMETHOD.

  METHOD test_calculator_division.
    DATA lv_result TYPE i.
    lv_result = cut->zif_abap_course_basics~calculator(
    iv_first_number = 6
    iv_second_number = 3
    iv_operator = '-' ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lv_result ).


  ENDMETHOD.

  METHOD test_calculator_input_valid.
    " Local structure for test data
    TYPES: BEGIN OF validate_input_test_case,
             iv_first_number  TYPE i,
             iv_second_number TYPE i,
             iv_operator      TYPE c LENGTH 1,
             expected_message TYPE string,
             message_check    TYPE abap_bool, "Flag if message should be checked
           END OF validate_input_test_case.

    DATA test_cases TYPE TABLE OF validate_input_test_case.

    " Populate the test table
    APPEND VALUE #( iv_first_number  = 5  iv_second_number = 3  iv_operator      = '+' expected_message = ''                                                                       ) TO test_cases. "Valid Case
    APPEND VALUE #( iv_first_number  = 5  iv_second_number = 3  iv_operator      = '%' expected_message = |Error: Invalid operator "%".|                                          ) TO test_cases. " Invalid Operator
    APPEND VALUE #( iv_first_number  = 5  iv_second_number = 0  iv_operator      = '/' expected_message = 'Error: Division by zero is not allowed!'                               ) TO test_cases. " Division by Zero
    APPEND VALUE #( iv_first_number  = 5  iv_second_number = 0  iv_operator      = '%' expected_message = |Error: Division by zero is not allowed and the operator "%" is invalid.| ) TO test_cases. "Both
    APPEND VALUE #( iv_first_number  = 1  iv_second_number = 0  iv_operator      = '+' expected_message = 'Error: Division by zero is not allowed!'                               ) TO test_cases. "second number is invalid number
    APPEND VALUE #( iv_first_number  = 1  iv_second_number = 0  iv_operator      = 'a' expected_message = |Error: Division by zero is not allowed and the operator "a" is invalid.| ) TO test_cases. "second number is invalid number and operator is invalid

    LOOP AT test_cases INTO DATA(test_case).
      DATA lv_message TYPE string.
      cut->validate_calculator_input(
       EXPORTING
          iv_first_number  = test_case-iv_first_number
          iv_second_number = test_case-iv_second_number
          iv_operator      = test_case-iv_operator

          IMPORTING ev_message = lv_message
      ).
      cl_abap_unit_assert=>assert_equals(
          act = lv_message
          exp = test_case-expected_message
          msg = |Test Failed: Input: { test_case-iv_first_number } { test_case-iv_second_number } { test_case-iv_operator }|
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD test_calculator_multiplication.
    DATA lv_result TYPE i.
    lv_result = cut->zif_abap_course_basics~calculator(
    iv_first_number = 5
    iv_second_number = 3
    iv_operator = '*' ).

    cl_abap_unit_assert=>assert_equals( exp = 15 act = lv_result ).

  ENDMETHOD.

  METHOD test_calculator_subtraction.
    DATA lv_result TYPE i.
    lv_result = cut->zif_abap_course_basics~calculator(
    iv_first_number = 5
    iv_second_number = 3
    iv_operator = '-' ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lv_result ).

  ENDMETHOD.

  METHOD test_date_parsing_numeric.
    DATA lv_result TYPE dats.
    lv_result = cut->zif_abap_course_basics~date_parsing( iv_date = '15 04 2025' ).
    cl_abap_unit_assert=>assert_equals( act = lv_result
                                        exp = '20250415'
                                        ).
  ENDMETHOD.

  METHOD test_fizz_buzz.
    DATA: rv_result        TYPE string,
          lt_result_lines  TYPE TABLE OF string,
          lv_expected_line TYPE string.

    " Call the fizz_buzz method
    rv_result = cut->zif_abap_course_basics~fizz_buzz( ).

    " Split the result into individual lines for validation
    SPLIT rv_result AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_result_lines.

    " Loop through expected results and compare
    DO 100 TIMES.
      DATA(lv_index) = sy-index.

      IF lv_index MOD 15 = 0.
        lv_expected_line = 'FizzBuzz'.
      ELSEIF lv_index MOD 3 = 0.
        lv_expected_line = 'Fizz'.
      ELSEIF lv_index MOD 5 = 0.
        lv_expected_line = 'Buzz'.
      ELSE.
        lv_expected_line = lv_index.
      ENDIF.

      " Read corresponding line from method output
      READ TABLE lt_result_lines INDEX lv_index INTO lv_expected_line.

      " Assert for each line
      cl_abap_unit_assert=>assert_equals(
        act = lv_expected_line
        exp = lv_expected_line
        msg = |Validation failed at index { lv_index }|
      ).
    ENDDO.
  ENDMETHOD.

  METHOD test_hello_world.

    DATA: lv_name           TYPE string VALUE 'John',
          lv_result         TYPE string,
          lv_expected       TYPE string,
          lv_user_system_id TYPE sy-uname.

    " Simulate system user ID
    lv_user_system_id = sy-uname.

    " Expected result
    CONCATENATE 'Hello , ' lv_name '. Your user ID is: ' lv_user_system_id INTO lv_expected.

    " Call the method
    lv_result = cut->zif_abap_course_basics~hello_world( EXPORTING iv_name = lv_name ).

    " Assert that the result matches the expected output
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = lv_expected
      msg = 'hello_world method failed to generate correct result'
    ).
  ENDMETHOD.

  METHOD test_scrabble_score_val_input.
    DATA(lv_input_valid) = abap_false.
    cut->validate_input_scrabble_score(
    EXPORTING
      iv_word_input = 'abc'
      IMPORTING iv_input_valid = lv_input_valid
    ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_input_valid
      exp = abap_true
      msg = 'Valid word should return abap_true.'
    ).
  ENDMETHOD.

  METHOD test_date_parsing_day_numeric.
    DATA(lv_valid) = abap_false.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = '32 04 2025'
                                IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( act = lv_valid
                                       msg = 'Test Failed: Invalid day (numeric) should be false.' ).
  ENDMETHOD.

  METHOD test_date_parsing_invalid_day.
    DATA(lv_valid) = abap_true.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = 'AA April 2025'
                                 IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( act = lv_valid
                                       msg = 'Test Failed: Invalid day (alpha) should be false.' ).
  ENDMETHOD.

  METHOD test_date_parsing_num_month.
    DATA(lv_valid) = abap_true.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = '15 13 2025'
                                 IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( act = lv_valid
                                       msg = 'Test Failed: Invalid month (numeric) should be false.' ).
  ENDMETHOD.

  METHOD test_date_parsing_inval_month.
    DATA(lv_valid) = abap_true.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = '15 Foo 2025'
                                 IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( act = lv_valid
                                       msg = 'Test Failed: Invalid month (alpha) should be false.' ).
  ENDMETHOD.

  METHOD test_date_parsing_invalid_year.
    DATA(lv_valid) = abap_true.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = '15 04 25'
                                 IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( act = lv_valid
                                       msg = 'Test Failed: Invalid year should be false.' ).
  ENDMETHOD.

  METHOD test_date_parsing_leap_year.
    DATA(lv_valid) = abap_false.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = '29 February 2024'
                                 IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_true( act = lv_valid
                                       msg = 'Test Failed: Valid leap year date should be true (current logic is wrong).' ).
  ENDMETHOD.

  METHOD test_date_parsing_no_leap_year.
    DATA(lv_valid) = abap_true.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = '29 February 2025'
                                 IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( act = lv_valid
                                       msg = 'Test Failed: Invalid non-leap year date should be false (current logic is wrong).' ).
  ENDMETHOD.

*  METHOD test_scrabble_score_no_letters.
*
*  ENDMETHOD.

  METHOD test_task7_agency_booking_jpy.

    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    DATA lt_result_travel_ids_task1 TYPE TABLE OF  lts_travel_id.
    cut->zif_abap_course_basics~internal_tables(
    IMPORTING
      et_travel_ids_task7_1 = lt_result_travel_ids_task1
    ).

    cl_abap_unit_assert=>assert_not_initial(
     act = lt_result_travel_ids_task1
     msg = 'internal table task 7.1 is incorrect.'
   ).
  ENDMETHOD.

  METHOD test_task7_delete_non_eur_sort.

    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    DATA lt_result_travel_ids_task3 TYPE TABLE OF  lts_travel_id.
    cut->zif_abap_course_basics~internal_tables(
   IMPORTING
     et_travel_ids_task7_3 = lt_result_travel_ids_task3
   ).
    cl_abap_unit_assert=>assert_not_initial(
     act = lt_result_travel_ids_task3
     msg = 'internal table task 7.3 is incorrect.'
   ).
  ENDMETHOD.

  METHOD test_task7_first_travel_ids.
    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    DATA lt_result_travel_ids_task7 TYPE TABLE OF  lts_travel_id.

    cut->zif_abap_course_basics~internal_tables(
    IMPORTING
      et_travel_ids_task7_3 = lt_result_travel_ids_task7
    ).
    cl_abap_unit_assert=>assert_not_initial(
     act = lt_result_travel_ids_task7
     msg = 'internal table task 7.3 is incorrect.'
   ).
  ENDMETHOD.

  METHOD test_task7_price_higher_usd.
    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    DATA lt_result_travel_ids_task7 TYPE TABLE OF  lts_travel_id.
    cut->zif_abap_course_basics~internal_tables(
   IMPORTING
     et_travel_ids_task7_2 = lt_result_travel_ids_task7
   ).

    cl_abap_unit_assert=>assert_not_initial(
     act = lt_result_travel_ids_task7
     msg = 'internal table task 7.2 is incorrect.'
   ).
  ENDMETHOD.

  METHOD test_task8_agency_booking_jpy.
    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    TYPES tty_ztravel_rp TYPE TABLE OF ztravel_rp.
    DATA lt_result_travel_ids_task8 TYPE TABLE OF  lts_travel_id.
    DATA: mt_travel_data TYPE tty_ztravel_rp.

    cut->zif_abap_course_basics~open_sql(
     IMPORTING
     et_travel_ids_task8_1 = lt_result_travel_ids_task8
     ).

    cl_abap_unit_assert=>assert_initial(
act = mt_travel_data
msg = 'open sql task 8.1 is incorrect.'
).

  ENDMETHOD.

  METHOD test_task8_first_travel_ids.
TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    DATA lt_result_travel_ids_task8 TYPE TABLE OF  lts_travel_id.
    cut->zif_abap_course_basics~open_sql(
     IMPORTING
     et_travel_ids_task8_3 = lt_result_travel_ids_task8
     ).
           cl_abap_unit_assert=>assert_initial(
      act = lt_result_travel_ids_task8
      msg = 'open sql task 8.3 is incorrect.'
    ).
  ENDMETHOD.

  METHOD test_task8_price_higher_usd.
TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    DATA lt_result_travel_ids_task8 TYPE TABLE OF  lts_travel_id.

    cut->zif_abap_course_basics~open_sql(
     IMPORTING
     et_travel_ids_task8_2 = lt_result_travel_ids_task8
     ).
          cl_abap_unit_assert=>assert_not_initial(
      act = lt_result_travel_ids_task8
      msg = 'open sql task 8.2 is incorrect.'
    ).
  ENDMETHOD.

  METHOD test_scrabble_score_empty_inpt.
    DATA(lv_input_valid) = abap_true.
    cut->validate_input_scrabble_score(
    EXPORTING
      iv_word_input = ''
      IMPORTING iv_input_valid = lv_input_valid
    ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_input_valid
      exp = abap_false
      msg = 'Empty word should return abap_false.'
    ).
  ENDMETHOD.

  METHOD test_scrabble_score_num_input.
    DATA(lv_input_valid) = abap_true.
    cut->validate_input_scrabble_score(
    EXPORTING
      iv_word_input = '123'
      IMPORTING iv_input_valid = lv_input_valid
    ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_input_valid
      exp = abap_false
      msg = 'Numeric word should return abap_false.'
    ).
  ENDMETHOD.

  METHOD test_date_parsing_inval_input.
    DATA lv_valid TYPE abap_bool.
    cut->validate_date_parsing(
    EXPORTING
    iv_input_string_date = '15-04-2025'
                                IMPORTING iv_input_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( act = lv_valid
                                       msg = 'Test Failed: Invalid format should be false.' ).

  ENDMETHOD.

  METHOD test_date_parsing_aplhab_month.
    DATA lv_result TYPE dats.
    lv_result = cut->zif_abap_course_basics~date_parsing( iv_date = '15 April 2025' ).
    cl_abap_unit_assert=>assert_equals( act = lv_result
                                        exp = '20250415'
                                         ).
  ENDMETHOD.

ENDCLASS.
