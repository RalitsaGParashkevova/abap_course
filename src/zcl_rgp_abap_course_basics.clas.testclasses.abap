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
    METHODS test_date_parsing FOR TESTING.

* Test methods scrabble_score
    METHODS test_scrabble_score_val_input FOR TESTING.
    METHODS test_scrabble_score_empty_inpt FOR TESTING.
    METHODS test_scrabble_score_num_input FOR TESTING.
    METHODS test_scrabble_score FOR TESTING.


*Test method fizz_buzz
    METHODS test_fizz_buzz FOR TESTING.

    METHODS test_hello_world FOR TESTING.

*test method get_current_date_time
    METHODS test_get_current_date_time FOR TESTING.

    METHODS test_task7_agency_booking_jpy FOR TESTING.
    METHODS test_task7_price_higher_usd FOR TESTING.
    METHODS test_task7_delete_non_eur_sort FOR TESTING.
    METHODS test_task7_first_travel_ids FOR TESTING.

    METHODS test_task8_agency_booking_jpy FOR TESTING.
    METHODS test_task8_price_higher_usd FOR TESTING.
    METHODS test_task8_first_10_travel_ids FOR TESTING.
    METHODS test_task8_incor_10_travel_ids FOR TESTING.


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
    iv_operator = '/' ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lv_result ).


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

  METHOD test_task8_first_10_travel_ids.
    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.
    TYPES tty_ztravel_rp TYPE TABLE OF ztravel_rp.
    DATA lt_result_travel_ids_task8 TYPE TABLE OF  lts_travel_id.
    cut->zif_abap_course_basics~open_sql(
     IMPORTING
     et_travel_ids_task8_3 = lt_result_travel_ids_task8
     ).
    cl_abap_unit_assert=>assert_not_initial(
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

  METHOD test_get_current_date_time.
    DATA lv_result TYPE timestampl.

    " Act: Call the method under test.
    lv_result = cut->zif_abap_course_basics~get_current_date_time( ).

    " Assert:  Check that the result is not initial.  We can't predict the exact timestamp,
    "          but we can be sure it's not the initial value.
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Test Failed: get_current_date_time should return a non-initial timestamp.'
    ).
  ENDMETHOD.

  METHOD test_scrabble_score.
    " Arrange: Define test data.  Include various cases: empty string,
    "          lowercase, uppercase, mixed case, and a word with repeated letters.
    TYPES: BEGIN OF lt_test_cases_input,
             lv_input          TYPE string,
             lv_expected_score TYPE i,
           END OF lt_test_cases_input.
    DATA lt_test_cases TYPE TABLE OF lt_test_cases_input.

    APPEND VALUE #( lv_input = '' lv_expected_score = 0 ) TO lt_test_cases.
    APPEND VALUE #( lv_input = 'hello' lv_expected_score = 52 ) TO lt_test_cases.
    APPEND VALUE #( lv_input = 'Hello' lv_expected_score = 52 ) TO lt_test_cases.
    APPEND VALUE #( lv_input = 'Rali' lv_expected_score = 40 ) TO lt_test_cases.
    APPEND VALUE #( lv_input = 'abcdefghijklmnopqrstuvwxyz' lv_expected_score = 351 ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '12345' lv_expected_score = 0 ) TO lt_test_cases. "Added test case to cover the ELSE part of the IF statement


    " Act & Assert:  Loop through test cases and assert the result for each.
    LOOP AT lt_test_cases INTO DATA(test_case).

      DATA lv_result TYPE i.

      lv_result = cut->zif_abap_course_basics~scrabble_score( iv_word = test_case-lv_input ).

      cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = test_case-lv_expected_score
      msg = |Test Failed: scrabble_score with input '{ test_case-lv_input }' should return '{ test_case-lv_expected_score }'.| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_task8_incor_10_travel_ids.
    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.

    DATA lt_result_travel_ids_task8 TYPE TABLE OF  lts_travel_id.

    DATA lt_incorrect_count_ids_task8_3 TYPE TABLE OF  lts_travel_id.

    cut->zif_abap_course_basics~open_sql(
     IMPORTING
      et_travel_ids_task8_3 = lt_result_travel_ids_task8
      ).

    DATA(lv_rows_method_table) = lines( lt_result_travel_ids_task8 ).
    DATA(lv_rows_test_table) = lines( lt_incorrect_count_ids_task8_3 ).

    DATA is_differ TYPE abap_bool VALUE 'X'.
    IF lv_rows_method_table > lv_rows_test_table.
      is_differ = abap_false.
    ENDIF.
    cl_abap_unit_assert=>assert_false(
     act = is_differ
     msg = 'Test Failed: The rows of test table are equal to method table.'
   ).

  ENDMETHOD.
  METHOD test_date_parsing.
    " Arrange: Define test data with various date formats.
    TYPES: BEGIN OF lt_test_cases_input,
             lv_input           TYPE string,
             lv_expected_format TYPE i,
           END OF lt_test_cases_input.
    DATA lt_test_cases TYPE TABLE OF lt_test_cases_input.

    " Test with month names (all lowercase)
    APPEND VALUE #( lv_input = '01 january 2023' lv_expected_format = '20230101' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '15 february 2024' lv_expected_format = '20240215' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '28 march 2025' lv_expected_format = '20250328' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '05 april 2026' lv_expected_format = '20260405' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '10 may 2027' lv_expected_format = '20270510' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '22 june 2028' lv_expected_format = '20280622' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '03 july 2029' lv_expected_format = '20290703' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '18 august 2030' lv_expected_format = '20300818' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '01 september 2031' lv_expected_format = '20310901' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '12 october 2032' lv_expected_format = '20321012' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '25 november 2033' lv_expected_format = '20331125' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '31 december 2034' lv_expected_format = '20341231' ) TO lt_test_cases.

    "test with month number
    APPEND VALUE #( lv_input = '01 01 2023' lv_expected_format = '20230101' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '15 02 2024' lv_expected_format = '20240215' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '28 03 2025' lv_expected_format = '20250328' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '05 04 2026' lv_expected_format = '20260405' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '10 05 2027' lv_expected_format = '20270510' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '22 06 2028' lv_expected_format = '20280622' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '03 07 2029' lv_expected_format = '20290703' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '18 08 2030' lv_expected_format = '20300818' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '01 09 2031' lv_expected_format = '20310901' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '12 10 2032' lv_expected_format = '20321012' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '25 11 2033' lv_expected_format = '20331125' ) TO lt_test_cases..
    APPEND VALUE #( lv_input = '31 12 2034' lv_expected_format = '20341231' ) TO lt_test_cases.


    " Test with month number without leading zero
    APPEND VALUE #( lv_input = '01 1 2023' lv_expected_format = '20230101' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '15 2 2024' lv_expected_format = '20240215' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '28 3 2025' lv_expected_format = '20250328' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '05 4 2026' lv_expected_format = '20260405' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '10 5 2027' lv_expected_format = '20270510' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '22 6 2028' lv_expected_format = '20280622' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '03 7 2029' lv_expected_format = '20290703' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '18 8 2030' lv_expected_format = '20300818' ) TO lt_test_cases.
    APPEND VALUE #( lv_input = '01 9 2031' lv_expected_format = '20310901' ) TO lt_test_cases.


    " Act & Assert:  Loop through test cases and assert the result for each.
    LOOP AT lt_test_cases INTO DATA(test_case).
      DATA lv_result TYPE string.
      lv_result = cut->zif_abap_course_basics~date_parsing( iv_date = test_case-lv_input ).
      cl_abap_unit_assert=>assert_equals(
        act = lv_result
        exp = test_case-lv_expected_format
        msg = |Test Failed: date_parsing with input '{ test_case-lv_input }' should return '{ test_case-lv_expected_format }'.|
      ).
    ENDLOOP.
  ENDMETHOD.



ENDCLASS.
