CLASS zcl_rgp_abap_course_basics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    INTERFACES zif_abap_course_basics .
* Method validate_input is declared in Public section due to be accessed by Test Cases
    METHODS validate_calculator_input
      IMPORTING
        iv_first_number  TYPE i
        iv_second_number TYPE i
        iv_operator      TYPE c
      EXPORTING
        ev_message       TYPE string.

    METHODS validate_date_parsing
      IMPORTING
        iv_input_string_date TYPE string

      EXPORTING
        iv_input_valid       TYPE abap_bool.

    METHODS validate_input_scrabble_score
      IMPORTING
        iv_word_input  TYPE string
      EXPORTING
        iv_input_valid TYPE abap_bool.


  PRIVATE SECTION.
    CONSTANTS: c_operator_add   TYPE c VALUE '+',
               c_operator_sub   TYPE c VALUE '-',
               c_operator_mul   TYPE c VALUE '*',
               c_operator_div   TYPE c VALUE '/',
               c_invalid_number TYPE i VALUE 0.

    DATA: result_message TYPE string. " Standard string attribute to hold messages
    DATA: result_validation TYPE abap_boolean.

    TYPES: BEGIN OF lts_travel_id,
             travel_id TYPE /dmo/travel_id,
           END OF lts_travel_id.


    DATA lt_result_travel_ids_task1 TYPE TABLE OF  lts_travel_id.
    DATA lt_result_travel_ids_task2  TYPE TABLE OF  lts_travel_id.
    DATA lt_result_travel_ids_task3  TYPE TABLE OF lts_travel_id.


    TYPES tty_ztravel_rp TYPE TABLE OF ztravel_rp.
    DATA: mt_travel_data TYPE tty_ztravel_rp.
    METHODS print_result_table_task7

*IMPORTING lt_internal_table TYPE tty_ztravel_rp
      EXPORTING lt_sorted_table TYPE tty_ztravel_rp.
    DATA lt_result_travel_ids_task8_1 TYPE TABLE OF  lts_travel_id.
    DATA lt_result_travel_ids_task8_2  TYPE TABLE OF  lts_travel_id.
    DATA lt_result_travel_ids_task8_3  TYPE TABLE OF lts_travel_id.


ENDCLASS.



CLASS ZCL_RGP_ABAP_COURSE_BASICS IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    " Input parameters for execution of method calculator "
    DATA lv_first_number TYPE i VALUE 4.
    DATA lv_second_number TYPE i VALUE 2.
    DATA lv_operator TYPE c VALUE '/'.
    DATA lv_result TYPE i.

    " Call the validation method first
    me->validate_calculator_input(
     EXPORTING
       iv_first_number  = lv_first_number
       iv_second_number = lv_second_number
       iv_operator      = lv_operator
     IMPORTING
       ev_message       = result_message ).  " Get the validation message

    IF result_message IS NOT INITIAL.
      " Validation failed, output the error message
      out->write( |Error in calculator input { result_message } | ).
      out->write( '--------------------------------------------------------------------' ).
    ELSE.
      " Validation passed, perform the calculation
      lv_result = me->zif_abap_course_basics~calculator(
          EXPORTING
            iv_first_number  = lv_first_number
            iv_second_number = lv_second_number
            iv_operator      = lv_operator
        ).

      out->write( 'Print result for method calculator' ).
      out->write( lv_result ).  " Output the result
      out->write( '--------------------------------------------------------------------' ).
    ENDIF.


    " Execution of method date_parsing
    DATA lv_input_string_date TYPE string VALUE '28 02 2025'.
    DATA lv_input_valid       TYPE abap_bool.
    DATA lv_date_result      TYPE dats.

*  " Call the validate_date_parsing method first
    me->validate_date_parsing(
       EXPORTING
         iv_input_string_date = lv_input_string_date
       IMPORTING
         iv_input_valid       = lv_input_valid ).

    IF lv_input_valid = abap_false.

      out->write( 'The input is not valid for method date_parsing!' ).
      out->write( '--------------------------------------------------------------------' ).
    ELSE.
      lv_date_result = me->zif_abap_course_basics~date_parsing(
        EXPORTING
          iv_date = lv_input_string_date
      ).
      out->write( 'Print result for method date_parsing' ).
      out->write( lv_date_result ).
      out->write( '--------------------------------------------------------------------' ).
    ENDIF.
    " Execution of method hello_world
    DATA lv_name TYPE string VALUE 'Ralitsa'.
    DATA lv_result_message TYPE string.

    lv_result_message =  me->zif_abap_course_basics~hello_world(
    EXPORTING
    iv_name = lv_name
    ).
    out->write( 'Print result for method hello_world' ).
    out->write( lv_result_message ).
    out->write( '--------------------------------------------------------------------' ).

*" Execution of method scrabble_score.
    DATA lv_word TYPE string VALUE ''.
    DATA lv_input_validation TYPE abap_bool VALUE 'X'.
    DATA lv_result_validation_message TYPE i.

    me->validate_input_scrabble_score(
      EXPORTING
        iv_word_input = lv_word
      IMPORTING
        iv_input_valid = lv_input_validation ).

    IF lv_input_validation = abap_false.
      out->write( 'The input is not valid for method scrabble_score!' ).
      out->write( '--------------------------------------------------------------------' ).
    ELSE.
      lv_result_validation_message =  me->zif_abap_course_basics~scrabble_score(
      EXPORTING
      iv_word = lv_word
      ).
      out->write( 'Print result for method scrabble_score' ).
      out->write( lv_result_validation_message ).
      out->write( '--------------------------------------------------------------------' ).
    ENDIF.

*" Execution of method zif_abap_course_basics~get_current_date_time.
    DATA lv_result_message_date_time TYPE timestampl.
*
    lv_result_message_date_time =  me->zif_abap_course_basics~get_current_date_time( ).
    out->write( 'Print result for method get_current_date_time' ).
    out->write( lv_result_message_date_time ).

    " Execution of method zif_abap_course_basics~fizz_buzz.
    DATA: lv_result_message_fizz_buzz TYPE string.

    lv_result_message_fizz_buzz = me->zif_abap_course_basics~fizz_buzz(  ).
    out->write( 'Print result for method fizz_buzz' ).
    out->write( lv_result_message_fizz_buzz ).
    out->write( '--------------------------------------------------------------------' ).
    me->zif_abap_course_basics~internal_tables(
    IMPORTING
    et_travel_ids_task7_1 = lt_result_travel_ids_task1
     et_travel_ids_task7_2 = lt_result_travel_ids_task2
     et_travel_ids_task7_3 = lt_result_travel_ids_task3
     ).

*Print sorted tables task7

    me->print_result_table_task7(

    IMPORTING
    lt_sorted_table = mt_travel_data
    ).
    out->write( '--------------------------------------------------------------------' ).
    out->write( 'Method should export a table containing all travels(TRAVEL_ID) for agency ( AGENCY_ID) 070001 with booking fee of 20 JPY (BOOKING_FEE CURRENCY_CODE)' ).
    out->write( lt_result_travel_ids_task1 ).
    out->write( '--------------------------------------------------' ).


    out->write( 'Method should export a table containing all travels with a price (TOTAL_PRICE) higher than 2000 USD.)' ).
    out->write( lt_result_travel_ids_task2 ).
    out->write( '--------------------------------------------------' ).
    out->write( 'Delete all rows of the internal table with prices not in Euro, sort them by cheapest price and earliest date. ' ).
    out->write( mt_travel_data ).
    out->write( '--------------------------------------------------' ).
    out->write( 'out->write( lt_result_travel_ids_task7_3' ).
    out->write( lt_result_travel_ids_task3 ).

** Print results task 8
    me->zif_abap_course_basics~open_sql(
    IMPORTING
     et_travel_ids_task8_1 = lt_result_travel_ids_task8_1
     et_travel_ids_task8_2 = lt_result_travel_ids_task8_2
     et_travel_ids_task8_3 = lt_result_travel_ids_task8_3
     ).
    out->write( '--------------------------------------------------------------------' ).
    out->write( 'Task 8_1 Method should export a table containing all travels(TRAVEL_ID) for agency ( AGENCY_ID) 070001 with booking fee of 20 JPY (BOOKING_FEE CURRENCY_CODE)' ).
    out->write( lt_result_travel_ids_task8_1 ).


    out->write( '--------------------------------------------------------------------' ).
    out->write( 'Task 8_2 Method should export a table containing all travels with a price (TOTAL_PRICE) higher than 2000 USD.)' ).
    out->write( lt_result_travel_ids_task8_2 ).

    out->write( '--------------------------------------------------------------------' ).
    out->write( 'Task 8_3 Method should Export a table containing the TRAVEL_ID of the first ten rows to screen.)' ).
    out->write( lt_result_travel_ids_task8_3 ).

  ENDMETHOD.


  METHOD print_result_table_task7.

    mt_travel_data = lt_sorted_table.
  ENDMETHOD.


  METHOD validate_calculator_input.
    " Initialize the message to empty
    ev_message = ''.


    IF  iv_second_number = c_invalid_number  AND
       ( iv_operator <> c_operator_add AND
         iv_operator <> c_operator_sub AND
         iv_operator <> c_operator_mul AND
         iv_operator <> c_operator_div ).
      ev_message = |Error: Division by zero is not allowed and the operator "{ iv_operator }" is invalid.|.
      RETURN. " Exit the method early
    ENDIF.

    IF iv_operator <> '+' AND
       iv_operator <> '-' AND
       iv_operator <> '*' AND
       iv_operator <> '/'.
      ev_message = |Error: Invalid operator "{ iv_operator }".|.
      RETURN. " Exit the method early
    ENDIF.

    IF iv_second_number = 0.
      ev_message = 'Error: Division by zero is not allowed!'.
      RETURN. " Exit the method early
    ENDIF.

  ENDMETHOD.


  METHOD validate_date_parsing.

    DATA: lt_parts  TYPE TABLE OF string.
    DATA: lv_day    TYPE string.
    DATA: lv_month  TYPE string.
    DATA: lv_year   TYPE string.



    SPLIT iv_input_string_date AT space INTO TABLE lt_parts.

    IF lines( lt_parts ) = 3.

      lv_day   = lt_parts[ 1 ].
      lv_month = lt_parts[ 2 ].
      lv_year  = lt_parts[ 3 ].

      iv_input_valid = abap_true.
    ENDIF.
    IF iv_input_valid = abap_true.

      DATA: lv_day_number   TYPE i,
            lv_month_number TYPE i,
            lt_valid_months TYPE TABLE OF string,
            lv_valid        TYPE string.

      " Populate table with valid month names
      APPEND 'January' TO lt_valid_months.
      APPEND 'February' TO lt_valid_months.
      APPEND 'March' TO lt_valid_months.
      APPEND 'April' TO lt_valid_months.
      APPEND 'May' TO lt_valid_months.
      APPEND 'June' TO lt_valid_months.
      APPEND 'July' TO lt_valid_months.
      APPEND 'August' TO lt_valid_months.
      APPEND 'September' TO lt_valid_months.
      APPEND 'October' TO lt_valid_months.
      APPEND 'November' TO lt_valid_months.
      APPEND 'December' TO lt_valid_months.

      "Validate month
      " Validate the month
      READ TABLE lt_valid_months WITH KEY table_line = lv_month INTO lv_valid.
      IF sy-subrc <> 0.
        iv_input_valid = abap_false. " Invalid month name
      ENDIF.
      " Validate day
      IF lv_day CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.

        iv_input_valid = abap_false.
      ELSE.
        lv_day_number = lv_day.
        IF lv_day_number < 1 OR lv_day_number > 31.
          iv_input_valid = abap_false.
        ENDIF.
      ENDIF.
      " Validate month (numeric or alphabetic)
      IF lv_month CN 'JanuaryFebruaryMarchAprilMayJuneJulyAugustSeptemberOctoberNovemberDecember'.
        lv_month_number = lv_month.
        IF lv_month_number < 1 OR lv_month_number > 12.
          iv_input_valid = abap_false.
        ENDIF.
      ENDIF.

      IF lv_year CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.
        iv_input_valid = abap_false.
        " Validate year
      ELSEIF strlen( lv_year ) <> 4.
        iv_input_valid = abap_false.
      ENDIF.

* Check if the year is leap when date is 29 and month is February
      IF lv_month = 'February' OR lv_month_number = 2 OR lv_month_number = 02.
        IF lv_day_number = 29 AND NOT ( lv_year MOD 4 = 0 AND ( lv_year MOD 100 <> 0 OR lv_year MOD 400 = 0 ) ).
          iv_input_valid = abap_false.
        ENDIF.
      ENDIF.

    ELSE.
      iv_input_valid = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD validate_input_scrabble_score.


    IF iv_word_input = ''.
      iv_input_valid = abap_false.
    ELSEIF iv_word_input CA '0123456789'.
      iv_input_valid = abap_false.
    ELSE.
      iv_input_valid = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abap_course_basics~calculator.

    CASE iv_operator.
      WHEN c_operator_add.
        rv_result = iv_first_number + iv_second_number.
      WHEN c_operator_sub.
        rv_result = iv_first_number - iv_second_number.
      WHEN c_operator_mul.
        rv_result = iv_first_number * iv_second_number.
      WHEN c_operator_div.
        rv_result = iv_first_number / iv_second_number.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abap_course_basics~date_parsing.
    DATA: lv_input TYPE string.
    DATA: lv_month_extract TYPE string.
    DATA: lv_day_extract TYPE string.
    DATA: lv_year_extract TYPE string.
    DATA: lt_parts TYPE TABLE OF string.


    lv_input = iv_date. " Copy to a local variable

    SPLIT lv_input AT space INTO TABLE lt_parts.

    IF lines( lt_parts ) = 3.

      lv_day_extract = lt_parts[ 1 ].
      IF strlen( lv_day_extract ) = 1. " Check if month is a single digit
        CONCATENATE '0' lv_day_extract INTO lv_day_extract. " Add leading zero
      ELSE.
        lv_day_extract = lv_day_extract.
      ENDIF.
      lv_month_extract = lt_parts[ 2 ].
      lv_year_extract  = lt_parts[ 3 ].
    ENDIF.



    TRANSLATE lv_month_extract TO LOWER CASE.

    " Extract components.
    IF
    lv_month_extract = 'january'.
      lv_month_extract = '01'.

    ELSEIF lv_month_extract = 'february'.
      lv_month_extract = '02'.

    ELSEIF lv_month_extract = 'march'.
      lv_month_extract = '03'.

    ELSEIF lv_month_extract = 'april'.
      lv_month_extract = '04'.

    ELSEIF lv_month_extract = 'may'.
      lv_month_extract = '05'.

    ELSEIF lv_month_extract = 'june'.
      lv_month_extract = '06'.

    ELSEIF lv_month_extract = 'july'.
      lv_month_extract = '07'.

    ELSEIF lv_month_extract = 'august'.
      lv_month_extract = '08'.

    ELSEIF lv_month_extract = 'september'.
      lv_month_extract = '09'.

    ELSEIF lv_month_extract = 'october'.
      lv_month_extract = '10'.

    ELSEIF lv_month_extract = 'november'.
      lv_month_extract = '11'.

    ELSEIF lv_month_extract = 'december'.
      lv_month_extract = '12'.
    ELSEIF lv_month_extract CO '0123456789'.

      IF strlen( lv_month_extract ) = 1. " Check if month is a single digit
        CONCATENATE '0' lv_month_extract INTO lv_month_extract. " Add leading zero
      ELSE.
        lv_month_extract = lv_month_extract.
      ENDIF.

    ENDIF.
    "YYYYMMDD

    CONCATENATE lv_year_extract lv_month_extract lv_day_extract INTO rv_result.
  ENDMETHOD.


  METHOD zif_abap_course_basics~fizz_buzz.
    DATA: lv_result_line TYPE string.
    DATA: lv_number      TYPE i.

    CLEAR rv_result.

    DO 100 TIMES.
      lv_number = sy-index.

      IF lv_number MOD 15 = 0.
        lv_result_line = 'FizzBuzz'.
      ELSEIF lv_number MOD 3 = 0.
        lv_result_line = 'Fizz'.
      ELSEIF lv_number MOD 5 = 0.
        lv_result_line = 'Buzz'.
      ELSE.
        lv_result_line = lv_number.
      ENDIF.

      IF rv_result IS NOT INITIAL.
        CONCATENATE rv_result cl_abap_char_utilities=>cr_lf lv_result_line INTO rv_result.
      ELSE.
        rv_result = lv_result_line.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD zif_abap_course_basics~get_current_date_time.

    DATA: lv_timestamp TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp.

    rv_result = lv_timestamp.

  ENDMETHOD.


  METHOD zif_abap_course_basics~hello_world.

    DATA: lv_user_system_id TYPE sy-uname.
    lv_user_system_id = sy-uname.

    CONCATENATE 'Hello , ' iv_name '. Your user ID is: ' lv_user_system_id INTO rv_result.

  ENDMETHOD.


  METHOD zif_abap_course_basics~internal_tables.


    SELECT * FROM ztravel_rp INTO TABLE @DATA(lt_travel).

    DELETE ztravel_rp FROM TABLE @lt_travel.
    COMMIT WORK AND WAIT.
    INSERT ztravel_rp FROM
   ( SELECT FROM /dmo/travel
       FIELDS travel_id          AS travel_uuid,
              agency_id        AS agency_id,
              customer_id      AS customer_id,
              begin_date       AS begin_date,
              end_date         AS end_date,
              booking_fee      AS booking_fee,
              total_price      AS total_price,
              currency_code    AS currency_code,
              description      AS description,
              CASE status
            WHEN 'B' THEN  'A'  " ACCEPTED
            WHEN 'X'  THEN 'X' " CANCELLED
                ELSE 'O'         " open
           END                 AS overall_status,
              createdby        AS createdby,
              createdat        AS createdat,
              lastchangedby    AS last_changed_by,
              lastchangedat    AS last_changed_at
       ORDER BY travel_id ).
    COMMIT WORK AND WAIT.


*    " Populate et_travel_ids_task7_1 using LOOP
    LOOP AT lt_travel INTO DATA(ls_travel_id)
         WHERE agency_id = '070001'
           AND booking_fee = 20
           AND currency_code = 'JPY'.
      APPEND ls_travel_id-travel_id TO et_travel_ids_task7_1.
    ENDLOOP.

*
*    " Populate et_travel_ids_task7_2 using LOOP
    LOOP AT lt_travel INTO DATA(ls_total_price)

     WHERE total_price > 2000 AND currency_code = 'USD'.


      APPEND ls_total_price-travel_id TO et_travel_ids_task7_2.

    ENDLOOP.
    " 3. Delete all rows of the internal table with prices not in Euro,
    "    sort them by cheapest price and earliest date.

    LOOP AT lt_travel INTO DATA(ls_travel_price)
    WHERE currency_code <> 'EUR'.
      DELETE lt_travel INDEX sy-tabix.
    ENDLOOP.
    SORT lt_travel BY total_price ASCENDING begin_date DESCENDING.


    " Call the method to export and print the sorted table
    print_result_table_task7(
    IMPORTING
    lt_sorted_table = lt_travel ).

    " 4. Populate et_travel_ids_task7_3: TRAVEL_ID of the first ten rows after sorting
    DATA: lv_counter TYPE i.
    lv_counter = 0.
    LOOP AT lt_travel INTO DATA(ls_travel_rp).
      IF lv_counter >= 10.
        EXIT.
      ENDIF.
      APPEND ls_travel_rp-travel_id TO et_travel_ids_task7_3.
      lv_counter = lv_counter + 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abap_course_basics~open_sql.
*The method should export a table containing all travels(TRAVEL_ID)
*for agency ( AGENCY_ID) 070001 with booking fee of 20 JPY (BOOKING_FEE CURRENCY_CODE)

    SELECT travel_id
      FROM ztravel_rp
      WHERE agency_id = '070001'
        AND booking_fee = 20
        AND currency_code = 'JPY'
      INTO TABLE @DATA(lt_result_travel_ids).


    LOOP AT lt_result_travel_ids INTO DATA(ls_travel).
      APPEND ls_travel-travel_id TO et_travel_ids_task8_1.
    ENDLOOP.

**
*******The method should export a table containing all travels with a price (TOTAL_PRICE) higher than 2000 USD. Hint: Currencies are convertible
    SELECT travel_id
    FROM ztravel_rp
    WHERE total_price > 2000 AND currency_code = 'USD'
    INTO TABLE @DATA(lt_result_travels).

    LOOP AT lt_result_travels INTO DATA(ls_travels).
      APPEND ls_travels-travel_id TO et_travel_ids_task8_2.
    ENDLOOP.

***Export a table containing the TRAVEL_ID of the first ten rows to screen.
    SELECT *
    FROM ztravel_rp
    INTO TABLE @DATA(ls_limit_travels)
    UP TO 10 ROWS.


    DATA: lv_counter TYPE i VALUE 0.

    LOOP AT ls_limit_travels INTO DATA(ls_travel_ids).
      IF lv_counter >= 10.
        EXIT.
      ENDIF.
      APPEND ls_travel_ids-travel_id TO et_travel_ids_task8_3.
      lv_counter = lv_counter + 1.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abap_course_basics~scrabble_score.

    DATA: lv_length TYPE i.
    DATA: lv_offset TYPE i.
    DATA: lv_char TYPE c LENGTH 1.
    DATA: lv_position TYPE i.
    DATA: lv_total_sum TYPE i.

    lv_length = strlen( iv_word ).
    lv_offset = 0.
    lv_total_sum = 0.


    WHILE lv_offset < lv_length.
      lv_char = iv_word+lv_offset(1).

      IF lv_char CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.
        TRANSLATE lv_char TO UPPER CASE.
        CASE lv_char.
          WHEN 'A'.
            lv_position = 1.
          WHEN 'B'.
            lv_position = 2.
          WHEN 'C'.
            lv_position = 3.
          WHEN 'D'.
            lv_position = 4.
          WHEN 'E'.
            lv_position = 5.
          WHEN 'F'.
            lv_position = 6.
          WHEN 'G'.
            lv_position = 7.
          WHEN 'H'.
            lv_position = 8.
          WHEN 'I'.
            lv_position = 9.
          WHEN 'J'.
            lv_position = 10.
          WHEN 'K'.
            lv_position = 11.
          WHEN 'L'.
            lv_position = 12.
          WHEN 'M'.
            lv_position = 13.
          WHEN 'N'.
            lv_position = 14.
          WHEN 'O'.
            lv_position = 15.
          WHEN 'P'.
            lv_position = 16.
          WHEN 'Q'.
            lv_position = 17.
          WHEN 'R'.
            lv_position = 18.
          WHEN 'S'.
            lv_position = 19.
          WHEN 'T'.
            lv_position = 20.
          WHEN 'U'.
            lv_position = 21.
          WHEN 'V'.
            lv_position = 22.
          WHEN 'W'.
            lv_position = 23.
          WHEN 'X'.
            lv_position = 24.
          WHEN 'Y'.
            lv_position = 25.
          WHEN 'Z'.
            lv_position = 26.

        ENDCASE.


        lv_total_sum = lv_total_sum + lv_position.
      ENDIF.

      lv_offset = lv_offset + 1.

    ENDWHILE.

    rv_result = lv_total_sum.
  ENDMETHOD.
ENDCLASS.
