CLASS zcl_rgp_abap_course_basics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    INTERFACES zif_abap_course_basics .
    DATA: result_message TYPE string. " Standard string attribute to hold messages
    DATA: result_validation TYPE abap_boolean.
   METHODS validate_input
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
       iv_input_valid TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
  CONSTANTS:   c_operator_add TYPE c VALUE '+',
               c_operator_sub TYPE c VALUE '-',
               c_operator_mul TYPE c VALUE '*',
               c_operator_div TYPE c VALUE '/',
               c_invalid_number TYPE i VALUE 0,
               c_year_length TYPE i VALUE 4.
  METHODS validate_input_scrabble_score
    IMPORTING
      iv_word_input  TYPE string
    EXPORTING
      iv_input_valid TYPE abap_bool.



ENDCLASS.



CLASS zcl_rgp_abap_course_basics IMPLEMENTATION.



 METHOD if_oo_adt_classrun~main.
 " Execution of method calculator "
* DATA lv_first_number TYPE i VALUE 12.
*    DATA lv_second_number TYPE i VALUE 3.
*    DATA lv_operator TYPE c VALUE '/'.
*    DATA lv_result TYPE i.
*
*    " Call the validation method first
*    CALL METHOD me->validate_input
*      EXPORTING
*        iv_first_number  = lv_first_number
*        iv_second_number = lv_second_number
*        iv_operator      = lv_operator
*      IMPORTING
*        ev_message       = result_message.  " Get the validation message
*
*    IF result_message IS NOT INITIAL.
*      " Validation failed, output the error message
*      out->write( result_message ).
*    ELSE.
*      " Validation passed, perform the calculation
*      lv_result = me->zif_abap_course_basics~calculator(
*          EXPORTING
*            iv_first_number  = lv_first_number
*            iv_second_number = lv_second_number
*            iv_operator      = lv_operator
*        ).
*      out->write( lv_result ).  " Output the result
*    ENDIF.


  " Execution of method date_parsing
* DATA lv_input_string_date TYPE string VALUE '1 4 2017'. " Corrected input string
*  DATA lv_input_valid       TYPE abap_bool.
*  DATA lv_date_result      TYPE dats.
*
*  " Call the validate_date_parsing method first
*  CALL METHOD me->validate_date_parsing
*    EXPORTING
*      iv_input_string_date = lv_input_string_date
*    IMPORTING
*      iv_input_valid = lv_input_valid.
*
*  IF lv_input_valid = abap_false.
*    out->write( 'The input is not valid!' ).
*  ELSE.
*    lv_date_result = me->zif_abap_course_basics~date_parsing(
*      EXPORTING
*        iv_date = lv_input_string_date
*    ).
*    out->write( lv_date_result ).
*  ENDIF.
 " Execution of method hello_world
*DATA lv_name TYPE string VALUE 'Ralitsa'.
*DATA lv_result_message TYPE string.
*
*lv_result_message =  me->zif_abap_course_basics~hello_world(
*EXPORTING
*iv_name = lv_name
*).
*
*out->write( lv_result_message ).

*" Execution of method scrabble_score.
*DATA lv_word TYPE string VALUE ''.
*DATA lv_input_valid TYPE abap_bool.
*DATA lv_result_message TYPE i.
*
*CALL METHOD me->validate_input_scrabble_score
*EXPORTING
*iv_word_input = lv_word
*IMPORTING
*iv_input_valid = lv_input_valid.
*
*IF lv_input_valid = abap_false.
* out->write( 'The input is not valid!' ).
*ELSE.
*lv_result_message =  me->zif_abap_course_basics~scrabble_score(
*EXPORTING
*iv_word = lv_word
*).
*
* out->write( lv_result_message ).
*ENDIF.

*" Execution of method zif_abap_course_basics~get_current_date_time.
*DATA lv_result_message TYPE timestampl.
**
*lv_result_message =  me->zif_abap_course_basics~get_current_date_time( ).
*out->write( lv_result_message ).

" Execution of method zif_abap_course_basics~fizz_buzz.
DATA: lv_result_message TYPE string.

lv_result_message = me->zif_abap_course_basics~fizz_buzz(  ).

out->write( lv_result_message ).


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

     " Validate day
    IF lv_day CN '0123456789' OR lv_day < '1' OR lv_day > '31'.
     iv_input_valid = abap_false.
     ELSE.
     iv_input_valid = abap_true.
    ENDIF.

  " Validate month (numeric or alphabetic)
    IF lv_month CN '0123456789'. " Check for alphabetic month
      IF lv_month CN 'JanuaryFebruaryMarchAprilMayJuneJulyAugustSeptemberOctoberNovemberDecember'.
        iv_input_valid = abap_false.
        ELSE.
        iv_input_valid = abap_true.
      ENDIF.
    ELSEIF " Numeric month
      lv_month < '1' OR lv_month > '12'.
        iv_input_valid = abap_false.
        ELSE.
        iv_input_valid = abap_true.
      ENDIF.

* Check if the year is leap when date is 29 and month is February
IF lv_month = 'February' or lv_month = 2 or lv_month = 02.
IF lv_day = 29 AND ( lv_year MOD 4 <> 0 OR ( lv_year MOD 100 = 0 AND lv_year MOD 400 <> 0 ) ).
        iv_input_valid = abap_false.
      ENDIF.
iv_input_valid = abap_false.
ELSE.
iv_input_valid = abap_true.
ENDIF.

    " Validate year
    IF lv_year CN '0123456789' OR strlen( lv_year ) <> 4.
      iv_input_valid = abap_false.
      ELSE.
      iv_input_valid = abap_true.
    ENDIF.

ENDMETHOD.


  METHOD validate_input.
    " Initialize the message to empty
    ev_message = ''.

    IF iv_second_number = c_invalid_number AND
       ( iv_operator <> c_operator_add OR
         iv_operator <> c_operator_sub OR
         iv_operator <> c_operator_mul OR
         iv_operator <> c_operator_div ).
      ev_message = |Error: Second number is 0 and the operator "{ iv_operator }" is invalid.|.
      RETURN. " Exit the method early
    ENDIF.

    IF iv_operator <> c_operator_add OR
       iv_operator <> c_operator_sub OR
       iv_operator <> c_operator_mul OR
       iv_operator <> c_operator_div.
      ev_message = |Error: Invalid operator "{ iv_operator }".|.
      RETURN. " Exit the method early
    ENDIF.

    IF iv_second_number = 0 AND iv_operator = c_operator_div.
      ev_message = 'Error: Division by zero is not allowed!'.
      RETURN. " Exit the method early
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
   DATA lv_input_length TYPE i.
   DATA: lv_month_extract TYPE string.
   DATA: lv_day_extract TYPE string.
   DATA: lv_year_extract TYPE string.
   DATA: lv_result TYPE string.
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
  DATA: lv_message TYPE string.

  lv_user_system_id = sy-uname.

CONCATENATE 'Hello , ' iv_name '. Your user ID is: ' lv_user_system_id INTO rv_result.

  ENDMETHOD.


  METHOD zif_abap_course_basics~internal_tables.
  ENDMETHOD.


  METHOD zif_abap_course_basics~open_sql.
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


  while lv_offset < lv_length.
  lv_char = iv_word+lv_offset(1).

  IF lv_char CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.
  TRANSLATE lv_char TO UPPER CASE.
  CASE lv_char.
    WHEN 'A'. lv_position = 1.
    WHEN 'B'. lv_position = 2.
    WHEN 'C'. lv_position = 3.
    WHEN 'D'. lv_position = 4.
    WHEN 'E'. lv_position = 5.
    WHEN 'F'. lv_position = 6.
    WHEN 'G'. lv_position = 7.
    WHEN 'H'. lv_position = 8.
    WHEN 'I'. lv_position = 9.
    WHEN 'J'. lv_position = 10.
    WHEN 'K'. lv_position = 11.
    WHEN 'L'. lv_position = 12.
    WHEN 'M'. lv_position = 13.
    WHEN 'N'. lv_position = 14.
    WHEN 'O'. lv_position = 15.
    WHEN 'P'. lv_position = 16.
    WHEN 'Q'. lv_position = 17.
    WHEN 'R'. lv_position = 18.
    WHEN 'S'. lv_position = 19.
    WHEN 'T'. lv_position = 20.
    WHEN 'U'. lv_position = 21.
    WHEN 'V'. lv_position = 22.
    WHEN 'W'. lv_position = 23.
    WHEN 'X'. lv_position = 24.
    WHEN 'Y'. lv_position = 25.
    WHEN 'Z'. lv_position = 26.

  ENDCASE.


  lv_total_sum = lv_total_sum + lv_position.
  ENDIF.

  lv_offset = lv_offset + 1.

  ENDWHILE.

  rv_result = lv_total_sum.
  ENDMETHOD.


  METHOD validate_input_scrabble_score.

    IF iv_word_input IS INITIAL.
    iv_input_valid = abap_false.
    ELSEIF iv_word_input CA '0123456789'.
    iv_input_valid = abap_false.
    ELSE.
    iv_input_valid = abap_true.
 ENDIF.
  ENDMETHOD.

ENDCLASS.
