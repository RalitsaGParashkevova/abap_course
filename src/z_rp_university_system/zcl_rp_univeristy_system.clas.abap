CLASS zcl_rp_univeristy_system DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .


    DATA lv_university_id TYPE i.
    DATA lv_university_name TYPE string.
    DATA lv_university_location TYPE string.

    DATA lv_student_id TYPE i.
    DATA lv_student_university_id TYPE i.
    DATA lv_student_name TYPE string.
    DATA lv_student_age TYPE i.
    DATA lv_student_major TYPE string.
    DATA lv_student_email TYPE string.

  ENDCLASS.



CLASS zcl_rp_univeristy_system IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DELETE FROM zrp_university.
    DELETE FROM zrp_student.
    DELETE FROM zrp_stud_draft.

*Execution of method create_university.
    DATA lv_university TYPE REF TO lcl_university.
    DATA lt_universities TYPE HASHED TABLE OF REF TO lcl_university WITH UNIQUE KEY table_line.

    TRY.
        lv_university = NEW #(
        iv_location = 'Sofia'
        iv_name = 'UNSS' ).


        INSERT lv_university INTO TABLE lt_universities.

        lv_university = NEW #(
        iv_location = 'Sofia'
        iv_name = 'Sofia University' ).

        INSERT lv_university INTO TABLE lt_universities.

        lv_university = NEW #(
        iv_location = 'Plovdiv'
        iv_name = 'MIT' ).

        INSERT lv_university INTO TABLE lt_universities.

        lv_university = NEW #(
        iv_location = 'Varna'
        iv_name = 'MEI' ).

        INSERT lv_university INTO TABLE lt_universities.


        lv_university = NEW #(
        iv_location = 'Sofia'
        iv_name = 'University of Medicine' ).

        INSERT lv_university INTO TABLE lt_universities.


      CATCH cx_abap_invalid_value.
    ENDTRY.
out->write( 'Print result of created universities of DB' ).
    LOOP AT lt_universities INTO DATA(lt_result_university).


      lv_university_location = lt_result_university->lif_university~get_location(  ).
      lv_university_name = lt_result_university->lif_common_utils_methods~get_name( ).

      out->write( lt_result_university->lif_university~create_university(
      IMPORTING

      iv_university_location = lv_university_location
      iv_university_name = lv_university_name ) ).
    ENDLOOP.
out->write( '--------------------------------------------------------------' ).
*Invocation of method create student

    DATA ls_student TYPE REF TO lcl_student. "Declare variable ls_student from reference type lcl_student
    DATA lt_students TYPE HASHED TABLE OF REF TO lcl_student WITH UNIQUE KEY table_line.
    DATA lv_student_university_id TYPE i.
    TRY.


        ls_student = NEW #(
        iv_student_name = 'Petar Hristov'
         iv_age = 19
         iv_major = 'DB' ).


        ls_student->lif_student~set_email(
        EXPORTING
        iv_email = 'p.hristov@gmail.com' ).

        ls_student->lif_student~set_university_id(
        EXPORTING
        iv_university_current_name = 'UNSS' ).

        INSERT ls_student INTO TABLE lt_students.


        ls_student = NEW #(
        iv_student_name = 'Lara Hristova'
        iv_age = 23
         iv_major = 'Sociology' ).


        ls_student->lif_student~set_email(
        EXPORTING
        iv_email = 'l.hristova@gmail.com' ).

        ls_student->lif_student~set_university_id(
        EXPORTING
        iv_university_current_name = 'Sofia University' ).

        INSERT ls_student INTO TABLE lt_students.

        ls_student = NEW #(
        iv_student_name = 'Nikola Nikolava'
        iv_age = 27
         iv_major = 'Accounting' ).

        ls_student->lif_student~set_email(
             EXPORTING
             iv_email = 'n.nikolova@gmail.com' ).

        ls_student->lif_student~set_university_id(
        EXPORTING
        iv_university_current_name = 'Sofia University' ).

        INSERT ls_student INTO TABLE lt_students.


        ls_student = NEW #(
        iv_student_name = 'Boris Hristov'
        iv_age = 23
         iv_major = 'AI' ).

        ls_student->lif_student~set_email(
               EXPORTING
               iv_email = 'b.hristov@gmail.com' ).

        ls_student->lif_student~set_university_id(
        EXPORTING
        iv_university_current_name = 'MEI' ).

        INSERT ls_student INTO TABLE lt_students.


      CATCH cx_abap_invalid_value.
    ENDTRY.

out->write( 'Print result of created students of DB' ).

    LOOP AT lt_students INTO DATA(lt_student).

      lv_student_name = lt_student->lif_common_utils_methods~get_name(  ).
      lv_student_age = lt_student->lif_student~get_age(  ).
      lv_student_major = lt_student->lif_student~get_student_major(  ).
      lv_student_email = lt_student->lif_student~get_student_email(  ).


      out->write( lt_student->lif_student~create_student(
       IMPORTING
       iv_student_name = lv_student_name
       iv_student_age = lv_student_age
       iv_email = lv_student_email
       iv_major = lv_student_major ) ).


    ENDLOOP.


* Add student to university

try.
    lv_university->lif_university~add_student(
    EXPORTING
    iv_student_id = 1
    iv_university_id = 1 ).
CATCH cx_abap_sql_error.
ENDTRY.

*Student: Retrieves a student by their student_id.
out->write( '--------------------------------------------------------------' ).
out->write( 'Print the student by ID' ).
    TRY.
        out->write( ls_student->lif_student~get_student(
        EXPORTING
        iv_student_id = 3 ) ).
      CATCH cx_abap_invalid_value.

    ENDTRY.

* List all students in the university
    DATA: lt_export_universities TYPE HASHED TABLE OF zrp_student WITH UNIQUE KEY student_id.

    lv_university->lif_university~list_students(
    IMPORTING
    et_all_students = lt_export_universities ).

out->write( '--------------------------------------------------------------' ).
out->write( 'Print all students in an university' ).

LOOP AT lt_export_universities INTO DATA(lv_result).

   out->write( |Student ID: { lv_result-student_id } University ID: { lv_result-university_id } Age: { lv_result-student_age } Major: {  lv_result-student_major } Email: {  lv_result-student_email }| ).
ENDLOOP.
*Updates the details of an existing student.
    DATA: lv_age   TYPE i VALUE 27,
          lv_name  TYPE string VALUE 'Maria Papazova',
          lv_major TYPE string VALUE 'ABAP',
          lv_email TYPE string VALUE 'r.parashkevova@gmail.com'.
    TRY.

        ls_student->lif_student~update_student(
        EXPORTING
        iv_student_id = 3
        CHANGING cv_age = lv_age
        cv_name = lv_name
        cv_major = lv_major
        cv_email = lv_email ).
      CATCH cx_abap_sql_error.
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
