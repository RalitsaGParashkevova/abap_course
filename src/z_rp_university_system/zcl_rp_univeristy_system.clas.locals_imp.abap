*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_student DEFINITION.

  PUBLIC SECTION.

    METHODS constructor IMPORTING
                                  iv_student_name TYPE zrp_student_name
                                  iv_age          TYPE zrp_student_age
                                  iv_major        TYPE zrp_student_major

                        RAISING   cx_abap_invalid_value.
    INTERFACES lif_student.
    INTERFACES lif_common_utils_methods.

PRIVATE SECTION.
  DATA student_id TYPE i.
  DATA university_id TYPE i.
  DATA name TYPE string.
  DATA age TYPE i.
  DATA major TYPE string.
  DATA email TYPE string.
  DATA lt_students TYPE HASHED TABLE OF zrp_student WITH UNIQUE KEY student_id.
  CLASS-DATA gv_id_counter TYPE i VALUE 0.
  DATA lt_university_students TYPE HASHED TABLE OF zrp_student WITH UNIQUE KEY student_id.

ENDCLASS.

CLASS lcl_student IMPLEMENTATION.

  METHOD constructor.

    me->student_id = me->lif_common_utils_methods~generate_id(  ).

    IF iv_student_name IS INITIAL OR name CA '012346789'.
      RAISE EXCEPTION TYPE cx_abap_invalid_value.
    ELSE.
      me->name = iv_student_name.
    ENDIF.

    IF iv_age IS INITIAL AND iv_age < 0  OR iv_age < 18.
      RAISE EXCEPTION TYPE cx_abap_invalid_value.
    ELSE.

      me->age = iv_age.

    ENDIF.
    IF  iv_major CA '012346789' OR iv_major IS INITIAL.
      RAISE EXCEPTION TYPE cx_abap_invalid_value.
    ELSE.

      me->major = iv_major.
    ENDIF.

  ENDMETHOD.

  METHOD lif_student~create_student.


    DATA ls_student TYPE zrp_student.

    ls_student-student_id = me->lif_common_utils_methods~get_id(  ).
    ls_student-university_id = me->lif_student~get_current_university_id(  ).
    ls_student-student_name =  iv_student_name.
    ls_student-student_age = iv_student_age.
    ls_student-student_email = iv_email.
    ls_student-student_major = iv_major.


    INSERT INTO zrp_student VALUES @ls_student.
    COMMIT WORK AND WAIT.
    IF sy-subrc = 0.
*      SELECT SINGLE
*      FROM zrp_student
*      FIELDS student_id
*      WHERE student_id = @ls_student-student_id
*      INTO @rv_student_id.

      SELECT SINGLE
         FROM zrp_r_student( p_student_id = @ls_student-student_id )
         FIELDS StudentId
         INTO @rv_student_id.
    ENDIF.
  ENDMETHOD.

  METHOD lif_student~get_student.

    SELECT SINGLE
    FROM zrp_student
    FIELDS *
    WHERE student_id = @iv_student_id
    INTO @rv_student.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_abap_sql_error
        EXPORTING
          explanation = 'Database operation failed: No student record found'.

    ENDIF.

  ENDMETHOD.

  METHOD lif_student~update_student.

*    SELECT SINGLE
*    FROM zrp_student
*    FIELDS *
*    WHERE student_id = @iv_student_id
*    INTO @DATA(lv_student).

 SELECT SINGLE
    FROM zrp_r_student( p_student_id = @iv_student_id )
    FIELDS StudentId
    INTO @DATA(lv_student_id).

    IF sy-subrc = 0.
      UPDATE zrp_student
      SET
      student_name = @cv_name,
      student_age = @cv_age,
      student_email = @cv_email,
      student_major = @cv_major
      WHERE student_id = @iv_student_id.
    ELSE.
      RAISE EXCEPTION TYPE cx_abap_sql_error
        EXPORTING
          explanation = 'Database operation failed: No student record found'.
    ENDIF.

  ENDMETHOD.

  METHOD lif_common_utils_methods~generate_id.
 gv_id_counter = gv_id_counter + 1.

    rv_id = gv_id_counter.
  ENDMETHOD.

  METHOD lif_student~get_age.
 rv_student_age = age.
  ENDMETHOD.

  METHOD lif_common_utils_methods~get_id.

    rv_id = student_id.
  ENDMETHOD.

  METHOD lif_common_utils_methods~get_name.
    rv_name = name.
  ENDMETHOD.

  METHOD lif_student~get_student_email.
    rv_student_email = email.
  ENDMETHOD.

  METHOD lif_student~get_student_major.
    rv_student_major = major.
  ENDMETHOD.

  METHOD lif_student~get_university_id_by_name.

    DATA lv_result_university_id TYPE i.
    SELECT SINGLE
    FROM zrp_university
    FIELDS university_id
    WHERE univeristy_name = @iv_university_name "To raise exception!!!!!!!!!!!!!!!!!
    INTO @rv_university_id.

if sy-subrc <> 0.
RAISE EXCEPTION TYPE cx_abap_sql_error
EXPORTING
          explanation = 'Database operation failed: This university does not exists!'.
ENDIF.

  ENDMETHOD.

  METHOD lif_student~get_current_university_id.

    rv_current_id = university_id.
  ENDMETHOD.

  METHOD lif_student~set_university_id.

TRY.
me->university_id = me->lif_student~get_university_id_by_name(
EXPORTING
iv_university_name = iv_university_current_name ).
CATCH CX_ABAP_INVALID_VALUE.
ENDTRY.
  ENDMETHOD.

  METHOD lif_student~set_email.
*The pattern ensures the email format is correct (username, @, domain, .com or similar suffix).
DATA: lv_pattern TYPE string,
      lv_match TYPE abap_bool.
lv_pattern = '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$'.  " Assign regex pattern

lv_match =  cl_abap_matcher=>matches( text = iv_email pattern = lv_pattern ).

if lv_match = abap_true.
email =  iv_email.
ELSE.
RAISE EXCEPTION TYPE cx_parameter_invalid.
ENDIF.

  ENDMETHOD.

  METHOD lif_common_utils_methods~set_id.
me->student_id = iv_id.
  ENDMETHOD.

ENDCLASS.
CLASS lcl_university DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_university.
    INTERFACES lif_common_utils_methods.
    METHODS constructor IMPORTING
                                  iv_name     TYPE string
                                  iv_location TYPE string
                        RAISING   cx_abap_invalid_value.


  PRIVATE SECTION.
    DATA id TYPE i.
    DATA name TYPE string.
    DATA location TYPE string.
    CLASS-DATA gv_id_counter TYPE i VALUE 0.
    DATA lt_university_students TYPE HASHED TABLE OF zrp_student WITH UNIQUE KEY student_id.
  ENDCLASS.
CLASS lcl_university IMPLEMENTATION.

  METHOD constructor.

    me->id = me->lif_common_utils_methods~generate_id(  ).

    IF iv_name IS INITIAL OR iv_name CA '0123456789'.
      RAISE EXCEPTION TYPE cx_abap_invalid_value.
    ENDIF.

    me->name = iv_name.

    me->location = iv_location.

  ENDMETHOD.

  METHOD lif_university~add_student.

    " Step 1: Check if the student exists
    SELECT SINGLE
      FROM zrp_student
      FIELDS *
      WHERE student_id = @iv_student_id
        AND university_id = @iv_university_id
        INTO @DATA(lv_student).


    IF sy-subrc = 0.

      INSERT lv_student INTO TABLE lt_university_students.
    ELSE.
      RAISE EXCEPTION TYPE cx_abap_sql_error
        EXPORTING
          explanation = 'Database operation failed: No student record found'.
    ENDIF.


  ENDMETHOD.

  METHOD lif_university~create_university.

    DATA ls_university TYPE zrp_university.


    ls_university-university_id = me->lif_common_utils_methods~get_id(  ).
    ls_university-univeristy_location =  iv_university_location.
    ls_university-univeristy_name = iv_university_name.
    INSERT INTO zrp_university VALUES @ls_university.

    COMMIT WORK AND WAIT.
    IF sy-subrc = 0.

*      SELECT SINGLE
*      FROM zrp_university
*      FIELDS university_id
*      WHERE univeristy_name = @ls_university-univeristy_name
*      INTO @rv_university_id.

    SELECT SINGLE
      FROM zrp_r_university( p_university_name = @ls_university-univeristy_name )
      FIELDS UniversityId
      INTO @rv_university_id.

    ENDIF.
  ENDMETHOD.

  METHOD lif_university~delete_student.

*    SELECT SINGLE
*    FROM zrp_student
*    FIELDS *
*    WHERE student_id = @iv_student_id
*    INTO @data(lv_student).

    SELECT SINGLE
       FROM zrp_r_student( p_student_id = @iv_student_id )
       FIELDS StudentId
       INTO @DATA(lv_student_id).
    IF sy-subrc = 0.

      DELETE FROM zrp_student WHERE student_id = @iv_student_id.

    ELSE.
      RAISE EXCEPTION TYPE cx_abap_sql_error
        EXPORTING
          explanation = 'Database operation failed: No student record found'.
    ENDIF.
  ENDMETHOD.

  METHOD lif_university~list_students.

    et_all_students = CORRESPONDING #( lt_university_students ).

  ENDMETHOD.

  METHOD lif_common_utils_methods~generate_id.

    gv_id_counter = gv_id_counter + 1.

    rv_id = gv_id_counter.
  ENDMETHOD.

  METHOD lif_common_utils_methods~get_name.

    rv_name = name.

  ENDMETHOD.

  METHOD lif_common_utils_methods~get_id.

    rv_id = id.
  ENDMETHOD.

  METHOD lif_university~get_location.
    e_location = location.
  ENDMETHOD.

  METHOD lif_common_utils_methods~set_id.
    me->id = iv_id.
  ENDMETHOD.

ENDCLASS.
