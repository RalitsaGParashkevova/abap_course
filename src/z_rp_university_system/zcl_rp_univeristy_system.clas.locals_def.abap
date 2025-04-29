*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

*"* Interface for defining university-related operations
INTERFACE lif_university.


*"* Define Hash table type to store student records uniquely
TYPES: tt_students TYPE HASHED TABLE OF zrp_student WITH UNIQUE KEY student_id.

*"* Method to create a university record in DB
METHODS create_university
EXPORTING
iv_university_name TYPE string
              iv_university_location TYPE string

              RETURNING VALUE(rv_university_id) TYPE i.

*" Method to add a student to a university hash table
METHODS add_student "Adds a student to the university.
IMPORTING iv_student_id TYPE i
iv_university_id type i
RAISING cx_abap_sql_error.

*" Method to delete a student from a university by their ID
METHODS delete_student "Deletes a student from the university by their student_id.

IMPORTING iv_student_id TYPE i
RAISING cx_abap_sql_error.

"*" Method to list all students in a university
METHODS list_students
EXPORTING et_all_students TYPE tt_students.


*" Method to retrieve university location
METHODS get_location
RETURNING VALUE(e_location) type string.


ENDINTERFACE.

*"* Interface for defining student-related operations
INTERFACE lif_student.

*" Type representing a student entity
types ls_student type zrp_student.

*" Method to create a student record in DB
METHODS create_student
EXPORTING iv_student_name TYPE string
iv_student_age type i
iv_major TYPE string
iv_email type string
RETURNING VALUE(rv_student_id) TYPE i.

*" Method to set student email with validation
METHODS set_email
IMPORTING iv_email type string
RAISING cx_parameter_invalid.


"! Method to retrieve student details by ID
"! @parameter iv_student_id |
"! @parameter rv_student |
"! @raising cx_abap_invalid_value |
METHODS get_student
IMPORTING iv_student_id TYPE i
RETURNING VALUE(rv_student) TYPE ls_student
RAISING cx_abap_invalid_value.


"! Method to update student information
"! @parameter iv_student_id |
"! @parameter cv_name |
"! @parameter cv_age |
"! @parameter cv_major |
"! @parameter cv_email |
"! @raising cx_abap_sql_error |
METHODS update_student
IMPORTING
iv_student_id type zrp_student_id
CHANGING
cv_name type string
cv_age TYPE i
cv_major TYPE string
cv_email type string
RAISING cx_abap_sql_error.

"! Method to retrieve student's age
"! @parameter rv_student_age |
METHODS get_age

RETURNING VALUE(rv_student_age) TYPE i.

"! Method to retrieve student's major
"! @parameter rv_student_major |
METHODS get_student_major

RETURNING VALUE(rv_student_major) TYPE string.

"! Method to retrieve student's email
"! @parameter rv_student_email |
METHODS get_student_email

RETURNING VALUE(rv_student_email) TYPE string.

"! Method to get university ID by university name
"! @parameter iv_university_NAME |
"! @parameter rv_university_id |
"! @raising cx_abap_invalid_value |
METHODS get_university_id_by_name
  IMPORTING iv_university_NAME TYPE STRING
  RETURNING VALUE(rv_university_id) TYPE I
  RAISING cx_abap_invalid_value.

"! Method to retrieve student's current university ID
"! @parameter rv_current_id |
METHODS get_current_university_id
RETURNING VALUE(rv_current_id) type i.

"! Method to assign a university_id by university name
"! @parameter iv_university_current_name |
METHODS set_university_id
IMPORTING
iv_university_current_name TYPE string.

ENDINTERFACE.


"! Interface for common utility methods
INTERFACE lif_common_utils_methods.


"! Class method to generate unique IDs
"! @parameter rv_id |
class-METHODS generate_id
RETURNING VALUE(rv_id) TYPE i.

"! Method to retrieve entity name
"! @parameter rv_name |
METHODS get_name
RETURNING VALUE(rv_name) type string.

"! Method to retrieve entity ID
"! @parameter rv_id |
METHODS get_id
RETURNING VALUE(rv_id) type i.


"! Method to assign an ID to an entity
"! @parameter iv_id |
METHODS set_id
IMPORTING iv_id TYPE zrp_university_id.
ENDINTERFACE.
