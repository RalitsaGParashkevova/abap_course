CLASS zrp_cl_order_application DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zrp_cl_order_application IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.


DELETE FROM ZRP_ORDERS.
  out->write( 'Delete all items.' ).
DELETE FROM zrp_items.
  out->write( 'Delete all items.' ).

COMMIT WORK AND WAIT.
  ENDMETHOD.

ENDCLASS.
