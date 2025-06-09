CLASS zrp_cl_complexity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  INTERFACES if_sadl_exit_calc_element_read.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zrp_cl_complexity IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~calculate.

  DATA lv_item_count TYPE int1.
  DATA lv_complexity type zrp_c_order-Complexity.

 CHECK NOT it_original_data is INITIAL.

 DATA lt_order_data TYPE STANDARD TABLE OF zrp_c_order.

 MOVE-CORRESPONDING it_original_data TO lt_order_data.

 LOOP AT lt_order_data ASSIGNING FIELD-SYMBOL(<ls_order>).


READ ENTITIES OF zrp_c_order
ENTITY Orders BY \_Item
FIELDS ( OrderUuid )
WITH VALUE #( ( OrderUuid = <ls_order>-OrderUuid ) )
RESULT DATA(ls_items).

DATA(item_count) = lines( ls_items ).
 CASE item_count.
 WHEN 0 or 1 or 2.
 lv_complexity = 'Easy'.
 WHEN 3 or 4.
 lv_complexity = 'Medium'.
 WHEN OTHERS.
 lv_complexity = 'Complex'.
 ENDCASE.

 <ls_order>-Complexity = lv_complexity.

 ENDLOOP.

 ct_calculated_data = CORRESPONDING #( lt_order_data ).

  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.


  ENDMETHOD.

ENDCLASS.
