class zrp_cl_currency_amdp definition final.

  public section.
    CLASS-METHODS convert_currency
      IMPORTING
        value(iv_amount)             TYPE zrp_price
        value(iv_currency_source)    TYPE /dmo/currency_code
        value(iv_currency_target)    TYPE /dmo/currency_code
        value(iv_exchange_rate_date) TYPE dats
      EXPORTING
        value(ev_amount)             TYPE i
        RAISING
          cx_sy_arithmetic_overflow   " For potential calculation overflows
          cx_sy_conversion_error.      " For general conversion errors, e.g., missing rate



  protected section.

  private section.

endclass.

class zrp_cl_currency_amdp implementation.


  METHOD convert_currency.
  DATA: lv_exchange_rate TYPE f,
          lv_factor_source TYPE f,
          lv_factor_target TYPE f,
          lv_converted_amount TYPE zrp_price.

    " Retrieve exchange rate from I_ExchangeRateRawData
    SELECT SINGLE ExchangeRate, NumberOfSourceCurrencyUnits, NumberOfTargetCurrencyUnits

      FROM I_ExchangeRateRawData
      WHERE ExchangeRateType = 'M'  " Standard exchange rate type
        AND SourceCurrency = @iv_currency_source
        AND TargetCurrency = @iv_currency_target
        AND ValidityStartDate = @iv_exchange_rate_date
         INTO (@lv_exchange_rate, @lv_factor_source, @lv_factor_target).

    " If no exchange rate is found, default value to zero
    IF sy-subrc <> 0.
      ev_amount = 0.
      RETURN.
    ENDIF.

    " Normalize the currency units
    lv_converted_amount = iv_amount * ( lv_factor_source / lv_factor_target ) * lv_exchange_rate.

    " Assign final converted amount
    ev_amount = lv_converted_amount.
  ENDMETHOD.

endclass.

CLASS lhc_Orders DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
   CONSTANTS:
      BEGIN OF order_status,
        inProcess     TYPE c LENGTH 2  VALUE 'IP', " In Process TO BE CHANGED WITH PREDEFINIED
        completed TYPE c LENGTH 1  VALUE 'C', " Completed
        canceled TYPE c LENGTH 2  VALUE 'CD', " Cancelled
        new TYPE c LENGTH 1 VALUE 'N',
      END OF order_status.



    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Orders RESULT result.

   METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Orders RESULT result.


    METHODS reCalculateTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Orders~reCalculateTotalPrice.

    METHODS completeOrder FOR MODIFY
      IMPORTING keys FOR ACTION Orders~completeOrder RESULT result.

    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Orders~setInitialStatus.

    METHODS incrementOrderId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Orders~incrementOrderId.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~validateCustomer.

    METHODS validateOrderItems FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~validateOrderItems.


    METHODS cancelOrder FOR MODIFY
      IMPORTING keys FOR ACTION Orders~cancelOrder RESULT result.
    METHODS inProcessOrder FOR MODIFY
      IMPORTING keys FOR ACTION Orders~inProcessOrder RESULT result.

    METHODS validateOrderName FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~validateOrderName.

    METHODS validateOrderPrice FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~validateOrderPrice.

    METHODS validateOrderQuantity FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~validateOrderQuantity.


    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Orders~calculateTotalPrice.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Orders RESULT result.

ENDCLASS.

CLASS lhc_Orders IMPLEMENTATION.

  METHOD get_instance_features.

   " Read the travel status of the existing travels
    READ ENTITIES OF zrp_i_order IN LOCAL MODE
      ENTITY Orders
        FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders)
      FAILED failed.

    result =
      VALUE #(
        FOR result_order IN orders
          LET in_process =   COND #( WHEN result_order-Status = order_status-inprocess
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled  )
              is_cancelled =   COND #( WHEN result_order-Status = order_status-canceled
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled )
              is_completion   =   COND #( WHEN result_order-Status = order_status-completed
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled )
          IN
            ( %tky                 = result_order-%tky
              %action-inProcessOrder = in_process
              %action-cancelOrder = is_cancelled
              %action-completeOrder = is_completion

             ) ).


  ENDMETHOD.


  METHOD reCalculateTotalPrice.
   TYPES: BEGIN OF ty_amount_per_currencycode,
           amount        TYPE zrp_price,
           currency_code TYPE /dmo/currency_code,
         END OF ty_amount_per_currencycode.

  DATA: amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

  " Read the current order data
  READ ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
        ENTITY Orders
        FIELDS ( Currency )
        WITH CORRESPONDING #( keys )
        RESULT DATA(order_data).

  DELETE order_data WHERE Currency IS INITIAL.

  LOOP AT order_data ASSIGNING FIELD-SYMBOL(<order>).
    " Initialize total price based on currency
    amount_per_currencycode = VALUE #( ( amount = 0 currency_code = <order>-Currency ) ).

    " Read all associated Items
    READ ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
       ENTITY Orders BY \_Item
       FIELDS ( Price Quantity Currency )
       WITH VALUE #( ( %tky = <order>-%tky ) )
       RESULT DATA(items).

    " Sum item values per currency
    LOOP AT items INTO DATA(item) WHERE ItemUuid IS NOT INITIAL.
      COLLECT VALUE ty_amount_per_currencycode( amount = item-Price * item-Quantity
                                                currency_code = item-Currency ) INTO amount_per_currencycode.
    ENDLOOP.

    CLEAR <order>-TotalPrice.
    LOOP AT amount_per_currencycode INTO DATA(single_amount_per_currencycode).
      " Perform currency conversion if necessary
      IF single_amount_per_currencycode-currency_code = <order>-Currency.
        <order>-TotalPrice += single_amount_per_currencycode-amount.
      ELSE.

        zrp_cl_currency_amdp=>convert_currency(
          EXPORTING
            iv_amount               = single_amount_per_currencycode-amount
            iv_currency_source      = single_amount_per_currencycode-currency_code
            iv_currency_target      = <order>-Currency
            iv_exchange_rate_date   = cl_abap_context_info=>get_system_date( )
          IMPORTING
            ev_amount               = DATA(converted_amount)
        ).
        <order>-TotalPrice += converted_amount.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  " Write back the modified total price
  MODIFY ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
    ENTITY Orders
      UPDATE FIELDS ( TotalPrice )
      WITH CORRESPONDING #( order_data ).

  ENDMETHOD.

  " Set completed status "
  METHOD completeOrder.
  MODIFY ENTITIES OF zrp_i_order IN LOCAL MODE
  ENTITY Orders
  UPDATE
  FIELDS ( Status )
  WITH VALUE #( FOR key IN keys
   ( %tky = key-%tky
   Status = order_status-completed
   CompletionDate = cl_abap_context_info=>get_system_date( ) ) )
   FAILED failed
   REPORTED reported.

   " Fill the response table "

   READ ENTITIES OF zrp_i_order IN LOCAL MODE
   ENTITY Orders
   ALL FIELDS WITH CORRESPONDING #( keys )
   RESULT DATA(orders).

   result = VALUE #( FOR order in orders
   ( %tky = order-%tky
   %param = order ) ).
  ENDMETHOD.


  METHOD setInitialStatus.

  READ ENTITIES OF zrp_i_order IN LOCAL MODE
  ENTITY Orders
  FIELDS ( Status ) WITH CORRESPONDING #( keys )
  RESULT DATA(orders).

  DELETE orders WHERE Status is not INITIAL.
  CHECK orders is not INITIAL.

  MODIFY ENTITIES OF zrp_i_order IN LOCAL MODE
  ENTITY Orders
  UPDATE FIELDS ( Status )
  WITH VALUE #( FOR order in orders
  ( %tky = order-%tky
  Status = order_status-new
  CreationDate = cl_abap_context_info=>get_system_date(  ) ) )
  REPORTED DATA(update_reported).

  reported = CORRESPONDING #( DEEP update_reported ).

  ENDMETHOD.

  METHOD incrementOrderId.

  READ ENTITIES OF zrp_i_order IN LOCAL MODE
  ENTITY Orders
  FIELDS ( OrderId ) WITH CORRESPONDING #( keys )
  RESULT DATA(orders).

   DELETE orders WHERE OrderId IS NOT INITIAL.

   CHECK orders IS NOT INITIAL.

    " Select max travel ID
    SELECT SINGLE
        FROM  zrp_orders
        FIELDS MAX( order_id ) AS orderID
        INTO @DATA(max_orderid).

    " Set the travel ID TO DO ANOTHER GENERATION WAY
    MODIFY ENTITIES OF zrp_i_order IN LOCAL MODE
    ENTITY Orders
      UPDATE
        FROM VALUE #( FOR order IN orders INDEX INTO i (
          %tky              = order-%tky
          OrderId          = max_orderid + i
          %control-OrderID = if_abap_behv=>mk-on ) )
    REPORTED DATA(update_reported).

    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.


  METHOD validateCustomer.
  " Read relevant travel instance data
    READ ENTITIES OF zrp_i_order IN LOCAL MODE
      ENTITY Orders
        FIELDS ( Customer ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( orders DISCARDING DUPLICATES MAPPING customer_id = Customer EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.
    IF customers IS NOT INITIAL.
      " Check if customer ID exist
      SELECT FROM /dmo/customer FIELDS customer_id
        FOR ALL ENTRIES IN @customers
        WHERE customer_id = @customers-customer_id
        INTO TABLE @DATA(customers_db).
    ENDIF.

    " Raise msg for non existing and initial customerID
    LOOP AT orders INTO DATA(order).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky        = order-%tky
                       %state_area = 'VALIDATE_CUSTOMER' )
        TO reported-orders.

      IF order-Customer IS INITIAL OR NOT line_exists( customers_db[ customer_id = order-Customer ] ).
        APPEND VALUE #(  %tky = order-%tky ) TO failed-orders.

        APPEND VALUE #(  %tky        = order-%tky
                         %state_area = 'VALIDATE_CUSTOMER'
                         %msg        = NEW zrp_cm(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zrp_cm=>customer_unknown
                                           customerid =  order-Customer )
                         %element-Customer = if_abap_behv=>mk-on )
          TO reported-orders.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateOrderItems.

   " Validate: An order must have at least one item before it can be saved.
    " This validation is triggered on 'save' for new and updated orders.

    " Read the orders that are currently being validated.
    READ ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
      ENTITY Orders
        FIELDS ( OrderUuid ) " We only need the key to read associated items
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_orders_to_validate).

    " Loop through each order that needs validation.
    LOOP AT lt_orders_to_validate ASSIGNING FIELD-SYMBOL(<fs_order>).

      " Read all items associated with the current order.
      " Using the composition association _Item.
      READ ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
        ENTITY Orders BY  \_Item
          FROM CORRESPONDING #( keys ) " Pass the key of the current order
        RESULT DATA(lt_items_for_order).

      " Check if the order has any items.
      IF lines( lt_items_for_order ) = 0.
        " If no items are found, report an error.
       APPEND VALUE #( %tky = <fs_order>-%tky ) TO failed-orders.

       APPEND VALUE #( %tky = <fs_order>-%tky
       %state_area = 'VALIDATE_ORDER'
       %msg = NEW zrp_cm(
       severity = if_abap_behv_message=>severity-error
       textid = zrp_cm=>item_unknown
       itemid = <fs_order>-OrderId )
       %element-_Item = if_abap_behv=>mk-on ) TO reported-orders.

       ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD cancelOrder.
   MODIFY ENTITIES OF zrp_i_order IN LOCAL MODE
  ENTITY Orders
  UPDATE
  FIELDS ( Status )
  WITH VALUE #( FOR key IN keys
   ( %tky = key-%tky
   Status = order_status-canceled
   CancellationDate = cl_abap_context_info=>get_system_date( ) ) )
   FAILED failed
   REPORTED reported.

   " Fill the response table "

   READ ENTITIES OF zrp_i_order IN LOCAL MODE
   ENTITY Orders
   ALL FIELDS WITH CORRESPONDING #( keys )
   RESULT DATA(orders).

   result = VALUE #( FOR order in orders
   ( %tky = order-%tky
   %param = order ) ).
  ENDMETHOD.

  METHOD inProcessOrder.
    MODIFY ENTITIES OF zrp_i_order IN LOCAL MODE
  ENTITY Orders
  UPDATE
  FIELDS ( Status )
  WITH VALUE #( FOR key IN keys
   ( %tky = key-%tky
   Status = order_status-inprocess ) )
   FAILED failed
   REPORTED reported.

   " Fill the response table "

   READ ENTITIES OF zrp_i_order IN LOCAL MODE
   ENTITY Orders
   ALL FIELDS WITH CORRESPONDING #( keys )
   RESULT DATA(orders).

   result = VALUE #( FOR order in orders
   ( %tky = order-%tky
   %param = order ) ).
  ENDMETHOD.

  METHOD validateOrderName.

  READ ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
  ENTITY Orders BY \_Item
  FROM CORRESPONDING #( keys )
  RESULT DATA(lt_items).


  LOOP AT lt_items INTO DATA(item).

APPEND VALUE #( %tky = item-%tky
%state_area = 'VALIDATE_ITEMNAME' ) TO reported-item.

IF item-Name is NOT INITIAL.
APPEND VALUE #( %tky = item-%tky ) TO failed-item.

APPEND VALUE #(  %tky        = item-%tky
                         %state_area = 'VALIDATE_ITEMNAME'
                         %msg        = NEW zrp_cm(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zrp_cm=>itname_unknown
                                           item_name = item-Name )
                         %element-Name = if_abap_behv=>mk-on )
          TO reported-item.
      ENDIF.
ENDLOOP.
ENDMETHOD.


  METHOD validateOrderPrice.

  READ ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
  ENTITY Orders BY \_Item
  FROM CORRESPONDING #( keys )
  RESULT DATA(lt_items).


  LOOP AT lt_items INTO DATA(item).

APPEND VALUE #( %tky = item-%tky
%state_area = 'VALIDATE_ITEMNAME' ) TO reported-item.

IF item-Price is NOT INITIAL.
APPEND VALUE #( %tky = item-%tky ) TO failed-item.

APPEND VALUE #(  %tky        = item-%tky
                         %state_area = 'VALIDATE_ITEMPRICE'
                         %msg        = NEW zrp_cm(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zrp_cm=>itprice_unknown
                                           item_price = item-Price )
                         %element-Name = if_abap_behv=>mk-on )
          TO reported-item.
   ELSEIF   item-Price < 0.
   APPEND VALUE #( %tky = item-%tky ) TO failed-item.

APPEND VALUE #(  %tky        = item-%tky
                         %state_area = 'VALIDATE_ITEMPRICE'
                         %msg        = NEW zrp_cm(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zrp_cm=>itprice_negative
                                           item_price = item-Price )
                         %element-Price = if_abap_behv=>mk-on )
          TO reported-item.


      ENDIF.
ENDLOOP.
  ENDMETHOD.

  METHOD validateOrderQuantity.

  READ ENTITIES OF ZRP_I_ORDER IN LOCAL MODE
  ENTITY Orders BY \_Item
  FROM CORRESPONDING #( keys )
  RESULT DATA(lt_items).


  LOOP AT lt_items INTO DATA(item).

APPEND VALUE #( %tky = item-%tky
%state_area = 'VALIDATE_ITEMNAME' ) TO reported-item.

IF item-Quantity is NOT INITIAL.
APPEND VALUE #( %tky = item-%tky ) TO failed-item.

APPEND VALUE #(  %tky        = item-%tky
                         %state_area = 'VALIDATE_ITEMQUANTITYUN'
                         %msg        = NEW zrp_cm(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zrp_cm=>itquantity_unknown
                                           item_quantity = item-Quantity )
                         %element-Quantity = if_abap_behv=>mk-on )
          TO reported-item.
   ELSEIF   item-Quantity < 0.
   APPEND VALUE #( %tky = item-%tky ) TO failed-item.

APPEND VALUE #(  %tky        = item-%tky
                         %state_area = 'VALIDATE_ITEMQUANTITYNEG'
                         %msg        = NEW zrp_cm(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zrp_cm=>itquantity_negative
                                           item_quantity = item-Quantity )
                         %element-Quantity = if_abap_behv=>mk-on )
          TO reported-item.


      ENDIF.
ENDLOOP.
  ENDMETHOD.

  METHOD calculateTotalPrice.

  MODIFY ENTITIES OF zrp_i_order IN LOCAL MODE
  ENTITY Orders
  EXECUTE reCalculateTotalPrice
  FROM CORRESPONDING #( keys )
  REPORTED DATA(executed_reported).

  reported = CORRESPONDING #( DEEP executed_reported ).
  ENDMETHOD.


  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

ENDCLASS.
