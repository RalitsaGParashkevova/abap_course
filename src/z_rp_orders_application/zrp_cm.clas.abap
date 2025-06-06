CLASS zrp_cm DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
 INTERFACES if_abap_behv_message .
    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

CONSTANTS:
      BEGIN OF customer_unknown,
        msgid TYPE symsgid VALUE 'ZRP_MSG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'CUSTOMERID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF customer_unknown .

       CONSTANTS:
      BEGIN OF item_unknown,
        msgid TYPE symsgid VALUE 'ZRP_MSG',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'ITEMID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF item_unknown .

 CONSTANTS:
      BEGIN OF itname_unknown,
        msgid TYPE symsgid VALUE 'ZRP_MSG',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'ITEMNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF itname_unknown.

      CONSTANTS:
      BEGIN OF itprice_unknown,
        msgid TYPE symsgid VALUE 'ZRP_MSG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'ITEMPRICEUN',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF itprice_unknown.

          CONSTANTS:
      BEGIN OF itprice_negative,
        msgid TYPE symsgid VALUE 'ZRP_MSG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'ITEMPRICENEG',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF itprice_negative.

           CONSTANTS:
      BEGIN OF itquantity_negative,
        msgid TYPE symsgid VALUE 'ZRP_MSG',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'ITEMQUANTITYNEG',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF itquantity_negative.

     CONSTANTS:
      BEGIN OF itquantity_unknown,
        msgid TYPE symsgid VALUE 'ZRP_MSG',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'ITEMQUANTITYUN',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF itquantity_unknown.

    METHODS constructor
      IMPORTING
       severity TYPE if_abap_behv_message=>t_severity DEFAULT if_abap_behv_message=>severity-error
       textid LIKE if_t100_message=>t100key OPTIONAL
        previous   TYPE REF TO cx_root OPTIONAL
        creation_date  TYPE timestampl OPTIONAL
        cancellation_date    TYPE timestampl OPTIONAL
         completion_date       TYPE  timestampl OPTIONAL
        itemid   TYPE zrp_id OPTIONAL
        customerid TYPE /dmo/customer_id OPTIONAL
        currency_code type /dmo/currency_code OPTIONAL
        item_name type zrp_name OPTIONAL
        item_price type zrp_om_price OPTIONAL
        item_quantity type zrp_quantity OPTIONAL.


          DATA creation_date type timestampl READ-ONLY.
  DATA cancellation_date    TYPE timestampl READ-ONLY.
  DATA  completion_date       TYPE  timestampl READ-ONLY.
  DATA itemid   TYPE zrp_id READ-ONLY.
  DATA customerid TYPE /dmo/customer_id READ-ONLY.
  DATA currency_code type /dmo/currency_code READ-ONLY.
  DATA item_name type zrp_name READ-ONLY.
  DATA item_price type zrp_om_price READ-ONLY.
  DATA item_quantity type zrp_quantity READ-ONLY.


  PROTECTED SECTION.
  PRIVATE SECTION.




ENDCLASS.



CLASS zrp_cm IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
    previous = previous
    ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->if_abap_behv_message~m_severity = severity.
    me->creation_date = creation_date.
    me->cancellation_date = cancellation_date.
    me->completion_date = completion_date .
    me->customerid = |{ customerid ALPHA = OUT }|.
    me->itemid = |{ itemid ALPHA = OUT }|.
    me->currency_code = |{ currency_code ALPHA = OUT }|.
    me->item_name = item_name.
    me->item_price = item_price.
    me->item_quantity = item_quantity.

  ENDMETHOD.
ENDCLASS.
