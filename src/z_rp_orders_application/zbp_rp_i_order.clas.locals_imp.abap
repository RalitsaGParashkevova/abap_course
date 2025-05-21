CLASS lhc_Orders DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Orders RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Orders RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Orders RESULT result.

    METHODS calculateTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Orders~calculateTotalPrice.

    METHODS orderCancelled FOR MODIFY
      IMPORTING keys FOR ACTION Orders~orderCancelled RESULT result.

    METHODS orderCompleted FOR MODIFY
      IMPORTING keys FOR ACTION Orders~orderCompleted RESULT result.

    METHODS orderInProcess FOR MODIFY
      IMPORTING keys FOR ACTION Orders~orderInProcess RESULT result.

    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Orders~setInitialStatus.

    METHODS incrementOrderId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Orders~incrementOrderId.

    METHODS insertCreationDate FOR DETERMINE ON SAVE
      IMPORTING keys FOR Orders~insertCreationDate.

    METHODS currency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~currency.

    METHODS customerValidation FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~customerValidation.

    METHODS deliveryCountry FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~deliveryCountry.

    METHODS orderName FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~orderName.

    METHODS orderValidation FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~orderValidation.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Orders~validateDates.

ENDCLASS.

CLASS lhc_Orders IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD calculateTotalPrice.
  ENDMETHOD.

  METHOD orderCancelled.
  ENDMETHOD.

  METHOD orderCompleted.
  ENDMETHOD.

  METHOD orderInProcess.
  ENDMETHOD.

  METHOD setInitialStatus.
  ENDMETHOD.

  METHOD incrementOrderId.
  ENDMETHOD.

  METHOD insertCreationDate.
  ENDMETHOD.

  METHOD currency.
  ENDMETHOD.

  METHOD customerValidation.
  ENDMETHOD.

  METHOD deliveryCountry.
  ENDMETHOD.

  METHOD orderName.
  ENDMETHOD.

  METHOD orderValidation.
  ENDMETHOD.

  METHOD validateDates.
  ENDMETHOD.

ENDCLASS.
