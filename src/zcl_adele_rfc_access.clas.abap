class ZCL_ADELE_RFC_ACCESS definition
  public
  create public .

public section.

  interfaces ZIF_ADELE_RFC_ACCESS .

  class-methods CREATE
    returning
      value(RR_INSTANCE) type ref to ZIF_ADELE_RFC_ACCESS .
  PROTECTED SECTION.

    DATA mv_error_message TYPE bapi_msg .
    DATA mv_error_subrc TYPE sysubrc .
    DATA mv_max_records TYPE i VALUE 10000 ##NO_TEXT.
    DATA mv_dimension TYPE string .
    DATA mv_facts TYPE string .
    DATA mt_selopt TYPE aqtdbos .
    DATA mv_rfcdest TYPE rfcdest VALUE 'NONE'.

    METHODS set_error
      IMPORTING
        !iv_msg  TYPE data
        !iv_code TYPE i DEFAULT 1 .

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADELE_RFC_ACCESS IMPLEMENTATION.


  METHOD create.
    " delegate this interface
    rr_instance = NEW  zcl_adele_rfc_access( ).
  ENDMETHOD.


  METHOD set_error.
    mv_error_message = iv_msg.
    mv_error_subrc   = iv_code.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~add_dimension.
    IF mv_dimension IS INITIAL.
      mv_dimension = iv_dim.
    ELSE.
      mv_dimension = |{ mv_dimension }, { iv_dim }|.
    ENDIF.
    rr_self = me.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~add_fact.
    IF mv_facts IS INITIAL.
      mv_facts = iv_fact.
    ELSE.
      mv_facts = |{ mv_facts }, { iv_fact }|.
    ENDIF.
    rr_self = me.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~add_param.

    DATA: ls_selpar TYPE rsparams.

*   add default param
    APPEND INITIAL LINE TO mt_selopt ASSIGNING FIELD-SYMBOL(<ls_param>).
    <ls_param>-name   = iv_param.
    <ls_param>-kind   = 'S'.
    <ls_param>-sign   = iv_sign.
    <ls_param>-option = iv_option.
    <ls_param>-low    = iv_value.
    <ls_param>-high   = space.

*   check for wildcard
    IF <ls_param>-option EQ 'EQ'
      AND <ls_param>-low CS '*'.
      <ls_param>-option = 'CP'.
    ENDIF.

    rr_self = me.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~add_param_interval.
    DATA: ls_selpar TYPE rsparams.

*   add default param
    APPEND INITIAL LINE TO mt_selopt ASSIGNING FIELD-SYMBOL(<ls_param>).
    <ls_param>-name   = iv_param.
    <ls_param>-kind   = 'S'.
    <ls_param>-sign   = iv_sign.
    <ls_param>-option = 'BT'.
    <ls_param>-low    = iv_from.
    <ls_param>-high   = iv_to.

    rr_self = me.

  ENDMETHOD.


  METHOD zif_adele_rfc_access~execute.

* ----- reset error
    rv_success = abap_false.
    zif_adele_rfc_access~reset_error( ).

* ----- check rfc dest
    IF mv_rfcdest IS INITIAL.
      set_error(
          iv_msg  = |wrong rfc destination|
          iv_code = 500
      ).
      RETURN.
    ENDIF.

* ----- call remote function
    CALL FUNCTION iv_function
      DESTINATION mv_rfcdest
      EXPORTING
        iv_max                = mv_max_records
*       IS_SELOPT             =
        iv_dim                = mv_dimension
        iv_fct                = mv_facts
      TABLES
        et_data               = ct_data
        it_selopt             = mt_selopt
      EXCEPTIONS
        wrong_interface       = 1
        wrong_sql             = 2
        errors_occured        = 3
        communication_failure = 4 MESSAGE mv_error_message
        system_failure        = 5 MESSAGE mv_error_message
        OTHERS                = 6.
    IF sy-subrc <> 0.
      IF mv_error_message IS INITIAL.
        mv_error_message = |unknown. subrc = { sy-subrc }|.
        mv_error_subrc   = sy-subrc.
      ENDIF.
    ELSE.
      rv_success = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_adele_rfc_access~get_error_code.
    rv_subrc = mv_error_subrc.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~get_error_message.
    rv_error = mv_error_message.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~get_rfcdest.
    rv_rfcdest = mv_rfcdest.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~reset.
    CLEAR: mv_dimension,
           mv_facts,
           mv_max_records,
           mt_selopt.
    rr_self = me.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~reset_error.
    CLEAR: mv_error_message,
           mv_error_subrc.
    rr_self = me.
  ENDMETHOD.


  METHOD zif_adele_rfc_access~set_max_records.
    mv_max_records = iv_max_records.
    rr_self = me.
  ENDMETHOD.


  method ZIF_ADELE_RFC_ACCESS~SET_RFCDEST.
    mv_rfcdest = iv_rfcdest.
    rr_self = me.
  endmethod.
ENDCLASS.
