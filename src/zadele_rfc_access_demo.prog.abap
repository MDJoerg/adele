*&---------------------------------------------------------------------*
*& Report ZADELE_RFC_ACCESS_DEMO
*&---------------------------------------------------------------------*
*& Demonstrate the usage of the remote access handler
*& call the api in record or olap mode
*&---------------------------------------------------------------------*
REPORT zadele_rfc_access_demo NO STANDARD PAGE HEADING LINE-SIZE 1023.

*---------- interface
PARAMETERS: p_rfc   TYPE rfcdest DEFAULT 'NONE'.
PARAMETERS: p_max   TYPE i       DEFAULT 1000.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_srct  TYPE string OBLIGATORY DEFAULT '*Max*'.
PARAMETERS: p_srcr  RADIOBUTTON GROUP srch DEFAULT 'X'.
PARAMETERS: p_srcs  RADIOBUTTON GROUP srch.
PARAMETERS: p_srcm  RADIOBUTTON GROUP srch.
PARAMETERS: p_srcl  RADIOBUTTON GROUP srch.
PARAMETERS: p_srcd  RADIOBUTTON GROUP srch.
PARAMETERS: p_srdr  RADIOBUTTON GROUP srch.
PARAMETERS: p_srdd  RADIOBUTTON GROUP srch.
PARAMETERS: p_srdp  RADIOBUTTON GROUP srch.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_olap  AS CHECKBOX.



START-OF-SELECTION.

* -------- local data
  DATA: lt_data       TYPE TABLE OF zadele_s_exp_dd04t_tf.
  DATA: lv_fieldname  TYPE fieldname.

* -------- get api handler
  DATA(lr_api) = zcl_adele_rfc_access=>create( ).

* -------- configure
  lr_api->set_rfcdest( p_rfc ).
  lr_api->set_max_records( p_max ).


* -------- set search field
  CASE 'X'.
    WHEN p_srcd.
      lv_fieldname = 'DDTEXT'.
    WHEN p_srcr.
      lv_fieldname = 'REPTEXT'.
    WHEN p_srcs.
      lv_fieldname = 'SCRTEXT_S'.
    WHEN p_srcm.
      lv_fieldname = 'SCRTEXT_M'.
    WHEN p_srcl.
      lv_fieldname = 'SCRTEXT_L'.
    when p_srdr.
      lv_fieldname = 'ROLLNAME'.
    when p_srdd.
      lv_fieldname = 'DOMNAME'.
    when p_srdp.
      lv_fieldname = 'DEVCLASS'.
  ENDCASE.


* -------- set search params
  lr_api->add_param(
      iv_param  = lv_fieldname
      iv_value  = p_srct
  ).


* -------- olap mode: set dimension lang
  IF p_olap EQ abap_true.
    lr_api->add_dimension( 'DDLANGUAGE' ).
  ENDIF.


* ------- execute
  IF lr_api->execute(
    EXPORTING
      iv_function = 'Z_ADELE_EXP_DTEL_TXT'
    CHANGING
      ct_data     = lt_data ) EQ abap_false.

* ------- errors occured
    WRITE: / |Errors occured: { lr_api->get_error_message( ) } (Code: { lr_api->get_error_code( ) }| COLOR 6.
  ELSE.
* ------- output
    LOOP AT lt_data INTO DATA(ls_data).

      IF p_olap EQ abap_true.
        WRITE: / ls_data-ddlanguage,
                 ls_data-adesys_count.
      ELSE.
        WRITE: / ls_data-rollname,
                 ls_data-ddlanguage,
                 ls_data-ddtext,
                 ls_data-reptext,
                 ls_data-scrtext_l,
                 ls_data-scrtext_m,
                 ls_data-scrtext_l,
                 ls_data-devclass,
                 ls_data-domname,
                 ls_data-datatype,
                 ls_data-leng.
      ENDIF.
    ENDLOOP.

  ENDIF.
