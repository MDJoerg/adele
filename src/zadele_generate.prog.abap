*&---------------------------------------------------------------------*
*& Report ZADELE_GENERATE
*&---------------------------------------------------------------------*
*& generate abap code from given structure
*& use it for Adele RFC API, create CDS or other scenarios
*&
*& last change: 04.05.2019
*&---------------------------------------------------------------------*
REPORT zadele_generate.

* -------- inteface
PARAMETERS: p_struc TYPE tabname OBLIGATORY.
PARAMETERS: p_omsi  RADIOBUTTON GROUP opti DEFAULT 'X'. " macro commands rfc api
PARAMETERS: p_orst  RADIOBUTTON GROUP opti.             " struc definition program inline
PARAMETERS: p_odds  RADIOBUTTON GROUP opti.             " struc definition ddic
PARAMETERS: p_ocds  RADIOBUTTON GROUP opti.             " struc definition cds style
PARAMETERS: p_orpa  RADIOBUTTON GROUP opti.             " report params
PARAMETERS: p_orso  RADIOBUTTON GROUP opti.             " report select options


* -------- global data
DATA: gv_type     TYPE string.
DATA: gv_inttype  TYPE string.
DATA: gv_comment  TYPE string.


* -------- business logig
START-OF-SELECTION.

  DATA(lr_type) = cl_abap_typedescr=>describe_by_name( p_struc ).
  IF lr_type IS INITIAL.
    WRITE: / 'unknown object'.
  ELSE.
* get as struc
    TRY.
        DATA(lr_struc) = CAST cl_abap_structdescr( lr_type ).
      CATCH cx_sy_move_cast_error INTO DATA(lx_execption).
        WRITE: / 'object is not a table or struc:', p_struc.
        RETURN.
    ENDTRY.

* get fields
    DATA(lt_fields) = lr_struc->get_ddic_field_list( ).
    IF lt_fields[] IS INITIAL.
      WRITE: / 'No fields?!'.
      RETURN.
    ENDIF.

* protocol
    WRITE: / 'Structure initialized:', p_struc.
    ULINE.

* generate output
    CASE 'X'.
      WHEN p_omsi. " rfc macros
        PERFORM output_as_macro_selopt USING lt_fields.
      WHEN p_orst. " remote structure
        PERFORM output_as_remote_struc USING lt_fields.
      WHEN p_odds.
        PERFORM output_as_dic_struc USING lt_fields.
      WHEN p_ocds.
        PERFORM output_as_cds_struc USING lt_fields.
      WHEN p_orpa.
        PERFORM output_as_report_params USING lt_fields.
      WHEN p_orso.
        PERFORM output_as_report_selopt USING lt_fields.
      WHEN OTHERS.
        WRITE: / 'output not implemented yet'.
    ENDCASE.

  ENDIF.

* ============================ SUB Routines
*&---------------------------------------------------------------------*
*&      Form  PREPARE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_FIELD  text
*----------------------------------------------------------------------*
FORM prepare_type  USING    ps_field TYPE dfies.
  CLEAR: gv_type,
         gv_inttype,
         gv_comment.
  gv_type     = ps_field-rollname.
  DATA(lv_len) = CONV integer( ps_field-leng ).

  gv_inttype  = |{ ps_field-datatype }{ lv_len }|.
  IF gv_type IS INITIAL.
    gv_type = gv_inttype.
  ENDIF.

  gv_comment = |" domain { ps_field-domname }, { ps_field-datatype }({ lv_len }), { ps_field-inttype }({ ps_field-intlen })|.

ENDFORM.

FORM prepare_text  USING    ps_field TYPE dfies.
  CLEAR: gv_type,
         gv_inttype,
         gv_comment.
  gv_type     = ps_field-rollname.
  DATA(lv_len) = CONV integer( ps_field-leng ).

  gv_inttype  = |{ ps_field-datatype }{ lv_len }|.
  IF gv_type IS INITIAL.
    gv_type = gv_inttype.
  ENDIF.

  gv_comment = |" { ps_field-fieldtext }, { ps_field-rollname }, { ps_field-datatype }({ lv_len }))|.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  OUTPUT_AS_MACRO_SELOPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FIELDS  text
*----------------------------------------------------------------------*
FORM output_as_macro_selopt  USING    pt_fields TYPE ddfields.

  WRITE: / 'Output Type: macro statemants for Export API'.

  LOOP AT pt_fields INTO DATA(ls_field).
    WRITE: / |         ade_check_selopt { ls_field-fieldname }.|.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_AS_REMOTE_STRUC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FIELDS  text
*----------------------------------------------------------------------*
FORM output_as_remote_struc  USING    pt_fields TYPE ddfields.

  WRITE: / 'data: begin of ls_data,'.

  LOOP AT pt_fields INTO DATA(ls_field).
    PERFORM prepare_type USING ls_field.
    WRITE: / |        { ls_field-fieldname }  type { gv_type },     " { gv_comment }|.
  ENDLOOP.

  WRITE: / '      end of_ls_data.'.
  WRITE: / 'data: lt_data like table of ls_data.'.

ENDFORM.

FORM output_as_dic_struc  USING    pt_fields TYPE ddfields.

  LOOP AT pt_fields INTO DATA(ls_field).
    PERFORM prepare_type USING ls_field.
    WRITE: / ls_field-fieldname,
             sy-vline, "cl_abap_char_utilities=>horizontal_tab,
             'type',
             sy-vline, "cl_abap_char_utilities=>horizontal_tab,
             gv_type.
  ENDLOOP.



ENDFORM.

FORM output_as_cds_struc  USING    pt_fields TYPE ddfields.

  WRITE: / |@EndUserText.label : 'generated structure from { p_struc }|.
  WRITE: / |@AbapCatalog.enhancementCategory : #EXTENSIBLE_CHARACTER_NUMERIC|.
  WRITE: / |define structure XXX | && '{'.

  LOOP AT pt_fields INTO DATA(ls_field).
    PERFORM prepare_type USING ls_field.
    WRITE: / |  { ls_field-fieldname } : { gv_type };           // { gv_comment }|.
  ENDLOOP.
  WRITE: / '}'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form OUTPUT_AS_REPORT_PARAMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_FIELDS
*&---------------------------------------------------------------------*
FORM output_as_report_params  USING     pt_fields TYPE ddfields.

  WRITE: / |TABLES: { p_struc }.|.

  LOOP AT pt_fields INTO DATA(ls_field).
    PERFORM prepare_text USING ls_field.
    DATA(lv_par) = |P_{ ls_field-fieldname }|.
    WRITE: / |PARAMETERS: { lv_par } LIKE { p_struc }-{ ls_field-fieldname }.           { gv_comment }|.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form OUTPUT_AS_REPORT_SELOPT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_FIELDS
*&---------------------------------------------------------------------*
FORM output_as_report_selopt  USING   pt_fields TYPE ddfields.

  WRITE: / |TABLES: { p_struc }.|.

  LOOP AT pt_fields INTO DATA(ls_field).
    PERFORM prepare_text USING ls_field.
    DATA(lv_par) = |SO_{ ls_field-fieldname }|.
    WRITE: / |SELECT-OPTIONS: { lv_par } FOR { p_struc }-{ ls_field-fieldname }.           { gv_comment }|.
  ENDLOOP.

ENDFORM.
