*&---------------------------------------------------------------------*
*&  Include           ZADELE_EXPORT_API_INCLUDE
*&---------------------------------------------------------------------*
*& macro based API to extract table data to external systems via
*& rfc-enabled components
*&---------------------------------------------------------------------*
*& This code is part of the ADELE project by joomp.de
*&
*& license:         GNU General Public License version 2.0 (GPLv2)
*& project site:    https://sourceforge.net/projects/adele
*& contact:         adele@joomp.de
*&---------------------------------------------------------------------*
*& version: 2016/09/05
*&---------------------------------------------------------------------*
*& Usage:
*& Include this file by an include statement within your remote enabled
*& function module. There are 4 types of extraction modules supported.
*& Depending on this type you have to implement the function module
*& interface and add at least the sample coding. Additionally you
*& can implement authorizations and other topic by yourself.
*& Enjoy!
*&---------------------------------------------------------------------*
*& Variant A: Extract a table only
*FUNCTION Z_ADELE_EXPORT_SFNF.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  IMPORTING
**"     VALUE(IV_MAX) TYPE  SYTABIX OPTIONAL
**"  TABLES
**"      ET_DATA STRUCTURE  SFLIGHT
**"  EXCEPTIONS
**"      WRONG_INTERFACE
**"      WRONG_SQL
**"      ERRORS_OCCURED
**"----------------------------------------------------------------------
*
** -------- include ADELE interface
*  INCLUDE ZADELE_EXPORT_API_INCLUDE.
*
* TRY.
** ------------ process
*      ade_datadef.
*      ade_process_select sflight.
** ------------ process errors
*    CATCH cx_sy_dynamic_osql_semantics.
*      RAISE wrong_sql.
*  ENDTRY.
*
*ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Variant B: Extract a table with options
*&
*FUNCTION z_adele_export_sfwf.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  IMPORTING
**"     VALUE(IV_MAX) TYPE  SYTABIX OPTIONAL
**"     VALUE(IS_SELOPT) TYPE  ZADELE_RFC_API_EXPORT_SFWF_SEL OPTIONAL
**"  TABLES
**"      ET_DATA STRUCTURE  ZADELE_RFC_API_EXPORT_SFWF_TF
**"      IT_SELOPT STRUCTURE  AQDBOS
**"  EXCEPTIONS
**"      WRONG_INTERFACE
**"      WRONG_SQL
**"      ERRORS_OCCURED
**"----------------------------------------------------------------------
*
** -------- include ADELE interface
*  INCLUDE zadele_export_api_include.
*
*
*  TRY.
** ------------ check every field of structure is_selopt
*      ade_prepare_where_selopt.
** -------------
*      IF lv_ade_lin GT 0.
*        ade_check_selopt carrid.
*        ade_check_selopt connid.
*        ade_check_selopt fldate.
*        ade_check_selopt planetype.
*        ade_check_selopt currency.
*      ENDIF.
** ------------ process
*      ade_process_select sflight.
** ------------ process errors
*    CATCH cx_sy_dynamic_osql_semantics.
*      RAISE wrong_sql.
*  ENDTRY.
*
*ENDFUNCTION.
*&
*& Remarks:
*& IS_SELOPT contains all fields of ET_DATA with allowed filtering.
*& IT_SELOPT will be filled by the program with abap style
*& selection-options.
*&
*& You have to add a line add_check_selopt for every field of IS_SELOPT.
*& After ade_process_select the name of an physical table or view is
*& expected.
*&
*&---------------------------------------------------------------------*
*& Variant C: Extract and analyze a table with options (include OLAP)
*&
*FUNCTION Z_ADELE_EXPORT_SFOE.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  IMPORTING
**"     VALUE(IV_MAX) TYPE  SYTABIX OPTIONAL
**"     VALUE(IS_SELOPT) TYPE  ZADELE_RFC_API_EXPORT_SFWF_SEL OPTIONAL
**"     VALUE(IV_DIM) TYPE  STRING OPTIONAL
**"     VALUE(IV_FCT) TYPE  STRING OPTIONAL
**"  TABLES
**"      ET_DATA STRUCTURE  ZADELE_RFC_API_EXPORT_SFOE_TF
**"      IT_SELOPT STRUCTURE  AQDBOS
**"  EXCEPTIONS
**"      WRONG_INTERFACE
**"      WRONG_SQL
**"      ERRORS_OCCURED
**"----------------------------------------------------------------------
*
** -------- include ADELE interface
*  INCLUDE ZADELE_EXPORT_API_INCLUDE.
*
*
*   TRY.
** ------------ check every field of structure is_selopt
*       ade_prepare_where_selopt.
** -------------
*       IF lv_ade_lin GT 0.
*         ade_check_selopt carrid.
*         ade_check_selopt connid.
*         ade_check_selopt fldate.
*         ade_check_selopt planetype.
*         ade_check_selopt currency.
*       ENDIF.
** ------------ process
*       ade_process sflight.
** ------------ process errors
*     CATCH cx_sy_dynamic_osql_semantics.
*       RAISE wrong_sql.
*   ENDTRY.
*
*ENDFUNCTION.
*&
*& Remarks:
*& You have to enhance the structure of ET_DATA with an field
*& ADESYS_COUNT TYPE SYDBCNT to get the statistical and OLAP features.
*&
*& You have to add a line add_check_selopt for every field of IS_SELOPT.
*& After ade_process the name of an physical table or view is expected.
*&---------------------------------------------------------------------*
*& Variant D: Select an itab table and use filters
*&
*FUNCTION z_adele_export_sfus.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  IMPORTING
**"     VALUE(IV_MAX) TYPE  SYTABIX OPTIONAL
**"     VALUE(IS_SELOPT) TYPE  ZADELE_RFC_API_EXPORT_SFWF_SEL OPTIONAL
**"  TABLES
**"      ET_DATA STRUCTURE  ZADELE_RFC_API_EXPORT_SFWF_TF
**"      IT_SELOPT STRUCTURE  AQDBOS
**"  EXCEPTIONS
**"      WRONG_INTERFACE
**"      WRONG_SQL
**"      ERRORS_OCCURED
**"----------------------------------------------------------------------
*
** -------- include ADELE interface
*  INCLUDE zadele_export_api_include.
*
*
** ---------- select itab
*  PERFORM select_data TABLES et_data.
*
** ------- filter itab
*  ade_process_itab.
*
*ENDFUNCTION.
*&
*& Remarks:
*& Implement the select_data form and process filter criterias from
*& IT_SELOPT by yourself.
*&---------------------------------------------------------------------*

* =============================== init and defs =======================*
  DEFINE ade_datadef.
    data: lt_ade_sql type table of tdline.
    data: lv_ade_line like line of lt_ade_sql.
    data: lv_ade_name like aqdbos-name.
    data: lv_ade_tabix type sytabix.
    data: lv_ade_param_type(1).
    data: lv_ade_found type i.
    data: lv_ade_lin   type i.
    data: lv_ade_valid type xfeld.

    data: lr_ade_table type ref to data.
    data: lr_ade_wa    type ref to data.

    data: lt_ade_dim   type table of fieldname.
    data: lt_ade_fct   type table of fieldname.
    data: lv_ade_field type fieldname.
    data: ls_ade_wa    like line of et_data.

    field-symbols: <ade_table> type table.
    field-symbols: <ade_wa>    type data.
    field-symbols: <ade_count> type data.
    field-symbols: <ade_src>   type data.
    field-symbols: <ade_trg>   type data.

    field-symbols: <ade_param> type data.
    field-symbols: <ade_field> type data.

*   additional datadef for selopt
    data: ls_ade_selopt type AQDBOS. "like it_selopt.
    data: lt_ade_selopt like table of ls_ade_selopt.
    data: lt_ade_selopt2 like table of ls_ade_selopt.
    data: begin of ls_ade_iselopt,
             name  type fieldname,
             count type sydbcnt,
             range type table of lvc_s_rngs, "selopt
          end of ls_ade_iselopt.
    data: lt_ade_iselopt like table of ls_ade_iselopt.
    data: ls_ade_range type lvc_s_rngs. " selopt
    field-symbols: <ade_iselopt> like ls_ade_iselopt.

*   paging
    FIELD-SYMBOLS: <ade_skip> type i.

*   ------------ init
    ASSIGN ('IV_SKIP') to <ade_skip>.

  END-OF-DEFINITION.

* =============================== params =======================*
  DEFINE ade_bind_param.
    unassign: <ade_param>.
    assign &1 to <ade_param>.
  END-OF-DEFINITION.

* =============================== selopt =======================*
* check filter criterias coming from IT_SELOPT for a fieldname
* &1 - name of the fieldname
  DEFINE ade_check_selopt.
    " create range
    data: lt_&1 like range of et_data-&1.
    data: ls_&1 like line of lt_&1.

    " set name
    lv_ade_name = '&1'.
    translate lv_ade_name to upper case.

    " check options
    clear lv_ade_found.
    loop at lt_ade_selopt into ls_ade_selopt
        where kind eq 'S'
          and name eq lv_ade_name.
      add 1 to lv_ade_found.
      move-corresponding ls_ade_selopt to ls_&1.
      append ls_&1 to lt_&1.
    endloop.
    " sort and clean
    sort lt_&1.
    delete adjacent duplicates from lt_&1.
    " append to code
    if lv_ade_found gt 0.
      if lt_ade_sql is not initial.
        append 'AND' to lt_ade_sql.
      endif.
      concatenate '&1' 'IN' 'LT_&1' into lv_ade_line separated by ' '.
      append lv_ade_line to  lt_ade_sql.
    endif.
  END-OF-DEFINITION.

* check for leading zeros
* &1 - name of the filter criteria
* &2 - reference field for type mapping
  DEFINE ade_check_zeros.
    if lt_ade_selopt[] is not initial.
      " set name
      lv_ade_name = '&1'.
      translate lv_ade_name to upper case.

      " loop selopt
      loop at lt_ade_selopt into ls_ade_selopt
          where kind eq 'S'
            and name eq lv_ade_name.

        if ls_ade_selopt-low is not initial.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = ls_ade_selopt-low
            importing
              output = &2.
          ls_ade_selopt-low = &2.
        endif.

        if ls_ade_selopt-high is not initial.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = ls_ade_selopt-high
            importing
              output = &2.
          ls_ade_selopt-high = &2.
        endif.

        modify lt_ade_selopt from ls_ade_selopt.
      endloop.
    endif.
  END-OF-DEFINITION.

* add a default value if no other values are available (EQ)
* &1 - name of the filter
* &2 - filter value
  DEFINE ade_add_default.
    lv_ade_name = '&1'.
    translate lv_ade_name to upper case.
    read table lt_ade_selopt transporting no fields
      with key name = lv_ade_name kind = 'S'.
    if sy-subrc ne 0.
      clear ls_ade_selopt.
      ls_ade_selopt-kind    = 'S'.
      ls_ade_selopt-name    = lv_ade_name.
      ls_ade_selopt-sign    = 'I'.
      ls_ade_selopt-option  = 'EQ'.
      ls_ade_selopt-low     = &2.
      translate ls_ade_selopt-name to upper case.
      append ls_ade_selopt to lt_ade_selopt.
      sort lt_ade_selopt.
      describe table lt_ade_selopt lines lv_ade_lin.
    endif.
  END-OF-DEFINITION.
* add a default value if no other values are available (with option)
* &1 - name of the filter
* &2 - option (EQ, CP)
* &3 - filter value
  DEFINE ade_add_default_full.
    lv_ade_name = '&1'.
    translate lv_ade_name to upper case.
    read table lt_ade_selopt transporting no fields
      with key name = lv_ade_name kind = 'S'.
    if sy-subrc ne 0.
      clear ls_ade_selopt.
      ls_ade_selopt-kind    = 'S'.
      ls_ade_selopt-name    = lv_ade_name.
      ls_ade_selopt-sign    = 'I'.
      ls_ade_selopt-option  = &2.
      ls_ade_selopt-low     = &3.
      translate ls_ade_selopt-name to upper case.
      append ls_ade_selopt to lt_ade_selopt.
      sort lt_ade_selopt.
      describe table lt_ade_selopt lines lv_ade_lin.
    endif.
  END-OF-DEFINITION.

* add a default value if no other values are available (interval)
* &1 - name of the filter
* &2 - filter value from
* &3 - filter value to
  DEFINE ade_add_default_interval.
    lv_ade_name = '&1'.
    translate lv_ade_name to upper case.
    read table lt_ade_selopt transporting no fields
      with key name = lv_ade_name kind = 'S'.
    if sy-subrc ne 0.
      clear ls_ade_selopt.
      ls_ade_selopt-kind    = 'S'.
      ls_ade_selopt-name    = lv_ade_name.
      ls_ade_selopt-sign    = 'I'.
      ls_ade_selopt-option  = 'BT'.
      ls_ade_selopt-low     = &2.
      ls_ade_selopt-high    = &3.
      translate ls_ade_selopt-name to upper case.
      append ls_ade_selopt to lt_ade_selopt.
      sort lt_ade_selopt.
      describe table lt_ade_selopt lines lv_ade_lin.
    endif.
  END-OF-DEFINITION.

*
  DEFINE ade_prepare_itab_selopt.
    refresh lt_ade_iselopt.
    loop at &1 into ls_ade_selopt.
      lv_ade_tabix = sy-tabix.

      unassign <ade_iselopt>.
      read table lt_ade_iselopt assigning <ade_iselopt>
        with key name = ls_ade_selopt-name.
      if sy-subrc ne 0.
        clear ls_ade_iselopt.
        ls_ade_iselopt-name   = ls_ade_selopt-name.
        ls_ade_iselopt-count  = 1.
        append ls_ade_iselopt to lt_ade_iselopt.
        read table lt_ade_iselopt assigning <ade_iselopt>
          with key name = ls_ade_selopt-name.
      endif.

      clear ls_ade_range.
      move-corresponding ls_ade_selopt to ls_ade_range.
      append ls_ade_range to <ade_iselopt>-range.

      sy-tabix = lv_ade_tabix.
    endloop.
  END-OF-DEFINITION.

  DEFINE ade_prepare_where_selopt.
*   include global data def
    ade_datadef.

*   coding
    refresh: lt_ade_selopt, lt_ade_selopt2.
    describe table it_selopt lines lv_ade_lin.
    if lv_ade_lin gt 0.
*     check for given param is_selopt_db
      ade_bind_param ('IS_SELOPT_DB').
      if <ade_param> is not assigned.
*         all params goes to main select
        lt_ade_selopt[] = it_selopt[].
      else.
        loop at it_selopt into ls_ade_selopt.
          unassign <ade_field>.
          assign component ls_ade_selopt-name of structure <ade_param> to <ade_field>.
          if <ade_field> is assigned.
            append ls_ade_selopt to lt_ade_selopt.
          else.
            append ls_ade_selopt to lt_ade_selopt2.
          endif.
        endloop.
      endif.
    endif.
  END-OF-DEFINITION.
* =============================== itab =======================*
  DEFINE ade_filter_itab.
    if lt_ade_iselopt[] is not initial.
      clear lv_ade_found.
      loop at &1 into ls_ade_wa.
        lv_ade_tabix = sy-tabix.

        if iv_max > 0 and lv_ade_found ge iv_max.
          delete &1.
          continue.
        endif.

        lv_ade_valid = 'X'.
        loop at lt_ade_iselopt into ls_ade_iselopt.
          unassign <ade_field>.
          assign component ls_ade_iselopt-name of structure ls_ade_wa to <ade_field>.
          if <ade_field> is not assigned.
            raise wrong_sql.
          endif.

          if <ade_field> not in ls_ade_iselopt-range.
            clear lv_ade_valid.
            exit. " from loop
          endif.
        endloop.

        if lv_ade_valid ne 'X'.
          delete &1.
        else.
          add 1 to lv_ade_found.
        endif.

        sy-tabix = lv_ade_tabix.
      endloop.
    else.
      describe table &1 lines lv_ade_lin.
      if iv_max > 0 and lv_ade_lin gt iv_max.
        loop at &1 into ls_ade_wa.
          lv_ade_tabix = sy-tabix.
          if lv_ade_tabix > iv_max.
            delete &1.
          endif.
        endloop.
      endif.
    endif.
  END-OF-DEFINITION.
* process itab only for table mode
  DEFINE ade_process_itab.
    ade_datadef.
    ade_prepare_itab_selopt it_selopt.
    ade_filter_itab et_data.
  END-OF-DEFINITION.
* =============================== olap =======================*
  DEFINE ade_prepare_groupby.
*   local data
    data: lt_ade_split type table of string.
    data: lv_ade_split type string.
    data: lv_ade_fields type string.
    data: lv_ade_groupby type string.

*   set initial field list
    lv_ade_fields = 'COUNT(*) AS ADESYS_COUNT'.

*   check for *
    if iv_dim ne '*'.
*   split groupby
      if iv_dim cs ','.
        split iv_dim at ',' into table lt_ade_split.
      else.
        append iv_dim to lt_ade_split.
      endif.

*   create group by
      clear lv_ade_groupby.
      loop at lt_ade_split into lv_ade_split.
*     append dimension
        concatenate lv_ade_fields lv_ade_split
          into lv_ade_fields separated by ' '.
*     append groupby
        if lv_ade_groupby is initial.
          lv_ade_groupby = lv_ade_split.
        else.
          concatenate lv_ade_groupby lv_ade_split
            into lv_ade_groupby separated by ' '.
        endif.
      endloop.
    endif.

*   check for facts
    if iv_fct is not initial.
*     split
      refresh lt_ade_split.
      if iv_fct cs ','.
        split iv_fct at ',' into table lt_ade_split.
      else.
        append iv_fct to lt_ade_split.
      endif.
*     append fields
      loop at lt_ade_split into lv_ade_split.
        concatenate lv_ade_fields
                    'SUM('
                    lv_ade_split
                    ') AS'
                     lv_ade_split
                    into lv_ade_fields
                    separated by ' '.
      endloop.
    endif.
  END-OF-DEFINITION.
* -------- process itab mode with params
* &1 table to parse
* &2 optional: table to collect (if empty: &1 will be used as output)
  DEFINE ade_bind_to_table.
    get reference of &1 into lr_ade_table.
    assign lr_ade_table->* to <ade_table>.
  END-OF-DEFINITION.
  DEFINE ade_process_itab_olap_noddef.
* --- prepare collect table
    unassign: <ade_table>.
    describe field &2 type lv_ade_param_type.
    if lv_ade_param_type eq 'C'.
      create data lr_ade_table like &1[].
      assign lr_ade_table->* to <ade_table>.
    else.
      ade_bind_to_table &2.
    endif.
    if <ade_table> is not assigned.
      raise wrong_interface.
    endif.

*   create work area
    create data lr_ade_wa like line of <ade_table>.
    assign lr_ade_wa->* to <ade_wa>.
*   assign count
    assign component 'ADESYS_COUNT' of structure <ade_wa> to <ade_count>.
    if <ade_count> is not assigned.
      refresh et_data.
      raise wrong_interface.
    endif.
*   dim required
    if iv_dim is not initial.
      if iv_dim cs ','.
        split iv_dim at ',' into table lt_ade_dim.
      else.
        append iv_dim to lt_ade_dim.
      endif.
    endif.

*   fct optionak
    if iv_fct is not initial.
      if iv_fct cs ','.
        split iv_fct at ',' into table lt_ade_fct.
      else.
        append iv_fct to lt_ade_fct.
      endif.
    endif.
*   loop
    loop at &1 into ls_ade_wa.
      lv_ade_tabix = sy-tabix.

*     check iv_max
      if iv_max gt 0 and lv_ade_tabix gt iv_max.
        exit.
      endif.

*     reset
      clear <ade_wa>.
      <ade_count> = 1.

*     get dims
      loop at lt_ade_dim into lv_ade_field.
        condense lv_ade_field.
        unassign: <ade_src>, <ade_trg>.
        assign component lv_ade_field of structure <ade_wa> to <ade_trg>.
        assign component lv_ade_field of structure ls_ade_wa to <ade_src>.
        if <ade_src> is not assigned or <ade_trg> is not assigned.
          raise wrong_sql.
        else.
          <ade_trg> = <ade_src>.
        endif.
      endloop.

*     get fct
      loop at lt_ade_fct into lv_ade_field.
        condense lv_ade_field.
        unassign: <ade_src>, <ade_trg>.
        assign component lv_ade_field of structure <ade_wa> to <ade_trg>.
        assign component lv_ade_field of structure ls_ade_wa to <ade_src>.
        if <ade_src> is not assigned or <ade_trg> is not assigned.
          raise wrong_sql.
        else.
          <ade_trg> = <ade_src>.
        endif.
      endloop.

*     collect
      collect <ade_wa> into <ade_table>.

      sy-tabix = lv_ade_tabix.
    endloop.

*   fill export and release mem
    sort <ade_table>.
    if lv_ade_param_type eq 'C'.
      &1[] = <ade_table>.
      unassign <ade_table>.
      free lr_ade_table.
    endif.
  END-OF-DEFINITION.
* process itab for table and olap
  DEFINE ade_process_itab_olap.
* --------- get access to table
    if iv_dim is initial and iv_fct is initial.
* --------- no olap
      ade_process_itab.
    else.
* ---------- olap
      ade_process_itab_olap_noddef et_data space.
    endif.
  END-OF-DEFINITION.

  DEFINE ade_process_itab_user_select.
*      filter data if itab separated selopt
    if lt_ade_selopt2[] is not initial.
      ade_prepare_itab_selopt lt_ade_selopt2.
      ade_filter_itab &1.
    endif.
*      olap wanted?
    if iv_dim is not initial
      or iv_fct is not initial.
      ade_process_itab_olap_noddef &1 et_data[].
    else.
      append lines of &1 to et_data.
    endif.
  END-OF-DEFINITION.

* =============================== process =======================*
  DEFINE ade_process.
    if iv_dim is initial.
* ------------ select dataset
      ade_process_select &1.
    else.
* ------------ select statistics
      ade_process_groupby &1.
    endif.
  END-OF-DEFINITION.
  DEFINE ade_process_select.
    select * from &1
      into corresponding fields of table et_data
      up to iv_max rows
      where (lt_ade_sql).
    if lt_ade_selopt2[] is not initial.
      ade_prepare_itab_selopt lt_ade_selopt2.
      ade_filter_itab et_data.
    endif.
  END-OF-DEFINITION.
  DEFINE ade_process_groupby.
    ade_prepare_groupby.
    select (lv_ade_fields)
      from &1
      into corresponding fields of table et_data
      up to iv_max rows
      where (lt_ade_sql)
      group by (lv_ade_groupby).
  END-OF-DEFINITION.
* =============================== other utils =======================*
  DEFINE ade_auth_check_table.
    authority-check object 'S_TABU_NAM'
             id 'ACTVT' field '03'
             id 'TABLE' field &1.
    if sy-subrc ne 0.
      raise wrong_authorization.
    endif.
  END-OF-DEFINITION.
