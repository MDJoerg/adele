*&---------------------------------------------------------------------*
*&  Include           ZADELE_IMPORT_API_INCLUDE
*&---------------------------------------------------------------------*
*& macro based API to import table data from external systems via
*& rfc-enabled components
*&---------------------------------------------------------------------*
*& This code is part of the ADELE project by joomp.de
*&
*& project site:  https://sourceforge.net/projects/adele
*& email to:      adele@joomp.de
*& license:       GNU General Public License version 2.0 (GPLv2)
*&---------------------------------------------------------------------*
*& version: 2016/09/05
*&---------------------------------------------------------------------*
*& Usage:
*& Include this file by an include statement within your remote enabled
*& function module. Implement a function module interface as described
*& below. At least a table IT_DATA was required to post data to sap.
*& Additionally you can implement authorizations and other topics
*& by yourself.
*&
*& Please keep in mind that use these type of import should only be
*& used for own z-tables. If you want to add entries to standard
*& sap objects use the official apis (e.g. BAPI) oder other techniques
*& like batch input.
*&
*& Enjoy!
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* The example function use the following structures.
*
* Table ZADIMPM_CULIST
*MANDT  MANDT
*ID	CHAR32
*.INCLUDE	ZADELE_IMP_CULIST_DATA
*NAME_ORG1  BU_NAMEOR1
*NAME_ORG2  BU_NAMEOR2
*COUNTRY  LAND1
*POST_CODE1	AD_PSTCD1
*CITY	AD_CITY1
*STREET	AD_STREET
*HOUSE_NUM1	AD_HSNM1
*EXTID  BU_BPEXT
*.INCLUDE	ZADELE_IMP_CULIST_DATA_INT
*CHDAT  BU_CHDAT
*CHTIM  BU_CHTIM
*CHUSER	BU_CHUSR
*
* transfer structure ZADELE_IMP_DATA_TF is defined as
*ID	CHAR32
*.INCLUDE	ZADELE_IMP_CULIST_DATA
*&
*&---------------------------------------------------------------------*
*& Variant A) keys comes from extern
*&
*FUNCTION z_adele_import_cdek.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  TABLES
**"      IT_DATA STRUCTURE  ZADELE_IMP_CULIST_DATA_TF OPTIONAL
**"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
**"----------------------------------------------------------------------
*
** ------- include adele import interface
*  INCLUDE zadele_import_api_include.
*
*  TRY.
** ------- init ade import interface
*      ade_imp_prepare 'ZADELE_IAPICUST'.
** ------- check import params
*      IF it_data[] IS INITIAL.
*        ade_imp_add_msg 'E' 'no data'.
*      ELSE.
** ------- process
*        ade_imp_process.
*      ENDIF.
** ------------ process errors
*    CATCH cx_sy_dynamic_osql_semantics.
*    CATCH cx_sy_dynamic_osql_syntax.
*    CATCH cx_sy_open_sql_db.
*      ade_imp_add_msg 'E' 'errors occured'.
*  ENDTRY.
*
*ENDFUNCTION.
*&---------------------------------------------------------------------*
*& Variant B) keys comes from extern with default values
*&
*FUNCTION z_adele_import_cddf.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  TABLES
**"      IT_DATA STRUCTURE  ZADELE_IMP_CULIST_DATA_TF OPTIONAL
**"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
**"----------------------------------------------------------------------
*
** ------- include adele import interface
*  INCLUDE zadele_import_api_include.
*
** ------- local data
*  DATA: ls_db TYPE zadele_imp_culist_data_int.
*
*
*  TRY.
** ------- init ade import interface
*      ade_imp_prepare 'ZADELE_IAPICUST'.
** ------- check import params
*      IF it_data[] IS INITIAL.
*        ade_imp_add_msg 'E' 'no data'.
*      ELSE.
** ------- set default data
*        ls_db-chdat  = sy-datum.
*        ls_db-chtim  = sy-uzeit.
*        ls_db-chuser = sy-uname.
*        MOVE-CORRESPONDING ls_db TO <db_def>.
** ------- process
*        ade_imp_process.
*      ENDIF.
** ------------ process errors
*    CATCH cx_sy_dynamic_osql_semantics.
*    CATCH cx_sy_dynamic_osql_syntax.
*    CATCH cx_sy_open_sql_db.
*      ade_imp_add_msg 'E' 'errors occured'.
*  ENDTRY.
*
*ENDFUNCTION.
*&---------------------------------------------------------------------*
*& Variant C) set internal keys and modify record
*&
*& add these lines to the function group top
*
** ------- include adele import interface
*INCLUDE zadele_import_api_include.
*
** ------- global data
*DATA: gs_db TYPE zadele_iapicust.
*
** ------- init ade import interface
*ade_imp_datadef.
** ------- activate callback
*ade_imp_prepare_callback.
*
**----------------------------------------------------------------------*
**       CLASS lcl_callback IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS lcl_callback IMPLEMENTATION.
*  METHOD  modify_table_row.
**     get current record in db struc
*    MOVE-CORRESPONDING cs_row TO gs_db.
**     get new guid
*    ade_imp_get_guid gs_db-id.
**     save back
*    MOVE-CORRESPONDING gs_db TO cs_row.
*  ENDMETHOD.                    "modify_table_row
*ENDCLASS.                    "lcl_callback IMPLEMENTATION
*
*& then create a function module in the same function group
*
*FUNCTION z_adele_import_cdik.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  TABLES
**"      IT_DATA STRUCTURE  ZADELE_IMP_CULIST_DATA_TF OPTIONAL
**"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
**"----------------------------------------------------------------------
*
** ------- local data
*  DATA: ls_db TYPE zadele_imp_culist_data_int.
*
*
*  TRY.
** ------- init ade import interface
*      ade_imp_prepare 'ZADELE_IAPICUST'.
** ------- check import params
*      IF it_data[] IS INITIAL.
*        ade_imp_add_msg 'E' 'no data'.
*      ELSE.
** ------- set default data
*        ls_db-chdat  = sy-datum.
*        ls_db-chtim  = sy-uzeit.
*        ls_db-chuser = sy-uname.
*        MOVE-CORRESPONDING ls_db TO <db_def>.
** ------- process with callback
*        ade_imp_activate_callback.
*        ade_imp_process_callback.
*      ENDIF.
** ------------ process errors
*    CATCH cx_sy_dynamic_osql_semantics.
*    CATCH cx_sy_dynamic_osql_syntax.
*    CATCH cx_sy_open_sql_db.
*      ade_imp_add_msg 'E' 'errors occured'.
*  ENDTRY.
*
*ENDFUNCTION.
*
*&---------------------------------------------------------------------*

* define variables
DEFINE ade_imp_datadef.
* -------- define vars
  data: lr_ade_imp_tab      type ref to data.
  data: lr_ade_imp_wa       type ref to data.
  data: lr_ade_imp_def      type ref to data.
  data: lv_ade_imp_tabname  type string.
  data: lv_ade_imp_save_insert type xfeld.
  data: lv_ade_imp_db_cnt   type i.
  data: lv_ade_imp_max_pkg  type i.
  data: lv_ade_imp_tabix    type sytabix.
  data: lv_ade_imp_guid16   type guid_16.
  data: lv_ade_imp_guid22   type guid_22.
  data: lv_ade_imp_guid32   type guid_32.
  field-symbols: <db_tab>   type table.
  field-symbols: <db_wa>    type data.
  field-symbols: <db_def>   type data.
END-OF-DEFINITION.

* init variables depending on the function module interface
* &1 - the name of the database table
DEFINE ade_imp_init.

* -------- datdef depending on interface
  data: ls_ade_imp_data     like line of it_data[].
  data: ls_ade_imp_return   like line of et_return.

* -------- remember the db tabname
  lv_ade_imp_tabname = &1.

* -------- create dynamic data
  create data lr_ade_imp_tab type table of (&1).
  assign lr_ade_imp_tab->* to <db_tab>.

  create data lr_ade_imp_wa like line of <db_tab>.
  assign lr_ade_imp_wa->* to <db_wa>.

  create data lr_ade_imp_def like line of <db_tab>.
  assign lr_ade_imp_def->* to <db_def>.

END-OF-DEFINITION.

* datadef and init in one step
* &1 - the name of the database table
DEFINE ade_imp_prepare.
  ade_imp_datadef.
  ade_imp_init &1.
END-OF-DEFINITION.

* define interace for callback mode
DEFINE ade_imp_prepare_callback.
*----------------------------------------------------------------------*
*       CLASS LCL_CALLBACK DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  class lcl_callback definition.
    public section.
      methods:
        modify_table_row
          importing iv_tabix type sytabix
          changing  cs_row type data.
  endclass.                    "LCL_CALLBACK DEFINITION
END-OF-DEFINITION.

* add a simple message to return table
* &1 - type like E A X I W S
* &2 - the message itself
DEFINE ade_imp_add_msg.
* reset
  clear ls_ade_imp_return.
* fill
  ls_ade_imp_return-id         = '*'.
  ls_ade_imp_return-number     = '000'.
  ls_ade_imp_return-type       = &1.
  ls_ade_imp_return-message    = &2.
* append
  append ls_ade_imp_return to et_return.
END-OF-DEFINITION.
* add a complex message to the return table
* &1 - type like E A X W I
* &2 - message class
* &3 - message class number
* &4 - parameter 1 (use space if empty)
* &5 - parameter 2 (use space if empty)
* &6 - parameter 3 (use space if empty)
* &7 - parameter 4 (use space if empty)
DEFINE ade_imp_add_msg_full.
* reset
  clear ls_ade_imp_return.
* fill
  ls_ade_imp_return-type       = &1.
  ls_ade_imp_return-id         = &2.
  ls_ade_imp_return-number     = &3.
  ls_ade_imp_return-message_v1 = &4.
  ls_ade_imp_return-message_v2 = &5.
  ls_ade_imp_return-message_v3 = &6.
  ls_ade_imp_return-message_v4 = &7.
* create message
  message id     ls_ade_imp_return-id
          type   ls_ade_imp_return-type
          number ls_ade_imp_return-number
          with   ls_ade_imp_return-message_v1
                 ls_ade_imp_return-message_v2
                 ls_ade_imp_return-message_v3
                 ls_ade_imp_return-message_v4
          into   ls_ade_imp_return-message.
* append
  append ls_ade_imp_return to et_return.
END-OF-DEFINITION.
* activate package mode for early commit
* &1 - package size (0 for deactivation)
DEFINE ade_imp_set_max_pkg.
  lv_ade_imp_max_pkg = &1.
END-OF-DEFINITION.

* internal: save the current table and reset
DEFINE ade_imp_process_save_db.
  if <db_tab>[] is not initial.
    if lv_ade_imp_save_insert eq 'X'.
*       insert all new to database
        insert (lv_ade_imp_tabname) from table <db_tab>.
    else.
*       modify all new to database
        modify (lv_ade_imp_tabname) from table <db_tab>.
    endif.
* commit data
    commit work.
* reset
    refresh <db_tab>.
    clear lv_ade_imp_db_cnt.
  endif.
END-OF-DEFINITION.

* standard call to save given table it_data to database table
DEFINE ade_imp_process.
*   transfer to db
  loop at it_data into ls_ade_imp_data.
*   reset
    clear <db_wa>.
*   fill
    move-corresponding <db_def> to <db_wa>.
    move-corresponding ls_ade_imp_data to <db_wa>.
*   append and count
    append <db_wa> to <db_tab>.
    add 1 to lv_ade_imp_db_cnt.
*   check for package mode
    if lv_ade_imp_max_pkg > 0 and lv_ade_imp_db_cnt >= lv_ade_imp_max_pkg.
      ade_imp_process_save_db.
    endif.
  endloop.
* save to db
  ade_imp_process_save_db.
END-OF-DEFINITION.

* save all data given by IT_DATA to target table and modify rows per callback
DEFINE ade_imp_process_callback.
*   transfer to db
  loop at it_data into ls_ade_imp_data.
*   get tabix
    lv_ade_imp_tabix = sy-tabix.
*   reset
    clear <db_wa>.
*   fill
    move-corresponding <db_def> to <db_wa>.
    move-corresponding ls_ade_imp_data to <db_wa>.
*   call callback
    if lr_ade_imp_callback is not initial.
      lr_ade_imp_callback->modify_table_row( exporting iv_tabix = lv_ade_imp_tabix changing cs_row = <db_wa> ).
    endif.
*   append and count
    append <db_wa> to <db_tab>.
    add 1 to lv_ade_imp_db_cnt.
*   check for package mode
    if lv_ade_imp_max_pkg > 0 and lv_ade_imp_db_cnt >= lv_ade_imp_max_pkg.
      ade_imp_process_save_db.
    endif.
  endloop.
* save to db
  ade_imp_process_save_db.
END-OF-DEFINITION.

* activate the callback mode
DEFINE ade_imp_activate_callback.
  data: lr_ade_imp_callback type ref to lcl_callback.
  create object lr_ade_imp_callback.
END-OF-DEFINITION.

* get a key info as guid, possible types X(16) or C(32)
* &1 - key field
DEFINE ade_imp_get_guid.
  call function 'GUID_CREATE'
    importing
      ev_guid_16 = lv_ade_imp_guid16.
  &1         = lv_ade_imp_guid16.
END-OF-DEFINITION.

* get a guid with 22 chars, possible type c(22)
* &1 - key field
DEFINE ade_imp_get_guid22.
  call function 'GUID_CREATE'
    importing
      ev_guid_22 = lv_ade_imp_guid22.
  &1         = lv_ade_imp_guid22.
END-OF-DEFINITION.

* get a new number from number range
* &1 - number range (e.g. ADRV)
* &2 - interval (e.g. 01)
* &3 - key field
DEFINE ade_imp_get_new_number.
  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = &2
      object                  = &1
    importing
      number                  = &3
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
END-OF-DEFINITION.

* set the insert mode to save data
DEFINE ade_imp_set_mode_insert.
  lv_ade_imp_save_insert = 'X'.
END-OF-DEFINITION.
* set the modify mode to save data
DEFINE ade_imp_set_mode_modify.
  lv_ade_imp_save_insert = ' '.
END-OF-DEFINITION.
