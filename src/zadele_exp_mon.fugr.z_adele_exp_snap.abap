FUNCTION z_adele_exp_snap.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MAX) TYPE  SYTABIX DEFAULT 100000
*"     VALUE(IS_SELOPT) TYPE  ZADELE_S_EXP_SNAP_SEL OPTIONAL
*"     VALUE(IV_DIM) TYPE  STRING OPTIONAL
*"     VALUE(IV_FCT) TYPE  STRING OPTIONAL
*"  TABLES
*"      ET_DATA STRUCTURE  ZADELE_S_EXP_SNAP_TF
*"      IT_SELOPT STRUCTURE  AQDBOS
*"  EXCEPTIONS
*"      WRONG_INTERFACE
*"      WRONG_SQL
*"      ERRORS_OCCURED
*"----------------------------------------------------------------------

* -------- include ADELE interface
  INCLUDE zadele_export_api_include.


  TRY.
* ------------ check every field of structure is_selopt
      ade_prepare_where_selopt.
* ------------- add default (only first line is relevant)
      ade_add_default seqno '000'.
* ------------- process filters
      IF lv_ade_lin GT 0.
        ade_check_selopt datum.
        ade_check_selopt uzeit.
        ade_check_selopt ahost.
        ade_check_selopt uname.
        ade_check_selopt mandt.
        ade_check_selopt modno.
        ade_check_selopt seqno.
        ade_check_selopt xhold.
        ade_check_selopt flist.
      ENDIF.
* ------------ process
      ade_process snap.
* ------------ process errors
    CATCH cx_sy_dynamic_osql_semantics.
      RAISE wrong_sql.
  ENDTRY.


ENDFUNCTION.
