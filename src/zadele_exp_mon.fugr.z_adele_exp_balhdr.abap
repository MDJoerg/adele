FUNCTION z_adele_exp_balhdr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MAX) TYPE  SYTABIX DEFAULT 100000
*"     VALUE(IS_SELOPT) TYPE  ZADELE_S_EXP_BALHDR_SEL OPTIONAL
*"     VALUE(IV_DIM) TYPE  STRING OPTIONAL
*"     VALUE(IV_FCT) TYPE  STRING OPTIONAL
*"  TABLES
*"      ET_DATA STRUCTURE  ZADELE_S_EXP_BALHDR_TF
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
* ------------- process filters
      IF lv_ade_lin GT 0.
        ade_check_selopt lognumber.
        ade_check_selopt object.
        ade_check_selopt subobject.
        ade_check_selopt extnumber.
        ade_check_selopt aldate.
        ade_check_selopt altime.
        ade_check_selopt aluser.
        ade_check_selopt altcode.
        ade_check_selopt alprog.
        ade_check_selopt almode.
        ade_check_selopt altext.
        ade_check_selopt userexitp.
        ade_check_selopt userexitf.
        ade_check_selopt probclass.
        ade_check_selopt aldate_del.
        ade_check_selopt del_before.
        ade_check_selopt alstate.
        ade_check_selopt userexitt.
        ade_check_selopt alchdate.
        ade_check_selopt alchtime.
        ade_check_selopt alchuser.
        ade_check_selopt log_handle.
        ade_check_selopt tabname.
        ade_check_selopt msg_cnt_al.
        ade_check_selopt msg_cnt_a.
        ade_check_selopt msg_cnt_e.
        ade_check_selopt msg_cnt_w.
        ade_check_selopt msg_cnt_i.
        ade_check_selopt msg_cnt_s.
        ade_check_selopt last_msgnr.
        ade_check_selopt tim_stmp.
        ade_check_selopt db_version.
        ade_check_selopt msg_cnt_p1.
        ade_check_selopt msg_cnt_p2.
        ade_check_selopt msg_cnt_p3.
        ade_check_selopt msg_cnt_p4.
        ade_check_selopt client_cre.
      ENDIF.
* ------------ process
      ade_process balhdr.
* ------------ process errors
    CATCH cx_sy_dynamic_osql_semantics.
      RAISE wrong_sql.
  ENDTRY.


ENDFUNCTION.
