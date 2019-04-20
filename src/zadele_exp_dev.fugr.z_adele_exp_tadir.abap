FUNCTION z_adele_exp_tadir.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MAX) TYPE  SYTABIX DEFAULT 1000
*"     VALUE(IS_SELOPT) TYPE  ZADELE_S_EXP_TADIR_SEL OPTIONAL
*"     VALUE(IV_DIM) TYPE  STRING OPTIONAL
*"     VALUE(IV_FCT) TYPE  STRING OPTIONAL
*"  TABLES
*"      ET_DATA STRUCTURE  ZADELE_S_EXP_TADIR_TF
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
* -------------
      IF lv_ade_lin GT 0.
        ade_check_selopt pgmid.
        ade_check_selopt object.
        ade_check_selopt obj_name.
        ade_check_selopt korrnum.
        ade_check_selopt srcsystem.
        ade_check_selopt author.
        ade_check_selopt srcdep.
        ade_check_selopt devclass.
        ade_check_selopt genflag.
        ade_check_selopt edtflag.
        ade_check_selopt cproject.
        ade_check_selopt masterlang.
        ade_check_selopt versid.
        ade_check_selopt paknocheck.
        ade_check_selopt objstablty.
        ade_check_selopt component.
        ade_check_selopt crelease.
        ade_check_selopt delflag.
        ade_check_selopt translttxt.
        ade_check_selopt created_on.
        ade_check_selopt check_date.
        ade_check_selopt check_cfg.
      ENDIF.

* ------------ process
      ade_process tadir.
* ------------ process errors
    CATCH cx_sy_dynamic_osql_semantics.
      RAISE wrong_sql.
  ENDTRY.


ENDFUNCTION.
