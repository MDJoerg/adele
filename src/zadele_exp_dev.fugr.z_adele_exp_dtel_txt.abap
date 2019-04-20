FUNCTION z_adele_exp_dtel_txt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MAX) TYPE  SYTABIX DEFAULT 1000
*"     VALUE(IS_SELOPT) TYPE  ZADELE_S_EXP_DD04T_SEL OPTIONAL
*"     VALUE(IV_DIM) TYPE  STRING OPTIONAL
*"     VALUE(IV_FCT) TYPE  STRING OPTIONAL
*"  TABLES
*"      ET_DATA STRUCTURE  ZADELE_S_EXP_DD04T_TF
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
        ade_check_selopt rollname.
        ade_check_selopt as4local.
        ade_check_selopt as4vers.
        ade_check_selopt ddlanguage.
        ade_check_selopt ddtext.
        ade_check_selopt reptext.
        ade_check_selopt scrtext_s.
        ade_check_selopt scrtext_m.
        ade_check_selopt scrtext_l.
        ade_check_selopt domname.
        ade_check_selopt routputlen.
        ade_check_selopt memoryid.
        ade_check_selopt logflag.
        ade_check_selopt headlen.
        ade_check_selopt scrlen1.
        ade_check_selopt scrlen2.
        ade_check_selopt scrlen3.
        ade_check_selopt actflag.
        ade_check_selopt as4user.
        ade_check_selopt as4date.
        ade_check_selopt as4time.
        ade_check_selopt dtelmaster.
        ade_check_selopt shlpname.
        ade_check_selopt shlpfield.
        ade_check_selopt deffdname.
        ade_check_selopt datatype.
        ade_check_selopt leng.
        ade_check_selopt decimals.
        ade_check_selopt outputlen.
        ade_check_selopt lowercase.
        ade_check_selopt signflag.
        ade_check_selopt convexit.
        ade_check_selopt valexi.
        ade_check_selopt entitytab.
        ade_check_selopt refkind.
        ade_check_selopt reftype.
        ade_check_selopt proxytype.
        ade_check_selopt ltrflddis.
        ade_check_selopt bidictrlc.
        ade_check_selopt nohistory.
        ade_check_selopt korrnum.
        ade_check_selopt srcsystem.
        ade_check_selopt author.
        ade_check_selopt srcdep.
        ade_check_selopt devclass.
        ade_check_selopt masterlang.
        ade_check_selopt component.
        ade_check_selopt crelease.
      ENDIF.

* ------------ process
      ade_process zv_adele_dtel.
* ------------ process errors
    CATCH cx_sy_dynamic_osql_semantics.
      RAISE wrong_sql.
  ENDTRY.


ENDFUNCTION.
