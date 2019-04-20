interface ZIF_ADELE_RFC_ACCESS
  public .


  methods GET_RFCDEST
    returning
      value(RV_RFCDEST) type RFCDEST .
  methods SET_RFCDEST
    importing
      !IV_RFCDEST type RFCDEST
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods RESET_ERROR
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods GET_ERROR_CODE
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods GET_ERROR_MESSAGE
    returning
      value(RV_ERROR) type STRING .
  methods SET_MAX_RECORDS
    importing
      !IV_MAX_RECORDS type I
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods RESET
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods ADD_DIMENSION
    importing
      !IV_DIM type DATA
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods ADD_FACT
    importing
      !IV_FACT type DATA
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods ADD_PARAM
    importing
      !IV_PARAM type DATA
      !IV_VALUE type DATA
      !IV_SIGN type TVARV_SIGN default 'I'
      !IV_OPTION type TVARV_OPTI default 'EQ'
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods ADD_PARAM_INTERVAL
    importing
      !IV_PARAM type DATA
      !IV_FROM type DATA
      !IV_TO type DATA
      !IV_SIGN type TVARV_SIGN default 'I'
    returning
      value(RR_SELF) type ref to ZIF_ADELE_RFC_ACCESS .
  methods EXECUTE
    importing
      !IV_FUNCTION type DATA
    changing
      !CT_DATA type TABLE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.
