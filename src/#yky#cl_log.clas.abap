CLASS /yky/cl_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_object     TYPE balobj_d
        !iv_subobject  TYPE balsubobj
        !iv_uuid       TYPE sysuuid_x16
        !iv_aldate_del TYPE aldate_del
      RAISING
        /yky/cx_exp_exception.
    METHODS add_message
      IMPORTING
        !is_sy               TYPE syst OPTIONAL
        !is_message          TYPE bal_s_msg OPTIONAL
        !iv_probclass        TYPE balprobcl OPTIONAL
        !iv_start_processing TYPE abap_bool OPTIONAL
      RAISING
        /yky/cx_exp_exception.
    METHODS add_free_text
      IMPORTING
        !iv_msgty     TYPE symsgty
        !iv_text      TYPE text255
        !iv_probclass TYPE balprobcl OPTIONAL
      RAISING
        /yky/cx_exp_exception.
    METHODS add_bapiret2
      IMPORTING
        !is_return    TYPE bapiret2
        !iv_probclass TYPE balprobcl OPTIONAL
      RAISING
        /yky/cx_exp_exception.
    METHODS save
      RAISING
        /yky/cx_exp_exception.
    METHODS free
      RAISING
        /yky/cx_exp_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_log_handle TYPE balloghndl .
ENDCLASS.



CLASS /YKY/CL_LOG IMPLEMENTATION.


  METHOD add_bapiret2.
    DATA(ls_msg) = VALUE bal_s_msg( msgty     = is_return-type
                                    msgid     = is_return-id
                                    msgno     = is_return-number
                                    msgv1     = is_return-message_v1
                                    msgv2     = is_return-message_v2
                                    msgv3     = is_return-message_v3
                                    msgv4     = is_return-message_v4 ).
    ls_msg-probclass = iv_probclass.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD add_free_text.
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = iv_msgty
        i_text           = iv_text
        i_probclass      = iv_probclass
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE /yky/cx_exp_exception
        MESSAGE e008.
    ENDIF.
  ENDMETHOD.


  METHOD add_message.
    IF is_sy IS SUPPLIED.
      DATA(ls_msg) = CORRESPONDING bal_s_msg( is_sy ).
    ELSEIF is_message IS SUPPLIED.
      ls_msg = is_message.
    ELSE.
      RETURN.
    ENDIF.

    ls_msg-probclass = iv_probclass.
    ls_msg-alsort   = COND #( WHEN iv_start_processing = abap_true THEN 'STA'  ELSE '' ).
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE /yky/cx_exp_exception
        MESSAGE e008.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    DATA(ls_log) = VALUE bal_s_log( extnumber  = iv_uuid
                                    object     = iv_object
                                    subobject  = iv_subobject
                                    aldate_del = iv_aldate_del ).

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE /yky/cx_exp_exception
        MESSAGE e008.
    ENDIF.
  ENDMETHOD.


  METHOD free.
    IF mv_log_handle IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_REFRESH'
        EXPORTING
          i_log_handle  = mv_log_handle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE /yky/cx_exp_exception
          MESSAGE e008.
      ENDIF.

      CLEAR mv_log_handle.
    ENDIF.
  ENDMETHOD.


  METHOD save.
    DATA(lt_log_handle) = VALUE bal_t_logh( ( mv_log_handle ) ).

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = lt_log_handle
      EXCEPTIONS
        msg_not_found  = 1
        OTHERS         = 2.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_t_log_handle   = lt_log_handle
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE /yky/cx_exp_exception
          MESSAGE e008.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
