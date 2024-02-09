class /YKY/CL_EXPENSE_PULL_IFL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_export,
        resource TYPE string,
      END OF t_export .
  types:
    BEGIN OF t_start_exports,
        export_scope          TYPE string,
        exporter_id           TYPE string,
        earliest_posting_date TYPE string,
      END OF t_start_exports .
  types:
    BEGIN OF t_expense,
        id                  TYPE string,
        generated_files     TYPE string_t,
        original_format     TYPE string,
        serialized_artefact TYPE string,
      END OF t_expense .
  types:
    BEGIN OF t_expense_response,
        resources TYPE STANDARD TABLE OF t_expense WITH EMPTY KEY,
      END OF t_expense_response .
  types:
    BEGIN OF t_export_status,
        ispending TYPE abap_bool,
      END OF t_export_status .
  types:
    BEGIN OF t_export_status_response,
        resource TYPE t_export_status,
      END OF t_export_status_response .
  types:
    BEGIN OF ty_oauth2_response,
        access_token TYPE string,
        expires_in   TYPE i,
        token_type   TYPE string,
      END OF ty_oauth2_response .
  types:
    tt_compid TYPE STANDARD TABLE OF /yky/exp_compid .

  methods CONSTRUCTOR
    importing
      !IV_EXPENSE_SYSTEM type /IFL/EXP_SYSTEM optional
      !IV_POSTING_DATE type DATUM optional
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods START_EXPORTS_ALL
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods CHECK_EXPORTS
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods FETCH_EXPENSES
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods FETCH_ATTACHMENTS
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods CHECK_EXPORT_BY_UUID
    importing
      !IV_UUID type SYSUUID_X16
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods FETCH_EXPENSES_BY_UUID
    importing
      !IV_UUID type SYSUUID_X16
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods FETCH_ATTACHMENT_BY_UUID
    importing
      !IV_UUID type SYSUUID_X16
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods FETCH_ATTACHMENT_BY_EXP_UUID
    importing
      !IV_UUID type SYSUUID_X16
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods START_EXPORTS
    importing
      !IV_EXPORT_TYPE type STRING
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  PROTECTED SECTION.
private section.

  data GO_OA2C_CLIENT type ref to IF_OAUTH2_CLIENT .
  data GS_SYS type /IFL/EXP_ORG .
  data GO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data GT_COMP type TT_COMPID .
  data GS_SYS_PULL type /YKY/EXP_ORG .
  data GV_LOG_EXPIRY type /IFL/EXP_LOG_EXPIRY .
  data GV_POSTING_DATE type DATUM .

  methods GET_CONFIG
    importing
      !IV_EXPENSE_SYSTEM type /IFL/EXP_SYSTEM
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods CREATE_CLIENT
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
  methods SET_TOKEN
    raising
      /IFL/CM_EXP_EXCEPTION
      /YKY/CX_EXP_EXCEPTION .
ENDCLASS.



CLASS /YKY/CL_EXPENSE_PULL_IFL IMPLEMENTATION.


  METHOD check_exports.

    DATA lt_exports TYPE TABLE OF /yky/exports-export_uuid.

    SELECT export_uuid
      FROM /yky/exports
      INTO TABLE lt_exports
      WHERE status = /yky/if_exp_constants=>doc_status-new
      AND expense_system = gs_sys-expense_system
      AND deleted NE abap_true.

    LOOP AT lt_exports ASSIGNING FIELD-SYMBOL(<fs_exports>).

      me->check_export_by_uuid( iv_uuid = <fs_exports> ).

    ENDLOOP.


  ENDMETHOD.


  METHOD constructor.

    IF iv_posting_date IS NOT INITIAL.
      gv_posting_date = iv_posting_date.
    ELSE.
      gv_posting_date = sy-datum.
    ENDIF.
    get_config( iv_expense_system ).

  ENDMETHOD.


  METHOD create_client.
    CLEAR: go_http_client, go_oa2c_client.

    WAIT UP TO 1 SECONDS.

    cl_http_client=>create_by_destination( EXPORTING  destination              = gs_sys_pull-http_destination_pull
         IMPORTING  client                   = go_http_client
         EXCEPTIONS argument_not_found       = 1
                    destination_not_found    = 2
                    destination_no_authority = 3
                    plugin_not_active        = 4
                    internal_error           = 5
                    OTHERS                   = 6 ).
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE /ifl/cm_exp_exception
        MESSAGE e036 WITH gs_sys-http_destination.
    ENDIF.

    IF gs_sys-oa2c_configuration IS NOT INITIAL.

      TRY.

          CALL METHOD cl_oauth2_client=>('CREATE')
            EXPORTING
              i_profile        = '/IFL/EXP_EXPENSE_API'
              i_configuration  = gs_sys-oa2c_configuration
            RECEIVING
              ro_oauth2_client = go_oa2c_client.

          cl_abap_classdescr=>describe_by_name( EXPORTING  p_name = 'CL_OA2C_CC_GRANT'
                                                EXCEPTIONS type_not_found = 1
                                                           OTHERS         = 2 ).
          IF sy-subrc EQ 0.

            TRY.
                DATA(lv_get_new_token) = abap_false.
                go_oa2c_client->set_token( EXPORTING io_http_client = go_http_client ).

              CATCH cx_oa2c_at_not_available
              cx_oa2c_at_expired.
                lv_get_new_token = abap_true.

            ENDTRY.

            IF lv_get_new_token = abap_true.

              TRY.
                  CALL METHOD go_oa2c_client->('EXECUTE_CC_FLOW').
                  go_oa2c_client->set_token( EXPORTING io_http_client = go_http_client ).

                CATCH cx_oa2c_secstore_adm.
                  CALL FUNCTION 'ENQUE_SLEEP'
                    EXPORTING
                      seconds = 2.

                  go_oa2c_client->set_token( EXPORTING io_http_client = go_http_client ).

              ENDTRY.

            ENDIF.


          ELSE.

            set_token( ).

          ENDIF.

        CATCH cx_oa2c INTO DATA(lx_oa2c).
          RAISE EXCEPTION TYPE /ifl/cm_exp_exception
            EXPORTING
              previous = lx_oa2c.

      ENDTRY.


    ENDIF.

    go_http_client->request->set_header_field( EXPORTING name  = 'X-Yk-Auth-Method'
                                                         value = 'yokoy' ).
    go_http_client->propertytype_logon_popup = if_http_client=>co_disabled.
  ENDMETHOD.


  METHOD fetch_attachments.

    DATA lt_attachments TYPE TABLE OF /yky/exports_att-attachment_uuid.

    SELECT a~attachment_uuid
      FROM /yky/exports_att AS a
      INNER JOIN /yky/exports AS e ON a~export_uuid = e~export_uuid
      INTO TABLE lt_attachments
      WHERE a~status = /yky/if_exp_constants=>doc_status-new
      AND e~expense_system = gs_sys-expense_system
      AND a~deleted NE abap_true
      AND e~deleted NE abap_true.

    LOOP AT lt_attachments ASSIGNING FIELD-SYMBOL(<fs_attachment>).

      me->fetch_attachment_by_uuid( iv_uuid = <fs_attachment> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD fetch_expenses.

    DATA lt_exports TYPE TABLE OF /yky/exports-export_uuid.

    SELECT export_uuid
      FROM /yky/exports
      INTO TABLE lt_exports
      WHERE status = /yky/if_exp_constants=>doc_status-ready
      AND expense_system = gs_sys-expense_system
      AND deleted NE abap_true.

    LOOP AT lt_exports ASSIGNING FIELD-SYMBOL(<fs_exports>).

      me->fetch_expenses_by_uuid( iv_uuid = <fs_exports> ).

    ENDLOOP.



  ENDMETHOD.


  METHOD set_token.

    DATA: ls_oauth2_response TYPE ty_oauth2_response.

    SELECT SINGLE * FROM /ifl/exp_oa2cc INTO @DATA(ls_oa2cc)
      WHERE oa2c_configuration = @gs_sys-oa2c_configuration.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE /ifl/cm_exp_exception
        MESSAGE e055 WITH gs_sys-oa2c_configuration.
    ENDIF.

    SELECT SINGLE * FROM /ifl/exp_oa2cct INTO @DATA(ls_oa2cct)
      WHERE oa2c_configuration = @ls_oa2cc-oa2c_configuration.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_oa2cct.
    ENDIF.

    IF ls_oa2cct-auth_token IS NOT INITIAL.
      GET TIME STAMP FIELD DATA(lv_now).
      DATA(lv_valid) = cl_abap_tstmp=>add( EXPORTING tstmp = lv_now
                                                              secs  = 180 ).
      DATA(lv_auth_token) = COND /ifl/exp_auth_token( WHEN ls_oa2cct-valid_until > lv_valid THEN ls_oa2cct-auth_token ).
    ENDIF.

    IF lv_auth_token IS INITIAL.

      cl_http_client=>create_by_destination( EXPORTING  destination              = ls_oa2cc-http_destination
                                             IMPORTING  client                   = DATA(lo_http_client)
                                             EXCEPTIONS argument_not_found       = 1
                                                        destination_not_found    = 2
                                                        destination_no_authority = 3
                                                        plugin_not_active        = 4
                                                        internal_error           = 5
                                                        OTHERS                   = 6 ).
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE /ifl/cm_exp_exception
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      lo_http_client->request->set_method( EXPORTING method = cl_http_request=>co_request_method_post ).
      lo_http_client->request->set_form_field( EXPORTING name  = 'grant_type'
                                                         value = 'client_credentials' ).
      lo_http_client->request->set_cdata( EXPORTING data = '{}' ).

      lo_http_client->send( EXCEPTIONS http_communication_failure = 1
                                       http_invalid_state         = 2
                                       http_processing_failed     = 3
                                       http_invalid_timeout       = 4
                                       OTHERS                     = 5 ).
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE /ifl/cm_exp_exception
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      lo_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                          http_invalid_state         = 2
                                          http_processing_failed     = 3
                                          OTHERS                     = 4 ).
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE /ifl/cm_exp_exception
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      lo_http_client->response->get_status( IMPORTING code = DATA(lv_status) ).
      IF lv_status <> '200'.
        RAISE EXCEPTION TYPE /ifl/cm_exp_exception
          MESSAGE e057 WITH lv_status.
      ENDIF.

      DATA(lv_response_data) = lo_http_client->response->get_data( ).
      /ifl/cl_exp_utils=>deserialize_json( EXPORTING iv_json = lv_response_data
                                           CHANGING  cs_data = ls_oauth2_response ).
      IF |{ ls_oauth2_response-token_type CASE = LOWER }| <> 'bearer'.
        RAISE EXCEPTION TYPE /ifl/cm_exp_exception
          MESSAGE e058 WITH ls_oauth2_response-token_type.
      ENDIF.

      lv_auth_token = ls_oauth2_response-access_token.
      GET TIME STAMP FIELD lv_now.

      ls_oa2cct = VALUE #( oa2c_configuration = ls_oa2cc-oa2c_configuration
                           auth_token         = lv_auth_token
                           valid_until        = cl_abap_tstmp=>add( EXPORTING tstmp = lv_now
                                                                              secs  = ls_oauth2_response-expires_in ) ).
      MODIFY /ifl/exp_oa2cct FROM ls_oa2cct.
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE /ifl/cm_exp_exception
          MESSAGE e082.
      ENDIF.
    ENDIF.

    IF lv_auth_token IS INITIAL.
      RAISE EXCEPTION TYPE /ifl/cm_exp_exception
        MESSAGE e056.
    ENDIF.

    go_http_client->request->set_header_field( EXPORTING name  = 'Authorization'
                                                         value = |Bearer { lv_auth_token }| ).
  ENDMETHOD.


  METHOD start_exports.

    DATA mo_client TYPE REF TO cl_rest_http_client.
    DATA ls_request TYPE t_start_exports.
    DATA ls_response TYPE t_export.
    DATA ls_export TYPE /yky/exports.

    LOOP AT gt_comp ASSIGNING FIELD-SYMBOL(<fs_comp>).

      DATA(lv_uri) = '/' && gs_sys-organization_id && '/legal-entities/' && <fs_comp>-company_id && '/expense-export-facilities/' && iv_export_type && '/export-tasks'.

      CONCATENATE gv_posting_date(4) gv_posting_date+4(2) gv_posting_Date+6(2)
      INTO ls_request-earliest_posting_date SEPARATED BY '-'.
      ls_request-export_scope = 'all'.
      ls_request-exporter_id = <fs_comp>-user_id.

      DO.

        create_client( ).

        cl_http_utility=>set_request_uri( EXPORTING request = go_http_client->request
                                                uri     = lv_uri ).

        mo_client = NEW #( io_http_client = go_http_client ).

        DATA(lo_entity) = mo_client->if_rest_client~create_request_entity( ).

        lo_entity->set_content_type( EXPORTING iv_media_type = if_rest_media_type=>gc_appl_json ).

        DATA(lo_json) = NEW /ui2/cl_json(
          compress         = abap_false                 " Skip empty elements
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case                 " Pretty Print property names
          ts_as_iso8601    =  abap_true                " Dump timestamps as string in ISO8601 format
        ).

        DATA(lv_json) = lo_json->serialize_int(
                          data       = ls_request              " Data to serialize
                                             ).

        lo_entity->set_string_data( iv_data = lv_json ).

        mo_client->if_rest_resource~post( io_entity = lo_entity ).

        DATA(lv_return_code) = mo_client->if_rest_client~get_status( ).
        DATA(lv_response) = mo_client->if_rest_client~get_response_entity( ).
        DATA(lv_binary) = lv_response->get_binary_data( ).

        IF lv_return_code = 429.
          CONTINUE.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      IF lv_return_code LT 200 OR lv_return_code GT 300.
        RAISE EXCEPTION TYPE /ifl/cm_exp_exception
          MESSAGE e038 WITH lv_return_code lv_response->get_string_data( ).
      ENDIF.

      /ui2/cl_json=>deserialize(
        EXPORTING
          jsonx            = lv_binary                 " JSON XString
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case                 " Pretty Print property names
        CHANGING
          data             = ls_response                " Data to serialize
      ).

      CHECK ls_response IS NOT INITIAL.
      SELECT COUNT(*) FROM /yky/exports WHERE export_id = ls_response-resource AND expense_system = gs_sys-expense_system. "Failsafe, Exports can't be pulled twice.
      CHECK sy-subrc NE 0.

      ls_export-expense_system = gs_sys-expense_system.
      ls_export-export_uuid = /ifl/cl_exp_utils=>generate_uuid( ).

      ls_export-export_id = ls_response-resource.
      ls_export-company_id = <fs_comp>-company_id.
      ls_export-status = /yky/if_exp_constants=>doc_status-new.
      ls_export-created_by = sy-uname.
      GET TIME STAMP FIELD ls_export-created_at.
      ls_export-changed_by = sy-uname.
      GET TIME STAMP FIELD ls_export-changed_at.
      MODIFY /yky/exports FROM ls_export.

      DATA(lo_log) = NEW /yky/cl_log(
        iv_object     = /yky/if_exp_constants=>appllog-object
        iv_subobject  = /yky/if_exp_constants=>appllog-subobj_export
        iv_uuid       = ls_export-export_uuid
        iv_aldate_del =  COND #( WHEN gv_log_expiry > 0 THEN sy-datum + gv_log_expiry )
      ).

      MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 000 WITH ls_export-export_id INTO sy-msgli .

      lo_log->add_message(
        is_sy               = sy
      ).

      lo_log->save( ).


      COMMIT WORK AND WAIT.

      lo_log->free( ).

    ENDLOOP.
  ENDMETHOD.


  METHOD check_export_by_uuid.

    DATA mo_client TYPE REF TO cl_rest_http_client.
    DATA ls_response TYPE t_export_status_response.
    DATA ls_export TYPE /yky/exports.

    SELECT SINGLE *
      FROM /yky/exports
      INTO ls_export
      WHERE export_uuid = iv_uuid.

    CHECK sy-subrc EQ 0.

    get_config( ls_export-expense_system ).

    DATA(lo_log) = NEW /yky/cl_log(
      iv_object     = /yky/if_exp_constants=>appllog-object
      iv_subobject  = /yky/if_exp_constants=>appllog-subobj_export
      iv_uuid       = ls_export-export_uuid
      iv_aldate_del =  COND #( WHEN gv_log_expiry > 0 THEN sy-datum + gv_log_expiry )
    ).

    MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 009 WITH ls_export-export_id INTO sy-msgli .

    lo_log->add_message(
      is_sy               = sy
    ).
    TRY.

        DATA(lv_uri) = '/' && gs_sys-organization_id && '/legal-entities/' && ls_export-company_id && '/export-tasks/' && ls_export-export_id.

        DO.
          create_client( ).
          cl_http_utility=>set_request_uri( EXPORTING request = go_http_client->request
                                                      uri     = lv_uri ).

          mo_client = NEW #( io_http_client = go_http_client ).

          mo_client->if_rest_resource~get( ).

          DATA(lv_return_code) = mo_client->if_rest_client~get_status( ).
          DATA(lv_response) = mo_client->if_rest_client~get_response_entity( ).
          DATA(lv_binary) = lv_response->get_binary_data( ).

          IF lv_return_code = 429.
            CONTINUE.
          ELSE.
            EXIT.
          ENDIF.

        ENDDO.

        IF lv_return_code NE 200.
          RAISE EXCEPTION TYPE /ifl/cm_exp_exception
            MESSAGE e038 WITH lv_return_code lv_response->get_string_data( ).
        ENDIF.

        /ui2/cl_json=>deserialize(
          EXPORTING
            jsonx            = lv_binary                 " JSON XString
            pretty_name      = /ui2/cl_json=>pretty_mode-camel_case                 " Pretty Print property names
          CHANGING
            data             = ls_response                " Data to serialize
        ).

        IF ls_response-resource-ispending = abap_false.

          ls_export-status = /yky/if_exp_constants=>doc_status-ready.
          ls_export-changed_by = sy-uname.
          GET TIME STAMP FIELD ls_export-changed_at.
          MODIFY /yky/exports FROM ls_export.

          MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 001 WITH ls_export-export_id INTO sy-msgli .

          lo_log->add_message(
            is_sy               = sy
          ).

          lo_log->save( ).

          COMMIT WORK AND WAIT.

        ELSE.

          MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 005 WITH ls_export-export_id INTO sy-msgli .

          lo_log->add_message(
            is_sy               = sy
          ).

          lo_log->save( ).

          COMMIT WORK AND WAIT.

        ENDIF.

      CATCH /ifl/cm_exp_exception INTO DATA(lx_exp).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'. "#EC CI_ROLLBACK
        sy-msgty = 'E'.
        sy-msgid = lx_exp->if_t100_message~t100key-msgid.
        sy-msgno = lx_exp->if_t100_message~t100key-msgno.
        sy-msgv1 = lx_exp->if_t100_dyn_msg~msgv1.
        sy-msgv2 = lx_exp->if_t100_dyn_msg~msgv2.
        sy-msgv3 = lx_exp->if_t100_dyn_msg~msgv3.
        sy-msgv4 = lx_exp->if_t100_dyn_msg~msgv4.

        lo_log->add_message(
          is_sy               = sy
        ).

        lo_log->save( ).
        COMMIT WORK.

      CATCH /yky/cx_exp_exception INTO DATA(lx_yky).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'. "EC CI_ROLLBACK
        sy-msgty = 'E'.
        sy-msgid = lx_yky->if_t100_message~t100key-msgid.
        sy-msgno = lx_yky->if_t100_message~t100key-msgno.
        sy-msgv1 = lx_yky->if_t100_dyn_msg~msgv1.
        sy-msgv2 = lx_yky->if_t100_dyn_msg~msgv2.
        sy-msgv3 = lx_yky->if_t100_dyn_msg~msgv3.
        sy-msgv4 = lx_yky->if_t100_dyn_msg~msgv4.

        lo_log->add_message(
          is_sy               = sy
        ).

        lo_log->save( ).
        COMMIT WORK.

    ENDTRY.

  ENDMETHOD.


  METHOD fetch_attachment_by_exp_uuid.

    DATA lt_attachments TYPE TABLE OF /yky/exports_att-attachment_uuid.

    SELECT attachment_uuid
      FROM /yky/exports_att
      INTO TABLE lt_attachments
      WHERE status NE /yky/if_exp_constants=>doc_status-successful
      AND export_uuid = iv_uuid
      AND deleted NE abap_true.

    LOOP AT lt_attachments ASSIGNING FIELD-SYMBOL(<fs_attachment>).

      me->fetch_attachment_by_uuid( iv_uuid = <fs_attachment> ).

    ENDLOOP.
  ENDMETHOD.


  METHOD fetch_attachment_by_uuid.

    DATA mo_client TYPE REF TO cl_rest_http_client.
    DATA ls_export TYPE /yky/exports.
    DATA ls_attachment TYPE /yky/exports_att.
    DATA lv_slug TYPE string.

    SELECT SINGLE *
      FROM /yky/exports_att
      INTO ls_attachment
      WHERE attachment_uuid = iv_uuid.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM /yky/exports
      INTO ls_export
      WHERE export_uuid = ls_attachment-export_uuid.

    CHECK sy-subrc EQ 0.

    get_config( ls_export-expense_system ).

    DATA(lo_export_log) = NEW /yky/cl_log(
    iv_object     = /yky/if_exp_constants=>appllog-object
    iv_subobject  = /yky/if_exp_constants=>appllog-subobj_export
    iv_uuid       = ls_export-export_uuid
    iv_aldate_del =  COND #( WHEN gv_log_expiry > 0 THEN sy-datum + gv_log_expiry )
    ).

    DATA(lo_attachment_log) = NEW /yky/cl_log(
    iv_object     = /yky/if_exp_constants=>appllog-object
    iv_subobject  = /yky/if_exp_constants=>appllog-subobj_attachment
    iv_uuid       = ls_attachment-attachment_uuid
    iv_aldate_del =  COND #( WHEN gv_log_expiry > 0 THEN sy-datum + gv_log_expiry )
    ).

    MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 011 WITH ls_attachment-filename ls_attachment-expense_id INTO sy-msgli .

    lo_attachment_log->add_message(
      is_sy               = sy
    ).

    lo_export_log->add_message(
      is_sy               = sy
    ).

    TRY.

        DATA(lo_expense_document) = /ifl/cl_exp_expdoc_factory=>get_instance_by_expense_id( EXPORTING iv_expense_system = gs_sys-expense_system
                                                                                                 iv_expense_id     = CONV #( ls_attachment-expense_id )
                                                                                                 iv_read_only      = abap_true ).

        IF lo_expense_document->get_status( )-status NE /ifl/if_exp_constants=>doc_status-successful.
          RAISE EXCEPTION TYPE /yky/cx_exp_exception
          MESSAGE ID '/YKY/EXPENSE' TYPE 'E' NUMBER 012 WITH ls_attachment-expense_id.
        ENDIF.

        DATA lv_string TYPE string.
        lv_string = ls_attachment-filename.

        cl_http_utility=>escape_url(
  EXPORTING
    unescaped = lv_string                 " Unencoded String
*              options   =                  " Reserve for Future Enhancements
  RECEIVING
    escaped   = lv_string                 " URL-Encoded String
).

*        DATA(lv_uri) = '/' && gs_sys-organization_id && '/legal-entities/' && ls_export-company_id && '/export-tasks/' && ls_export-export_id && '/artefacts/' && ls_attachment-artifact_id
*        && '/files?filename=' && ls_attachment-filename.

        DATA(lv_uri) = '/' && gs_sys-organization_id && '/legal-entities/' && ls_export-company_id && '/export-tasks/' && ls_export-export_id && '/artefacts/' && ls_attachment-artifact_id
        && '/files?filename=' && lv_string.

        DO.

          create_client( ).

          cl_http_utility=>set_request_uri( EXPORTING request = go_http_client->request
                                                      uri     = lv_uri ).
          mo_client = NEW #( io_http_client = go_http_client ).

          mo_client->if_rest_resource~get( ).

          DATA(lv_return_code) = mo_client->if_rest_client~get_status( ).
          DATA(lv_response) = mo_client->if_rest_client~get_response_entity( ).
          DATA(lv_binary) = lv_response->get_binary_data( ).
          DATA(lv_media_type) = lv_response->get_header_field( iv_name = 'content-type').
          DATA(lv_text) = lv_response->get_string_data( ).

          IF lv_return_code = 429.
            CONTINUE.
          ELSE.
            EXIT.
          ENDIF.

        ENDDO.

        IF lv_return_code NE 200.
          RAISE EXCEPTION TYPE /ifl/cm_exp_exception
            MESSAGE e038 WITH lv_return_code lv_response->get_string_data( ).
        ENDIF.

        lv_slug = ls_attachment-filename.
        lo_expense_document->add_attachment( EXPORTING iv_media_type = lv_media_type
                                                       iv_slug       = lv_slug
                                                       iv_doctype    = 'attachment'
                                                       iv_data       = lv_binary ).

        ls_attachment-status = /yky/if_exp_constants=>doc_status-successful.
        ls_attachment-changed_by = sy-uname.
        GET TIME STAMP FIELD ls_attachment-changed_at.
        MODIFY /yky/exports_att FROM ls_attachment.

        MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 006 WITH ls_attachment-filename ls_attachment-expense_id INTO sy-msgli.

        lo_export_log->add_message(
          is_sy               = sy
        ).

        lo_attachment_log->add_message(
          is_sy               = sy
        ).

        lo_export_log->save( ).
        lo_attachment_log->save( ).
        COMMIT WORK.

        lo_expense_document->free( ).

      CATCH /ifl/cm_exp_exception INTO DATA(lx_exp).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'. "EC CI_ROLLBACK
        sy-msgty = 'E'.
        sy-msgid = lx_exp->if_t100_message~t100key-msgid.
        sy-msgno = lx_exp->if_t100_message~t100key-msgno.
        sy-msgv1 = lx_exp->if_t100_dyn_msg~msgv1.
        sy-msgv2 = lx_exp->if_t100_dyn_msg~msgv2.
        sy-msgv3 = lx_exp->if_t100_dyn_msg~msgv3.
        sy-msgv4 = lx_exp->if_t100_dyn_msg~msgv4.

        lo_export_log->add_message(
          is_sy               = sy
        ).

        lo_attachment_log->add_message(
          is_sy               = sy
        ).

        lo_export_log->save( ).
        lo_attachment_log->save( ).
        COMMIT WORK.

      CATCH /yky/cx_exp_exception INTO DATA(lx_yky).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        sy-msgty = 'E'.
        sy-msgid = lx_yky->if_t100_message~t100key-msgid.
        sy-msgno = lx_yky->if_t100_message~t100key-msgno.
        sy-msgv1 = lx_yky->if_t100_dyn_msg~msgv1.
        sy-msgv2 = lx_yky->if_t100_dyn_msg~msgv2.
        sy-msgv3 = lx_yky->if_t100_dyn_msg~msgv3.
        sy-msgv4 = lx_yky->if_t100_dyn_msg~msgv4.

        lo_export_log->add_message(
          is_sy               = sy
        ).

        lo_attachment_log->add_message(
          is_sy               = sy
        ).

        lo_export_log->save( ).
        lo_attachment_log->save( ).
        COMMIT WORK.

    ENDTRY.
  ENDMETHOD.


  METHOD fetch_expenses_by_uuid.

    DATA mo_client TYPE REF TO cl_rest_http_client.
    DATA lt_response TYPE t_expense_response.
    DATA ls_export TYPE /yky/exports.
    DATA ls_attachment TYPE /yky/exports_att.

    SELECT SINGLE *
      FROM /yky/exports
      INTO ls_export
      WHERE export_uuid = iv_uuid.

    CHECK sy-subrc EQ 0.

    get_config( ls_export-expense_system ).

    DATA(lo_log) = NEW /yky/cl_log(
    iv_object     = /yky/if_exp_constants=>appllog-object
    iv_subobject  = /yky/if_exp_constants=>appllog-subobj_export
    iv_uuid       = ls_export-export_uuid
    iv_aldate_del =  COND #( WHEN gv_log_expiry > 0 THEN sy-datum + gv_log_expiry )
    ).

    MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 010 WITH ls_export-export_id INTO sy-msgli .

    lo_log->add_message(
      is_sy               = sy
    ).

    TRY.

        DATA(lv_uri) = '/' && gs_sys-organization_id && '/legal-entities/' && ls_export-company_id && '/export-tasks/' && ls_export-export_id && '/artefacts'.

        DO.
          create_client( ).

          cl_http_utility=>set_request_uri( EXPORTING request = go_http_client->request
                                                      uri     = lv_uri ).

          mo_client = NEW #( io_http_client = go_http_client ).

          mo_client->if_rest_resource~get( ).

          DATA(lv_return_code) = mo_client->if_rest_client~get_status( ).
          DATA(lv_response) = mo_client->if_rest_client~get_response_entity( ).
          DATA(lv_binary) = lv_response->get_binary_data( ).

          IF lv_return_code = 429.
            CONTINUE.
          ELSE.
            EXIT.
          ENDIF.

        ENDDO.

        IF lv_return_code NE 200.
          RAISE EXCEPTION TYPE /ifl/cm_exp_exception
            MESSAGE e038 WITH lv_return_code lv_response->get_string_data( ).
        ENDIF.

        /ui2/cl_json=>deserialize(
          EXPORTING
            jsonx            = lv_binary                 " JSON XString
            pretty_name      = /ui2/cl_json=>pretty_mode-camel_case                 " Pretty Print property names
          CHANGING
            data             = lt_response                " Data to serialize
        ).

        DATA(ls_expense_document) = VALUE /ifl/if_exp_rest_messages=>ts_expense_document( ).

        LOOP AT lt_response-resources ASSIGNING FIELD-SYMBOL(<fs_data>).

          /ui2/cl_json=>deserialize(
          EXPORTING
            json             = <fs_data>-serialized_artefact                " JSON XString
            pretty_name      = /ui2/cl_json=>pretty_mode-camel_case                 " Pretty Print property names
          CHANGING
            data             = ls_expense_document                " Data to serialize
        ).

          DATA(lo_expense_document) = /ifl/cl_exp_expdoc_factory=>get_instance_by_expense_id( EXPORTING iv_expense_system = gs_sys-expense_system
                                                                                                        iv_expense_id     = ls_expense_document-expense_id ).

          IF lo_expense_document->get_status( )-status NE /ifl/if_exp_constants=>doc_status-new.
            MESSAGE ID '/YKY/EXPENSE' TYPE 'E' NUMBER 007 WITH ls_expense_document-expense_id INTO sy-msgli .
            lo_log->add_message(
             is_sy               = sy
             ).
            CONTINUE.
          ENDIF.

          lo_expense_document->update_document_from_rest_data( EXPORTING is_expense_document = ls_expense_document ).

*     Spesenbeleg buchen, falls so konfiguriert
          IF lo_expense_document->is_post_immediately( ) = abap_true.
            lo_expense_document->post( ).
          ENDIF.

*     Speichern
          lo_expense_document->save( ).

          MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 002 WITH ls_expense_document-expense_id INTO sy-msgli .

          lo_log->add_message(
            is_sy               = sy
          ).

          LOOP AT <fs_data>-generated_files ASSIGNING FIELD-SYMBOL(<fs_filename>).

            ls_attachment-attachment_uuid = /ifl/cl_exp_utils=>generate_uuid( ).
            ls_attachment-export_uuid = ls_export-export_uuid.
            ls_attachment-artifact_id = <fs_data>-id.
            ls_attachment-expense_id = ls_expense_document-expense_id.
            ls_attachment-filename = <fs_filename>.
            ls_attachment-status = /yky/if_exp_constants=>doc_status-new.
            ls_attachment-created_by = sy-uname.
            GET TIME STAMP FIELD ls_attachment-created_at.
            ls_attachment-changed_by = sy-uname.
            GET TIME STAMP FIELD ls_attachment-changed_at.

            MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 003 WITH <fs_filename> INTO sy-msgli .

            lo_log->add_message(
              is_sy               = sy
            ).
            MODIFY /yky/exports_att FROM ls_attachment.

          ENDLOOP.

*     Commit
          COMMIT WORK.
          lo_expense_document->dequeue( ).
          lo_expense_document->free( ).

          CLEAR ls_expense_document.


        ENDLOOP.

        ls_export-status = /yky/if_exp_constants=>doc_status-successful.
        ls_export-changed_by = sy-uname.
        GET TIME STAMP FIELD ls_export-changed_at.
        MODIFY /yky/exports FROM ls_export.

        MESSAGE ID '/YKY/EXPENSE' TYPE 'I' NUMBER 004 INTO sy-msgli .

        lo_log->add_message(
          is_sy               = sy
        ).

        lo_log->save( ).
        COMMIT WORK.

      CATCH /ifl/cm_exp_exception INTO DATA(lx_exp).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        sy-msgty = 'E'.
        sy-msgid = lx_exp->if_t100_message~t100key-msgid.
        sy-msgno = lx_exp->if_t100_message~t100key-msgno.
        sy-msgv1 = lx_exp->if_t100_dyn_msg~msgv1.
        sy-msgv2 = lx_exp->if_t100_dyn_msg~msgv2.
        sy-msgv3 = lx_exp->if_t100_dyn_msg~msgv3.
        sy-msgv4 = lx_exp->if_t100_dyn_msg~msgv4.

        lo_log->add_message(
          is_sy               = sy
        ).

        lo_log->save( ).
        COMMIT WORK.

      CATCH /yky/cx_exp_exception INTO DATA(lx_yky).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        sy-msgty = 'E'.
        sy-msgid = lx_yky->if_t100_message~t100key-msgid.
        sy-msgno = lx_yky->if_t100_message~t100key-msgno.
        sy-msgv1 = lx_yky->if_t100_dyn_msg~msgv1.
        sy-msgv2 = lx_yky->if_t100_dyn_msg~msgv2.
        sy-msgv3 = lx_yky->if_t100_dyn_msg~msgv3.
        sy-msgv4 = lx_yky->if_t100_dyn_msg~msgv4.

        lo_log->add_message(
          is_sy               = sy
        ).

        lo_log->save( ).
        COMMIT WORK.

    ENDTRY.


  ENDMETHOD.


  METHOD get_config.

    IF gs_sys-expense_system NE iv_expense_system.

      SELECT SINGLE log_expiry FROM /ifl/exp_sys INTO gv_log_expiry
        WHERE expense_system = iv_expense_system.

      SELECT SINGLE * FROM /ifl/exp_org INTO @gs_sys
        WHERE /ifl/exp_org~expense_system = @iv_expense_system
        AND /ifl/exp_org~sysid          = @sy-sysid
        AND /ifl/exp_org~client         = @sy-mandt.

      SELECT * FROM /yky/exp_compid INTO TABLE gt_comp
        WHERE expense_system = iv_expense_system
        AND sysid = sy-sysid
        AND client = sy-mandt.

      SELECT SINGLE * FROM /yky/exp_org INTO gs_sys_pull
        WHERE expense_system = iv_expense_system
        AND sysid = sy-sysid
        AND client = sy-mandt.

    ENDIF.

  ENDMETHOD.


  METHOD start_exports_all.

    start_exports( /yky/if_exp_constants=>export_type-expenses ).
    start_exports( /yky/if_exp_constants=>export_type-creditcard ).
    start_exports( /yky/if_exp_constants=>export_type-travelexpense ).

  ENDMETHOD.
ENDCLASS.
