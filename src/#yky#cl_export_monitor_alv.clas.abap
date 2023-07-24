CLASS /yky/cl_export_monitor_alv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_salv_ida_calc_field_handler .

    DATA mo_alv TYPE REF TO if_salv_gui_table_ida .

    EVENTS request_filter_update .
    EVENTS show_attachments
      EXPORTING
        VALUE(iv_export_uuid) TYPE sysuuid_x16 .

    METHODS constructor
      IMPORTING
        !io_gui_container TYPE REF TO cl_gui_container .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_key,
        exportuuid TYPE /yky/export_uuid,
      END OF ty_key .
    TYPES:
      ty_key_t TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_calc_fields,
        export_status_icon     TYPE icon_int,
        object_status_icon     TYPE icon_int,
        attachment_status_icon TYPE icon_int,
        appllog_button         TYPE lvc_value,
        createddate            TYPE dats,
        createdtime            TYPE tims,
        changeddate            TYPE dats,
        changedtime            TYPE tims,
      END OF ty_calc_fields .

    DATA mv_multi_select TYPE abap_bool .

    METHODS on_cell_action
      FOR EVENT cell_action OF if_salv_gui_field_display_opt
      IMPORTING
        !ev_field_name
        !eo_row_data .
    METHODS on_function_selected
      FOR EVENT function_selected OF if_salv_gui_toolbar_ida
      IMPORTING
        !ev_fcode .
    METHODS get_selected_keys
      RETURNING
        VALUE(rt_keys) TYPE ty_key_t .
    METHODS action_refresh .
    METHODS action_process
      IMPORTING
        !it_keys TYPE ty_key_t .
    METHODS action_delete
      IMPORTING
        !it_keys TYPE ty_key_t .
    METHODS action_show_attachments
      IMPORTING
        !it_keys TYPE ty_key_t .
ENDCLASS.



CLASS /YKY/CL_EXPORT_MONITOR_ALV IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <lv_selection_mode> TYPE if_salv_gui_selection_ida=>y_selection_mode.

    mo_alv = cl_salv_gui_table_ida=>create_for_cds_view( EXPORTING iv_cds_view_name      = '/YKY/EXPORT_ALV'
                                                                 io_gui_container      = io_gui_container
                                                                 io_calc_field_handler = me ).
    DATA(lo_alv_display_options) = mo_alv->display_options( ).
    lo_alv_display_options->set_title( EXPORTING iv_title = CONV #( 'Exports'(001) ) ).
    lo_alv_display_options->enable_alternating_row_pattern( ).
    lo_alv_display_options->set_empty_table_text( EXPORTING iv_empty_table_text = CONV #( 'No Data Found'(002) ) ).

    DATA(lo_selection) = mo_alv->selection( ).
    ASSIGN COMPONENT 'MULTI' OF STRUCTURE if_salv_gui_selection_ida=>cs_selection_mode TO <lv_selection_mode>.
    IF sy-subrc IS INITIAL.
      mv_multi_select = abap_true.
    ELSE.
      mv_multi_select = abap_false.
      ASSIGN if_salv_gui_selection_ida=>cs_selection_mode-single TO <lv_selection_mode>.
    ENDIF.
    lo_selection->set_selection_mode( EXPORTING iv_mode = <lv_selection_mode> ).

    DATA(lo_field_catalog) = mo_alv->field_catalog( ).
    lo_field_catalog->get_available_fields( IMPORTING ets_field_names = DATA(lt_fields) ).
    DELETE lt_fields WHERE table_line = 'EXPORTUUID'.
    lo_field_catalog->set_available_fields( EXPORTING its_field_names = lt_fields ).

    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'EXPORT_STATUS_ICON'
                                                    iv_header_text  = CONV #( 'Status'(003) )
                                                    iv_tooltip_text = CONV #( 'Status'(003) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'APPLLOG_BUTTON'
                                                        iv_header_text  = CONV #( 'ApplLog'(004) )
                                                        iv_tooltip_text = CONV #( 'Application Log'(005) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'CREATEDDATE'
                                                        iv_header_text  = CONV #( 'Create Date'(006) )
                                                        iv_tooltip_text = CONV #( 'Create Date'(006) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'CREATEDTIME'
                                                        iv_header_text  = CONV #( 'Create Time'(007) )
                                                        iv_tooltip_text = CONV #( 'Create Time'(007) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'CHANGEDDATE'
                                                        iv_header_text  = CONV #( 'Change Date'(008) )
                                                        iv_tooltip_text = CONV #( 'Change Date'(008) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'CHANGEDTIME'
                                                        iv_header_text  = CONV #( 'Change Time'(009) )
                                                        iv_tooltip_text = CONV #( 'Change Time'(009) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'EXPENSESYSTEM'
                                                    iv_header_text  = CONV #( 'Expense System'(017) )
                                                    iv_tooltip_text = CONV #( 'Expense System'(017) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'OBJECT_STATUS_ICON'
                                                iv_header_text  = CONV #( 'Stat: Obj'(018) )
                                                iv_tooltip_text = CONV #( 'Stat: Obj'(018) ) ).
    lo_field_catalog->set_field_header_texts( EXPORTING iv_field_name   = 'ATTACHMENT_STATUS_ICON'
                                            iv_header_text  = CONV #( 'Stat: Att'(019) )
                                            iv_tooltip_text = CONV #( 'Stat: Att'(019) ) ).

    DATA(lo_fc_display_options) = lo_field_catalog->display_options( ).
    lo_fc_display_options->display_as_button( EXPORTING iv_field_name               = 'APPLLOG_BUTTON'
                                                        iv_hide_if_value_is_initial = abap_true ).

    SET HANDLER me->on_cell_action FOR lo_fc_display_options.

    DATA(lo_layout) = mo_alv->default_layout( ).
    lo_layout->set_visible_fields( EXPORTING it_visible_fields = VALUE #( ( 'EXPORT_STATUS_ICON' )
                                                                          ( 'EXPENSESYSTEM' )
                                                                          ( 'EXPORTID' )
                                                                          ( 'COMPANYID' )
                                                                          ( 'OBJECT_STATUS_ICON' )
                                                                          ( 'ATTACHMENT_STATUS_ICON' )
                                                                          ( 'APPLLOG_BUTTON' )
                                                                          ( 'CREATEDBY' )
                                                                          ( 'CREATEDDATE' )
                                                                          ( 'CREATEDTIME' )
                                                                          ( 'CHANGEDBY' )
                                                                          ( 'CHANGEDDATE' )
                                                                          ( 'CHANGEDTIME' ) ) ).

    lo_layout->set_sort_order( EXPORTING it_sort_order = VALUE #( ( field_name = 'CREATEDAT'
                                                                descending = abap_true ) ) ).
    DATA(lo_toolbar) = mo_alv->toolbar( ).
    lo_toolbar->add_button( EXPORTING iv_fcode                     = 'PROCESS'
                                  iv_icon                      = icon_execute_object
                                  iv_quickinfo                 = CONV #( 'Process'(011) )
                                  iv_before_standard_functions = abap_true ).
    lo_toolbar->add_button( EXPORTING iv_fcode                     = 'DELETE'
                                  iv_icon                      = icon_delete
                                  iv_quickinfo                 = CONV #( 'Delete'(012) )
                                  iv_before_standard_functions = abap_true ).
    lo_toolbar->add_button( EXPORTING iv_fcode                     = 'SHOW_ATTACHMENTS'
                                      iv_icon                      = icon_reference_list
                                      iv_quickinfo                 = CONV #( 'Show Attachments'(020) )
                                      iv_before_standard_functions = abap_true ).
    lo_toolbar->add_button( EXPORTING iv_fcode                     = 'REFRESH'
                                  iv_icon                      = icon_refresh
                                  iv_quickinfo                 = CONV #( 'Refresh'(010) )
                                  iv_before_standard_functions = abap_true ).
    SET HANDLER me->on_function_selected FOR lo_toolbar.

  ENDMETHOD.


  METHOD if_salv_ida_calc_field_handler~calculate_line.

    DATA lt_status TYPE TABLE OF /yky/exports_att-status.

    FIELD-SYMBOLS: <ls_data>              TYPE /yky/export_alv,
                   <ls_data_old>          TYPE /yky/iexport_alv,
                   <ls_calculated_fields> TYPE ty_calc_fields.

    CLEAR es_calculated_fields.
    ASSIGN es_calculated_fields TO <ls_calculated_fields>.

* Rückwärtskompatibilität
    DATA(lo_structdescr) = cl_abap_structdescr=>describe_by_data( EXPORTING p_data = is_data_base_line ).
    DATA(lv_name) = lo_structdescr->get_relative_name( ).

    IF lv_name = '/YKY/EXPORT_ALV'.

      ASSIGN is_data_base_line TO <ls_data>.

      CONVERT TIME STAMP <ls_data>-createdat TIME ZONE sy-zonlo
        INTO DATE <ls_calculated_fields>-createddate TIME <ls_calculated_fields>-createdtime.
      IF sy-subrc IS NOT INITIAL.
        <ls_calculated_fields>-createddate = '99991231'.
        <ls_calculated_fields>-createdtime = '235959'.
      ENDIF.
      CONVERT TIME STAMP <ls_data>-changedat TIME ZONE sy-zonlo
        INTO DATE <ls_calculated_fields>-changeddate TIME <ls_calculated_fields>-changedtime.
      IF sy-subrc IS NOT INITIAL.
        <ls_calculated_fields>-changeddate = '99991231'.
        <ls_calculated_fields>-changedtime = '235959'.
      ENDIF.

      <ls_calculated_fields>-export_status_icon = SWITCH #( <ls_data>-status WHEN /yky/if_exp_constants=>doc_status-new THEN icon_workflow_activity
                                                                             WHEN /yky/if_exp_constants=>doc_status-error THEN icon_cancel
                                                                             WHEN /yky/if_exp_constants=>doc_status-successful THEN icon_okay
                                                                             WHEN /yky/if_exp_constants=>doc_status-ready THEN icon_wf_workitem_ready
                                                                                                                          ELSE icon_dummy ).



      <ls_calculated_fields>-object_status_icon = SWITCH #( <ls_data>-status  WHEN /yky/if_exp_constants=>doc_status-new THEN icon_workflow_activity
                                                                              WHEN /yky/if_exp_constants=>doc_status-error THEN icon_led_red
                                                                              WHEN /yky/if_exp_constants=>doc_status-successful THEN icon_led_green
                                                                              WHEN /yky/if_exp_constants=>doc_status-ready THEN icon_wf_workitem_ready ).

      SELECT status
        FROM /yky/exports_att
        INTO TABLE lt_status
        WHERE export_uuid = <ls_data>-exportuuid
        AND deleted EQ abap_false.

      IF sy-subrc EQ 0.

        READ TABLE lt_status WITH KEY table_line = /yky/if_exp_constants=>doc_status-error TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          <ls_calculated_fields>-attachment_status_icon = icon_led_red.
          <ls_calculated_fields>-export_status_icon = icon_cancel.
        ELSE.
          READ TABLE lt_status WITH KEY table_line = /yky/if_exp_constants=>doc_status-new TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            <ls_calculated_fields>-attachment_status_icon = icon_wf_workitem_ready.
            <ls_calculated_fields>-export_status_icon = icon_wf_workitem_ready.
          ELSE.
            <ls_calculated_fields>-attachment_status_icon = icon_led_green.
            <ls_calculated_fields>-export_status_icon = icon_okay.
          ENDIF.
        ENDIF.

      ENDIF.

      IF <ls_data>-deleted = abap_true.

        <ls_calculated_fields>-export_status_icon = icon_delete.

      ENDIF.

      <ls_calculated_fields>-appllog_button = icon_protocol.

    ELSE.

      ASSIGN is_data_base_line TO <ls_data_old>.

      CONVERT TIME STAMP <ls_data_old>-createdat TIME ZONE sy-zonlo
        INTO DATE <ls_calculated_fields>-createddate TIME <ls_calculated_fields>-createdtime.
      IF sy-subrc IS NOT INITIAL.
        <ls_calculated_fields>-createddate = '99991231'.
        <ls_calculated_fields>-createdtime = '235959'.
      ENDIF.
      CONVERT TIME STAMP <ls_data_old>-changedat TIME ZONE sy-zonlo
        INTO DATE <ls_calculated_fields>-changeddate TIME <ls_calculated_fields>-changedtime.
      IF sy-subrc IS NOT INITIAL.
        <ls_calculated_fields>-changeddate = '99991231'.
        <ls_calculated_fields>-changedtime = '235959'.
      ENDIF.

      <ls_calculated_fields>-export_status_icon = SWITCH #( <ls_data_old>-status WHEN /yky/if_exp_constants=>doc_status-new THEN icon_workflow_activity
                                                                             WHEN /yky/if_exp_constants=>doc_status-error THEN icon_cancel
                                                                             WHEN /yky/if_exp_constants=>doc_status-successful THEN icon_okay
                                                                             WHEN /yky/if_exp_constants=>doc_status-ready THEN icon_wf_workitem_ready
                                                                                                                          ELSE icon_dummy ).



      <ls_calculated_fields>-object_status_icon = SWITCH #( <ls_data_old>-status  WHEN /yky/if_exp_constants=>doc_status-new THEN icon_workflow_activity
                                                                              WHEN /yky/if_exp_constants=>doc_status-error THEN icon_led_red
                                                                              WHEN /yky/if_exp_constants=>doc_status-successful THEN icon_led_green
                                                                              WHEN /yky/if_exp_constants=>doc_status-ready THEN icon_wf_workitem_ready ).

      SELECT status
        FROM /yky/exports_att
        INTO TABLE lt_status
        WHERE export_uuid = <ls_data_old>-exportuuid
        AND deleted EQ abap_false.

      IF sy-subrc EQ 0.

        READ TABLE lt_status WITH KEY table_line = /yky/if_exp_constants=>doc_status-error TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          <ls_calculated_fields>-attachment_status_icon = icon_led_red.
          <ls_calculated_fields>-export_status_icon = icon_cancel.
        ELSE.
          READ TABLE lt_status WITH KEY table_line = /yky/if_exp_constants=>doc_status-new TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            <ls_calculated_fields>-attachment_status_icon = icon_wf_workitem_ready.
            <ls_calculated_fields>-export_status_icon = icon_wf_workitem_ready.
          ELSE.
            <ls_calculated_fields>-attachment_status_icon = icon_led_green.
            <ls_calculated_fields>-export_status_icon = icon_okay.
          ENDIF.
        ENDIF.

      ENDIF.

      IF <ls_data_old>-deleted = abap_true.

        <ls_calculated_fields>-export_status_icon = icon_delete.

      ENDIF.

      <ls_calculated_fields>-appllog_button = icon_protocol.

    ENDIF.
  ENDMETHOD.


  METHOD if_salv_ida_calc_field_handler~end_page.
  ENDMETHOD.


  METHOD if_salv_ida_calc_field_handler~get_calc_field_structure.
    ro_calc_field_structure = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'TY_CALC_FIELDS' ) ).
  ENDMETHOD.


  METHOD if_salv_ida_calc_field_handler~get_requested_fields.

    INSERT CONV #( 'DELETED' )     INTO TABLE rts_db_field_name.
    INSERT CONV #( 'STATUS' )     INTO TABLE rts_db_field_name.
    INSERT CONV #( 'CREATEDAT' )          INTO TABLE rts_db_field_name.
    INSERT CONV #( 'CHANGEDAT' )          INTO TABLE rts_db_field_name.
  ENDMETHOD.


  METHOD if_salv_ida_calc_field_handler~start_page.
  ENDMETHOD.


  METHOD action_delete.

    IF it_keys IS INITIAL.
      MESSAGE s013.
      RETURN.
    ENDIF.

    LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<fs_keys>).
      UPDATE /yky/exports SET deleted = abap_true WHERE export_uuid = <fs_keys>-exportuuid.
      UPDATE /yky/exports_att SET deleted = abap_true WHERE export_uuid = <fs_keys>-exportuuid.
    ENDLOOP.
    COMMIT WORK AND WAIT.
    me->action_refresh( ).
  ENDMETHOD.


  METHOD action_process.
    DATA lo_pull_ifl TYPE REF TO object.

    IF it_keys IS INITIAL.
      MESSAGE s013.
      RETURN.
    ENDIF.

    cl_abap_classdescr=>describe_by_name( EXPORTING  p_name = '/YKY/CL_EXPENSE_PULL_IFL'
                                      EXCEPTIONS type_not_found = 1
                                                 OTHERS         = 2 ).
    IF sy-subrc EQ 0.

      TRY.

          CREATE OBJECT lo_pull_ifl TYPE ('/YKY/CL_EXPENSE_PULL_IFL').
          LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<fs_keys>).
            CALL METHOD lo_pull_ifl->('CHECK_EXPORT_BY_UUID') EXPORTING iv_uuid = <fs_keys>-exportuuid.
            CALL METHOD lo_pull_ifl->('FETCH_EXPENSES_BY_UUID') EXPORTING iv_uuid = <fs_keys>-exportuuid.
            CALL METHOD lo_pull_ifl->('FETCH_ATTACHMENT_BY_EXP_UUID') EXPORTING iv_uuid = <fs_keys>-exportuuid.
          ENDLOOP.

        CATCH cx_root INTO DATA(cx_exp).
          MESSAGE cx_exp TYPE 'E' DISPLAY LIKE 'I'.

      ENDTRY.

    ENDIF.
    me->action_refresh( ).
  ENDMETHOD.


  METHOD action_refresh.
    RAISE EVENT request_filter_update.
  ENDMETHOD.


  METHOD get_selected_keys.

    TRY.
        DATA(lo_selection) = mo_alv->selection( ).

*     Rückwärtskompatibilität
        IF mv_multi_select = abap_true.
          CALL METHOD lo_selection->('GET_SELECTED_RANGE')
            IMPORTING
              et_selected_rows = rt_keys.
        ELSE.
          APPEND INITIAL LINE TO rt_keys ASSIGNING FIELD-SYMBOL(<ls_key>).
          lo_selection->get_selected_row( IMPORTING es_row = <ls_key> ).
        ENDIF.

      CATCH cx_root.
        CLEAR rt_keys.
    ENDTRY.

  ENDMETHOD.


  METHOD on_cell_action.

    DATA: lr_lev1_sort       TYPE REF TO bal_s_sort,
          lt_log_handle      TYPE bal_t_logh,
          lt_log_handle_load TYPE bal_t_logh,
          lt_log_header      TYPE balhdr_t,
          ls_row             TYPE /YKY/Export_ALV,
          ls_display_profile TYPE bal_s_prof.

    CASE ev_field_name.
      WHEN 'APPLLOG_BUTTON'.
        eo_row_data->get_row_data( EXPORTING iv_request_type      = if_salv_gui_selection_ida=>cs_request_type-key_and_requested_fields
                                             its_requested_fields = VALUE #( ( CONV #( 'EXPORTUUID' ) ) )
                                   IMPORTING es_row               = ls_row ).


        DATA(ls_log_filter) = VALUE bal_s_lfil( extnumber = VALUE #( ( sign = 'I' option = 'EQ' low = ls_row-exportuuid ) )
                                                object    = VALUE #( ( sign = 'I' option = 'EQ' low = /yky/if_exp_constants=>appllog-object ) )
                                                subobject = VALUE #( ( sign = 'I' option = 'EQ' low = /yky/if_exp_constants=>appllog-subobj_export ) ) ).
        CALL FUNCTION 'BAL_DB_SEARCH'
          EXPORTING
            i_s_log_filter     = ls_log_filter
          IMPORTING
            e_t_log_header     = lt_log_header
          EXCEPTIONS
            log_not_found      = 1
            no_filter_criteria = 2
            OTHERS             = 3.
        IF sy-subrc IS INITIAL.
          CLEAR lt_log_handle_load.
          CALL FUNCTION 'BAL_DB_LOAD'
            EXPORTING
              i_t_log_header     = lt_log_header
            IMPORTING
              e_t_log_handle     = lt_log_handle_load
            EXCEPTIONS
              no_logs_specified  = 1
              log_not_found      = 2
              log_already_loaded = 3
              OTHERS             = 4.
          IF sy-subrc IS INITIAL.
            INSERT LINES OF lt_log_handle_load INTO TABLE lt_log_handle.
          ENDIF.
        ENDIF.

        IF lt_log_handle IS INITIAL.
          RETURN.
        ENDIF.

        CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
          IMPORTING
            e_s_display_profile = ls_display_profile.
        CLEAR ls_display_profile-lev1_sort.
        APPEND VALUE #( ref_table = 'BAL_S_SHOW' ref_field = 'ALDATE' down = abap_true ) TO ls_display_profile-lev1_sort.
        APPEND VALUE #( ref_table = 'BAL_S_SHOW' ref_field = 'ALTIME' down = abap_true ) TO ls_display_profile-lev1_sort.
        APPEND VALUE #( ref_table = 'BAL_S_SHOW' ref_field = 'LOGNUMBER' down = abap_true ) TO ls_display_profile-lev1_sort.

        CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
          EXPORTING
            i_s_display_profile  = ls_display_profile
            i_t_log_handle       = lt_log_handle
          EXCEPTIONS
            profile_inconsistent = 1
            internal_error       = 2
            no_data_available    = 3
            no_authority         = 4
            OTHERS               = 5.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        LOOP AT lt_log_handle ASSIGNING FIELD-SYMBOL(<lv_log_handle>).
          CALL FUNCTION 'BAL_LOG_REFRESH'
            EXPORTING
              i_log_handle  = <lv_log_handle>
            EXCEPTIONS
              log_not_found = 1
              OTHERS        = 2.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDLOOP.

    ENDCASE.
  ENDMETHOD.


  METHOD on_function_selected.
    CASE ev_fcode.
      WHEN 'REFRESH'.
        me->action_refresh( ).
      WHEN 'PROCESS'.
        me->action_process( EXPORTING it_keys = me->get_selected_keys( ) ).
      WHEN 'DELETE'.
        me->action_delete( EXPORTING it_keys = me->get_selected_keys( ) ).
      WHEN 'SHOW_ATTACHMENTS'.
        me->action_show_attachments( EXPORTING it_keys = me->get_selected_keys( ) ).
    ENDCASE.
  ENDMETHOD.


  METHOD action_show_attachments.

    IF it_keys IS INITIAL.
      MESSAGE s013.
      RETURN.
    ENDIF.

    IF lines( it_keys ) <> 1.

      MESSAGE s014.
      RETURN.

    ENDIF.

    RAISE EVENT show_attachments
      EXPORTING
        iv_export_uuid = it_keys[ 1 ]-exportuuid.
  ENDMETHOD.
ENDCLASS.
