*&---------------------------------------------------------------------*
*& Include /YKY/EXPORT_MONITOR_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.

MODULE alv_0100 OUTPUT.
  IF go_alv_export IS NOT BOUND.
    go_alv_export = NEW /yky/cl_export_monitor_alv( io_gui_container = NEW cl_gui_custom_container(
      container_name              =  'CONTAINER_ALV'
      repid                       = sy-repid
      dynnr                       = '0100'                 )
    ).

    SET HANDLER lcl_alv_events=>on_request_fiter_update FOR go_alv_export.
    SET HANDLER lcl_alv_events=>on_show_attachments FOR go_alv_export.

    PERFORM init_0100.
    PERFORM set_filter_0100 USING abap_true.
  ENDIF.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABSTRIP_FILTER'. DO NOT CHANGE THIS L
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabstrip_filter_active_tab_set OUTPUT.
  tabstrip_filter-activetab = g_tabstrip_filter-pressed_tab.
  CASE g_tabstrip_filter-pressed_tab.
    WHEN c_tabstrip_filter-tab1.
      g_tabstrip_filter-subscreen = '9000'.
    WHEN c_tabstrip_filter-tab2.
      g_tabstrip_filter-subscreen = '9001'.
    WHEN c_tabstrip_filter-tab3.
      g_tabstrip_filter-subscreen = '9002'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'STATUS_0101'.
  SET TITLEBAR 'TITLE_0101'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ALV_0101 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE alv_0101 OUTPUT.
  IF go_alv_attachment IS NOT BOUND.

    go_alv_attachment = NEW /yky/cl_attachment_monitor_alv( io_gui_container = NEW cl_gui_custom_container(
    container_name              =  'CONTAINER_ATTACHMENT_ALV'
    repid                       = sy-repid
    dynnr                       = '0101'                 )
    ).

    SET HANDLER lcl_alv_events=>on_request_fiter_update_0101 FOR go_alv_attachment.

  ENDIF.
  PERFORM set_filter_0101.
ENDMODULE.
