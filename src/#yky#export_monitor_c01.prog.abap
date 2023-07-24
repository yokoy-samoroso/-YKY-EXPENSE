*&---------------------------------------------------------------------*
*& Include          /YKY/EXPORT_MONITOR_C01
*&---------------------------------------------------------------------*
CLASS lcl_alv_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_request_fiter_update FOR EVENT request_filter_update OF /yky/cl_export_monitor_alv,
      on_request_fiter_update_0101 FOR EVENT request_filter_update OF /yky/cl_attachment_monitor_alv,

      on_show_attachments FOR EVENT show_attachments OF /yky/cl_export_monitor_alv
        IMPORTING
          iv_export_uuid.

ENDCLASS.

CLASS lcl_alv_events IMPLEMENTATION.
  METHOD on_request_fiter_update.
    PERFORM set_filter_0100 USING abap_true.
  ENDMETHOD.
  METHOD on_request_fiter_update_0101.
    PERFORM set_filter_0101.
  ENDMETHOD.
  METHOD on_show_attachments.
    gv_export_uuid = iv_export_uuid.

    CALL SCREEN '0101' STARTING AT 5 5.
  ENDMETHOD.
ENDCLASS.
