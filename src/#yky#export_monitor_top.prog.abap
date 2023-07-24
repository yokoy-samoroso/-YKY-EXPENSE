*----------------------------------------------------------------------*
***INCLUDE /YKY/EXPORT_MONITOR_TOP.
*----------------------------------------------------------------------*
TABLES: /yky/exports.

DATA okcode TYPE syucomm.

DATA: go_container_alv            TYPE REF TO cl_gui_custom_container,
      go_container_attachment_alv TYPE REF TO cl_gui_custom_container,
      go_alv_export               TYPE REF TO /yky/cl_export_monitor_alv,
      go_alv_attachment           TYPE REF TO /yky/cl_attachment_monitor_alv,
      gv_export_uuid              TYPE /yky/export_uuid,
      gv_okcode                   TYPE sy-ucomm.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABSTRIP_FILTER'
CONSTANTS: BEGIN OF c_tabstrip_filter,
             tab1 LIKE sy-ucomm VALUE 'TABSTRIP_FILTER_FC1',
             tab2 LIKE sy-ucomm VALUE 'TABSTRIP_FILTER_FC2',
             tab3 LIKE sy-ucomm VALUE 'TABSTRIP_FILTER_FC3',
           END OF c_tabstrip_filter.
*&SPWIZARD: DATA FOR TABSTRIP 'TABSTRIP_FILTER'
CONTROLS:  tabstrip_filter TYPE TABSTRIP.
DATA: BEGIN OF g_tabstrip_filter,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE '/YKY/EXPORT_MONITOR',
        pressed_tab LIKE sy-ucomm VALUE c_tabstrip_filter-tab1,
      END OF g_tabstrip_filter.
