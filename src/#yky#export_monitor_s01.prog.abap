*&---------------------------------------------------------------------*
*& Include          /YKY/EXPORT_MONITOR_S01
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 9000 AS SUBSCREEN.
  SELECT-OPTIONS: so_expsy FOR /yky/exports-expense_system,
                  so_expid FOR /yky/exports-export_id,
                  so_comid FOR /yky/exports-company_id.
SELECTION-SCREEN END OF SCREEN 9000.

SELECTION-SCREEN BEGIN OF SCREEN 9001 AS SUBSCREEN.
  SELECT-OPTIONS: so_statu FOR /yky/exports-status,
                  so_delet FOR /yky/exports-deleted.
SELECTION-SCREEN END OF SCREEN 9001.

SELECTION-SCREEN BEGIN OF SCREEN 9002 AS SUBSCREEN.
  SELECT-OPTIONS: so_creby FOR /yky/exports-created_by,
                  so_creda FOR sy-datum,
                  so_chgby FOR /yky/exports-changed_by,
                  so_chgda FOR sy-datum.
SELECTION-SCREEN END OF SCREEN 9002.

AT SELECTION-SCREEN ON so_expsy.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_expid.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_comid.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_statu.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_delet.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_creby.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_creda.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_chgby.
  PERFORM set_filter_0100 USING abap_false.

AT SELECTION-SCREEN ON so_chgda.
  PERFORM set_filter_0100 USING abap_false.
