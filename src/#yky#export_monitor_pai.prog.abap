*----------------------------------------------------------------------*
***INCLUDE /YKY/EXPORT_MONITOR_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  gv_okcode = okcode.
  CLEAR okcode.

  CASE gv_okcode.
    WHEN '&F15'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      okcode = gv_okcode.
  ENDCASE.


ENDMODULE.
MODULE user_command_0101 INPUT.
  gv_okcode = okcode.
  CLEAR okcode.

  CASE gv_okcode.
    WHEN 'ENTER'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      okcode = gv_okcode.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0100 INPUT.
  LEAVE PROGRAM.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TABSTRIP_FILTER'. DO NOT CHANGE THIS LI
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabstrip_filter_active_tab_get INPUT.
  okcode = sy-ucomm.
  CASE okcode.
    WHEN c_tabstrip_filter-tab1.
      g_tabstrip_filter-pressed_tab = c_tabstrip_filter-tab1.
    WHEN c_tabstrip_filter-tab2.
      g_tabstrip_filter-pressed_tab = c_tabstrip_filter-tab2.
    WHEN c_tabstrip_filter-tab3.
      g_tabstrip_filter-pressed_tab = c_tabstrip_filter-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0101 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
