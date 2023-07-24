*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /YKY/EXP_COMPIDV................................*
TABLES: /YKY/EXP_COMPIDV, */YKY/EXP_COMPIDV. "view work areas
CONTROLS: TCTRL_/YKY/EXP_COMPIDV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/YKY/EXP_COMPIDV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/YKY/EXP_COMPIDV.
* Table for entries selected to show on screen
DATA: BEGIN OF /YKY/EXP_COMPIDV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /YKY/EXP_COMPIDV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /YKY/EXP_COMPIDV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /YKY/EXP_COMPIDV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /YKY/EXP_COMPIDV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /YKY/EXP_COMPIDV_TOTAL.

*.........table declarations:.................................*
TABLES: /YKY/EXP_COMPID                .
