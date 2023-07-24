*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /YKY/EXP_ORGV...................................*
TABLES: /YKY/EXP_ORGV, */YKY/EXP_ORGV. "view work areas
CONTROLS: TCTRL_/YKY/EXP_ORGV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/YKY/EXP_ORGV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/YKY/EXP_ORGV.
* Table for entries selected to show on screen
DATA: BEGIN OF /YKY/EXP_ORGV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /YKY/EXP_ORGV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /YKY/EXP_ORGV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /YKY/EXP_ORGV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /YKY/EXP_ORGV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /YKY/EXP_ORGV_TOTAL.

*.........table declarations:.................................*
TABLES: /YKY/EXP_ORG                   .
