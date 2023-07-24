*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /YKY/EXP_COMPID.................................*
DATA:  BEGIN OF STATUS_/YKY/EXP_COMPID               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/YKY/EXP_COMPID               .
CONTROLS: TCTRL_/YKY/EXP_COMPID
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */YKY/EXP_COMPID               .
TABLES: /YKY/EXP_COMPID                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
