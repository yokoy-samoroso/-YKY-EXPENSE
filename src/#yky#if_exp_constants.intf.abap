INTERFACE /yky/if_exp_constants
  PUBLIC .

  CONSTANTS:
    BEGIN OF doc_status,
      new        TYPE /yky/export_status VALUE 'NEW',
      ready      TYPE /yky/export_status VALUE 'READY',
      error      TYPE /yky/export_status VALUE 'ERROR',
      successful TYPE /yky/export_status VALUE 'SUCCESSFUL',
    END OF doc_status,

    BEGIN OF appllog,
      object            TYPE balobj_d VALUE  '/YKY/EXPENSE',
      subobj_export     TYPE balsubobj VALUE '/YKY/EXPORT',
      subobj_attachment TYPE balsubobj VALUE '/YKY/EXPORT_ATT',
    END OF appllog,

    BEGIN OF export_type,
      expenses      TYPE string VALUE 'expense',
      creditcard    TYPE string VALUE 'card',
      travelexpense TYPE string VALUE 'travel',
    END OF export_type.
ENDINTERFACE.
