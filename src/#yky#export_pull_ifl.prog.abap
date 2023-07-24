*&---------------------------------------------------------------------*
*& Report /YKY/EXPORT_PULL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /yky/export_pull_ifl.

DATA go_pull TYPE REF TO /yky/cl_expense_pull_ifl.

PARAMETERS: p_system TYPE /ifl/exp_dochead-expense_system OBLIGATORY.
PARAMETERS: p_pdate TYPE datum.
PARAMETERS: p_exp TYPE xfeld.
PARAMETERS: p_card TYPE xfeld.
PARAMETERS: p_travel TYPE xfeld.
PARAMETERS: p_step2 TYPE xfeld.
PARAMETERS: p_step3 TYPE xfeld.
PARAMETERS: p_step4 TYPE xfeld.

INITIALIZATION.

  p_pdate = sy-datum.

START-OF-SELECTION.

  TRY.

      CREATE OBJECT go_pull
        EXPORTING
          iv_expense_system = p_system
          iv_posting_date   = p_pdate.               " Expense: Expense system

      IF p_exp = abap_true.
        go_pull->start_exports( /yky/if_exp_constants=>export_type-expenses ).
      ENDIF.
      IF p_card = abap_true.
        go_pull->start_exports( /yky/if_exp_constants=>export_type-creditcard ).
      ENDIF.
      IF p_travel = abap_true.
        go_pull->start_exports( /yky/if_exp_constants=>export_type-travelexpense ).
      ENDIF.
      IF p_step2 = abap_true.
        go_pull->check_exports( ).
      ENDIF.
      IF p_step3 = abap_true.
        go_pull->fetch_expenses( ).
      ENDIF.
      IF p_step4 = abap_true.
        go_pull->fetch_attachments( ).
      ENDIF.

    CATCH /ifl/cm_exp_exception INTO DATA(lx_ifl).
      MESSAGE lx_ifl TYPE 'I' DISPLAY LIKE 'E'.
    CATCH /yky/cx_exp_exception INTO DATA(lx_yky).
      MESSAGE lx_yky TYPE 'I' DISPLAY LIKE 'E'.


  ENDTRY.
