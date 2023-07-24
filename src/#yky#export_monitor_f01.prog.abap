*----------------------------------------------------------------------*
***INCLUDE /YKY/EXPORT_MONITOR_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form set_filter
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_filter_0100 USING lv_force TYPE abap_bool.
  DATA lv_update TYPE abap_bool.
  DATA: lr_tstmp TYPE RANGE OF tzntstmps.
  STATICS: lt_expsy LIKE TABLE OF so_expsy,
           lt_expid LIKE TABLE OF so_expid,
           lt_comid LIKE TABLE OF so_comid,
           lt_statu LIKE TABLE OF so_statu,
           lt_delet LIKE TABLE OF so_delet,
           lt_creby LIKE TABLE OF so_creby,
           lt_creda LIKE TABLE OF so_creda,
           lt_chgby LIKE TABLE OF so_chgby,
           lt_chgda LIKE TABLE OF so_chgda.

  DATA(lo_range_tab_collector) = NEW cl_salv_range_tab_collector( ).

  CHECK sy-ucomm IS INITIAL AND sy-dynnr CP '900*' OR sy-ucomm = 'ACPT' OR lv_force IS NOT INITIAL.

  IF lt_expsy NE so_expsy[]. lt_expsy = so_expsy[]. lv_update = abap_true. ENDIF.
  IF lt_expid NE so_expid[]. lt_expid = so_expid[]. lv_update = abap_true. ENDIF.
  IF lt_comid NE so_comid[]. lt_comid = so_comid[]. lv_update = abap_true. ENDIF.
  IF lt_statu NE so_statu[]. lt_statu = so_statu[]. lv_update = abap_true. ENDIF.
  IF lt_delet NE so_delet[]. lt_delet = so_delet[]. lv_update = abap_true. ENDIF.
  IF lt_creby NE so_creby[]. lt_creby = so_creby[]. lv_update = abap_true. ENDIF.
  IF lt_creda NE so_creda[]. lt_creda = so_creda[]. lv_update = abap_true. ENDIF.
  IF lt_chgby NE so_chgby[]. lt_chgby = so_chgby[]. lv_update = abap_true. ENDIF.
  IF lt_chgda NE so_chgda[]. lt_chgda = so_chgda[]. lv_update = abap_true. ENDIF.

  CHECK lv_update IS NOT INITIAL OR lv_force IS NOT INITIAL.

  lo_range_tab_collector->add_ranges_for_name(
    iv_name   = 'EXPENSESYSTEM'
    it_ranges =  lt_expsy
  ).

  lo_range_tab_collector->add_ranges_for_name(
    iv_name   = 'EXPORTID'
    it_ranges =  lt_expid
  ).

  lo_range_tab_collector->add_ranges_for_name(
    iv_name   = 'COMPANYID'
    it_ranges =  lt_comid
  ).

  lo_range_tab_collector->add_ranges_for_name(
    iv_name   = 'STATUS'
    it_ranges =  lt_statu
  ).

  lo_range_tab_collector->add_ranges_for_name(
    iv_name   = 'DELETED'
    it_ranges =  lt_delet
  ).

  lo_range_tab_collector->add_ranges_for_name(
    iv_name   = 'CREATEDBY'
    it_ranges =  lt_creby
  ).

  lo_range_tab_collector->add_ranges_for_name(
    iv_name   = 'CHANGEDBY'
    it_ranges =  lt_chgby
  ).

  CLEAR lr_tstmp[].
  LOOP AT lt_creda ASSIGNING FIELD-SYMBOL(<fs_dats>).
    APPEND INITIAL LINE TO lr_tstmp ASSIGNING FIELD-SYMBOL(<fs_tstmp>).
    <fs_tstmp>-sign = <fs_dats>-sign.
    <fs_tstmp>-option = 'BT'.
    IF <fs_dats>-low IS NOT INITIAL.
      CONVERT DATE <fs_dats>-low TIME '000000' INTO TIME STAMP <fs_tstmp>-low TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE <fs_dats>-high TIME '000000' INTO TIME STAMP <fs_tstmp>-low TIME ZONE sy-zonlo.
    ENDIF.
    IF <fs_dats>-high IS NOT INITIAL.
      CONVERT DATE <fs_dats>-high TIME '235959' INTO TIME STAMP <fs_tstmp>-high TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE <fs_dats>-low TIME '235959' INTO TIME STAMP <fs_tstmp>-high TIME ZONE sy-zonlo.
    ENDIF.
  ENDLOOP.

  lo_range_tab_collector->add_ranges_for_name( EXPORTING iv_name   = 'CREATEDAT'
                                                       it_ranges = lr_tstmp ).
  CLEAR lr_tstmp[].
  LOOP AT lt_creda ASSIGNING <fs_dats>.
    APPEND INITIAL LINE TO lr_tstmp ASSIGNING <fs_tstmp>.
    <fs_tstmp>-sign = <fs_dats>-sign.
    <fs_tstmp>-option = 'BT'.
    IF <fs_dats>-low IS NOT INITIAL.
      CONVERT DATE <fs_dats>-low TIME '000000' INTO TIME STAMP <fs_tstmp>-low TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE <fs_dats>-high TIME '000000' INTO TIME STAMP <fs_tstmp>-low TIME ZONE sy-zonlo.
    ENDIF.
    IF <fs_dats>-high IS NOT INITIAL.
      CONVERT DATE <fs_dats>-high TIME '235959' INTO TIME STAMP <fs_tstmp>-high TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE <fs_dats>-low TIME '235959' INTO TIME STAMP <fs_tstmp>-high TIME ZONE sy-zonlo.
    ENDIF.
  ENDLOOP.

  lo_range_tab_collector->add_ranges_for_name( EXPORTING iv_name   = 'CHANGEDAT'
                                                     it_ranges = lr_tstmp ).

  lo_range_tab_collector->get_collected_ranges(
    IMPORTING
      et_named_ranges = DATA(lt_ranges)
  ).

  go_alv_export->mo_alv->set_select_options(
    it_ranges    = lt_ranges
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form init_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_0100.

  APPEND INITIAL LINE TO so_delet ASSIGNING FIELD-SYMBOL(<fs_delet>).
  <fs_delet>-sign = 'I'.
  <fs_delet>-option = 'EQ'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_filter_0101
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_filter_0101 .
  DATA lt_expid TYPE RANGE OF /yky/exports_att-export_uuid.
  DATA lt_delet TYPE RANGE OF /yky/exports_att-deleted.
  DATA(lo_range_tab_collector) = NEW cl_salv_range_tab_collector( ).

  APPEND INITIAL LINE TO lt_expid ASSIGNING FIELD-SYMBOL(<fs_expid>).
  <fs_expid>-low = gv_export_uuid.
  <fs_expid>-option = 'EQ'.
  <fs_expid>-sign = 'I'.

  APPEND INITIAL LINE TO lt_delet ASSIGNING FIELD-SYMBOL(<fs_delet>).
  <fs_delet>-low = abap_false.
  <fs_delet>-option = 'EQ'.
  <fs_delet>-sign = 'I'.

  lo_range_tab_collector->add_ranges_for_name(
  iv_name   = 'EXPORTUUID'
  it_ranges =  lt_expid
  ).

  lo_range_tab_collector->add_ranges_for_name(
  iv_name   = 'DELETED'
  it_ranges =  lt_delet
  ).

  lo_range_tab_collector->get_collected_ranges(
  IMPORTING
    et_named_ranges = DATA(lt_ranges)
  ).

  go_alv_attachment->mo_alv->set_select_options(
  it_ranges    = lt_ranges
  ).

  PERFORM set_filter_0100 USING abap_true.

ENDFORM.
