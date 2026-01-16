REPORT ztool_help_coding.

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: sscrfields.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ts_display,
         ptype      TYPE string,
         progn      TYPE string,
         r3state    TYPE r3state,
         created_by TYPE unam,
         created_dt TYPE dats,
         changed_by TYPE unam,
         changed_dt TYPE dats,
         changed_tm TYPE tims,

         scolo      TYPE lvc_t_scol,
       END OF ts_display.

TYPES: BEGIN OF ts_funcgroup,
         prog TYPE reposrc-progname,
         area TYPE enlfdir-area,
         indx TYPE numc2,
       END OF ts_funcgroup,
       tt_funcgroup TYPE STANDARD TABLE OF ts_funcgroup WITH DEFAULT KEY.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gt_exclude TYPE TABLE OF sy-ucomm.

DATA: gt_display TYPE TABLE OF ts_display.

DATA: gr_docking TYPE REF TO cl_gui_docking_container.
DATA: gr_edit TYPE REF TO cl_gui_textedit.
DATA: gr_grid_alv TYPE REF TO cl_gui_alv_grid.

*&----------------------------------------------------------------------
*                     Selection Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  PARAMETERS: p_uname TYPE sy-uname DEFAULT sy-uname OBLIGATORY,
              p_cprog TYPE sy-cprog.

  SELECT-OPTIONS: s_datum FOR sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN FUNCTION KEY 1.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

  " --> 执行按钮
  APPEND 'ONLI' TO gt_exclude.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_exclude.
  " <--

  sscrfields-functxt_01 = VALUE smp_dyntxt(
    quickinfo = '修改记录'
    text      = '修改记录' ).

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  IF gr_docking IS INITIAL.
    gr_docking = NEW #( repid = sy-repid
                        dynnr = sy-dynnr
                        side  = cl_gui_docking_container=>dock_at_bottom
                        extension = 300 ).
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = 'P_CPROG'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF sy-ucomm = 'FC01'.
    PERFORM retrieve_change_detial.
  ENDIF.

*&----------------------------------------------------------------------
*      Form retrieve_change_detial
*&----------------------------------------------------------------------
FORM retrieve_change_detial.
  DATA: lt_fieldcat TYPE lvc_t_fcat.
  DATA: ls_funcgroup TYPE ts_funcgroup,
        lt_funcgroup TYPE tt_funcgroup.

  SELECT
    progname,
    r3state,
    appl,
    cnam,
    cdat,
    unam,
    udat,
    utime
    FROM reposrc
    WHERE ( cnam = @p_uname AND cdat IN @s_datum )
       OR ( unam = @p_uname AND udat IN @s_datum )
    INTO TABLE @DATA(lt_rep).

  DELETE lt_rep WHERE cnam = 'SAP*' OR unam = 'SAP*'.

  FREE gt_display.

  LOOP AT lt_rep ASSIGNING FIELD-SYMBOL(<ls_rep>).
    APPEND VALUE #( created_by = <ls_rep>-cnam
                    created_dt = <ls_rep>-cdat
                    changed_by = <ls_rep>-unam
                    changed_dt = <ls_rep>-udat
                    changed_tm = <ls_rep>-utime
                    r3state    = <ls_rep>-r3state
                    progn      = <ls_rep>-progname ) TO gt_display
                    ASSIGNING FIELD-SYMBOL(<ls_display>).

    IF <ls_rep>-appl = 'S'.

      IF <ls_display>-progn CS '='.
        " class
        <ls_display>-ptype = 'SE24'.
      ELSE.
        PERFORM get_func_from_prog USING <ls_display>-progn CHANGING ls_funcgroup.

        IF ls_funcgroup IS INITIAL.
          " include
          <ls_display>-ptype = 'SE38'.
        ELSE.
          " function
          <ls_display>-ptype = 'SE37'.
          APPEND ls_funcgroup TO lt_funcgroup.
        ENDIF.

        CLEAR ls_funcgroup.
      ENDIF.
    ELSE.
      <ls_display>-ptype = 'SE38'.
    ENDIF.

    PERFORM set_color_for_change_detial CHANGING <ls_display>.
  ENDLOOP.

  " funcion name
  IF lt_funcgroup IS NOT INITIAL.

    SELECT
      funcname,
      area,
      include
      FROM info_func
      FOR ALL ENTRIES IN @lt_funcgroup
      WHERE area = @lt_funcgroup-area
      INTO TABLE @DATA(lt_func).
    IF sy-subrc = 0.
      SORT lt_func BY area include.
    ENDIF.
  ENDIF.

  LOOP AT gt_display ASSIGNING <ls_display> WHERE ptype = 'SE37'.
    READ TABLE lt_funcgroup ASSIGNING FIELD-SYMBOL(<ls_funcgroup>) WITH KEY prog = <ls_display>-progn.
    IF sy-subrc = 0.
      READ TABLE lt_func ASSIGNING FIELD-SYMBOL(<ls_func>) WITH KEY area = <ls_funcgroup>-area.
      IF sy-subrc = 0.
        <ls_display>-progn = <ls_func>-funcname.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_display BY changed_dt changed_tm DESCENDING.

  lt_fieldcat = VALUE #( ( fieldname = 'PTYPE' key = abap_true coltext = 'Type' )
                         ( fieldname = 'PROGN' key = abap_true outputlen = 40 coltext = 'Program' )
                         ( fieldname = 'R3STATE' key = abap_true coltext = 'Status' )
                         ( fieldname = 'CREATED_BY' coltext = 'Created By' )
                         ( fieldname = 'CREATED_DT' coltext = 'Created Date' )
                         ( fieldname = 'CHANGED_BY' coltext = 'Changed By' )
                         ( fieldname = 'CHANGED_DT' coltext = 'Changed Date' )
                         ( fieldname = 'CHANGED_TM' coltext = 'Changed Time' ) ).

  IF gr_grid_alv IS INITIAL.
    gr_grid_alv = NEW #( i_parent = gr_docking ).

    gr_grid_alv->set_table_for_first_display(
      EXPORTING
        is_layout       = VALUE #( zebra = abap_true
                                   ctab_fname = 'SCOLO' )
      CHANGING
        it_outtab       = gt_display
        it_fieldcatalog = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error = 2
        too_many_lines = 3
        OTHERS = 4 ).

  ENDIF.

ENDFORM.
*&----------------------------------------------------------------------
*      Form get_func_from_prog
*&----------------------------------------------------------------------
FORM get_func_from_prog USING iv_progname
                      CHANGING cs_funcgroup TYPE ts_funcgroup.
  DATA: lv_typ_idx TYPE i.

  SPLIT iv_progname AT '/' INTO TABLE DATA(lt_parts).
  IF sy-subrc = 0.
    DATA(lv_max_Lines) = lines( lt_parts ).

    IF lv_max_Lines < 3.

      IF lv_max_lines = 1.

        IF iv_progname(1) = 'L'.
          lv_typ_idx = strlen( iv_progname ) - 3.

          cs_funcgroup-prog = iv_progname.
          cs_funcgroup-indx = iv_progname+lv_typ_idx.

          lv_typ_idx = lv_typ_idx - 1.
          cs_funcgroup-area = iv_progname+lv_typ_idx(1).
        ENDIF.
      ENDIF.
    ENDIF.

    " other type
    RETURN.
  ENDIF.

  DATA(lv_is_group) = lt_parts[ 3 ].
  IF lv_is_group(1) <> 'L'.
    " other type
    RETURN.
  ENDIF.

  " remove L
  lt_parts[ 3 ] = lv_is_group+1.

  DATA(lv_last_str) = lt_parts[ lv_max_Lines ].

  lv_typ_idx = strlen( lv_last_str ) - 3.

  IF lv_last_str+lv_typ_idx(1) = 'U'.
    lt_parts[ lv_max_Lines ] = lv_last_str(lv_typ_idx).

    lv_typ_idx = lv_typ_idx + 1.

    cs_funcgroup-prog = iv_progname.
    cs_funcgroup-indx = lv_last_str+lv_typ_idx.

    CONCATENATE LINES OF lt_parts INTO cs_funcgroup-area SEPARATED BY '/'.
  ELSE.
    " func but other include
    cs_funcgroup-indx = 99.
  ENDIF.
ENDFORM.
*&----------------------------------------------------------------------
*      Form set_color_for_change_detial
*&----------------------------------------------------------------------
FORM set_color_for_change_detial CHANGING cs_display TYPE ts_display.
  DEFINE _.
    IF cs_display-&1 = &2.
      APPEND VALUE #( fname = '&1' color-col = &3 color-inv = 1 ) TO cs_display-scolo.
    ENDIF.
  END-OF-DEFINITION.

  _ changed_by p_uname 5.
  _ created_by p_uname 5.
  _ changed_dt sy-datum 5.
  _ created_dt sy-datum 5.
ENDFORM.