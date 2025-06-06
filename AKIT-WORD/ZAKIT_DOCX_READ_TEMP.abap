REPORT zakit_docx_read_temp.

" 暂时只支持简单结构

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: tt_code TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

*&----------------------------------------------------------------------
*                     Class
*&----------------------------------------------------------------------
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: gr_abap_edit TYPE REF TO cl_gui_abapedit,
      gr_popup     TYPE REF TO cl_gui_dialogbox_container,
      gr_event     TYPE REF TO lcl_event_receiver.

DATA: g_processed_gid TYPE TABLE OF i,
      g_abap_table    TYPE tt_code,
      g_abap_code     TYPE tt_code,
      g_nested_defs   TYPE tt_code. " 存储嵌套结构定义

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_close
        FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.

    DATA: dialogbox_status TYPE c.  "'X': does exist, SPACE: does not ex.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_close.
* §6.Handle the CLOSE-button of the dialogbox

* set dialogbox invisible
* (the dialogbox is destroyed outomatically when the user
* switches to another dynpro).
    CALL METHOD sender->set_visible
      EXPORTING
        visible = space.
* In this example closing the dialogbox leads
* to make it invisible. It is also conceivable to destroy it
* and recreate it if the user doubleclicks a line again.
* Displaying a great amount of data has a greater impact on performance.
  ENDMETHOD.
ENDCLASS.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  PARAMETERS: p_f_locl RADIOBUTTON GROUP gp1 DEFAULT 'X' USER-COMMAND gp1,
              p_f_smw0 RADIOBUTTON GROUP gp1.
SELECTION-SCREEN END OF BLOCK blck1.
SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  PARAMETERS: p_path TYPE rlgrap-filename MODIF ID pth.
  PARAMETERS: p_smw0 TYPE wwwdatatab-objid MODIF ID smw.
SELECTION-SCREEN END OF BLOCK blck2.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_f_locl = 'X' AND screen-group1 = 'SMW'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF p_f_locl = '' AND screen-group1 = 'PTH'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
      mask      = 'Upload Word|*.docx'(002)
    CHANGING
      file_name = p_path.

*&----------------------------------------------------------------------
*                     Start-of-selection
*&----------------------------------------------------------------------
START-OF-SELECTION.
  CLEAR: g_processed_gid, g_abap_table, g_abap_code, g_nested_defs.
  PERFORM frm_read_temp.

  WRITE ''.

*&---------------------------------------------------------------------*
*& Form FRM_READ_TEMP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM frm_read_temp .

  DATA(l_expo) = NEW zcl_akit_docx_tool( ).

  IF p_f_locl = 'X'.
    l_expo->load_file_local( |{ p_path }| ).
  ELSE.
    l_expo->load_file_smw0( |{ p_smw0 }| ).
  ENDIF.

  DATA: l_document TYPE xstring.

  APPEND `TYPES: BEGIN OF TY_DATA,` TO g_abap_code.

  LOOP AT l_expo->zip->files INTO DATA(l_file).
    CASE l_file-name(9).
      WHEN `word/docu` OR `word/foot` OR `word/head`.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    l_expo->zip->get(
      EXPORTING
        name    = l_file-name
      IMPORTING
        content = l_document ).

    l_expo->wd2o( EXPORTING xstr = l_document
                  IMPORTING oxml = DATA(l_xml)
                            odoc = DATA(l_doc) ).

    DATA(l_root) = l_doc->get_root( ).

    DATA(l_filter) = l_root->create_filter_name_ns( name      = `sdt`
                                                    namespace = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

    DATA(l_iterator) = l_root->create_iterator_filtered( filter = l_filter depth = 0 ).

    DO.
      DATA(l_item) = l_iterator->get_next( ).

      IF l_item IS NOT BOUND.
        EXIT.
      ENDIF.

      DATA(l_gid) = l_item->get_gid( ).

      READ TABLE g_processed_gid TRANSPORTING NO FIELDS WITH KEY table_line = l_gid.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      APPEND l_gid TO g_processed_gid.

      PERFORM frm_read_node USING l_item g_abap_code.
    ENDDO.

  ENDLOOP.

  APPEND ` END OF TY_DATA.` TO g_abap_code.

  " 先输出嵌套结构定义
  APPEND LINES OF g_nested_defs TO g_abap_table.
  " 再输出根结构定义
  APPEND LINES OF g_abap_code TO g_abap_table.

  CALL FUNCTION 'PRETTY_PRINTER'
    EXPORTING
      inctoo             = abap_false  " X = Process Include Programs as Well
    TABLES
      ntext              = g_abap_table   " Table of Formatted Source Code
      otext              = g_abap_table   " Table of Source Code Pending Editing
    EXCEPTIONS
      enqueue_table_full = 0
      include_enqueued   = 0
      include_readerror  = 0
      include_writeerror = 0
      OTHERS             = 0.

  gr_abap_edit = NEW #( parent = cl_gui_container=>screen0  ).

  gr_abap_edit->draw( ).
  gr_abap_edit->set_text( g_abap_table ).
  gr_abap_edit->undraw( ).
ENDFORM.

FORM frm_read_node USING node TYPE REF TO if_ixml_node code TYPE tt_code.

  DATA(l_element) = CAST if_ixml_element( node ).

  " 检查类型
  DATA(l_rs) = l_element->find_from_name_ns( depth = 2 name = `repeatingSection` uri = `http://schemas.microsoft.com/office/word/2012/wordml` ).

  IF l_rs IS NOT INITIAL.
    " 重复替换 - 处理嵌套结构
    DATA: l_nested_code TYPE tt_code.   " 存储当前嵌套结构的字段

    DATA(l_filter) = node->create_filter_name_ns( name      = `sdt`
                                                  namespace = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

    DATA(l_iterator) = node->create_iterator_filtered( filter = l_filter depth = 0 ).

    DATA(l_ttag) = l_element->find_from_name_ns( depth = 2 name = `tag` uri = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

    CHECK l_ttag IS BOUND.

    DATA(l_ttag_name) = l_ttag->get_attribute_ns( name = `val` uri = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

    " 遍历并递归处理子节点
    DO.
      DATA(l_item) = l_iterator->get_next( ).

      IF l_item IS NOT BOUND.
        EXIT.
      ENDIF.

      DATA(l_gid) = l_item->get_gid( ).

      READ TABLE g_processed_gid TRANSPORTING NO FIELDS WITH KEY table_line = l_gid.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      APPEND l_gid TO g_processed_gid.

      PERFORM frm_read_node USING l_item l_nested_code.
    ENDDO.

    " 生成嵌套结构定义并添加到全局表
    APPEND |TYPES: BEGIN OF ty_{ l_ttag_name },| TO g_nested_defs.
    APPEND LINES OF l_nested_code TO g_nested_defs.
    APPEND | END OF ty_{ l_ttag_name },| TO g_nested_defs.
    APPEND | tt_{ l_ttag_name } TYPE STANDARD TABLE OF ty_{ l_ttag_name } WITH DEFAULT KEY.| TO g_nested_defs.

    " 在父结构中添加嵌套字段引用
    APPEND | { l_ttag_name } TYPE tt_{ l_ttag_name },| TO code.

  ELSE.

    " 判断是否还有 sdt
    DATA(l_sub_sdt) = l_element->find_from_name_ns( depth = 0 name = `sdt` uri = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

    IF l_sub_sdt IS BOUND.

      DATA(l_sub_filter) = node->create_filter_name_ns( name      = `sdt`
                                                        namespace = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

      DATA(l_sub_iterator) = node->create_iterator_filtered( filter = l_sub_filter depth = 0 ).

      DATA(l_sub_ttag) = l_element->find_from_name_ns( depth = 2 name = `tag` uri = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

      CHECK l_sub_ttag IS BOUND.

      DATA(l_sub_ttag_name) = l_sub_ttag->get_attribute_ns( name = `val` uri = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).
      APPEND | BEGIN OF { l_sub_ttag_name },| TO code.


      " 遍历并递归处理子节点
      DO.
        DATA(l_sub_item) = l_sub_iterator->get_next( ).

        IF l_sub_item IS NOT BOUND.
          EXIT.
        ENDIF.

        DATA(l_sub_gid) = l_sub_item->get_gid( ).

        READ TABLE g_processed_gid TRANSPORTING NO FIELDS WITH KEY table_line = l_sub_gid.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.

        APPEND l_sub_gid TO g_processed_gid.

        PERFORM frm_read_node USING l_sub_item code.
      ENDDO.

      APPEND | END OF { l_sub_ttag_name },| TO code.

    ELSE.
      " 直接替换 - 处理简单字段
      l_ttag = l_element->find_from_name_ns( depth = 2 name = `tag` uri = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

      CHECK l_ttag IS BOUND.

      l_ttag_name = l_ttag->get_attribute_ns( name = `val` uri = `http://schemas.openxmlformats.org/wordprocessingml/2006/main` ).

      APPEND | { l_ttag_name } TYPE string,| TO code.
    ENDIF.

  ENDIF.

ENDFORM.