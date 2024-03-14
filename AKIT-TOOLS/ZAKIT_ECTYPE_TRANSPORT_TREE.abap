REPORT zakit_ectype_transport_tree.

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: e070, e07t.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_display,
         transport       TYPE trkorr,      " 请求
         type            TYPE trfunction,  " 请求类型
         typeshow        TYPE ddtext,
         trstatus        TYPE trstatus,    " 请求状态
         dsstatus        TYPE text5,
         target_system   TYPE tr_target,   " 目标系统
         owner           TYPE tr_as4user,  " 所有者
         creation_date   TYPE as4date,     " 创建日期
         creation_time   TYPE as4time,     " 创建时间
         description     TYPE as4text,     " 描述

         iself           TYPE icon_l4,     " 释放请求
         iectype         TYPE icon_l4,     " 创建副本
         irelease_ectype TYPE icon_l4,     " 创建副本并释放

         source          TYPE trkorr,
       END OF ty_display,
       tt_normal  TYPE SORTED TABLE OF ty_display WITH UNIQUE KEY transport,
       tt_subnode TYPE STANDARD TABLE OF ty_display WITH NON-UNIQUE KEY source.

TYPES: BEGIN OF MESH ty_data_load,
         normal  TYPE tt_normal ASSOCIATION _nodes TO subnode ON source = transport,
         subnode TYPE tt_subnode,
       END OF MESH ty_data_load.

*&----------------------------------------------------------------------
*                     Class
*&----------------------------------------------------------------------
CLASS lcl_event_receiver DEFINITION DEFERRED.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: go_alv_tree TYPE REF TO cl_gui_alv_tree.

DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.
DATA: gt_tree     TYPE TABLE OF ty_display,
      g_data_load TYPE ty_data_load.

DATA: go_popup_dailog TYPE REF TO cl_gui_dialogbox_container,
      go_popup_alv    TYPE REF TO cl_gui_alv_grid,
      go_popup_event  TYPE REF TO lcl_event_receiver.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECT-OPTIONS s_trnum FOR e070-trkorr.
  SELECT-OPTIONS s_owner FOR e070-as4user DEFAULT sy-uname.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  PARAMETERS: p_edabl RADIOBUTTON GROUP gp1 DEFAULT 'X' USER-COMMAND gp1,
              p_relea RADIOBUTTON GROUP gp1.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  PARAMETERS: p_adate AS CHECKBOX DEFAULT 'X'.
  PARAMETERS: p_sublk AS CHECKBOX DEFAULT 'X'.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_lkecy AS CHECKBOX DEFAULT 'X'  USER-COMMAND ley.
    SELECTION-SCREEN COMMENT 2(16) com1 FOR FIELD p_lkecy.
    PARAMETERS: p_day TYPE numc2 DEFAULT 30 MODIF ID ley.
    SELECTION-SCREEN COMMENT 22(6) com2 FOR FIELD p_day MODIF ID ley.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck3.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  %_s_trnum_%_app_%-text = '请求'.
  %_s_owner_%_app_%-text = '用户'.

  %_p_edabl_%_app_%-text = '可修改'.
  %_p_relea_%_app_%-text = '已释放'.

  %_p_adate_%_app_%-text = '自动日期替换'.
  %_p_sublk_%_app_%-text = '子节点关联'.

  com1 = '关联副本时间限制'.
  com2 = '天'.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_lkecy = '' AND screen-group1 = 'LEY'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM frm_get_data.

  PERFORM frm_set_fieldcat.

  CALL SCREEN 9000.

  " PERFORM frm_set_layout.
  " PERFORM frm_alv_display.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_close
        FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.

    DATA: dialogbox_status TYPE c.  "'X': does exist, SPACE: does not ex.

ENDCLASS.

CLASS lcl_tree_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_node_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key sender.

    METHODS handle_link_click
      FOR EVENT link_click OF cl_gui_alv_tree
      IMPORTING fieldname node_key sender.
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

CLASS lcl_tree_event_receiver IMPLEMENTATION.
  METHOD handle_node_double_click.

    sender->get_first_child( EXPORTING i_node_key = node_key
                             IMPORTING e_child_node_key = DATA(l_child_first) ).

    IF NOT l_child_first IS INITIAL.
      CALL METHOD sender->expand_node
        EXPORTING
          i_node_key    = node_key
          i_level_count = 2.
    ENDIF.

  ENDMETHOD.

  METHOD handle_link_click.
    DATA: line    TYPE ty_display,
          new     TYPE ty_display,
          new_key TYPE lvc_nkey.

    sender->get_outtab_line( EXPORTING i_node_key = node_key
                             IMPORTING e_outtab_line = line ).

    IF line-type = 'T'.
      sender->get_parent( EXPORTING i_node_key = node_key
                          IMPORTING e_parent_node_key = DATA(l_parent_key) ).
    ELSE.
      l_parent_key = node_key.

    ENDIF.

    CASE fieldname.
      WHEN 'ISELF'.
      WHEN 'IECTYPE'.
        IF NOT ( line-trstatus = 'D' OR line-trstatus = 'R' ).
          MESSAGE '不允许针对已释放内容创建副本' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        PERFORM frm_create_request USING line-description line-target_system new.

        PERFORM frm_move_line2node USING new l_parent_key new_key.

        " 添加内容
        PERFORM frm_fill_request USING line-transport new-transport.

      WHEN 'IRELEASE_ECTYPE'.
        IF line-type <> 'T'.
          IF NOT ( line-trstatus = 'D' OR line-trstatus = 'R' ).
            MESSAGE '不允许针对已释放内容创建副本' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          PERFORM frm_create_request USING line-description line-target_system new.

          PERFORM frm_move_line2node USING new l_parent_key new_key.

          PERFORM frm_fill_request USING line-transport new-transport.

        ELSE.
          IF NOT ( line-trstatus = 'D' OR line-trstatus = 'R' ).
            MESSAGE '传输副本已释放' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          new = line.
          new_key = node_key.
        ENDIF.

        " 释放请求
        PERFORM frm_release_request USING new-transport .

        CLEAR: new-irelease_ectype.
        new-dsstatus = '已释放'.
        PERFORM frm_change_node USING new new_key.

      WHEN OTHERS.
    ENDCASE.

    sender->frontend_update( ).

    sender->get_first_child( EXPORTING i_node_key = node_key
                             IMPORTING e_child_node_key = DATA(l_child_first) ).
    IF l_child_first IS NOT INITIAL.
      sender->expand_node( i_node_key = node_key ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

MODULE pbo OUTPUT.
  SET PF-STATUS 'STANDARD'.

  IF go_alv_tree IS INITIAL.

    go_alv_tree = NEW #( parent = cl_gui_container=>screen0
                         node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
                         item_selection = 'X'
                         no_html_header = 'X'
                         no_toolbar     = 'X'
    ).

    go_alv_tree->set_table_for_first_display(
      EXPORTING
        is_hierarchy_header = VALUE #( heading = 'Transport'
                                       tooltip = 'Normal Transport'
                                       width   = 25 )
      CHANGING
        it_outtab       = gt_tree
        it_fieldcatalog = gt_fieldcat
    ).

    " 添加节点
    PERFORM frm_create_hierarchy.

    " 事件注册
    PERFORM frm_register_events.

    " 显示更新
    go_alv_tree->frontend_update( ).
  ENDIF.

ENDMODULE.

MODULE pai INPUT.

  CASE sy-ucomm .
    WHEN '&F03' OR '&F15' OR  '&F12'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& 拉取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA: lt_dd07v TYPE TABLE OF dd07v.
  DATA: lt_range_status TYPE RANGE OF e070-trstatus.
  DATA: lt_range_name TYPE RANGE OF e071-obj_name.
  DATA: lt_range_day TYPE RANGE OF datum.

  IF p_lkecy = 'X'.
    lt_range_day = VALUE #( sign = 'I' option = 'GE' ( low = sy-datum - p_day ) ).
  ENDIF.

  IF p_relea = 'X'.
    lt_range_status = VALUE #( sign = 'I' option = 'EQ' ( low = 'O' ) ( low = 'R' ) ( low = 'N' ) ).
  ELSE.
    lt_range_status = VALUE #( sign = 'I' option = 'EQ' ( low = 'D' ) ( low = 'L' ) ).
  ENDIF.

  SELECT FROM e070
    LEFT JOIN e07t ON e07t~trkorr = e070~trkorr
    LEFT JOIN e070 AS sup ON sup~trkorr = e070~strkorr
    FIELDS
      e070~trkorr AS transport,
      e070~trfunction AS type,
      e070~trstatus,
      e070~as4user AS owner,
      e070~as4date AS creation_date,
      e070~as4time AS creation_time,
      CASE WHEN e070~tarsystem <> @space
           THEN e070~tarsystem ELSE sup~tarsystem END AS target_system,
      e07t~as4text AS description
     WHERE e070~trkorr  IN @s_trnum
       AND e070~as4user IN @s_owner
       AND ( e070~strkorr     = '' )
       AND ( e070~trstatus   IN @lt_range_status )
       AND ( e070~trfunction <> 'T' )
     ORDER BY e070~trkorr DESCENDING, e070~as4date DESCENDING
     INTO CORRESPONDING FIELDS OF TABLE @g_data_load-normal.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'TRFUNCTION'
      langu          = '1'
      text           = 'T'
    TABLES
      dd07v_tab      = lt_dd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  SORT lt_dd07v BY domvalue_l.

  LOOP AT g_data_load-normal ASSIGNING FIELD-SYMBOL(<ls_normal>).
    <ls_normal>-iself                  = icon_transport.

    <ls_normal>-iectype                = icon_copy_object.
    <ls_normal>-irelease_ectype        = icon_import_transport_request.
    " <ls_normal>-irelease_import_ectype = icon_pm_insert.

    READ TABLE lt_dd07v INTO DATA(ls_dd07v) WITH KEY domvalue_l = CONV #( <ls_normal>-type ).
    IF sy-subrc = 0.
      <ls_normal>-typeshow = ls_dd07v-ddtext.
    ENDIF.

    CASE <ls_normal>-trstatus.
      WHEN 'D' OR 'L'.
        <ls_normal>-dsstatus = '可修改'.
      WHEN 'R' OR 'N'.
        <ls_normal>-dsstatus = '已释放'.
      WHEN OTHERS.
    ENDCASE.

    APPEND VALUE #( sign = 'I' option = 'CP' low = |*{ <ls_normal>-transport }*| ) TO lt_range_name.
  ENDLOOP.

  IF p_sublk = 'X'.
    SELECT
      trkorr,
      strkorr
      FROM e070
      FOR ALL ENTRIES IN @g_data_load-normal
      WHERE strkorr = @g_data_load-normal-transport
      INTO TABLE @DATA(lt_sub_link).
    SORT lt_sub_link BY trkorr.

    lt_range_name = VALUE #( BASE lt_range_name FOR sub IN lt_sub_link ( sign = 'I' option = 'CP' low = |*{ sub-trkorr }*| ) ).
    SORT lt_range_name BY low.
    DELETE ADJACENT DUPLICATES FROM lt_range_name COMPARING low.
  ENDIF.

  IF lt_range_name IS NOT INITIAL AND p_relea IS INITIAL.
    SELECT
      e1~trkorr AS transport,
      e1~as4pos,
      e1~obj_name,
      e0~trfunction AS type,
      e0~trstatus,
      e0~as4user AS owner,
      e0~as4date AS creation_date,
      e0~as4time AS creation_time,
      e0~tarsystem AS target_system,
      et~as4text AS description
      FROM e071 AS e1
      INNER JOIN e070 AS e0 ON e0~trkorr = e1~trkorr AND e0~trfunction = 'T'
      LEFT JOIN e07t AS et ON et~trkorr = e0~trkorr AND et~langu = @sy-langu
      WHERE e1~pgmid  = 'CORR'
        AND e1~object = 'MERG'
        AND e0~as4date IN @lt_range_day
        AND e1~obj_name IN @lt_range_name
      INTO TABLE @DATA(lt_subnode).
    SORT lt_subnode BY transport obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_subnode COMPARING transport obj_name.

    LOOP AT lt_subnode REFERENCE INTO DATA(lr_subnode).
      APPEND CORRESPONDING #( lr_subnode->* ) TO g_data_load-subnode
        ASSIGNING FIELD-SYMBOL(<ls_subnode>).

      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domvalue_l = CONV #( <ls_subnode>-type ).
      IF sy-subrc = 0.
        <ls_subnode>-typeshow = ls_dd07v-ddtext.
      ENDIF.

      CASE <ls_subnode>-trstatus.
        WHEN 'D' OR 'L'.
          <ls_subnode>-dsstatus = '可修改'. " K
          <ls_subnode>-irelease_ectype = icon_import_transport_request.
        WHEN 'R' OR 'N'.
          <ls_subnode>-dsstatus = '已释放'. " Y
        WHEN OTHERS.
      ENDCASE.

      " 正则匹配请求内容
      DATA(l_regex) = |({ lr_subnode->transport(4) }\\d+)|.

      " 正则匹配内容
      FIND REGEX l_regex IN lr_subnode->obj_name SUBMATCHES DATA(l_transport).

      " 关联排除
      IF p_sublk = 'X'.
        " 夫请求 替换 子请求
        READ TABLE lt_sub_link INTO DATA(ls_sub_link) WITH KEY trkorr = l_transport BINARY SEARCH.
        IF sy-subrc = 0.
          l_transport = ls_sub_link-strkorr.

          <ls_subnode>-dsstatus = '-'.
        ENDIF.
      ENDIF.

      <ls_subnode>-source = l_transport.

      CLEAR l_transport.
    ENDLOOP.

  ENDIF.
  SORT g_data_load-subnode BY source transport dsstatus DESCENDING creation_date creation_time.
  DELETE ADJACENT DUPLICATES FROM g_data_load-subnode COMPARING source transport.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'TYPESHOW'          18 'C' ''       '请求类型'(001)." TEXT-001. " 请求类型
  PERFORM frm_set_fcat USING 'TARGET_SYSTEM'     14 'C' ''       '目标系统'(002)." TEXT-002. " 目标系统
  PERFORM frm_set_fcat USING 'DSSTATUS'          14 'C' ''       '状态'(003).    " TEXT-003. " 状态
  PERFORM frm_set_fcat USING 'OWNER'             16 ' ' ''       '所有者'(004).  " TEXT-004. " 所有者
  PERFORM frm_set_fcat USING 'CREATION_DATE'     14 'C' 'DATS'   '创建日期'(005)." TEXT-005. " 创建日期
  PERFORM frm_set_fcat USING 'CREATION_TIME'     14 'C' 'TIMS'   '创建时间'(006)." TEXT-006. " 创建日期
  PERFORM frm_set_fcat USING 'DESCRIPTION'       70 ' ' ''       '描述'(007).    " TEXT-007. " 描述

  CHECK p_relea = ''.

* PERFORM frm_set_fcat USING 'ISELF'             12 ' ' ''       '释放请求'(008)." TEXT-008. " 释放请求
  PERFORM frm_set_fcat USING 'IECTYPE'           12 ' ' ''       '创建副本'(009)." TEXT-009. " 创建副本
  PERFORM frm_set_fcat USING 'IRELEASE_ECTYPE'   12 ' ' ''       '副本释放'(010)." TEXT-010. " 副本释放

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FCAT
*&---------------------------------------------------------------------*
*&  设置fcat
*&---------------------------------------------------------------------*
FORM frm_set_fcat USING   p_fieldname
                          p_outputlen
                          p_just
                          p_datatype
                          p_coltext.
  DATA: lv_filedname TYPE lvc_s_fcat-fieldname,
        lv_coltext   TYPE lvc_s_fcat-coltext.

  lv_filedname = p_fieldname.
  lv_coltext   = p_coltext.


  APPEND VALUE lvc_s_fcat( fieldname  = lv_filedname
                           coltext    = p_coltext
                           scrtext_l  = p_coltext
                           scrtext_m  = p_coltext
                           scrtext_s  = p_coltext
                           just       = p_just
                           datatype   = p_datatype
                           outputlen  = p_outputlen ) TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).

  IF p_fieldname+0(1) = 'I'.
    <ls_fieldcat>-icon = 'X'.
    " <ls_fieldcat>-hotspot = 'X'. " >> 有用 但是针对有子节点的无效果
  ENDIF.

  " IF p_fieldname = 'TRANSPORT' OR ( strlen( p_fieldname ) > 7 AND p_fieldname+0(7) = 'ECTYPE_' ).
  "   <ls_fieldcat>-hotspot = 'X'.
  " ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_create_hierarchy
*&---------------------------------------------------------------------*
*&  添加节点
*&---------------------------------------------------------------------*
FORM frm_create_hierarchy .
  DATA: lv_obj_name TYPE e071-obj_name,
        l_node_key  TYPE lvc_nkey,
        l_parent_k  TYPE lvc_nkey.

  LOOP AT g_data_load-normal REFERENCE INTO DATA(lr_normal).

    PERFORM frm_move_line2node USING lr_normal->* '' l_parent_k.
    " go_alv_tree->add_node(
    "   EXPORTING
    "     i_relat_node_key = ''
    "     i_relationship   = cl_gui_column_tree=>relat_first_child " relat_last_child
    "     is_outtab_line   = lr_normal->*
    "     it_item_layout   = VALUE #( ( fieldname = 'IECTYPE'  class = cl_gui_column_tree=>item_class_link )
    "                                 ( fieldname = 'IRELEASE_ECTYPE'  class = cl_gui_column_tree=>item_class_link )
    "                                 ( fieldname = 'DSSTATUS' style = COND #( WHEN lr_normal->dsstatus = '可修改'
    "                                                                          THEN cl_gui_column_tree=>style_emphasized_positive
    "                                                                          ELSE cl_gui_column_tree=>style_emphasized ) ) )
    "     i_node_text      = CONV #( lr_normal->transport )
    "   IMPORTING
    "     e_new_node_key   = DATA(l_node_key)
    " ).

    DATA(lt_nodes) = VALUE tt_subnode( FOR sub IN g_data_load-normal\_nodes[ lr_normal->* ]
      ( sub ) ).

    LOOP AT lt_nodes REFERENCE INTO DATA(lr_node).

      DATA(ls_display) = CORRESPONDING ty_display( lr_node->* ).

      PERFORM frm_move_line2node USING lr_node->* l_parent_k l_node_key.
      " go_alv_tree->add_node(
      "     i_relat_node_key = l_node_key
      "     i_relationship   = cl_gui_column_tree=>relat_first_child " relat_last_child
      "     is_outtab_line   = ls_display
      "     it_item_layout   = VALUE #( ( fieldname = 'IRELEASE_ECTYPE'
      "                                         class = COND #( WHEN lr_node->irelease_ectype IS NOT INITIAL
      "                                                         THEN cl_gui_column_tree=>item_class_link ) )
      "                                 ( fieldname = 'DSSTATUS' style = COND #( WHEN lr_node->dsstatus = '可修改'
      "                                                                          THEN cl_gui_column_tree=>style_emphasized_positive
      "                                                                          ELSE cl_gui_column_tree=>style_emphasized ) ) )
      "     i_node_text      = CONV #( lr_node->transport )
      " ).
    ENDLOOP.

    CLEAR: l_parent_k, l_node_key.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_register_events
*&---------------------------------------------------------------------*
*&  事件注册
*&---------------------------------------------------------------------*
FORM frm_register_events .
  DATA: lt_events        TYPE cntl_simple_events,
        l_event          TYPE cntl_simple_event,
        l_event_receiver TYPE REF TO lcl_tree_event_receiver.

  go_alv_tree->get_registered_events( IMPORTING events = lt_events ).

  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND l_event TO lt_events.

  l_event-eventid = cl_gui_column_tree=>eventid_link_click.
  APPEND l_event TO lt_events.

  go_alv_tree->set_registered_events( EXPORTING events = lt_events ).

  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->handle_node_double_click FOR go_alv_tree.
  SET HANDLER l_event_receiver->handle_link_click FOR go_alv_tree.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_REQUEST
*&---------------------------------------------------------------------*
*& 创建请求
*&---------------------------------------------------------------------*
FORM frm_create_request  USING p_description  TYPE ty_display-description
                               p_targetsystem TYPE ty_display-target_system
                               new TYPE ty_display.
  DATA: lt_text        TYPE catsxt_longtext_itab,
        lv_text        TYPE as4text,
        lv_description TYPE ty_display-description.

  lv_description = p_description.

  IF p_adate = 'X'.
    REPLACE REGEX '\d{4}[01]\d[0123]\d' IN lv_description WITH sy-datum.
  ENDIF.

  APPEND |副本_{ lv_description }| TO lt_text.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title = '副本描述'(t01)
    CHANGING
      ch_text  = lt_text.

  CONCATENATE LINES OF lt_text INTO lv_text.

  TRY.
      " 创建副本
      cl_adt_cts_management=>create_empty_request(
        EXPORTING iv_type = 'T' " 副本请求
                  iv_text = lv_text
                  iv_target = p_targetsystem
        IMPORTING es_request_header = DATA(ls_transport_header) ).

      " DATA(ls_transport_header) = VALUE trwbo_request_header( ).

      new = CORRESPONDING #( ls_transport_header MAPPING transport = trkorr
                                                         type      = trfunction
                                                         owner     = as4user
                                                         creation_date = as4date
                                                         creation_time = as4time
                                                         target_system = tarsystem
                                                         description   = as4text ).

      new-dsstatus = '可修改'.
      new-typeshow = '传输副本'.
      new-irelease_ectype = icon_import_transport_request.

    CATCH cx_root INTO DATA(cx).
      MESSAGE cx->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FILL_REQUEST
*&---------------------------------------------------------------------*
*&  补充请求内容
*&---------------------------------------------------------------------*
FORM frm_fill_request  USING p_source_t TYPE e070-trkorr
                             p_target_t TYPE e070-trkorr.
  DATA: lt_request_headers TYPE trwbo_request_headers.

  CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
    EXPORTING
      iv_trkorr          = p_source_t
    IMPORTING
      et_request_headers = lt_request_headers
*     ET_REQUESTS        =
    EXCEPTIONS
      invalid_input      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
           sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_request_headers INTO DATA(ls_rh) WHERE trkorr = p_source_t
                                                 OR strkorr = p_source_t.

    " 将请求内容添加到副本请求中

    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
        wi_dialog                = ' '
        wi_trkorr_from           = ls_rh-trkorr
        wi_trkorr_to             = p_target_t
        wi_without_documentation = ''
      EXCEPTIONS
        db_access_error          = 1
        trkorr_from_not_exist    = 2
        trkorr_to_is_repair      = 3
        trkorr_to_locked         = 4
        trkorr_to_not_exist      = 5
        trkorr_to_released       = 6
        user_not_owner           = 7
        no_authorization         = 8
        wrong_client             = 9
        wrong_category           = 10
        object_not_patchable     = 11
        OTHERS                   = 12.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
             sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_release_request
*&---------------------------------------------------------------------*
*&  释放请求
*&---------------------------------------------------------------------*
FORM frm_release_request  USING p_transport TYPE e070-trkorr.

  TRY.
      DATA(lo_cts) = cl_cts_rest_api_factory=>create_instance( ).

      lo_cts->release( iv_trkorr = p_transport iv_ignore_locks = 'X' ).

    CATCH cx_root INTO DATA(cx).
      MESSAGE cx->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_move_line2node
*&---------------------------------------------------------------------*
*&  将行项目写入节点
*&---------------------------------------------------------------------*
FORM frm_move_line2node  USING p_line     TYPE ty_display
                               p_node_key TYPE lvc_nkey
                               p_new_key  TYPE lvc_nkey.

  " 颜色参考: SAPTREX_NODE_STYLES

  go_alv_tree->add_node(
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_first_child " relat_last_child
      is_outtab_line   = p_line
      it_item_layout   = VALUE #( ( fieldname = 'IECTYPE'  class = COND #( WHEN p_line-iectype IS NOT INITIAL
                                                                           THEN cl_gui_column_tree=>item_class_link ) )
                                  ( fieldname = 'IRELEASE_ECTYPE'  class = COND #( WHEN p_line-irelease_ectype IS NOT INITIAL
                                                                                   THEN cl_gui_column_tree=>item_class_link ) )
                                  ( fieldname = 'DSSTATUS' style = COND #( WHEN p_line-dsstatus = '可修改'
                                                                           THEN cl_gui_column_tree=>style_emphasized_positive
                                                                           ELSE cl_gui_column_tree=>style_emphasized ) ) )
      i_node_text      = CONV #( p_line-transport )
    IMPORTING
      e_new_node_key   = DATA(l_node_key)
  ).

  p_new_key = l_node_key.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_NODE
*&---------------------------------------------------------------------*
*&  修改节点
*&---------------------------------------------------------------------*
FORM frm_change_node  USING p_new TYPE ty_display
                            p_key TYPE lvc_nkey.

  " sender->change_item 更改单元格 => 要更改 is_item_layout-u_* 才生效
  " sender->change_node 更改行

  go_alv_tree->change_node(
    EXPORTING
      i_node_key      = p_key
      i_outtab_line   = p_new
      it_item_layout  = VALUE #( ( fieldname = 'IECTYPE'  class = COND #( WHEN p_new-iectype IS NOT INITIAL
                                                                          THEN cl_gui_column_tree=>item_class_link ) )
                                 ( fieldname = 'IRELEASE_ECTYPE'  class = COND #( WHEN p_new-irelease_ectype IS NOT INITIAL
                                                                                  THEN cl_gui_column_tree=>item_class_link ) )
                                 ( fieldname = 'DSSTATUS' style = COND #( WHEN p_new-dsstatus = '可修改'
                                                                          THEN cl_gui_column_tree=>style_emphasized_positive
                                                                          ELSE cl_gui_column_tree=>style_emphasized ) ) )
  ).

ENDFORM.