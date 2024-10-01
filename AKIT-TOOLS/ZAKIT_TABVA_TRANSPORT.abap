REPORT zakit_tabva_transport.

" 表内容传输请求

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: tvdir, dd02l.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_display,
         tabname  TYPE tvdir-tabname, " 视图/表
         ddtext   TYPE dd02t-ddtext,  " 描述
         type     TYPE tvdir-type,    " 维护类型
         bastab   TYPE tvdir-bastab,  " 类型
         count    TYPE i,             " 维护数据量
         readc    TYPE flag,          " 处理过

         tabclass TYPE dd02l-tabclass,
         box      TYPE flag,
       END OF ty_display.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.
DATA: gt_display TYPE TABLE OF ty_display.

DATA: go_float_docker TYPE REF TO cl_gui_docking_container.
DATA: go_detial_alv TYPE REF TO cl_gui_alv_grid.

DATA: gs_detial_layout   TYPE lvc_s_layo,
      gt_detial_fieldcat TYPE lvc_t_fcat.
DATA: go_detial TYPE REF TO data.
DATA: g_currer_detial TYPE ty_display.
FIELD-SYMBOLS: <gt_detial> TYPE table.

CLASS lcl_detial_handle DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS init_events IMPORTING i_alv TYPE REF TO cl_gui_alv_grid.

    METHODS handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm sender.
ENDCLASS.

*&----------------------------------------------------------------------
*                     Selection Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECT-OPTIONS: s_table FOR dd02l-tabname.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  PARAMETERS: p_sm30 AS CHECKBOX USER-COMMAND sm30 DEFAULT 'X'.
  PARAMETERS: p_edit AS CHECKBOX USER-COMMAND edit DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blck2.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

  %_p_sm30_%_app_%-text  = '仅 SM30 维护内容'.
  %_p_edit_%_app_%-text  = '仅 可维护 表'.
  %_s_table_%_app_%-text = '表'.

AT SELECTION-SCREEN OUTPUT.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_get_data.

  PERFORM frm_set_fieldcat.
  PERFORM frm_set_layout.
  PERFORM frm_alv_display.

CLASS lcl_detial_handle IMPLEMENTATION.
  METHOD init_events.
    " Alv 事件注册
    DATA: lr_alv_event TYPE REF TO lcl_detial_handle.

    CREATE OBJECT lr_alv_event.
    SET HANDLER lr_alv_event->handle_user_command FOR i_alv.
    SET HANDLER lr_alv_event->handle_toolbar      FOR i_alv.

  ENDMETHOD.

  METHOD handle_user_command.
    DATA: lt_rowid TYPE lvc_t_row.

    DATA: lt_e071k TYPE TABLE OF e071k,
          lt_e071  TYPE TABLE OF e071.
    DATA: lt_fieldcat_key TYPE lvc_t_fcat,
          lo_keys         TYPE REF TO data,
          lo_key          TYPE REF TO data,
          tabkey          TYPE trobj_name,
          length          TYPE i.
    FIELD-SYMBOLS: <ls_key> TYPE data,
                   <lt_key> TYPE STANDARD TABLE.

    CASE e_ucomm.
      WHEN 'ADD_TF'.
        sender->get_selected_rows( IMPORTING et_index_rows = lt_rowid ).

        IF lines( lt_rowid ) < 1.
          MESSAGE '请至少选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        DATA order TYPE e071-trkorr.
        DATA task TYPE e071-trkorr.

        " 选择请求
        CALL FUNCTION 'TRINT_ORDER_CHOICE'
          EXPORTING
            wi_order_type          = 'K' " K 工作台 W 定制
            wi_task_type           = 'S' " S 开发更正 Q 定制
            wi_category            = 'SYST' " SYST / CUST
          IMPORTING
            we_order               = order
            we_task                = task
          TABLES
            wt_e071                = lt_e071
            wt_e071k               = lt_e071k
          EXCEPTIONS
            no_correction_selected = 1
            display_mode           = 2
            object_append_error    = 3
            recursive_call         = 4
            wrong_order_type       = 5
            OTHERS                 = 6.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
          RETURN.
        ENDIF.

        " 添加选中内容进请求
        APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = g_currer_detial-tabname objfunc = 'K' ) TO lt_e071.

        MOVE-CORRESPONDING gt_detial_fieldcat TO lt_fieldcat_key.
        DELETE lt_fieldcat_key WHERE key = abap_false.

        TRY.
            cl_alv_table_create=>create_dynamic_table( EXPORTING it_fieldcatalog = lt_fieldcat_key
                                                       IMPORTING ep_table        = lo_keys ).
            ASSIGN lo_keys->* TO <lt_key>.
            CREATE DATA lo_key LIKE LINE OF <lt_key>.
          CATCH cx_root INTO DATA(l_cx_root).
            MESSAGE l_cx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.

        " 参考 ZAL30\ZACA_REASSIGN_DEVCLASS\TALV
        LOOP AT lt_rowid INTO DATA(ls_rowid).
          READ TABLE <gt_detial> ASSIGNING FIELD-SYMBOL(<ls_detial>) INDEX ls_rowid-index.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO <lt_key> ASSIGNING <ls_key>.

            MOVE-CORRESPONDING <ls_detial> TO <ls_key>.
            tabkey = <ls_key>.

            APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' mastertype = 'TABU'
              mastername = g_currer_detial-tabname objname = g_currer_detial-tabname tabkey = tabkey ) TO lt_e071k.

          ENDIF.
        ENDLOOP.


        CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
          EXPORTING
            wi_suppress_key_check          = ' ' " X 不检查主键
            wi_trkorr                      = task
          TABLES
            wt_e071                        = lt_e071
            wt_e071k                       = lt_e071k
          EXCEPTIONS
            key_char_in_non_char_field     = 1
            key_check_keysyntax_error      = 2
            key_inttab_table               = 3
            key_longer_field_but_no_generc = 4
            key_missing_key_master_fields  = 5
            key_missing_key_tablekey       = 6
            key_non_char_but_no_generic    = 7
            key_no_key_fields              = 8
            key_string_longer_char_key     = 9
            key_table_has_no_fields        = 10
            key_table_not_activ            = 11
            key_unallowed_key_function     = 12
            key_unallowed_key_object       = 13
            key_unallowed_key_objname      = 14
            key_unallowed_key_pgmid        = 15
            key_without_header             = 16
            ob_check_obj_error             = 17
            ob_devclass_no_exist           = 18
            ob_empty_key                   = 19
            ob_generic_objectname          = 20
            ob_ill_delivery_transport      = 21
            ob_ill_lock                    = 22
            ob_ill_parts_transport         = 23
            ob_ill_source_system           = 24
            ob_ill_system_object           = 25
            ob_ill_target                  = 26
            ob_inttab_table                = 27
            ob_local_object                = 28
            ob_locked_by_other             = 29
            ob_modif_only_in_modif_order   = 30
            ob_name_too_long               = 31
            ob_no_append_of_corr_entry     = 32
            ob_no_append_of_c_member       = 33
            ob_no_consolidation_transport  = 34
            ob_no_original                 = 35
            ob_no_shared_repairs           = 36
            ob_no_systemname               = 37
            ob_no_systemtype               = 38
            ob_no_tadir                    = 39
            ob_no_tadir_not_lockable       = 40
            ob_privat_object               = 41
            ob_repair_only_in_repair_order = 42
            ob_reserved_name               = 43
            ob_syntax_error                = 44
            ob_table_has_no_fields         = 45
            ob_table_not_activ             = 46
            tr_enqueue_failed              = 47
            tr_errors_in_error_table       = 48
            tr_ill_korrnum                 = 49
            tr_lockmod_failed              = 50
            tr_lock_enqueue_failed         = 51
            tr_not_owner                   = 52
            tr_no_systemname               = 53
            tr_no_systemtype               = 54
            tr_order_not_exist             = 55
            tr_order_released              = 56
            tr_order_update_error          = 57
            tr_wrong_order_type            = 58
            ob_invalid_target_system       = 59
            tr_no_authorization            = 60
            ob_wrong_tabletyp              = 61
            ob_wrong_category              = 62
            ob_system_error                = 63
            ob_unlocal_objekt_in_local_ord = 64
            tr_wrong_client                = 65
            ob_wrong_client                = 66
            key_wrong_client               = 67
            OTHERS                         = 68.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE '添加完成' TYPE 'S'.
        ENDIF.
      WHEN 'HIDDEN'.
        go_float_docker->set_visible( abap_false ).
    ENDCASE.

  ENDMETHOD.

  METHOD handle_toolbar.
    "分割符
    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.

    " 添加到请求
    " IF sy-sysid = 'DEV'.
    APPEND VALUE #( function  = 'ADD_TF'
                    text      = '添加到请求'
                    quickinfo = '将表内容添加到请求中'
                    butn_type = 4
                    icon      = icon_export ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.
    " ENDIF.

    " 关闭
    APPEND VALUE #( function  = 'HIDDEN'
                    text      = '关闭'
                    quickinfo = '关闭'
                    butn_type = 4
                    icon      = icon_close ) TO e_object->mt_toolbar.

  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
*&  获取基础数据
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA: ls_display TYPE ty_display.

  DATA: lt_range_mainflag TYPE RANGE OF dd02l-mainflag.

  IF s_table[] IS INITIAL.
    s_table[] = VALUE #( sign = 'I' option = 'CP' ( low = 'Z*' ) ).
  ENDIF.

  IF p_edit = 'X'.
    lt_range_mainflag = VALUE #( sign = 'I' option = 'EQ' ( low = 'X' ) ).
  ENDIF.

  SELECT
    tabname,
    tabclass
    FROM dd02l
    WHERE tabname IN @s_table
      AND as4local = 'A'
      AND mainflag IN @lt_range_mainflag
      AND tabclass IN ('TRANSP', 'VIEW')
    INTO TABLE @DATA(lt_dd02l).

  SELECT
    *
    FROM tvdir
    WHERE tabname IN @s_table
    INTO TABLE @DATA(lt_tvdir).
  SORT lt_tvdir BY tabname.

  SELECT
    tabname,
    ddlanguage,
    ddtext
    FROM dd02t
    WHERE tabname IN @s_table
      AND as4local = 'A'
    INTO TABLE @DATA(lt_dd02t).
  SORT lt_dd02t BY tabname ddlanguage.

  LOOP AT lt_dd02l INTO DATA(ls_dd02l).
    ls_display-tabname  = ls_dd02l-tabname.
    ls_display-tabclass = ls_dd02l-tabclass.

    READ TABLE lt_tvdir INTO DATA(ls_tvdir) WITH KEY tabname = ls_dd02l-tabname BINARY SEARCH.
    IF sy-subrc = 0.
      ls_display-type    = ls_tvdir-type.
      ls_display-bastab  = ls_tvdir-bastab.
    ELSE.
      IF p_sm30 = 'X'.
        CLEAR ls_display.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE lt_dd02t INTO DATA(ls_dd02t) WITH KEY tabname = ls_dd02l-tabname BINARY SEARCH.
    IF sy-subrc = 0.
      ls_display-ddtext = ls_dd02t-ddtext.
    ENDIF.

    TRY.
        SELECT COUNT(*) FROM (ls_dd02l-tabname) INTO ls_display-count.
      CATCH cx_root.
        ls_display-ddtext = 'error count=>' && ls_display-ddtext.
    ENDTRY.

    APPEND ls_display TO gt_display.
    CLEAR ls_display.
  ENDLOOP.

  SORT gt_display BY count DESCENDING.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'TABNAME' 'TVDIR' 'TABNAME' '视图/表'(001).
  PERFORM frm_set_fcat USING 'DDTEXT'  'DD02T' 'DDTEXT'  '描述'(002).
  PERFORM frm_set_fcat USING 'TYPE'    'TVDIR' 'TYPE'    '维护类型'(003).
  PERFORM frm_set_fcat USING 'BASTAB'  'TVDIR' 'BASTAB'  '类型'(004).
  PERFORM frm_set_fcat USING 'COUNT'   ''      ''        '维护数据量'(005).
  PERFORM frm_set_fcat USING 'READC'   ''      ''        '处理过'(006).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FCAT
*&---------------------------------------------------------------------*
*&  设置fcat
*&---------------------------------------------------------------------*
FORM frm_set_fcat USING   p_fieldname
                          p_ref_table TYPE lvc_s_fcat-ref_table
                          p_ref_field TYPE lvc_s_fcat-ref_field
                          p_coltext.
  DATA: lv_filedname TYPE lvc_s_fcat-fieldname,
        lv_coltext   TYPE lvc_s_fcat-coltext.

  lv_filedname = p_fieldname.
  lv_coltext   = p_coltext.


  APPEND VALUE lvc_s_fcat( fieldname  = lv_filedname
                           coltext    = p_coltext
                           ref_table  = p_ref_table
                           ref_field  = p_ref_field
                           scrtext_l  = p_coltext
                           scrtext_m  = p_coltext
                           scrtext_s  = p_coltext ) TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).

  IF lv_filedname = 'READC'.
    <ls_fieldcat>-checkbox = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*&  设置 LAYOUT
*&---------------------------------------------------------------------*
FORM frm_set_layout .

  gs_layout = VALUE #(
                       zebra = 'X'
                       cwidth_opt = 'X'
                       ctab_fname = 'COLOR'
                       box_fname  = 'BOX'
                      ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PF_STATUS
*&---------------------------------------------------------------------*
*       toolbar 设置
*----------------------------------------------------------------------*
FORM frm_pf_status USING p_extab TYPE slis_t_extab.
  DATA: lt_extab TYPE slis_t_extab.
  MOVE-CORRESPONDING p_extab TO lt_extab.

  SET PF-STATUS 'STANDARD' EXCLUDING lt_extab.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*       点击事件处理
*----------------------------------------------------------------------*
FORM frm_user_command USING p_ucomm LIKE sy-ucomm
                            ps_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_rows TYPE lvc_t_row,
        ls_row  TYPE lvc_s_row.
  DATA: lv_subrc TYPE i.

  CASE p_ucomm.
    WHEN '&IC1'.
      READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<ls_display>) INDEX ps_selfield-tabindex.

      PERFORM frm_popup_detial USING <ls_display>.
      <ls_display>-readc = 'X'.

    WHEN OTHERS.
  ENDCASE.

  ps_selfield-refresh = 'X'.
  " ps_selfield-col_stable = 'X' .
  ps_selfield-row_stable = 'X' .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_alv_display
*&---------------------------------------------------------------------*
*&  ALV 展示
*&---------------------------------------------------------------------*
FORM frm_alv_display .
  DATA: lt_events TYPE slis_t_event WITH HEADER LINE.
  DATA: lt_events_exit TYPE slis_t_event_exit.
  DATA: ls_variant TYPE disvariant.
  DATA: lv_lines TYPE i.

  ls_variant-report = sy-repid.

  " APPEND VALUE #( ucomm = '&F12' before = 'X' ) TO lt_events_exit.
  " APPEND VALUE #( ucomm = '&F15' before = 'X' ) TO lt_events_exit.
  " APPEND VALUE #( ucomm = '&F03' before = 'X' ) TO lt_events_exit.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_save                  = 'X'
      i_default               = 'X'
      is_variant              = ls_variant
      i_callback_program      = sy-repid
      "i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command = 'FRM_USER_COMMAND'
      is_layout_lvc           = gs_layout
      it_fieldcat_lvc         = gt_fieldcat
      it_events               = lt_events[]
      it_event_exit           = lt_events_exit
    TABLES
      t_outtab                = gt_display
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_POPUP_DETIAL
*&---------------------------------------------------------------------*
*&  展示详情
*&---------------------------------------------------------------------*
FORM frm_popup_detial USING ps_display TYPE ty_display.
  DATA: ls_variant TYPE disvariant.

  IF go_detial_alv IS NOT INITIAL.
    " 清空上次的 detial
    go_detial_alv->free( ).
    CLEAR go_detial_alv.

    CLEAR gt_detial_fieldcat.
    CLEAR g_currer_detial.
  ENDIF.

  " 检查是否有配置数据
  IF ps_display-count = 0.
    MESSAGE '目前环境无可包请求的数据' TYPE 'S' DISPLAY LIKE 'E'.

    " 隐藏
    IF go_float_docker IS NOT INITIAL. go_float_docker->set_visible( abap_false ). ENDIF.
    RETURN.
  ENDIF.

  " 检查当前是否可包请求（开发未考虑）
  IF ps_display-tabclass = 'VIEW'.
    MESSAGE '暂不支持表维护' TYPE 'S' DISPLAY LIKE 'E'.

    " 隐藏
    IF go_float_docker IS NOT INITIAL. go_float_docker->set_visible( abap_false ). ENDIF.
    RETURN.
  ENDIF.

  g_currer_detial = ps_display.

  IF go_float_docker IS INITIAL.
    go_float_docker = NEW #( dynnr = sy-dynnr extension = 205 side  = cl_gui_docking_container=>dock_at_bottom ).
  ENDIF.

  " 显示
  go_float_docker->set_visible( abap_true ).

  " >> load data
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = ps_display-tabname
    CHANGING
      ct_fieldcat            = gt_detial_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  " APPEND VALUE #( fieldname = 'BOX' ) TO gt_detial_fieldcat.

  TRY.
      CALL METHOD cl_alv_table_create=>create_dynamic_table
        EXPORTING
          it_fieldcatalog           = gt_detial_fieldcat
        IMPORTING
          ep_table                  = go_detial
        EXCEPTIONS
          generate_subpool_dir_full = 1
          OTHERS                    = 2.
      CHECK sy-subrc = 0.

      ASSIGN go_detial->* TO <gt_detial>.

      SELECT
        *
        FROM (ps_display-tabname)
        INTO CORRESPONDING FIELDS OF TABLE <gt_detial>.
    CATCH cx_root INTO DATA(l_cx_root).
      MESSAGE l_cx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  MOVE-CORRESPONDING gs_layout TO gs_detial_layout.
  gs_detial_layout-sel_mode = 'D'.
  " <<

  " display
  IF go_detial_alv IS INITIAL.
    go_detial_alv = NEW #( i_parent = go_float_docker ).

    ls_variant-report = sy-repid.

    lcl_detial_handle=>init_events( go_detial_alv ).

    go_detial_alv->set_table_for_first_display(
       EXPORTING
         is_variant                    = ls_variant
         is_layout                     = gs_detial_layout
       " it_toolbar_excluding          = lt_exclude
       CHANGING
         it_outtab                     = <gt_detial>
         it_fieldcatalog               = gt_detial_fieldcat
       EXCEPTIONS
         invalid_parameter_combination = 1
         program_error                 = 2
         too_many_lines                = 3
         OTHERS                        = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.


ENDFORM.