REPORT zdemo_soap_auth_list.

" 查询获取系统中 soap 发布信息

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: srt_cfg_dir.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.
DATA: go_display TYPE REF TO data.
FIELD-SYMBOLS <gt_display> TYPE STANDARD TABLE.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.

  SELECT-OPTIONS: s_obj FOR srt_cfg_dir-dt_obj_name.

SELECTION-SCREEN END OF BLOCK blck1.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

*&----------------------------------------------------------------------
*                     At Selection Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

*&----------------------------------------------------------------------
*                     Start Of Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_get_data.

  PERFORM frm_set_layout.
  PERFORM frm_alv_display.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*&  获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .
  TYPES: BEGIN OF ty_hash_filter,
           prop_value TYPE srt_rtc_data-prop_value,
         END OF ty_hash_filter.
  DATA: lt_hash_filter TYPE TABLE OF ty_hash_filter.

  " 查询特定接口

  "SELECT
  "  VEPNAME, " 服务定义
  "  VERSION  " 程序状态
  "  FROM vepheader
  "  WHERE vepname LIKE 'Z%'
  "  INTO TABLE @DATA(lt_header).

  " 配置目录
  SELECT
    cfg~type,            " 类型 CR 消费、SR 服务
    cfg~state,
    cfg~config_name,
    cfg~dt_obj_name,
    cfg~config_name_ext,
    cfg~config_key,
    " -> 时间信息

    " -> 处理函数信息
    fun~function
    FROM srt_cfg_dir AS cfg
    " >> 多对多的信息（也可考虑VEPFUNCST）
    LEFT JOIN vepfunction AS fun ON fun~vepname = cfg~dt_obj_name AND fun~version = cfg~state
    WHERE dt_obj_name IN @s_obj
    INTO TABLE @DATA(lt_srt_cfg).

  IF lt_srt_cfg IS INITIAL.
    MESSAGE 'No found value' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " 配置信息表
  lt_hash_filter = VALUE #( FOR _ IN lt_srt_cfg ( prop_value = _-config_key ) ).
  SELECT
    binding_key,
    prop_value
    FROM srt_rtc_data
    FOR ALL ENTRIES IN @lt_hash_filter
    WHERE prop_value = @lt_hash_filter-prop_value
      AND prop_name = `ConfigKey`
    INTO TABLE @DATA(lt_hashkey_map).
  SORT lt_hashkey_map BY prop_value.

  IF lt_hashkey_map IS NOT INITIAL.
    SELECT
      binding_key,
      prop_name,
      prop_value
      FROM srt_rtc_data
      FOR ALL ENTRIES IN @lt_hashkey_map
      WHERE binding_key = @lt_hashkey_map-binding_key
        AND prop_name LIKE 'AuthenticationMethod%'
      INTO TABLE @DATA(lt_cfg_data).
    SORT lt_cfg_data BY binding_key.
  ENDIF.

  " 发布链接
  SELECT
    binding_key,
    url
    FROM srt_cfg_srv_asgn
    FOR ALL ENTRIES IN @lt_hashkey_map
    WHERE binding_key = @lt_hashkey_map-binding_key
    INTO TABLE @DATA(lt_cfg_srv).
  SORT lt_cfg_srv BY binding_key.

  " 请求链接
  IF line_exists( lt_srt_cfg[ type = 'CR' ] ).
    SELECT
      binding_key,
      prop_name,
      prop_value
      FROM srt_rtc_data
      FOR ALL ENTRIES IN @lt_hashkey_map
      WHERE binding_key = @lt_hashkey_map-binding_key
        AND prop_name LIKE `URL%`
      INTO TABLE @DATA(lt_cfg_data_cr).
    SORT lt_cfg_data_cr BY binding_key.
  ENDIF.

  " 行列转换并显示
  PERFORM frm_set_fieldcat.

  cl_alv_table_create=>create_dynamic_table(
    EXPORTING
      it_fieldcatalog = gt_fieldcat
    IMPORTING
    ep_table = go_display ).

  ASSIGN go_display->* TO <gt_display>.

  DATA: lv_filename TYPE string,
        lv_url      TYPE string.

  LOOP AT lt_srt_cfg INTO DATA(ls_srt_cfg).

    APPEND INITIAL LINE TO <gt_display> ASSIGNING FIELD-SYMBOL(<ls_display>).

    MOVE-CORRESPONDING ls_srt_cfg TO <ls_display>.

    " 补充
    READ TABLE lt_hashkey_map INTO DATA(ls_hashkey_map) WITH KEY prop_value = CONV srt_rtc_data-prop_value( ls_srt_cfg-config_key ) BINARY SEARCH.
    IF sy-subrc = 0.

      READ TABLE lt_cfg_data TRANSPORTING NO FIELDS WITH KEY binding_key = ls_hashkey_map-binding_key BINARY SEARCH.
      IF sy-subrc = 0.

        LOOP AT lt_cfg_data INTO DATA(ls_cfg_data) FROM sy-tabix.
          IF ls_cfg_data-binding_key <> ls_hashkey_map-binding_key.
            EXIT.
          ENDIF.

          CASE ls_cfg_data-prop_name.
            WHEN `AuthenticationMethod`.
              lv_filename = 'AUTH'.
            WHEN OTHERS.
              CLEAR lv_filename.

              FIND REGEX `Username|UNM` IN ls_cfg_data-prop_name.
              IF sy-subrc = 0.
                lv_filename = 'AUTHUNM'.
              ENDIF.

              FIND REGEX `Password|Pwd` IN ls_cfg_data-prop_name.
              IF sy-subrc = 0.
                lv_filename = 'AUTHPWD'.
                ls_cfg_data-prop_value = '-'.
              ENDIF.

          ENDCASE.

          ASSIGN COMPONENT lv_filename OF STRUCTURE <ls_display> TO FIELD-SYMBOL(<lv_value>).
          IF sy-subrc = 0.
            <lv_value> = ls_cfg_data-prop_value.
          ENDIF.

        ENDLOOP.

      ENDIF.

      " 服务url
      READ TABLE lt_cfg_srv INTO DATA(ls_cfg_srv) WITH KEY binding_key = ls_hashkey_map-binding_key BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_cfg_srv TO <ls_display>.
      ENDIF.

      " 消费url
      CHECK ls_srt_cfg-type = 'CR'.
      lv_url = `&1&2&3&4&5`.

      READ TABLE lt_cfg_data_cr TRANSPORTING NO FIELDS WITH KEY binding_key = ls_hashkey_map-binding_key BINARY SEARCH.
      IF sy-subrc = 0.

        LOOP AT lt_cfg_data_cr INTO DATA(ls_cfg_data_cr) FROM sy-tabix.
          IF ls_cfg_data_cr-binding_key <> ls_hashkey_map-binding_key.
            EXIT.
          ENDIF.

          CHECK ls_cfg_data_cr-prop_value IS NOT INITIAL.

          CASE ls_cfg_data_cr-prop_name.
            WHEN `URLHost`.
              REPLACE `&2` IN lv_url WITH ls_cfg_data_cr-prop_value.
            WHEN `URLLanguage`.
              REPLACE `&5` IN lv_url WITH |?sap-language={ ls_cfg_data_cr-prop_value }|.
            WHEN `URLPath`.
              REPLACE `&4` IN lv_url WITH ls_cfg_data_cr-prop_value.
            WHEN `URLPort`.
              REPLACE `&3` IN lv_url WITH |:{ ls_cfg_data_cr-prop_value }|.
            WHEN `URLProtocol`.
              REPLACE `&1` IN lv_url WITH |{ ls_cfg_data_cr-prop_value }://|.
            WHEN OTHERS.
          ENDCASE.

        ENDLOOP.

        REPLACE ALL OCCURRENCES OF REGEX `&\d` IN lv_url WITH ''.

        ASSIGN COMPONENT 'URL' OF STRUCTURE <ls_display> TO FIELD-SYMBOL(<lv_url>).
        IF sy-subrc = 0.
          <lv_url> = lv_url.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*&  设置 LAYOUT
*&---------------------------------------------------------------------*
FORM frm_set_layout .

  gs_layout = VALUE #(
    zebra      = 'X'
    cwidth_opt = 'X'
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'TYPE'            'SRT_CFG_DIR'  'TYPE'            ' ' '类型'(001).       "
  PERFORM frm_set_fcat USING 'STATE'           'SRT_CFG_DIR'  'STATE'           ' ' '状态'(002).       "
  PERFORM frm_set_fcat USING 'CONFIG_NAME'     'SRT_CFG_DIR'  'CONFIG_NAME'     ' ' '配置名'(003).     "
  PERFORM frm_set_fcat USING 'DT_OBJ_NAME'     'SRT_CFG_DIR'  'DT_OBJ_NAME'     ' ' '对象名'(004).     "
  PERFORM frm_set_fcat USING 'CONFIG_NAME_EXT' 'SRT_CFG_DIR'  'CONFIG_NAME_EXT' 'X' '对象名扩展'(005). "
  PERFORM frm_set_fcat USING 'CONFIG_KEY'      'SRT_CFG_DIR'  'CONFIG_KEY'      'X' '配置key'(006).    "
  PERFORM frm_set_fcat USING 'FUNCTION'        'VEPFUNCTION'  'FUNCTION'        ' ' '处理方法'(007).   "
  PERFORM frm_set_fcat USING 'AUTH'            'SRT_RTC_DATA' 'PROP_VALUE'      ' ' '认证方式'(008).   "
  PERFORM frm_set_fcat USING 'AUTHUNM'         'SRT_RTC_DATA' 'PROP_VALUE'      ' ' '认证账号'(009).   "
  PERFORM frm_set_fcat USING 'AUTHPWD'         'SRT_RTC_DATA' 'PROP_VALUE'      ' ' '认证密码'(010).   "
  PERFORM frm_set_fcat USING 'URL'             'SRT_CFG_SRV_ASGN' 'URL'         ' ' 'URL'(011).        "

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FCAT
*&---------------------------------------------------------------------*
*&  设置fcat
*&---------------------------------------------------------------------*
FORM frm_set_fcat USING   p_fieldname
                          p_ref_table TYPE lvc_s_fcat-ref_table
                          p_ref_field TYPE lvc_s_fcat-ref_field
                          p_noout
                          p_coltext.
  DATA: lv_filedname TYPE lvc_s_fcat-fieldname,
        lv_coltext   TYPE lvc_s_fcat-coltext.
  DATA: lv_edit TYPE lvc_s_fcat-edit.

  lv_filedname = p_fieldname.
  lv_coltext   = p_coltext.

  APPEND VALUE lvc_s_fcat( fieldname = lv_filedname
                           coltext   = p_coltext
                           ref_table = p_ref_table
                           ref_field = p_ref_field
                           no_out    = p_noout
                           scrtext_l = p_coltext
                           scrtext_m = p_coltext
                           scrtext_s = p_coltext ) TO gt_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_alv_display
*&---------------------------------------------------------------------*
*&  ALV 展示
*&---------------------------------------------------------------------*
FORM frm_alv_display .
  DATA: lt_events TYPE slis_t_event WITH HEADER LINE.
  DATA: ls_variant TYPE disvariant.
  DATA: lv_lines TYPE i.
  DATA: lt_sort TYPE lvc_t_sort.

  ls_variant-report = sy-repid.
  ls_variant-handle = '00'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_save                  = 'X'
      i_default               = 'X'
      is_variant              = ls_variant
      i_callback_program      = sy-repid
      i_callback_user_command = 'FRM_USER_COMMAND'
      is_layout_lvc           = gs_layout
      it_fieldcat_lvc         = gt_fieldcat
      it_events               = lt_events[]
      it_sort_lvc             = lt_sort
    TABLES
      t_outtab                = <gt_display> " gt_display
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

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

  " CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
  "   IMPORTING
  "     e_grid = lr_grid.
  "
  " lr_grid->check_changed_data( ).

  CASE p_ucomm.
    WHEN '&IC1'.
      MESSAGE ps_selfield-fieldname TYPE 'S'.
    WHEN OTHERS.
  ENDCASE.

  " ps_selfield-refresh = 'X'.
  " ps_selfield-col_stable = 'X' .
  " ps_selfield-row_stable = 'X' .
ENDFORM.