*&---------------------------------------------------------------------*
* 程序名   : ZAKIT_FILE_REPORT
* 开发者   : SL
* 事务代码 :
* 程序类型 : 功能
* 描述     : xx-xxx 文件明细查询
* 修正     :
*&---------------------------------------------------------------------*
* 000   2022/08/25        SL         -             新增
* 版本  修改日期      修改人员    修改请求号     修改原因
*&---------------------------------------------------------------------*
REPORT zakit_file_report.

* 文件报表
* 文件查询报表 基于存储内容 ZTXX_SERVERFILE

* WARNNING 按钮逻辑未测试

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: ztxx_serverfile.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_display.
         INCLUDE TYPE ztxx_serverfile.
TYPES:   box TYPE char1,
       END OF ty_display.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.

DATA: gt_display TYPE TABLE OF ty_display.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECT-OPTIONS: s_cdate FOR ztxx_serverfile-create_date,
                  s_ctime FOR ztxx_serverfile-create_time,
                  s_udate FOR ztxx_serverfile-change_date,
                  s_utime FOR ztxx_serverfile-change_time,
                  s_cuser FOR ztxx_serverfile-create_user.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT +0(20) TEXT-c01 FOR FIELD p_disdle.
    SELECTION-SCREEN POSITION 33.
    PARAMETERS: p_disdle AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck2.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

*&----------------------------------------------------------------------
*                     Value-Request
*&----------------------------------------------------------------------


*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM frm_get_data.

  PERFORM frm_set_fieldcat.
  PERFORM frm_set_layout.
  PERFORM frm_alv_display.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*&  获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA: lt_range_delete TYPE RANGE OF ztxx_serverfile-delete_flag.

  IF p_disdle = ''.
    lt_range_delete = VALUE #( sign = 'I' option = 'EQ' ( low = 'X' ) ).
  ENDIF.

  SELECT
    *
    FROM ztxx_serverfile
    WHERE create_date IN @s_cdate
      AND create_time IN @s_ctime
      AND change_date IN @s_udate
      AND change_time IN @s_utime
      AND create_user IN @s_cuser
      AND delete_flag IN @lt_range_delete
    INTO CORRESPONDING FIELDS OF TABLE @gt_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'GUID'             'ZTXX_SERVERFILE' 'GUID'             TEXT-001. " 全局唯一标识符
  PERFORM frm_set_fcat USING 'ALIASS'           'ZTXX_SERVERFILE' 'ALIASS'           TEXT-002. " AL11 配置名
  PERFORM frm_set_fcat USING 'FILE_NAME'        'ZTXX_SERVERFILE' 'FILE_NAME'        TEXT-003. " 文件名称
  PERFORM frm_set_fcat USING 'FILE_EXTENSION'   'ZTXX_SERVERFILE' 'FILE_EXTENSION'   TEXT-004. " 后缀
  PERFORM frm_set_fcat USING 'FILE_LENGTH'      'ZTXX_SERVERFILE' 'FILE_LENGTH'      TEXT-005. " 文件大小
  PERFORM frm_set_fcat USING 'FILE_PATH'        'ZTXX_SERVERFILE' 'FILE_PATH'        TEXT-006. " 文件路径
  PERFORM frm_set_fcat USING 'FILE_ROOTPATH'    'ZTXX_SERVERFILE' 'FILE_ROOTPATH'    TEXT-007. " 文件根目录
  PERFORM frm_set_fcat USING 'FILE_SUBPATH'     'ZTXX_SERVERFILE' 'FILE_SUBPATH'     TEXT-008. " 文件子路径
  PERFORM frm_set_fcat USING 'FILE_FULLNAME'    'ZTXX_SERVERFILE' 'FILE_FULLNAME'    TEXT-009. " 文件全名
  PERFORM frm_set_fcat USING 'UPLOAD_FILE_NAME' 'ZTXX_SERVERFILE' 'UPLOAD_FILE_NAME' TEXT-010. " 文件名称
  PERFORM frm_set_fcat USING 'MIME_TYPE'        'ZTXX_SERVERFILE' 'MIME_TYPE'        TEXT-011. " 文件类型
  PERFORM frm_set_fcat USING 'DELETE_FLAG'      'ZTXX_SERVERFILE' 'DELETE_FLAG'      TEXT-012. " 删除标识
  PERFORM frm_set_fcat USING 'CREATE_DATE'      'ZTXX_SERVERFILE' 'CREATE_DATE'      TEXT-013. " 创建日期
  PERFORM frm_set_fcat USING 'CREATE_TIME'      'ZTXX_SERVERFILE' 'CREATE_TIME'      TEXT-014. " 创建时间
  PERFORM frm_set_fcat USING 'CREATE_USER'      'ZTXX_SERVERFILE' 'CREATE_USER'      TEXT-015. " 创建人
  PERFORM frm_set_fcat USING 'CHANGE_DATE'      'ZTXX_SERVERFILE' 'CHANGE_DATE'      TEXT-016. " 修改日期
  PERFORM frm_set_fcat USING 'CHANGE_TIME'      'ZTXX_SERVERFILE' 'CHANGE_TIME'      TEXT-017. " 修改时间
  PERFORM frm_set_fcat USING 'CHANGE_USER'      'ZTXX_SERVERFILE' 'CHANGE_USER'      TEXT-018. " 最后修改人

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

  IF lv_filedname = 'GUID'.
    <ls_fieldcat>-key = 'X'.
  ENDIF.
  IF lv_filedname = 'ICON'.
    <ls_fieldcat>-icon = 'X'.
  ENDIF.
  IF lv_filedname = 'DELETE_FLAG'.
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
    WHEN 'DELETE'.
      PERFORM frm_delete_file.
    WHEN 'UPLOAD'.
      PERFORM frm_upload_file.
    WHEN 'DOWNLOAD'.
      PERFORM frm_download_file.
    WHEN OTHERS.
  ENDCASE.

  ps_selfield-refresh = 'X'.
  ps_selfield-col_stable = 'X' .
  ps_selfield-row_stable = 'X' .
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

  lv_lines = lines( gt_display ).
  IF lv_lines <> 0.
    MESSAGE s008(zxx01) WITH lv_lines. " 查到 & 条数据
  ENDIF.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_save                   = 'X'
      i_default                = 'X'
      is_variant               = ls_variant
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
      it_events                = lt_events[]
    TABLES
      t_outtab                 = gt_display
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_delete_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_delete_file .

  DATA: lv_icon     TYPE char4,
        lv_text     TYPE text10,
        lv_icontext TYPE char40.
  DATA: lt_display TYPE TABLE OF ty_display,
        ls_display TYPE ty_display.
  DATA: lv_debug TYPE c VALUE ''.

  IF NOT line_exists( gt_display[ box = 'X' ] ).
    MESSAGE '请选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lv_debug = ''.

    MOVE gt_display TO lt_display.
    DELETE lt_display WHERE box = ''.
    IF lines( lt_display ) > 1.
      MESSAGE '只允许选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    READ TABLE lt_display INTO ls_display INDEX 1.
    IF sy-subrc <> 0.
      MESSAGE '请选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(ls_result) = zcl_akit_file_server=>delete( guid = ls_display-guid ).
    IF ls_result-type = 'S'.
      lv_icon = icon_led_green.
      MESSAGE ls_result-message TYPE 'S'.
    ELSE.
      lv_icon = icon_led_red.
      MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    lv_text = ls_result-message.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = lv_icon
        text                  = lv_text
      IMPORTING
        result                = lv_icontext
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.

    MESSAGE lv_icontext TYPE 'I'.
  ELSE.
    DATA: lv_count TYPE i.

    LOOP AT gt_display INTO ls_display WHERE box = 'X'.
      ADD 1 TO lv_count.
    ENDLOOP.

    LOOP AT gt_display INTO ls_display WHERE box = 'X'.
      ls_result = zcl_akit_file_server=>delete( guid = ls_display-guid ).
      IF ls_result-type = 'S'.
        lv_icon = icon_led_green.
        MESSAGE ls_result-message TYPE 'S'.
      ELSE.
        lv_icon = icon_led_red.
        MESSAGE ls_result-message TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_upload_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_upload_file .

  DATA: lv_path TYPE rlgrap-filename.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
      mask      = '*'
    CHANGING
      file_name = lv_path.
  TYPES: ty_hex TYPE x LENGTH 255.

  DATA: lv_file_name    TYPE string,
        lt_file_data    TYPE TABLE OF ty_hex WITH DEFAULT KEY,
        lv_file_xstring TYPE xstring,
        lv_length       TYPE i.

  lv_file_name = lv_path.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file_name
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_length
    TABLES
      data_tab                = lt_file_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CONCATENATE LINES OF lt_file_data INTO lv_file_xstring IN BYTE MODE.

  IF lv_length > 0.
    lv_file_xstring = lv_file_xstring( lv_length ).
  ENDIF.

  DATA: lv_icon     TYPE char4,
        lv_text     TYPE text10,
        lv_icontext TYPE char40.
  DATA: ls_display TYPE ty_display.

  DATA: lv_filename  TYPE string,
        lv_extension TYPE string.
  DATA: lv_regex TYPE string.

  " 从完整路径提取文件名，再拆分名称与扩展名
  lv_filename = lv_path.
  REPLACE ALL OCCURRENCES OF REGEX '^.*[/\\]' IN lv_filename WITH ''.

  FIND FIRST OCCURRENCE OF '.' IN lv_filename.
  IF sy-subrc = 0.
    lv_regex = '(.*)\.(\w*)$'.
    FIND REGEX lv_regex IN lv_filename SUBMATCHES lv_filename lv_extension.
  ENDIF.

  DATA(ls_result) = zcl_akit_file_server=>upload( filename = lv_filename extension = lv_extension content = lv_file_xstring ).
  IF ls_result-type = 'S'.
    lv_icon = icon_led_green.
    MESSAGE ls_result-message TYPE 'S'.

    SELECT SINGLE * FROM ztxx_serverfile WHERE guid = @ls_result-guid INTO CORRESPONDING FIELDS OF @ls_display.
    APPEND ls_display TO gt_display.
  ELSE.
    lv_icon = icon_led_red.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  lv_text = ls_result-message.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = lv_icon
      text                  = lv_text
    IMPORTING
      result                = lv_icontext
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

  MESSAGE lv_icontext TYPE 'I'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_download_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_download_file .

  DATA: lv_icon     TYPE char4,
        lv_text     TYPE text10,
        lv_icontext TYPE char40.
  DATA: lt_display TYPE TABLE OF ty_display,
        ls_display TYPE ty_display.

  IF NOT line_exists( gt_display[ box = 'X' ] ).
    MESSAGE '请选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  MOVE gt_display TO lt_display.
  DELETE lt_display WHERE box = ''.
  IF lines( lt_display ) > 1.
    MESSAGE '只允许选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE lt_display INTO ls_display INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE '请选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(ls_result) = zcl_akit_file_server=>download( guid = ls_display-guid guidown = 'X' ).
  IF ls_result-type = 'S'.
    lv_icon = icon_led_green.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    lv_icon = icon_led_red.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  lv_text = ls_result-message.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = lv_icon
      text                  = lv_text
    IMPORTING
      result                = lv_icontext
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

  MESSAGE lv_icontext TYPE 'I'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_progress
*&---------------------------------------------------------------------*
*&  进度条
*&---------------------------------------------------------------------*
FORM frm_progress  USING p_percentage p_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percentage
      text       = p_text.
ENDFORM.