REPORT zakit_demo_file_server.

" 服务器文件上载示例

" 需配置 AL11 文件上载路径
" 需配置 SM49 维护创建文件夹命令 ZMKDIR ANYOS

" 仅展示逻辑，不可执行

" 文件上载

START-OF-SELECTION.

  MESSAGE '仅展示逻辑，不可执行' TYPE 'I' DISPLAY LIKE 'W'.
  STOP.

  PERFORM frm_upload_file.

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

  DATA: lv_filename  TYPE string,
        lv_extension TYPE string.
  DATA: lv_regex TYPE string.

  " 获取文件名称和文件扩展名
  FIND FIRST OCCURRENCE OF '.' IN lv_path.
  IF sy-subrc = 0.
    lv_regex = '(.*)\.(\w*)$'.
    FIND REGEX lv_regex IN lv_path SUBMATCHES lv_filename lv_extension.
  ELSE.
    lv_filename = lv_path.
  ENDIF.

  DATA(ls_result) = zcl_akit_file_server=>upload(
    EXPORTING
      filename  = lv_filename
      filepath  = ''
      extension = lv_extension
      content   = lv_file_xstring
   ).
  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.
