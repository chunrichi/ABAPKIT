REPORT zakit_demo_xlsx.

" 关于 AKIT XLSX 程序处理的内容


DATA: lv_xstring TYPE xstring.
DATA: lv_filename TYPE string.

" 上载文件
DATA: lv_path TYPE rlgrap-filename.

CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
  EXPORTING
    static    = 'X'
    mask      = 'xlsx|*.xlsx'
  CHANGING
    file_name = lv_path.

CHECK lv_path IS NOT INITIAL.

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
CONCATENATE LINES OF lt_file_data INTO lv_xstring IN BYTE MODE.

zcl_akit_xlsx=>upload( content = lv_xstring ).
