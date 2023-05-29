CLASS zcl_akit_file_server DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_result,
        " 返回消息
        type    TYPE bapi_mtype,
        message TYPE bapi_msg,
      END OF ty_result .

    CLASS-DATA aliass TYPE char10 .

    CLASS-METHODS gen_guid
      EXPORTING
        !ev_guid TYPE data
        !ev_uuid TYPE data .
    CLASS-METHODS delete
      IMPORTING
        !file_fullname   TYPE data
      RETURNING
        VALUE(rs_result) TYPE ty_result .
    CLASS-METHODS dirhome
      RETURNING
        VALUE(dir) TYPE dirname .
    CLASS-METHODS download
      IMPORTING
        !full_filename   TYPE data
        !guidown         TYPE abap_bool OPTIONAL
      EXPORTING
        !ev_filename     TYPE string
        !ev_extension    TYPE string
        !ev_content      TYPE xstring
      RETURNING
        VALUE(rs_result) TYPE ty_result .
    CLASS-METHODS gen_info
      IMPORTING
        !filename          TYPE data
        !extension         TYPE data OPTIONAL
        !date              TYPE datum DEFAULT sy-datum
      EXPORTING
        !ev_full_name      TYPE data
        !ev_file_name      TYPE data
        !ev_file_extension TYPE data
        !ev_file_path      TYPE data
        !ev_file_root_path TYPE data
        !ev_file_sub_path  TYPE data
      RETURNING
        VALUE(rs_result)   TYPE ty_result .
    CLASS-METHODS mkdir
      IMPORTING
        !dir             TYPE data
      RETURNING
        VALUE(rs_result) TYPE ty_result .
    CLASS-METHODS upload
      IMPORTING
        !filename        TYPE data OPTIONAL
        !filepath        TYPE data OPTIONAL
        !extension       TYPE data OPTIONAL
        !content         TYPE xstring
      EXPORTING
        !ev_filename     TYPE string
        !ev_filelength   TYPE i
      RETURNING
        VALUE(rs_result) TYPE ty_result .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS guidown
      IMPORTING
        !filename   TYPE data
        !filelength TYPE i
        !solix      TYPE solix_tab .
ENDCLASS.



CLASS ZCL_AKIT_FILE_SERVER IMPLEMENTATION.


  METHOD delete.
    " 删除文件

    DATA: lv_file_fullname TYPE string.

    lv_file_fullname = file_fullname.

    IF lv_file_fullname IS INITIAL.
      rs_result = VALUE #( type = 'E' message = '文件名必输' ).
      RETURN.
    ENDIF.

    " 检查文件是否存在
    OPEN DATASET lv_file_fullname FOR INPUT IN BINARY MODE.
    IF sy-subrc = 8.
      rs_result = VALUE #( type = 'S' message = '文件不存在' ).
      RETURN.
    ENDIF.

    " 删除
    TRY.
        DELETE DATASET lv_file_fullname.
      CATCH cx_sy_file_authority  INTO DATA(lo_cx_sy_file_authority).
        rs_result = VALUE #( type = 'E' message = |{ lo_cx_sy_file_authority->get_text( ) }| ).
      CATCH cx_sy_file_open  INTO DATA(lo_cx_sy_file_open).
        rs_result = VALUE #( type = 'E' message = |{ lo_cx_sy_file_open->get_text( ) }| ).
    ENDTRY.

    " 检查文件是否删除
    OPEN DATASET lv_file_fullname FOR INPUT IN BINARY MODE.
    IF sy-subrc = 8.
      rs_result = VALUE #( type = 'S' message = '文件删除成功' ).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD dirhome.
    " 获取文件上载基础路径

    " 借用 AL11 配置点

    " AL11目录
    SELECT SINGLE dirname
      FROM user_dir
      INTO @dir
     WHERE aliass = @zcl_akit_file_server=>aliass. " 'ZAPP_FILE'.

  ENDMETHOD.


  METHOD download.
    " 从服务器下载文件

    DATA: lv_full_filename TYPE string.
    DATA: lv_status   TYPE extcmdexex-status,
          lv_exitcode TYPE extcmdexex-exitcode,
          lt_protocol TYPE STANDARD TABLE OF btcxpm.
    DATA: lv_failed  TYPE abap_bool,
          lv_message TYPE string.
    DATA: lv_max_length    TYPE i VALUE 255,
          lv_actual_length TYPE i,
          lv_file_length   TYPE i,
          ls_solix         TYPE solix,
          lt_solix         TYPE solix_tab.
    DATA: lv_path_separator TYPE dmc_mds_path_separator.
    DATA: lv_file_path TYPE string,
          lv_regex     TYPE string.



    IF full_filename IS INITIAL.
      rs_result = VALUE #( type = 'E' message = '文件名不能为空' ).
      RETURN.
    ENDIF.
    lv_full_filename = full_filename.

    " 判断文件是否存在
    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = 'DIR'
        additional_parameters         = CONV sxpgcolist-parameters( lv_full_filename )
        operatingsystem               = sy-opsys " => 'UNIX'
      IMPORTING
        status                        = lv_status
        exitcode                      = lv_exitcode
      TABLES
        exec_protocol                 = lt_protocol
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc <> 0 OR lv_status <> 'O' OR lv_exitcode <> 0.
      rs_result = VALUE #( type = 'E' message = '文件不存在;' ).
      LOOP AT lt_protocol INTO DATA(ls_protocol).
        rs_result-message = rs_result-message && ls_protocol-message && ';'.
      ENDLOOP.
      RETURN.
    ENDIF.
    CLEAR: lv_status, lv_exitcode, lt_protocol[].

    " 打开文件
    TRY.
        OPEN DATASET lv_full_filename FOR INPUT IN BINARY MODE MESSAGE lv_message.
      CATCH cx_sy_file_open INTO DATA(lx_file_open).
        lv_failed = abap_true.
        lv_message = lx_file_open->get_text( ).
      CATCH cx_sy_codepage_converter_init INTO DATA(lx_codepage_converter_init).
        lv_failed = abap_true.
        lv_message = lx_codepage_converter_init->get_text( ).
      CATCH cx_sy_conversion_codepage INTO DATA(lx_conversion_codepage).
        lv_failed = abap_true.
        lv_message = lx_conversion_codepage->get_text( ).
      CATCH cx_sy_file_authority INTO DATA(lx_file_authority).
        lv_failed = abap_true.
        lv_message = lx_file_authority->get_text( ).
      CATCH cx_sy_pipes_not_supported INTO DATA(lx_pipes_not_supported).
        lv_failed = abap_true.
        lv_message = lx_pipes_not_supported->get_text( ).
      CATCH cx_sy_too_many_files INTO DATA(lx_too_many_files).
        lv_failed = abap_true.
        lv_message = lx_too_many_files->get_text( ).
      CATCH cx_root INTO DATA(lx_root).
        lv_failed = abap_true.
        lv_message = lx_root->get_text( ).
    ENDTRY.

    IF lv_failed IS NOT INITIAL.
      rs_result = VALUE #( type = 'E' message = lv_message ).
      RETURN.
    ENDIF.
    CLEAR: lv_message.

    " 读取文件
    DO.
      lv_max_length = 255.

      TRY.
          READ DATASET lv_full_filename INTO ls_solix-line MAXIMUM LENGTH lv_max_length ACTUAL LENGTH lv_actual_length.
        CATCH cx_sy_codepage_converter_init INTO lx_codepage_converter_init.
          lv_failed = abap_true.
          lv_message = lx_codepage_converter_init->get_text( ).
        CATCH cx_sy_conversion_codepage INTO lx_conversion_codepage.
          lv_failed = abap_true.
          lv_message = lx_conversion_codepage->get_text( ).
        CATCH cx_sy_file_authority INTO lx_file_authority.
          lv_failed = abap_true.
          lv_message = lx_file_authority->get_text( ).
        CATCH cx_sy_file_io INTO DATA(lx_file_io).
          lv_failed = abap_true.
          lv_message = lx_file_io->get_text( ).
        CATCH cx_sy_file_open INTO lx_file_open.
          lv_failed = abap_true.
          lv_message = lx_file_open->get_text( ).
        CATCH cx_sy_file_open_mode INTO DATA(lx_file_open_mode).
          lv_failed = abap_true.
          lv_message = lx_file_open_mode->get_text( ).
        CATCH cx_sy_pipe_reopen INTO DATA(lx_pipe_reopen).
          lv_failed = abap_true.
          lv_message = lx_pipe_reopen->get_text( ).
        CATCH cx_root INTO lx_root.
          lv_failed = abap_true.
          lv_message = lx_root->get_text( ).
      ENDTRY.
      IF lv_failed IS NOT INITIAL.
        rs_result = VALUE #( type = 'E' message = lv_message ).
        EXIT.
      ENDIF.
      IF lv_actual_length IS INITIAL.
        EXIT.
      ENDIF.
      ADD lv_actual_length TO lv_file_length.
      APPEND ls_solix TO lt_solix.
      CLEAR: lv_message, lv_actual_length, ls_solix.
    ENDDO.

    IF lv_failed IS NOT INITIAL.
      RETURN.
    ENDIF.
    CLEAR lv_message.

    " 转换二进制
    IF lt_solix IS NOT INITIAL.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_file_length
        IMPORTING
          buffer       = ev_content
        TABLES
          binary_tab   = lt_solix
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
    ENDIF.

    TRY.
        CLOSE DATASET lv_full_filename.
      CATCH cx_sy_file_close INTO DATA(lx_file_close).
        lv_failed = abap_true.
        lv_message = lx_file_close->get_text( ).
      CATCH cx_root INTO lx_root.
        lv_failed = abap_true.
        lv_message = lx_root->get_text( ).
    ENDTRY.

    IF lv_failed IS NOT INITIAL.
      rs_result = VALUE #( type = 'E' message = '文件下载失败' && lv_message ).
      RETURN.
    ENDIF.

    " 文件名解析
    CALL FUNCTION 'DMC_MDS_GET_PATHSEPARATOR'
      IMPORTING
        ev_path_separator     = lv_path_separator
      EXCEPTIONS
        opsys_not_supported   = 1
        filesys_not_supported = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      lv_path_separator = '/'.
    ENDIF.

    FIND FIRST OCCURRENCE OF lv_path_separator IN lv_full_filename.
    IF sy-subrc = 0.
      lv_regex = '(.*)\' && lv_path_separator && '(.*)'.
      FIND REGEX lv_regex IN lv_full_filename SUBMATCHES lv_file_path ev_filename.
    ELSE.
      ev_filename = lv_full_filename.
    ENDIF.

    FIND FIRST OCCURRENCE OF '.' IN lv_full_filename.
    IF sy-subrc = 0.
      lv_regex = '(.*)\.(.*)'.
      FIND REGEX lv_regex IN ev_filename SUBMATCHES ev_filename ev_extension.
    ELSE.
      ev_filename = lv_full_filename.
    ENDIF.

    IF guidown = abap_true.
      guidown(
        filename   = ev_filename
        filelength = lv_file_length
        solix      = lt_solix
      ).
    ENDIF.

  ENDMETHOD.


  METHOD gen_guid.
    " 生成唯一标识

    CHECK ev_guid IS SUPPLIED OR ev_uuid IS SUPPLIED.

    TRY.
        DATA(lv_uuid) = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
    ENDTRY.

    IF ev_guid IS SUPPLIED.
      ev_guid = lv_uuid.
    ENDIF.

    IF ev_uuid IS SUPPLIED.
      TRY.
          CALL METHOD cl_system_uuid=>convert_uuid_c32_static
            EXPORTING
              uuid     = lv_uuid
            IMPORTING
              uuid_c36 = DATA(lv_uuid_c36).
        CATCH cx_uuid_error.
      ENDTRY.
      ev_uuid = lv_uuid_c36.
      TRANSLATE ev_uuid TO LOWER CASE.
    ENDIF.

  ENDMETHOD.


  METHOD gen_info.
    " 生成文件信息

    DATA:
      lv_file_path      TYPE string,
      lv_path_separator TYPE dmc_mds_path_separator.

    DATA(lv_root_path) = zcl_akit_file_server=>dirhome( ).
    IF lv_root_path IS INITIAL.
      rs_result = VALUE #( type = 'E' message = '文件服务器目录未维护' ).
      RETURN.
    ENDIF.

    " 文件分隔符
    CALL FUNCTION 'DMC_MDS_GET_PATHSEPARATOR'
      IMPORTING
        ev_path_separator     = lv_path_separator
      EXCEPTIONS
        opsys_not_supported   = 1
        filesys_not_supported = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      lv_path_separator = '/'.
    ENDIF.

    " 文件目录
    ev_file_root_path = lv_root_path.
    ev_file_sub_path = COND #( WHEN date IS INITIAL THEN '' ELSE date ).
    ev_file_path = lv_root_path && lv_path_separator && ev_file_sub_path.

    ev_file_name = filename.

    " 文件全名
    ev_full_name = ev_file_path && lv_path_separator && ev_file_name.
    IF extension IS NOT INITIAL.
      ev_file_extension = extension.
      ev_full_name = ev_full_name && '.' && ev_file_extension.
    ENDIF.

  ENDMETHOD.


  METHOD guidown.

    " 选择保存路径
    DATA: lv_default_file_name TYPE string,
          lv_file_name         TYPE string,
          lv_file_path         TYPE string,
          lv_path              TYPE string,
          lv_user_action       TYPE i.
    DATA: lt_solix TYPE solix_tab.

    lv_default_file_name = filename.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title              = `选择文件夹`
        default_file_name         = lv_default_file_name
        prompt_on_overwrite       = abap_true
      CHANGING
        filename                  = lv_file_name
        path                      = lv_path
        fullpath                  = lv_file_path
        user_action               = lv_user_action
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lv_user_action <> cl_gui_frontend_services=>action_ok.
      RETURN.
    ENDIF.

    lt_solix = solix.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize            = filelength
        filename                = lv_file_path
        filetype                = 'BIN'
      CHANGING
        data_tab                = lt_solix
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      MESSAGE '文件保存成功' TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD mkdir.

    " 创建文件夹

    " 需要在 SM49 维护创建文件夹命令 ZMKDIR ANYOS

    DATA: lv_dir TYPE sxpgcolist-parameters.
    DATA: lv_status   TYPE extcmdexex-status,
          lv_exitcode TYPE extcmdexex-exitcode,
          lt_protocol TYPE STANDARD TABLE OF btcxpm.

    lv_dir = dir.
    IF lv_dir IS INITIAL.
      rs_result = VALUE #( type = 'E' message = '文件夹不能为空' ).
      RETURN.
    ENDIF.

    " 判断文件夹是否已经存在
    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = 'DIR'
        additional_parameters         = lv_dir
        operatingsystem               = sy-opsys " 'UNIX'
      IMPORTING
        status                        = lv_status
        exitcode                      = lv_exitcode
      TABLES
        exec_protocol                 = lt_protocol
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc = 0 AND lv_status = 'O' AND lv_exitcode = 0.
      rs_result = VALUE #( type = 'S' message = '文件夹已存在;' ).
      LOOP AT lt_protocol INTO DATA(ls_protocol).
        rs_result-message = rs_result-message && ls_protocol-message && ';'.
      ENDLOOP.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = 'ZMKDIR'
        additional_parameters         = lv_dir
        operatingsystem               = 'ANYOS' " SY-OPSYS
      IMPORTING
        status                        = lv_status
        exitcode                      = lv_exitcode
      TABLES
        exec_protocol                 = lt_protocol
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc = 0 AND lv_status = 'O' AND lv_exitcode = 0.
      rs_result = VALUE #( type = 'S' message = '文件夹创建成功' ).
      RETURN.
    ELSE.
      rs_result = VALUE #( type = 'S' message = '文件夹创建失败;' ).
      LOOP AT lt_protocol INTO ls_protocol.
        rs_result-message = rs_result-message && ls_protocol-message && ';'.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD upload.

    " 文件上载到服务器
    " 使用记录文件上载位置等信息

    DATA: lv_filename TYPE string,
          lv_filepath TYPE string,
          lv_fullname TYPE string.
    DATA: ls_result TYPE ty_result.
    DATA: lv_path_separator TYPE dmc_mds_path_separator.
    DATA: lv_message TYPE string,
          lv_failed  TYPE abap_bool.
    DATA: lt_solix TYPE solix_tab.
    DATA: lv_length TYPE i.

    " 生成 UUID 文件名
    IF filename IS INITIAL.
      gen_guid( IMPORTING ev_uuid = lv_filename ).
    ELSE.
      lv_filename = filename.
    ENDIF.

    " 创建文件夹
    lv_filepath = filepath.
    IF lv_filepath IS NOT INITIAL.
      ls_result = mkdir( dir = lv_filepath ).
    ENDIF.
    IF ls_result-type = 'E'.
      rs_result = ls_result.
      RETURN.
    ENDIF.

    " 文件路径分隔符
    CALL FUNCTION 'DMC_MDS_GET_PATHSEPARATOR'
      IMPORTING
        ev_path_separator     = lv_path_separator
      EXCEPTIONS
        opsys_not_supported   = 1
        filesys_not_supported = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      lv_path_separator = '/'.
    ENDIF.

    " 文件全名
    lv_fullname = lv_filepath && lv_path_separator && lv_filename.
    IF extension IS NOT INITIAL.
      lv_fullname = lv_fullname && '.' && extension.
    ENDIF.

    " 打开文件
    TRY.
        OPEN DATASET lv_fullname FOR OUTPUT IN BINARY MODE MESSAGE lv_message.
      CATCH cx_sy_file_open INTO DATA(lx_file_open).
        lv_failed = abap_true.
        lv_message = lx_file_open->get_text( ).
      CATCH cx_sy_codepage_converter_init INTO DATA(lx_codepage_converter_init).
        lv_failed = abap_true.
        lv_message = lx_codepage_converter_init->get_text( ).
      CATCH cx_sy_conversion_codepage INTO DATA(lx_conversion_codepage).
        lv_failed = abap_true.
        lv_message = lx_conversion_codepage->get_text( ).
      CATCH cx_sy_file_authority INTO DATA(lx_file_authority).
        lv_failed = abap_true.
        lv_message = lx_file_authority->get_text( ).
      CATCH cx_sy_pipes_not_supported INTO DATA(lx_pipes_not_supported).
        lv_failed = abap_true.
        lv_message = lx_pipes_not_supported->get_text( ).
      CATCH cx_sy_too_many_files INTO DATA(lx_too_many_files).
        lv_failed = abap_true.
        lv_message = lx_too_many_files->get_text( ).
      CATCH cx_root INTO DATA(lx_root).
        lv_failed = abap_true.
        lv_message = lx_root->get_text( ).
    ENDTRY.

    IF lv_failed IS NOT INITIAL.
      rs_result = VALUE #( type = 'E' message = lv_message ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = content
      IMPORTING
        output_length = ev_filelength
      TABLES
        binary_tab    = lt_solix.

    ev_filename  = lv_filename.

    " 写文件
    LOOP AT lt_solix INTO DATA(ls_solix).
      DESCRIBE FIELD ls_solix-line LENGTH lv_length IN BYTE MODE.
      IF lv_length > ev_filelength.
        lv_length = ev_filelength.
      ELSE.
        SUBTRACT lv_length FROM ev_filelength.
      ENDIF.
      CHECK lv_length > 0.
      CLEAR lv_message.
      TRY.
          TRANSFER ls_solix-line TO lv_fullname LENGTH lv_length.
        CATCH cx_sy_codepage_converter_init INTO lx_codepage_converter_init.
          lv_failed = abap_true.
          lv_message = lx_codepage_converter_init->get_text( ).
        CATCH cx_sy_conversion_codepage INTO lx_conversion_codepage.
          lv_failed = abap_true.
          lv_message = lx_conversion_codepage->get_text( ).
        CATCH cx_sy_file_authority INTO lx_file_authority.
          lv_failed = abap_true.
          lv_message = lx_file_authority->get_text( ).
        CATCH cx_sy_file_io INTO DATA(lx_file_io).
          lv_failed = abap_true.
          lv_message = lx_file_io->get_text( ).
        CATCH cx_sy_file_open INTO lx_file_open.
          lv_failed = abap_true.
          lv_message = lx_file_open->get_text( ).
        CATCH cx_sy_file_open_mode INTO DATA(lx_file_open_mode).
          lv_failed = abap_true.
          lv_message = lx_file_open_mode->get_text( ).
        CATCH cx_sy_pipe_reopen INTO DATA(lx_pipe_reopen).
          lv_failed = abap_true.
          lv_message = lx_pipe_reopen->get_text( ).
        CATCH cx_sy_too_many_files INTO lx_too_many_files.
          lv_failed = abap_true.
          lv_message = lx_too_many_files->get_text( ).
        CATCH cx_root INTO lx_root.
          lv_failed = abap_true.
          lv_message = lx_root->get_text( ).
      ENDTRY.
      IF lv_failed IS NOT INITIAL.
        rs_result = VALUE #( type = 'E' message = lv_message ).
        EXIT.
      ENDIF.
    ENDLOOP.

    IF rs_result-type = 'E'.
      RETURN.
    ENDIF.

    " 关闭文件
    TRY.
        CLOSE DATASET lv_fullname.
      CATCH cx_sy_file_close INTO DATA(lx_file_close).
        lv_failed = abap_true.
        lv_message = lx_file_close->get_text( ).
      CATCH cx_root INTO lx_root.
        lv_failed = abap_true.
        lv_message = lx_root->get_text( ).
    ENDTRY.

    IF lv_failed IS NOT INITIAL.
      rs_result = VALUE #( type = 'E' message = lv_message ).
      RETURN.
    ENDIF.

    rs_result = VALUE #( type = 'S' message = '文件上传成功' ).

  ENDMETHOD.
ENDCLASS.