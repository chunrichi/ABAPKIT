REPORT zdemo_cust_table.

" 自建表查询与批导

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------

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

  PARAMETERS: p_srch RADIOBUTTON GROUP zud USER-COMMAND zud DEFAULT 'X',
              p_down RADIOBUTTON GROUP zud,
              p_updw RADIOBUTTON GROUP zud.

SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  PARAMETERS: p_tabl TYPE dd02l-tabname .
  PARAMETERS: p_maxl TYPE syst_tabix DEFAULT 500 MODIF ID lim OBLIGATORY.

  PARAMETERS: p_path TYPE rlgrap-filename MODIF ID pth.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN FUNCTION KEY 1.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

  %_p_srch_%_app_%-text = '查询'(t01).
  %_p_down_%_app_%-text = '下载'(t02).
  %_p_updw_%_app_%-text = '上载'(t03).
  %_p_tabl_%_app_%-text = '表名'(t04).
  %_p_maxl_%_app_%-text = '表行'(t05).
  %_p_path_%_app_%-text = '文件名'(t06).

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF ( p_srch = 'X' OR p_down = 'X' ) AND screen-group1 = 'PTH'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF p_down = 'X' AND screen-group1 = 'LIM'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF p_updw = 'X' AND ( screen-group1 = 'RAD' OR screen-group1 = 'LIM' ).
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'P_PATH' OR screen-name = 'P_TABL'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&----------------------------------------------------------------------
*                     At Selection Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
      mask      = 'Upload Excel|*.xlsx'(002)
    CHANGING
      file_name = p_path.

*&----------------------------------------------------------------------
*                     Start Of Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_check_table.
  PERFORM frm_gen_strcut.

  IF p_srch = 'X'.
    PERFORM frm_get_data.
  ELSEIF p_down = 'X'.
    PERFORM frm_download_template.
  ELSE.
    PERFORM frm_upload_data.
  ENDIF.

  PERFORM frm_set_layout.
  PERFORM frm_alv_display.

*&---------------------------------------------------------------------*
*& Form FRM_CHECK_TABLE
*&---------------------------------------------------------------------*
*&  检查表是否存在
*&---------------------------------------------------------------------*
FORM frm_check_table .

  SELECT COUNT(*) FROM dd02l WHERE tabname = p_tabl.
  IF sy-subrc <> 0.
    MESSAGE 'Table not exists'(003) TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF NOT ( p_tabl+0(1) = 'Z' OR p_tabl+0(1) = 'Y' ).
    MESSAGE 'Just can change cust table'(004) TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GEN_STRCUT
*&---------------------------------------------------------------------*
*&  生成结构
*&---------------------------------------------------------------------*
FORM frm_gen_strcut .

  PERFORM frm_set_fieldcat.

  cl_alv_table_create=>create_dynamic_table(
    EXPORTING
      it_fieldcatalog = gt_fieldcat
    IMPORTING
    ep_table = go_display ).

  ASSIGN go_display->* TO <gt_display>.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*&  获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .

  IF p_tabl IS INITIAL.
    MESSAGE s002(00) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " 查询数据
  SELECT
    *
    FROM (p_tabl)
    INTO CORRESPONDING FIELDS OF TABLE @<gt_display>
    UP TO @p_maxl ROWS BYPASSING BUFFER.

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

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_tabl
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Get table fieldcat failed'(005) TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FCAT
*&---------------------------------------------------------------------*
*&  设置fcat
*&---------------------------------------------------------------------*
FORM frm_set_fcat USING   p_fieldname
                          p_ref_table TYPE lvc_s_fcat-ref_table
                          p_ref_field TYPE lvc_s_fcat-ref_field
                          p_coltext
                          p_noout.
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
    WHEN 'SAVE'.
      PERFORM frm_btn_save.
    WHEN OTHERS.
  ENDCASE.

  " ps_selfield-refresh = 'X'.
  " ps_selfield-col_stable = 'X' .
  " ps_selfield-row_stable = 'X' .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*&  下载XLSX
*&---------------------------------------------------------------------*
FORM frm_download_template .
  DATA: lt_fieldcat LIKE gt_fieldcat.

  DATA: lv_default_file_name TYPE string,
        lv_file_name         TYPE string,
        lv_file_path         TYPE string,
        lv_path              TYPE string,
        lv_user_action       TYPE i.

  IF p_tabl IS INITIAL.
    MESSAGE s002(00) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  lv_default_file_name = p_tabl && '.xlsx'.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title              = CONV #( '选择文件夹'(006) )
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
    STOP.
  ENDIF.

  MOVE-CORRESPONDING gt_fieldcat TO lt_fieldcat.

  DELETE lt_fieldcat WHERE fieldname = 'MANDT'.

  MODIFY lt_fieldcat FROM VALUE #( ref_table = '' ref_field = '' ) TRANSPORTING ref_table ref_field WHERE fieldname <> ''.

  " 补全部分描述
  IF line_exists( lt_fieldcat[ reptext = '' ] ).

    SELECT
      tabname,
      fieldname,
      ddlanguage,
      ddtext
      FROM dd03t AS 3t
      WHERE tabname = @p_tabl
        AND as4local = 'A'
        AND ddlanguage IN ('1','E')
      INTO TABLE @DATA(lt_dd03t).
    SORT lt_dd03t BY tabname fieldname ddlanguage.
    DELETE ADJACENT DUPLICATES FROM lt_dd03t COMPARING tabname fieldname.

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WHERE reptext IS INITIAL.

      " 补充描述
      READ TABLE lt_dd03t INTO DATA(ls_dd03t) WITH KEY tabname = p_tabl fieldname = <ls_fieldcat>-fieldname BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_fieldcat>-reptext = ls_dd03t-ddtext.
      ENDIF.

      " 描述补充
      IF <ls_fieldcat>-reptext IS INITIAL.
        <ls_fieldcat>-reptext = <ls_fieldcat>-scrtext_m.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DATA: l_length     TYPE i,
        l_xml_stream TYPE xml_rawdata,
        lv_flavour   TYPE string,
        lv_version   TYPE string.
  DATA: lr_result_data TYPE REF TO cl_salv_ex_result_data_table.
  DATA: ir_data_ref TYPE REF TO data,
        lv_xstring  TYPE xstring.

  TRY.

      lr_result_data = cl_salv_ex_util=>factory_result_data_table(
        r_data                      = go_display
        t_fieldcatalog              = lt_fieldcat
      ).

      CASE cl_salv_bs_a_xml_base=>get_version( ).
        WHEN if_salv_bs_xml=>version_25.
          lv_version = if_salv_bs_xml=>version_25.
        WHEN if_salv_bs_xml=>version_26.
          lv_version = if_salv_bs_xml=>version_26. " = 2.6
      ENDCASE.


      lv_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export. "Flavor for Complete ALV XML

      CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
        EXPORTING
          xml_type      = if_salv_bs_xml=>c_type_xlsx  "XLSX
          xml_version   = lv_version
          r_result_data = lr_result_data
          xml_flavour   = lv_flavour
          gui_type      = if_salv_bs_xml=>c_gui_type_gui  "Y6DK066330
        IMPORTING
          xml           = lv_xstring.

    CATCH cx_root.
      CLEAR lv_xstring.
  ENDTRY.

  DATA: lv_size TYPE i.
  DATA: lt_bintab TYPE solix_tab.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_xstring
    IMPORTING
      output_length = lv_size
    TABLES
      binary_tab    = lt_bintab.


  cl_gui_frontend_services=>gui_download(
    EXPORTING
      bin_filesize              = lv_size
      filename                  = lv_file_path
      filetype                  = 'BIN'
    CHANGING
      data_tab                  = lt_bintab
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24
         ).

  STOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&  上载数据
*&---------------------------------------------------------------------*
FORM frm_upload_data .

  DATA: lt_excel TYPE STANDARD TABLE OF alsmex_tabline.
  DATA: lv_skip_field TYPE i.
  DATA: lo_display TYPE REF TO data.

  IF line_exists( gt_fieldcat[ fieldname = 'MANDT' ] ).
    lv_skip_field = lv_skip_field + 1.
  ENDIF.

  CREATE DATA lo_display LIKE LINE OF <gt_display>.
  ASSIGN lo_display->* TO FIELD-SYMBOL(<ls_display>).

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_path
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = lines( gt_fieldcat )
      i_end_row               = 20006
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE '文件不存在或已被用户取消'(007) TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  LOOP AT lt_excel INTO DATA(ls_excel).
    ls_excel-col = ls_excel-col + lv_skip_field.

    ASSIGN COMPONENT ls_excel-col OF STRUCTURE <ls_display> TO FIELD-SYMBOL(<lv_value>).
    IF sy-subrc = 0.

      DESCRIBE FIELD <lv_value> TYPE DATA(lv_ftype).
      CASE lv_ftype.
        WHEN 'D'.
          " 日期
          PERFORM frm_filter_date USING <lv_value> ls_excel-value.
        WHEN 'T'.
          " 时间
          PERFORM frm_filter_time USING <lv_value> ls_excel-value.
        WHEN OTHERS.
          DESCRIBE FIELD <lv_value> EDIT MASK DATA(l_mask).
          IF l_mask IS NOT INITIAL.
            PERFORM frm_generalize_conversion USING l_mask ls_excel-value <lv_value>.
          ELSE.
            TRY.
                <lv_value> = ls_excel-value.
              CATCH cx_sy_conversion_no_number."非数值

            ENDTRY.
          ENDIF.
      ENDCASE.

      CLEAR l_mask.
    ENDIF.
    AT END OF row.
      APPEND <ls_display> TO <gt_display>.
      CLEAR <ls_display>.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FILTER_DATE
*&---------------------------------------------------------------------*
*&  日期
*&---------------------------------------------------------------------*
FORM frm_filter_date USING p_date p_value.
  DATA: lv_year  TYPE string,
        lv_month TYPE string,
        lv_date  TYPE string,
        lv_no(8) TYPE n.

  FIND REGEX '\D' IN p_value.
  IF sy-subrc = 0.
    SPLIT p_value AT '.' INTO lv_year lv_month lv_date.
    IF lv_month IS INITIAL AND lv_date IS INITIAL.
      SPLIT p_value AT '/' INTO lv_year lv_month lv_date.
      IF lv_month IS INITIAL AND lv_date IS INITIAL.
        SPLIT p_value AT '-' INTO lv_year lv_month lv_date.
      ENDIF.
    ENDIF.

    " 年份置后日期格式
    IF strlen( lv_date ) = 4.
      lv_no+0(4) = lv_date.
      lv_no+4(2) = lv_year.
      lv_no+6(2) = lv_month.
    ELSE.
      lv_no+0(4) = lv_year.
      lv_no+4(2) = lv_month.
      lv_no+6(2) = lv_date.
    ENDIF.

    p_date = lv_no.
  ELSE.
    p_date = p_value.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_filter_time
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM frm_filter_time USING p_time p_value.
  DATA: lv_h TYPE string,
        lv_m TYPE string,
        lv_s TYPE string.

  FIND REGEX '\D' IN p_value.
  IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF '\D' IN p_value WITH ''.

    p_time = p_value.
  ELSE.
    p_time = p_value.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_generalize_conversion
*&---------------------------------------------------------------------*
*&  通用转换
*&---------------------------------------------------------------------*
FORM frm_generalize_conversion  USING p_mask
                                      p_value
                                      p_target.
  DATA: lv_rule TYPE string.

  REPLACE FIRST OCCURRENCE OF '==' IN p_mask WITH ''.
  CONDENSE p_mask NO-GAPS.

  lv_rule = |CONVERSION_EXIT_{ p_mask }_INPUT|.

  CALL FUNCTION lv_rule
    EXPORTING
      input         = p_value
    IMPORTING
      output        = p_target
    EXCEPTIONS
      error_message = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    p_target = p_value.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_BTN_SAVE
*&---------------------------------------------------------------------*
*&  保存
*&---------------------------------------------------------------------*
FORM frm_btn_save .
  DATA: lr_table TYPE REF TO data.
  FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                 <lv_field> TYPE any.

  CREATE DATA lr_table TYPE TABLE OF (p_tabl).
  ASSIGN lr_table->* TO <lt_table>.

  MOVE-CORRESPONDING <gt_display> TO <lt_table>.

  MODIFY (p_tabl) FROM TABLE <lt_table>.
  IF sy-subrc = 0.
    MESSAGE 'SUCCESS' TYPE 'S'.
    COMMIT WORK AND WAIT.
  ELSE.
    MESSAGE 'ERROR' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.