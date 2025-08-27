REPORT zakit_tabva_client_copy.

" 跨 client copy table value

" 注意: 本程序主要依赖过期语法 `CLIENT SPECIFIED` 执行，该语法不支持 cds 视图及 cds 作为替换对象的表

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gt_fieldcat TYPE lvc_t_fcat.
DATA: go_tablev TYPE REF TO data.
FIELD-SYMBOLS <gt_tablev> TYPE STANDARD TABLE.

*&----------------------------------------------------------------------
*                     Selection Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  PARAMETERS: p_tabv TYPE dd02l-tabname OBLIGATORY MEMORY ID dtb.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.

  PARAMETERS: p_sclt TYPE sy-mandt OBLIGATORY.
  PARAMETERS: p_tclt TYPE sy-mandt OBLIGATORY DEFAULT sy-mandt.

SELECTION-SCREEN END OF BLOCK blck2.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

  %_p_tabv_%_app_%-text = '表名'(t01).
  %_p_sclt_%_app_%-text = '源集团'(t02).
  %_p_tclt_%_app_%-text = '目标集团'(t03).

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_process_data.

*&---------------------------------------------------------------------*
*& Form frm_process_data
*&---------------------------------------------------------------------*
*&  处理数据
*&---------------------------------------------------------------------*
FORM frm_process_data .

  SELECT SINGLE
    tabname
    FROM dd02l
    WHERE tabclass = 'TRANSP'
      AND as4local = 'A'
      AND tabname  = @p_tabv
    INTO @DATA(lv_tabname).

  IF sy-subrc <> 0.
    MESSAGE '表不存在'(m01) TYPE 'E'.
  ELSE.
    IF NOT lv_tabname(1) CA 'ZY'.
      MESSAGE '不允许修改系统表'(m02) TYPE 'E'.
    ENDIF.
  ENDIF.

  IF p_sclt = p_tclt.
    MESSAGE '源集团 和 目标集团 一致'(m03) TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_tabv
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE 'get table fieldcat failed'(m04) TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  READ TABLE gt_fieldcat INTO DATA(ls_mandt_field) WITH KEY domname = 'MANDT'.
  IF sy-subrc <> 0.
    MESSAGE '无集团字段 无需拷贝'(m05) TYPE 'E'.
  ELSE.
    IF ls_mandt_field-key = ' '.
      MESSAGE '集团字段不是主键 无需拷贝'(m06) TYPE 'E'.
    ENDIF.
  ENDIF.

  cl_alv_table_create=>create_dynamic_table(
    EXPORTING
      it_fieldcatalog = gt_fieldcat
    IMPORTING
      ep_table        = go_tablev ).

  ASSIGN go_tablev->* TO <gt_tablev>.

  DATA(lv_where) = |{ ls_mandt_field-fieldname } = '{ p_sclt }'|.

  SELECT
    *
    FROM (lv_tabname)
    CLIENT SPECIFIED
    WHERE (lv_where)
    INTO TABLE @<gt_tablev>.

  LOOP AT <gt_tablev> ASSIGNING FIELD-SYMBOL(<ls_tablev>).

    ASSIGN COMPONENT ls_mandt_field-fieldname OF STRUCTURE <ls_tablev> TO FIELD-SYMBOL(<lv_mandt>).
    IF sy-subrc = 0.
      <lv_mandt> = p_tclt.
    ENDIF.

  ENDLOOP.

  MODIFY (lv_tabname) CLIENT SPECIFIED FROM TABLE <gt_tablev>.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE '保存成功'(m07) TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE '保存失败'(m08) TYPE 'E'.
  ENDIF.

ENDFORM.