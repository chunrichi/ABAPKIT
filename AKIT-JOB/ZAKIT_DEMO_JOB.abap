*&---------------------------------------------------------------------*
REPORT zakit_demo_job.

" 前台执行 -> 创建 JOB
" 后台执行 -> JOB 运行

" 后台运行-> 选择界面
PARAMETERS: p_print TYPE char10 NO-DISPLAY.

START-OF-SELECTION.

  IF sy-batch = 'X'.
    PERFORM frm_run_back.
  ELSE.
    PERFORM frm_run_front.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form frm_run_back
*&---------------------------------------------------------------------*
FORM frm_run_back .
  DATA: lv_timestamp TYPE timestamp,
        lv_message   TYPE string.

  GET TIME STAMP FIELD lv_timestamp.
  lv_message = lv_timestamp && ':' && p_print.

  MESSAGE lv_message TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_run_front
*&---------------------------------------------------------------------*
FORM frm_run_front .

  CONSTANTS: lc_jobname TYPE tbtcs-jobname VALUE 'ZZ_AKIT_DEMO_JOB'.
  DATA: lv_message TYPE string,
        ls_info    TYPE tbtcs,
        lv_goon    TYPE abap_bool.

  ls_info = zcl_akit_job=>check_exists( jobname = lc_jobname periodic = 'X' ).

  IF ls_info IS NOT INITIAL.
    " 已经设置 job
    lv_message = '已设置, 执行周期为 '
      && COND #( WHEN ls_info-prdmonths <> 0 THEN |{ ls_info-prdmonths ALPHA = OUT } 月| )
      && COND #( WHEN ls_info-prdweeks <> 0  THEN |{ ls_info-prdweeks ALPHA = OUT } 周| )
      && COND #( WHEN ls_info-prddays <> 0   THEN |{ ls_info-prddays ALPHA = OUT } 天| )
      && COND #( WHEN ls_info-prdhours <> 0  THEN |{ ls_info-prdhours ALPHA = OUT } 小时| )
      && COND #( WHEN ls_info-prdmins <> 0   THEN |{ ls_info-prdmins ALPHA = OUT } 分钟| ).

    MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'S'.

    PERFORM frm_check_goon USING '是否取消JOB'
                           CHANGING lv_goon.
    IF lv_goon = 'X'.
      CLEAR lv_message.

      TRY.
          zcl_akit_job=>delete_job(
            jobcount = ls_info-jobcount
            jobname  = ls_info-jobname
          ).
        CATCH cx_root INTO DATA(lr_cx_del).
          lv_message = lr_cx_del->get_text( ).
      ENDTRY.

      IF lv_message IS INITIAL.
        MESSAGE '删除成功' TYPE 'I' DISPLAY LIKE 'S'.
      ELSE.
        MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ELSE.
    " 创建新 JOB
    TRY.
        DATA(lr_job) = NEW zcl_akit_job( name = lc_jobname priority = 'C' ).

        lr_job->new_step(
          program = 'ZAKIT_DEMO_JOB'
          selection_table = VALUE rsparams_tt(
              ( selname = 'P_PRINT' kind = 'P' low = 'testing' ) )
        )->repetition( mins = '45' )->run_now( ).

      CATCH cx_root INTO DATA(lr_cx_root).
        lv_message = lr_cx_root->get_text( ).
    ENDTRY.

    IF lv_message IS INITIAL.
      MESSAGE '设置成功' TYPE 'I' DISPLAY LIKE 'S'.
    ELSE.
      MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_check_goon
*&---------------------------------------------------------------------*
FORM frm_check_goon  USING    pv_message
                     CHANGING cv_goon.
  DATA: lv_return TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question = pv_message
    IMPORTING
      answer        = lv_return.
  IF lv_return = '1'.
    cv_goon = 'X'.
  ELSEIF lv_return = '2' OR lv_return = 'A'.
    CLEAR cv_goon.
  ENDIF.
ENDFORM.