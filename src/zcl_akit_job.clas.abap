CLASS zcl_akit_job DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      " job priority
      BEGIN OF job_priority,
        "! A = highest priority
        a TYPE btcjobclas VALUE tybtc_jobclass_a,
        "! B = medium priority (default)
        b TYPE btcjobclas VALUE tybtc_jobclass_b,
        "! C = low priority
        c TYPE btcjobclas VALUE tybtc_jobclass_c,
      END OF job_priority .
    CONSTANTS:
      BEGIN OF null_value,
        date TYPE d VALUE '        ',
        time TYPE t VALUE '      ',
      END OF null_value .
    DATA:
      BEGIN OF j,
        " job import values
        " base
        name                        TYPE btcjob,
        priority                    TYPE btcjobclas,
        " time
        laststrtdt                  TYPE d,
        laststrttm                  TYPE t,
        " 开始时间
        sdlstrtdt                   TYPE d,
        sdlstrttm                   TYPE t,
        start_on_workday_not_before TYPE d,
        " 重复
        prddays                     TYPE tbtcjob-prddays,
        prdhours                    TYPE tbtcjob-prdhours,
        prdmins                     TYPE tbtcjob-prdmins,
        prdmonths                   TYPE tbtcjob-prdmonths,
        prdweeks                    TYPE tbtcjob-prdweeks,
        at_opmode                   TYPE spfba-baname,
        at_opmode_periodic          TYPE btch0000-char1,
        " LBTCHDEF->btc_process_always ' '
        startdate_restriction       TYPE tbtcjob-prdbehav VALUE ' ',
        start_on_workday_nr         TYPE tbtcstrt-wdayno VALUE 0,
        workday_count_direction     TYPE tbtcstrt-wdaycdir VALUE 0,
        " 指定 job 之后
        pred_jobcount               TYPE tbtcjob-jobcount,
        pred_jobname                TYPE tbtcjob-jobname,
        predjob_checkstat           TYPE tbtcstrt-checkstat,
        " 立即开始
        strtimmed                   TYPE btch0000-char1,
        direct_start                TYPE btch0000-char1,
        " ???
        calendar_id                 TYPE tbtcjob-calendarid,
        " event
        event_id                    TYPE tbtcjob-eventid,
        event_param                 TYPE tbtcjob-eventparm,
        event_periodic              TYPE btch0000-char1,
        " target
        targetsystem                TYPE msxxlist-name,
        targetserver                TYPE btctgtsrvr-srvname,
        targetgroup                 TYPE bpsrvgrp,
        "
        recipient_obj               TYPE swotobjid,
        "
        dont_release                TYPE btch0000-char1 VALUE space,
      END OF j .
    DATA:
      BEGIN OF jinfo,
        " job info
        count TYPE tbtcjob-jobcount,
        info  TYPE i,

        step  TYPE tbtcjob-stepcount,
      END OF jinfo .

    METHODS constructor
      IMPORTING
        !name     TYPE btcjob
        !priority TYPE btcjobclas DEFAULT job_priority-b
      EXCEPTIONS
        lcx_job_exception .
    METHODS new_step
      IMPORTING
        !program            TYPE program
        !variant            TYPE variant
        !user               TYPE syuname DEFAULT sy-uname
        !language           TYPE sylangu DEFAULT sy-langu
        !selection_table    TYPE rsparams_tt OPTIONAL
        !print_parameters   TYPE bapipripar OPTIONAL
        !archive_parameters TYPE bapiarcpar OPTIONAL
      RETURNING
        VALUE(job)          TYPE REF TO zcl_akit_job
      EXCEPTIONS
        lcx_job_exception .
    METHODS jopen
      EXCEPTIONS
        lcx_job_exception .
    METHODS run_now
      IMPORTING
        !error_if_cant_start_immed TYPE abap_bool DEFAULT abap_false
      EXCEPTIONS
        lcx_job_exception .
    METHODS run_at
      IMPORTING
        !date                TYPE d
        !time                TYPE t
        !not_later_than_date TYPE tbtcjob-laststrtdt OPTIONAL             " laststrtdt
        !not_later_than_time TYPE tbtcjob-laststrttm OPTIONAL             " laststrttm
      EXCEPTIONS
        lcx_job_exception .
    METHODS run_after_jobid
      IMPORTING
        !name              TYPE btcjob
        !count             TYPE tbtcjob-jobcount
        !predjob_checkstat TYPE tbtcstrt-checkstat DEFAULT abap_false
      EXCEPTIONS
        lcx_job_exception .
    METHODS run_after_job
      IMPORTING
        !predecessor       TYPE REF TO zcl_akit_job
        !predjob_checkstat TYPE tbtcstrt-checkstat DEFAULT abap_false
      EXCEPTIONS
        lcx_job_exception .
    METHODS repetition
      IMPORTING
        !days      TYPE tbtcjob-prddays OPTIONAL
        !hours     TYPE tbtcjob-prdhours OPTIONAL
        !mins      TYPE tbtcjob-prdmins OPTIONAL
        !months    TYPE tbtcjob-prdmonths OPTIONAL
        !weeks     TYPE tbtcjob-prdweeks OPTIONAL
      RETURNING
        VALUE(job) TYPE REF TO zcl_akit_job .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS version .
    METHODS submit_job
      IMPORTING
        !jobname                     TYPE btcjob
        !jobcount                    TYPE btcjobcnt
        !arcparams                   TYPE arc_params OPTIONAL
        !authcknam                   TYPE tbtcjob-authcknam DEFAULT sy-uname
        !commandname                 TYPE sxpgcolist-name OPTIONAL
        !operatingsystem             TYPE sy-opsys DEFAULT sy-opsys
        !extpgm_name                 TYPE tbtcstep-program OPTIONAL
        !extpgm_param                TYPE tbtcstep-parameter OPTIONAL
        !extpgm_set_trace_on         TYPE btch0000-char1 OPTIONAL
        !extpgm_stderr_in_joblog     TYPE btch0000-char1 DEFAULT 'X'
        !extpgm_stdout_in_joblog     TYPE btch0000-char1 DEFAULT 'X'
        !extpgm_system               TYPE tbtcstep-xpgtgtsys OPTIONAL
        !extpgm_rfcdest              TYPE tbtcstep-xpgrfcdest OPTIONAL
        !extpgm_wait_for_termination TYPE btch0000-char1 OPTIONAL
        !language                    TYPE sy-langu DEFAULT sy-langu
        !priparams                   TYPE pri_params OPTIONAL
        !report                      TYPE sy-repid OPTIONAL
        !variant                     TYPE raldb-variant OPTIONAL
      EXPORTING
        !step                        TYPE tbtcjob-stepcount
      RETURNING
        VALUE(subrc)                 TYPE sysubrc
      EXCEPTIONS
        lcx_job_exception .
    METHODS submit_abap
      IMPORTING
        !jobname         TYPE btcjob
        !jobcount        TYPE btcjobcnt
        !program         TYPE program
        !variant         TYPE variant
        !selection_table TYPE rsparams_tt
        !arcparams       TYPE arc_params
        !user            TYPE syuname
        !priparams       TYPE pri_params
      RETURNING
        VALUE(subrc)     TYPE sysubrc
      EXCEPTIONS
        lcx_job_exception .
    METHODS jclose .
ENDCLASS.



CLASS ZCL_AKIT_JOB IMPLEMENTATION.


  METHOD constructor.

    " 变量
    me->j-name = name.          " JOB 名称（允许重复）
    me->j-priority = priority.  " JOB 等级

    " 重置部分属性
    me->j-laststrtdt = me->null_value-date.
    me->j-laststrttm = me->null_value-time.
    me->j-sdlstrtdt  = me->null_value-date.
    me->j-sdlstrttm  = me->null_value-time.

    GET TIME.
    me->j-start_on_workday_not_before = sy-datum.

    " 预先定义开始
    me->jopen( ).

  ENDMETHOD.


  METHOD jopen.

    " 开始 job 定义
    DATA: lv_ret TYPE i.
    DATA: dummy TYPE string.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = me->j-name
        jobclass         = me->j-priority
      IMPORTING
        jobcount         = me->jinfo-count
        info             = me->jinfo-info
      CHANGING
        ret              = lv_ret
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    " catch error

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " based on subroutine XM_MAKE_BAPIRET2_EX in program SAPLSXBP
          CASE lv_ret.
            WHEN tybtc_err_invalid_step_number.
              MESSAGE e224(xm) INTO dummy. " Wrong step number
            WHEN tybtc_err_no_authority.
              MESSAGE e234(xm) INTO dummy. " You do not have change authorization
            WHEN tybtc_err_job_doesnt_have_step.
              MESSAGE e220(xm) INTO dummy. " Step not in job
            WHEN tybtc_err_child_register_error.
              MESSAGE e089(xm) INTO dummy. " Error while registering a child job
            WHEN tybtc_err_wrong_selection_par.
              MESSAGE e096(xm) INTO dummy. " Wrong selection parameters
            WHEN tybtc_err_invalid_jobclass.
              MESSAGE e235(xm) INTO dummy. " Invalid job class
            WHEN tybtc_err_spoollist_recipient.
              MESSAGE e237(xm) INTO dummy. " Receiver object could not be created
            WHEN tybtc_err_plain_recipient.
              MESSAGE e270(xm) INTO dummy. " Could not determine recipient data
            WHEN OTHERS.
              MESSAGE e034(xm) INTO dummy. " Internal problem (function &1)
          ENDCASE.
        WHEN 2.
          MESSAGE e202(xm) INTO dummy. " Invalid new job data
        WHEN 3.
          MESSAGE e046(xm) INTO dummy. " Job name missing (function &1)
        WHEN 4.
          MESSAGE e034(xm) INTO dummy. " Internal problem (function &1)
      ENDCASE.
    ENDIF.

    lcx_job_exception=>raise_t100( ).

  ENDMETHOD.


  METHOD version.

    " v1.00.00

    " 通过 OPEN_JOB 设置定时任务

    " 参考: https://github.com/sandraros/joboo

  ENDMETHOD.


  METHOD jclose.

    DATA: dummy TYPE string.
    DATA: lv_ret TYPE i.
    DATA: lv_job_was_released TYPE btch0000-char1.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        " mode
        at_opmode                   = me->j-at_opmode
        at_opmode_periodic          = me->j-at_opmode_periodic
        " base
        jobcount                    = me->jinfo-count
        jobname                     = me->j-name
        " 周期
        laststrtdt                  = me->j-laststrtdt
        laststrttm                  = me->j-laststrttm
        prddays                     = me->j-prddays
        prdhours                    = me->j-prdhours
        prdmins                     = me->j-prdmins
        prdmonths                   = me->j-prdmonths
        prdweeks                    = me->j-prdweeks
        sdlstrtdt                   = me->j-sdlstrtdt
        sdlstrttm                   = me->j-sdlstrttm
        " ???
        startdate_restriction       = me->j-startdate_restriction " default BTC_PROCESS_ALWAYS
        start_on_workday_not_before = me->j-start_on_workday_not_before " SY-DATUM
        start_on_workday_nr         = me->j-start_on_workday_nr
        workday_count_direction     = me->j-workday_count_direction
        " 前置任务
        predjob_checkstat           = me->j-predjob_checkstat
        pred_jobcount               = me->j-pred_jobcount
        pred_jobname                = me->j-pred_jobname
        " ???
        calendar_id                 = me->j-calendar_id
        " ---
        strtimmed                   = me->j-strtimmed
        direct_start                = me->j-direct_start
        " ---
        event_id                    = me->j-event_id
        event_param                 = me->j-event_param
        event_periodic              = me->j-event_periodic
        " ---
        targetsystem                = me->j-targetsystem
        targetserver                = me->j-targetserver
        targetgroup                 = me->j-targetgroup
        " ---
        recipient_obj               = me->j-recipient_obj
        " ---
        dont_release                = me->j-dont_release
      " INHERIT_RECIPIENT           = me->j-INHERIT_RECIPIENT
      " INHERIT_TARGET              = me->j-INHERIT_TARGET
      " REGISTER_CHILD              = me->j-REGISTER_CHILD " ABAP_FALSE
      " TIME_ZONE                   = me->j-TIME_ZONE
      " EMAIL_NOTIFICATION          = me->j-EMAIL_NOTIFICATION
      IMPORTING
        job_was_released            = lv_job_was_released
      CHANGING
        ret                         = lv_ret
      EXCEPTIONS
        cant_start_immediate        = 1
        invalid_startdate           = 2
        jobname_missing             = 3
        job_close_failed            = 4
        job_nosteps                 = 5
        job_notex                   = 6
        lock_failed                 = 7
        invalid_target              = 8
        invalid_time_zone           = 9
        OTHERS                      = 10.
    IF sy-subrc = 0.
      IF me->j-dont_release = abap_false AND lv_job_was_released = abap_false.
        MESSAGE e054(xm) INTO dummy. " No authorization to release a job

        lcx_job_exception=>raise_t100( ).
      ENDIF.
    ELSE.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e066(xm) INTO dummy. " Immediate start not currently possible
        WHEN 2.
          MESSAGE e068(xm) INTO dummy. " Invalid date or invalid time specified
        WHEN 3.
          MESSAGE e046(xm) INTO dummy. " Job name missing (function &1)
        WHEN 4.
          CASE lv_ret.
            WHEN tybtc_err_invalid_step_number.
              MESSAGE e224(xm) INTO dummy. " Wrong step number
            WHEN tybtc_err_no_authority.
              MESSAGE e234(xm) INTO dummy. " You do not have change authorization
            WHEN tybtc_err_job_doesnt_have_step.
              MESSAGE e220(xm) INTO dummy. " Step not in job
            WHEN tybtc_err_child_register_error.
              MESSAGE e089(xm) INTO dummy. " Error while registering a child job
            WHEN tybtc_err_wrong_selection_par.
              MESSAGE e096(xm) INTO dummy. " Wrong selection parameters
            WHEN tybtc_err_invalid_jobclass.
              MESSAGE e235(xm) INTO dummy. " Invalid job class
            WHEN tybtc_err_spoollist_recipient.
              MESSAGE e237(xm) INTO dummy. " Receiver object could not be created
            WHEN tybtc_err_plain_recipient.
              MESSAGE e270(xm) INTO dummy. " Could not determine recipient data
            WHEN OTHERS.
              MESSAGE e034(xm) INTO dummy. " Internal problem (function &1)
          ENDCASE.
        WHEN 5.
          MESSAGE e059(xm) INTO dummy. " The specified job does not have any steps
        WHEN 6.
          MESSAGE e049(xm) INTO dummy. " Job does not exist (function &1)
        WHEN 7.
          MESSAGE e261(xm) WITH space me->j-name me->jinfo-count INTO dummy. " Could not lock job &2, job count &3
        WHEN 8.
          " Invalid server name specified (server name = &1)
          IF me->j-targetsystem IS NOT INITIAL.
            MESSAGE e069(xm) INTO dummy WITH me->j-targetsystem.
          ELSEIF me->j-targetserver IS NOT INITIAL.
            MESSAGE e069(xm) INTO dummy WITH me->j-targetserver.
          ELSE.
            MESSAGE e069(xm) INTO dummy WITH me->j-targetgroup.
          ENDIF.
        WHEN 9.
          MESSAGE e034(xm) INTO dummy. " Internal problem (function &1)
      ENDCASE.

      lcx_job_exception=>raise_t100( ).
    ENDIF.

    CLEAR me->jinfo-step.

    " 程序 close 及 对象失效
    " 不使用 JOPEN 时以下内容无效

    " 清空数据
    CLEAR: me->j-strtimmed, me->j-direct_start.     " 立刻开始
    CLEAR: me->j-sdlstrtdt, me->j-sdlstrttm.        " 指定时间开始
    CLEAR: me->j-pred_jobcount, me->j-pred_jobname. " 指定job后开始
    CLEAR: me->j-prddays, me->j-prdhours, me->j-prdmins, me->j-prdmonths, me->j-prdweeks,
           me->j-at_opmode_periodic. " 周期执行

    " 重置数据
    me->j-laststrtdt = null_value-date.
    me->j-laststrttm = null_value-time.
    me->j-sdlstrtdt  = null_value-date.
    me->j-sdlstrttm  = null_value-time.
    GET TIME.
    me->j-start_on_workday_not_before = sy-datum.

  ENDMETHOD.


  METHOD new_step.

    " 添加新的步骤

    " 新步骤
    DATA: arcparams   TYPE arc_params,
          priparams   TYPE pri_params,
          step_number TYPE i,
          dummy       TYPE string.

    DATA: subrc TYPE sysubrc.


    IF selection_table IS INITIAL.

      subrc = me->submit_job(
        EXPORTING
          jobname        = me->j-name
          jobcount       = me->jinfo-count
          arcparams      = arcparams
          authcknam      = user
          language       = language
          priparams      = priparams
          report         = program
          variant        = variant
        IMPORTING
          step           = step_number ).

    ELSE.

      subrc = me->submit_abap(
          jobname         = me->j-name
          jobcount        = me->jinfo-count
          selection_table = selection_table
          arcparams       = arcparams
          user            = user
          priparams       = priparams
          program         = program
          variant         = variant ).
      IF subrc <> 0.
        MESSAGE e027(bt) WITH program INTO dummy. " Failed to create job step & (see system log)

        lcx_job_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

    ADD 1 TO me->jinfo-step.

    job = me.

  ENDMETHOD.


  METHOD repetition.
    " 周期执行
    " 只有在指定日期时有效

    me->j-prddays   = days.
    me->j-prdhours  = hours.
    me->j-prdmins   = mins.
    me->j-prdmonths = months.
    me->j-prdweeks  = weeks.

    IF me->j-prddays   IS NOT INITIAL
    OR me->j-prdhours  IS NOT INITIAL
    OR me->j-prdmins   IS NOT INITIAL
    OR me->j-prdmonths IS NOT INITIAL
    OR me->j-prdweeks  IS NOT INITIAL.
      me->j-at_opmode_periodic = 'X'.
    ENDIF.

    job = me.

  ENDMETHOD.


  METHOD run_after_job.
    " 运行 指定job后

    " 基于对象类

    me->j-pred_jobcount = predecessor->jinfo-count.
    me->j-pred_jobname = predecessor->j-name.

    me->jclose( ).

  ENDMETHOD.


  METHOD run_after_jobid.
    " 运行 指定job后 直接指定标识

    me->j-pred_jobcount = count.
    me->j-pred_jobname  = name.

    me->jclose( ).

  ENDMETHOD.


  METHOD run_at.
    " 运行 指定时间

    me->j-sdlstrtdt = date.
    me->j-sdlstrttm = time.

    me->jclose( ).

  ENDMETHOD.


  METHOD run_now.
    " 运行 立即

    me->j-strtimmed = abap_true.
    IF error_if_cant_start_immed = abap_true.
      " 不考虑任何因素直接开始
      me->j-direct_start = abap_true.
    ELSE.
      me->j-direct_start = abap_false.
    ENDIF.

    me->jclose( ).

  ENDMETHOD.


  METHOD submit_abap.

    DATA: dummy TYPE string.

    IF selection_table IS NOT INITIAL AND variant IS INITIAL AND user IS INITIAL.

      SUBMIT (program)
            VIA JOB jobname NUMBER jobcount
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

    ELSEIF variant IS INITIAL.

      SUBMIT (program)
            VIA JOB jobname NUMBER jobcount
            USER user
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

    ELSEIF user IS INITIAL.

      SUBMIT (program)
            VIA JOB jobname NUMBER jobcount
            USING SELECTION-SET variant
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

    ELSE.

      SUBMIT (program)
            VIA JOB jobname NUMBER jobcount USER user
            USING SELECTION-SET variant
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

    ENDIF.

  ENDMETHOD.


  METHOD submit_job.
    " 调用标准运行程序

    DATA: dummy TYPE string.

    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
        arcparams                   = arcparams
        authcknam                   = authcknam
        commandname                 = commandname
        operatingsystem             = operatingsystem
        extpgm_name                 = extpgm_name
        extpgm_param                = extpgm_param
        extpgm_set_trace_on         = extpgm_set_trace_on
        extpgm_stderr_in_joblog     = extpgm_stderr_in_joblog
        extpgm_stdout_in_joblog     = extpgm_stdout_in_joblog
        extpgm_system               = extpgm_system
        extpgm_rfcdest              = extpgm_rfcdest
        extpgm_wait_for_termination = extpgm_wait_for_termination
        jobcount                    = jobcount
        jobname                     = jobname
        language                    = language
        priparams                   = priparams
        report                      = report
        variant                     = variant
      IMPORTING
        step_number                 = me->jinfo-step
      EXCEPTIONS
        bad_priparams               = 1
        bad_xpgflags                = 2
        invalid_jobdata             = 3
        jobname_missing             = 4
        job_notex                   = 5
        job_submit_failed           = 6
        lock_failed                 = 7
        program_missing             = 8
        prog_abap_and_extpg_set     = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " should not happen, it should have been intercepted during
          " method process_print_archive_params.
          MESSAGE e034(xm) INTO dummy. " Internal problem (function &1)
        WHEN 2.
          " TODO create a message for XPGFLAGS
        WHEN 3.
          MESSAGE e202(xm) INTO dummy. " Invalid new job data
        WHEN 4.
          MESSAGE e046(xm) INTO dummy. " Job name missing (function &1)
        WHEN 5.
          MESSAGE e049(xm) INTO dummy. " Job does not exist (function &1)
        WHEN 6.
          MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        WHEN 7.
          MESSAGE e194(xm) INTO dummy. " Job could not be locked
        WHEN 8.
          MESSAGE e050(xm) INTO dummy. " Report or program not specified or name incomplete (function &1)
        WHEN 9.
          " can't happen
          MESSAGE e034(xm) INTO dummy. " Internal problem (function &1)
        WHEN 10.
          MESSAGE e034(xm) INTO dummy. " Internal problem (function &1)
      ENDCASE.

      lcx_job_exception=>raise_t100( ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
