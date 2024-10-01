REPORT zakit_ectype_transposrt.

" 通过 ADT 相关类进行副本处理

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: e070, e07t.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_display,
         transport              TYPE trkorr,      " 请求
         type                   TYPE trfunction,  " 请求类型
         typeshow               TYPE ddtext,
         target_system          TYPE tr_target,   " 目标系统
         owner                  TYPE tr_as4user,  " 所有者
         creation_date          TYPE as4date,     " 创建日期
         description            TYPE as4text,     " 描述
         iself                  TYPE icon_l4,     " 释放请求
         iectype                TYPE icon_l4,     " 创建副本
         irelease_ectype        TYPE icon_l4,     " 创建副本并释放
         irelease_import_ectype TYPE icon_l4,     " 创建副本、释放、导入
         ectype_number          TYPE trkorr,      " 副本编码
         ectype_status          TYPE string,      " 副本状态

         release_time           TYPE timestamp,   " 释放时间

         release                TYPE flag,
         box                    TYPE flag,
         color                  TYPE lvc_t_scol,  "
       END OF ty_display.


TYPES: BEGIN OF ty_ectype,
         trkorr    TYPE e070-trkorr,
         trstatus  TYPE e070-trstatus,
         dsstatus  TYPE text5,
         tarsystem TYPE e070-tarsystem,
         as4user   TYPE e070-as4user,
         as4date   TYPE e070-as4date,
         as4time   TYPE e070-as4time,
         as4text   TYPE e07t-as4text,
         color     TYPE lvc_t_scol,
       END OF ty_ectype,
       BEGIN OF ty_popup_data,
         transport TYPE trkorr,      " 请求
         data      TYPE STANDARD TABLE OF ty_ectype WITH EMPTY KEY,
       END OF ty_popup_data.

*&----------------------------------------------------------------------
*                     Class
*&----------------------------------------------------------------------
CLASS lcl_event_receiver DEFINITION DEFERRED.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.
DATA: gt_display TYPE TABLE OF ty_display.
DATA: gv_pbo_ts TYPE ty_display-transport.

DATA: gs_popup_data   TYPE ty_popup_data.
DATA: go_popup_dailog TYPE REF TO cl_gui_dialogbox_container,
      go_popup_alv    TYPE REF TO cl_gui_alv_grid,
      go_popup_event  TYPE REF TO lcl_event_receiver.
DATA: go_popup_salv TYPE REF TO cl_salv_table.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECT-OPTIONS s_trnum FOR e070-trkorr.
  SELECT-OPTIONS s_owner FOR e070-as4user.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  PARAMETERS: p_edabl RADIOBUTTON GROUP gp1 DEFAULT 'X' USER-COMMAND gp1,
              p_relea RADIOBUTTON GROUP gp1.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  PARAMETERS: p_adate AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blck3.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  %_s_trnum_%_app_%-text = '请求'.
  %_s_owner_%_app_%-text = '用户'.
  %_p_adate_%_app_%-text = '自动日期替换'.

  %_p_edabl_%_app_%-text = '可修改'.
  %_p_relea_%_app_%-text = '已释放'.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM frm_get_data.

  PERFORM frm_set_fieldcat.
  PERFORM frm_set_layout.
  PERFORM frm_alv_display.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_close
        FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.

    DATA: dialogbox_status TYPE c.  "'X': does exist, SPACE: does not ex.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_close.
* §6.Handle the CLOSE-button of the dialogbox

* set dialogbox invisible
* (the dialogbox is destroyed outomatically when the user
* switches to another dynpro).
    CALL METHOD sender->set_visible
      EXPORTING
        visible = space.
* In this example closing the dialogbox leads
* to make it invisible. It is also conceivable to destroy it
* and recreate it if the user doubleclicks a line again.
* Displaying a great amount of data has a greater impact on performance.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& 拉取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA: lt_dd07v TYPE TABLE OF dd07v.
  DATA: lt_range_status TYPE RANGE OF e070-trstatus.

  IF p_relea = 'X'.
    lt_range_status = VALUE #( sign = 'I' option = 'EQ' ( low = 'O' ) ( low = 'R' ) ( low = 'N' ) ).
  ELSE.
    lt_range_status = VALUE #( sign = 'I' option = 'EQ' ( low = 'D' ) ( low = 'L' ) ).
  ENDIF.

  SELECT FROM e070
    LEFT JOIN e07t ON e07t~trkorr = e070~trkorr
    LEFT JOIN e070 AS sup ON sup~trkorr = e070~strkorr
    FIELDS
      e070~trkorr AS transport,
      e070~trfunction AS type,
      e070~as4user AS owner,
      e070~as4date AS creation_date,
      CASE WHEN e070~tarsystem <> @space
           THEN e070~tarsystem ELSE sup~tarsystem END AS target_system,
      e07t~as4text AS description
     WHERE e070~trkorr  IN @s_trnum
       AND e070~as4user IN @s_owner
       AND ( e070~strkorr     = '' )
       AND ( e070~trstatus   IN @lt_range_status )
       AND ( e070~trfunction <> 'T' )
     ORDER BY e070~trkorr DESCENDING, e070~as4date DESCENDING
     INTO CORRESPONDING FIELDS OF TABLE @gt_display.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'TRFUNCTION'
      langu          = '1'
      text           = 'T'
    TABLES
      dd07v_tab      = lt_dd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  SORT lt_dd07v BY domvalue_l.

  " 释放时间
  IF p_relea = 'X' AND gt_display IS NOT INITIAL.
    SELECT
      trkorr,
      reference
      FROM e070a
      FOR ALL ENTRIES IN @gt_display
      WHERE trkorr = @gt_display-transport
        AND attribute = 'EXPORT_TIMESTAMP'
      INTO TABLE @DATA(lt_e070a).
    SORT lt_e070a BY trkorr.
  ENDIF.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<ls_display>).
    <ls_display>-iself                  = icon_transport.

    <ls_display>-iectype                = icon_copy_object.
    <ls_display>-irelease_ectype        = icon_import_transport_request.
    <ls_display>-irelease_import_ectype = icon_pm_insert.

    READ TABLE lt_dd07v INTO DATA(ls_dd07v) WITH KEY domvalue_l = CONV #( <ls_display>-type ).
    IF sy-subrc = 0.
      <ls_display>-typeshow = ls_dd07v-ddtext.
    ENDIF.

    READ TABLE lt_e070a INTO DATA(ls_e070a) WITH KEY trkorr = <ls_display>-transport BINARY SEARCH.
    IF sy-subrc = 0.
      CONVERT DATE ls_e070a-reference(8) TIME ls_e070a-reference+8 INTO TIME STAMP <ls_display>-release_time TIME ZONE 'UTC+8'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'TRANSPORT'              '' '' '请求______'(001)." TEXT-001. " 请求
  PERFORM frm_set_fcat USING 'TYPESHOW'               '' '' '请求类型'(002)." TEXT-002. " 请求类型
  PERFORM frm_set_fcat USING 'TARGET_SYSTEM'          '' '' '目标系统'(003)." TEXT-003. " 目标系统
  PERFORM frm_set_fcat USING 'OWNER'                  '' '' '所有者'(004)." TEXT-004. " 所有者
  PERFORM frm_set_fcat USING 'CREATION_DATE'          '' '' '创建日期'(005)." TEXT-005. " 创建日期
  PERFORM frm_set_fcat USING 'DESCRIPTION'            '' '' '描述'(006)." TEXT-006. " 描述

  IF p_relea = 'X'.
    PERFORM frm_set_fcat USING 'RELEASE_TIME'         '' '' '释放时间'(013)." TEXT-013. " 释放时间
  ENDIF.

  CHECK p_relea = ''.

* PERFORM frm_set_fcat USING 'ISELF'                  '' '' '释放请求'(007)." TEXT-007. " 释放请求
  PERFORM frm_set_fcat USING 'IECTYPE'                '' '' '创建副本'(008)." TEXT-008. " 创建副本
  PERFORM frm_set_fcat USING 'IRELEASE_ECTYPE'        '' '' '副本释放'(009)." TEXT-009. " 副本释放
* PERFORM frm_set_fcat USING 'IRELEASE_IMPORT_ECTYPE' '' '' '副本导入'(010)." TEXT-010. " 副本导入
  PERFORM frm_set_fcat USING 'ECTYPE_NUMBER'          '' '' '副本编码'(011)." TEXT-011. " 副本编码
  PERFORM frm_set_fcat USING 'ECTYPE_STATUS'          '' '' '副本状态'(012)." TEXT-012. " 副本状态

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

  IF p_fieldname+0(1) = 'I'.
    <ls_fieldcat>-icon = 'X'.
    <ls_fieldcat>-hotspot = 'X'.
  ENDIF.

" IF p_fieldname = 'TRANSPORT' OR ( strlen( p_fieldname ) > 7 AND p_fieldname+0(7) = 'ECTYPE_' ).
"   <ls_fieldcat>-hotspot = 'X'.
" ENDIF.

  IF p_fieldname = 'TYPESHOW' OR p_fieldname = 'TARGET_SYSTEM'.
    <ls_fieldcat>-just = 'C'.
  ENDIF.

  IF p_fieldname = 'RELEASE_TIME'.
    <ls_fieldcat>-convexit = 'TSTRM'.
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

  "PERFORM adapt_excluding_tab(saplslvc_fullscreen) CHANGING lt_extab[].

  " SET PF-STATUS 'STANDARD' EXCLUDING lt_extab OF PROGRAM 'SAPLKKBL'.
  SET PF-STATUS 'STANDARD'.

  " 弹框显示部分逻辑
  IF gv_pbo_ts IS NOT INITIAL.
    "PERFORM frm_display_ectype USING gv_pbo_ts.
  ENDIF.

  CLEAR gv_pbo_ts.
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

      CASE ps_selfield-fieldname.
        WHEN 'IECTYPE'.
          " 创建副本
          CHECK ps_selfield-value IS NOT INITIAL.

          PERFORM frm_create_request USING <ls_display>.

          PERFORM frm_fill_request USING <ls_display>.
        WHEN 'IRELEASE_ECTYPE'.
          " 创建并释放
          CHECK ps_selfield-value IS NOT INITIAL.

          PERFORM frm_create_request USING <ls_display>.

          PERFORM frm_fill_request USING <ls_display>.

          PERFORM frm_release_request USING <ls_display>.
        WHEN 'IRELEASE_IMPORT_ECTYPE'.
          " 创建 + 释放 + 导入
          CHECK ps_selfield-value IS NOT INITIAL.

          PERFORM frm_create_request USING <ls_display>.

          PERFORM frm_fill_request USING <ls_display>.

          PERFORM frm_release_request USING <ls_display>.

          PERFORM frm_target_import USING <ls_display>.
        WHEN 'ISELF'.
          " 释放请求
          CHECK ps_selfield-value IS NOT INITIAL.

          PERFORM frm_release_self USING <ls_display>.
        WHEN 'TRANSPORT'.
          " 跳转
          CHECK ps_selfield-value IS NOT INITIAL.

          PERFORM frm_call_transport USING <ls_display>-transport.
        WHEN 'ECTYPE_NUMBER'.
          " 显示关联
          gv_pbo_ts = <ls_display>-transport.
          PERFORM frm_display_ectype USING <ls_display>-transport.
        WHEN OTHERS.
      ENDCASE.

      " WHEN '&F12' OR '&F15' OR '&F03'.
      "   IF 1 = 2.
      "     CLEAR p_ucomm.
      "   ELSE.
      "     ps_selfield-exit = 'X'.
      "   ENDIF.
    WHEN 'PACK'.

      PERFORM frm_btn_pack.

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
      i_save                   = 'X'
      i_default                = 'X'
      is_variant               = ls_variant
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
      it_events                = lt_events[]
      it_event_exit            = lt_events_exit
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
*& Form FRM_CREATE_REQUEST
*&---------------------------------------------------------------------*
*& 创建请求
*&---------------------------------------------------------------------*
FORM frm_create_request  USING p_display TYPE ty_display.
  DATA: lt_text TYPE catsxt_longtext_itab,
        lv_text TYPE as4text.

  IF p_display-release IS NOT INITIAL.
    MESSAGE '请求已被释放, 无法处理副本' TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CHECK p_display-ectype_number IS INITIAL.

  IF p_adate = 'X'.
    REPLACE REGEX '\d{4}[01]\d[0123]\d' IN p_display-description WITH sy-datum.
  ENDIF.

  APPEND |副本_{ p_display-description }| TO lt_text.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title = '副本描述'(t01)
    CHANGING
      ch_text  = lt_text.

  CONCATENATE LINES OF lt_text INTO lv_text.

  TRY.
      " 创建副本
      cl_adt_cts_management=>create_empty_request(
        EXPORTING iv_type = 'T' " 副本请求
                  iv_text = lv_text
                  iv_target = p_display-target_system
        IMPORTING es_request_header = DATA(ls_transport_header) ).

      p_display-ectype_number = ls_transport_header-trkorr.
      p_display-ectype_status = 'Created'.

    CATCH cx_root INTO DATA(cx).
      MESSAGE cx->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FILL_REQUEST
*&---------------------------------------------------------------------*
*&  补充请求内容
*&---------------------------------------------------------------------*
FORM frm_fill_request  USING p_display TYPE ty_display.
  DATA: lt_request_headers TYPE trwbo_request_headers.

  IF p_display-release IS NOT INITIAL.
    MESSAGE '请求已被释放, 无法处理副本' TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CHECK p_display-ectype_number IS NOT INITIAL.

  CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
    EXPORTING
      iv_trkorr          = p_display-transport
    IMPORTING
      et_request_headers = lt_request_headers
*     ET_REQUESTS        =
    EXCEPTIONS
      invalid_input      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
           sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_request_headers INTO DATA(ls_rh) WHERE trkorr = p_display-transport
                                                 OR strkorr = p_display-transport.

    " 将请求内容添加到副本请求中

    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
        wi_dialog                = ' '
        wi_trkorr_from           = ls_rh-trkorr
        wi_trkorr_to             = p_display-ectype_number
        wi_without_documentation = ''
      EXCEPTIONS
        db_access_error          = 1
        trkorr_from_not_exist    = 2
        trkorr_to_is_repair      = 3
        trkorr_to_locked         = 4
        trkorr_to_not_exist      = 5
        trkorr_to_released       = 6
        user_not_owner           = 7
        no_authorization         = 8
        wrong_client             = 9
        wrong_category           = 10
        object_not_patchable     = 11
        OTHERS                   = 12.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
             sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

  p_display-ectype_status = 'Filled'.
  p_display-color = VALUE #( ( fname = 'ECTYPE_STATUS' color = VALUE #( col = 7 int = 0 ) ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_release_request
*&---------------------------------------------------------------------*
*&  释放请求
*&---------------------------------------------------------------------*
FORM frm_release_request  USING p_display TYPE ty_display.

  IF p_display-release IS NOT INITIAL.
    MESSAGE '请求已被释放, 无法处理副本' TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  TRY.
      DATA(lo_cts) = cl_cts_rest_api_factory=>create_instance( ).

      lo_cts->release( iv_trkorr = p_display-ectype_number iv_ignore_locks = 'X' ).

    CATCH cx_root INTO DATA(cx).
      MESSAGE cx->get_text( ) TYPE 'E'.
  ENDTRY.

  p_display-ectype_status = 'Released'.
  p_display-color = VALUE #( ( fname = 'ECTYPE_STATUS' color = VALUE #( col = 3 int = 0 ) ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_TARGET_IMPORT
*&---------------------------------------------------------------------*
*& 目标系统导入
*&---------------------------------------------------------------------*
FORM frm_target_import  USING p_display TYPE ty_display.

  "Refresh Import queue
  DATA ls_exception TYPE stmscalert.
  CALL FUNCTION 'TMS_MGR_REFRESH_IMPORT_QUEUES'
    EXPORTING
      iv_system    = p_display-target_system
      iv_monitor   = abap_true
      iv_verbose   = abap_true
    IMPORTING
      es_exception = ls_exception
    EXCEPTIONS
      OTHERS       = 99.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  DATA lv_rcode TYPE stpa-retcode.
  CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST' DESTINATION p_display-target_system
    EXPORTING
      iv_system                  = p_display-target_system
      iv_request                 = p_display-transport
      iv_client                  = ''
    IMPORTING
      ev_tp_ret_code             = lv_rcode
    EXCEPTIONS
      read_config_failed         = 1
      table_of_requests_is_empty = 2
      OTHERS                     = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
           sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_display-ectype_status = 'Imported'.
  p_display-color = VALUE #( ( fname = 'ECTYPE_STATUS' color = VALUE #( col = 4 int = 0 ) ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_release_self
*&---------------------------------------------------------------------*
*&  释放请求本身
*&---------------------------------------------------------------------*
FORM frm_release_self  USING p_display TYPE ty_display.

  CHECK p_display-release IS INITIAL.

  TRY.
      DATA(lo_cts) = cl_cts_rest_api_factory=>create_instance( ).

      lo_cts->release( iv_trkorr = p_display-transport iv_ignore_locks = 'X' ).

    CATCH cx_root INTO DATA(cx).
      MESSAGE cx->get_text( ) TYPE 'E'.
  ENDTRY.

  p_display-release = 'X'.
  p_display-ectype_status = 'Released'.
  p_display-color = VALUE #( ( fname = 'ECTYPE_STATUS' color = VALUE #( col = 5 int = 0 ) )
                             ( fname = 'ISELF'         color = VALUE #( col = 5 int = 0 ) ) ).
  CLEAR: p_display-iself,
         p_display-iectype,
         p_display-irelease_ectype,
         p_display-irelease_import_ectype.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_CALL_TRANSPORT
*&---------------------------------------------------------------------*
*&  跳转
*&---------------------------------------------------------------------*
FORM frm_call_transport  USING p_transport TYPE ty_display-transport.
  DATA lt_bdcdata TYPE TABLE OF bdcdata.

  APPEND VALUE #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) TO lt_bdcdata.
  APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=TSSN' ) TO lt_bdcdata.

  APPEND VALUE #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) TO lt_bdcdata.
  APPEND VALUE #( fnam = 'BDC_SUBSCR' fval = 'RDDM0001                                0210COMMONSUBSCREEN' ) TO lt_bdcdata.
  APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'TRDYSE01SN-TR_TRKORR' ) TO lt_bdcdata.
  APPEND VALUE #( fnam = 'TRDYSE01SN-TR_TRKORR' fval = p_transport ) TO lt_bdcdata.

  CALL TRANSACTION 'SE01' USING lt_bdcdata MODE 'E' UPDATE 'A'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_display_ectype
*&---------------------------------------------------------------------*
*&  显示关联副本
*&---------------------------------------------------------------------*
FORM frm_display_ectype  USING p_transport TYPE ty_display-transport.
  DATA: lt_range_objname TYPE RANGE OF e071-obj_name.

  lt_range_objname = VALUE #( sign = 'I' option = 'CP' ( low = |{ p_transport }*| ) ).

  IF go_popup_event IS INITIAL.
    go_popup_event = NEW #( ).
  ENDIF.

  " LOAD DATA
  IF gs_popup_data-transport <> p_transport.
    SELECT
     trkorr,
     strkorr
     FROM e070
     WHERE strkorr = @p_transport
     INTO TABLE @DATA(lt_sub_link).
    SORT lt_sub_link BY trkorr.
    IF lt_sub_link IS NOT INITIAL.
      lt_range_objname = VALUE #( BASE lt_range_objname FOR sub IN lt_sub_link ( sign = 'I' option = 'CP' low = |*{ sub-trkorr }*| ) ).
      SORT lt_range_objname BY low.
      DELETE ADJACENT DUPLICATES FROM lt_range_objname COMPARING low.
    ENDIF.

    SELECT FROM e071 AS e1
      INNER JOIN e070 AS e0 ON e0~trkorr = e1~trkorr
      LEFT JOIN e07t AS et ON et~trkorr = e0~trkorr AND et~langu = @sy-langu
      FIELDS
      e0~trkorr,
      e0~trstatus,
      CASE e0~trstatus WHEN 'D' THEN '可修改'
        WHEN 'R' THEN '已释放' ELSE e0~trstatus END AS dsstatus,
      e0~tarsystem,
      e0~as4user,
      e0~as4date,
      e0~as4time,
      et~as4text
      WHERE e1~pgmid = 'CORR'
        AND e1~object = 'MERG'
        AND e1~obj_name IN @lt_range_objname
      INTO CORRESPONDING FIELDS OF TABLE @gs_popup_data-data.
    SORT gs_popup_data-data BY trkorr DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gs_popup_data-data COMPARING trkorr.

    LOOP AT gs_popup_data-data REFERENCE INTO DATA(lr_data).
      IF lr_data->trstatus = 'D'.
        lr_data->color = VALUE #( ( fname = 'DSSTATUS' color = VALUE #( col = 5 int = 0 ) ) ).
      ELSE.
        lr_data->color = VALUE #( ( fname = 'DSSTATUS' color = VALUE #( col = 3 int = 0 ) ) ).
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF gs_popup_data-data IS INITIAL.
    MESSAGE '无关联数据' TYPE 'S' DISPLAY LIKE 'W'.
    EXIT.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = ''
        IMPORTING
          r_salv_table = go_popup_salv
        CHANGING
          t_table      = gs_popup_data-data[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  DATA(lo_functions) = go_popup_salv->get_functions( ).
  lo_functions->set_default( abap_true ).

  DATA(lo_display_lay) = go_popup_salv->get_display_settings( ).
  lo_display_lay->set_striped_pattern( cl_salv_display_settings=>true ).

  go_popup_salv->set_screen_popup(
     start_line   = 10
     start_column = 30
     end_line     = 20
     end_column   = 130 ).

  TRY.
      go_popup_salv->get_columns( )->set_color_column( 'COLOR' ).
      go_popup_salv->get_columns( )->set_optimize( abap_true ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
  ENDTRY.

  DATA lo_colum TYPE REF TO cl_salv_column.
  TRY.
      lo_colum = go_popup_salv->get_columns( )->get_column( 'TRKORR' ).
      lo_colum->set_short_text( '请求'(101) ).
      lo_colum->set_medium_text( '请求'(101) ).
      lo_colum->set_long_text( '请求'(101) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lo_colum = go_popup_salv->get_columns( )->get_column( 'DSSTATUS' ).
      lo_colum->set_alignment( if_salv_c_alignment=>centered ).
      lo_colum->set_short_text( '请求状态'(102) ).
      lo_colum->set_medium_text( '请求状态'(102) ).
      lo_colum->set_long_text( '请求状态'(102) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lo_colum = go_popup_salv->get_columns( )->get_column( 'TARSYSTEM' ).
      lo_colum->set_alignment( if_salv_c_alignment=>centered ).
      lo_colum->set_short_text( '目标系统'(103) ).
      lo_colum->set_medium_text( '目标系统'(103) ).
      lo_colum->set_long_text( '目标系统'(103) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lo_colum = go_popup_salv->get_columns( )->get_column( 'AS4USER' ).
      lo_colum->set_short_text( '所有者'(104) ).
      lo_colum->set_medium_text( '所有者'(104) ).
      lo_colum->set_long_text( '所有者'(104) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lo_colum = go_popup_salv->get_columns( )->get_column( 'AS4DATE' ).
      lo_colum->set_short_text( '创建日期'(105) ).
      lo_colum->set_medium_text( '创建日期'(105) ).
      lo_colum->set_long_text( '创建日期'(105) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lo_colum = go_popup_salv->get_columns( )->get_column( 'AS4TIME' ).
      lo_colum->set_short_text( '创建时间'(106) ).
      lo_colum->set_medium_text( '创建时间'(106) ).
      lo_colum->set_long_text( '创建时间'(106) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lo_colum = go_popup_salv->get_columns( )->get_column( 'AS4TEXT' ).
      lo_colum->set_short_text( '描述'(107) ).
      lo_colum->set_medium_text( '描述'(107) ).
      lo_colum->set_long_text( '描述'(107) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      go_popup_salv->get_columns( )->get_column( 'TRSTATUS' )->set_technical( 'X' ).
      go_popup_salv->get_columns( )->get_column( 'COLOR' )->set_technical( 'X' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  go_popup_salv->display( ).

  " IF go_popup_event->dialogbox_status IS INITIAL.
  "
  "   go_popup_event->dialogbox_status = 'X'.
  "
  "   IF go_popup_dailog IS INITIAL.
  "     CREATE OBJECT go_popup_dailog
  "       EXPORTING
  "         top      = 150
  "         left     = 150
  "         lifetime = 1
  "         caption  = '副本请求'(200)
  "         width    = 850
  "         height   = 180.
  "
  "     SET HANDLER go_popup_event->handle_close FOR go_popup_dailog.
  "
  "   ENDIF.
  "
  "   IF go_popup_alv IS INITIAL.
  "     go_popup_alv = NEW #( i_parent = go_popup_dailog  ).
  "
  "     DATA(lt_fieldcat) = VALUE lvc_t_fcat(
  "       ( fieldname  = 'TRKORR'    coltext = '请求'(101) )
  "       ( fieldname  = 'DSSTATUS'  just = 'C' coltext = '请求状态'(102) )
  "       ( fieldname  = 'TARSYSTEM' just = 'C' coltext = '目标系统'(103) )
  "       ( fieldname  = 'AS4USER'   coltext = '所有者'(104) )
  "       ( fieldname  = 'AS4DATE'   coltext = '创建日期'(105) )
  "       ( fieldname  = 'AS4TIME'   coltext = '创建时间'(106) )
  "       ( fieldname  = 'AS4TEXT'   coltext = '描述'(107) ) ).
  "
  "     CALL METHOD go_popup_alv->set_table_for_first_display
  "       EXPORTING
  "         i_save                        = 'A'
  "         is_layout                     = VALUE #( zebra = 'X' sel_mode = 'D' cwidth_opt = 'X' ctab_fname = 'COLOR' )
  "       CHANGING
  "         it_outtab                     = gs_popup_data-data[]
  "         it_fieldcatalog               = lt_fieldcat[]
  "       EXCEPTIONS
  "         invalid_parameter_combination = 1
  "         program_error                 = 2
  "         too_many_lines                = 3
  "         OTHERS                        = 4.
  "   ENDIF.
  "
  "   LEAVE SCREEN.
  " ELSE.
  "   go_popup_dailog->set_visible( 'X' ).
  "   go_popup_alv->refresh_table_display( ).
  " ENDIF.

  "cl_gui_cfw=>flush( ).
  "go_popup_alv->set_focus( go_popup_alv ).
  "go_popup_dailog->set_focus( go_popup_dailog ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_btn_pack
*&---------------------------------------------------------------------*
*&  打包副本
*&---------------------------------------------------------------------*
FORM frm_btn_pack .

  IF NOT line_exists( gt_display[ box = 'X'] ).
    MESSAGE '至少选中一行数据' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA: lt_text TYPE catsxt_longtext_itab,
        lv_text TYPE as4text.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title = '副本请求'
    CHANGING
      ch_text  = lt_text.

  ASSIGN ('(SAPLCATSXT_UTIL)OK_CODE') TO FIELD-SYMBOL(<lv_code>).
  IF sy-subrc = 0.
    IF <lv_code> <> 'CX_CONT'.
      MESSAGE '已取消' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  CONCATENATE LINES OF lt_text INTO lv_text.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<ls_display>) WHERE box = 'X'.
    CHECK <ls_display>-release IS INITIAL.

    IF lv_text IS INITIAL.
      PERFORM frm_create_request USING <ls_display>.
      lv_text = <ls_display>-ectype_number.
    ELSE.
      <ls_display>-ectype_number = lv_text.
    ENDIF.

    PERFORM frm_fill_request USING <ls_display>.
  ENDLOOP.
ENDFORM.