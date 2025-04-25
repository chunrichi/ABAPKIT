*&---------------------------------------------------------------------*
*& Report ZTOOL_WECHAT_APPROVAL_TEMP
*&---------------------------------------------------------------------*
*& 工具 获取审批信息
*&---------------------------------------------------------------------*
REPORT ztool_wechat_approval_temp.

" 账号密码暂使用明文

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_text,
         text TYPE string,
         lang TYPE string,
       END OF ty_text,
       tt_text TYPE STANDARD TABLE OF ty_text WITH DEFAULT KEY,

       BEGIN OF ty_property,
         control     TYPE string,
         id          TYPE string,
         title       TYPE tt_text,
         placeholder TYPE tt_text,
         require     TYPE i,
         un_print    TYPE i,
       END OF ty_property,

       BEGIN OF ty_options,
         key   TYPE string,
         value TYPE tt_text,
       END OF ty_options,
       tt_options TYPE STANDARD TABLE OF ty_options WITH DEFAULT KEY,

       BEGIN OF ty_config_normal,
         type    TYPE string,
         mode    TYPE string,
         options TYPE tt_options,
       END OF ty_config_normal,
       tt_config_normal TYPE STANDARD TABLE OF ty_config_normal WITH DEFAULT KEY,

       BEGIN OF ty_config,
         name   TYPE string,
         config TYPE ty_config_normal,
       END OF ty_config,
       tt_config TYPE SORTED TABLE OF ty_config WITH UNIQUE KEY name,


       BEGIN OF ty_controls,
         property TYPE ty_property,
         config   TYPE tt_config, " => /UI2/CL_JSON ASSOC_ARRAYS
       END OF ty_controls,
       tt_controls TYPE STANDARD TABLE OF ty_controls WITH DEFAULT KEY,

       BEGIN OF ty_template_content,
         controls TYPE tt_controls,
       END OF ty_template_content.


*&----------------------------------------------------------------------
*                     Class
*&----------------------------------------------------------------------
CLASS lcl_wx_oa_fc_text DEFINITION DEFERRED.
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: gr_abap_edit TYPE REF TO cl_gui_abapedit,
      gr_popup     TYPE REF TO cl_gui_dialogbox_container,
      gr_event     TYPE REF TO lcl_event_receiver.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
PARAMETERS: p_tempid TYPE text200 DEFAULT `C4ZXKAttHPFgVA7gQh57zwCgzgeMQ9BN4aC3BVbud`.

SELECTION-SCREEN END OF BLOCK blck1.

CLASS lcl_wx_oa_fc_text DEFINITION.
  PUBLIC SECTION.

    METHODS constructor.

ENDCLASS.

CLASS lcl_wx_oa_fc_text IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

ENDCLASS.



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

" 解析

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_load_template_info.

  WRITE ''.


*&---------------------------------------------------------------------*
*& Form FRM_LOAD_TEMPLATE_INFO
*&---------------------------------------------------------------------*
*&  加载模板信息
*&---------------------------------------------------------------------*
FORM frm_load_template_info .
  DATA: BEGIN OF ls_res,
          errcode          TYPE i,
          errmsg           TYPE string,
          template_names   TYPE tt_text,
          template_content TYPE ty_template_content,
        END OF ls_res.

  DATA(l_client) = NEW zcl_wechat_http(
    corpid     = `xxxxxxxx`
    corpsecret = `xxxxxxxxxxxxxxxx`
  ).

  DATA: BEGIN OF ls_req,
          template_id TYPE string,
        END OF ls_req.
  DATA: l_res TYPE string.

  ls_req-template_id = p_tempid.

  l_client->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

  DATA(ecode) = l_client->post(
    EXPORTING
      url    = `https://qyapi.weixin.qq.com/cgi-bin/oa/gettemplatedetail`
      data   = ls_req
    IMPORTING
      result = l_res
  ).

  /ui2/cl_json=>deserialize(
    EXPORTING
      json             = l_res
      assoc_arrays     = abap_true
      assoc_arrays_opt = abap_true
    CHANGING
      data             = ls_res
  ).

  IF ls_res-errcode <> 0.
    MESSAGE ls_res-errmsg TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " 将内容转换为代码
  DATA: lt_string TYPE TABLE OF string.
  DATA: ls_text TYPE ty_text.

  DEFINE _read_text.
    CLEAR ls_text.
    READ TABLE &1 INTO ls_text WITH KEY lang = `zh_CN`.
    IF sy-subrc <> 0.
      READ TABLE &1 INTO ls_text INDEX 1.
    ELSE.
    ENDIF.
  END-OF-DEFINITION.

  APPEND |" ------------------- 接口初始化 ----------------------| TO lt_string.
  APPEND |DATA(l_approval) = NEW zcl_wechat_approval( corpid = `fixme` corpsecret = `fixme` ).| TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND |" ------------------- 审批赋值 ----------------------| TO lt_string.
  _read_text ls_res-template_names.
  APPEND |" 审批标题: { ls_text-text }| TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND |" ------------------- 基础赋值 ----------------------| TO lt_string.
  APPEND |DATA(l_fc) = NEW zcl_wx_oa_fc( ).| TO lt_string.
  APPEND `" 申请人userid` TO lt_string.
  APPEND |l_fc->creator_userid        = l_approval->userid( ).| TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND `" 模板id` TO lt_string.
  APPEND |l_fc->template_id           = '{ p_tempid }'.| TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND `" 审批人模式：0 指定、1 使用模板` TO lt_string.
  APPEND |l_fc->use_template_approver = { 1 }. | TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND `" 提单者提单部门id，不填默认为主部门` TO lt_string.
  APPEND |" l_fc->choose_department   = { 2 }.| TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND |" ------------------- 流程列表 ----------------------| TO lt_string.
  APPEND `" use_template_approver = 0 时必填` TO lt_string.
  APPEND `" 暂不支持` TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND |" ------------------- 摘要信息 ----------------------| TO lt_string.
  APPEND `" 摘要信息，用于显示在审批通知卡片、审批列表的摘要信息，最多3行` TO lt_string.
  APPEND `APPEND VALUE #( summary_info = VALUE #( ( text = '摘要1' lang = 'zh_CN' ) ) ) TO l_fc->summary_list.` TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND |" ------------------- 控件赋值 ----------------------| TO lt_string.
  APPEND INITIAL LINE TO lt_string.

  APPEND `DATA: l_if_fc TYPE REF TO zif_wx_oa_fc.` TO lt_string.

  LOOP AT ls_res-template_content-controls INTO DATA(ls_tmpc).
    DATA(l_index) = CONV numc2( sy-tabix ).

    _read_text ls_tmpc-property-title.
    APPEND |" 控件: { ls_tmpc-property-control } { ls_text-text }| TO lt_string.
    DATA(l_name) = |l_{ l_index }_{ to_lower( ls_tmpc-property-control ) }|.
    IF ls_tmpc-config IS NOT INITIAL AND ls_tmpc-config[ 1 ]-config-type IS NOT INITIAL.
      DATA(l_init_info) = ` type = ` && l_name && `=>` && ls_tmpc-config[ 1 ]-config-type && ` `.
    ELSE.
      CLEAR l_init_info.
    ENDIF.

    APPEND |DATA({ l_name }) = NEW zcl_wx_oa_fc_{ to_lower( ls_tmpc-property-control ) }( id = `{ ls_tmpc-property-id }`{ l_init_info }).| TO lt_string.

    " 必输 / 非必输
    IF ls_tmpc-property-require = 0.
      DATA(l_reqire) = `" `.
      APPEND `" 赋值:非必输` TO lt_string.
    ELSE.
      CLEAR l_reqire.
      APPEND `" 赋值:必输` TO lt_string.
    ENDIF.

    CASE ls_tmpc-property-control.
      WHEN `Text` OR `Textarea`.
        APPEND |{ l_reqire }{ l_name }->value-text = '<fixme>'.| TO lt_string.
      WHEN `Number`.
        APPEND |{ l_reqire }{ l_name }->value-new_number = '<fixme>'.| TO lt_string.
      WHEN `Money`.
        APPEND |{ l_reqire }{ l_name }->value-new_money = '<fixme>'.| TO lt_string.
      WHEN `Date`.
        APPEND |{ l_reqire }{ l_name }->value-date-s_timestamp = <fixme>.| TO lt_string.
      WHEN `Selector`.
        IF ls_tmpc-config[ 1 ]-config-type = `single`.
          DATA(l_selector) = `" `.
        ENDIF.
        LOOP AT ls_tmpc-config[ 1 ]-config-options INTO DATA(l_opt).
          _read_text l_opt-value.
          APPEND |{ l_reqire }{ l_selector }APPEND VALUE #( key = '{ l_opt-key }' ) TO { l_name }->value-selector-options. " { ls_text-text }| TO lt_string.
          CLEAR l_selector.
        ENDLOOP.
      WHEN `File`.
        APPEND |{ l_reqire }APPEND VALUE #(| TO lt_string.
        APPEND |{ l_reqire }  file_id   = '<fixme>' " 使用 zcl_wechat_approval->meida_upload( ) 上载获取| TO lt_string.
        APPEND |{ l_reqire }" file_name = '' " 可选| TO lt_string.
        APPEND |{ l_reqire }" file_size = '' " 可选| TO lt_string.
        APPEND |{ l_reqire }" file_type = '' " 可选| TO lt_string.
        APPEND |{ l_reqire }" file_url  = '' " 可选| TO lt_string.
        APPEND |{ l_reqire }) TO { l_name }->value-files.| TO lt_string.

      WHEN OTHERS.
        " 其他类型暂不支持
    ENDCASE.

    " 添加到正文
    APPEND `" 数据写入内容` TO lt_string.
    APPEND |{ l_reqire }l_if_fc ?= { l_name }.| TO lt_string.
    APPEND |{ l_reqire }APPEND l_if_fc TO l_fc->apply_data-contents.| TO lt_string.

    APPEND INITIAL LINE TO lt_string.

  ENDLOOP.

  APPEND |" ------------------- 发起审批 ----------------------| TO lt_string.
  APPEND `DATA: l_result TYPE string.` TO lt_string.
  APPEND `l_approval->send( EXPORTING data = l_fc IMPORTING result = l_result ).` TO lt_string.
  APPEND INITIAL LINE TO lt_string.


  gr_abap_edit = NEW #( parent = cl_gui_container=>screen0  ).

  gr_abap_edit->draw( ).
  gr_abap_edit->set_text( lt_string ).
  gr_abap_edit->undraw( ).

ENDFORM.
