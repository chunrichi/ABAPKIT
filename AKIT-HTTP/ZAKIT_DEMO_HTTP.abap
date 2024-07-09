REPORT zakit_demo_http.

" 关于 HTTP 简单的封装（简单的 postman 模拟)

" 该程序无相关类

" 目前只封装了 rest 接口，后续考虑增加 form-data


*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: sscrfields.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gt_exclude TYPE TABLE OF sy-ucomm.

DATA: gr_docking TYPE REF TO cl_gui_docking_container.
DATA: gr_splitter TYPE REF TO cl_gui_splitter_container.

DATA: gr_edit_o TYPE REF TO cl_gui_textedit,
      gr_edit_i TYPE REF TO cl_gui_textedit.

DATA: gt_head_btn_indx TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line,
      gt_head_hidden   TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.

CLASS lcl_http_util DEFINITION DEFERRED.
CLASS lcl_pretty_json DEFINITION DEFERRED.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_url FOR FIELD p_url.
    PARAMETERS: p_url TYPE text256.

    SELECTION-SCREEN COMMENT 58(5) t_type FOR FIELD p_type.
    PARAMETERS: p_type TYPE char10 AS LISTBOX VISIBLE LENGTH 6 DEFAULT 'POST' OBLIGATORY.

    SELECTION-SCREEN COMMENT 72(3) t_auth FOR FIELD p_auth.
    PARAMETERS: p_auth AS CHECKBOX USER-COMMAND auth.

    SELECTION-SCREEN COMMENT 79(6) t_head FOR FIELD p_head.
    PARAMETERS: p_head AS CHECKBOX USER-COMMAND head.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_uname FOR FIELD p_uname MODIF ID aut.
    PARAMETERS: p_uname TYPE text40 MODIF ID aut.

    SELECTION-SCREEN COMMENT 52(8) t_passw FOR FIELD p_passw MODIF ID aut.
    PARAMETERS: p_passw TYPE text40 MODIF ID aut.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  SELECTION-SCREEN COMMENT 2(20) t_hinfo MODIF ID hea.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_key01 FOR FIELD p_key01 MODIF ID hb1.
    PARAMETERS: p_key01 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb1.

    SELECTION-SCREEN COMMENT 38(8) t_val01 FOR FIELD p_val01 MODIF ID hb1.
    PARAMETERS: p_val01 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb1.

    " 激活
    SELECTION-SCREEN COMMENT 73(2) t_act01 FOR FIELD p_act01 MODIF ID hb1.
    PARAMETERS: p_act01 AS CHECKBOX MODIF ID hb1 DEFAULT 'X'.

    " 添加
    SELECTION-SCREEN PUSHBUTTON 79(4) t_btn01 USER-COMMAND bt1 MODIF ID hb1.

  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_key02 FOR FIELD p_key02 MODIF ID hb2.
    PARAMETERS: p_key02 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb2.
    SELECTION-SCREEN COMMENT 38(8) t_val02 FOR FIELD p_val02 MODIF ID hb2.
    PARAMETERS: p_val02 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb2.
    SELECTION-SCREEN COMMENT 73(2) t_act02 FOR FIELD p_act02 MODIF ID hb2.
    PARAMETERS: p_act02 AS CHECKBOX MODIF ID hb2 DEFAULT 'X'.
    SELECTION-SCREEN PUSHBUTTON 79(4) t_btn02 USER-COMMAND bt2 MODIF ID hb2.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_key03 FOR FIELD p_key03 MODIF ID hb3.
    PARAMETERS: p_key03 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb3.
    SELECTION-SCREEN COMMENT 38(8) t_val03 FOR FIELD p_val03 MODIF ID hb3.
    PARAMETERS: p_val03 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb3.
    SELECTION-SCREEN COMMENT 73(2) t_act03 FOR FIELD p_act03 MODIF ID hb3.
    PARAMETERS: p_act03 AS CHECKBOX MODIF ID hb3 DEFAULT 'X'.
    SELECTION-SCREEN PUSHBUTTON 79(4) t_btn03 USER-COMMAND bt3 MODIF ID hb3.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_key04 FOR FIELD p_key04 MODIF ID hb4.
    PARAMETERS: p_key04 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb4.
    SELECTION-SCREEN COMMENT 38(8) t_val04 FOR FIELD p_val04 MODIF ID hb4.
    PARAMETERS: p_val04 TYPE text40 VISIBLE LENGTH 24 MODIF ID hb4.
    SELECTION-SCREEN COMMENT 73(2) t_act04 FOR FIELD p_act04 MODIF ID hb4.
    PARAMETERS: p_act04 AS CHECKBOX MODIF ID hb4 DEFAULT 'X'.
    SELECTION-SCREEN PUSHBUTTON 79(4) t_btn04 USER-COMMAND bt4 MODIF ID hb4.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck3.

SELECTION-SCREEN FUNCTION KEY 1.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

  t_url  = 'URL'.
  t_type = 'TYPE'.
  t_auth = '认证'.
  t_head = 'MORE'.
  t_hinfo = 'Headers'.

  t_uname = '账号'.
  t_passw = '密码'.

  t_act01 = t_act02 = t_act03 = t_act04 = '激活'.
  t_key01 = t_key02 = t_key03 = t_key04 = 'Key'.
  t_val01 = t_val02 = t_val03 = t_val04 = 'Value'.
  t_btn01 = t_btn02 = t_btn03 = t_btn04 = '+'.
  t_btn04 = '-'.

  gt_head_btn_indx = VALUE #( ( 1 ) ).
  gt_head_hidden   = VALUE #( ( 2 ) ( 3 ) ( 4 ) ).

  " 下拉框
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TYPE'
      values = VALUE vrm_values( ( key = 'POST' text = 'POST' )
                                 ( key = 'GET'  text = 'GET' ) ).

  " --> 执行按钮
  APPEND 'ONLI' TO gt_exclude.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_exclude.
  " <--

  sscrfields-functxt_01 = VALUE smp_dyntxt(
    quickinfo = '测试执行'
    text      = '执行' ).

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  IF gr_docking IS INITIAL.
    " docker 初始化
    gr_docking = NEW #( repid = sy-repid dynnr = '1000' extension = 305 side  = cl_gui_docking_container=>dock_at_bottom ).
  ENDIF.

  IF gr_splitter IS INITIAL.
    gr_splitter = NEW #( parent = gr_docking rows = 1 columns = 2 ).

    gr_edit_o = NEW #(
      parent = gr_splitter->get_container( row = 1 column = 1 )
    ).

    gr_edit_i = NEW #(
      parent = gr_splitter->get_container( row = 1 column = 2 )
    ).
    gr_edit_i->set_readonly_mode( 1 ).
  ENDIF.

  LOOP AT SCREEN.
    IF p_auth = '' AND screen-group1 = 'AUT'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF p_head = '' AND ( screen-group1 = 'HEA' OR screen-group1+0(2) = 'HB' ).
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF p_head = 'X' AND screen-group1+0(2) = 'HB'.
      READ TABLE gt_head_btn_indx TRANSPORTING NO FIELDS WITH KEY table_line = screen-group1+2(1).
      IF sy-subrc <> 0.
        "IF NOT line_exists( gt_head_btn_indx[ screen-group1+2(1) ] ).
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name = 'P_PASSW'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

  IF sy-ucomm = 'FC01'.
    " 检查输入
    PERFORM frm_input_check.
    " 发起请求
    PERFORM frm_send_request.
  ENDIF.

  IF sy-ucomm+0(2) = 'BT'.
    " 按钮处理
    PERFORM frm_head_btn USING sy-ucomm+2(1).
  ENDIF.



*&----------------------------------------------------------------------
*                     Class definition
*&----------------------------------------------------------------------
CLASS lcl_http_util DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_token,
        token         TYPE text50,
        expires_in    TYPE i,
        exp_timestamp TYPE timestamp, " 过期时戳
      END OF ty_token .
    TYPES:
      BEGIN OF ty_result,
        type    TYPE bapi_mtype,
        message TYPE bapi_msg,
      END OF ty_result .

    DATA token TYPE ty_token .
    DATA result TYPE ty_result .
    DATA request_id TYPE string .
    DATA status_code TYPE i .

    " PROTECTED SECTION.
    " PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ty_key_value .
    TYPES:
      tty_key_value TYPE TABLE OF ty_key_value .

    METHODS post
      IMPORTING
        !url        TYPE string
        !headers    TYPE tty_key_value OPTIONAL
        !params     TYPE tty_key_value OPTIONAL
        !json       TYPE string OPTIONAL
      RETURNING
        VALUE(data) TYPE string .
    METHODS get
      IMPORTING
        !url        TYPE string
        !headers    TYPE tty_key_value OPTIONAL
        !params     TYPE tty_key_value OPTIONAL
      RETURNING
        VALUE(data) TYPE string .
    METHODS code_desc
      IMPORTING
        VALUE(code)   TYPE i OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_result .
ENDCLASS.

CLASS lcl_pretty_json DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: pretty IMPORTING json               TYPE string
                          RETURNING VALUE(pretty_json) TYPE string.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Form frm_input_check
*&---------------------------------------------------------------------*
*&  检查输入
*&---------------------------------------------------------------------*
FORM frm_input_check .

  " 屏幕中 url 必输检查
  IF p_url IS INITIAL.
    MESSAGE 'url必输，请输入url!' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_send_request
*&---------------------------------------------------------------------*
*&  发起请求
*&---------------------------------------------------------------------*
FORM frm_send_request .
  TYPES:
    BEGIN OF ty_key_value,
      key   TYPE string,
      value TYPE string,
    END OF ty_key_value .

  DATA: lv_req_string TYPE string,
        lv_res_string TYPE string.

  DATA: lr_request TYPE REF TO lcl_http_util.

  DATA: lv_str  TYPE string,
        lv_xstr TYPE xstring.

  DATA: lt_headers TYPE TABLE OF ty_key_value.

  DATA: lv_head_field TYPE string,
        lv_numc2      TYPE numc2.

  lr_request = NEW #( ).

  " 发出去的内容
  gr_edit_o->get_textstream( IMPORTING text = lv_req_string ).
  cl_gui_cfw=>flush( ).

  " --> 认证
  IF p_auth IS NOT INITIAL AND p_uname IS NOT INITIAL.
    lv_str = |{ p_uname }:{ p_passw }|.
    " STR -> XSTR
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_str
      IMPORTING
        buffer = lv_xstr
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    " xstr -> base64
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = lv_xstr
      IMPORTING
        output = lv_str.
    lv_str = |Basic { lv_str }|.

    lt_headers = VALUE #( ( key = 'Authorization' value = lv_str ) ).
  ENDIF.
  " <--

  " --> 头内容填写
  DO 4 TIMES.
    lv_numc2 = sy-index.
    lv_head_field = |p_act{ lv_numc2 }|.
    ASSIGN (lv_head_field) TO FIELD-SYMBOL(<lv_act>).
    IF sy-subrc = 0.
      IF <lv_act> = 'X'.
        lv_head_field = |p_key{ lv_numc2 }|.
        ASSIGN (lv_head_field) TO FIELD-SYMBOL(<lv_key>).
        IF sy-subrc = 0 AND <lv_key> IS NOT INITIAL.
          " 仅考虑 key 有值的内容
          APPEND INITIAL LINE TO lt_headers ASSIGNING FIELD-SYMBOL(<ls_head>).
          <ls_head>-key = <lv_key>.
        ELSE.
          CONTINUE.
        ENDIF.

        lv_head_field = |p_val{ lv_numc2 }|.
        ASSIGN (lv_head_field) TO FIELD-SYMBOL(<lv_val>).
        IF sy-subrc = 0.
          <ls_head>-value = <lv_val>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.
  " <--

  CASE p_type.
    WHEN 'POST'.
      " POST 请求
      lv_res_string = lr_request->post( url = |{ p_url }|
                                    headers = lt_headers
                                       json = lv_req_string ).

    WHEN 'GET'.
      " GET 请求
      lv_res_string = lr_request->get( url = |{ p_url }|
                                   headers = lt_headers ).

    WHEN OTHERS.
  ENDCASE.

  IF lr_request->result-type = 'E'.
    MESSAGE lr_request->result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  " 写入返回的内容
  IF lv_res_string IS NOT INITIAL.
    TRY.
        lv_res_string = lcl_pretty_json=>pretty( lv_res_string ).
      CATCH cx_root.
    ENDTRY.

    gr_edit_i->set_textstream( EXPORTING text =  lv_res_string ).
  ENDIF.
  cl_gui_cfw=>flush( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_HEAD_BTN
*&---------------------------------------------------------------------*
*&  HEADERS BTN
*&---------------------------------------------------------------------*
FORM frm_head_btn USING p_indx.
  DATA: lv_field TYPE string.
  DATA: lv_indi TYPE i,
        lv_indx TYPE numc2.
  DATA: lv_btn TYPE char1.
  DATA: lv_lines TYPE i.

  lv_indx = lv_indi = p_indx.

  " 更新按钮文本
  lv_field = |T_BTN{ lv_indx }|.
  ASSIGN (lv_field) TO FIELD-SYMBOL(<lv_btn>).
  IF sy-subrc = 0.
    lv_btn = <lv_btn>.
    <lv_btn> = '-'.
  ENDIF.

  " 展示处理
  IF lv_btn = '+'.
    READ TABLE gt_head_hidden INTO DATA(lv_hid) INDEX 1.
    IF sy-subrc = 0.
      INSERT lv_hid INTO TABLE gt_head_btn_indx .
      DELETE gt_head_hidden INDEX 1.
    ENDIF.
  ELSE.
    lv_field = |p_act{ lv_indx }|.
    ASSIGN (lv_field) TO FIELD-SYMBOL(<lv_act>).
    IF sy-subrc = 0.
      <lv_act> = ''.
    ENDIF.

    DELETE gt_head_btn_indx WHERE table_line = lv_indi.
    INSERT lv_indi INTO TABLE gt_head_hidden.
  ENDIF.

  lv_lines = lines( gt_head_btn_indx ).
  IF lv_lines < 4.
    READ TABLE gt_head_btn_indx INTO DATA(lv_btn_last) INDEX lv_lines.
    IF sy-subrc = 0.
      lv_indx = lv_btn_last.
      lv_field = |T_BTN{ lv_indx }|.
      ASSIGN (lv_field) TO <lv_btn>.
      IF sy-subrc = 0.
        <lv_btn> = '+'.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.

CLASS lcl_http_util IMPLEMENTATION.

  METHOD get.

    " HTTP 请求 GET 处理

    " 开发日期: 20221030

    DATA: lv_url TYPE string.
    DATA: lr_http_client TYPE REF TO if_http_client.

    DATA: lv_debug     TYPE abap_bool,
          lv_error_msg TYPE string.

    " 必输校验
    IF url IS INITIAL.
      RETURN.
    ENDIF.

    " url 内容赋值
    lv_url = url.

    IF params IS SUPPLIED.

      LOOP AT params INTO DATA(ls_params).
        cl_http_server=>append_field_url( EXPORTING name = ls_params-key value = ls_params-value CHANGING url = lv_url ).
      ENDLOOP.

    ENDIF.

    " 请求初始化
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lr_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 7.
    IF sy-subrc <> 0.
      me->result-type = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO me->result-message.
      RETURN.
    ENDIF.

    " 设置http协议版本
    " lr_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    " 设置http请求方法
    lr_http_client->request->set_method( if_http_request=>co_request_method_get ).

    " 头部信息 设定传输请求内容格式以及编码格式
    IF headers IS SUPPLIED.

      LOOP AT headers INTO DATA(ls_headers).
        lr_http_client->request->set_header_field( name = ls_headers-key value = ls_headers-value ).
      ENDLOOP.

    ENDIF.

    " 设置请求参数（请求参数也可以直接写在URI）
    "lr_http_client->request->set_form_field( name = 'grant_type' value = 'password' ).
    "lr_http_client->request->set_form_field( name = 'username' value = lv_username ).
    "lr_http_client->request->set_form_field( name = 'password' value = lv_password ).


    CALL METHOD lr_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    CHECK sy-subrc = 0.

    IF lv_debug = 'X'.
      DATA(lv_request_xstring) = lr_http_client->request->to_xstring( ).
    ENDIF.

    CALL METHOD lr_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      lr_http_client->get_last_error(
        IMPORTING
          code    = DATA(lv_subrc)
          message = lv_error_msg
      ).
      me->result = VALUE #( type = 'E' message = |{ lv_error_msg }| ).
      RETURN.
    ENDIF.

    IF lv_debug = 'X'.
      DATA(lv_response_xstring) = lr_http_client->response->to_xstring( ).
    ENDIF.

    lr_http_client->response->get_status( IMPORTING code = me->status_code ).
    me->result = me->code_desc( me->status_code ).
    CHECK me->result-type = 'S'.

    data = lr_http_client->response->get_cdata( ).

    lr_http_client->close( ).
    CLEAR: lr_http_client.

  ENDMETHOD.

  METHOD code_desc.

    DATA: lv_code TYPE i.

    IF code IS SUPPLIED.
      lv_code = code.
    ELSE.
      lv_code = me->status_code.
    ENDIF.

    CASE me->status_code.
      WHEN 200.
        result = VALUE #( type = 'S' message = 'Success, Ok' ).
      WHEN 302.
        result = VALUE #( type = 'E' message = 'Resource access temporarily redirected (HTTP 302). Check the URL' ).
      WHEN 401.
        result = VALUE #( type = 'E' message = 'Unauthorized access to resource (HTTP 401). Check your credentials' ).
      WHEN 403.
        result = VALUE #( type = 'E' message = 'Access to resource forbidden (HTTP 403)' ).
      WHEN 404.
        result = VALUE #( type = 'E' message = 'Resource not found (HTTP 404). Check the URL' ).
      WHEN 407.
        result = VALUE #( type = 'E' message = 'Proxy authentication required (HTTP 407). Check your credentials' ).
      WHEN 408.
        result = VALUE #( type = 'E' message = 'Request timeout (HTTP 408)' ).
      WHEN 415.
        result = VALUE #( type = 'E' message = 'Unsupported media type (HTTP 415)' ).
      WHEN 422.
        result = VALUE #( type = 'E' message = 'Unprocessable entity (HTTP 422). Check, if URL has to end with ".git"' ).
      WHEN OTHERS.
        result = VALUE #( type = 'E' message = |(HTTP { lv_code })| ).
    ENDCASE.

  ENDMETHOD.

  METHOD post.

    " HTTP 请求 POST 处理

    " 开发日期: 20221030

    DATA: lv_url TYPE string.
    DATA: lr_http_client TYPE REF TO if_http_client.

    DATA: lv_debug     TYPE abap_bool,
          lv_error_msg TYPE string.

    " 必输校验
    IF url IS INITIAL.
      RETURN.
    ENDIF.

    " url 内容赋值
    lv_url = url.

    IF params IS SUPPLIED.

      LOOP AT params INTO DATA(ls_params).
        cl_http_server=>append_field_url( EXPORTING name = ls_params-key value = ls_params-value CHANGING url = lv_url ).
      ENDLOOP.

    ENDIF.

    " 请求初始化
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lr_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 7.
    IF sy-subrc <> 0.
      me->result-type = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO me->result-message.
      RETURN.
    ENDIF.

    " 设置http协议版本
    " lr_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    " 设置http请求方法
    lr_http_client->request->set_method( if_http_request=>co_request_method_post ).

    " 头部信息 设定传输请求内容格式以及编码格式
    IF headers IS SUPPLIED.

      LOOP AT headers INTO DATA(ls_headers).
        lr_http_client->request->set_header_field( name = ls_headers-key value = ls_headers-value ).
      ENDLOOP.

    ENDIF.

    " 设置请求参数（请求参数也可以直接写在URI）
    "lr_http_client->request->set_form_field( name = 'grant_type' value = 'password' ).
    "lr_http_client->request->set_form_field( name = 'username' value = lv_username ).
    "lr_http_client->request->set_form_field( name = 'password' value = lv_password ).

    IF json IS SUPPLIED.
      lr_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).

      lr_http_client->request->set_cdata( data = json ).
    ENDIF.

    CALL METHOD lr_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    CHECK sy-subrc = 0.

    IF lv_debug = 'X'.
      DATA(lv_request_xstring) = lr_http_client->request->to_xstring( ).
    ENDIF.

    CALL METHOD lr_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      lr_http_client->get_last_error(
        IMPORTING
          code    = DATA(lv_subrc)
          message = lv_error_msg
      ).
      me->result = VALUE #( type = 'E' message = |{ lv_error_msg }| ).
      RETURN.
    ENDIF.

    IF lv_debug = 'X'.
      DATA(lv_response_xstring) = lr_http_client->response->to_xstring( ).
    ENDIF.

    lr_http_client->response->get_status( IMPORTING code = me->status_code ).

    data = lr_http_client->response->get_cdata( ).

    lr_http_client->close( ).
    CLEAR: lr_http_client.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_pretty_json IMPLEMENTATION.
  METHOD pretty.

    "cloud
    "DATA(json_xstring) = cl_abap_conv_codepage=>create_out( )->convert( json ).
    "on_premise
    DATA(json_xstring) = cl_abap_codepage=>convert_to( json ).

    "Check and pretty print JSON

    DATA(reader) = cl_sxml_string_reader=>create( json_xstring ).
    DATA(writer) = CAST if_sxml_writer(
                          cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    reader->next_node( ).
    reader->skip_node( writer ).

    "cloud
    "DATA(json_formatted_string) = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( writer )->get_output( ) ).
    "on premise
    DATA(json_formatted_string) = cl_abap_codepage=>convert_from( CAST cl_sxml_string_writer( writer )->get_output( ) ).

    pretty_json = escape( val = json_formatted_string format = cl_abap_format=>e_xml_text  ).

  ENDMETHOD.
ENDCLASS.