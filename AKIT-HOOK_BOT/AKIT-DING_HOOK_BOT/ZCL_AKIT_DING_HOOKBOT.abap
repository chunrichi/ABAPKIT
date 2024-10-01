CLASS zcl_akit_ding_hookbot DEFINITION
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
    TYPES:
      BEGIN OF ty_ding_result,
        errcode TYPE i,
        errmsg  TYPE string,
      END OF ty_ding_result .
    TYPES:
      BEGIN OF ty_at,
        " @ 特定人员
        is_at_all   TYPE abap_bool,
        at_user_ids TYPE STANDARD TABLE OF string WITH EMPTY KEY,
        at_mobiles  TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      END OF ty_at .

    DATA:
      BEGIN OF botv,
        " 机器人相关配置
        url   TYPE string,
        token TYPE string,
      END OF botv .
    DATA result TYPE ty_result .
    DATA status_code TYPE i .
    " fix time
    DATA fix_time TYPE p LENGTH 10 DECIMALS 3.

    METHODS constructor
      IMPORTING
        !url   TYPE string
        !token TYPE string OPTIONAL .
    METHODS push
      IMPORTING
        !content      TYPE string
        !at           TYPE ty_at OPTIONAL
        !msg_type     TYPE string DEFAULT 'text'
      RETURNING
        VALUE(result) TYPE ty_result .
    METHODS genjson_str
      IMPORTING
        !ijson       TYPE string
        !msg_type    TYPE string DEFAULT 'text'
        !at          TYPE ty_at OPTIONAL
      RETURNING
        VALUE(rjson) TYPE string .
    METHODS gensign .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_send_base,
        " 发送基础结构
        timestamp TYPE string,
        sign      TYPE string,
        msg_type  TYPE string,
      END OF ty_send_base .
    TYPES:
      BEGIN OF ty_key_value,
        " 头内容 key value
        key   TYPE string,
        value TYPE string,
      END OF ty_key_value .
    TYPES:
      tty_key_value TYPE TABLE OF ty_key_value .

    DATA send_base TYPE ty_send_base .

    METHODS post
      IMPORTING
        !url        TYPE string
        !headers    TYPE tty_key_value OPTIONAL
        !params     TYPE tty_key_value OPTIONAL
        !json       TYPE string OPTIONAL
      RETURNING
        VALUE(data) TYPE string .
ENDCLASS.



CLASS ZCL_AKIT_DING_HOOKBOT IMPLEMENTATION.


  METHOD constructor.

    me->botv-url = url.
    me->botv-token = token.

  ENDMETHOD.


  METHOD genjson_str.

    DATA: lv_at TYPE string.

    " @
    IF at IS NOT INITIAL.
      lv_at = /ui2/cl_json=>serialize(
         EXPORTING data = at
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case  ).

      rjson = `{`
      && |  "msgtype": "{ msg_type }",|
      && |  "at": { lv_at },|
      && |  "{ msg_type }": |
      && ijson
      && `}`.
    ELSE.
      rjson = `{`
      && |  "msgtype": "{ msg_type }",|
      && |  "{ msg_type }": |
      && ijson
      && `}`.
    ENDIF.

  ENDMETHOD.


  METHOD gensign.

    " 签名生成

    DATA: lv_timestampl TYPE timestampl,
          lv_timestamp  TYPE timestamp,
          lv_cstamp     TYPE string,
          lv_msecond    TYPE i.
    DATA: lv_sign TYPE string.
    DATA: lv_hmac_result TYPE string.

    CHECK me->botv-token IS NOT INITIAL.

    GET TIME STAMP FIELD lv_timestampl.

    " 计算时戳 秒（从 19700101 开始）
    CONVERT TIME STAMP lv_timestampl TIME ZONE ' ' INTO DATE DATA(lv_date)
                                                       TIME DATA(lv_time).

    lv_timestamp = lv_time.
    lv_timestamp = lv_timestamp + lv_date * 86400.
    lv_timestamp = lv_timestamp - ( CONV datum( '19700101' ) * 86400 ).

    " 毫秒
    lv_cstamp = lv_timestampl.
    SPLIT lv_cstamp AT '.' INTO TABLE DATA(lt_val).
    IF sy-subrc = 0.
      lv_msecond = substring( val = |{ VALUE #( lt_val[ 2 ] OPTIONAL ) WIDTH = 3 PAD = '0' }|
                              off = 0
                              len = 3 ).
    ENDIF.

    lv_timestamp = lv_timestamp * 1000 + lv_msecond. " + 毫秒

    " 针对系统时间不准增加的时间处理
    IF me->fix_time IS NOT INITIAL.
      lv_timestamp = lv_timestamp + me->fix_time * 1000.
    ENDIF.

    " 加密
    lv_sign = |{ lv_timestamp }\n{ me->botv-token }|.

    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
                if_algorithm           = 'SHA256'
                if_key                 = cl_abap_hmac=>string_to_xstring( lv_sign )
                if_data                = cl_abap_hmac=>string_to_xstring( me->botv-token )
              IMPORTING
                ef_hmacb64string       = lv_hmac_result
        ).
      CATCH cx_abap_message_digest.
        " pass
    ENDTRY.

    " urlencode
    me->send_base-sign = cl_http_utility=>if_http_utility~escape_url( lv_hmac_result ).
    me->send_base-timestamp = lv_timestamp.

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


  METHOD push.

    " 文档:
    "  - 说明 https://open.dingtalk.com/document/robots/custom-robot-access
    "  - 加密 https://open.dingtalk.com/document/robots/customize-robot-security-settings

    " 每分钟最多发送20条消息，如果超过20条，会限流10分钟

    " 生成发送数据
    DATA: lv_res_string TYPE string,
          lv_req_string TYPE string.
    DATA: ls_res TYPE ty_ding_result.

    CHECK content IS NOT INITIAL.

    CHECK msg_type = `text`
       OR msg_type = `link`
       OR msg_type = `markdown`
       OR msg_type = `actionCard`
       OR msg_type = `feedCard`.

    IF at IS NOT INITIAL.
      CHECK msg_type = `text`
         OR msg_type = `markdown`.
    ENDIF.

    " 生成密匙
    me->gensign( ).

    lv_req_string = me->genjson_str( ijson = content
                                  msg_type = msg_type
                                        at = at ).

    " 发送消息
    lv_res_string = me->post( url = me->botv-url
                           params = COND #( WHEN me->botv-token IS NOT INITIAL
                                            THEN VALUE #(
                                      ( key = `timestamp` value = me->send_base-timestamp )
                                      ( key = `sign`      value = me->send_base-sign ) ) )
                             json = lv_req_string ).

    " 返回消息处理
    IF me->result-type = 'E'.
      result = me->result.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING json = lv_res_string
       CHANGING data = ls_res ).

    result-type = COND #( WHEN ls_res-errcode = 0 THEN 'S' ELSE 'E' ).
    result-message = ls_res-errmsg.

  ENDMETHOD.
ENDCLASS.