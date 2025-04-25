CLASS zcl_wechat_http DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_kv,
        key   TYPE string,
        value TYPE string,
      END OF ty_kv .
    TYPES:
      tt_kv TYPE TABLE OF ty_kv .

    DATA g_pretty_name TYPE /ui2/cl_json=>pretty_name_mode VALUE `` ##NO_TEXT.
    DATA g_read_mode TYPE /ui2/cl_json=>pretty_name_mode VALUE `` ##NO_TEXT.
    DATA g_error_message TYPE string .

    METHODS constructor
      IMPORTING
        !corpid     TYPE string
        !corpsecret TYPE string .
    METHODS post
      IMPORTING
        !url           TYPE string
        VALUE(params)  TYPE tt_kv OPTIONAL
        VALUE(headers) TYPE tt_kv OPTIONAL
        !data          TYPE data OPTIONAL
      EXPORTING
        !result        TYPE data
      RETURNING
        VALUE(ecode)   TYPE i .
    METHODS get
      IMPORTING
        !url           TYPE string
        VALUE(params)  TYPE tt_kv OPTIONAL
        VALUE(headers) TYPE tt_kv OPTIONAL
      EXPORTING
        !result        TYPE data
      RETURNING
        VALUE(ecode)   TYPE i .
    METHODS userid
      IMPORTING
        !phone       TYPE string
      EXPORTING
        !userid      TYPE string
      RETURNING
        VALUE(ecode) TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: corpid     TYPE string, " 企业 ID
          corpsecret TYPE string. " 应用的凭证密钥

    DATA: BEGIN OF token_cache,
            timestamp TYPE timestamp,
            token     TYPE string,
          END OF token_cache.

    METHODS request
      IMPORTING
        !method      TYPE string
        !url         TYPE string
        !params      TYPE tt_kv OPTIONAL
        !headers     TYPE tt_kv OPTIONAL
        !data        TYPE data OPTIONAL
      EXPORTING
        !result      TYPE data
      RETURNING
        VALUE(ecode) TYPE i .
    METHODS access_token .
ENDCLASS.



CLASS ZCL_WECHAT_HTTP IMPLEMENTATION.


  METHOD access_token.
    DATA: l_timestamp_now TYPE timestamp,
          l_timestamp_cal TYPE timestamp.
    DATA: BEGIN OF result,
            errcode      TYPE i,
            errmsg       TYPE string,
            access_token TYPE string,
            expires_in   TYPE i,
          END OF result.

    GET TIME STAMP FIELD l_timestamp_now.

    " TIMESTAMP + 10sec[buffer] = 提前 10 秒重置 token
    l_timestamp_cal = cl_abap_tstmp=>add_to_short( tstmp = l_timestamp_now secs = 10 ).

    " 内部取值
    IF me->token_cache IS NOT INITIAL
      AND me->token_cache-timestamp > l_timestamp_cal.
      " 当期 cache 可用：直接返回
      RETURN.
    ELSE.
      " 重置缓存
      CLEAR: me->token_cache.
    ENDIF.

    " TODO: 缓存取值 =》从自建表拉取

    " 重置取值
    GET TIME STAMP FIELD l_timestamp_now.

    me->request(
      EXPORTING
        method = 'GET'
        url    = `https://qyapi.weixin.qq.com/cgi-bin/gettoken`
        params = VALUE #( ( key = `corpid`     value = me->corpid )
                          ( key = `corpsecret` value = me->corpsecret ) )
      IMPORTING
        result = result
    ).

    IF result-errcode = 0.
      me->token_cache-token = result-access_token.
      me->token_cache-timestamp = cl_abap_tstmp=>add_to_short( tstmp = l_timestamp_now secs = result-expires_in ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    me->corpid     = corpid.
    me->corpsecret = corpsecret.

    me->access_token( ).
  ENDMETHOD.


  METHOD get.
    me->access_token( ).

    READ TABLE params TRANSPORTING NO FIELDS WITH KEY key = `access_token`.
    IF sy-subrc <> 0.
      APPEND VALUE #( key = `access_token` value = me->token_cache-token ) TO params.
    ENDIF.

    ecode = me->request(
      EXPORTING
        method  = 'GET'
        url     = url
        params  = params
        headers = headers
      IMPORTING
        result  = result
     ).
  ENDMETHOD.


  METHOD post.

    me->access_token( ).

    READ TABLE params TRANSPORTING NO FIELDS WITH KEY key = `access_token`.
    IF sy-subrc <> 0.
      APPEND VALUE #( key = `access_token` value = me->token_cache-token ) TO params.
    ENDIF.

    ecode = me->request(
      EXPORTING
        method  = 'POST'
        url     = url
        params  = params
        headers = headers
        data    = data
      IMPORTING
        result  = result
     ).
  ENDMETHOD.


  METHOD request.

    DATA: l_req_json TYPE string,
          l_res_json TYPE string.
    DATA: lv_url         TYPE string,
          lr_http_client TYPE REF TO if_http_client.

    IF data IS NOT INITIAL.
      DESCRIBE FIELD data TYPE DATA(l_dt).

      CASE l_dt.
        WHEN 'l' OR 'u' OR 'v' OR 'h' OR 'r'. " data ref/flat struct/deep struct/table/classObject
          l_req_json = /ui2/cl_json=>serialize( data = data
                                        pretty_name = me->g_pretty_name ).
        WHEN OTHERS.
          l_req_json = data.
      ENDCASE.
    ENDIF.

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
      ecode = 500.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO me->g_error_message.
      RETURN.
    ENDIF.

    " 设置http协议版本
    " lr_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    " 设置http请求方法
    lr_http_client->request->set_method( method ).

    lr_http_client->request->set_header_field( name = `Content-Type` value = `application/json` ).
    lr_http_client->request->set_header_field( name = `Accept` value = `application/json` ).

    IF headers IS SUPPLIED.

      LOOP AT headers INTO DATA(ls_headers).
        lr_http_client->request->set_header_field( name = ls_headers-key value = ls_headers-value ).
      ENDLOOP.

    ENDIF.

    IF l_req_json IS NOT INITIAL.
      lr_http_client->request->set_cdata( data = l_req_json ).
    ENDIF.

    CALL METHOD lr_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      lr_http_client->get_last_error(
        IMPORTING
          code    = DATA(lv_subrc)
          message = DATA(lv_error_msg)
      ).
      ecode = '500'."错误
      me->g_error_message = 'Code:' && lv_subrc && 'message:' && lv_error_msg.
      RETURN.
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
          code    = lv_subrc
          message = lv_error_msg
      ).
      ecode = '500'."错误
      me->g_error_message = 'Code:' && lv_subrc && 'message:' && lv_error_msg.
      RETURN.
    ENDIF.

    lr_http_client->response->get_status( IMPORTING code = DATA(lv_status_code)
                                                  reason = DATA(lv_status_reason) ).

    ecode = lv_status_code.
    l_res_json = lr_http_client->response->get_cdata( ).

    IF ecode <> 200.
      me->g_error_message = lv_status_reason && ';'.
    ENDIF.

    " 关闭请求
    lr_http_client->close( ).

    " 返回格式化
    DESCRIBE FIELD result TYPE l_dt.

    CASE l_dt.
      WHEN 'l' OR 'u' OR 'v' OR 'h' OR 'r'. " data ref/flat struct/deep struct/table/classObject
        /ui2/cl_json=>deserialize(
                           EXPORTING
                             json        = l_res_json
                             pretty_name = me->g_read_mode
                           CHANGING data = result ).
      WHEN OTHERS.
        result = l_res_json.
    ENDCASE.

  ENDMETHOD.


  METHOD userid.

    DATA: BEGIN OF ls_req,
            mobile TYPE string,
          END OF ls_req,
          BEGIN OF ls_res,
            errcode TYPE i,
            errmsg  TYPE string,
            userid  TYPE string,
          END OF ls_res.

    ls_req-mobile = phone.

    me->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

    ecode = me->post(
      EXPORTING
        url    = `https://qyapi.weixin.qq.com/cgi-bin/user/getuserid`
        data   = ls_req
      IMPORTING
        result = ls_res
    ).

    IF ecode = 200 AND ls_res-errcode = 0.
      userid = ls_res-userid.
    ELSE.
      IF ls_res-errcode <> 0.
        me->g_error_message = ls_res-errmsg.
      ENDIF.
    ENDIF.

    CLEAR: me->g_pretty_name,
           me->g_read_mode.
  ENDMETHOD.
ENDCLASS.
