CLASS zcl_akit_weixin_hook_bot DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_result,
        " 返回消息
        type    TYPE bapi_mtype,
        message TYPE bapi_msg,
      END OF ty_result .
    TYPES:
      BEGIN OF ty_formdata,
        filename     TYPE string,
        content_type TYPE string,
        raw          TYPE xstring,
      END OF ty_formdata .
    TYPES:
      BEGIN OF ty_file_result,
        type       TYPE bapi_mtype,
        message    TYPE bapi_msg,
        filetype   TYPE string,
        media_id   TYPE string,
        created_at TYPE string,
      END OF ty_file_result .

    DATA:
      BEGIN OF botv,
        " 机器人相关配置
        url TYPE string,
      END OF botv .
    DATA result TYPE ty_result .
    DATA status_code TYPE i .

    METHODS constructor
      IMPORTING
        !url TYPE string .
    METHODS fileupload
      IMPORTING
        !raw          TYPE ty_formdata
      RETURNING
        VALUE(result) TYPE ty_file_result .
    METHODS push
      IMPORTING
        !msg_type     TYPE string
        !content      TYPE string
      RETURNING
        VALUE(result) TYPE ty_result .
    CLASS-METHODS genimage
      IMPORTING
        !content    TYPE xstring
      RETURNING
        VALUE(json) TYPE string .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_key_value,
        " 头内容 key value
        key   TYPE string,
        value TYPE string,
      END OF ty_key_value .
    TYPES:
      tty_key_value TYPE TABLE OF ty_key_value .
    TYPES:
      BEGIN OF ty_weixin_result,
        errcode    TYPE i,
        errmsg     TYPE string,
        type       TYPE string,  " 消息推送不用
        media_id   TYPE string,  " 消息推送不用
        created_at TYPE string,  " 消息推送不用
      END OF ty_weixin_result .

    METHODS post
      IMPORTING
        !url        TYPE string
        !headers    TYPE tty_key_value OPTIONAL
        !params     TYPE tty_key_value OPTIONAL
        !json       TYPE string OPTIONAL
        !raw        TYPE ty_formdata OPTIONAL
      RETURNING
        VALUE(data) TYPE string .
    METHODS genjson_str
      IMPORTING
        !ijson       TYPE string
        !msg_type    TYPE string DEFAULT 'text'
      RETURNING
        VALUE(rjson) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AKIT_WEIXIN_HOOK_BOT IMPLEMENTATION.


  METHOD constructor.

    me->botv-url = url.

  ENDMETHOD.


  METHOD fileupload.

    " 素材上传得到media_id，该media_id仅三天内有效
    " media_id只能是对应上传文件的机器人可以使用

    " 文件上载大小在5B~20M之间

    DATA: lv_res_string TYPE string,
          ls_res        TYPE ty_weixin_result.
    DATA: lv_url_key TYPE string.

    IF NOT ( raw-content_type = `image`
      OR raw-content_type = `voice`
      OR raw-content_type = `video`
      OR raw-content_type = `file` ).
      result-type = 'E'.
      result-message = `error content`.
    ENDIF.

    FIND REGEX `key=([^&]*)` IN me->botv-url
      SUBMATCHES lv_url_key.


    lv_res_string = me->post(
      url    = `https://qyapi.weixin.qq.com/cgi-bin/webhook/upload_media`
      params = VALUE #(
          ( key = `key`  value = lv_url_key )
          ( key = `type` value = raw-content_type )
        )
      raw    = raw
    ).

    " 返回消息处理
    IF me->result-type = 'E'.
      MOVE-CORRESPONDING me->result TO result.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING json = lv_res_string
       CHANGING data = ls_res ).

    result-type = COND #( WHEN ls_res-errcode = 0 THEN 'S' ELSE 'E' ).
    result-message = ls_res-errmsg.

    result-filetype = ls_res-type.
    result-media_id = ls_res-media_id.
    result-created_at = ls_res-created_at.

  ENDMETHOD.


  METHOD genimage.
    DATA: lv_base64 TYPE string,
          lv_md5    TYPE string.

    " BASE64
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = content
      IMPORTING
        output = lv_base64.

    " MD5
    " CALCULATE_HASH_FOR_RAW
    " CALCULATE_HASH_FOR_CHAR
    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        alg            = 'MD5'
        data           = content
      IMPORTING
        hashstring     = lv_md5
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.

    json = `{`
    && |"base64": "{ lv_base64 }",|
    && |"md5": "{ lv_md5 }"|
    && `}`.

  ENDMETHOD.


  METHOD genjson_str.

    " 生成报文

    rjson = `{`
    && |  "msgtype": "{ msg_type }",|
    && |  "{ msg_type }": |
    && ijson
    && `}`.

  ENDMETHOD.


  METHOD post.

    " HTTP 请求 POST 处理

    " 开发日期: 20221030

    DATA: lv_url TYPE string.
    DATA: lr_http_client TYPE REF TO if_http_client.

    DATA: lv_debug     TYPE abap_bool,
          lv_error_msg TYPE string.

    " 必输校验
    IF url IS INITIAL
      OR ( json IS SUPPLIED AND raw IS SUPPLIED ).
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

    IF raw IS SUPPLIED.
      lr_http_client->request->set_content_type( content_type = 'multipart/form-data' ).
      lr_http_client->request->set_formfield_encoding( formfield_encoding = cl_http_request=>if_http_entity~co_encoding_raw ).
      DATA(lr_part) = lr_http_client->request->if_http_entity~add_multipart( ).

      lr_part->set_header_field( name  = 'Content-Disposition'
                                 value = |form-data; name="{ `media`
                                 }"; filename="{ raw-filename
                                 }"; filename*="UTF-8''{ cl_http_utility=>if_http_utility~escape_url( raw-filename ) }";| ).

      lr_part->set_content_type( COND #( WHEN raw-content_type IS INITIAL
                                         THEN `application/octet-stream`
                                         ELSE raw-content_type ) ).

      lr_part->set_data( data = raw-raw
                         offset = 0
                         length = xstrlen( raw-raw ) ).
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

  ENDMETHOD.


  METHOD push.

    " 每个机器人发送的消息不能超过20条/分钟。
    " 文档: https://developer.work.weixin.qq.com/document/path/91770

    " 生成发送数据
    DATA: lv_res_string TYPE string,
          lv_req_string TYPE string.
    DATA: ls_res TYPE ty_weixin_result.

    CHECK content IS NOT INITIAL.

    CHECK msg_type = `text`
       OR msg_type = `markdown`
       OR msg_type = `image`
       OR msg_type = `news`
       OR msg_type = `file`
       OR msg_type = `template_card`.

    lv_req_string = me->genjson_str( ijson = content
                                  msg_type = msg_type ).

    " 发送消息
    lv_res_string = me->post( url = me->botv-url
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
