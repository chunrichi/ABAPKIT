CLASS zcl_wechat_approval DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_filter,
             key   TYPE string,
             value TYPE string,
           END OF ty_filter,
           tt_filters    TYPE STANDARD TABLE OF ty_filter WITH DEFAULT KEY,
           tt_sp_no_list TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA g_error_message TYPE string.

    METHODS constructor
      IMPORTING
        !corpid     TYPE string
        !corpsecret TYPE string .
    METHODS send
      IMPORTING
        !data   TYPE REF TO zcl_wx_oa_fc
      EXPORTING
        !result TYPE data.
    METHODS read
      IMPORTING
        !sp_no  TYPE string
      EXPORTING
        !result TYPE data.
    METHODS list
      IMPORTING
                !starttime        TYPE timestamp
                !endtime          TYPE timestamp
                !new_cursor       TYPE i
                !size             TYPE i DEFAULT 100
                !filters          TYPE tt_filters
      EXPORTING
                subrc             TYPE sysubrc
                next_cursor       TYPE i
      RETURNING VALUE(sp_no_list) TYPE tt_sp_no_list.
    METHODS userid
      IMPORTING
        !uname        TYPE syuname DEFAULT sy-uname
      RETURNING
        VALUE(userid) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA http TYPE REF TO zcl_wechat_http.
ENDCLASS.



CLASS ZCL_WECHAT_APPROVAL IMPLEMENTATION.


  METHOD constructor.

    me->http = NEW #( corpid = corpid corpsecret = corpsecret ).

  ENDMETHOD.


  METHOD list.
    DATA: BEGIN OF ls_req,
            starttime  TYPE string,
            endtime    TYPE string,
            new_cursor TYPE string,
            size       TYPE i,
            filters    TYPE tt_filters,
          END OF ls_req,
          BEGIN OF ls_res,
            errcode     TYPE i,
            errmsg      TYPE string,
            sp_no_list  TYPE tt_sp_no_list,
            next_cursor TYPE i,
          END OF ls_res.

    CONVERT TIME STAMP starttime TIME ZONE 'UTC' INTO DATE DATA(l_date) TIME DATA(l_time).
    cl_pco_utility=>convert_abap_timestamp_to_java( EXPORTING iv_date = l_date
                                                              iv_time = l_time
                                                              iv_msec = 0
                                               IMPORTING ev_timestamp = ls_req-starttime ).

    CONVERT TIME STAMP endtime TIME ZONE 'UTC' INTO DATE l_date TIME l_time.
    cl_pco_utility=>convert_abap_timestamp_to_java( EXPORTING iv_date = l_date
                                                              iv_time = l_time
                                                              iv_msec = 0
                                               IMPORTING ev_timestamp = ls_req-endtime ).

    ls_req-new_cursor = new_cursor.
    ls_req-size       = size.
    ls_req-filters    = filters.

    me->http->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

    DATA(l_ecode) = me->http->post(
      EXPORTING
        url    = `https://qyapi.weixin.qq.com/cgi-bin/oa/getapprovalinfo`
        data   = ls_req
      IMPORTING
        result = ls_res
    ).

    CLEAR me->g_error_message.
    IF l_ecode = 200 AND ls_res-errcode = 0.
      sp_no_list = ls_res-sp_no_list.
      next_cursor = ls_res-next_cursor.
    ELSE.
      subrc = 4.
      IF ls_res-errmsg IS INITIAL.
        me->g_error_message = l_ecode && '->' && me->http->g_error_message.
      ELSE.
        me->g_error_message = ls_res-errmsg.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD read.
    DATA: BEGIN OF ls_req,
            sp_no TYPE string,
          END OF ls_req.

    " 参考 https://developer.work.weixin.qq.com/document/path/91983
    " 此处处理做简化

    me->http->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

    DATA(l_ecode) = me->http->post(
      EXPORTING
        url    = `https://qyapi.weixin.qq.com/cgi-bin/oa/getapprovaldetail`
        data   = ls_req
      IMPORTING
        result = result
    ).

  ENDMETHOD.


  METHOD send.
    me->http->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

    DATA(l_ecode) = me->http->post(
      EXPORTING
        url    = `https://qyapi.weixin.qq.com/cgi-bin/oa/applyevent`
        data   = data
      IMPORTING
        result = result
    ).

  ENDMETHOD.


  METHOD userid.

    " 当前用户电话号码取值
    SELECT SINGLE
      us~bname AS uname,
      cp~tel_number AS phone
      FROM usr21 AS us
      LEFT JOIN adcp AS cp ON cp~addrnumber = us~addrnumber
                          AND cp~persnumber = us~persnumber
      WHERE us~bname = @uname
      INTO @DATA(ls_sap_info).

    " 电话号码和缓存校验 => 一致则直接返回 userid
    SELECT SINGLE
      phone,
      userid
      FROM ztwx_user_info
      WHERE uname = @uname
      INTO @DATA(ls_info_cache).

    IF ls_sap_info-phone = ls_info_cache-phone AND ls_info_cache-userid IS NOT INITIAL.
      userid = ls_info_cache-userid.
      RETURN.
    ENDIF.

    " 取新值
    DATA(l_ecode) = me->http->userid(
      EXPORTING
        phone  = |{ ls_sap_info-phone }|
      IMPORTING
        userid = userid
    ).

    DATA: ls_user_info TYPE ztwx_user_info.
    IF l_ecode = 200.
      ls_user_info-uname = uname.
      ls_user_info-phone = ls_user_info-phone.
      ls_user_info-userid = userid.

      IF ls_info_cache-userid IS NOT INITIAL.
        GET TIME STAMP FIELD ls_user_info-created_on.
      ENDIF.
      GET TIME STAMP FIELD ls_user_info-changed_on.

      MODIFY ztwx_user_info FROM ls_user_info.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
