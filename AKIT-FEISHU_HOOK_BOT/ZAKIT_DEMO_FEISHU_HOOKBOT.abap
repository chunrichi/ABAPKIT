REPORT zakit_demo_feishu_hookbot.

" 飞书 hook bot 使用样例

" 官方文档: https://open.feishu.cn/document/ukTMukTMukTM/ucTM5YjL3ETO24yNxkjN#383d6e48
" 编写时间: 2023.05.17

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

DATA: gr_edit_o TYPE REF TO cl_gui_textedit.

CLASS lcl_pretty_json DEFINITION DEFERRED.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_url FOR FIELD p_url.
    PARAMETERS: p_url TYPE text180 MEMORY ID url.

    SELECTION-SCREEN COMMENT 58(5) t_type FOR FIELD p_type.
    PARAMETERS: p_type TYPE text12 AS LISTBOX VISIBLE LENGTH 10 USER-COMMAND ctype.

    SELECTION-SCREEN COMMENT 78(3) t_auth FOR FIELD p_auth.
    PARAMETERS: p_auth AS CHECKBOX USER-COMMAND auth.

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_token FOR FIELD p_token MODIF ID aut.
    PARAMETERS: p_token TYPE text40 MODIF ID aut.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  SELECTION-SCREEN COMMENT 5(50) t_desc .
SELECTION-SCREEN END OF BLOCK blck3.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  t_url  = 'URL'.
  t_type = '类型'.
  t_auth = '认证'.
  t_token = '加签'.

  " 下拉框
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TYPE'
      values = VALUE vrm_values( ( key = 'text'    text = '文本' )
                                 ( key = 'post'    text = '富文本' )
                                 ( key = 'image'   text = '图片' )
                                 ( key = 'share_chat'    text = '分享群名片' )
                                 ( key = 'interactive'   text = '消息卡片' ) ).

  t_desc  = '推送测试，需要先按自定义机器人获取参数的方法更改一部分参数'.

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
    gr_splitter = NEW #( parent = gr_docking rows = 1 columns = 1 ).

    gr_edit_o = NEW #(
      parent = gr_splitter->get_container( row = 1 column = 1 )
    ).

  ENDIF.

  LOOP AT SCREEN.
    IF p_auth = '' AND screen-group1 = 'AUT'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'P_URL'.
      screen-required = 2.
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

  IF sy-ucomm = 'CTYPE'.
    PERFORM frm_set_demo_json.
  ENDIF.

  IF p_auth = ''.
    CLEAR p_token.
  ENDIF.

*&----------------------------------------------------------------------
*                     Class definition
*&----------------------------------------------------------------------
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

  IF p_type IS INITIAL.
    MESSAGE '请选择 type 类型' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_send_request
*&---------------------------------------------------------------------*
*&  发起请求
*&---------------------------------------------------------------------*
FORM frm_send_request .

  DATA: lv_json TYPE string.

  " 发出去的内容
  gr_edit_o->get_textstream( IMPORTING text = lv_json ).
  cl_gui_cfw=>flush( ).

  " 调用方法
  DATA(lr_bot) = NEW zcl_akit_feishu_hook_bot( url = |{ p_url }|
                                          token = |{ p_token }| ).

  " 启用token 由于编写系统时间比实际时间慢 1小时 17分钟 50秒
  " lr_bot->fix_time = 1 * 60 * 60 + 17 * 60 + 50.

  DATA(ls_result) = lr_bot->push( content = lv_json
                                 msg_type = |{ p_type }| ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_demo_json
*&---------------------------------------------------------------------*
*&  设置 demo json
*&---------------------------------------------------------------------*
FORM frm_set_demo_json .

  DATA: lv_json TYPE string.

  CASE p_type.
    WHEN `text`. " 文本
      lv_json = `{ "text":"测试" }`.
    WHEN `post`. " 富文本
      PERFORM frm_push_post USING lv_json.
    WHEN `image`. " 图片
      PERFORM frm_push_image USING lv_json.
    WHEN `share_chat`. " 分享群名片
      PERFORM frm_push_share_chat USING lv_json.
    WHEN `interactive`. " 消息卡片
      PERFORM frm_push_interactive USING lv_json.
    WHEN OTHERS.
  ENDCASE.

  IF lv_json IS NOT INITIAL.
    TRY.
        lv_json = lcl_pretty_json=>pretty( lv_json ).
      CATCH cx_root.
    ENDTRY.
    gr_edit_o->set_textstream( EXPORTING text = lv_json ).
  ENDIF.

ENDFORM.


CLASS lcl_pretty_json IMPLEMENTATION.
  METHOD pretty.

    "cloud
    DATA(json_xstring) = cl_abap_conv_codepage=>create_out( )->convert( json ).
    "on_premise
    "DATA(json_xstring) = cl_abap_codepage=>convert_to( json_string_in ).

    "Check and pretty print JSON

    DATA(reader) = cl_sxml_string_reader=>create( json_xstring ).
    DATA(writer) = CAST if_sxml_writer(
                          cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    reader->next_node( ).
    reader->skip_node( writer ).

    "cloud
    DATA(json_formatted_string) = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( writer )->get_output( ) ).
    "on premise
    "DATA(json_formatted_string) = cl_abap_codepage=>convert_from( CAST cl_sxml_string_writer( writer )->get_output( ) ).

    pretty_json = escape( val = json_formatted_string format = cl_abap_format=>e_xml_text  ).

  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*& Form frm_push_post
*&---------------------------------------------------------------------*
*& 富文本
*&---------------------------------------------------------------------*
FORM frm_push_post USING p_json TYPE string.

  " 只支持标题、不带格式的文本、图片、链接、at人样式。
  " 更复杂的带格式的内容建议使用消息卡片实现。

  " 自定义机器人仅支持通过 open_id @ 特定人员
  " https://open.feishu.cn/document/home/user-identity-introduction/open-id
  " 更多富文本信息、
  " https://open.feishu.cn/document/uAjLw4CM/ukTMukTMukTM/im-v1/message/create_json

  p_json = `{`
  && cl_abap_char_utilities=>cr_lf && ` "post": {`
  && cl_abap_char_utilities=>cr_lf && `   "zh_cn": {`
  && cl_abap_char_utilities=>cr_lf && `     "title": "项目更新通知",`
  && cl_abap_char_utilities=>cr_lf && `     "content": [`
  && cl_abap_char_utilities=>cr_lf && `       [{`
  && cl_abap_char_utilities=>cr_lf && `           "tag": "text",`
  && cl_abap_char_utilities=>cr_lf && `           "text": "项目有更新: "`
  && cl_abap_char_utilities=>cr_lf && `         },`
  && cl_abap_char_utilities=>cr_lf && `         {`
  && cl_abap_char_utilities=>cr_lf && `           "tag": "a",`
  && cl_abap_char_utilities=>cr_lf && `           "text": "请查看",`
  && cl_abap_char_utilities=>cr_lf && `           "href": "http://www.example.com/"`
  && cl_abap_char_utilities=>cr_lf && `         },`
  && cl_abap_char_utilities=>cr_lf && `         {`
  && cl_abap_char_utilities=>cr_lf && `           "tag": "at",`
  && cl_abap_char_utilities=>cr_lf && `           "user_id": "ou_18eac8********17ad4f02e8bbbb"`
  && cl_abap_char_utilities=>cr_lf && `         }`
  && cl_abap_char_utilities=>cr_lf && `       ]`
  && cl_abap_char_utilities=>cr_lf && `     ]`
  && cl_abap_char_utilities=>cr_lf && `   }`
  && cl_abap_char_utilities=>cr_lf && ` }`
  && cl_abap_char_utilities=>cr_lf && `}`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_share_chat
*&---------------------------------------------------------------------*
*&  推送分享会话
*&---------------------------------------------------------------------*
FORM frm_push_share_chat USING p_json TYPE string.

  p_json = `{ "share_chat_id": "oc_f5b1a7eb27ae2c7b6adc2a74faf339ff" }`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_image
*&---------------------------------------------------------------------*
*&  推送图片
*&---------------------------------------------------------------------*
FORM frm_push_image USING p_json TYPE string.

  " 需要创建 机器人应用上传图片 从而获取 image_key
  " https://open.feishu.cn/document/uAjLw4CM/ukTMukTMukTM/reference/im-v1/image/create

  p_json = `{ "image_key": "img_ecffc3b9-8f14-400f-a014-05eca1a4310g" }`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_interactive
*&---------------------------------------------------------------------*
*&  推送消息卡片
*&---------------------------------------------------------------------*
FORM frm_push_interactive USING p_json TYPE string.

  " 可视化生成卡片
  " https://open.feishu.cn/tool/cardbuilder?from=custom_bot_doc

  p_json = `{`
  && cl_abap_char_utilities=>cr_lf && `  "elements": [{`
  && cl_abap_char_utilities=>cr_lf && `    "tag": "div",`
  && cl_abap_char_utilities=>cr_lf && `    "text": {`
  && cl_abap_char_utilities=>cr_lf && `      "content": "**西湖**，位于浙江省杭州市西湖区龙井路1号，杭州市区西部，`
  && `景区总面积49平方千米，汇水面积为21.22平方千米，湖面面积为6.38平方千米。",`
  && cl_abap_char_utilities=>cr_lf && `      "tag": "lark_md"`
  && cl_abap_char_utilities=>cr_lf && `    }`
  && cl_abap_char_utilities=>cr_lf && `  }, {`
  && cl_abap_char_utilities=>cr_lf && `    "actions": [{`
  && cl_abap_char_utilities=>cr_lf && `      "tag": "button",`
  && cl_abap_char_utilities=>cr_lf && `      "text": {`
  && cl_abap_char_utilities=>cr_lf && `        "content": "更多景点介绍 :玫瑰:",`
  && cl_abap_char_utilities=>cr_lf && `        "tag": "lark_md"`
  && cl_abap_char_utilities=>cr_lf && `      },`
  && cl_abap_char_utilities=>cr_lf && `      "url": "https://www.example.com",`
  && cl_abap_char_utilities=>cr_lf && `      "type": "default",`
  && cl_abap_char_utilities=>cr_lf && `      "value": {}`
  && cl_abap_char_utilities=>cr_lf && `    }],`
  && cl_abap_char_utilities=>cr_lf && `    "tag": "action"`
  && cl_abap_char_utilities=>cr_lf && `  }],`
  && cl_abap_char_utilities=>cr_lf && `  "header": {`
  && cl_abap_char_utilities=>cr_lf && `    "title": {`
  && cl_abap_char_utilities=>cr_lf && `      "content": "今日旅游推荐",`
  && cl_abap_char_utilities=>cr_lf && `      "tag": "plain_text"`
  && cl_abap_char_utilities=>cr_lf && `    }`
  && cl_abap_char_utilities=>cr_lf && `  }`
  && cl_abap_char_utilities=>cr_lf && `} `.

ENDFORM.