REPORT zakit_demo_feishu_hookbot.

" 飞书 hook bot 使用样例

" 官方文档: https://open.feishu.cn/document/ukTMukTMukTM/ucTM5YjL3ETO24yNxkjN#383d6e48
" 编写时间: 2023.05.17

DATA: gv_url   TYPE string VALUE `https://open.feishu.cn/open-apis/bot/v2/hook/???????`,
      gv_token TYPE string.
DATA: gr_feishu TYPE REF TO zcl_akit_feishu_hook_bot.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE. " 文本
    SELECTION-SCREEN COMMENT 5(23) t_text FOR FIELD p_text.
    PARAMETERS: p_text RADIOBUTTON GROUP gp1 DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 富文本
    SELECTION-SCREEN COMMENT 5(23) t_post FOR FIELD p_post.
    PARAMETERS: p_post RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 图片
    SELECTION-SCREEN COMMENT 5(23) t_image FOR FIELD p_image.
    PARAMETERS: p_image RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 分享群名片
    SELECTION-SCREEN COMMENT 5(23) t_share FOR FIELD p_share.
    PARAMETERS: p_share RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 消息卡片
    SELECTION-SCREEN COMMENT 5(23) t_inter FOR FIELD p_inter.
    PARAMETERS: p_inter RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  SELECTION-SCREEN COMMENT 5(50) t_desc FOR FIELD p_inter.
SELECTION-SCREEN END OF BLOCK blck2.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  t_text  = '文本'(001).
  t_post  = '富文本'(002).
  t_image = '图片'(003).
  t_share = '分享群名片'(004).
  t_inter = '消息卡片'(005).

  t_desc  = '推送测试，需要先按自定义机器人获取参数的方法更改程序一部分参数'(006).

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_init.

  " 文本
  IF p_text = 'X'.
    PERFORM frm_push_text.
  ENDIF.

  " 富文本
  IF p_post = 'X'.
    PERFORM frm_push_post.
  ENDIF.

  " 图片
  IF p_image = 'X'.
    PERFORM frm_push_image.
  ENDIF.

  " 分享群名片
  IF p_share = 'X'.
    PERFORM frm_push_share_chat.
  ENDIF.

  " 消息卡片
  IF p_inter = 'X'.
    PERFORM frm_push_interactive.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*&  初始化
*&---------------------------------------------------------------------*
FORM frm_init .
  cl_demo_input=>add_field( CHANGING field = gv_url ).
  cl_demo_input=>add_field( CHANGING field = gv_token ).
  cl_demo_input=>request( ).

  IF gv_url IS INITIAL.
    MESSAGE 'GV_URL IS INPUT REQUERED' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " 测试需要先将下面的参数更改为自己的
  " 相关参数获取方法 https://open.feishu.cn/document/ukTMukTMukTM/ucTM5YjL3ETO24yNxkjN#d65d109d
  gr_feishu = NEW zcl_akit_feishu_hook_bot(
    url   = gv_url
    token = gv_token
  ).

  " 由于编写系统时间比实际时间慢 1小时 17分钟 50秒
  gr_feishu->fix_time = 1 * 60 * 60 + 17 * 60 + 50.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PUSH_TEXT
*&---------------------------------------------------------------------*
*&  文本推送
*&---------------------------------------------------------------------*
FORM frm_push_text .

  DATA(ls_result) = gr_feishu->push(
    content = `{ "text":"测试" }`
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_post
*&---------------------------------------------------------------------*
*& 富文本推送
*&---------------------------------------------------------------------*
FORM frm_push_post .

  " 只支持标题、不带格式的文本、图片、链接、at人样式。
  " 更复杂的带格式的内容建议使用消息卡片实现。

  " 自定义机器人仅支持通过 open_id @ 特定人员
  " https://open.feishu.cn/document/home/user-identity-introduction/open-id
  " 更多富文本信息、
  " https://open.feishu.cn/document/uAjLw4CM/ukTMukTMukTM/im-v1/message/create_json

  DATA: lv_post TYPE string.

  lv_post = `{`
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

  DATA(ls_result) = gr_feishu->push(
    msg_type = 'post'
    content  = lv_post
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_share_chat
*&---------------------------------------------------------------------*
*&  推送分享会话
*&---------------------------------------------------------------------*
FORM frm_push_share_chat .

  DATA(ls_result) = gr_feishu->push(
    msg_type = 'share_chat'
    content = `{ "share_chat_id": "oc_f5b1a7eb27ae2c7b6adc2a74faf339ff" }`
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_image
*&---------------------------------------------------------------------*
*&  推送图片
*&---------------------------------------------------------------------*
FORM frm_push_image .

  " 需要创建 机器人应用上传图片 从而获取 image_key
  " https://open.feishu.cn/document/uAjLw4CM/ukTMukTMukTM/reference/im-v1/image/create

  DATA(ls_result) = gr_feishu->push(
    msg_type = 'image'
    content = `{ "image_key": "img_ecffc3b9-8f14-400f-a014-05eca1a4310g" }`
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_interactive
*&---------------------------------------------------------------------*
*&  推送消息卡片
*&---------------------------------------------------------------------*
FORM frm_push_interactive .

  " 可视化生成卡片
  " https://open.feishu.cn/tool/cardbuilder?from=custom_bot_doc

  DATA: lv_card TYPE string.

  lv_card = `{`
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

  DATA(ls_result) = gr_feishu->push(
    msg_type = `interactive`
    content  = lv_card
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.