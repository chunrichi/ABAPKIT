REPORT zakit_demo_weixin_hookbot.

" 企业微信 hook bot 使用样例

" 官方文档: https://developer.work.weixin.qq.com/document/path/91770
" 编写时间: 2023.05.22

DATA: gv_url   TYPE string VALUE `https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=???`.
DATA: gr_weixin TYPE REF TO zcl_akit_weixin_hook_bot.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE. " 文本
    SELECTION-SCREEN COMMENT 5(23) t_text FOR FIELD p_text.
    PARAMETERS: p_text RADIOBUTTON GROUP gp1 DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " markdown
    SELECTION-SCREEN COMMENT 5(23) t_mark FOR FIELD p_mark.
    PARAMETERS: p_mark RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 图片
    SELECTION-SCREEN COMMENT 5(23) t_image FOR FIELD p_image.
    PARAMETERS: p_image RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 图文
    SELECTION-SCREEN COMMENT 5(23) t_news FOR FIELD p_news.
    PARAMETERS: p_news RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 文件
    SELECTION-SCREEN COMMENT 5(23) t_file FOR FIELD p_file.
    PARAMETERS: p_file RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 文本卡片
    SELECTION-SCREEN COMMENT 5(23) t_textc FOR FIELD p_textc.
    PARAMETERS: p_textc RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE. " 图文卡片
    SELECTION-SCREEN COMMENT 5(23) t_newsc FOR FIELD p_newsc.
    PARAMETERS: p_newsc RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  SELECTION-SCREEN COMMENT 5(50) t_desc.
SELECTION-SCREEN END OF BLOCK blck2.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  t_text  = '文本'(001).
  t_mark  = 'markdown'(002).
  t_image = '图片'(003).
  t_news  = '图文'(004).
  t_file  = '文件'(005).
  t_textc = '文本卡片'(006).
  t_newsc = '图文卡片'(007).

  t_desc  = '推送测试，需要先按自定义机器人获取参数的方法更改程序一部分参数'(008).

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
  IF p_mark = 'X'.
    PERFORM frm_push_markdown.
  ENDIF.

  " 图片
  IF p_image = 'X'.
    PERFORM frm_push_image.
  ENDIF.

  " 图文
  IF p_news = 'X'.
    PERFORM frm_push_news.
  ENDIF.

  IF p_file = 'X'.
    PERFORM frm_push_file.
  ENDIF.

  " 文本卡片
  IF p_textc = 'X'.
    PERFORM frm_push_text_notice.
  ENDIF.

  " 图文卡片
  IF p_newsc = 'X'.
    PERFORM frm_push_news_notice.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*&  初始化
*&---------------------------------------------------------------------*
FORM frm_init .
  cl_demo_input=>add_field( CHANGING field = gv_url ).
  cl_demo_input=>request( ).

  IF gv_url IS INITIAL.
    MESSAGE 'GV_URL IS INPUT REQUERED' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " 测试需要先将下面的参数更改为自己的
  " 相关参数获取方法 https://developer.work.weixin.qq.com/document/path/91770
  gr_weixin = NEW zcl_akit_weixin_hook_bot(
    url   = gv_url
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PUSH_TEXT
*&---------------------------------------------------------------------*
*&  文本推送
*&---------------------------------------------------------------------*
FORM frm_push_text .
  DATA: lv_content TYPE string.

  " https://developer.work.weixin.qq.com/document/path/91770#%E6%96%87%E6%9C%AC%E7%B1%BB%E5%9E%8B
  " 支持 @群成员

  lv_content = `{`
  && cl_abap_char_utilities=>cr_lf && `    "content": "广州今日天气：29度，大部分多云，降雨概率：60%",`
  && cl_abap_char_utilities=>cr_lf && `    "mentioned_list":["@all"],`
* && cl_abap_char_utilities=>cr_lf && `    "mentioned_mobile_list":["13800001111","@all"]`
  && cl_abap_char_utilities=>cr_lf && `}`.

  DATA(ls_result) = gr_weixin->push(
    msg_type = `text`
     content = lv_content
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_markdown
*&---------------------------------------------------------------------*
*& markdown 推送
*&---------------------------------------------------------------------*
FORM frm_push_markdown .

  " https://developer.work.weixin.qq.com/document/path/91770#markdown%E7%B1%BB%E5%9E%8B
  " 支持 @群成员

  DATA: lv_post TYPE string.

  lv_post = `{`
  && cl_abap_char_utilities=>cr_lf && `    "content": "实时新增用户反馈<font color=\"warning\">132例</font>，请相关同事注意。\n`
  && cl_abap_char_utilities=>cr_lf && `     >类型:<font color=\"comment\">用户反馈</font>`
  && cl_abap_char_utilities=>cr_lf && `     >普通用户反馈:<font color=\"comment\">117例</font>`
  && cl_abap_char_utilities=>cr_lf && `     >VIP用户反馈:<font color=\"comment\">15例</font>"`
  && cl_abap_char_utilities=>cr_lf && `}`.

  DATA(ls_result) = gr_weixin->push(
    msg_type = 'markdown'
    content  = lv_post
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

  " 注：图片（base64编码前）最大不能超过2M，支持JPG,PNG格式

  DATA: lv_xstring TYPE xstring.

  " 上载文件
  DATA: lv_path TYPE rlgrap-filename.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
      mask      = 'jpg|*.jpg|png|*png'
    CHANGING
      file_name = lv_path.

  CHECK lv_path IS NOT INITIAL.

  TYPES: ty_hex TYPE x LENGTH 255.

  DATA: lv_file_name    TYPE string,
        lt_file_data    TYPE TABLE OF ty_hex WITH DEFAULT KEY,
        lv_file_xstring TYPE xstring,
        lv_length       TYPE i.

  lv_file_name = lv_path.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file_name
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_length
    TABLES
      data_tab                = lt_file_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CONCATENATE LINES OF lt_file_data INTO lv_xstring IN BYTE MODE.

  " 发送
  DATA(ls_result) = gr_weixin->push(
    msg_type = 'image'
    content  = zcl_akit_weixin_hook_bot=>genimage( lv_xstring )
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_news
*&---------------------------------------------------------------------*
*&  推送图文
*&---------------------------------------------------------------------*
FORM frm_push_news .

  " https://developer.work.weixin.qq.com/document/path/91770#%E5%9B%BE%E6%96%87%E7%B1%BB%E5%9E%8B

  " 图文消息，一个图文消息支持1到8条图文
  " 标题，不超过128个字节，超过会自动截断
  " 描述，不超过512个字节，超过会自动截断
  " 图文消息的图片链接，支持JPG、PNG格式，较好的效果为大图 1068*455，小图150*150。

  DATA: lv_content TYPE string.

  lv_content = `{`
  && cl_abap_char_utilities=>cr_lf && `   "articles" : [`
  && cl_abap_char_utilities=>cr_lf && `       {`
  && cl_abap_char_utilities=>cr_lf && `           "title" : "中秋节礼品领取",`
  && cl_abap_char_utilities=>cr_lf && `           "description" : "今年中秋节公司有豪礼相送",`
  && cl_abap_char_utilities=>cr_lf && `           "url" : "www.qq.com",`
  && cl_abap_char_utilities=>cr_lf && `           "picurl" : "http://res.mail.qq.com/node/ww/wwopenmng/images/independent/doc/test_pic_msg1.png"`
  && cl_abap_char_utilities=>cr_lf && `       }`
  && cl_abap_char_utilities=>cr_lf && `    ]`
  && cl_abap_char_utilities=>cr_lf && `}`.

  DATA(ls_result) = gr_weixin->push(
    msg_type = 'news'
    content = lv_content
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_file
*&---------------------------------------------------------------------*
*&  文件推送
*&---------------------------------------------------------------------*
FORM frm_push_file .

  " 注：图片（base64编码前）最大不能超过2M，支持JPG,PNG格式

  DATA: lv_xstring TYPE xstring.

  " 上载文件
  DATA: lv_path TYPE rlgrap-filename.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
      mask      = '*'
    CHANGING
      file_name = lv_path.

  CHECK lv_path IS NOT INITIAL.

  TYPES: ty_hex TYPE x LENGTH 255.

  DATA: lv_file_name    TYPE string,
        lt_file_data    TYPE TABLE OF ty_hex WITH DEFAULT KEY,
        lv_file_xstring TYPE xstring,
        lv_length       TYPE i.

  lv_file_name = lv_path.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file_name
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_length
    TABLES
      data_tab                = lt_file_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CONCATENATE LINES OF lt_file_data INTO lv_xstring IN BYTE MODE.

  " 发送
  DATA(ls_file) = gr_weixin->fileupload(
    raw = VALUE #(
      filename     = `upload.txt`
      content_type = `file`
      raw          = lv_xstring
    )
  ).
  IF ls_file-type = 'E'.
    MESSAGE ls_file-message TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(ls_result) = gr_weixin->push(
    msg_type = 'file'
    content  = |\{ "media_id": "{ ls_file-media_id }" \}|
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_text_notice
*&---------------------------------------------------------------------*
*&  文本卡片
*&---------------------------------------------------------------------*
FORM frm_push_text_notice .

  " https://developer.work.weixin.qq.com/document/path/91770#%E6%96%87%E6%9C%AC%E9%80%9A%E7%9F%A5%E6%A8%A1%E7%89%88%E5%8D%A1%E7%89%87

  " 相对示例有删减内容

  DATA: lv_content TYPE string.

  lv_content = `{`
  && cl_abap_char_utilities=>cr_lf && `    "card_type":"text_notice",`
  && cl_abap_char_utilities=>cr_lf && `    "source":{`
  && cl_abap_char_utilities=>cr_lf && `        "icon_url":"https://wework.qpic.cn/wwpic/252813_jOfDHtcISzuodLa_1629280209/0",`
  && cl_abap_char_utilities=>cr_lf && `        "desc":"企业微信",`
  && cl_abap_char_utilities=>cr_lf && `        "desc_color":0`
  && cl_abap_char_utilities=>cr_lf && `    },`
  && cl_abap_char_utilities=>cr_lf && `    "main_title":{`
  && cl_abap_char_utilities=>cr_lf && `        "title":"欢迎使用企业微信",`
  && cl_abap_char_utilities=>cr_lf && `        "desc":"您的好友正在邀请您加入企业微信"`
  && cl_abap_char_utilities=>cr_lf && `    },`
  && cl_abap_char_utilities=>cr_lf && `    "emphasis_content":{`
  && cl_abap_char_utilities=>cr_lf && `        "title":"100",`
  && cl_abap_char_utilities=>cr_lf && `        "desc":"数据含义"`
  && cl_abap_char_utilities=>cr_lf && `    },`
  && cl_abap_char_utilities=>cr_lf && `    "sub_title_text":"下载企业微信还能抢红包！",`
  && cl_abap_char_utilities=>cr_lf && `    "horizontal_content_list":[`
  && cl_abap_char_utilities=>cr_lf && `        {`
  && cl_abap_char_utilities=>cr_lf && `            "keyname":"邀请人",`
  && cl_abap_char_utilities=>cr_lf && `            "value":"张三"`
  && cl_abap_char_utilities=>cr_lf && `        },`
  && cl_abap_char_utilities=>cr_lf && `        {`
  && cl_abap_char_utilities=>cr_lf && `            "keyname":"企微官网",`
  && cl_abap_char_utilities=>cr_lf && `            "value":"点击访问",`
  && cl_abap_char_utilities=>cr_lf && `            "type":1,`
  && cl_abap_char_utilities=>cr_lf && `            "url":"https://work.weixin.qq.com/?from=openApi"`
  && cl_abap_char_utilities=>cr_lf && `        }`
  && cl_abap_char_utilities=>cr_lf && `    ],`
  && cl_abap_char_utilities=>cr_lf && `    "card_action":{`
  && cl_abap_char_utilities=>cr_lf && `        "type":1,`
  && cl_abap_char_utilities=>cr_lf && `        "url":"https://work.weixin.qq.com/?from=openApi",`
  && cl_abap_char_utilities=>cr_lf && `        "appid":"APPID",`
  && cl_abap_char_utilities=>cr_lf && `        "pagepath":"PAGEPATH"`
  && cl_abap_char_utilities=>cr_lf && `    }`
  && cl_abap_char_utilities=>cr_lf && `}`.

  DATA(ls_result) = gr_weixin->push(
    msg_type = 'template_card'
    content = lv_content
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_news_notice
*&---------------------------------------------------------------------*
*&  图文卡片
*&---------------------------------------------------------------------*
FORM frm_push_news_notice .

  " https://developer.work.weixin.qq.com/document/path/91770#%E5%9B%BE%E6%96%87%E5%B1%95%E7%A4%BA%E6%A8%A1%E7%89%88%E5%8D%A1%E7%89%87

  " 相对示例有删减内容

  DATA: lv_content TYPE string.

  lv_content = `{`
  && cl_abap_char_utilities=>cr_lf && `    "card_type":"news_notice",`
  && cl_abap_char_utilities=>cr_lf && `    "source":{`
  && cl_abap_char_utilities=>cr_lf && `        "icon_url":"https://wework.qpic.cn/wwpic/252813_jOfDHtcISzuodLa_1629280209/0",`
  && cl_abap_char_utilities=>cr_lf && `        "desc":"企业微信",`
  && cl_abap_char_utilities=>cr_lf && `        "desc_color":0`
  && cl_abap_char_utilities=>cr_lf && `    },`
  && cl_abap_char_utilities=>cr_lf && `    "main_title":{`
  && cl_abap_char_utilities=>cr_lf && `        "title":"欢迎使用企业微信",`
  && cl_abap_char_utilities=>cr_lf && `        "desc":"您的好友正在邀请您加入企业微信"`
  && cl_abap_char_utilities=>cr_lf && `    },`
  && cl_abap_char_utilities=>cr_lf && `    "card_image":{`
  && cl_abap_char_utilities=>cr_lf && `        "url":"https://wework.qpic.cn/wwpic/354393_4zpkKXd7SrGMvfg_1629280616/0",`
  && cl_abap_char_utilities=>cr_lf && `        "aspect_ratio":2.25`
  && cl_abap_char_utilities=>cr_lf && `    },`
  && cl_abap_char_utilities=>cr_lf && `    "image_text_area":{`
  && cl_abap_char_utilities=>cr_lf && `        "type":1,`
  && cl_abap_char_utilities=>cr_lf && `        "url":"https://work.weixin.qq.com",`
  && cl_abap_char_utilities=>cr_lf && `        "title":"欢迎使用企业微信",`
  && cl_abap_char_utilities=>cr_lf && `        "desc":"您的好友正在邀请您加入企业微信",`
  && cl_abap_char_utilities=>cr_lf && `        "image_url":"https://wework.qpic.cn/wwpic/354393_4zpkKXd7SrGMvfg_1629280616/0"`
  && cl_abap_char_utilities=>cr_lf && `    },`
  && cl_abap_char_utilities=>cr_lf && `    "vertical_content_list":[`
  && cl_abap_char_utilities=>cr_lf && `        {`
  && cl_abap_char_utilities=>cr_lf && `            "title":"惊喜红包等你来拿",`
  && cl_abap_char_utilities=>cr_lf && `            "desc":"下载企业微信还能抢红包！"`
  && cl_abap_char_utilities=>cr_lf && `        }`
  && cl_abap_char_utilities=>cr_lf && `    ],`
  && cl_abap_char_utilities=>cr_lf && `    "horizontal_content_list":[`
  && cl_abap_char_utilities=>cr_lf && `        {`
  && cl_abap_char_utilities=>cr_lf && `            "keyname":"邀请人",`
  && cl_abap_char_utilities=>cr_lf && `            "value":"张三"`
  && cl_abap_char_utilities=>cr_lf && `        },`
  && cl_abap_char_utilities=>cr_lf && `        {`
  && cl_abap_char_utilities=>cr_lf && `            "keyname":"企微官网",`
  && cl_abap_char_utilities=>cr_lf && `            "value":"点击访问",`
  && cl_abap_char_utilities=>cr_lf && `            "type":1,`
  && cl_abap_char_utilities=>cr_lf && `            "url":"https://work.weixin.qq.com/?from=openApi"`
  && cl_abap_char_utilities=>cr_lf && `        },`
  && cl_abap_char_utilities=>cr_lf && `    ],`
  && cl_abap_char_utilities=>cr_lf && `    "jump_list":[`
  && cl_abap_char_utilities=>cr_lf && `        {`
  && cl_abap_char_utilities=>cr_lf && `            "type":1,`
  && cl_abap_char_utilities=>cr_lf && `            "url":"https://work.weixin.qq.com/?from=openApi",`
  && cl_abap_char_utilities=>cr_lf && `            "title":"企业微信官网"`
  && cl_abap_char_utilities=>cr_lf && `        },`
  && cl_abap_char_utilities=>cr_lf && `    ],`
  && cl_abap_char_utilities=>cr_lf && `    "card_action":{`
  && cl_abap_char_utilities=>cr_lf && `        "type":1,`
  && cl_abap_char_utilities=>cr_lf && `        "url":"https://work.weixin.qq.com/?from=openApi",`
  && cl_abap_char_utilities=>cr_lf && `        "appid":"APPID",`
  && cl_abap_char_utilities=>cr_lf && `        "pagepath":"PAGEPATH"`
  && cl_abap_char_utilities=>cr_lf && `    }`
  && cl_abap_char_utilities=>cr_lf && `}`.

  DATA(ls_result) = gr_weixin->push(
    msg_type = 'template_card'
    content = lv_content
  ).

  IF ls_result-type = 'S'.
    MESSAGE ls_result-message TYPE 'S'.
  ELSE.
    MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.