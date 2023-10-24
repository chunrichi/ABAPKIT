REPORT zakit_demo_weixin_hookbot.

" 企业微信 hook bot 使用样例

" 官方文档: https://developer.work.weixin.qq.com/document/path/91770
" 编写时间: 2023.05.22

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

DATA: BEGIN OF gs_docker_status,
        visible TYPE abap_bool VALUE abap_true,
      END OF gs_docker_status.

CLASS lcl_pretty_json DEFINITION DEFERRED.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(8) t_url FOR FIELD p_url.
    PARAMETERS: p_url TYPE text180 MEMORY ID url.

    SELECTION-SCREEN COMMENT 58(5) t_type FOR FIELD p_type.
    PARAMETERS: p_type TYPE text15 AS LISTBOX VISIBLE LENGTH 10 USER-COMMAND ctype.

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
  SELECTION-SCREEN COMMENT 5(50) t_desc.
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
      values = VALUE vrm_values( ( key = 'text'          text = '文本' )
                                 ( key = 'markdown'      text = 'markdown' )
                                 ( key = 'image'         text = '图片' )
                                 ( key = 'news'          text = '图文' )
                                 ( key = 'file'          text = '文件' )
                                 ( key = 'template_card1' text = '文本卡片' )
                                 ( key = 'template_card2' text = '图文卡片' ) ).

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
    IF screen-name = 'P_AUTH'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

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
  DATA(lr_bot) = NEW zcl_akit_weixin_hook_bot( url = |{ p_url }| ).

  CASE p_type.
    WHEN 'template_card1' OR 'template_card2'.
      DATA(ls_result) = lr_bot->push( content = lv_json
                                     msg_type = `template_card` ).

      IF ls_result-type = 'S'.
        MESSAGE ls_result-message TYPE 'S'.
      ELSE.
        MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'image'.
      PERFORM frm_push_image USING lr_bot.
    WHEN 'file'.
      PERFORM frm_push_file USING lr_bot.
    WHEN OTHERS.
      ls_result = lr_bot->push( content = lv_json
                               msg_type = |{ p_type }| ).

      IF ls_result-type = 'S'.
        MESSAGE ls_result-message TYPE 'S'.
      ELSE.
        MESSAGE ls_result-message TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_demo_json
*&---------------------------------------------------------------------*
*&  设置 demo json
*&---------------------------------------------------------------------*
FORM frm_set_demo_json .

  DATA: lv_json TYPE string.

  IF gs_docker_status-visible = abap_false.
    gs_docker_status-visible = abap_true.
    gr_docking->set_visible( abap_true ).
  ENDIF.

  CASE p_type.
    WHEN 'text'.          " 文本
      PERFORM frm_push_text USING lv_json.
    WHEN 'markdown'.      " markdown
      PERFORM frm_push_markdown USING lv_json.
    WHEN 'image'.         " 图片
      gs_docker_status-visible = abap_false.
      gr_docking->set_visible( abap_false ).

    WHEN 'news'.          " 图文
      PERFORM frm_push_news USING lv_json.
    WHEN 'file'.          " 文件
      gs_docker_status-visible = abap_false.
      gr_docking->set_visible( abap_false ).

    WHEN 'template_card1'. " 文本卡片
      PERFORM frm_push_text_notice USING lv_json.
    WHEN 'template_card2'. " 图文卡片
      PERFORM frm_push_news_notice USING lv_json.
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
*& Form FRM_PUSH_TEXT
*&---------------------------------------------------------------------*
*&  文本推送
*&---------------------------------------------------------------------*
FORM frm_push_text USING p_json TYPE string.

  " https://developer.work.weixin.qq.com/document/path/91770#%E6%96%87%E6%9C%AC%E7%B1%BB%E5%9E%8B
  " 支持 @群成员

  p_json = `{`
  && cl_abap_char_utilities=>cr_lf && `    "content": "广州今日天气：29度，大部分多云，降雨概率：60%",`
  && cl_abap_char_utilities=>cr_lf && `    "mentioned_list":["@all"],`
* && cl_abap_char_utilities=>cr_lf && `    "mentioned_mobile_list":["13800001111","@all"]`
  && cl_abap_char_utilities=>cr_lf && `}`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_markdown
*&---------------------------------------------------------------------*
*& markdown 推送
*&---------------------------------------------------------------------*
FORM frm_push_markdown USING p_json TYPE string.

  " https://developer.work.weixin.qq.com/document/path/91770#markdown%E7%B1%BB%E5%9E%8B
  " 支持 @群成员

  p_json = `{`
  && cl_abap_char_utilities=>cr_lf && `    "content": "实时新增用户反馈<font color=\"warning\">132例</font>，请相关同事注意。\n`
  && cl_abap_char_utilities=>cr_lf && `     >类型:<font color=\"comment\">用户反馈</font>`
  && cl_abap_char_utilities=>cr_lf && `     >普通用户反馈:<font color=\"comment\">117例</font>`
  && cl_abap_char_utilities=>cr_lf && `     >VIP用户反馈:<font color=\"comment\">15例</font>"`
  && cl_abap_char_utilities=>cr_lf && `}`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_image
*&---------------------------------------------------------------------*
*&  推送图片
*&---------------------------------------------------------------------*
FORM frm_push_image USING pr_bot TYPE REF TO zcl_akit_weixin_hook_bot.

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
  DATA(ls_result) = pr_bot->push(
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
FORM frm_push_news USING p_json TYPE string.

  " https://developer.work.weixin.qq.com/document/path/91770#%E5%9B%BE%E6%96%87%E7%B1%BB%E5%9E%8B

  " 图文消息，一个图文消息支持1到8条图文
  " 标题，不超过128个字节，超过会自动截断
  " 描述，不超过512个字节，超过会自动截断
  " 图文消息的图片链接，支持JPG、PNG格式，较好的效果为大图 1068*455，小图150*150。

  p_json = `{`
  && cl_abap_char_utilities=>cr_lf && `   "articles" : [`
  && cl_abap_char_utilities=>cr_lf && `       {`
  && cl_abap_char_utilities=>cr_lf && `           "title" : "中秋节礼品领取",`
  && cl_abap_char_utilities=>cr_lf && `           "description" : "今年中秋节公司有豪礼相送",`
  && cl_abap_char_utilities=>cr_lf && `           "url" : "www.qq.com",`
  && cl_abap_char_utilities=>cr_lf && `           "picurl" : "http://res.mail.qq.com/node/ww/wwopenmng/images/independent/doc/test_pic_msg1.png"`
  && cl_abap_char_utilities=>cr_lf && `       }`
  && cl_abap_char_utilities=>cr_lf && `    ]`
  && cl_abap_char_utilities=>cr_lf && `}`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_file
*&---------------------------------------------------------------------*
*&  文件推送
*&---------------------------------------------------------------------*
FORM frm_push_file USING pr_bot TYPE REF TO zcl_akit_weixin_hook_bot.

  " 注：图片（base64编码前）最大不能超过2M，支持JPG,PNG格式

  DATA: lv_xstring TYPE xstring.
  DATA: lv_filename TYPE string.

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

  " 获取文件名
  SPLIT lv_file_name AT '\' INTO TABLE DATA(lt_split).
  READ TABLE lt_split INTO DATA(ls_split) INDEX lines( lt_split ).
  IF sy-subrc = 0.
    MOVE ls_split TO lv_filename.
  ENDIF.

  " 发送
  DATA(ls_file) = pr_bot->fileupload(
    raw = VALUE #(
      filename     = lv_filename
      content_type = `file`
      raw          = lv_xstring
    )
  ).

  IF ls_file-type = 'E'.
    MESSAGE ls_file-message TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(ls_result) = pr_bot->push(
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
FORM frm_push_text_notice USING p_json TYPE string.

  " https://developer.work.weixin.qq.com/document/path/91770#%E6%96%87%E6%9C%AC%E9%80%9A%E7%9F%A5%E6%A8%A1%E7%89%88%E5%8D%A1%E7%89%87

  " 相对示例有删减内容

  p_json = `{`
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_push_news_notice
*&---------------------------------------------------------------------*
*&  图文卡片
*&---------------------------------------------------------------------*
FORM frm_push_news_notice USING p_json TYPE string.

  " https://developer.work.weixin.qq.com/document/path/91770#%E5%9B%BE%E6%96%87%E5%B1%95%E7%A4%BA%E6%A8%A1%E7%89%88%E5%8D%A1%E7%89%87

  " 相对示例有删减内容

  p_json = `{`
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

ENDFORM.
