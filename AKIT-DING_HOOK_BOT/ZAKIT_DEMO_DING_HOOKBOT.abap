REPORT zakit_demo_ding_hookbot.

" 钉钉 hookbot 使用样例

" 官方文档: https://open.dingtalk.com/document/robots/custom-robot-access
" 编写时间: 2023.05.31


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
    PARAMETERS: p_type TYPE text10 AS LISTBOX VISIBLE LENGTH 10 USER-COMMAND ctype.

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
      values = VALUE vrm_values( ( key = 'text'       text = 'text' )
                                 ( key = 'link'       text = 'link' )
                                 ( key = 'markdown'   text = 'markdown' )
                                 ( key = 'actionCard' text = 'actionCard' )
                                 ( key = 'feedCard'   text = 'feedCard' ) ).

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
  DATA(lr_bot) = NEW zcl_akit_ding_hookbot( url = |{ p_url }|
                                          token = |{ p_token }| ).

  " 启用token由于编写系统时间比实际时间慢需修正 1小时 18分钟 41秒 毫秒 => 修正到毫秒
  " lr_bot->fix_time = 1 * 60 * 60 + 18 * 60 + 41 + '0.100'.

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
    WHEN `text`.
      lv_json = `{ "content":"我就是我, 不一样的烟火" }`.
    WHEN `link`.
      lv_json = `{`
      && `    "text": "这个即将发布的新版本，创始人xx称它为红树林。而在此之前，每当面临重大升级，产品经理们都会取一个应景的代号，这一次，为什么是红树林", `
      && `    "title": "时代的火车向前开", `
      && `    "picUrl": "", `
      && `    "messageUrl":  "https://www.dingtalk.com/s?__biz=MzA4NjMwMTA2Ng==&mid=2650316842&idx=1&sn=`
      && `60da3ea2b29f1dcc43a7c8e4a7c97a16&scene=2&srcid=09189AnRJEdIiWVaKltFzNTw&from=timeline&isappinstalled=`
      && `0&key=&ascene=2&uin=&devicetype=android-23&version=26031933&nettype=WIFI"`
      && `}`.
    WHEN `markdown`.
      lv_json = `{`
      && `    "title":"杭州天气",`
      && `    "text": "#### 杭州天气\n > 9度，西北风1级，空气良89，`
      && `相对温度73%\n > ![screenshot](https://img.alicdn.com/tfs/TB1NwmBEL9TBuNjy1zbXXXp`
      && `epXa-2400-1218.png)\n > ###### 10点20分发布 [天气](https://www.dingtalk.com) \n"`
      && `}`.
    WHEN `actionCard`.
      lv_json = `{`
      && `    "title": "我 20 年前想打造一间苹果咖啡厅，而它正是 Apple Store 的前身", `
      && `    "text": "![screenshot](https://img.alicdn.com/tfs/TB1NwmBEL9TBuNjy1zbXXXpepXa-2400-1218.png) \n\n #`
      && `### 乔布斯 20 年前想打造的苹果咖啡厅 \n\n Apple Store 的设计正从原来满满的科技感走向生活化，而其生活化的走向其实`
      && `可以追溯到 20 年前苹果一个建立咖啡馆的计划", `
      && `    "btnOrientation": "0", `
      && `    "btns": [`
      && `        {`
      && `            "title": "内容不错", `
      && `            "actionURL": "https://www.dingtalk.com/"`
      && `        }, `
      && `        {`
      && `            "title": "不感兴趣", `
      && `            "actionURL": "https://www.dingtalk.com/"`
      && `        }`
      && `    ]`
      && `}`.
    WHEN `feedCard`.
      lv_json = `{`
      && `    "links": [`
      && `        {`
      && `            "title": "时代的火车向前开1", `
      && `            "messageURL": "https://www.dingtalk.com/", `
      && `            "picURL": "https://img.alicdn.com/tfs/TB1NwmBEL9TBuNjy1zbXXXpepXa-2400-1218.png"`
      && `        },`
      && `        {`
      && `            "title": "时代的火车向前开2", `
      && `            "messageURL": "https://www.dingtalk.com/", `
      && `            "picURL": "https://img.alicdn.com/tfs/TB1NwmBEL9TBuNjy1zbXXXpepXa-2400-1218.png"`
      && `        }`
      && `    ]`
      && `}`.
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