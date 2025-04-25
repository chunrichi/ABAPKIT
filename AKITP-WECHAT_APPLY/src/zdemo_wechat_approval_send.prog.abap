*&---------------------------------------------------------------------*
*& Report ZDEMO_WECHAT_APPROVAL_SEND
*&---------------------------------------------------------------------*
*& 调用接口获取审批结构
*&---------------------------------------------------------------------*
REPORT zdemo_wechat_approval_send.

" ------------------- 接口初始化 ----------------------
DATA(l_approval) = NEW zcl_wechat_approval(
    corpid     = `xxxxxxxx`
    corpsecret = `xxxxxxxxxxxxxxxx` ).

" ------------------- 审批赋值 ----------------------
" 审批标题: 发文审批

" ------------------- 基础赋值 ----------------------
DATA(l_fc) = NEW zcl_wx_oa_fc( ).
" 申请人userid
l_fc->creator_userid        = l_approval->userid( ).

" 模板id
l_fc->template_id           = 'C4ZXKAttHPFgVA7gQh57zwCgzgeMQ9BN4aC3BVbud'.

" 审批人模式：0 指定、1 使用模板
l_fc->use_template_approver = 1.

" 提单者提单部门id，不填默认为主部门
" l_fc->choose_department   = 2.

" ------------------- 流程列表 ----------------------
" use_template_approver = 0 时必填
" 暂不支持

" ------------------- 摘要信息 ----------------------
" 摘要信息，用于显示在审批通知卡片、审批列表的摘要信息，最多3行
APPEND VALUE #( summary_info = VALUE #( ( text = '摘要1' lang = 'zh_CN' ) ) ) TO l_fc->summary_list.

" ------------------- 控件赋值 ----------------------

DATA: l_if_fc TYPE REF TO zif_wx_oa_fc.
" 控件: Text 发文标题
DATA(l_01_text) = NEW zcl_wx_oa_fc_text( id = `Text-1572857932948`).
" 赋值:必输
l_01_text->value-text = '<fixme>'.
" 数据写入内容
l_if_fc ?= l_01_text.
APPEND l_if_fc TO l_fc->apply_data-contents.

" 控件: Selector 发文类型
DATA(l_02_selector) = NEW zcl_wx_oa_fc_selector( id = `Selector-1573203804088` type = zcl_wx_oa_fc_selector=>single ).
" 赋值:必输
APPEND VALUE #( key = 'option-1573203804088' ) TO l_02_selector->value-selector-options. " 类型一
"APPEND VALUE #( key = 'option-1573203804089' ) TO l_02_selector->value-selector-options. " 类型二
" 数据写入内容
l_if_fc ?= l_02_selector.
APPEND l_if_fc TO l_fc->apply_data-contents.

" 控件: Number 文件编号
DATA(l_03_number) = NEW zcl_wx_oa_fc_number( id = `Number-1573203825701`).
" 赋值:必输
l_03_number->value-new_number = '10'.
" 数据写入内容
l_if_fc ?= l_03_number.
APPEND l_if_fc TO l_fc->apply_data-contents.

" 控件: Textarea 内容概要
DATA(l_04_textarea) = NEW zcl_wx_oa_fc_textarea( id = `Textarea-1573203837334`).
" 赋值:必输
l_04_textarea->value-text = '<fixme>'.
" 数据写入内容
l_if_fc ?= l_04_textarea.
APPEND l_if_fc TO l_fc->apply_data-contents.

" 控件: Textarea 备注
DATA(l_05_textarea) = NEW zcl_wx_oa_fc_textarea( id = `Textarea-1573203855514`).
" 赋值:非必输
" l_05_textarea->value-text = '<fixme>'.
" 数据写入内容
" l_if_fc ?= l_05_textarea.
" APPEND l_if_fc TO l_fc->apply_data-contents.

" 控件: File 附件
DATA(l_06_file) = NEW zcl_wx_oa_fc_file( id = `File-1573203844838`).
" 赋值:非必输
" APPEND VALUE #(
"   file_id   = '<fixme>' " 使用 zcl_wechat_approval->meida_upload( ) 上载获取
" " file_name = '' " 可选
" " file_size = '' " 可选
" " file_type = '' " 可选
" " file_url  = '' " 可选
" ) TO l_06_file->value-files.
" 数据写入内容
" l_if_fc ?= l_06_file.
" APPEND l_if_fc TO l_fc->apply_data-contents.

" ------------------- 发起审批 ----------------------
DATA: l_result TYPE string.
l_approval->send( EXPORTING data = l_fc IMPORTING result = l_result ).

BREAK-POINT.
