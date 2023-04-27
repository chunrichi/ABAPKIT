CLASS zcl_akit_email DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_attachment,
        file_type       TYPE so_obj_tp,      " 文件类型
        file_name       TYPE so_obj_des,     " 文件名
        file_size       TYPE so_obj_len,     " 文件大小
        file_lagu       TYPE so_obj_la,      " 文件语言
        file_text       TYPE soli_tab,       " 文件文本
        file_binary     TYPE solix_tab,      " 文件二进制数据
        attachment_head TYPE soli_tab,       " 文件标题
        virus_scan      TYPE vscan_profile,  "
      END OF ty_attachment .
    TYPES:
      tt_attachments TYPE TABLE OF ty_attachment .

    CLASS-METHODS sendmail
      IMPORTING
        VALUE(i_sender)           TYPE adr6-smtp_addr
        VALUE(i_title)            TYPE so_obj_des
        VALUE(i_html)             TYPE os_boolean DEFAULT abap_true
        VALUE(i_send_immediately) TYPE os_boolean DEFAULT abap_true
        VALUE(it_mail_text)       TYPE bcsy_text OPTIONAL
        VALUE(it_reciver)         TYPE bcsy_smtpa
        VALUE(it_carboncopy)      TYPE bcsy_smtpa OPTIONAL
        VALUE(it_bcarboncopy)     TYPE bcsy_smtpa OPTIONAL
        VALUE(it_attachments)     TYPE tt_attachments OPTIONAL
      RETURNING
        VALUE(subrc)              TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AKIT_EMAIL IMPLEMENTATION.


  METHOD sendmail.
    DATA: lr_send_request   TYPE REF TO cl_bcs,
          lr_document       TYPE REF TO cl_document_bcs,
          lr_fail           TYPE REF TO cx_bcs,
          lr_mail_recipient TYPE REF TO if_recipient_bcs,
          lr_mail_sender    TYPE REF TO if_sender_bcs.

    DATA: ls_mail_addr TYPE adr6-smtp_addr.

    DATA: lv_type   TYPE so_obj_tp,
          lv_result TYPE os_boolean.

    IF i_html = abap_true.
      lv_type = 'HTM'.
    ELSE.
      lv_type = 'RAW'.
    ENDIF.


    " 创建请求
    lr_send_request = cl_bcs=>create_persistent( ).

    " 创建邮件内容
    lr_document = cl_document_bcs=>create_document(
                    i_type = lv_type
                    i_text = it_mail_text[]
                    i_subject = i_title
                  ).

    " 添加发件人
    lr_mail_sender = cl_cam_address_bcs=>create_internet_address( i_sender ).
    lr_send_request->set_sender( i_sender = lr_mail_sender ).


    " 添加收件人
    LOOP AT it_reciver INTO ls_mail_addr.
      lr_mail_recipient = cl_cam_address_bcs=>create_internet_address( ls_mail_addr ).
      lr_send_request->add_recipient( lr_mail_recipient ).
      CLEAR: ls_mail_addr.
    ENDLOOP.


    " 添加抄送人
    LOOP AT it_carboncopy INTO ls_mail_addr.
      lr_mail_recipient = cl_cam_address_bcs=>create_internet_address( ls_mail_addr ).
      CALL METHOD lr_send_request->add_recipient
        EXPORTING
          i_recipient  = lr_mail_recipient
          i_express    = 'X'
          i_copy       = 'X'
          i_blind_copy = ' '
          i_no_forward = ' '.
      CLEAR: ls_mail_addr.
    ENDLOOP.

    " 添加密送人
    LOOP AT it_bcarboncopy INTO ls_mail_addr.
      lr_mail_recipient = cl_cam_address_bcs=>create_internet_address( ls_mail_addr ).
      CALL METHOD lr_send_request->add_recipient
        EXPORTING
          i_recipient  = lr_mail_recipient
          "i_express    = 'X'
          "i_copy       = 'X'
          i_blind_copy = 'X'
          i_no_forward = ' '.
    ENDLOOP.

    " 添加附件
    LOOP AT it_attachments INTO DATA(ls_attachment).
      lr_document->add_attachment(
         i_attachment_type     = ls_attachment-file_type
         i_attachment_subject  = ls_attachment-file_name
         i_attachment_size     = ls_attachment-file_size
         i_attachment_language = ls_attachment-file_lagu
         i_att_content_text    = ls_attachment-file_text
         i_att_content_hex     = ls_attachment-file_binary
         i_attachment_header   = ls_attachment-attachment_head
         iv_vsi_profile        = ls_attachment-virus_scan
       ).
    ENDLOOP.

    " 添加邮件内容
    lr_send_request->set_document( lr_document ).

    " 立即发送
    IF i_send_immediately = abap_true.
      lr_send_request->set_send_immediately( 'X' ).
    ENDIF.

    " 发送邮件
    lv_result = lr_send_request->send( ).
    IF lv_result = 'X'.
      CLEAR subrc.
      COMMIT WORK.
    ELSE.
      subrc = 4.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.
ENDCLASS.