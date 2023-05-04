REPORT zcl_akit_email.

" 邮件发送

CONSTANTS: lc_test_html TYPE abap_bool VALUE abap_false.

" --> 发送邮件
DATA: lv_sender    TYPE adr6-smtp_addr,
      lv_title     TYPE so_obj_des,
      lt_mail_text TYPE bcsy_text,
      lt_reciver   TYPE bcsy_smtpa,
      lt_attach    TYPE zcl_akit_email=>tt_attachments.
DATA: ls_attach    LIKE LINE OF lt_attach.

" 发送者邮箱
lv_sender = `demo@demo.com`.

" 标题
lv_title  = `demo 邮件`.

" 收件人
APPEND 'demo_reciver01' TO lt_reciver.
APPEND 'demo_reciver02' TO lt_reciver.

" 抄送人
" APPEND 'demo_reciver03' TO LT_CARBONCOPY.

IF lc_test_html = abap_true.

  " 正文(HTML)
  APPEND `<table border="1">` TO lt_mail_text.
  APPEND `  <tr>` TO lt_mail_text.
  APPEND `    <th>Month</th>` TO lt_mail_text.
  APPEND `    <th>Savings</th>` TO lt_mail_text.
  APPEND `  </tr>` TO lt_mail_text.
  APPEND `  <tr>` TO lt_mail_text.
  APPEND `    <td>January</td>` TO lt_mail_text.
  APPEND `    <td>$100</td>` TO lt_mail_text.
  APPEND `  </tr>` TO lt_mail_text.
  APPEND `</table>` TO lt_mail_text.
ELSE.

  " 正文 (txt)
  APPEND `尊贵的用户，您好，` TO lt_mail_text.
  APPEND `    这是一封测试邮件` TO lt_mail_text.
ENDIF.


" 附件
" ls_attach-file_type = 'BIN'.
" ls_attach-file_name = '附件.xlsx'.
" ls_attach-file_size = xstrlen( lv_xstring ).
" ls_attach-file_binary = cl_document_bcs=>xstring_to_solix( lv_xstring ).
" APPEND ls_attach TO lt_attach.

IF zcl_akit_email=>sendmail(
  i_sender           = lv_sender
  i_title            = lv_title
  i_html             = lc_test_html
  i_send_immediately = 'X' " 立即发送
  it_mail_text       = lt_mail_text
  it_reciver         = lt_reciver
 "IT_CARBONCOPY  " 抄送人
 "IT_BCARBONCOPY " 密送人
 "IT_ACCACHMENTS " 附件
) = 0.
  MESSAGE 'SEND SUCCESS' TYPE 'S'.
ELSE.
  MESSAGE 'SEND FAIL' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.
