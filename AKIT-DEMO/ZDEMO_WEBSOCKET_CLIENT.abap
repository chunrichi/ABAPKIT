REPORT zdemo_websocket_client.

" https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenapc_ws_client_abexa.htm

" socket 测试

" 处理方法 handler

CLASS lcl_apc_handler DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_apc_wsp_event_handler.
    DATA message TYPE string.

ENDCLASS.

CLASS lcl_apc_handler IMPLEMENTATION.

  METHOD if_apc_wsp_event_handler~on_open.

  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_message.
    TRY.
        message = i_message->get_text( ).
      CATCH cx_apc_error INTO DATA(apc_error).
        message = apc_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_close.
    message = 'Connection closed!'.
  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_error.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  PERFORM frm_process.


*&---------------------------------------------------------------------*
*& Form FRM_PROCESS
*&---------------------------------------------------------------------*
*&  处理
*&---------------------------------------------------------------------*
FORM frm_process .

  TRY.

      DATA(lo_event_handler) = NEW lcl_apc_handler( ).

      " Client
      DATA(lo_client) = cl_apc_wsp_client_manager=>create_by_url(
            i_url           = 'ws://demo.com:8010/sap/bc/apc/sap/zdemo_001'
            i_event_handler = lo_event_handler ).

      " 设置请求头(认证)
      DATA(lo_request) = lo_client->get_context( )->get_initial_request( ).
      lo_request->set_header_field( i_name = 'Authorization' i_value = `Basic demo=` ).

      lo_client->connect( ).

      " 发送消息
      DATA(lo_message_manager) = CAST if_apc_wsp_message_manager( lo_client->get_message_manager( ) ).
      DATA(lo_message) = CAST if_apc_wsp_message( lo_message_manager->create_message( ) ).

      lo_message->set_text( `测试` ).
      lo_message_manager->send( CAST #( lo_message ) ).

      " 等待返回
      CLEAR lo_event_handler->message.
      WAIT FOR PUSH CHANNELS
           UNTIL lo_event_handler->message IS NOT INITIAL
           UP TO 10 SECONDS.
      IF sy-subrc = 4.
        cl_demo_output=>display(
          'No handler for APC messages registered!' ).
      ELSEIF sy-subrc = 8.
        cl_demo_output=>display(
          'Timeout occurred!' ).
      ELSE.
        cl_demo_output=>display(
          |TCP client received:\n\n{ lo_event_handler->message }| ).
      ENDIF.

      lo_client->close(
        i_reason = 'Application closed connection!' ).
    CATCH cx_apc_error INTO DATA(lcx_error).
      cl_demo_output=>display( lcx_error->get_text( ) ).
  ENDTRY.

ENDFORM.