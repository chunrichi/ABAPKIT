REPORT zakit_docx_write_temp.


START-OF-SELECTION.

  TYPES: BEGIN OF ty_tab001,
           id   TYPE string,
           text TYPE string,
         END OF ty_tab001,
         tt_tab001 TYPE STANDARD TABLE OF ty_tab001 WITH DEFAULT KEY.
  TYPES: BEGIN OF ty_data,
           uname     TYPE string,
           timestamp TYPE string,
           first     TYPE string,
           secend    TYPE string,
           BEGIN OF namespace,
             data TYPE string,
           END OF namespace,
           tab001    TYPE tt_tab001,
           header    TYPE string,
         END OF ty_data.

  DATA: ls_list TYPE ty_data.

  ls_list-header = '这是一个头'.
  ls_list-uname = sy-uname.
  ls_list-timestamp = sy-datum && sy-uzeit.
  ls_list-first  = '连续 1'.
  "ls_list-secend = '连续 2'.
  ls_list-namespace-data = '嵌入数据'.

  APPEND VALUE #( id = 1 text = 'this is text 01 a' ) TO ls_list-tab001.
  APPEND VALUE #( id = 2 text = 'this is text 02 b' ) TO ls_list-tab001.
  APPEND VALUE #( id = 3 text = 'this is text 03 c' ) TO ls_list-tab001.
  APPEND VALUE #( id = 3 text = 'this is text 03 c' ) TO ls_list-tab001.
  APPEND VALUE #( id = 3 text = 'this is text 03 c' ) TO ls_list-tab001.
  APPEND VALUE #( id = 3 text = 'this is text 03 c' ) TO ls_list-tab001.
  APPEND VALUE #( id = 3 text = 'this is text 03 c' ) TO ls_list-tab001.
  APPEND VALUE #( id = 3 text = 'this is text 03 c' ) TO ls_list-tab001.

  DATA(l_word_expo) = NEW zcl_akit_docx_tool( ).

  l_word_expo->load_file_smw0( 'ZDOCX_TEMP_001' ).

  l_word_expo->process_expo( data = REF #( ls_list ) ).

  l_word_expo->store( ).