REPORT zakit_demo_abap2xml.

TYPES: BEGIN OF ty_list,
         int TYPE i,
         str TYPE string,
       END OF ty_list.
DATA: line TYPE ty_list.

DATA: refdata TYPE c VALUE 'a'.

DATA: BEGIN OF data,
        message TYPE string,
        list    TYPE TABLE OF ty_list WITH EMPTY KEY,
        ints    TYPE TABLE OF i,
        ref     TYPE REF TO data,
        refline TYPE REF TO ty_list,
        _cm     TYPE string,
        cm      TYPE string,
        cm_m    TYPE string,
      END OF data.

line-int = 12.
line-str = 'str'.

data-message = 'test'.

data-list = VALUE #( ( int = 1 ) ( int = 2 str = '12' ) ).
data-ints = VALUE #( ( 1 ) ( 2 ) ).

data-ref = REF #( refdata ).
data-refline = REF #( line ).

data-_cm = '_cm'.
data-cm = 'cm'.
data-cm_m = 'cm_m'.

" 深层结构测试
" DATA(l_str) = zcl_akit_abap2xml=>serialize( data = data pretty_name = 'X' ).

" 结构测试
" DATA(l_str) = zcl_akit_abap2xml=>serialize( data = line pretty_name = 'L' ).

" 表测试
" DATA(l_str) = zcl_akit_abap2xml=>serialize( data = data-list pretty_name = 'L' ).

" 字段测试
" DATA(l_str) = zcl_akit_abap2xml=>serialize( data = line-str pretty_name = 'L' ).

" 引用测试
" DATA(l_str) = zcl_akit_abap2xml=>serialize( data = data-refline pretty_name = 'L' ).

" 根节点设置
DATA(l_str) = zcl_akit_abap2xml=>serialize( data = data pretty_name = 'X' rootname = 'Test' ).

cl_demo_output=>display( l_str ).