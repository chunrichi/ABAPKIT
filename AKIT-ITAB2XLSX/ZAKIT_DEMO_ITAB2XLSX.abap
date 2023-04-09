REPORT zakit_demo_itab2xlsx.

" 附件生成

" 内容查询
SELECT
  st~werks,
  st~matnr,
  kt~maktx,
  3t~wgbez,
  ra~matkl,
  ra~mtart
  FROM mast AS st
  LEFT JOIN makt AS kt ON kt~matnr = st~matnr AND kt~spras = '1'
  LEFT JOIN mara AS ra ON ra~matnr = st~matnr
  LEFT JOIN t023t AS 3t ON 3t~matkl = ra~matkl AND 3t~spras = '1'
  UP TO 20 ROWS
  INTO TABLE @DATA(lt_mast).

" set field cat
DATA: lt_fieldcat TYPE lvc_t_fcat.
APPEND VALUE #( fieldname = 'WERKS' ref_table = 'MAST'  ref_field = 'WERKS' coltext = '工厂'     ) TO lt_fieldcat.
APPEND VALUE #( fieldname = 'MATNR' ref_table = 'MAST'  ref_field = 'MATNR' coltext = '物料'     ) TO lt_fieldcat.
APPEND VALUE #( fieldname = 'MAKTX' ref_table = 'MAKT'  ref_field = 'MAKTX' coltext = '物料描述' ) TO lt_fieldcat.
APPEND VALUE #( fieldname = 'WGBEZ' ref_table = 'T023T' ref_field = 'WGBEZ' coltext = '物料组' ) TO lt_fieldcat.
APPEND VALUE #( fieldname = 'MATKL' ref_table = 'MARA'  ref_field = 'MATKL' coltext = '物料组' ) TO lt_fieldcat.
APPEND VALUE #( fieldname = 'MTART' ref_table = 'MARA'  ref_field = 'MTART' coltext = '物料组' ) TO lt_fieldcat.

" get refrerence of itab
GET REFERENCE OF lt_mast INTO DATA(lr_data_ref).

" gen xlsx xstring
DATA(lr_xlsx) = NEW zcl_akit_itab2xlsx( ).
DATA(lv_xstring) = lr_xlsx->itab_to_xstring( EXPORTING ir_data_ref = lr_data_ref
                                                       it_fcat = lt_fieldcat ).

" 如有需要可下载到本地
lr_xlsx->xstring_to_xlsx( iv_xstring = lv_xstring iv_path = 'D:\demo.xlsx' ).