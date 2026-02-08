REPORT ztest_aes_cbc.

DATA: lv_data   TYPE string,
      lv_key    TYPE string,
      lv_result TYPE string.


DATA: lv_xdata   TYPE xstring,
      lv_xkey    TYPE xstring,
      lv_xiv     TYPE xstring,
      lv_xresult TYPE xstring.

lv_xkey = '9D34721CB514365FA11DE89A49D7B415'.
lv_xiv  = 'D2438BC75C58F49B6FCB6246F666DC7E'.

lv_data = `Hello, AES-CBC with PKCS7!88888723adsdadfd d d d da s d`.

CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
  EXPORTING
    text   = lv_data
  IMPORTING
    buffer = lv_xdata
  EXCEPTIONS
    failed = 1
    OTHERS = 2.
IF sy-subrc <> 0.
  BREAK-POINT.
ENDIF.



zcl_akit_aes_tool=>encrypt_xstring(
  EXPORTING
    iv_key                   = lv_xkey
    iv_data                  = lv_xdata
    iv_initialization_vector = lv_xiv
    iv_padding_standard      = 'PKCS7'
    iv_encryption_mode       = 'CBC'
  IMPORTING
    ev_data                  = lv_xresult
).


lv_xresult = lv_xresult+16.

CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
  EXPORTING
    input  = lv_xresult
  IMPORTING
    output = lv_result.

BREAK-POINT.


zcl_akit_aes_tool=>decrypt_xstring(
  EXPORTING
    iv_key                   = lv_xkey
    iv_data                  = lv_xresult
    iv_initialization_vector = lv_xiv
    iv_padding_standard      = 'PKCS7'
    iv_encryption_mode       = 'CBC'
  IMPORTING
    ev_data                  = lv_xdata
).


BREAK-POINT.