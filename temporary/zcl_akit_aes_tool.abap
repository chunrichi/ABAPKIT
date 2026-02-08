CLASS zcl_akit_aes_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ts_encryption_mode TYPE c LENGTH 10 .
    TYPES:
      ts_padding_standard TYPE c LENGTH 10 .

    CONSTANTS mc_encryption_mode_cbc TYPE ts_encryption_mode VALUE 'CBC' ##NO_TEXT.
    CONSTANTS mc_padding_standard_pkcs7 TYPE ts_padding_standard VALUE 'PKCS7'. "#EC NOTEXT
    CONSTANTS mc_padding_standard_none  TYPE ts_padding_standard VALUE 'NONE'.

    CLASS-METHODS encrypt_xstring
      IMPORTING
        !iv_key                   TYPE xstring
        !iv_data                  TYPE xstring
        !iv_initialization_vector TYPE xstring OPTIONAL
        !iv_padding_standard      TYPE ts_padding_standard OPTIONAL
        !iv_encryption_mode       TYPE ts_encryption_mode OPTIONAL
      EXPORTING
        !ev_data                  TYPE xstring .
    CLASS-METHODS decrypt_xstring
      IMPORTING
        !iv_key                   TYPE xstring
        !iv_data                  TYPE xstring
        !iv_initialization_vector TYPE xstring OPTIONAL
        !iv_padding_standard      TYPE ts_padding_standard OPTIONAL
        !iv_encryption_mode       TYPE ts_encryption_mode OPTIONAL
      EXPORTING
        !ev_data                  TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS encrypt_cbc
      IMPORTING
        !iv_key                   TYPE xstring
        !iv_data                  TYPE xstring
        !iv_initialization_vector TYPE xstring OPTIONAL
      EXPORTING
        !ev_data                  TYPE xstring .

    CLASS-METHODS decrypt_cbc
      IMPORTING
        !iv_key                   TYPE xstring
        !iv_data                  TYPE xstring
        !iv_initialization_vector TYPE xstring OPTIONAL
      EXPORTING
        !ev_data                  TYPE xstring .

    CLASS-METHODS pkcs7
      IMPORTING
                iv_data        TYPE xstring
      RETURNING VALUE(rv_data) TYPE xstring.
ENDCLASS.



CLASS ZCL_AKIT_AES_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_AKIT_AES_TOOL=>DECRYPT_CBC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        XSTRING
* | [--->] IV_DATA                        TYPE        XSTRING
* | [--->] IV_INITIALIZATION_VECTOR       TYPE        XSTRING(optional)
* | [<---] EV_DATA                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD decrypt_cbc.

    DATA: lv_algorithm TYPE string,
          lv_xdata     TYPE xstring.

    DATA(lv_keysize) = xstrlen( iv_key ).

    IF lv_keysize <= 16.
      lv_algorithm = cl_sec_sxml_writer=>co_aes128_algorithm.
    ELSEIF lv_keysize <= 32.
      lv_algorithm = cl_sec_sxml_writer=>co_aes192_algorithm.
    ELSE.
      lv_algorithm = cl_sec_sxml_writer=>co_aes256_algorithm.
    ENDIF.

    lv_xdata = iv_initialization_vector && iv_data.

    CALL METHOD cl_sec_sxml_writer=>decrypt(
      EXPORTING
        ciphertext = lv_xdata
        key        = iv_key
        algorithm  = lv_algorithm
      IMPORTING
        plaintext  = ev_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_AKIT_AES_TOOL=>DECRYPT_XSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        XSTRING
* | [--->] IV_DATA                        TYPE        XSTRING
* | [--->] IV_INITIALIZATION_VECTOR       TYPE        XSTRING(optional)
* | [--->] IV_PADDING_STANDARD            TYPE        TS_PADDING_STANDARD(optional)
* | [--->] IV_ENCRYPTION_MODE             TYPE        TS_ENCRYPTION_MODE(optional)
* | [<---] EV_DATA                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD decrypt_xstring.
    DATA: lv_len   TYPE i.


    IF iv_encryption_mode = mc_encryption_mode_cbc.
      decrypt_cbc( EXPORTING iv_key                   = iv_key
                             iv_data                  = iv_data
                             iv_initialization_vector = iv_initialization_vector
                   IMPORTING ev_data                  = ev_data ).
    ENDIF.


    IF iv_padding_standard = mc_padding_standard_pkcs7.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_AKIT_AES_TOOL=>ENCRYPT_CBC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        XSTRING
* | [--->] IV_DATA                        TYPE        XSTRING
* | [--->] IV_INITIALIZATION_VECTOR       TYPE        XSTRING(optional)
* | [<---] EV_DATA                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD encrypt_cbc.
    DATA: lv_algorithm TYPE string.

    DATA(lv_keysize) = xstrlen( iv_key ).

    IF lv_keysize <= 16.
      lv_algorithm = cl_sec_sxml_writer=>co_aes128_algorithm.
    ELSEIF lv_keysize <= 32.
      lv_algorithm = cl_sec_sxml_writer=>co_aes192_algorithm.
    ELSE.
      lv_algorithm = cl_sec_sxml_writer=>co_aes256_algorithm.
    ENDIF.


    CALL METHOD cl_sec_sxml_writer=>encrypt_iv(
      EXPORTING
        plaintext  = iv_data
        key        = iv_key
        iv         = iv_initialization_vector
        algorithm  = lv_algorithm
      IMPORTING
        ciphertext = ev_data ).



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_AKIT_AES_TOOL=>ENCRYPT_XSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        XSTRING
* | [--->] IV_DATA                        TYPE        XSTRING
* | [--->] IV_INITIALIZATION_VECTOR       TYPE        XSTRING(optional)
* | [--->] IV_PADDING_STANDARD            TYPE        TS_PADDING_STANDARD(optional)
* | [--->] IV_ENCRYPTION_MODE             TYPE        TS_ENCRYPTION_MODE(optional)
* | [<---] EV_DATA                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD encrypt_xstring.
    DATA: lv_xdata TYPE xstring,
          lv_len   TYPE i.


    IF iv_padding_standard = mc_padding_standard_pkcs7.
      lv_xdata = pkcs7( iv_data ).
    ENDIF.

    IF iv_encryption_mode = mc_encryption_mode_cbc.
      encrypt_cbc( EXPORTING iv_key                   = iv_key
                             iv_data                  = lv_xdata
                             iv_initialization_vector = iv_initialization_vector
                   IMPORTING ev_data                  = ev_data ).
    ENDIF.


    IF iv_padding_standard = mc_padding_standard_pkcs7.
      lv_len = xstrlen( ev_data ) - 16.

      ev_data = ev_data(lv_len).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_AKIT_AES_TOOL=>PKCS7
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        XSTRING
* | [<-()] RV_DATA                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD pkcs7.
    DATA: lv_len     TYPE i,
          lv_pad_len TYPE i,
          lv_pad_hex TYPE x.
    CONSTANTS: lc_16 TYPE i VALUE 16.

    lv_len = xstrlen( iv_data ).

    rv_data = iv_data.

    IF lv_len <= lc_16 * 2.
      RETURN.
    ENDIF.

    lv_pad_len = lc_16 - ( lv_len MOD lc_16 ).

    lv_pad_hex = lv_pad_len.

    DO lv_pad_len TIMES.
      CONCATENATE rv_data lv_pad_hex INTO rv_data IN BYTE MODE.
    ENDDO.

  ENDMETHOD.
ENDCLASS.