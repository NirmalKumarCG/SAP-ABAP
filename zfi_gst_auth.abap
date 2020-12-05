This program was an intergration of PHP and ABAP of a E-invoicing portal to modify/monitoring database level GSTN , E-Way bills of the client . 

*&---------------------------------------------------------------------*
*&  Include           ZFI_API_AUTH_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS     :  p_bukrs  TYPE bukrs          OBLIGATORY ,"DEFAULT '1000',
                  p_gstin  TYPE zstcd3         OBLIGATORY ,"DEFAULT '33AAACA4651L1ZT',
                  p_client TYPE zclient_id     OBLIGATORY ,"DEFAULT 'AAACA33TXP2MQTZ',
                  p_clsec  TYPE zclient_secret OBLIGATORY ,"DEFAULT 'vj0PDd4r86LlZzFTHhBb',
                  p_uname  TYPE zchar_uname    OBLIGATORY ,"DEFAULT 'Ashokley33',
                  p_pass   TYPE zchar_pass     OBLIGATORY ."DEFAULT 'Ashokley33'.
SELECTION-SCREEN END OF BLOCK b1.


*&---------------------------------------------------------------------*
*&  Include           ZFI_API_AUTH_TOP
*&---------------------------------------------------------------------*

  DATA gv_appkey TYPE string.

  TYPES: BEGIN OF ty_input,
           gstin                   TYPE zstcd3,
           clientid                TYPE zclient_id,
           clientsecret            TYPE zclient_secret,
           username                TYPE char50,
           password                TYPE char50,
           appkey                  TYPE string,
           forcerefreshaccesstoken TYPE char05,
         END OF ty_input.

  DATA it_token  TYPE STANDARD TABLE OF zfi_get_token.
  DATA wa_token  TYPE zfi_get_token.
  DATA it_detail TYPE STANDARD TABLE OF zfi_token_det.
  DATA wa_detail TYPE zfi_token_det.

  DATA it_input TYPE STANDARD TABLE OF ty_input.
  DATA wa_input TYPE ty_input.

  DATA gv_ser_input TYPE string. " Serialized Output

  DATA gv_response TYPE string. " Final Response from PHP

  DATA gv_auth TYPE string.     " Authentication Indicator

*-Declaration for deserialization-*

  TYPES : BEGIN OF ty_deser,
            clientid    TYPE char50,
            username    TYPE char50,
            authtoken   TYPE char100,
            sek         TYPE string,
            tokenexpiry TYPE char20,
            infodtls    TYPE string,
          END OF ty_deser.

  DATA : it_deser TYPE STANDARD TABLE OF ty_deser,
         wa_deser TYPE ty_deser.

  DATA it_string TYPE string_table.

  DATA : gc_url TYPE string.

  IF sy-sysid = 'QE1'.
    gc_url = 'http://aleinv.ashokleyland.com/authentication.php'.
  ELSEIF sy-sysid = 'PE1'.
    gc_url = 'http://aleinvprod.ashokleyland.com/authentication.php'." prodcution URL
  ENDIF.





*&---------------------------------------------------------------------*
*&  Include           ZFI_API_AUTH_SUB
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_INPUT
*&---------------------------------------------------------------------*
*       To get input values
*----------------------------------------------------------------------*
FORM get_input.
  REFRESH it_token.

  SELECT * FROM zfi_get_token
           INTO TABLE it_token
           WHERE company_code = p_bukrs
           AND gstin = p_gstin
           AND client_id = p_client
           AND client_secret = p_clsec
           AND user_name = p_uname
           AND password = p_pass.


  PERFORM generate_appkey.
  PERFORM serialize.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GENERATE_APPKEY
*&---------------------------------------------------------------------*
*       To generate 32 digit random App Key                            *
*----------------------------------------------------------------------*
FORM generate_appkey .
  CONSTANTS lc_32    TYPE i VALUE 32.
  CONSTANTS lc_true TYPE char05 VALUE 'true'.

*-FM to create AppKey
  CLEAR gv_appkey.
  CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
    EXPORTING
      number_chars  = lc_32
    IMPORTING
      random_string = gv_appkey.

*-Transfer the values for serialization-*
  IF sy-subrc IS INITIAL.
    REFRESH it_input.
    LOOP AT it_token INTO wa_token.
      CLEAR wa_input.
      wa_input-gstin                   = p_gstin.
      wa_input-clientid                = p_client.
      wa_input-clientsecret            = p_clsec.
      wa_input-username                = wa_token-user_name.
      wa_input-password                = wa_token-password.
      wa_input-forcerefreshaccesstoken = lc_true.
      wa_input-appkey                  = gv_appkey.
      APPEND wa_input TO it_input.
    ENDLOOP.
    IF it_token IS INITIAL AND it_input IS INITIAL.


      wa_input-gstin                   = p_gstin.
      wa_input-clientid                = p_client.
      wa_input-clientsecret            = p_clsec.
      wa_input-username                = p_uname.
      wa_input-password                = p_pass.
      wa_input-forcerefreshaccesstoken = lc_true.
      wa_input-appkey                  = gv_appkey.
      APPEND wa_input TO it_input.
      MOVE-CORRESPONDING wa_input TO wa_token.
      wa_token-client_id = p_client.
      wa_token-client_secret            = p_clsec.
      wa_token-user_name                = p_uname.
      wa_token-company_code = p_bukrs.
      APPEND wa_token TO it_token.
      IF it_token IS NOT INITIAL.
        MODIFY zfi_get_token FROM TABLE it_token.
        CALL FUNCTION 'DB_COMMIT'.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SERIALIZE
*&---------------------------------------------------------------------*
*       Serialize the inputs in JSON format
*----------------------------------------------------------------------*
FORM serialize .

*-JSON converter class-Method-> Serialize method to convert data in JSON

  IF it_input IS INITIAL.
    MESSAGE : 'No data found' TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT it_input INTO wa_input.
    CLEAR gv_ser_input.

    gv_ser_input =
    /ui2/cl_json=>serialize(
    data = wa_input
    compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    APPEND gv_ser_input TO it_string.
  ENDLOOP.


*-Concatenate [$$$$$$-Uncomment this if required-$$$$$]-*
*****  READ TABLE it_input INTO wa_input INDEX 1.
*****  CLEAR gv_ser_input.
*****  CONCATENATE '{'
*****              '"data"' ':' '{'
*****              '"UserName"' ':' '"' wa_input-username '"' ','
*****              '"Password"' ':' '"' wa_input-password '"' ','
*****              '"AppKey"' ':' '"' wa_input-appkey '"' ','
*****              '"ForceRefreshAccessToken"' ':' wa_input-forcerefreshaccesstoken
*****              '}'
*****              '}'
*****  INTO gv_ser_input.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONNECT_PHP
*&---------------------------------------------------------------------*
*       Connect with PHP and get the output
*----------------------------------------------------------------------*

FORM connect_php .

  DATA it_response TYPE string_table.

  DATA lo_http_client TYPE REF TO if_http_client.


  cl_http_client=>create_by_url(

  EXPORTING

  url = yclass_concat_string=>concat_string( url = gc_url it_string = it_string )

  IMPORTING

  client = lo_http_client

  EXCEPTIONS

  argument_not_found = 1

  plugin_not_active = 2

  internal_error = 3

  OTHERS = 4 ).

  IF sy-subrc <> 0.

    RETURN.

  ENDIF.

*- Sending the request -*

  lo_http_client->send(

  EXCEPTIONS

  http_communication_failure = 1

  http_invalid_state = 2 ).

*- Receiving the response -*

  lo_http_client->receive(

  EXCEPTIONS

  http_communication_failure = 1

  http_invalid_state = 2

  http_processing_failed = 3 ).

*- Check the response. Hopefully you get back a JSON response -*

  CLEAR gv_response.

  gv_response = lo_http_client->response->get_cdata( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONNECT_PHP_THRU_POST
*&---------------------------------------------------------------------*
*       Connect with PHP through "POST" method
*----------------------------------------------------------------------*
FORM connect_php_thru_post .

  DATA lo_http_client TYPE REF TO if_http_client.

  cl_http_client=>create_by_url(

  EXPORTING

  url = gc_url

  IMPORTING

  client = lo_http_client

  EXCEPTIONS

  argument_not_found = 1

  plugin_not_active = 2

  internal_error = 3

  OTHERS = 4 ).

  IF sy-subrc <> 0.

    RETURN.

  ENDIF.

*- POST -*

  CALL METHOD lo_http_client->request->set_method
    EXPORTING
      method = if_http_request=>co_request_method_post.

*-Set Content-*

  CALL METHOD lo_http_client->request->set_content_type(
      content_type = 'application/json' ).

*-SET CDATA-*

  CALL METHOD lo_http_client->request->set_cdata
    EXPORTING
      data = gv_ser_input.

*- Sending the request -*

  lo_http_client->send(

  EXCEPTIONS

  http_communication_failure = 1

  http_invalid_state = 2 ).

*- Receiving the response -*

  lo_http_client->receive(

  EXCEPTIONS

  http_communication_failure = 1

  http_invalid_state = 2

  http_processing_failed = 3 ).

*-To check connectivity status-*

  CALL METHOD lo_http_client->response->get_status
    IMPORTING
      code   = DATA(lv_code)
      reason = DATA(lv_reason).

 CLEAR gv_response.

  gv_response = lo_http_client->response->get_cdata( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DESERIALIZE
*&---------------------------------------------------------------------*
*       Deserialize PHP output into SAP internal format
*----------------------------------------------------------------------*

FORM deserialize.

*-JSON converter class - Method -> Deserialize convert JSON string data into internal table-*

  DATA gv_response1 TYPE string.
  DATA gv_response2 TYPE string.
  DATA gv_response3 TYPE string.

  REFRESH it_deser.
  IF gv_response IS NOT INITIAL.
*****  CONCATENATE '{"Status":"1","ErrorDetails":null,"Data":{"ClientId":"AAACA33TXP2MQTZ","UserName":"Ashokley33",'
*****  '"AuthToken":"j3SMztlYWxxCdhjwb3Ag8AaDE","Sek":"DO9xO/hWiXDUPO4U9ZjlhL1X6QHr+fUWfYcEY/w65OjkAbCgJh/V2acX0tOU/eaE","TokenExpiry":"2020-02-12 10:15:37"}}'
*****  INTO gv_response.

    SPLIT gv_response AT '{' INTO gv_response1 gv_response2 gv_response3.
*    REPLACE ALL OCCURRENCES OF '}}' IN gv_response3 WITH '}]'.
    REPLACE ALL OCCURRENCES OF '}' IN gv_response3 WITH ' '.
    CONDENSE gv_response3.
    gv_response3 = '[{' && gv_response3 && '}]'.

    IF gv_response2 IS NOT INITIAL.
      IF gv_response2+9(1) = '1'. " Check whether response is error free
        IF gv_response3 IS NOT INITIAL.

          /ui2/cl_json=>deserialize(
            EXPORTING json = gv_response3
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
            CHANGING data = it_deser ).

          IF it_deser IS NOT INITIAL.
            PERFORM transfer_dat.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE 'Error Data' TYPE 'E'.
      ENDIF.
    ELSE.
      MESSAGE gv_response TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'No Response from IRP' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DAT
*&---------------------------------------------------------------------*
*       Transfer data to ZFI_TOKEN_DET
*----------------------------------------------------------------------*
FORM transfer_dat .
  DATA lv_date TYPE char08.
  DATA lv_time TYPE char08.

  CLEAR gv_auth.

  REFRESH it_detail.
  READ TABLE it_token INTO wa_token INDEX 1.
  IF sy-subrc IS INITIAL.
    READ TABLE it_deser INTO wa_deser INDEX 1.
    IF sy-subrc IS INITIAL.
*-Split date and time-*
      CLEAR : lv_date, lv_time.
      CONCATENATE wa_deser-tokenexpiry+0(4) wa_deser-tokenexpiry+5(2)
                  wa_deser-tokenexpiry+8(2) INTO lv_date.
      CONCATENATE wa_deser-tokenexpiry+11(2) wa_deser-tokenexpiry+14(2)
                  wa_deser-tokenexpiry+17(2) INTO lv_time.
*-*
      CLEAR wa_detail.
      wa_detail-company_code  = wa_token-company_code.
      wa_detail-gstin         = wa_token-gstin.
      wa_detail-valid_from_dt = sy-datum.
      wa_detail-valid_from_tm = sy-uzeit.
      wa_detail-valid_to_dt   = lv_date.
      wa_detail-valid_to_tm   = lv_time.
      wa_detail-auth_token    = wa_deser-authtoken.
      wa_detail-sek           = wa_deser-sek.
      wa_detail-app_key       = gv_appkey.
      wa_detail-cr_date       = sy-datum.

      APPEND wa_detail TO it_detail.
      gv_auth  = 'X'.
    ENDIF.
  ENDIF.

*-Z-table Update-*
  IF it_detail IS NOT INITIAL.
    MODIFY zfi_token_det FROM TABLE it_detail.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
      IF gv_auth IS NOT INITIAL.
        MESSAGE 'Authentication Completed Successfully ! . . . ' TYPE 'I' DISPLAY LIKE 'S'.
*        LEAVE to SCREEN 0.
      ELSE.
        MESSAGE 'Authentication Failed ! . . . ' TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE PROGRAM.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'No Data Found' TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
