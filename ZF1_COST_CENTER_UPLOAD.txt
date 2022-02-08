REPORT  ZFI_COST_CENTER_UPLOAD
                            NO STANDARD PAGE HEADING
                            LINE-SIZE  100
                            LINE-COUNT 65.
************************************************************************
* This program uploads a flat file from the PC and uploades the         *
* Cost Center by calling Transaction KS01                    .*
************************************************************************
*                         CHANGE HISTORY                               *
*----------------------------------------------------------------------*
* Version# | Date     | Changed by | Description                       *
*----------+----------+------------+-----------------------------------*
*          | 06.03.2009 | Suresh G      | Initial program              *
************************************************************************
*----------------------------------------------------------------------*
*Tables
TABLES : csks,   "Cost Center Master Data
         tka05,  "Cost Center Types
         cepc.   "Profit Center Master Data Table
* Type-pools
TYPE-POOLS : truxs.
* include program for common routines for interface programs
DATA: zzret_code LIKE sy-subrc,
      v_open.
DATA: it_raw TYPE truxs_t_text_data.
* constants - these values will stay the same
CONSTANTS: c_tcode(4)      TYPE c   VALUE 'FB01'.

DATA: w_upllen             TYPE i   VALUE '2000',
      err_msg(100)         TYPE c,
*      sap_cusno            LIKE kna1-kunnr,
      batch_name(30)       TYPE c.

* table for transaction status
DATA: BEGIN OF status_tab OCCURS 0,
        status(10),
        btc_sess(12)       TYPE c,
*        cus_no             LIKE kna1-kunnr,
        message(100),
      END OF status_tab.

DATA: BEGIN OF inrec OCCURS 0,
        filler(3000),
      END OF inrec.
* format of the input file
DATA: BEGIN OF t_indata OCCURS 0,
      cost_center(20),
        cont_area(20),
        valid_f(10),
        valid_t(10),
        name(20),
        description(40),
        per_resp(20),
        category(10),
        hier_ar(10),
        bus_ar(10),
        currency(10),
        profit_center(10),
      END OF t_indata.
*Format for Download file
DATA: BEGIN OF t_outdat OCCURS 0,
      cost_center(20),
        cont_area(20),
        valid_f(10),
        valid_t(10),
        name(20),
        description(40),
        per_resp(20),
        category(10),
        hier_ar(10),
        bus_ar(10),
        currency(10),
        profit_center(10),
      END OF t_outdat.

*Calculating the total number of records
DATA: BEGIN OF tot,
        read               TYPE i,
        ok                 TYPE i,
        err                TYPE i,
      END OF tot.
DATA: v_msgno LIKE sy-msgno.

DATA :BEGIN OF  t_err  OCCURS 0.
INCLUDE  STRUCTURE t_outdat.
DATA : desc TYPE string.
DATA : sort TYPE c.
DATA : END OF t_err.
*Message tab for BDC
DATA:  messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : t_ddown LIKE t_indata OCCURS 0 WITH HEADER LINE.
DATA : w_initial LIKE t_outdat.
DATA : BEGIN OF w_init1.
        INCLUDE STRUCTURE w_initial.
DATA : desc TYPE string.
DATA : END OF w_init1.
*Final internal table for Download
DATA : t_fdown LIKE w_init1 OCCURS 0 WITH HEADER LINE.
DATA : g_init TYPE i,
       g_mess TYPE string,
       v_flag TYPE c,
       v_flag1 TYPE c.
INCLUDE zbdcincl.
*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK 001 WITH FRAME TITLE text-001.
PARAMETERS: p_upl       LIKE rlgrap-filename DEFAULT 'c:\temp\parbcust.xls',
            p_head       AS CHECKBOX DEFAULT 'X',     " With header line? X or Y for yes
            p_test      AS CHECKBOX,     " test run? X or Y for yes
            p_sess(12)  TYPE c ,
            p_mode     .
PARAMETERS: p_rest      LIKE rlgrap-filename
                         DEFAULT 'c:\temp\Success.xls'.
SELECTION-SCREEN : END OF BLOCK 001.
*----------------------------------------------------------------------*
* At Selection-screen                                                  *
*----------------------------------------------------------------------*
*Get File name for Upload
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upl.
  CALL FUNCTION 'WS_FILENAME_GET'
    IMPORTING
      filename         = p_upl
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
*Get File name for download in download radio Button
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rest.
  CALL FUNCTION 'WS_FILENAME_GET'
    IMPORTING
      filename         = p_rest
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
*----------------------------------------------------------------------*
*  Start Of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
*Perform To Upload The File
  PERFORM 100_upload_file.
  IF v_flag EQ 'X'.
    CLEAR : v_flag.
    WRITE: /01      text-001 ,
         27        ':',
         30      sy-repid,
*          92      text-001,
         92      sy-pagno,
        /01      text-003,
         10      'by',
         27      ':',
         30      sy-uname,
         82      sy-datum DD/MM/YY,
         92      sy-uzeit.

    ULINE.
    SKIP 1.
    ULINE.
    WRITE : 'Error in Uploading the File'.
    ULINE.
    EXIT.
  ELSE.
    CLEAR : v_flag.
  ENDIF.
*Checking Whether the Internal table is initial or not
  IF t_indata[] IS INITIAL.
    WRITE: / 'No records were uploaded.'.
    ULINE.
    EXIT.
  ENDIF.
*Adding the intial line of the xls.
  PERFORM initial_line.
  PERFORM validate_fields.
  IF p_test IS INITIAL.
    PERFORM create_cost_center.
  ENDIF.
*----------------------------------------------------------------------*
* End of Selection                                                  *
*----------------------------------------------------------------------*
END-OF-SELECTION.
*Sorting the Success and error files in a internal table
  IF  t_err[] IS NOT INITIAL.
    SORT t_err BY sort.
    READ TABLE t_err INDEX 1.
    IF t_err-sort = 'A'.
      CLEAR : t_err.
      t_err-cost_center = 'Success'.
      INSERT t_err INTO t_err INDEX 1.
    ENDIF.
    READ TABLE t_err WITH KEY sort = 'B'.
    IF sy-subrc = 0.
      g_init = sy-tabix.
      CLEAR : t_err.
      t_err-cost_center = 'Errors'.
      INSERT t_err INTO t_err INDEX g_init.
      INSERT INITIAL LINE INTO  t_err INDEX g_init.
    ENDIF.
*Download error and Success files
    PERFORM download_result.
  ENDIF.
  WRITE:     /01     text-001,
              29        ':',
              32     sy-repid,
*          92      text-001,
              92      sy-pagno,
             /01      text-003,
              10      'By',
              29      ':',
              32      sy-uname,
              82      sy-datum DD/MM/YY,
              92      sy-uzeit.

  ULINE.
  SKIP 1.
  IF v_flag1 EQ 'X'.
    CLEAR v_flag1.
    WRITE : /01 'Error in Downloading the File'.
    ULINE.
*    EXIT.
  else.
    IF  p_test IS INITIAL.
        WRITE: /01     text-005,
                25     p_rest.
*         /01     text-006,
*          20     batch_name.
      ELSEIF p_test  IS NOT INITIAL
         AND t_err[] IS NOT INITIAL.
        WRITE: /01     text-005,
                25     p_rest.
      ENDIF.
  ENDIF.
  ULINE.
  SKIP 2.
  WRITE: / 'TOTAL INPUT RECORDS READ',   40 tot-read.
  IF p_test IS INITIAL.
    WRITE: / 'TOTAL COST CENTER CREATED',    40 tot-ok.
    WRITE: / 'TOTAL COST CENTER IN ERROR',   40 tot-err.
  ELSE.
    tot-ok = tot-read - tot-err.
    WRITE: / 'TOTAL RECORDS SUCCESS',    40 tot-ok.
    WRITE: / 'TOTAL RECORDS ERROR',   40 tot-err.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  100_UPLOAD_FILE
*&---------------------------------------------------------------------*
*        Excel to Internal Table
*----------------------------------------------------------------------*

FORM 100_UPLOAD_FILE .
 CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
       EXPORTING
*   I_FIELD_SEPERATOR          = ','
        i_line_header              = p_head
         i_tab_raw_data             = it_raw
         i_filename                 = p_upl
       TABLES
         i_tab_converted_data       = t_indata
      EXCEPTIONS
        conversion_failed          = 1
        OTHERS                     = 2
               .

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    v_flag = 'X'.
  ENDIF.
ENDFORM.                    " 100_UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  INITIAL_LINE
*&---------------------------------------------------------------------*
*         Adding the intial line of the xls
*----------------------------------------------------------------------*

FORM INITIAL_LINE .
  w_initial-cost_center    = 'Cost Center'.
  w_initial-cont_area      = 'Controlling Area'.
  w_initial-valid_f        = 'Valid From'.
  w_initial-valid_t        = 'Valid To'.
  w_initial-name           = 'Name'.
  w_initial-description    = 'Description'.
  w_initial-per_resp       = 'Person Responsible'.
  w_initial-category       = 'Category'.
  w_initial-hier_ar        = 'Hierarchy Area'.
  w_initial-bus_ar         = 'Business Area'.
  w_initial-currency       = 'Currency'.
  w_initial-profit_center  = 'Profit Center'.

ENDFORM.                    " INITIAL_LINE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FIELDS
*&---------------------------------------------------------------------*
*       Validating The Fields Before Uploading
*----------------------------------------------------------------------*

FORM VALIDATE_FIELDS .
data : l_costcenter(10)   type n,
       l_profitcenter(10) type n.
loop at t_indata.
ADD 1 TO tot-read.
if t_indata-cost_center co '0123456789 '.
l_costcenter         = t_indata-cost_center.
t_indata-cost_center = l_costcenter.
endif.
if not t_indata-cost_center is initial.
SELECT SINGLE * FROM CSKS
*                      INTO l_cost_center
                     WHERE KOKRS = t_indata-cont_area
                       AND KOSTL = t_indata-cost_center
                       AND DATBI BETWEEN t_indata-valid_f
                                     AND t_indata-valid_t.
if sy-subrc eq 0.
ADD 1 TO tot-err.
          MOVE-CORRESPONDING t_indata TO t_err.
          t_err-sort = 'B'.
          CONCATENATE  'Cost Center'
                       t_indata-cost_center
                       'Already Exist in the SAP System'
                       INTO t_err-desc
                       SEPARATED BY space.
          APPEND t_err.
          DELETE t_indata.
          CONTINUE.
endif.
endif.
if not t_indata-category is initial.
SELECT SINGLE * from tka05
                where KOSAR = t_indata-category.
if sy-subrc ne 0.
ADD 1 TO tot-err.
          MOVE-CORRESPONDING t_indata TO t_err.
          t_err-sort = 'B'.
          CONCATENATE  'Cost Center'
                       t_indata-cost_center
                       'Already Exist in the SAP System'
                       INTO t_err-desc
                       SEPARATED BY space.
          APPEND t_err.
          DELETE t_indata.
          CONTINUE.
endif.
endif.
if t_indata-profit_center co '0123456789 '.
l_profitcenter         = t_indata-profit_center.
t_indata-profit_center = l_profitcenter.
endif.
if not t_indata-profit_center is initial.
SELECT SINGLE * from cepc
                where prctr = t_indata-profit_center.
if sy-subrc ne 0.
ADD 1 TO tot-err.
          MOVE-CORRESPONDING t_indata TO t_err.
          t_err-sort = 'B'.
          CONCATENATE  'Cost Center'
                       t_indata-cost_center
                       'Already Exist in the SAP System'
                       INTO t_err-desc
                       SEPARATED BY space.
          APPEND t_err.
          DELETE t_indata.
          CONTINUE.
endif.
endif.
endloop.
ENDFORM.                    " VALIDATE_FIELDS
*&---------------------------------------------------------------------*
*&      Form  CREATE_COST_CENTER
*&---------------------------------------------------------------------*
*      Peroform BDC operation
*----------------------------------------------------------------------*

FORM CREATE_COST_CENTER .
CLEAR: err_msg,
          batch_name,
          t_err.
  LOOP AT t_indata.
    PERFORM do_KS01_by_calltran
                       TABLES t_indata
                       USING  p_mode        "Show errors only
                              err_msg
                              batch_name.
*  perform bdc_transaction using 'J1ID'.
    WAIT UP TO 1 SECONDS.
    CALL TRANSACTION 'KS01' USING bdcdata MODE p_mode MESSAGES INTO messtab.
    IF sy-subrc NE 0.
      zzret_code = sy-subrc.
      IF v_open NE 'X'.
        PERFORM bdc_open_group USING p_sess.
        v_open = 'X'.
      ENDIF.
      PERFORM bdc_insert USING 'KS01'.
    ELSE.
      zzret_code = 0.
    ENDIF.
    READ TABLE messtab WITH  KEY msgtyp = 'E'.
    IF sy-subrc =  0.
      ADD 1 TO tot-err.
*Error File
      MOVE-CORRESPONDING t_indata TO t_err .
      t_err-sort = 'B'.
*Get the Error details
      PERFORM format_message.
      APPEND t_err.
    ELSE.
      ADD 1 TO tot-ok.
*Success Message
      MOVE-CORRESPONDING t_indata TO t_err.
      t_err-sort = 'A'.
*      PERFORM FORMAT_SUCC_MESSAGE.
      CONCATENATE T_INDATA-cost_center
                  'Has Been Successfully Created'
                  INTO
                  t_err-desc.
      APPEND t_err.
    ENDIF.
*If Any errors in Bdc Transfer to Session
    IF v_open = 'X'.
      PERFORM bdc_close_group USING p_sess.
    ENDIF.
*End of Mod002
    REFRESH bdcdata.
    CLEAR :
            v_open,
            g_mess,
            messtab,
            t_err
            .
    REFRESH : messtab.
  ENDLOOP.
ENDFORM.                    " CREATE_COST_CENTER
*&---------------------------------------------------------------------*
*&      Form  DO_KS01_BY_CALLTRAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_INDATA  text
*      -->P_P_MODE  text
*      -->P_ERR_MSG  text
*      -->P_BATCH_NAME  text
*----------------------------------------------------------------------*
FORM DO_KS01_BY_CALLTRAN  TABLES   P_T_INDATA STRUCTURE t_indata
                                     "Insert correct name for <...>
                          USING    P_P_MODE
                                   P_ERR_MSG
                                   P_BATCH_NAME.
  PERFORM process_screen_200.
  PERFORM process_screen_299.

ENDFORM.                    " DO_KS01_BY_CALLTRAN
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SCREEN_200
*&---------------------------------------------------------------------*
*       Initial Screen Entry
*----------------------------------------------------------------------*

FORM PROCESS_SCREEN_200 .
    PERFORM bdc_dynpro      USING 'SAPLKMA1' '0200'.
    PERFORM bdc_field       USING 'BDC_CURSOR'        'CSKSZ-DATAB_ANFO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'        '/00'.
    PERFORM bdc_field       USING 'CSKSZ-KOKRS'       t_indata-cont_area.
    PERFORM bdc_field       USING 'CSKSZ-KOSTL'       t_indata-cost_center.
    PERFORM bdc_field       USING 'CSKSZ-DATAB_ANFO'  t_indata-valid_f.
    PERFORM bdc_field       USING 'CSKSZ-DATBI_ANFO'  t_indata-valid_t.
ENDFORM.                    " PROCESS_SCREEN_200
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SCREEN_299
*&---------------------------------------------------------------------*
*       Second Screen Entry
*----------------------------------------------------------------------*

FORM PROCESS_SCREEN_299 .
    PERFORM bdc_dynpro      USING 'SAPLKMA1' '0299'.
    PERFORM bdc_field       USING 'BDC_OKCODE'        '=BU'.
    PERFORM bdc_field       USING 'BDC_CURSOR'        'CSKSZ-PRCTR'.
    PERFORM bdc_field       USING 'CSKSZ-KTEXT'       t_indata-name.
    PERFORM bdc_field       USING 'CSKSZ-LTEXT'       t_indata-description.
    PERFORM bdc_field       USING 'CSKSZ-VERAK_USER'  ''.
    PERFORM bdc_field       USING 'CSKSZ-VERAK'       t_indata-per_resp.
    PERFORM bdc_field       USING 'CSKSZ-KOSAR'       t_indata-category.
    PERFORM bdc_field       USING 'CSKSZ-KHINR'       t_indata-hier_ar.
    PERFORM bdc_field       USING 'CSKSZ-GSBER'       t_indata-bus_ar.
    PERFORM bdc_field       USING 'CSKSZ-WAERS'       t_indata-currency.
    PERFORM bdc_field       USING 'CSKSZ-PRCTR'       t_indata-profit_center.
ENDFORM.                    " PROCESS_SCREEN_299
*&---------------------------------------------------------------------*
*&      Form  FORMAT_MESSAGE
*&---------------------------------------------------------------------*
*       For Getting The Error Message
*----------------------------------------------------------------------*

FORM FORMAT_MESSAGE .
LOOP AT messtab WHERE msgtyp = 'E'.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = messtab-msgid
        lang      = 'EN'
        no        = messtab-msgnr
        v1        = messtab-msgv1
        v2        = messtab-msgv2
        v3        = messtab-msgv3
        v4        = messtab-msgv4
      IMPORTING
        msg       = g_mess
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*    CONCATENATE t_outdata-desc
*                g_mess
*                INTO t_outdata-desc SEPARATED BY space.
    CONCATENATE
                t_err-desc
                g_mess
                INTO t_err-desc SEPARATED BY space.
*    t_err-desc = g_mess.
  ENDLOOP.
ENDFORM.                    " FORMAT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  FORMAT_SUCC_MESSAGE
*&---------------------------------------------------------------------*
*      Success Message
*----------------------------------------------------------------------*

FORM FORMAT_SUCC_MESSAGE .
LOOP AT messtab WHERE msgtyp = 'S'.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = messtab-msgid
        lang      = 'EN'
        no        = messtab-msgnr
        v1        = messtab-msgv1
        v2        = messtab-msgv2
        v3        = messtab-msgv3
        v4        = messtab-msgv4
      IMPORTING
        msg       = g_mess
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*    CONCATENATE t_outdata-desc
*                g_mess
*                INTO t_outdata-desc SEPARATED BY space.
    CONCATENATE
                t_err-desc
                g_mess
                INTO t_err-desc SEPARATED BY space.
*    t_err-desc = g_mess.
  ENDLOOP.
ENDFORM.                    " FORMAT_SUCC_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_RESULT
*&---------------------------------------------------------------------*
*      Downloading the Excel File in the Given Resultant Path
*----------------------------------------------------------------------*

FORM DOWNLOAD_RESULT .
DATA : f_string TYPE string.
  f_string = p_rest.
*The Error table contains sort field which is not neccessary while downloading
* so transfer the records to another internal table for download
  LOOP AT t_err.
    MOVE-CORRESPONDING t_err TO t_fdown.
    APPEND t_fdown.
  ENDLOOP.
  MOVE-CORRESPONDING w_initial TO w_init1.


  INSERT w_init1 INTO t_fdown INDEX 1.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*    BIN_FILESIZE                    =
      filename                        = f_string
     filetype                        = 'ASC'
*    APPEND                          = ' '
     write_field_separator           = 'X'
    TABLES
      data_tab                        = t_fdown[]
*    FIELDNAMES                      =
   EXCEPTIONS
     file_write_error                = 1
     no_batch                        = 2
     gui_refuse_filetransfer         = 3
     invalid_type                    = 4
     no_authority                    = 5
     unknown_error                   = 6
     header_not_allowed              = 7
     separator_not_allowed           = 8
     filesize_not_allowed            = 9
     header_too_long                 = 10
     dp_error_create                 = 11
     dp_error_send                   = 12
     dp_error_write                  = 13
     unknown_dp_error                = 14
     access_denied                   = 15
     dp_out_of_memory                = 16
     disk_full                       = 17
     dp_timeout                      = 18
     file_not_found                  = 19
     dataprovider_exception          = 20
     control_flush_error             = 21
     OTHERS                          = 22
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    v_flag1 = 'X'.
  ENDIF.
ENDFORM.                    " DOWNLOAD_RESULT
