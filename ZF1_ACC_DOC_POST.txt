*----------------------------------------------------------------------*
* Report       : ZFI_ACC_DOC_POST                                      *
* Version      : 1.0                                                   *
* Application  : FI                                                    *
* Date created : 04 MAR 2009                                           *
* Description  : BAPI PROGRAM for Uploading/Posting GL Document        *
*                                                                      *
*----------------------------------------------------------------------*
* Modification History                                                 *
*----------------------------------------------------------------------*
* Request no. |Date       |Programmer      | Description               *
*----------------------------------------------------------------------*
*             |04.03.2009 |Naveen K George |                           *
*----------------------------------------------------------------------*
* Upload File Format   = xls                                           *
* Download File Format = xls                                           *
*----------------------------------------------------------------------*

REPORT  zfi_acc_doc_post.

*--------------------------------------------------------------------*
*  Selection Screen
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl02 WITH FRAME TITLE custmas.
*PARAMETERS : p_r1 RADIOBUTTON GROUP rad USER-COMMAND clk DEFAULT 'X'.
*PARAMETERS : p_r2 RADIOBUTTON GROUP rad.
PARAMETERS:  p_ddate LIKE sy-datum,
             p_pdate LIKE sy-datum,
             p_type    LIKE bkpf-blart DEFAULT 'SA',
             p_ccode   LIKE bkpf-bukrs DEFAULT 'NPIN',
             p_period(2) DEFAULT '11',
             p_curr    LIKE bkpf-waers DEFAULT 'INR',
             p_ref(16),
             p_txt(25).
PARAMETERS:  p_file    LIKE rlgrap-filename DEFAULT 'C:\' OBLIGATORY,
             p_err     LIKE rlgrap-filename DEFAULT 'C:\TEMP\ERROR.XLS' OBLIGATORY,
             p_head    AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK bl02.

*--------------------------------------------------------------------*
*  Data Declaration
*--------------------------------------------------------------------*
*--Type-pools
TYPE-POOLS : truxs.
DATA: it_raw TYPE truxs_t_text_data.

*-- Bapi Declaration
DATA:
  gd_documentheader    LIKE bapiache09,
  gd_customercpd       LIKE bapiacpa09,
  gd_fica_hd           LIKE bapiaccahd,
  it_accountreceivable LIKE TABLE OF bapiacar09 WITH HEADER LINE,
  it_accountgl         LIKE TABLE OF bapiacgl09 WITH HEADER LINE,
  it_accounttax        LIKE TABLE OF bapiactx09 WITH HEADER LINE,
  it_criteria          LIKE TABLE OF bapiackec9 WITH HEADER LINE,
  it_valuefield        LIKE TABLE OF bapiackev9 WITH HEADER LINE,
  it_currencyamount    LIKE TABLE OF bapiaccr09 WITH HEADER LINE,
  it_return            LIKE TABLE OF bapiret2   WITH HEADER LINE,
  it_receivers         LIKE TABLE OF bdi_logsys WITH HEADER LINE,
  it_fica_it           LIKE TABLE OF bapiaccait WITH HEADER LINE,
  it_accountpayable    LIKE TABLE OF bapiacap09 WITH HEADER LINE,
  it_paymentcard       LIKE TABLE OF bapiacpc09 WITH HEADER LINE,
  it_ext               LIKE TABLE OF bapiacextc WITH HEADER LINE,
  it_re                LIKE TABLE OF bapiacre09 WITH HEADER LINE,
  it_ext2              LIKE TABLE OF bapiparex  WITH HEADER LINE.

*DATA: BEGIN OF t_data OCCURS 0,
*       field(200),
*      END OF t_data.
*-- Internal Table to hold data of Upload file
DATA : BEGIN OF t_data1 OCCURS 0,
        post_key(2),            "Posting Key
        gl_account(10),         "Account
        amount(13),             "Amount
        profit_center(10),      "Profit center
        cost_center(10),        "Cost Center
        tax(2),                 "Tax Code
        h_l(1),                 "Header/Line Item deciding flag
        doc_no(1),              "Doc No for Header and linitem matching
        ref_doc(16),            "Ref Doc No
        doc_txt(40),            "Document txt
        txt(5000),              "Comments/Error/Success msg
       END OF t_data1.
*-- Table to hold error records
DATA : t_err LIKE t_data1 OCCURS 0 WITH HEADER LINE.

*-- Header table to hold BAPI header details
DATA: BEGIN OF t_head OCCURS 0,
       doc_no(5) TYPE c,        "Document No to find header and its corr line item
       ind(1),                  "Flag to indicate header or line item
       doc_date(10),            "Document Date
*       ACCOUNT(10),
       doc_type(2),             "Document Type
       comp_code(4),            "Company Code
       post_date(10),           "Posting Date
       period(3),               "Period
       curr(3),                 "Currency
       ref(16),                 "Reference Doc No
       head_txt(25),            "Document header txt
      END OF t_head.
*-- Line Item table to hold BAPI lineitem details
DATA: BEGIN OF t_item OCCURS 0,
       doc_no(5) TYPE c,        "Document No to find header and its corr line item
       ind(1),                  "Flag to indicate header or line item
       post_key(2),             "Posting Key
       account(10),             "Account
       amount(13),              "Amount
       bus_area(4),             "Bus area
       tax_code(2),             "Tax Code
       calc_tax(1),             "Calcaulate tax
       cost_center(10),         "Cost Center
       profit_center(10),       "Profit center
       text(50),
       pay_method,
       pay_terms(4),
       bline_date(10),
      END OF t_item.

DATA: w_item TYPE i.
DATA: w_net_amt TYPE wrbtr,
      w_tax_amt TYPE fwste,
      w_waers LIKE bkpf-waers.

DATA g_flag.

CLASS cl_abap_char_utilities DEFINITION LOAD.
CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

*--------------------------------------------------------------------*
*  At Selection Screen on Value request for File
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM 100_get_file USING p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_err.
  PERFORM 100_get_file USING p_err.

*--------------------------------------------------------------------*
*  Start Of Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
*--Error checks if Mandatory Parameter values not passed
  IF p_file IS INITIAL.
    MESSAGE 'Please Specify Upload file' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_err IS INITIAL.
    MESSAGE 'Please Specify Download file' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_ddate IS INITIAL.
    MESSAGE 'Please Specify Document date' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_pdate IS INITIAL.
    MESSAGE 'Please Specify Posting date' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_type IS INITIAL.
    MESSAGE 'Please Specify Documetn type' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_ccode IS INITIAL.
    MESSAGE 'Please Specify Company code' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_period IS INITIAL.
    MESSAGE 'Please Specify Period' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_curr IS INITIAL.
    MESSAGE 'Please Specify Currency' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-- perform to Upload File data to Internal table
  PERFORM 200_upload_file.
*--Perform to Split header and line items
  PERFORM 300_fill_hl_table.
*--Perform to Post the GL Accounting Document via BAPI
  PERFORM 400_post_acc_doc.
  IF t_err[] IS NOT INITIAL.
*--Perform to Download Error records if any
    PERFORM 500_error_download.
  ENDIF.
  IF t_err[] IS NOT INITIAL.
    WRITE : / 'Error File Downloaded to', p_err.
  ELSE.
    WRITE : / 'Document Posted Successfully'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  100_GET_FILE
*&---------------------------------------------------------------------*
*       Get Filename
*----------------------------------------------------------------------*
FORM 100_get_file  USING    p_filename.
  CALL FUNCTION 'F4_FILENAME'
* EXPORTING
*   PROGRAM_NAME        = SYST-CPROG
*   DYNPRO_NUMBER       = SYST-DYNNR
*   FIELD_NAME          = ' '
   IMPORTING
     file_name           = p_filename.
ENDFORM.                    " 100_GET_FILE

*&---------------------------------------------------------------------*
*&      Form  200_UPLOAD_FILE
*&---------------------------------------------------------------------*
*       Upload Data from file to Internal Table
*----------------------------------------------------------------------*
FORM 200_upload_file .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
     EXPORTING
*   I_FIELD_SEPERATOR          = ','
      i_line_header               = p_head
       i_tab_raw_data             = it_raw
       i_filename                 = p_file
     TABLES
       i_tab_converted_data       = t_data1
    EXCEPTIONS
      conversion_failed          = 1
      OTHERS                     = 2
             .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " 200_UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  FILL_CURRENCYAMOUNT1
*&---------------------------------------------------------------------*
*       Pass Currency amount
*----------------------------------------------------------------------*
FORM fill_currencyamount1 .
  CLEAR it_currencyamount.
  it_currencyamount-itemno_acc   = w_item.
  it_currencyamount-curr_type    = '00'.
  it_currencyamount-currency     = t_head-curr.
* IT_CURRENCYAMOUNT-CURRENCY_ISO =
  it_currencyamount-amt_doccur   = w_tax_amt.
* IT_CURRENCYAMOUNT-EXCH_RATE    = .
  it_currencyamount-amt_base     = t_item-amount.
* IT_CURRENCYAMOUNT-DISC_BASE    =
* it_currencyamount-exch_rate_v  =
* it_currencyamount-disc_amt     =
  APPEND it_currencyamount.
ENDFORM.                    " FILL_CURRENCYAMOUN


*&---------------------------------------------------------------------*
*&      Form  300_FILL_HL_TABLE
*&---------------------------------------------------------------------*
*       Split and Fill Header Line Item table
*----------------------------------------------------------------------*
FORM 300_fill_hl_table .
  DATA l_cnt TYPE i.
*  IF p_r1 = 'X'.
  t_head-ind = 'H'.
  t_head-doc_no    = '1'."t_data1-doc_no.
  t_head-doc_date  = p_ddate."'20081130'. "sy-datum.
  t_head-doc_type  = p_type.
  t_head-comp_code = p_ccode.
  t_head-post_date = p_pdate."'20081130'."sy-datum.
  t_head-period    = p_period.
  t_head-curr      = p_curr.
  t_head-ref       = p_ref.
  t_head-head_txt  = p_txt.
  APPEND t_head.
  LOOP AT t_data1.
    t_item-ind            = 'L'.
    t_item-doc_no         = '1'."t_data1-doc_no.
    t_item-post_key       = t_data1-post_key.
    t_item-account        = t_data1-gl_account.
    IF p_type = 'SA'.
      IF t_data1-post_key = '50'.
        t_item-amount         = '-1' * t_data1-amount.
      ELSEIF t_data1-post_key = '40'.
        t_item-amount         = t_data1-amount.
      ENDIF.
    ENDIF.
    IF t_data1-cost_center IS NOT INITIAL.
      t_item-cost_center    = t_data1-cost_center.
    ENDIF.
    t_item-bus_area       = ' '.
    t_item-tax_code       = t_data1-tax.
    t_item-calc_tax       = ' '.
*    t_item-cost_center    = ' '.
    IF t_data1-profit_center IS NOT INITIAL.
      t_item-profit_center  = t_data1-profit_center.
    ENDIF.
    APPEND t_item.
  ENDLOOP.

*  ELSEIF p_r2 = 'X'.
*    LOOP AT t_data1.
*      l_cnt = l_cnt + 1.
*      t_head-ind = 'H'.
*      t_head-doc_no    = l_cnt."t_data1-doc_no.
*      t_head-doc_date  = p_ddate."'20081130'. "sy-datum.
*      t_head-doc_type  = p_type.
*      t_head-comp_code = p_ccode.
*      t_head-post_date = p_pdate."'20081130'."sy-datum.
*      t_head-period    = p_period.
*      t_head-curr      = p_curr.
*      t_head-ref       = p_ref.
*      t_head-head_txt  = p_txt.
*      APPEND t_head.
*      t_item-ind            = 'L'.
*      t_item-doc_no         = l_cnt."t_data1-doc_no.
*      t_item-post_key       = t_data1-post_key.
*      t_item-account        = t_data1-gl_account.
*      IF p_type = 'DR'.
*        IF t_data1-post_key = '11' OR t_data1-post_key = '50'.
*          t_item-amount         = '-1' * t_data1-amount.
*        ELSEIF t_data1-post_key = '40' OR t_data1-post_key = '1'.
*          t_item-amount         = t_data1-amount.
*        ENDIF.
*      ENDIF.
*      IF t_data1-cost_center IS NOT INITIAL.
*        t_item-cost_center    = t_data1-cost_center.
*      ENDIF.
*      t_item-bus_area       = ' '.
*      t_item-tax_code       = t_data1-tax.
*      t_item-calc_tax       = ' '.
*      t_item-cost_center    = ' '.
*      IF t_data1-profit_center IS NOT INITIAL.
*        t_item-profit_center  = t_data1-profit_center.
*      ENDIF.
*      APPEND t_item.
*
*      t_item-ind                = 'L'.
*      t_item-doc_no             = l_cnt."t_data1-doc_no.
*      IF t_data1-post_key = '11'.
*        t_item-post_key         = '40'.
*      ELSEIF t_data1-post_key = '1'.
*        t_item-post_key         = '50'.
*      ELSEIF t_data1-post_key = '50'.
*        t_item-post_key         = '40'.
*      ENDIF.
*      t_item-account            = '9000160'.
*      IF p_type = 'DR'.
*        IF t_data1-post_key = '11' OR t_data1-post_key = '50'.
*          t_item-amount         = '-1' * t_data1-amount.
*        ELSEIF t_data1-post_key = '40' OR t_data1-post_key = '1'.
*          t_item-amount         = t_data1-amount.
*        ENDIF.
*      ENDIF.
*      IF t_data1-cost_center IS NOT INITIAL.
*        t_item-cost_center    = t_data1-cost_center.
*      ENDIF.
*      t_item-bus_area         = ' '.
*      t_item-tax_code         = t_data1-tax.
*      t_item-calc_tax         = ' '.
*      t_item-cost_center      = ' '.
*      IF t_data1-profit_center IS NOT INITIAL.
*        t_item-profit_center  = t_data1-profit_center.
*      ENDIF.
*      APPEND t_item.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    " 300_FILL_HL_TABLE

*&---------------------------------------------------------------------*
*&      Form  400_POST_ACC_DOC
*&---------------------------------------------------------------------*
*       Post Account docuent and display errors if any
*----------------------------------------------------------------------*
FORM 400_post_acc_doc .
  data l_cnt type i.
*  Loop thru the Header internal table and pass it to BAPI
*    to post document
  CLEAR gd_documentheader.
  LOOP AT t_head.
    CLEAR g_flag.
*--Perform to fill Internal table required for Bapi
    PERFORM fill_internal_tables.

    DATA: l_type LIKE gd_documentheader-obj_type,
          l_key  LIKE gd_documentheader-obj_key,
          l_sys  LIKE gd_documentheader-obj_sys.
*--BAPI FM to post the accounting document
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = gd_documentheader
        customercpd       = gd_customercpd
        contractheader    = gd_fica_hd
      IMPORTING
        obj_type          = l_type
        obj_key           = l_key
        obj_sys           = l_sys
      TABLES
        accountgl         = it_accountgl
        accountreceivable = it_accountreceivable
        accountpayable    = it_accountpayable
        accounttax        = it_accounttax
        currencyamount    = it_currencyamount
        criteria          = it_criteria
        valuefield        = it_valuefield
        extension1        = it_ext
        return            = it_return
        paymentcard       = it_paymentcard
        contractitem      = it_fica_it
        extension2        = it_ext2
        realestate        = it_re.
*-- Display result of Post
    AT FIRST.
      WRITE: / 'Result of post:'.
      SKIP 1.                                               "#EC NOTEXT
    ENDAT.
*    PERFORM show_messages.
*-- Read return table of BAPI and filter errors if any
    READ TABLE it_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      g_flag = 'X'.
      t_err-h_l           = 'H'.
      t_err-doc_no        = t_head-doc_no.
      t_err-post_key      = t_head-doc_type.
      t_err-gl_account    = t_head-comp_code.
      t_err-amount        = t_head-period.
      t_err-profit_center = t_head-curr.
      t_err-ref_doc       = t_head-ref.
      t_err-doc_txt       = t_head-head_txt.
      APPEND t_err.
      LOOP AT t_item WHERE doc_no = t_head-doc_no.
        l_cnt = l_cnt + 1.
        t_err-h_l           = 'L'.
        t_err-doc_no        = t_item-doc_no.
        t_err-post_key      = t_item-post_key.
        t_err-gl_account    = t_item-account.
        t_err-amount        = t_item-amount.
        t_err-tax           = t_item-tax_code.
        t_err-profit_center = t_item-profit_center.
        t_err-cost_center   = t_item-cost_center.
        CLEAR t_err-txt.
        LOOP AT it_return WHERE type NE 'S' and ROW = l_cnt. "AND message_v1 = t_item-account.
          CONCATENATE t_err-txt
                      '/'
                      it_return-message
                 INTO t_err-txt
         SEPARATED BY space.
        ENDLOOP.
        APPEND t_err.
      ENDLOOP.
      CLEAR l_cnt.
    ENDIF.
    IF g_flag IS INITIAL.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " 400_POST_ACC_DOC

*---------------------------------------------------------------------*
*      Form  fill_internal_tables
*---------------------------------------------------------------------*
FORM fill_internal_tables.

  DATA: t_tax LIKE rtax1u15 OCCURS 0 WITH HEADER LINE.
  DATA: l_amount TYPE wrbtr.
*--Perform to fill header details
  PERFORM fill_header.

  REFRESH: it_accountpayable,it_accountgl,it_currencyamount.
  CLEAR w_item.
*--Loop thru internal table and fill the req internal tables for BAPI
  LOOP AT t_item WHERE doc_no EQ t_head-doc_no.
    w_item = w_item + 1.
    IF t_item-post_key EQ '34' OR
       t_item-post_key EQ '31'.
      t_item-amount = t_item-amount * '-1'.
      w_net_amt = t_item-amount.
*--Perform to fill AP
      PERFORM fill_accountap.
*--Perform to fill Currency amt
      PERFORM fill_currencyamount.
    ELSE.
*--Perform to fill GL Lineitem details
      PERFORM fill_accountgl.
      IF t_item-tax_code EQ 'P1' OR
       t_item-tax_code EQ 'S1'.
        l_amount = t_item-amount.
        w_waers = t_head-curr.
*--FM to calcaulate Tax from Gross amt
        CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
          EXPORTING
            i_bukrs                 = t_head-comp_code
            i_mwskz                 = t_item-tax_code
*           I_TXJCD                 = ' '
            i_waers                 = w_waers
            i_wrbtr                 = l_amount
           IMPORTING
*   E_FWNAV                 =
*   E_FWNVV                 =
            e_fwste                 = w_tax_amt
            e_fwast                 = w_tax_amt
          TABLES
            t_mwdat                 = t_tax
         EXCEPTIONS
           bukrs_not_found         = 1
           country_not_found       = 2
           mwskz_not_defined       = 3
           mwskz_not_valid         = 4
           ktosl_not_found         = 5
           kalsm_not_found         = 6
           parameter_error         = 7
           knumh_not_found         = 8
           kschl_not_found         = 9
           unknown_error           = 10
           account_not_found       = 11
           txjcd_not_valid         = 12
           OTHERS                  = 13
                  .
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        w_net_amt = l_amount - w_tax_amt.
*--Perform to fill Currency amt
        PERFORM fill_currencyamount.
        w_item = w_item + 1.
*--Perform to fill A/C Tax
        PERFORM fill_accounttax.
*--Perform to fill Currency amt
        PERFORM fill_currencyamount1.
      ELSE.
        w_net_amt = t_item-amount.
*--Perform to fill Currency amt
        PERFORM fill_currencyamount.
      ENDIF.
    ENDIF.
*    PERFORM FILL_CURRENCYAMOUNT.
  ENDLOOP.
*  PERFORM FILL_ACCOUNTAR.
*  PERFORM FILL_ACCOUNTTAX.

*  PERFORM FILL_CRITERIA.
*  PERFORM FILL_VALUEFIELD.
*  PERFORM FILL_RE.
*  PERFORM FILL_CPD.
*  PERFORM FILL_CONTRACTITEM.
*  PERFORM FILL_CONTRACTHEADER.
*  PERFORM FILL_PAYMENTCARD.
*  PERFORM FILL_EXTENSION.

ENDFORM.                               " fill_internal_tables

*---------------------------------------------------------------------*
*      Form  Show_messages
*---------------------------------------------------------------------*
FORM show_messages.

  IF it_return[] IS INITIAL.
    WRITE: / 'no messages'.
  ELSE.
    SKIP 1.
    LOOP AT it_return.
      WRITE: /    it_return-type,
             (2)  it_return-id,
                  it_return-number,
             (80) it_return-message,
*                 IT_RETURN-LOG_NO
*                 IT_RETURN-LOG_MSG_NO
*                 IT_RETURN-MESSAGE_V1
*                 IT_RETURN-MESSAGE_V2
*                 IT_RETURN-MESSAGE_V3
*                 IT_RETURN-MESSAGE_V4
             (20) it_return-parameter,
             (3)  it_return-row,
                  it_return-field.
*                 IT_RETURN-SYSTEM
    ENDLOOP.
  ENDIF.
  ULINE.

ENDFORM.                               " Show_messages


*---------------------------------------------------------------------*
*       FORM fill_accountgl                                           *
*---------------------------------------------------------------------*
FORM fill_accountgl.
  DATA: w_account(10) TYPE n,
*        w_cost(10) TYPE n,
        w_cost(10),
*        w_profit(10) TYPE n.
        w_profit(10).
  DATA: l_cust LIKE knb1-kunnr,
        l_account LIKE knb1-kunnr.

  CLEAR it_accountgl.
*  IF p_r1 = 'X'.
*    w_account = t_item-account.
*  ELSEIF p_r2 = 'X'.
*    l_cust = t_item-account.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = l_cust
*      IMPORTING
*        output = l_cust.
*    it_accountgl-itemno_acc     = w_item.
*    SELECT SINGLE akont FROM knb1
*                  INTO l_account
*                  WHERE kunnr = l_cust.
*    IF sy-subrc = 0.
*      w_account = l_account.
*    ELSE.
*      w_account = t_item-account.
*    ENDIF.
*  ENDIF.
  it_accountgl-itemno_acc       = w_item.
  w_account = t_item-account.
  it_accountgl-gl_account     = w_account.
* IT_ACCOUNTGL-STAT_CON       =
* IT_ACCOUNTGL-REF_KEY_1      =
* IT_ACCOUNTGL-REF_KEY_2      =
* IT_ACCOUNTGL-REF_KEY_3      =
  it_accountgl-tax_code       = t_item-tax_code.
* IT_ACCOUNTGL-ACCT_KEY       =
* IT_ACCOUNTGL-TAXJURCODE     =
* IT_ACCOUNTGL-CSHDIS_IND     =
* IT_ACCOUNTGL-ALLOC_NMBR     =
  it_accountgl-item_text      = t_item-text.
* IT_ACCOUNTGL-BUS_AREA       =
  IF NOT t_item-cost_center IS INITIAL.
    w_cost = t_item-cost_center.
    it_accountgl-costcenter     = w_cost.
  ELSE.
    it_accountgl-costcenter     = t_item-cost_center.
  ENDIF.
* IT_ACCOUNTGL-ORDERID        =
* it_accountgl-ext_object_id  =
* it_accountgl-bus_scenario   =
* IT_ACCOUNTGL-MATERIAL       =
* IT_ACCOUNTGL-QUANTITY       =
* IT_ACCOUNTGL-BASE_UOM       =
* IT_ACCOUNTGL-BASE_UOM_ISO   =
* IT_ACCOUNTGL-PLANT          =
  IF NOT t_item-profit_center IS INITIAL.
    w_profit = t_item-profit_center.
    it_accountgl-profit_ctr     = w_profit.
  ELSE.
    it_accountgl-profit_ctr     = t_item-profit_center.
  ENDIF.
* IT_ACCOUNTGL-PART_PRCTR     =
* IT_ACCOUNTGL-WBS_ELEMENT    =
* IT_ACCOUNTGL-NETWORK        =
* IT_ACCOUNTGL-CMMT_ITEM      =
* IT_ACCOUNTGL-FUNDS_CTR      =
* IT_ACCOUNTGL-FUND           =
* IT_ACCOUNTGL-SALES_ORD      =
* IT_ACCOUNTGL-S_ORD_ITEM     =
* IT_ACCOUNTGL-P_EL_PRCTR     =
* IT_ACCOUNTGL-BILL_TYPE      =
* IT_ACCOUNTGL-DISTR_CHAN     =
* IT_ACCOUNTGL-SOLD_TO        =
* IT_ACCOUNTGL-DIVISION       =
* IT_ACCOUNTGL-SALESORG       =
* IT_ACCOUNTGL-SALES_OFF      =
* IT_ACCOUNTGL-SALES_GRP      =
* IT_ACCOUNTGL-INV_QTY        =
* IT_ACCOUNTGL-SALES_UNIT     =
* IT_ACCOUNTGL-SALES_UNIT_ISO =
* IT_ACCOUNTGL-INV_QTY_SU     =
* IT_ACCOUNTGL-NET_WEIGHT     =
* IT_ACCOUNTGL-GROSS_WT       =
* IT_ACCOUNTGL-UNIT_OF_WT     =
* IT_ACCOUNTGL-UNIT_OF_WT_ISO =
* IT_ACCOUNTGL-VOLUME         =
* IT_ACCOUNTGL-VOLUMEUNIT     =
* IT_ACCOUNTGL-VOLUMEUNIT_ISO =
* it_accountgl-fm_area        =
* it_accountgl-log_proc       =
* it_accountgl-ac_doc_no      =
* it_accountgl-acct_type      =
* it_accountgl-doc_type       =
* it_accountgl-comp_code      =
* it_accountgl-func_area      =
* it_accountgl-plant          =
* it_accountgl-fis_period     =
* it_accountgl-fisc_year      =
* it_accountgl-pstng_date     =
* it_accountgl-value_date     =
* it_accountgl-customer       =
* it_accountgl-vendor_no      =
* it_accountgl-costobject     =
* it_accountgl-acttype        =
* it_accountgl-order_itno     =
* it_accountgl-routing_no     =
* it_accountgl-activity       =
* it_accountgl-cond_type      =
* it_accountgl-cond_count     =
* it_accountgl-cond_st_no     =
* it_accountgl-co_busproc     =
* it_accountgl-asset_no       =
* it_accountgl-sub_number     =
* it_accountgl-de_cre_ind     =
* it_accountgl-p_el_prctr     =
* it_accountgl-xmfrw          =
* it_accountgl-po_pr_qnt      =
* it_accountgl-po_pr_uom      =
* it_accountgl-po_pr_uom_iso  =
* it_accountgl-entry_qnt      =
* it_accountgl-entry_uom      =
* it_accountgl-entry_uom_iso  =
* it_accountgl-item_cat       =
* it_accountgl-matl_type      =
* it_accountgl-mvt_ind        =
* it_accountgl-reval_ind      =
* it_accountgl-orig_group     =
* it_accountgl-orig_mat       =
* it_accountgl-serial_no      =
* it_accountgl-part_acct      =
* it_accountgl-tr_part_ba     =
* it_accountgl-trade_id       =
* it_accountgl-val_area       =
* it_accountgl-val_type       =
* it_accountgl-asval_date     =
* it_accountgl-po_number      =
* it_accountgl-po_item        =
  APPEND it_accountgl.

ENDFORM.                    "fill_accountgl

*---------------------------------------------------------------------*
*       FORM fill_header                                              *
*---------------------------------------------------------------------*
FORM fill_header.

  DATA: w_date1 LIKE sy-datum,
        w_date2 LIKE sy-datum.

  MOVE: t_head-doc_date TO w_date1,
        t_head-post_date TO w_date2.

*  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
*    IMPORTING
*      OWN_LOGICAL_SYSTEM = GD_DOCUMENTHEADER-OBJ_SYS.

* OBJ_TYPE has to be replaced by customers object key (Y* or Z*)
  gd_documentheader-obj_type   = 'IDOC'.
  gd_documentheader-obj_key    = 'TEST' ."'000000010001002009'.
  gd_documentheader-username   = sy-uname.
  gd_documentheader-header_txt = t_head-head_txt.
* gd_documentheader-obj_key_r  =
* GD_DOCUMENTHEADER-reason_rev =
  gd_documentheader-comp_code  = t_head-comp_code.
* GD_DOCUMENTHEADER-AC_DOC_NO  =
  gd_documentheader-doc_type = t_head-doc_type.
*  GD_DOCUMENTHEADER-FISC_YEAR  = '2009'.
  gd_documentheader-doc_date   = t_head-doc_date.
                                                            "W_DATE1.
  gd_documentheader-pstng_date = t_head-post_date.          "W_DATE2.
* GD_DOCUMENTHEADER-TRANS_DATE =
* GD_DOCUMENTHEADER-VALUE_DATE =
* GD_DOCUMENTHEADER-FIS_PERIOD =
  gd_documentheader-doc_type   = t_head-doc_type.
  gd_documentheader-ref_doc_no = t_head-ref.
* GD_DOCUMENTHEADER-COMPO_ACC  =
  gd_documentheader-bus_act    = 'RFBU'.

ENDFORM.                    "fill_header

*---------------------------------------------------------------------*
*       FORM fill_contractheader                                     *
*---------------------------------------------------------------------*
FORM fill_contractheader.

*  gd_fica_hd-doc_no           =
*  gd_fica_hd-doc_type_ca      =
*  gd_fica_hd-res_key          =
*  gd_fica_hd-fikey            =
*  gd_fica_hd-payment_form_ref =

ENDFORM.                    "fill_contractheader

*---------------------------------------------------------------------*
*       FORM fill_cpd                                                 *
*---------------------------------------------------------------------*
FORM fill_cpd.

*  gd_customercpd-name
*  gd_customercpd-name_2
*  gd_customercpd-name_3
*  gd_customercpd-name_4
*  gd_customercpd-postl_code
*  gd_customercpd-city
*  gd_customercpd-country
*  gd_customercpd-country_iso
*  gd_customercpd-street
*  gd_customercpd-po_box
*  gd_customercpd-pobx_pcd
*  gd_customercpd-pobk_curac
*  gd_customercpd-bank_acct
*  gd_customercpd-bank_no
*  gd_customercpd-bank_ctry
*  gd_customercpd-bank_ctry_iso
*  gd_customercpd-tax_no_1
*  gd_customercpd-tax_no_2
*  gd_customercpd-tax
*  gd_customercpd-equal_tax
*  gd_customercpd-region
*  gd_customercpd-ctrl_key
*  gd_customercpd-instr_key
*  gd_customercpd-dme_ind
*  gd_customercpd-langu_iso

ENDFORM.                    "fill_cpd

*---------------------------------------------------------------------*
*       FORM fill_ar                                                  *
*---------------------------------------------------------------------*
FORM fill_accountar.

* CLEAR it_accountreceivable.
* it_accountreceivable-itemno_acc =
* it_accountreceivable-customer   =
* IT_ACCOUNTRECEIVABLE-REF_KEY_1  =
* IT_ACCOUNTRECEIVABLE-REF_KEY_2  =
* IT_ACCOUNTRECEIVABLE-REF_KEY_3  =
* IT_ACCOUNTRECEIVABLE-PMNTTRMS   =
* IT_ACCOUNTRECEIVABLE-BLINE_DATE =
* IT_ACCOUNTRECEIVABLE-DSCT_DAYS1 =
* IT_ACCOUNTRECEIVABLE-DSCT_DAYS2 =
* IT_ACCOUNTRECEIVABLE-NETTERMS   =
* IT_ACCOUNTRECEIVABLE-DSCT_PCT1  =
* IT_ACCOUNTRECEIVABLE-DSCT_PCT2  =
* IT_ACCOUNTRECEIVABLE-PYMT_METH  =
* IT_ACCOUNTRECEIVABLE-DUNN_KEY   =
* IT_ACCOUNTRECEIVABLE-DUNN_BLOCK =
* IT_ACCOUNTRECEIVABLE-PMNT_BLOCK =
* IT_ACCOUNTRECEIVABLE-VAT_REG_NO =
* IT_ACCOUNTRECEIVABLE-ALLOC_NMBR =
* it_accountreceivable-item_text  =
* IT_ACCOUNTRECEIVABLE-PARTNER_BK =
* IT_ACCOUNTRECEIVABLE-GL_ACCOUNT =
* it_accountreceivable-comp_code
* it_accountreceivable-bus_area
* it_accountreceivable-pmtmthsupl
* it_accountreceivable-paymt_ref
* it_accountreceivable-scbank_ind
* it_accountreceivable-businessplace
* it_accountreceivable-sectioncode
* it_accountreceivable-branch
* it_accountreceivable-pymt_cur
* it_accountreceivable-pymt_cur_iso
* it_accountreceivable-pymt_amt
* it_accountreceivable-c_ctr_area
* it_accountreceivable-bank_id
* it_accountreceivable-supcountry
* it_accountreceivable-supcountry_iso
* it_accountreceivable-tax_code
* it_accountreceivable-taxjurcode
* it_accountreceivable-tax_date
* it_accountreceivable-sp_gl_ind
* it_accountreceivable-partner_guid = '1465464654'.
* APPEND it_accountreceivable.

ENDFORM.                    "fill_accountar

*---------------------------------------------------------------------*
*       FORM fill_ap                                                  *
*---------------------------------------------------------------------*
FORM fill_accountap.

*  CLEAR it_accountpayable.
*  it_accountpayable-itemno_acc = w_item.
*  it_accountpayable-vendor_no  = t_item-account.
** it_accountpayable-gl_account
** it_accountpayable-ref_key_1
** it_accountpayable-ref_key_2
** it_accountpayable-ref_key_3
** it_accountpayable-comp_code
** it_accountpayable-bus_area
*  it_accountpayable-pmnttrms = t_item-pay_terms.
** it_accountpayable-bline_date
** it_accountpayable-dsct_days1
** it_accountpayable-dsct_days2
** it_accountpayable-netterms
** it_accountpayable-dsct_pct1
** it_accountpayable-dsct_pct2
*  it_accountpayable-pymt_meth = t_item-pay_method.
** it_accountpayable-pmtmthsupl
** it_accountpayable-pmnt_block
** it_accountpayable-scbank_ind
** it_accountpayable-supcountry
** it_accountpayable-supcountry_iso
** it_accountpayable-bllsrv_ind
** it_accountpayable-alloc_nmbr
*  it_accountpayable-item_text  = t_item-text.
** it_accountpayable-po_sub_no
** it_accountpayable-po_checkdg
** it_accountpayable-po_ref_no
** it_accountpayable-w_tax_code
** it_accountpayable-businessplace
** it_accountpayable-sectioncode
** it_accountpayable-instr1
** it_accountpayable-instr2
** it_accountpayable-instr3
** it_accountpayable-instr4
** it_accountpayable-branch
** it_accountpayable-pymt_cur
** it_accountpayable-pymt_amt
** it_accountpayable-pymt_cur_iso
** it_accountpayable-sp_gl_ind
*
*  APPEND it_accountpayable.

ENDFORM.                    "fill_accountap

*---------------------------------------------------------------------*
*       FORM fill_tax                                                 *
*---------------------------------------------------------------------*
FORM fill_accounttax.

*  CLEAR it_accounttax.
*  it_accounttax-itemno_acc = w_item.
*  IF t_item-tax_code EQ 'P1'.
*    it_accounttax-gl_account = '0000135000'.
*  ELSEIF t_item-tax_code EQ 'S1'.
*    it_accounttax-gl_account = '0000235000'.
*  ENDIF.
*  it_accounttax-tax_code   = t_item-tax_code.
*  it_accounttax-acct_key   = 'VST'.
** IT_ACCOUNTTAX-TAXJURCODE =
*  it_accounttax-cond_key   = 'OZ10'.
** IT_ACCOUNTTAX-TAX_RATE   =
** IT_ACCOUNTTAX-TAX_DATE   =
** IT_ACCOUNTTAX-STAT_CON   =
** IT_ACCOUNTTAX-taxjurcode_deep
** IT_ACCOUNTTAX-taxjurcode_level
*  APPEND it_accounttax.

ENDFORM.                    "fill_accounttax

*---------------------------------------------------------------------*
*       FORM fill_currencyamount                                      *
*---------------------------------------------------------------------*
FORM fill_currencyamount.

  CLEAR it_currencyamount.
  it_currencyamount-itemno_acc   = w_item.
  it_currencyamount-curr_type    = '00'.
  it_currencyamount-currency     = t_head-curr.
* IT_CURRENCYAMOUNT-CURRENCY_ISO =
  it_currencyamount-amt_doccur   = w_net_amt.
* IT_CURRENCYAMOUNT-EXCH_RATE    = .
* it_currencyamount-amt_base     =
* IT_CURRENCYAMOUNT-DISC_BASE    =
* it_currencyamount-exch_rate_v  =
* it_currencyamount-disc_amt     =
  APPEND it_currencyamount.

ENDFORM.                    "fill_currencyamount

*---------------------------------------------------------------------*
*       FORM fill_criteria                                            *
*---------------------------------------------------------------------*
FORM fill_criteria.

* CLEAR it_criteria.
* it_criteria-itemno_acc = 2.
* it_criteria-fieldname = 'BZIRK'.
* it_criteria-character = '000001'.
* append it_criteria.

ENDFORM.                    "fill_criteria

*---------------------------------------------------------------------*
*       FORM fill_valuefield                                          *
*---------------------------------------------------------------------*
FORM fill_valuefield.

* CLEAR it_valuefield.
* it_valuefield-itemno_acc = 2.
* it_valuefield-fieldname = 'VV010'.
* it_valuefield-curr_type
* it_valuefield-currency = 'EUR'.
* it_valuefield-currency_iso
* it_valuefield-amt_valcom
* it_valuefield-base_uom
* it_valuefield-base_uom_iso
* it_valuefield-qua_valcom
* append it_valuefield.

ENDFORM.                    "fill_valuefield

*---------------------------------------------------------------------*
*       FORM fill_extension                                           *
*---------------------------------------------------------------------*
FORM fill_extension.

* CLEAR it_ext.
* it_ext-field1
* it_ext-field2
* it_ext-field3
* it_ext-field4
* APPEND it_ext.

* DATA: ls_zzz TYPE ZZZ_ACCIT.
* CLEAR it_ext2.
* it_ext2-structure = 'ZZZ_ACCIT'.
*  ls_zzz-posnr = 2.
*  ls_zzz-awref_reb = '123654'.
*  ls_zzz-aworg_reb = '654654'.
*  ls_zzz-grant_nbr = '0022002'.
* MOVE ls_zzz TO it_ext2-valuepart1.
* APPEND it_ext2.

ENDFORM.                    "fill_extension

*---------------------------------------------------------------------*
*       FORM fill_paymentcard                                         *
*---------------------------------------------------------------------*
FORM fill_paymentcard.

*  CLEAR it_paymentcard.
*  it_paymentcard-itemno_acc = 1.
*  it_paymentcard-cc_glaccount
*  it_paymentcard-cc_type
*  it_paymentcard-cc_number
*  it_paymentcard-cc_seq_no
*  it_paymentcard-cc_valid_f
*  it_paymentcard-cc_valid_t
*  it_paymentcard-cc_name
*  it_paymentcard-dataorigin
*  it_paymentcard-authamount = '100'.
*  it_paymentcard-currency = 'EUR'.
*  it_paymentcard-currency_iso
*  it_paymentcard-cc_autth_no
*  it_paymentcard-auth_refno
*  it_paymentcard-auth_date
*  it_paymentcard-auth_time
*  it_paymentcard-merchidcl
*  it_paymentcard-point_of_receipt
*  it_paymentcard-terminal
*  it_paymentcard-cctyp = '1'.
*  APPEND it_paymentcard.

ENDFORM.                    "fill_paymentcard

*---------------------------------------------------------------------*
*       FORM fill_contractitem                                        *
*---------------------------------------------------------------------*
FORM fill_contractitem.
* CLEAR it_fica_it.
*  it_fica_it-itemno_acc
*  it_fica_it-cont_acct
*  it_fica_it-main_trans
*  it_fica_it-sub_trans
*  it_fica_it-func_area
*  it_fica_it-fm_area
*  it_fica_it-cmmt_item
*  it_fica_it-funds_ctr
*  it_fica_it-fund
* append it_fica_it.
ENDFORM.                    "fill_contractitem

*&---------------------------------------------------------------------*
*&      Form  fill_re
*&---------------------------------------------------------------------*
FORM fill_re .

*  CLEAR it_re.
*  it_re-itemno_acc      =
*  it_re-business_entity =
*  it_re-building        =
*  it_re-property        =
*  it_re-rental_object   =
*  it_re-serv_charge_key =
*  it_re-settlement_unit =
*  it_re-contract_no     =
*  APPEND it_re.
*
ENDFORM.                    "fill_re

*&---------------------------------------------------------------------*
*&      Form  500_ERROR_DOWNLOAD
*&---------------------------------------------------------------------*
*       Error Download
*----------------------------------------------------------------------*
FORM 500_error_download .

  DATA l_filename TYPE string.
  l_filename = p_err.
*--FM to Download Internal table data to flat file
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*         BIN_FILESIZE                    =
      filename                        = l_filename
     filetype                        = 'ASC'
*         APPEND                          = ' '
     write_field_separator           = 'X'
*         HEADER                          = '00'
*         TRUNC_TRAILING_BLANKS           = ' '
*         WRITE_LF                        = 'X'
*         COL_SELECT                      = ' '
*         COL_SELECT_MASK                 = ' '
*         DAT_MODE                        = ' '
*         CONFIRM_OVERWRITE               = ' '
*         NO_AUTH_CHECK                   = ' '
*         CODEPAGE                        = ' '
*         IGNORE_CERR                     = ABAP_TRUE
*         REPLACEMENT                     = '#'
*         WRITE_BOM                       = ' '
*         TRUNC_TRAILING_BLANKS_EOL       = 'X'
*         WK1_N_FORMAT                    = ' '
*         WK1_N_SIZE                      = ' '
*         WK1_T_FORMAT                    = ' '
*         WK1_T_SIZE                      = ' '
*         WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*         SHOW_TRANSFER_STATUS            = ABAP_TRUE
*       IMPORTING
*         FILELENGTH                      =
    TABLES
      data_tab                        = t_err[]
*         FIELDNAMES                      =
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " 500_ERROR_DOWNLOAD
