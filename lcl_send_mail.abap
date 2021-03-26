FORM send_mail .


  DATA : lv_mail     TYPE REF TO zcl_mail,
         lo_mailer   TYPE REF TO cl_bcs,
         lo_subject  TYPE REF TO cl_bcs,
         lt_head     TYPE  zcl_head_type,
         lt_head_exc TYPE  zcl_head_type,
         lv_head     TYPE  string,
         lv_date     TYPE char10,
         document    TYPE REF TO cl_document_bcs ##NEEDED.

  CREATE OBJECT : lv_mail.

  lv_date = |{ sy-datum DATE = USER }|.
  lv_head = 'Segment wise LCV Plant Stock on' && ` ` && lv_date.

  lt_head = VALUE #( ( heading = 'Segment' )
                     ( heading = 'Less than 30 days' )
                     ( heading = '30 days to 60 days' )
                     ( heading = 'More than 60 days' )
                     ( heading = 'Total' )  ).

  lt_head_exc = VALUE #( ( heading = 'Plant' )
*===> Begin Of Changes by TP_HTL005689 -  SE1K9A1U5C
                         ( heading = 'Plant Name' )
*===> End Of Changes by TP_HTL005689 -  SE1K9A1U5C
                         ( heading = 'PTS Date' )
                         ( heading = 'Material' )
                         ( heading = 'Age in (No. of Days)' )
                         ( heading = 'Model' )
*                         ( heading = 'Segment' )
                         ( heading = 'Equipment Number' )  ).


  CALL METHOD lv_mail->mail_body
    EXPORTING
      i_heading          = lt_head
      i_table            = it_mail
      heading            = lv_head
      sub_doc            = 'DM062 - AL LCV Plant Stock : Ageing Mailer'
      no_auto_gen_msg    = ' '
      no_of_records_mseg = 'X'
    IMPORTING
      lo_document        = document
    CHANGING
      lo_bcs             = lo_mailer.

*  CALL METHOD lv_mail->mail_body
*    EXPORTING
*      i_heading       = lt_head
*      i_table         = it_mail
*      heading         = lv_head
*      sub_doc         = 'DM062 - AL LCV Plant Stock : Ageing Mailer'
*      no_auto_gen_msg = ' '
*
*    IMPORTING
*      lo_document     = document
*    CHANGING
*      lo_bcs          = lo_mailer.


  CALL METHOD lv_mail->excel_attachment
    EXPORTING
      i_table        = it_excel
      i_heading      = lt_head_exc
      lo_document    = document
      excel_doc_name = 'LCV Vintage Mailer'
    CHANGING
      lo_bcs         = lo_mailer.


  IF  p_dlist IS NOT INITIAL.

    CALL METHOD lv_mail->add_recipient
      EXPORTING
*       i_receipient    = i_rep
*       i_receipient_cc =
*       i_receipient_bcc =
        lo_bcs          = lo_mailer
*       mail_id         = 'X'
        dlist           = 'X'
        dlist_id        = p_dlist
      EXCEPTIONS
        invalid_mail_id = 1
*       empty_mail_id   = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  CALL METHOD lv_mail->add_subject
    EXPORTING
      subject                  = 'DM062 - AL LCV Plant Stock : Ageing Mailer'
      lo_bcs                   = lo_mailer
    EXCEPTIONS
      subject_lenght_outbounds = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL METHOD lv_mail->add_send_mail
    EXPORTING
      lo_bcs = lo_mailer.
ENDFORM.
