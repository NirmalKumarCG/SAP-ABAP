
&*-----------------------------------------------------------------------------------------*

&*-------------------       ZCL_SEND_MAIL  DEFINITION     ---------------------------------*

&*-----------------------------------------------------------------------------------------*

 CLASS zcl_send_mail DEFINITION.
 
 PUBLIC SECTION.
 DATA : IP_HEADING TYPE SO_OBJ_DES,
        IT_BODY_TEXT TYPE ZMAIL_T_TEXT,
        IT_BODY_TBL_HEAD TYPE ZMAIL_T_COL_ATTR OPTIONAL,
        IT_BODY_TABLE TYPE STANDARD TABLE OPTIONAL,
        IP_DFLT_FOOTER TYPE CHAR1  DEFAULT 'X',
        IT_FOOTER TYPE ZMAIL_T_TEXT OPTIONAL,
        IP_SUBTOTAL TYPE FIELDNAME OPTIONAL,
        IP_XL_SUB TYPE SO_OBJ_DES OPTIONAL,
        IT_EXL_HEAD TYPE ZMAIL_T_TEXT OPTIONAL,
        IT_EXL_TABLE TYPE STANDARD TABLE OPTIONAL,
        IP_DLIST TYPE SO_DLI_NAM OPTIONAL,
        IT_TO_MAIL TYPE ZCL_REP_TYPE OPTIONAL,
        IT_CC_MAIL TYPE ZCL_REP_TYPE OPTIONAL,
        IT_BCC_MAIL TYPE ZCL_REP_TYPE OPTIONAL.
        
  DATA : gt_body           TYPE TABLE OF soli ##NEEDED,
           gs_body           TYPE soli ##NEEDED,
           gt_data           TYPE REF TO data,
           gcl_table         TYPE REF TO cl_abap_tabledescr,
           gcl_struc         TYPE REF TO cl_abap_structdescr,
           gt_fields         TYPE abap_compdescr_tab,
           gt_fieldcat       TYPE TABLE OF lvc_s_fcat,
           gs_dyn_str        TYPE REF TO data,
           gs_dyn_str1       TYPE REF TO data,
           gs_dyn_str2       TYPE REF TO data,
           gv_fld_nam        TYPE string,
           gt_sortord        TYPE abap_sortorder_tab,
           gs_sortord        TYPE abap_sortorder,
           gv_string         TYPE string,
           gt_binary         TYPE solix_tab,
           gv_size           TYPE so_obj_len,
           gv_dlist          TYPE so_obj_nam,
           gt_member         TYPE TABLE OF sodm1,
           gt_objpara        TYPE TABLE OF selc,
           gt_objparab       TYPE TABLE OF soop1,
           gcl_send_request  TYPE REF TO cl_bcs ##NEEDED,
           gcl_document      TYPE REF TO cl_document_bcs ##NEEDED,
           gcm_recipient     TYPE REF TO if_recipient_bcs ##NEEDED,
           gv_subject        TYPE so_obj_des ##NEEDED,
           gcm_bcs_exception TYPE REF TO cx_bcs ##NEEDED,
           gv_sent           TYPE os_boolean ##NEEDED,
           gv_mail           TYPE comm_id_long ##NEEDED,
           gv_cc             TYPE c LENGTH 1,
           gv_bcc            TYPE c LENGTH 1,
           lv_date           TYPE c LENGTH 10,
           lv_string         TYPE string.

    FIELD-SYMBOLS : <lfss_value>  TYPE any,
                    <lfss_value1> TYPE any,
                    <lfss_value2> TYPE any,
                    <lfss_final>  TYPE any,
                    <lfst_sum>    TYPE STANDARD TABLE,
                    <lfss_sum>    TYPE any,
                    <lfst_sub>    TYPE STANDARD TABLE,
                    <lfss_sub>    TYPE any,
                    <gfst_body>   TYPE STANDARD TABLE.
        
 METHODS : send_mail.
 
 PROTECTED SECTION.
 
 PRIVATE SECTION.
 
 ENDCLASS.
 
&*-----------------------------------------------------------------------------------------*

&*-------------------       ZCL_SEND_MMAIL IMPLEMENTATION   -------------------------------*

&*-----------------------------------------------------------------------------------------*

 CLASS zcl_send_mail IMPLEMENTATION.
  METHOD send_mail.
   
   IF ip_dlist     IS NOT INITIAL OR
       it_to_mail[] IS NOT INITIAL.

      REFRESH gt_body[]. CLEAR gs_body.

      gs_body-line = '<HTML>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = '<BODY>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = '<HEAD>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = '<STYLE>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = 'TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = 'TH, TD'." { TEXT-ALIGN: LEFT; }'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = 'TR:NTH-CHILD(EVEN){ BACKGROUND-COLOR: #F2F2F2; }'. "ZEBRA
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = 'TH { BACKGROUND-COLOR: #FFFF00; COLOR: BLACK; }'.  "Heading border
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = 'TH { font-family:Tahoma; font-size:9pt; }'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = 'TD { font-family:Tahoma; font-size:9pt; }'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = '</STYLE>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = '</HEAD>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      LOOP AT it_body_text INTO DATA(ls_body_text).
        CONCATENATE '<font face="Tahoma"; font size="2">' ls_body_text-ztext '</font>' INTO gs_body-line.
        APPEND gs_body TO gt_body.
        CLEAR gs_body.

        gs_body-line = '<br>'.
        APPEND gs_body TO gt_body.
        CLEAR gs_body.

        gs_body-line = '<br>'.
        APPEND gs_body TO gt_body.
        CLEAR gs_body.
      ENDLOOP.

      IF it_body_tbl_head[] IS NOT INITIAL AND
         it_body_table[]    IS NOT INITIAL.

        gs_body-line = '<TABLE BORDER="1" >'.
        APPEND gs_body TO gt_body.
        CLEAR gs_body.

        gs_body-line = '<TR>'.  "New ROW for heading
        APPEND gs_body TO gt_body.
        CLEAR gs_body.

        LOOP AT it_body_tbl_head INTO DATA(ls_body_tbl_head).
          CONCATENATE '<TH>' ls_body_tbl_head-ftext '</TH>' INTO gs_body-line.
          APPEND gs_body TO gt_body.
          CLEAR gs_body.
        ENDLOOP.

        gs_body-line = '</TR>'.  "New ROW
        APPEND gs_body TO gt_body.
        CLEAR gs_body.

        GET REFERENCE OF it_body_table INTO gt_data.
        ASSIGN gt_data->* TO <gfst_body>.
        gcl_table ?= cl_abap_structdescr=>describe_by_data_ref( gt_data ).
        gcl_struc ?= gcl_table->get_table_line_type( ).

        gt_fields = gcl_struc->components.

*        <gfst_body> = it_body_table[].

        IF ip_subtotal IS NOT INITIAL.
          REFRESH gt_sortord[]. CLEAR gs_sortord.
          READ TABLE gt_fields WITH KEY name = ip_subtotal
                               TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            DATA(lv_index) = sy-tabix.
          ENDIF.

          LOOP AT gt_fields INTO DATA(ls_fields)
                            FROM 1 TO lv_index.
            MOVE ls_fields-name TO gs_sortord-name.
            APPEND gs_sortord TO gt_sortord.
          ENDLOOP.

          SORT <gfst_body> BY (gt_sortord).
        ENDIF.

        READ TABLE it_body_tbl_head WITH KEY zzsum = 'X'
                                    TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          REFRESH gt_fieldcat[].
          LOOP AT it_body_tbl_head INTO ls_body_tbl_head.
            READ TABLE gt_fields INTO ls_fields
                                 WITH KEY name = ls_body_tbl_head-field.
            IF sy-subrc = 0.
              gt_fieldcat[] = VALUE #( BASE gt_fieldcat ( fieldname = ls_fields-name
                                                          inttype   = ls_fields-type_kind
                                                          intlen    = ls_fields-length
                                                          decimals  = ls_fields-decimals ) ).
            ENDIF.
          ENDLOOP.

          IF gt_fieldcat[] IS NOT INITIAL.
            CALL METHOD cl_alv_table_create=>create_dynamic_table
              EXPORTING
                it_fieldcatalog           = gt_fieldcat
              IMPORTING
                ep_table                  = gs_dyn_str
              EXCEPTIONS
                generate_subpool_dir_full = 1
                OTHERS                    = 2.

            ASSIGN gs_dyn_str->* TO <lfst_sum>.
            CREATE DATA gs_dyn_str1 LIKE LINE OF <lfst_sum>.
            ASSIGN gs_dyn_str1->* TO <lfss_sum>.

            ASSIGN gs_dyn_str->* TO <lfst_sub>.
            CREATE DATA gs_dyn_str2 LIKE LINE OF <lfst_sub>.
            ASSIGN gs_dyn_str2->* TO <lfss_sub>.
          ENDIF.
        ENDIF.

        LOOP AT <gfst_body> ASSIGNING FIELD-SYMBOL(<lfss_body>).
          gs_body-line = '<TR>'.  "New ROW for heading
          APPEND gs_body TO gt_body.
          CLEAR gs_body.

          LOOP AT gt_fields ASSIGNING FIELD-SYMBOL(<lfss_fields>).
            ASSIGN COMPONENT <lfss_fields>-name OF STRUCTURE <lfss_body>
                                                TO <lfss_value>.
            IF sy-subrc = 0.
              READ TABLE it_body_tbl_head INTO ls_body_tbl_head
                                          WITH KEY field = <lfss_fields>-name.
              IF sy-subrc = 0.
                IF ls_body_tbl_head-datef EQ 'X'.
                  CLEAR lv_date.
                  WRITE <lfss_value> TO lv_date DD/MM/YYYY.

                  IF ls_body_tbl_head-cbold EQ 'X'.
                    IF ls_body_tbl_head-rgtag EQ 'X'.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px" align="right"> <b>' lv_date '</b> </TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ELSE.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px"> <b>' lv_date '</b> </TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ENDIF.
                  ELSE.
                    IF ls_body_tbl_head-rgtag EQ 'X'.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px" align="right">' lv_date '</TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ELSE.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px">' lv_date '</TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ENDIF.
                  ENDIF.
                ELSE.
                  CLEAR lv_string.
                  MOVE <lfss_value> TO lv_string.
                  ASSIGN ('LV_STRING') TO <lfss_final>.
                  IF ls_body_tbl_head-cbold EQ 'X'.
                    IF ls_body_tbl_head-rgtag EQ 'X'.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px" align="right"> <b>' <lfss_final> '</b> </TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ELSE.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px"> <b>' <lfss_final> '</b> </TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ENDIF.
                  ELSE.
                    IF ls_body_tbl_head-rgtag EQ 'X'.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px" align="right">' <lfss_final> '</TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ELSE.
                      CONCATENATE '<TD bgcolor="' ls_body_tbl_head-clrcd '"nowrap style="padding-right: 10px">' <lfss_final> '</TD>' INTO gs_body-line.
                      APPEND gs_body TO gt_body.
                      CLEAR gs_body.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
                CLEAR lv_string.
                MOVE <lfss_value> TO lv_string.
                ASSIGN ('LV_STRING') TO <lfss_final>.

                CONCATENATE '<TD nowrap style="padding-right: 10px">' <lfss_final> '</TD>' INTO gs_body-line.
                APPEND gs_body TO gt_body.
                CLEAR gs_body.
              ENDIF.

              CLEAR gv_fld_nam.
              CONCATENATE '<LFSS_SUM>-' <lfss_fields>-name INTO gv_fld_nam.
              ASSIGN (gv_fld_nam) TO <lfss_value1>.
              IF sy-subrc = 0.
                <lfss_value1> = <lfss_value1> + <lfss_value>.
              ENDIF.

              CLEAR gv_fld_nam.
              CONCATENATE '<LFSS_SUB>-' <lfss_fields>-name INTO gv_fld_nam.
              ASSIGN (gv_fld_nam) TO <lfss_value2>.
              IF sy-subrc = 0.
                <lfss_value2> = <lfss_value2> + <lfss_value>.
              ENDIF.
            ENDIF.
          ENDLOOP.

          gs_body-line = '</TR>'.  "New ROW for heading
          APPEND gs_body TO gt_body.
          CLEAR gs_body.

          IF ip_subtotal IS NOT INITIAL.
            AT END OF (ip_subtotal).
              gs_body-line = '<TR bgcolor="#FFFF00" style="font-weight:bold">'.
              APPEND gs_body TO gt_body.
              CLEAR gs_body.

              LOOP AT it_body_tbl_head ASSIGNING FIELD-SYMBOL(<lfss_tbl_fld>)
                                       WHERE zzsum EQ 'X'.
                CLEAR gv_fld_nam.
                CONCATENATE '<LFSS_SUB>-' <lfss_tbl_fld>-field INTO gv_fld_nam.
                ASSIGN (gv_fld_nam) TO <lfss_value1>.
                IF sy-subrc = 0.
                  CLEAR lv_string.
                  MOVE <lfss_value1> TO lv_string.
                  ASSIGN ('LV_STRING') TO <lfss_final>.
                  CONCATENATE '<TD nowrap style="padding-right: 10px" align="right">' <lfss_final>  '</TD>' INTO gs_body-line.
                  APPEND gs_body TO gt_body.
                  CLEAR gs_body.
                ENDIF.
              ENDLOOP.

              gs_body-line = '</TR>'.  "New ROW
              APPEND gs_body TO gt_body.
              CLEAR gs_body.

              IF <lfss_sub> IS ASSIGNED.
                CLEAR <lfss_sub>.
              ENDIF.
            ENDAT.
          ENDIF.

          READ TABLE it_body_tbl_head WITH KEY zzsum = 'X'
                                      TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            AT LAST.
              gs_body-line = '<TR bgcolor="#FFFF00" style="font-weight:bold">'.
              APPEND gs_body TO gt_body.
              CLEAR gs_body.

              LOOP AT it_body_tbl_head ASSIGNING <lfss_tbl_fld>
                                       WHERE zzsum EQ 'X'.
                CLEAR gv_fld_nam.
                CONCATENATE '<LFSS_SUM>-' <lfss_tbl_fld>-field INTO gv_fld_nam.
                ASSIGN (gv_fld_nam) TO <lfss_value1>.
                IF sy-subrc = 0.
                  CLEAR lv_string.
                  MOVE <lfss_value1> TO lv_string.
                  ASSIGN ('LV_STRING') TO <lfss_final>.

                  CONCATENATE '<TD nowrap style="padding-right: 10px" align="right">' <lfss_final>  '</TD>' INTO gs_body-line.
                  APPEND gs_body TO gt_body.
                  CLEAR gs_body.
                ENDIF.
              ENDLOOP.

              gs_body-line = '</TR>'.  "New ROW
              APPEND gs_body TO gt_body.
              CLEAR gs_body.
            ENDAT.
          ENDIF.
        ENDLOOP.

        gs_body-line = '</TABLE>'.
        APPEND gs_body TO gt_body.
        CLEAR gs_body.

        gs_body-line = '<BR>'.  "New line
        APPEND gs_body TO gt_body.
        CLEAR gs_body.

        gs_body-line = '<BR>'.  "New line
        APPEND gs_body TO gt_body.
        CLEAR gs_body.
      ENDIF.

      IF it_footer[] IS NOT INITIAL.
        LOOP AT it_footer INTO DATA(ls_footer).
          CONCATENATE '<font face="Tahoma"; font size="2">' ls_footer-ztext '</font>' INTO gs_body-line.
          APPEND gs_body TO gt_body.
          CLEAR gs_body.

          gs_body-line = '<BR>'.  "New line
          APPEND gs_body TO gt_body.
          CLEAR gs_body.
        ENDLOOP.
      ENDIF.

      IF ip_dflt_footer EQ 'X'.
        CONCATENATE '<font face="Tahoma"; font size="2">' '************** This is an auto generated mail. Please do not reply to this mail**************' '</font>' INTO gs_body-line.
        APPEND gs_body TO gt_body.
        CLEAR gs_body.
      ENDIF.

      gs_body-line = '</BODY>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      gs_body-line = '</HTML>'.
      APPEND gs_body TO gt_body.
      CLEAR gs_body.

      IF it_exl_head[]  IS NOT INITIAL AND
         it_exl_table[] IS NOT INITIAL.

        CLEAR gv_string.

        CONCATENATE '<!DOCTYPE HTML> <HTML> <HEAD> <STYLE>'
                    'P {FONT-FAMILY:"CALIBRI"; FONT-SIZE:11.0PT }'
                    'TABLE, TH, TD { BORDER: 1PX SOLID #EEE; BORDER-COLLAPSE: COLLAPSE;}'
                    '</STYLE> </HEAD> <BODY> <TABLE ID="T01"> <TR>' INTO gv_string
                    SEPARATED BY space.

        LOOP AT it_exl_head INTO DATA(ls_exl_head).
          CONCATENATE gv_string '<th  bgcolor = #ffff00 >' ls_exl_head-ztext '</th>' INTO gv_string SEPARATED BY space.

          AT LAST.
            CONCATENATE gv_string '</tr>' INTO gv_string.
          ENDAT.
        ENDLOOP.

        REFRESH : gt_fields[]. CLEAR gt_data.

        GET REFERENCE OF it_exl_table INTO gt_data.
        gcl_table ?= cl_abap_structdescr=>describe_by_data_ref( gt_data ).
        gcl_struc ?= gcl_table->get_table_line_type( ).

        gt_fields = gcl_struc->components.

        LOOP AT it_exl_table ASSIGNING FIELD-SYMBOL(<lfss_xl_table>).
          CONCATENATE gv_string '<tr>' INTO gv_string.
          LOOP AT gt_fields ASSIGNING <lfss_fields>.
            ASSIGN COMPONENT <lfss_fields>-name OF STRUCTURE <lfss_xl_table>
                                                TO <lfss_value>.
            IF sy-subrc = 0.
              CLEAR lv_string.
              MOVE <lfss_value> TO lv_string.
              ASSIGN ('LV_STRING') TO <lfss_final>.
              CONCATENATE gv_string '<td>' <lfss_final> '</td>' INTO gv_string SEPARATED BY space.
            ENDIF.
          ENDLOOP.
          CONCATENATE gv_string '</tr>' INTO gv_string.
        ENDLOOP.

        CONCATENATE gv_string '</table> <br> </br> </body> </html>' INTO gv_string SEPARATED BY space.

        IF gv_string IS NOT INITIAL.
          REFRESH gt_binary[]. CLEAR gv_size.
          TRY.
              cl_bcs_convert=>string_to_solix(
                EXPORTING
                  iv_string   = gv_string
                  iv_codepage = '4103'  "suitable for MS Excel, leave empty
                  iv_add_bom  = 'X'     "for other doc types
                IMPORTING
                  et_solix  = gt_binary
                  ev_size   = gv_size ).
            CATCH cx_bcs.
              MESSAGE e445(so).
          ENDTRY.
        ENDIF.
      ENDIF.

      IF ip_dlist IS NOT INITIAL.
        MOVE ip_dlist TO  gv_dlist.
        REFRESH: gt_member, gt_objpara, gt_objparab.
        CALL FUNCTION 'SO_DLI_READ' ##FM_SUBRC_OK
          EXPORTING
            distributionlist           = gv_dlist
            system_dli                 = 'X'
          TABLES
            member                     = gt_member
            objpara                    = gt_objpara
            objparb                    = gt_objparab
          EXCEPTIONS
            active_user_not_exist      = 1
            communication_failure      = 2
            component_not_available    = 3
            dl_name_not_exist          = 4
            folder_not_exist           = 5
            folder_no_authorization    = 6
            forwarder_not_exist        = 7
            object_not_exist           = 8
            object_no_authorization    = 9
            operation_no_authorization = 10
            owner_not_exist            = 11
            parameter_error            = 12
            substitute_not_active      = 13
            substitute_not_defined     = 14
            system_failure             = 15
            user_not_exist             = 16
            x_error                    = 17
            OTHERS                     = 18.
      ENDIF.

      IF it_to_mail[] IS NOT INITIAL.
        gt_member[] = VALUE #( BASE gt_member FOR ls_to_mail IN it_to_mail
                             ( address = ls_to_mail-mail_id ) ).
      ENDIF.

      IF it_cc_mail[] IS NOT INITIAL.
        gt_member[] = VALUE #( BASE gt_member FOR ls_cc_mail IN it_cc_mail
                             ( address = ls_cc_mail-mail_id
                               sndcp   = 'X' ) ).
      ENDIF.

      IF it_bcc_mail[] IS NOT INITIAL.
        gt_member[] = VALUE #( BASE gt_member FOR ls_bcc_mail IN it_bcc_mail
                             ( address = ls_bcc_mail-mail_id
                               sndbc   = 'X' ) ).
      ENDIF.

      SORT gt_member BY address.
      DELETE ADJACENT DUPLICATES FROM gt_member
                      COMPARING address.

      TRY .
          IF gt_member[] IS NOT INITIAL.
            gcl_send_request = cl_bcs=>create_persistent( ).

            MOVE ip_heading TO gv_subject.

            gcl_document = cl_document_bcs=>create_document(
               i_type    = 'HTM'
               i_text    = gt_body
               i_subject = gv_subject ).

            IF gt_binary IS NOT INITIAL.
              gcl_document->add_attachment(
                  i_attachment_type    = 'xls'
                  i_attachment_subject = ip_xl_sub
                  i_attachment_size    = gv_size
                  i_att_content_hex    = gt_binary ).
            ENDIF.

            gcl_send_request->set_document( gcl_document ).

            LOOP AT gt_member INTO DATA(ls_member).
              CLEAR gv_mail.
              MOVE ls_member-address TO gv_mail.
              gcm_recipient = cl_cam_address_bcs=>create_internet_address( gv_mail ).
              CLEAR gv_cc.
              IF ls_member-sndcp = 'X'.
                MOVE 'X' TO gv_cc.
              ENDIF.

              CLEAR gv_bcc.
              IF ls_member-sndbc = 'X'.
                MOVE 'X' TO gv_bcc.
              ENDIF.

              CALL METHOD gcl_send_request->add_recipient
                EXPORTING
                  i_recipient  = gcm_recipient
                  i_copy       = gv_cc
                  i_blind_copy = gv_bcc.

              CLEAR : ls_member, gv_mail.
            ENDLOOP.

            gcl_send_request->set_sender( cl_sapuser_bcs=>create( sy-uname ) ).

            gv_sent = gcl_send_request->send( i_with_error_screen = 'X' ).
            COMMIT WORK.

            IF gv_sent IS INITIAL.
              MESSAGE i500(sbcoms) WITH gv_mail.
            ELSE.
              MESSAGE s022(so).
            ENDIF.
          ENDIF.
        CATCH cx_bcs INTO gcm_bcs_exception.
          MESSAGE i865(so) WITH gcm_bcs_exception->error_type.
      ENDTRY.

      SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.
    ENDIF.
  ENDMETHOD.
  
  ENDCLASS.
