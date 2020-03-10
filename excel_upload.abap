*&----------------------------------------------------------------------*
*&-------------------ABAP coding for Excel Upload-----------------------*
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*                             
*&----------------------------------------------------------------------*


DATA : p_file TYPE rlgrap-filename,
       it_excelfile TYPE alsmex_tabline,
       wa_excelfile TYPE alsmex_tabline.
       
* The function modules to process the excel data to an internal table.



1. AT SELECTION-SCREEN ON VALUE REQUEST FOR p_file.
   cl_gui_frontend_services=>file_open_dialog. (CHANGING PARAMETERS -> gt_tab TYPE filetable and v_rc TYPE sy-subrc)
   
   CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_extension       = '.XLSX'
      file_filter             = 'Microsoft Excel Files (*.XLS;*.XLSX;*.XLSM)|*.XLS;*.XLSX;*.XLSM|' ##NO_TEXT
    CHANGING
      file_table              = gt_tab
      rc                      = v_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0
    AND gt_tab IS NOT INITIAL.
    p_file = gt_tab[ 1 ].
  ENDIF.
  
     
2. F4_FILENAME.

     CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = 'P_FILE'
      IMPORTING
        file_name     = p_file.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE : 'Enter Valid File Format' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    

3. GUI_DOWNLOAD.(Declare the fields which the excel needs to have ty_dwld , it_dwld - output of GUI_DOWNLOAD )

CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = gv_file
        filetype                = 'DAT'
        write_field_separator   = 'X'
      TABLES
        data_tab                = it_dwld
*       fieldnames              = gt_fieldname
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    MESSAGE 'Operation Cancelled'(023) TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.

4. cl_gui_frontend_services=>file_save_dialog.

 IF sy-ucomm = 'FILE'.
    CLEAR: gv_path,gv_filename.
    gv_path = ' '.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name         = gv_path
      CHANGING
        filename                  = gv_filename
        path                      = gv_path
        fullpath                  = gv_path
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CONCATENATE gv_path gv_filename '.xls' INTO gv_file.
    PERFORM get_fields.
  ENDIF.




5. ALSM_EXCEL_TO_INTERNAL_TABLE. (for conversion of dates to internal table format use CONVERT_DATE_TO_INTERN_FORMAT
   while looping it_excelfile to wa_excelfile and then to internal table). 
   
   CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 10
      i_end_row               = 65536
    TABLES
      intern                  = it_excelfile
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Select a file to upload!!!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.


