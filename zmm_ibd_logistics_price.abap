*&---------------------------------------------------------------------*
*&             Report  ZMM_IBD_LOGISTICS_PRICE_REP                     *
* Author's name   : Nirmal Kumar CG                                    *
* Program title   : IBD Logistics Price Report                         *
* Module          : MM
*&---------------------------------------------------------------------*
* Description : Generating AlV report based on the selection creteria.
*----------------------------------------------------------------------*

REPORT zmm_ibd_logistics_price_rep.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_shtyp-low.

  IF s_kschl IS NOT INITIAL.

    PERFORM list_parameter.

  ENDIF.

  START-OF-SELECTION.

  PERFORM get_data.
  PERFORM fill_fieldcatlog.
  PERFORM display_alv.
   

*&---------------------------------------------------------------------*
*&  Include           ZMM_IBD_LOGISTIC_PRICE_REP_TOP
*&---------------------------------------------------------------------*

INCLUDE zmm_ibd_logistics_price_rep_top.
  
TABLES : a659 ,a620 ,lfa1 , t173t , konp ##NEEDED.

TYPES : BEGIN OF ty_final,
          mandt    TYPE mandt,
          kschl    TYPE a659-kschl,
          shtyp    TYPE a659-shtyp,
          bezei    TYPE t173t-bezei,
          tdlnr    TYPE a659-tdlnr,
          name1    TYPE lfa1-name1,
          werks    TYPE a659-werks,
          tplst    TYPE a620-tplst,
          route    TYPE a659-route,
          rodes    type tvrot-bezei,
          vsart    TYPE a659-vsart,
          shdes    TYPE tvtkt-bezei,
          kfrst    TYPE a659-kfrst,
          datbi    TYPE a659-datbi,
          datab    TYPE a659-datab,
          kbstat   TYPE a659-kbstat,
          knumh    TYPE a659-knumh,
          kbetr    TYPE konp-kbetr,
          konwa    TYPE konp-konwa,
          loevm_ko TYPE loevm_ko,
        END OF ty_final.



DATA : gt_final   TYPE STANDARD TABLE OF ty_final ##NEEDED,
       gs_final   TYPE                   ty_final ##NEEDED,
       gt_fcat    TYPE lvc_t_fcat ##NEEDED,
       gs_fcat    TYPE lvc_s_fcat ##NEEDED,
       gv_tabname TYPE slis_tabname ##NEEDED,
       gs_layout  TYPE lvc_s_layo ##NEEDED.

TYPES : BEGIN OF ty_shtyp,
          shtyp TYPE shtyp,
        END OF ty_shtyp.


DATA: it_dynpro_values TYPE STANDARD TABLE OF dynpread ##NEEDED,
      progname         TYPE sy-repid ##NEEDED,
      dynnum           TYPE sy-dynnr ##NEEDED.

DATA       : it_shtyp TYPE STANDARD TABLE OF ty_shtyp ##NEEDED,
             wa_shtyp TYPE ty_shtyp ##NEEDED.
             
*&---------------------------------------------------------------------*
*&  Include           ZMM_IBD_LOGISTICS_PRICE_REP_SS
*&---------------------------------------------------------------------*

INCLUDE zmm_ibd_logitics_price_rep_ss.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.   """Inputs

SELECTION-SCREEN SKIP.


SELECT-OPTIONS : s_kschl FOR a659-kschl NO INTERVALS,
                 s_shtyp FOR a659-shtyp NO INTERVALS,
                 s_tdlnr FOR a659-tdlnr NO INTERVALS,
                 s_tplst FOR a620-tplst NO INTERVALS,
                 s_route FOR a659-route NO INTERVALS,
                 s_vsart FOR a659-vsart NO INTERVALS,
                 s_date  FOR a659-datab NO INTERVALS NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&  Include           ZMM_IBD_LOGISTIC_PRICE_REP_SUB
*&---------------------------------------------------------------------*

INCLUDE zmm_ibd_logitics_price_rep_sub.

FORM list_parameter.

  SELECT DISTINCT shtyp
           FROM a659
           INTO TABLE @DATA(it_a659).

  SELECT DISTINCT shtyp
         FROM a620
         INTO TABLE @DATA(it_a620).

  LOOP AT s_kschl.
    IF s_kschl-low EQ 'ZM01' OR
       s_kschl-low EQ 'ZM13'.
      APPEND LINES OF it_a659 TO it_shtyp.

    ELSEIF s_kschl-low EQ 'ZF00'.
      APPEND LINES OF it_a620 TO it_shtyp.

    ENDIF.
  ENDLOOP.
  SORT it_shtyp BY shtyp.
  DELETE ADJACENT DUPLICATES FROM it_shtyp COMPARING shtyp.
  CLEAR wa_shtyp.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST' ##FM_SUBRC_OK
    EXPORTING
      retfield        = 'SHTYP'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'S_SHTYP-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = it_shtyp
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.



FORM get_data.

  DATA : lv_date TYPE dats.

  READ TABLE s_date INDEX 1.
  IF sy-subrc EQ 0 .
    lv_date = s_date-low.
  ENDIF.

  IF s_kschl IS NOT INITIAL.
    CASE s_kschl-low.
      WHEN 'ZM01'.

        IF lv_date IS INITIAL.

          SELECT a~mandt , a~kschl , a~shtyp ,a~tdlnr ,a~werks ,a~route ,a~vsart ,b~bezei,
          a~kfrst ,a~datbi ,a~datab ,a~kbstat ,a~knumh ,c~kbetr ,c~konwa ,c~loevm_ko , d~bezei AS bezei0 , e~name1 , f~bezei AS bezei1
          FROM a659  AS a
          INNER JOIN tvtkt AS b ON b~shtyp = a~shtyp
          INNER JOIN konp AS c ON c~knumh = a~knumh AND c~kschl = a~kschl
          INNER JOIN t173t AS d ON d~vsart = a~vsart
          INNER JOIN lfa1 AS e ON e~lifnr = a~tdlnr
          INNER JOIN tvrot AS f ON f~route = a~route
          INTO TABLE @DATA(gt_data)
           WHERE a~kschl = 'ZM01'
           AND a~kschl IN @s_kschl
           AND d~spras = @sy-langu
           AND b~spras = @sy-langu
           AND f~spras = @sy-langu
           AND a~shtyp  IN @s_shtyp
           AND a~tdlnr IN @s_tdlnr
           AND a~werks IN @s_tplst
           AND a~route IN @s_route
           AND a~vsart IN @s_vsart.


        ELSE.
          REFRESH : gt_data.
          SELECT a~mandt  a~kschl  a~shtyp a~tdlnr a~werks a~route a~vsart b~bezei
       a~kfrst a~datbi a~datab a~kbstat a~knumh c~kbetr c~konwa c~loevm_ko  d~bezei AS bezei0  e~name1  f~bezei AS bezei1
       FROM a659  AS a
       INNER JOIN tvtkt AS b ON b~shtyp = a~shtyp
       INNER JOIN konp AS c ON c~knumh = a~knumh AND c~kschl = a~kschl
       INNER JOIN t173t AS d ON d~vsart = a~vsart
       INNER JOIN lfa1 AS e ON e~lifnr = a~tdlnr
       INNER JOIN tvrot AS f ON f~route = a~route
       INTO TABLE gt_data
        WHERE a~kschl = 'ZM01'
        AND a~kschl IN s_kschl
        AND d~spras = sy-langu
        AND b~spras = sy-langu
        AND f~spras = sy-langu
        AND a~shtyp  IN s_shtyp
        AND a~tdlnr IN s_tdlnr
        AND a~werks IN s_tplst
        AND a~route IN s_route
        AND a~vsart IN s_vsart
        AND a~datab LE lv_date
        AND a~datbi GE lv_date.

        ENDIF.

        IF gt_data IS NOT INITIAL.

          gt_final[] = VALUE #( FOR gs_data IN gt_data
                              ( mandt    = gs_data-mandt
                                kschl    = gs_data-kschl
                                shtyp    = gs_data-shtyp
                                shdes    = gs_data-bezei0
                                tdlnr    = gs_data-tdlnr
                                werks    = gs_data-werks
                                route    = gs_data-route
                                rodes    = gs_data-bezei1
                                vsart    = gs_data-vsart
                                bezei    = gs_data-bezei
                                kfrst    = gs_data-kfrst
                                datbi    = gs_data-datbi
                                datab    = gs_data-datab
                                kbstat   = gs_data-kbstat
                                knumh    = gs_data-knumh
                                kbetr    = gs_data-kbetr
                                konwa    = gs_data-konwa
                                loevm_ko = gs_data-loevm_ko
                                name1    = gs_data-name1 ) ).

        ELSE.
          MESSAGE text-040 TYPE 'S' DISPLAY LIKE 'I'.
          LEAVE LIST-PROCESSING.

        ENDIF.

      WHEN 'ZM13'.

        IF lv_date IS INITIAL.

          SELECT a~mandt , a~kschl , a~shtyp ,a~tdlnr ,a~werks ,a~route ,a~vsart ,b~bezei ,
          a~kfrst ,a~datbi ,a~datab ,a~kbstat ,a~knumh ,c~kbetr ,c~konwa ,c~loevm_ko , d~bezei AS bezei0 , e~name1 , f~bezei AS bezei1
          FROM a659  AS a
          INNER JOIN tvtkt AS b ON b~shtyp = a~shtyp
          INNER JOIN konp AS c ON c~knumh = a~knumh AND c~kschl = a~kschl
          INNER JOIN t173t AS d ON d~vsart = a~vsart
          INNER JOIN lfa1 AS e ON e~lifnr = a~tdlnr
          INNER JOIN tvrot AS f ON f~route = a~route
           INTO TABLE @DATA(gt_data1)
           WHERE a~kschl = 'ZM13'
           AND a~kschl IN @s_kschl
           AND b~spras = @sy-langu
           AND d~spras = @sy-langu
           AND f~spras = @sy-langu
           AND a~shtyp IN @s_shtyp
           AND a~tdlnr IN @s_tdlnr
           AND a~werks IN @s_tplst
           AND a~route IN @s_route
           AND a~vsart IN @s_vsart.

        ELSE.

          REFRESH : gt_data1.
          SELECT a~mandt  a~kschl  a~shtyp a~tdlnr a~werks a~route a~vsart b~bezei
       a~kfrst a~datbi a~datab a~kbstat a~knumh c~kbetr c~konwa c~loevm_ko  d~bezei AS bezei0  e~name1  f~bezei AS bezei1
       FROM a659  AS a
       INNER JOIN tvtkt AS b ON b~shtyp = a~shtyp
       INNER JOIN konp AS c ON c~knumh = a~knumh AND c~kschl = a~kschl
       INNER JOIN t173t AS d ON d~vsart = a~vsart
       INNER JOIN lfa1 AS e ON e~lifnr = a~tdlnr
       INNER JOIN tvrot AS f ON f~route = a~route
        INTO TABLE gt_data1
        WHERE a~kschl = 'ZM13'
        AND a~kschl IN s_kschl
        AND b~spras = sy-langu
        AND d~spras = sy-langu
        AND f~spras = sy-langu
        AND a~shtyp IN s_shtyp
        AND a~tdlnr IN s_tdlnr
        AND a~werks IN s_tplst
        AND a~route IN s_route
        AND a~vsart IN s_vsart
        AND a~datab LE lv_date
        AND a~datbi GE lv_date.


        ENDIF.



        IF gt_data1 IS NOT INITIAL.

          gt_final[] = VALUE #( FOR gs_data1 IN gt_data1
                              ( mandt    = gs_data1-mandt
                                kschl    = gs_data1-kschl
                                shtyp    = gs_data1-shtyp
                                shdes    = gs_data1-bezei0
                                tdlnr    = gs_data1-tdlnr
                                werks    = gs_data1-werks
                                route    = gs_data1-route
                                rodes    = gs_data1-bezei1
                                vsart    = gs_data1-vsart
                                bezei    = gs_data1-bezei
                                kfrst    = gs_data1-kfrst
                                datbi    = gs_data1-datbi
                                datab    = gs_data1-datab
                                kbstat   = gs_data1-kbstat
                                knumh    = gs_data1-knumh
                                kbetr    = gs_data1-kbetr
                                konwa    = gs_data1-konwa
                                loevm_ko = gs_data1-loevm_ko
                                name1    = gs_data1-name1 ) ).

          ELSE.
          MESSAGE text-040 TYPE 'S' DISPLAY LIKE 'I'.
          LEAVE LIST-PROCESSING.

        ENDIF.



      WHEN 'ZF00'.


        IF lv_date IS INITIAL.

          SELECT a~mandt , a~kschl , a~shtyp ,a~tdlnr ,a~tplst ,a~route ,a~vsart ,b~bezei ,
          a~kfrst ,a~datbi ,a~datab ,a~kbstat ,a~knumh ,c~kbetr ,c~konwa ,c~loevm_ko , d~bezei AS bezei0 , e~name1 , f~bezei AS bezei1
          FROM a620  AS a
          INNER JOIN tvtkt AS b ON b~shtyp = a~shtyp
          INNER JOIN konp AS c ON c~knumh = a~knumh AND c~kschl = a~kschl
          INNER JOIN t173t AS d ON d~vsart = a~vsart
          INNER JOIN lfa1 AS e ON e~lifnr = a~tdlnr
          INNER JOIN tvrot AS f ON f~route = a~route
           INTO TABLE @DATA(gt_data2)
            WHERE a~kschl = 'ZF00'
           AND a~kschl IN @s_kschl
           AND b~spras = @sy-langu
           AND d~spras = @sy-langu
           AND f~spras = @sy-langu
           AND a~shtyp IN @s_shtyp
           AND a~tdlnr IN @s_tdlnr
           AND a~tplst IN @s_tplst
           AND a~route IN @s_route
           AND a~vsart IN @s_vsart.

        ELSE.

          REFRESH : gt_data2.
          SELECT a~mandt  a~kschl  a~shtyp a~tdlnr a~tplst a~route a~vsart b~bezei
         a~kfrst a~datbi a~datab a~kbstat a~knumh c~kbetr c~konwa c~loevm_ko  d~bezei AS bezei0  e~name1 f~bezei AS bezei1
         FROM a620  AS a
         INNER JOIN tvtkt AS b ON b~shtyp = a~shtyp
         INNER JOIN konp AS c ON c~knumh = a~knumh AND c~kschl = a~kschl
         INNER JOIN t173t AS d ON d~vsart = a~vsart
         INNER JOIN lfa1 AS e ON e~lifnr = a~tdlnr
         INNER JOIN tvrot AS f ON f~route = a~route
          INTO TABLE gt_data2
           WHERE a~kschl = 'ZF00'
          AND a~kschl IN s_kschl
          AND b~spras = sy-langu
          AND d~spras = sy-langu
          AND f~spras = sy-langu
          AND a~shtyp IN s_shtyp
          AND a~tdlnr IN s_tdlnr
          AND a~tplst IN s_tplst
          AND a~route IN s_route
          AND a~vsart IN s_vsart
          AND a~datab LE lv_date
          AND a~datbi GE lv_date.

        ENDIF.

        IF gt_data2 IS NOT INITIAL.

          gt_final[] = VALUE #( FOR gs_data2 IN gt_data2
                              ( mandt    = gs_data2-mandt
                                kschl    = gs_data2-kschl
                                shtyp    = gs_data2-shtyp
                                shdes    = gs_data2-bezei0
                                tdlnr    = gs_data2-tdlnr
                                tplst    = gs_data2-tplst
                                route    = gs_data2-route
                                rodes    = gs_data2-bezei1
                                vsart    = gs_data2-vsart
                                bezei    = gs_data2-bezei
                                kfrst    = gs_data2-kfrst
                                datbi    = gs_data2-datbi
                                datab    = gs_data2-datab
                                kbstat   = gs_data2-kbstat
                                knumh    = gs_data2-knumh
                                kbetr    = gs_data2-kbetr
                                konwa    = gs_data2-konwa
                                loevm_ko = gs_data2-loevm_ko
                                name1    = gs_data2-name1 ) ).
          ELSE.
          MESSAGE text-040 TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.

        ENDIF.

        WHEN OTHERS.
          MESSAGE text-101 TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.

    ENDCASE.

  ENDIF.
ENDFORM.


FORM fill_fieldcatlog.

  CASE s_kschl-low.

    WHEN 'ZM01' OR 'ZM13'.

      gv_tabname = 'GT_FINAL' ##NO_TEXT.

      gs_fcat-fieldname = 'KSCHL'.
      gs_fcat-reptext   = text-010.        "Condition type.
      gs_fcat-outputlen = 12.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'SHTYP'.
      gs_fcat-reptext   = text-011.        "Shipment Type.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'BEZEI'.
      gs_fcat-reptext   = text-019.        "Shipment Type- Description.
      gs_fcat-outputlen = 25.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'VSART'.
      gs_fcat-reptext   = text-018.        "Shipping Type.
      gs_fcat-outputlen = 10.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'SHDES'.
      gs_fcat-reptext   = text-012.        "Shipping Type - Description.
      gs_fcat-outputlen = 25.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'TDLNR'.
      gs_fcat-reptext   = text-013.        "Number of forwarding agent in the shipment stage.
      gs_fcat-outputlen = 25.
      gs_fcat-no_zero   = 'X'.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'NAME1'.
      gs_fcat-reptext   = text-014.        "Vendor Description.
      gs_fcat-outputlen = 40.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'WERKS'.
      gs_fcat-reptext   = text-015.        "Plant.
      gs_fcat-outputlen = 8.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'ROUTE'.
      gs_fcat-reptext   = text-017.        "Route Code.
      gs_fcat-outputlen = 10.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'RODES'.
      gs_fcat-reptext   = text-030.        "Route Description.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KFRST'.
      gs_fcat-reptext   = text-020.        "Release Status.
      gs_fcat-outputlen = 12.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'DATAB'.
      gs_fcat-reptext   = text-021.        "Validity Start date of the condition record.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'DATBI'.
      gs_fcat-reptext   = text-022.        "Validity end date of the condition record.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KBSTAT'.
      gs_fcat-reptext   = text-023.        "Processing Status for Conditions.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.


      gs_fcat-fieldname = 'KNUMH'.
      gs_fcat-reptext   = text-024.        "Condition Record Number.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KBETR'.
      gs_fcat-reptext   = text-025.        "Rate.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KONWA'.
      gs_fcat-reptext   = text-026.        "Rate Unit
      gs_fcat-outputlen = 7.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.


      gs_fcat-fieldname = 'LOEVM_KO'.
      gs_fcat-reptext   = text-027.        "Deletion Indicator.
      gs_fcat-outputlen = 2.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

    WHEN 'ZF00'.

      gv_tabname = 'GT_FINAL' ##NO_TEXT.


      gs_fcat-fieldname = 'KSCHL'.
      gs_fcat-reptext   = text-010.        "Condition type.
      gs_fcat-outputlen = 12.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'SHTYP'.
      gs_fcat-reptext   = text-011.        "Shipment Type.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'SHDES'.
      gs_fcat-reptext   = text-012.        "Shipment Type - Description.
      gs_fcat-outputlen = 25.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'VSART'.
      gs_fcat-reptext   = text-018.        "Shipping Type.
      gs_fcat-outputlen = 10.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'BEZEI'.
      gs_fcat-reptext   = text-019.        "Shipping type - Description.
      gs_fcat-outputlen = 25.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'TDLNR'.
      gs_fcat-reptext   = text-013.        "Number of forwarding agent in the shipment stage.
      gs_fcat-outputlen = 25.
      gs_fcat-no_zero   = 'X'.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'NAME1'.
      gs_fcat-reptext   = text-014.        "Vendor Description.
      gs_fcat-outputlen = 40.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.


      gs_fcat-fieldname = 'TPLST'.
      gs_fcat-reptext   = text-016.        "Transportation Planning Point.
      gs_fcat-outputlen = 20.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'ROUTE'.
      gs_fcat-reptext   = text-017.        "Route Code.
      gs_fcat-outputlen = 10.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'RODES'.
      gs_fcat-reptext   = text-030.        "Route Description.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KFRST'.
      gs_fcat-reptext   = text-020.        "Release Status.
      gs_fcat-outputlen = 12.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'DATAB'.
      gs_fcat-reptext   = text-021.        "Validity Start date of the condition record.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.


      gs_fcat-fieldname = 'DATBI'.
      gs_fcat-reptext   = text-022.        "Validity end date of the condition record.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KBSTAT'.
      gs_fcat-reptext   = text-023.        "Processing Status for Conditions.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.


      gs_fcat-fieldname = 'KNUMH'.
      gs_fcat-reptext   = text-024.        "Condition Record Number.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KBETR'.
      gs_fcat-reptext   = text-025.        "Scale Value.
      gs_fcat-outputlen = 15.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KONWA'.
      gs_fcat-reptext   = text-026.        "Condition Scale unit of measure.
      gs_fcat-outputlen = 7.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.


      gs_fcat-fieldname = 'LOEVM_KO'.
      gs_fcat-reptext   = text-027.        "Deletion Indicator.
      gs_fcat-outputlen = 2.
      gs_fcat-tabname   = gv_tabname.
      APPEND gs_fcat TO gt_fcat.
      CLEAR gs_fcat.

  ENDCASE.


ENDFORM.

FORM display_alv.

  gs_layout = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fcat
    TABLES
      t_outtab                 = gt_final.



ENDFORM.

FORM set_pf_status USING rt_extab TYPE slis_t_extab ##CALLED ##NEEDED.
  SET PF-STATUS 'ZSTANDARD'.
ENDFORM.
