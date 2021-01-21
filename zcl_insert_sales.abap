// Class to Add/Modify records into the table (SAP CLOUD Trail - ABAP ) // 

CLASS zcl_insert_sales DEFINITION
 PUBLIC
 FINAL
 CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_insert_sales IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA:it_sales TYPE TABLE OF zsales_order.
    DATA: lv_sales_no(10) TYPE n.
    DATA: lv_customer(6) TYPE n.

*   fill internal Sales order table
    DO 10 TIMES.
      lv_sales_no = lv_sales_no + 1.
      lv_customer = lv_customer + 1 .
      CONDENSE lv_customer  NO-GAPS.
      APPEND VALUE #( sales_order = lv_sales_no
                      customer = lv_customer
                      created_by = sy-uname
                      created_on = '20191008'
                      total_sales = lv_sales_no * 10
                      currency_code = 'USD' ) TO it_sales.
    ENDDO.

    DELETE FROM zsales_order .
*
*   insert the new table entries
    INSERT zsales_order FROM TABLE @it_sales.

*   check the result
    SELECT * FROM zsales_order INTO TABLE @it_sales.
    IF sy-subrc EQ 0.
      out->write( 'Sales order data inserted successfully!').
    ENDIF.

  ENDMETHOD.
ENDCLASS.
