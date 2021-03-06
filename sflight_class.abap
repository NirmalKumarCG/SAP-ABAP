*This a ABAP OO program to display flight data based on certain general guidelines.

REPORT  z003_flights_class.

*General Guidelines

*The Class will have 1 attribute - an internal table that replicates all the data found in SPFLI.
*All access to the data must be through object Methods so the attribute should be private.
*Create methods to access all the data in the internal table.
*Create a constructor to fill the internal table from the Database.
*
*Methods:
*constructor - Load the records from SPFLI into the internal table attribute.
*showAllData - write out to the screen all of the data
*showConnidData - take in a connection ID and write out the following:
*                 City From, Country From, City To, Country to, Flight Time (in minutes), distance in KM/miles
*                 If no connection ID exists write out “No record found matching connid:” followed by the connection ID value passed in.
*numFlightsTo - take in an airport code (FRA, SFC etc..) and return the number of flights that travel to that airport.
*getConnid - this method will take in 2 airport codes, 1 representing the departure airport
*            and 1 representing the arrival airport.
*            Return the connection ID of the flight that matches.
*            If the parameter combination has no matching records return 0.
*getFlightTime - accepts a connection ID and returns the number of minutes in flight time.
*getAllConnectionFacts - take in a connection ID and return a structure that contains all the information
*                        about that connection. This structure should correspond to a row in the table.


CLASS flights DEFINITION.
  PUBLIC SECTION.
*    TYPES: fl_tab_type type STANDARD TABLE OF SPFLI.
*    DATA: flight_table TYPE fl_tab_type READ-ONLY.   " Change this to private without the READ-ONLY.

    METHODS constructor.        " Load the records from SPFLI into the internal table attribute.

    METHODS showAllData.

    METHODS showConnidData
      IMPORTING cid TYPE S_CONN_ID.

    METHODS numFlightsTo
      IMPORTING city TYPE S_TO_CITY
      RETURNING VALUE(numflights) TYPE i.

    METHODS: getConnid
      IMPORTING cityfrom TYPE S_FROM_CIT
                cityto TYPE S_TO_CITY
      RETURNING VALUE(connid) TYPE S_CONN_ID.

    METHODS: getFlightTime
      IMPORTING cid TYPE S_CONN_ID
      RETURNING VALUE(minutes) TYPE i.

    METHODS: getAllConnectionFacts
      IMPORTING cid TYPE S_CONN_ID
      RETURNING VALUE(conn) TYPE SPFLI.


  PRIVATE SECTION.
    TYPES: fl_tab_type type STANDARD TABLE OF SPFLI.
    DATA: flight_table TYPE fl_tab_type.

ENDCLASS.                    "flights DEFINITION


CLASS flights IMPLEMENTATION.

  METHOD constructor.
    SELECT * FROM spfli INTO TABLE flight_table.
    if sy-subrc <> 0.
      WRITE: 'There was a problem reading the SPFLI DB Table'.
    ENDIF.
  ENDMETHOD.

  METHOD showAllData.
    DATA: wa TYPE SPFLI.

    LOOP AT flight_table into wa.
      WRITE: / wa-carrid, 5 wa-connid, 10 wa-countryfr, 14 wa-cityfrom, 36 wa-airpfrom, 40 wa-countryto,
      44 wa-cityto, 66 wa-airpto, 69 wa-fltime, 77 wa-deptime, 87 wa-arrtime, 97 wa-distance, 107 wa-distid,
      110 wa-fltype, 115 wa-period.
    ENDLOOP.
    uline.

  ENDMETHOD.

  METHOD showConnidData.
    DATA: wa type spfli.

    READ TABLE flight_table INTO wa with key connid = cid.
    IF sy-subrc = 0.
      WRITE: / wa-cityfrom, 22 wa-airpfrom, 27 wa-countryto, 49 wa-fltime, 54 wa-distance.
    ELSE.
      WRITE: 'No record found matching connid: ', cid.
    ENDIF.

  ENDMETHOD.

  METHOD numFlightsTo.
    LOOP at flight_table TRANSPORTING NO FIELDS WHERE AIRPTO = city.
      numflights = numflights + 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD getConnid.
    DATA: wa TYPE spfli.

    connid = 0.
    READ TABLE flight_table INTO wa WITH KEY AIRPFROM = cityfrom
                                             AIRPTO   = cityto.
    connid = wa-connid.

  ENDMETHOD.

  METHOD getFlightTime.
    DATA: wa TYPE SPFLI.

    minutes = 0.
    READ TABLE flight_table INTO wa WITH KEY connid = cid.
    minutes = wa-FLTIME.

  ENDMETHOD.

  METHOD getAllConnectionFacts.

    CLEAR conn.
    READ TABLE flight_table INTO conn WITH KEY connid = cid.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  data: temp type i,
        wa TYPE spfli.

  DATA my_flight_class TYPE REF TO FLIGHTS.
  CREATE OBJECT my_flight_class.

  my_flight_class->showAllData( ).

  my_flight_class->showConnidData( 1984 ).
  uline.

  temp = my_flight_class->numFlightsTo( 'JFK').
  WRITE temp.
  uline.

  temp = my_flight_class->getConnid( cityfrom = 'JFK' cityto = 'FRA').
  WRITE temp.
  uline.

  temp = my_flight_class->getFlightTime( 3504 ).
  WRITE temp.
  uline.

  wa = my_flight_class->getAllConnectionFacts( 3504 ).
  WRITE: / wa-carrid, 5 wa-connid, 10 wa-countryfr, 14 wa-cityfrom, 36 wa-airpfrom, 40 wa-countryto,
  44 wa-cityto, 66 wa-airpto, 69 wa-fltime, 77 wa-deptime, 87 wa-arrtime, 97 wa-distance, 107 wa-distid,
  110 wa-fltype, 115 wa-period.
