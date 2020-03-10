*Creating Classes and Subclasses for better understanding the concepts of Inheritance.

*&---------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------------------------*
*&                            CLASS lcl_vehicle DEFINITON                                *
*&---------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------------------------*


CLASS lcl_vehicle DEFINITION.

PUBLIC SECTION.

  METHODS gofaster.

  METHODS writespeed.

PROTECTED SECTION.

  DATA : speed TYPE i.

PRIVATE SECTION.

ENDCLASS.

*&---------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------------------------*
*&                            CLASS lcl_vehicle IMPLEMENTATION                           *
*&---------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------------------------*


CLASS lcl_vehicle IMPLEMENTATION.

METHOD gofaster.

DATA : tmpspeed TYPE i.
tmpspeed = speed + 1.
speed = tmpspeed.
CLEAR tmpspeed.

ENDMETHOD.

METHOD writespeed.

WRITE : / 'The Vehicle Speed is' , speed.

ENDMETHOD.

ENDCLASS.

