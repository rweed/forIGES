
! This file is part of the forIGES library
! https://github.com/rweed/forIGES

! Copyright (C) 2024 Richard Weed.
! All rights reserved.

! Redistribution and use in source and binary forms, with or without 
! modification, are permitted provided that the following conditions are met:

! 1. Redistributions of source code, in whole or in part, must retain the  
! above copyright notice, this list of conditions and the following 
! disclaimer.

! 2. Redistributions in binary form, in whole or in part, must reproduce the 
! above copyright notice, this list of conditions and the following disclaimer 
! in the documentation and/or other materials provided with the distribution.

! 3. The names of the contributors may not be used to endorse or promote from 
! products derived from this software without specific prior written 
! permission.

! 4. Redistribution of this source code, including any modifications, may 
! not be intentionally obfuscated.

! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
! IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
! EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
! OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
! WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
! OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
! ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


Module IGES_params

!! Parameters, enumerators, and derived types used by forIGES routines

!! Author  : Richard Weed

!! Version : 0.1 (Beta release 1)
!! Date    : August 18, 2024

  USE ISO_FORTRAN_ENV, stdin=>input_unit, stdout=>output_unit,                &
                       stderr=>error_unit, SP=>REAL32, DP=>REAL64

  USE ISO_C_BINDING, ONLY: C_NULL_CHAR
 
  Implicit NONE

#ifdef REAL4
  Integer, Parameter :: WP = SP 
  !! Working precision set to single-precision 
#else
  Integer, Parameter :: WP = DP 
  !! Working precision set to double-precision 
#endif

! Define some enumerators for color no., line font pattern, drafting standard
! flag, and units flag per tables in IGES 5.3 specification 

  Enum, Bind(C)
    Enumerator :: NO_COLOR=0, BLACK, RED, GREEN, BLUE, YELLOW, MAGENTA, CYAN, &
                  WHITE
    !! Color no. enumerator
  End Enum


  Enum, Bind(C)
    Enumerator :: NO_FONT_PATTERN=0, SOLID, DASHED, PHANTOM, CENTERLINE,      &
                  DOTTED
    !! Line Font Pattern enumerator
  End Enum


  Enum, Bind(C)

    Enumerator :: NO_STANDARD=0, ISO, AFNOR, ANSI, BSI, CSA, DIN, JIS
    !! Drafting Standard Flag enumerator

  End Enum

  Enum, Bind(C)
   
    Enumerator :: INCHES=1, MILLIMETERS, PARAM15, FEET, MILES, METERS,        &
                  KILOMETERS, MILS, MICRONS, CENTIMETERS, MICROINCHES
    !! Units Flag enumerator

  End Enum

! Default values for various G section entries and field separator values

  Integer,      Parameter :: IGES_VERSION = 11 
  !! Default IGES version number
  Integer,      Parameter :: C_ENUM = KIND(INCHES)
  !! Make a C_ENUM parameter for enumerator types
  Character(7), Parameter :: DEFAULT_REAL_FORMAT = "(g0.10) "
  !! Define a default format for outputing REAL data in IGES files 
  Character(7)            :: REAL_FORMAT         = DEFAULT_REAL_FORMAT
  !! Initialize REAL_FORMAT to default
  Character(5), Parameter :: FORIGES_VERSION     = "V0.1 "
  !! Current forIGES version
  Character(1), Parameter :: default_field_separator   = ","
  !! Default field separator
  Character(1), Parameter :: default_record_terminator = ";"
  !! Default record terminator 

  Character(1), Parameter :: aSpace    = " "
  !! Define a space character
  Character(2), Save :: nullField = ",,"
  !! Define a null field (Repeated commas)
  Character(1), Save :: field_separator   = default_field_separator
  !! Set field separator to default
  Character(1), Save :: record_terminator = default_record_terminator
  !! Set record terminator to default


  Type :: string_t
!! Type string_t is used to define a ragged array of strings
    Character(:), Allocatable :: str 
  End Type

  Type :: records_t
  !! Type records_t holds an array of 80 character records
    Character(80), Allocatable :: recs(:)
  End Type

  Type :: intArray_t
!! Type intArray_t is used to define a ragged array of integers
    Integer, Allocatable :: vals(:)
  End Type

  Integer, Parameter, Dimension(38) :: forIGES_entities = &
!! forIGES_entities contains the entity numbers of all the entities supported by
!! forIGES

    [   0, & !! Null  
      100, & !! Circular  arc
      102, & !! Composite curve
      104, & !! Conic arc 
      106, & !! Copious data (forms 1-3, 11-13, 63)
      108, & !! Plane
      110, & !! Line
      112, & !! Parametric Spline Curve
      114, & !! Parametric Spline Surface 
      116, & !! Point
      118, & !! Ruled Surface
      120, & !! Surface of Revolution
      122, & !! Tabulated Cylinder
      123, & !! Direction
      124, & !! Transformation Matrix
      126, & !! Rational B-spline curve
      128, & !! Rational B-spline surface
      130, & !! Offset Curve
      140, & !! Offset Surface
      141, & !! Boundary 
      142, & !! Curve on a Parametric Surface
      143, & !! Bounded Surface
      144, & !! Trimmed (parametric) surface
      190, & !! Plane Surface   
      192, & !! Right Circular Cylinder Surface
      194, & !! Right Circular Conical Surface
      196, & !! Spherical Surface
      198, & !! Toroidal Surface
      212, & !! General Note
      308, & !! Subfigure Definition
      314, & !! Color Definition
      402, & !! Associativity Instance 
      406, & !! Property (forms 1 and 15) 
      408, & !! Singular Subfigure instance 
      502, & !! Vertex list
      504, & !! Edge
      508, & !! Loop
      510  & !! Face
    ]

! Might add these in the future since they are supported by libIGES

!      154,    & !! Right Cicular Cylinder
!      164,    & !! Solid of Linear Extrusion
!      180,    & !! Boolean Tree
!      186,    & !! Manifold Solid BREP Object
!      514     & !! Shell (Form 1 only)

Contains

  Subroutine setRealFormat(num_decimals)
!! Sets number of decimal places to be used in output of REAL values and
!! modify global REAL_FORMAT string from default value

    Integer, Optional, Intent(IN) :: num_decimals
    !!  Number of decimal points to be used for REAL number output in IGES files

    Integer      :: nd
    Character(2) :: ndc

    If (WP==SP) Then
      nd = 6
    ElseIf(WP==DP) Then
      nd = 15
    Else
      nd = 9 
    End If

    If (PRESENT(num_decimals)) nd = num_decimals

    nd = nd + 1

    ndc = "  "   
    Write(ndc,'(i0)') nd
    REAL_FORMAT = "(g0."//TRIM(ADJUSTL(ndc))//")"

  End Subroutine setRealFormat
 
End Module IGES_params 
