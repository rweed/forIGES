! This file is part of the forIGES libary
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
Module IGES_Gsection
!! IGES_Gsection defines classes for reading an IGES file G section, outputing
!! the contents of the resulting derived types, creating a Gsection class from 
!! user input and converting a Gsection class type to 80 character records for
!! inclusion in a user created IGES file.

!! Author  : Richard Weed

!! Version : 0.1 (Beta release 1)
!! Date    : August 18, 2024

  USE IGES_params
  USE IGES_utils, ONLY: getUnitName, fieldsToRecs

  Implicit NONE

  PRIVATE

  Type :: Gsection_t
!! Gsection_t is a class that holds all the pertinent G section items defined
!! in the IGES 5.3 manual along with some type bound methods for manipulating
!! the components

    Character(1)              :: pardel = default_field_separator
    !! Parameter delimiter character (Default is a comma (,)
    Character(1)              :: recdel = default_record_terminator
    !! Record delimiter character (Default is a semi-colon(;)
    Character(:), Allocatable :: sender_id
    !! Product id from sending systme
    Character(:), Allocatable :: filename
    !! This IGES filename
    Character(:), Allocatable :: native_id
    !! Native system id
    Character(:), Allocatable :: preproc_vers
    !! Preprocessor version
    Integer                   :: integer_bits   = BIT_SIZE(1)
    !! Number of binary bits for an integer
    Integer                   :: real32_max_e10 = 38
    !! Maximum power representable by a single-precison floating point number
    Integer                   :: real32_digits  = 6
    !! Maximum number of significant digits for a single-precision floating
    !! point number
    Integer                   :: real64_max_e10 = 380
    !! Maximum power representable by a double-precison floating point number
    Integer                   :: real64_digits  = 15
    !! Maximum number of significant digits for a single-precision floating
    !! point number

    Character(:), Allocatable :: receive_id
    !! Product id for receiving system
    Real(WP)                  :: model_space_scale = 1.0_WP
    !! Model space scale
    Integer                   :: units_flag = 1
    !! Units flag (default is inches)
    Character(:), Allocatable :: units_name
    !! Units name (default is "INCH")
    Integer                   :: max_weight_grads = 1
    !! Maximum number of line weight gradations
    Real(WP)                  :: max_line_width   = 0.02_WP
    !! Width of maximum line weight in units
    Character(:), Allocatable :: file_time
    !! 15 character date and time of exchange file generation. Format is
    !! 15HYYYYMMDD.HHNNSS
    Real(WP)                  :: min_res   = 0.000001_WP
    !! Minimum user resolution for model
    Real(WP)                  :: max_coord = 0.0_WP
    !! Approximate maximum coordinate value of model
    Character(:), Allocatable :: author
    !! Name of Author
    Character(:), Allocatable :: organization
    !! Authors organization
    Integer                   :: IGES_vers = 11
    !! Flag value for the version of the IGES standard this file complies with
    Integer                   :: drafting_standard = 0
    !! Flag value for Drafting Standard this files data
    Character(:), Allocatable :: model_time
    !! Date and time of this models creation or modificattion
    Character(:), Allocatable :: app_protocol
    !! Descriptor for application protocol, subset MIL standard etc.

  Contains

    Procedure :: dealloc   => deallocGsection
    !! Deallocates a Gsection
    Procedure :: output    => outputGsection
    !! Outputs a Gsection to a specified unit
    Procedure :: init      => initGsection
    !! Initializes a Gsection with user specified data
    Procedure :: getGS     => getGsection
    !! Creates a G section class from IGES file records
    Procedure :: makeGrecs
    !! Makes IGES file records from a G section class

  End Type

  PUBLIC :: Gsection_t

Contains

  Subroutine deallocGsection(this)
!! Type bound procedure for deallocating and resetting Gsection_t components

    Class(Gsection_t), Intent(INOUT) :: this

    this%pardel = default_field_separator
    this%recdel = default_record_terminator
    If(ALLOCATED(this%sender_id))    DEALLOCATE(this%sender_id)
    If(ALLOCATED(this%filename))     DEALLOCATE(this%filename)
    If(ALLOCATED(this%native_id))    DEALLOCATE(this%native_id)
    If(ALLOCATED(this%preproc_vers)) DEALLOCATE(this%preproc_vers)

    this%integer_bits = 32
    this%real32_max_e10 = 38
    this%real32_digits = 6
    this%real64_max_e10 = 380
    this%real64_digits = 15

    If(ALLOCATED(this%receive_id))   DEALLOCATE(this%receive_id)
    this%model_space_scale = 0.0_WP
    this%units_flag = 1
    If(ALLOCATED(this%units_name))   DEALLOCATE(this%units_name)
    this%max_weight_grads = 1
    this%max_line_width = 0.02_WP
    If(ALLOCATED(this%file_time))    DEALLOCATE(this%file_time)
    this%min_res = 0.000001_WP
    this%max_coord = 0.0_WP
    If(ALLOCATED(this%author))       DEALLOCATE(this%author)
    If(ALLOCATED(this%organization)) DEALLOCATE(this%organization)
    this%IGES_vers = 11
    this%drafting_standard = 0
    If(ALLOCATED(this%model_time))   DEALLOCATE(this%model_time)
    If(ALLOCATED(this%app_protocol)) DEALLOCATE(this%app_protocol)

  End Subroutine deallocGsection

  Subroutine initGsection(this,                                               &
                    pardel,                                                   &
                    recdel,                                                   &
                    sender_id,                                                &
                    filename,                                                 &
                    native_id,                                                &
                    preproc_vers,                                             &
                    integer_bits,                                             &
                    real32_max_e10,                                           &
                    real32_digits,                                            &
                    real64_max_e10,                                           &
                    real64_digits,                                            &
                    receive_id,                                               &
                    model_space_scale,                                        &
                    units_flag,                                               &
                    units_name,                                               &
                    max_weight_grads,                                         &
                    max_line_width,                                           &
                    file_time,                                                &
                    min_res,                                                  &
                    max_coord,                                                &
                    author,                                                   &
                    organization,                                             &
                    IGES_vers,                                                &
                    drafting_standard,                                        &
                    model_time,                                               &
                    app_protocol)
!! InitGsection is a type bound procedure used to initialize selected 
!! components of a Gsection_t class. The default values defined for the
!! components are used if not overridden by an InitGsection input

    Class(Gsection_t),      Intent(INOUT) :: this
    !! This Gsection_t passed argument. 
    !! All other dummy arguments correspond to the components of the Gsection_t
    !! derived type
    Character(1), Optional, Intent(IN) :: pardel
    Character(1), Optional, Intent(IN) :: recdel
    Character(*), Optional, Intent(IN) :: sender_id
    Character(*), Optional, Intent(IN) :: filename
    Character(*), Optional, Intent(IN) :: native_id
    Character(*), Optional, Intent(IN) :: preproc_vers

    Integer,      Optional, Intent(IN) :: integer_bits
    Integer,      Optional, Intent(IN) :: real32_max_e10
    Integer,      Optional, Intent(IN) :: real32_digits
    Integer,      Optional, Intent(IN) :: real64_max_e10
    Integer,      Optional, Intent(IN) :: real64_digits

    Character(*), Optional, Intent(IN) :: receive_id
    Real(WP),     Optional, Intent(IN) :: model_space_scale
    Integer,      Optional, Intent(IN) :: units_flag
    Character(*), Optional, Intent(IN) :: units_name
    Integer,      Optional, Intent(IN) :: max_weight_grads
    Real(WP),     Optional, Intent(IN) :: max_line_width
    Character(*), Optional, Intent(IN) :: file_time
    Real(WP),     Optional, Intent(IN) :: min_res
    Real(WP),     Optional, Intent(IN) :: max_coord
    Character(*), Optional, Intent(IN) :: author
    Character(*), Optional, Intent(IN) :: organization
    Integer,      Optional, Intent(IN) :: IGES_vers
    Integer,      Optional, Intent(IN) :: drafting_standard
    Character(*), Optional, Intent(IN) :: model_time
    Character(*), Optional, Intent(IN) :: app_protocol

    Integer(C_ENUM) :: iflag
    Integer         :: id
    Character(8)    :: date
    Character(10)   :: time
    Character(15)   :: datetime

    Call DATE_AND_TIME(date, time)
    datetime = date//"."//time(1:6)

    this%pardel = ","
    this%recdel = ";"

    if (.NOT. ALLOCATED(this%sender_id))      this%sender_id    = "forIGES"
    If (.NOT. ALLOCATED(this%filename))       this%filename     = "forIGES.igs"
    If (.NOT. ALLOCATED(this%native_id))                                    &
             this%native_id    = "forIGES "// FORIGES_VERSION
   If (.NOT. ALLOCATED(this%preproc_vers))   this%preproc_vers = "Unknown"
    If (.NOT. ALLOCATED(this%receive_id))     this%receive_id   = "forIGES"
    If (.NOT. ALLOCATED(this%units_name))     this%units_name   = "INCH"
    If (.NOT. ALLOCATED(this%author))         this%author       = "forIGES"
    If (.NOT. ALLOCATED(this%organization))   this%organization = "forIGES"
    If (.NOT. ALLOCATED(this%file_time))      this%file_time    = datetime
    If (.NOT. ALLOCATED(this%model_time))     this%model_time   = datetime
    If (.NOT. ALLOCATED(this%app_protocol))   this%app_protocol = ","

    If (PRESENT(pardel))     this%pardel    = pardel
    If (PRESENT(recdel))     this%recdel    = recdel
    If (PRESENT(sender_id))  this%sender_id = sender_id
    If (PRESENT(filename))   this%filename  = TRIM(ADJUSTL(filename))
    If (.NOT.PRESENT(sender_id) .AND. PRESENT(filename)) Then
      id = INDEX(this%filename,".") - 1
      if (id > 0) Then
        this%sender_id = this%filename(1:id)
      Else
        this%sender_id = this%filename
      End If
    End If
    If (PRESENT(native_id))         this%native_id         = native_id
    If (PRESENT(preproc_vers))      this%preproc_vers      = preproc_vers
    If (PRESENT(integer_bits))      this%integer_bits      = integer_bits
    If (PRESENT(real32_max_e10))    this%real32_max_e10    = real32_max_e10
    If (PRESENT(real32_digits))     this%real32_digits     = real32_digits
    If (PRESENT(real64_max_e10))    this%real64_max_e10    = real64_max_e10
    If (PRESENT(real64_digits))     this%real64_digits     = real64_digits
    If (PRESENT(receive_id))   Then
      this%receive_id  = receive_id
    Else
      this%receive_id  = this%sender_id
    End If
    If (PRESENT(model_space_scale)) this%model_space_scale = model_space_scale
    If (PRESENT(units_flag))       Then
      this%units_flag        = units_flag
      If (.NOT. PRESENT(units_name)) Then
        iflag = INT(units_flag, C_ENUM)
        Call getUnitName(iflag, this%units_name)
      End If
    End If
    If (PRESENT(units_name))        this%units_name        = units_name
    If (PRESENT(max_weight_grads))  this%max_weight_grads  = max_weight_grads
    If (PRESENT(max_line_width))    this%max_line_width    = max_line_width
    If (PRESENT(file_time))         this%file_time         = file_time
    If (PRESENT(min_res))           this%min_res           = min_res
    If (PRESENT(max_coord))         this%max_coord         = max_coord
    If (PRESENT(author))            this%author            = author
    If (PRESENT(organization))      this%organization      = organization
    If (PRESENT(IGES_vers))         this%IGES_vers         = IGES_vers
    If (PRESENT(drafting_standard)) this%drafting_standard = drafting_standard
    If (PRESENT(model_time))        this%model_time        = model_time
    If (PRESENT(app_protocol))      this%app_protocol      = app_protocol

  End Subroutine initGsection

  Subroutine getGsection(this, Grecords)
!! Translate an array of 80 character Grecords into the components of a
!! Gsection_t class. Grecords are assumed to be defined from an input 
!! IGES file

    Class(Gsection_t), Intent(INOUT) :: this
    !! This Gsection_t passed argument
    Character(80),     Intent(IN)    :: Grecords(:)
    !! Array of 80 character records input from an IGES file that are used
    !! to define Gsection_t component values 

    Integer                   :: i, glen, ih, igs, ifield, is, ie, numGrecs 
    Integer                   :: fstart(24), fend(24)
    Character(:), Allocatable :: gtemp
    Character(:), Allocatable :: gtemp2

    Type(string_t) :: fieldBuf(26)
    Character(80)  :: sender_id
    Character(80)  :: filename
    Character(80)  :: native_id
    Character(80)  :: preproc_vers
    Character(80)  :: receive_id
    Character(80)  :: units_name
    Character(15)  :: file_time
    Character(80)  :: author
    Character(80)  :: organization
    Character(15)  :: model_time
    Character(80)  :: app_protocol
    Integer        :: integer_bits
    Integer        :: real32_max_e10
    Integer        :: real32_digits
    Integer        :: real64_max_e10
    Integer        :: real64_digits
    Real(WP)       :: model_space_scale
    Integer        :: units_flag
    Integer        :: max_weight_grads
    Real(WP)       :: max_line_width
    Real(WP)       :: min_res
    Real(WP)       :: max_coord
    Integer        :: IGES_vers
    Integer        :: drafting_standard

! Merge individual records into one string

    numGrecs = SIZE(Grecords)
    Gtemp = TRIM(ADJUSTL(Grecords(1)(1:72))) 
    Do i=2, numGrecs 
      gtemp = gtemp // TRIM(ADJUSTL(Grecords(i)(1:72)))
    End Do 

! Check for non-default field separators and record_terminator

    glen = LEN_TRIM(gtemp)
    igs  = 3
    If (gtemp(1:2) /= ",,") Then
      If (SCAN(gtemp(1:2), '1Hh') > 0) Then
        field_separator = gtemp(3:3)
        If (gtemp(4:4) == field_separator .AND.                               &
            gtemp(5:5) == field_separator) Then
            record_terminator = ";"
            igs = 6 
        Else If (SCAN(gtemp(5:6),'1Hh') > 0) Then
          record_terminator = gtemp(7:7)
          igs = 9
        End If
      Else If(gtemp(1:3) == ',' .AND. (SCAN(gtemp(2:3), "1Hh") > 0)) Then
        record_terminator = gtemp(4:4)
        igs = 6
      End If
    End If

! Set record_termination in gtemp to blank
      
    Do i =glen, 1, -1
      If (gtemp(i:i) == record_terminator) Then
        gtemp(i:i) = "/"
        EXIT
      End if
    End Do

! Reset field separator to a comma so we can use Fortran list directed input
! from an internal file (gtemp)

    this%pardel = field_separator
    this%recdel = record_terminator
    
    sender_id    = REPEAT(" ",80)
    filename     = REPEAT(" ",80)
    native_id    = REPEAT(" ",80)
    preproc_vers = REPEAT(" ",80)
    receive_id   = REPEAT(" ",80)
    units_name   = REPEAT(" ",80)
    file_time    = REPEAT(" ",15)
    author       = REPEAT(" ",80)
    organization = REPEAT(" ",80)
    model_time   = REPEAT(" ",15)
    app_protocol = REPEAT(" ",80)

    integer_bits      = 32 
    real32_max_e10    = 38 
    real32_digits     = 6 
    real64_max_e10    = 308 
    real64_digits     = 15 
    model_space_scale = 1.0_WP
    units_flag        = 1
    max_weight_grads  = 0
    max_line_width    = 0.0_WP
    min_res           = 0.0_WP
    max_coord         = 0.0_WP
    IGES_vers         = 5
    drafting_standard = 0

! Trim gtemp into gtemp2 and then get starting and ending index
! of each of the remaining 24 data fields

    gtemp2 = TRIM(ADJUSTL(gtemp(igs:LEN_TRIM(gtemp))))
    fstart(1) = 1
    ifield = 0
    Do i = 1, LEN_TRIM(gtemp2)
      If (gtemp2(i:i) == field_separator .OR. gtemp2(i:i) == record_terminator) Then
        ifield = ifield+1 
        fend(ifield) = i-1
      End If
    End Do
    Do i=2,24
      fstart(i) = fend(i-1)+2
    End Do
    fend(24) = LEN_TRIM(gtemp2) - 1

    Do i=1,24
      is = fstart(i)
      ie = fend(i)
      fieldBuf(i)%str = gtemp2(is:ie)
    End Do

    Read(fieldBuf(1)%str,'(A)') sender_id 
    Read(fieldBuf(2)%str,'(A)') filename 
    Read(fieldBuf(3)%str,'(A)') native_id 
    Read(fieldBuf(4)%str,'(A)') preproc_vers 
    Read(fieldBuf(5)%str,*) integer_bits
    Read(fieldBuf(6)%str,*) real32_max_e10
    Read(fieldBuf(7)%str,*) real32_digits
    Read(fieldBuf(8)%str,*) real64_max_e10
    Read(fieldBuf(9)%str,*) real64_digits
    Read(fieldBuf(10)%str,'(A)') receive_id
    Read(fieldBuf(11)%str,*) model_space_scale
    Read(fieldBuf(12)%str,*) units_flag 
    Read(fieldBuf(13)%str,'(A)') units_name
    Read(fieldBuf(14)%str,*) max_weight_grads 
    Read(fieldBuf(15)%str,*) max_line_width
    Read(fieldBuf(16)%str,'(A)') file_time 
    Read(fieldBuf(17)%str,*) min_res
    Read(fieldBuf(18)%str,*) max_coord
    Read(fieldBuf(19)%str,'(A)') author 
    Read(fieldBuf(20)%str,'(A)') organization 
    Read(fieldBuf(21)%str,*) IGES_vers 
    Read(fieldBuf(22)%str,*) drafting_standard 
    Read(fieldBuf(23)%str,'(A)') model_time 
    Read(fieldBuf(24)%str,'(A)') app_protocol 
   
        
    ih = INDEX(sender_id,"H")+1
    this%sender_id = TRIM(ADJUSTL(sender_id(ih:)))    
    ih = INDEX(filename,"H")+1
    this%filename = TRIM(ADJUSTL(filename(ih:)))    
    ih = INDEX(native_id,"H")+1
    this%native_id = TRIM(ADJUSTL(native_id(ih:)))    
    ih = INDEX(preproc_vers,"H")+1
    this%preproc_vers = TRIM(ADJUSTL(preproc_vers(ih:)))
    ih = INDEX(receive_id,"H")+1
    this%receive_id = TRIM(ADJUSTL(receive_id(ih:)))
    ih = INDEX(units_name,"H")+1
    this%units_name = TRIM(ADJUSTL(units_name(ih:)))
    ih = INDEX(file_time,"H")+1
    this%file_time = TRIM(ADJUSTL(file_time(ih:)))
    ih = INDEX(author,"H")+1
    this%author = TRIM(ADJUSTL(author(ih:)))
    ih = INDEX(organization,"H")+1
    this%organization = TRIM(ADJUSTL(organization(ih:)))
    ih = INDEX(model_time,"H")+1
    this%model_time = TRIM(ADJUSTL(model_time(ih:)))
    ih = INDEX(app_protocol,"H")+1
    this%app_protocol = TRIM(ADJUSTL(app_protocol(ih:)))
   
    this%integer_bits      = integer_bits
    this%real32_max_e10    = real32_max_e10
    this%real32_digits     = real32_digits
    this%real64_max_e10    = real64_max_e10
    this%real64_digits     = real64_digits
    this%model_space_scale = model_space_scale
    this%units_flag        = units_flag
    this%max_weight_grads  = max_weight_grads
    this%max_line_width    = max_line_width
    this%min_res           = min_res    
    this%max_coord         = max_coord
    this%IGES_vers         = IGES_vers
    this%drafting_standard = drafting_standard
   
  End Subroutine getGsection

  Subroutine makeGrecs(this, Grecords)
!! Creates an array of 80 character Grecords from the components of a user 
!! supplied Gsection_t class. The resulting Grecords array are then used to
!! create a user defined IGES file.

    Class(Gsection_t),              Intent(IN)    :: this
    !! This Gsection_t passed argument
    Character(80),     Allocatable, Intent(INOUT) :: Grecords(:)
    !! Array of 80 character records created from the components of this
    !! Gsection_t class

    Integer :: i, ilen, nrecs

    Type(string_t)              :: fields(26)
    Type(string_t), Allocatable :: recbuf(:)
    Character(72)               :: Hbuf
    Character(:),   Allocatable :: Hstring
    Character(8)                :: date
    Character(10)               :: time
    Character(18)               :: datetime

    Character(:), Allocatable :: fmt

    fmt = REAL_FORMAT

! Create each str component of the fields string array based on Gsection_t
! component type 

    Call DATE_AND_TIME(date, time)
    datetime = "15H"//date//"."//time(1:6)
    Hstring = "1H,"
    fields(1)%str = Hstring//","
    Hstring = "1H;"
    fields(2)%str = Hstring//","
    ilen = LEN(this%sender_id)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(3)%str = Hstring//TRIM(ADJUSTL(this%sender_id))//","
    ilen = LEN_TRIM(this%filename)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(4)%str = Hstring//TRIM(ADJUSTL(this%filename))//","
    ilen = LEN(this%native_id)
    Hbuf = REPEAT(" ",72)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(5)%str =  Hstring//TRIM(ADJUSTL(this%native_id))//","
    ilen = LEN(this%preproc_vers)
    Hbuf = REPEAT(" ",72)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(6)%str = Hstring// TRIM(ADJUSTL(this%preproc_vers))//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%integer_bits
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(7)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%real32_max_E10
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(8)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%real32_digits
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(9)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%real64_max_E10
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(10)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%real64_digits
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(11)%str =  Hstring//","
    ilen = LEN(this%receive_id)
    Hbuf = REPEAT(" ",72)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
   fields(12)%str = Hstring//TRIM(ADJUSTL(this%receive_id))//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,fmt) this%model_space_scale
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(13)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%units_flag
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(14)%str =  Hstring//","
    ilen = LEN(this%units_name)
    Hbuf = REPEAT(" ",72)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(15)%str =  Hstring//TRIM(ADJUSTL(this%units_name))//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,fmt) this%model_space_scale
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(16)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,fmt) this%max_line_width
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(17)%str =  Hstring//","
    Hstring = datetime
    fields(18)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,fmt) this%min_res
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(19)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,fmt) this%max_coord
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(20)%str =  Hstring//","
    ilen = LEN(this%author)
    Hbuf = REPEAT(" ",72)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(21)%str =  Hstring//TRIM(ADJUSTL(this%author))//","
    ilen = LEN(this%organization)
    Hbuf = REPEAT(" ",72)
    Write(Hbuf, '(i0,"H")') ilen
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(22)%str =  Hstring//TRIM(ADJUSTL(this%organization))//","

    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%IGES_vers
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(23)%str =  Hstring//","
    Hbuf = REPEAT(" ",72)
    Write(Hbuf,'(i0)') this%drafting_standard
    Hstring = TRIM(ADJUSTL(Hbuf))
    fields(24)%str =  Hstring//","
    Hstring = datetime
    fields(25)%str =  Hstring//","
    If (this%app_protocol /= ",") Then
      ilen = LEN(this%app_protocol)
      Hbuf = REPEAT(" ",72)
      Write(Hbuf, '(i0,"H")') ilen
      Hstring = TRIM(ADJUSTL(Hbuf))
      fields(26)%str = Hstring//TRIM(ADJUSTL(this%app_protocol))//";"
    Else
      fields(26)%str = ";"
    End If

! Next translate fields to recbuf records
 
    Call fieldsToRecs(fields, recbuf, 72)

! Finally, add column 73-80 G section id and sequence number

    nrecs = SIZE(recbuf)
    ALLOCATE(Grecords(nrecs))
    Do i=1,nrecs
      ilen = LEN(recbuf(i)%str)
      Grecords(i) = REPEAT(" ",80)
      Grecords(i)(1:ilen) = recbuf(i)%str(1:ilen)
      Grecords(i)(73:73) = "G"
      Write(Grecords(i)(74:80), '(i7)') i
    End Do

  End Subroutine makeGrecs

  Subroutine outputGsection(this, ounit)
!! Type bound procedure that prints the components of a Gsection_t class to a
!! user defined output unit. This output can be used for verifying a IGES file
!! was read correctly.
 
    Class(Gsection_t), Intent(IN) :: this
    !! This Gsection_t passed argument
    Integer,           Intent(IN) :: ounit
    !! User define output unit

    Write(ounit,*) ''
    Write(ounit,*) ' ***** G L O B A L  D A T A **** '
    Write(ounit,*) ''

    Write(ounit,'(" Parameter Delimiter                     : ", A)')  this%pardel
    Write(ounit,'(" Record Delimiter                        : ", A)')  this%recdel
    Write(ounit,'(" Sender Product ID                       : ", A)')  TRIM(ADJUSTL(this%sender_id))
    Write(ounit,'(" File Name                               : ", A)')  TRIM(ADJUSTL(this%filename))
    Write(ounit,'(" Native System ID                        : ", A)')  TRIM(ADJUSTL(this%native_id))
    Write(ounit,'(" Preprocessor Version                    : ", A)')  TRIM(ADJUSTL(this%preproc_vers))
    Write(ounit,'(" Integer Bit Size                        : ", I0)') this%integer_bits
    Write(ounit,'(" Single Precision Magnitude              : ", I0)') this%real32_max_e10
    Write(ounit,'(" Single Precision Significant Digits     : ", I0)') this%real32_digits
    Write(ounit,'(" Double Precision Magnitude              : ", I0)') this%real64_max_e10
    Write(ounit,'(" Double Precision Significant Digits     : ", I0)') this%real64_digits
    Write(ounit,'(" Receiving System Product ID             : ", A)')  TRIM(ADJUSTL(this%receive_id))
    Write(ounit,'(" Model Space Scale                       : ", ES15.6)') this%model_space_scale
    Write(ounit,'(" Units Flag                              : ", I0)') this%units_flag
    Write(ounit,'(" Units Name                              : ", A)')  TRIM(ADJUSTL(this%units_name))
    Write(ounit,'(" Maximum Line Weight Gradiations         : ", I0)') this%max_weight_grads
    Write(ounit,'(" Maximum Line Weight Width               : ", ES15.6)') this%max_line_width
    Write(ounit,'(" Date and Time File Generated            : ", A)')  TRIM(ADJUSTL(this%file_time))
    Write(ounit,'(" Minimum User Resolution                 : ", ES15.6)') this%min_res
    Write(ounit,'(" Maximum Coordinate Value                : ", ES15.6)') this%max_coord
    Write(ounit,'(" Author                                  : ", A)')  TRIM(ADJUSTL(this%author))
    Write(ounit,'(" Organization                            : ", A)')  TRIM(ADJUSTL(this%organization))
    Write(ounit,'(" IGES Version Flag                       : ", I0)') this%IGES_vers
    Write(ounit,'(" Drafting Standard                       : ", I0)') this%drafting_standard
    Write(ounit,'(" Date and Time Model Created or Modified : ", A)')  TRIM(ADJUSTL(this%model_time))
    Write(ounit,'(" Application Protocol                    : ", A)')  TRIM(ADJUSTL(this%app_protocol))

  End Subroutine outputGsection

End Module IGES_Gsection
