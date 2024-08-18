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

Module IGES_Dsection
!! IGES_Dsection defines derived types and methods for reading the directory
!! section (D section) of IGES files, storing the input in the Dentry_t derived
!! type, creating an IGES file D section for output from Dentry_t components,
!! creating a Dentry_t type from user values, and outputing the contents of
!! a Dentry_t type to a user defined output unit

!! Author  : Richard Weed

!! Version : 0.1 (Beta release 1)
!! Date    : August 18, 2024

  USE IGES_params
  USE IGES_utils, ONLY: DE2PTR, PTR2DE, statusString

  Implicit NONE

  PRIVATE

  Type :: Dentry_t
  !! Class that holds data defined in an IGES dictionary section entry
    Logical      :: has_Tmat = .FALSE.
    !! Flag to signal that this directory entry has a non-zero transformation
    !! matrix field
    Integer      :: thisPentity = 0
    !! Back pointer to associated Pentity 
    Integer      :: thisSeqNo   = 0
    !! Stores starting sequence number in file
    Integer      :: entity_type           = 0
    !! Entity type number 
    Integer      :: param_data            = 0
    !! Starting sequence number in IGES file P section for corresponding
    !! parameter data  
    Integer      :: structure             = 0
    !! Index number in directory entry array of a definition entity that 
    !! specifies this entities meaning. Converted from a negated sequence
    !! number pointer in IGES file
    Integer      :: line_font_pattern     = 0
    !! Line font pattern number or an index number in directory entry array
    !! of a line font definition entity that specifies this font pattern 
    !! The index number corresponds to a negated sequence number pointer in the 
    !! IGES file
    Integer      :: level                 = 0
    !! Number of levels on which a entity resides or an Directory entry index
    !! derived from a negated pointer to a Definition Level Property (type
    !! 406 form 1
    Integer      :: view                  = 0
    !! Index in Directory Entry of a View type entity (Type 410) or a Views
    !! Visible entiry. Neither of these entities are currently supported
    !! by forIGES.
    Integer      :: transformation_matrix = 0
    !! Index number of Dictionary Entry of a transformation matrix for this
    !! entity. This number is converted to an index from a corresponding 
    !! sequence number in the IGES file Directory section.
    Integer      :: label_display         = 0
    !! Index number of a Directory Entry of a label Display Associativity
    !! Type 402, form 5. This is not supported by forIGES
    Integer      :: status_numbers(4)     = 0
    !! Integer array containing the four two digit status numbers. These
    !! numbers define the following status variables.
    !! status_numbers(1) -> Blank Status
    !! status_numbers(2) -> Subordinate Entity Status
    !! status_numbers(3) -> Entity Use Flag
    !! status_numbers(3) -> Hierarchy 
    Integer      :: sequence_number       = 0
    !! Starting sequece number in IGES file section for this dictionay entry
    Integer      :: line_weight_number    = 0
    !! System Display thickness given as a graduation value
    Integer      :: color_number          = 0
    !! Color number or an index in the Directory Entry array that points to a
    !! Color Definition entity (Type 314)
    Integer      :: param_line_count      = 0
    !! Number of lines in the corresponding parameter data section for this
    !! entry 
    Integer      :: form_number           = 0
    !! form number for this entity type
    Character(8) :: entity_label          = "        "
    !! An eight character label for this entity
    Integer      :: entity_sub_number     = 0
    !! 1 to 8 digit number associated with the entity label

  Contains
    Procedure :: dealloc  => deallocDE
    !! Deallocates a Directory Entry array
    Procedure :: init     => initDE
    !! Initializes a Directory entry with user defined input
    Procedure :: output   => outputDE
    !! Outputs a Directory Entry classes components to a user specified unit
    Procedure :: copyTo   => copyToDE
    !! Copies a Directory Entry class to another DE class
    Procedure :: initType => initTypeDE
    !! Uses a user supplied DE to initialize this DE 
    Procedure :: makeDErecs
    !! Makes an array of 80 character records from a Directory Entry class

  End Type

  Type :: Dsection_t
    !! Class that stores an entire IGES file D section as an array of Dentry_t
    !! types
    Integer                     :: nentries
    !! number of entries in the D section array
    Type(Dentry_t), Allocatable :: DE(:)
    !! Allocatable array of Dentry_t classes that holds the individual D section
    !! entries for each entity 

  Contains

    Procedure :: dealloc => deallocDsection
    !! Deallocats the DE array
    Procedure :: output  => outputDsection
    !! Outputs all the DE values in the DE array
    Procedure :: getDS   => getDsection
    !! Builds the Dsection_t DE array from Drecords input from an IGES file

  End Type

  PUBLIC :: Dentry_t, Dsection_t

Contains

  Subroutine deallocDE(this)
!! Type bound procedure to deallocate and reset the values of a Dentry_t class

    Class(Dentry_t), Intent(INOUT) :: this
    !! This Dentry_t class passed argument

    this%entity_type           = 0
    this%param_data            = 0
    this%structure             = 0
    this%line_font_pattern     = 1
    this%level                 = 0
    this%view                  = 0
    this%transformation_matrix = 0
    this%label_display         = 0
    this%status_numbers(4)     = 0
    this%sequence_number       = 0
    this%line_weight_number    = 0
    this%color_number          = 0
    this%param_line_count      = 0
    this%form_number           = 0
    this%entity_label          = "        "
    this%entity_sub_number     = 0

  End Subroutine deallocDE

  Subroutine initDE(this,                                                     &
                    entity_type,                                              &
                    param_data,                                               &
                    structure,                                                &
                    line_font_pattern,                                        &
                    level,                                                    &
                    view,                                                     &
                    transformation_matrix,                                    &
                    label_display,                                            &
                    status_numbers,                                           &
                    sequence_number,                                          &
                    line_weight_number,                                       &
                    color_number,                                             &
                    param_line_count,                                         &
                    form_number,                                              &
                    entity_label,                                             &
                    entity_sub_number)
!!  Type bound procedure to intialize a Dentry_t class to user supplied values.
!!  The default values of Dentry_t will be overridden with the user supplied
!!  values

    Class(Dentry_t),        Intent(INOUT) :: this
    !! this Dentry_t passed argument
    !! The other dummy arguments correspond to the components of a Dentry_t
    !! class

    Integer,                Intent(IN)    :: entity_type
    Integer,      Optional, Intent(IN)    :: param_data
    Integer,      Optional, Intent(IN)    :: structure
    Integer,      Optional, Intent(IN)    :: line_font_pattern
    Integer,      Optional, Intent(IN)    :: level
    Integer,      Optional, Intent(IN)    :: view
    Integer,      Optional, Intent(IN)    :: transformation_matrix
    Integer,      Optional, Intent(IN)    :: label_display
    Integer,      Optional, Intent(IN)    :: status_numbers(4)
    Integer,      Optional, Intent(IN)    :: sequence_number
    Integer,      Optional, Intent(IN)    :: line_weight_number
    Integer,      Optional, Intent(IN)    :: color_number
    Integer,      Optional, Intent(IN)    :: param_line_count
    Integer,      Optional, Intent(IN)    :: form_number
    Character(8), Optional, Intent(IN)    :: entity_label
    Integer,      Optional, Intent(IN)    :: entity_sub_number

    this%entity_type                                  = entity_type
    If (PRESENT(param_data))          this%param_data = param_data
    If (PRESENT(structure ))          this%structure  = structure
    If (PRESENT(line_font_pattern ))  this%line_font_pattern = line_font_pattern
    If (PRESENT(level))               this%level      = level
    If (PRESENT(view))                this%level      = view
    If (PRESENT(transformation_matrix))                              &
                                       this%transformation_matrix =  &
                                            transformation_matrix
    If (PRESENT(label_display))        this%label_display     = label_display
    If (PRESENT(status_numbers))       this%status_numbers    = status_numbers
    If (PRESENT(sequence_number))      this%sequence_number   = sequence_number
    If (PRESENT(line_weight_number))   this%line_weight_number = line_weight_number
    If (PRESENT(color_number))         this%color_number      = color_number
    If (PRESENT(param_line_count))     this%param_line_count  = param_line_count
    If (PRESENT(form_number))          this%form_number       = form_number 
    If (PRESENT(entity_label))         this%entity_label      = entity_label
    If (PRESENT(entity_sub_number))    this%entity_sub_number = entity_sub_number

  End Subroutine initDE

  Subroutine copyToDE(this,that)
  !! Copies this DE class into another one
    Class(DEntry_t), Intent(IN)   :: this
  !! This DE passed argument that is the source for the copy
    Type(DEntry_t),  Intent(OUT)  :: that
  !! A DE type that is the target for the copy

    that = this

  End Subroutine copyToDE

  Subroutine initTypeDE(this,that)
  !! Initializes this DE class with a user supplied DE

    Class(DEntry_t), Intent(OUT) :: this
    !! A Dentry_t passed argument that is the target for a copyTo
    Type(DEntry_t),  Intent(IN)  :: that
    !! A Dentry_t type that is the source for a copyTo

    Call that%copyTo(this)

  End Subroutine initTypeDE

  Subroutine makeDErecs(this, Drecords)
  !! Builds an 80 character Drecords array from the components of this Dentry_t
  !! class 

    Class(DEntry_t), Intent(INOUT) :: this
    !! This is a DE class passed argument
    Character(80),   Intent(INOUT) :: Drecords(2)
    !! Output array of 80 character records

    Character(8) :: status

! Use this Dentry_t values to create the two D records required for this
! P entity using internal files.

    Drecords(1) = REPEAT(" ",80)
    Drecords(2) = REPEAT(" ",80)

    Write(Drecords(1)(1:8),   '(i8)')  this%entity_type
    Write(Drecords(1)(9:16),  '(i8)')  this%param_data
    Write(Drecords(1)(17:24), '(i8)')  this%structure
    Write(Drecords(1)(25:32), '(i8)')  this%line_font_pattern
    Write(Drecords(1)(33:40), '(i8)')  this%level
    Write(Drecords(1)(41:48), '(i8)')  this%view
    Write(Drecords(1)(49:56), '(i8)')  DE2PTR(this%transformation_matrix)
    Write(Drecords(1)(57:64), '(i8)')  this%label_display
    If (ANY(this%status_numbers /= 0)) Then
      status = statusString(this%status_numbers)
      Write(Drecords(1)(65:72), '(A)')  status
    Else
      Drecords(1)(65:72) = "        "
    End If
    Drecords(1)(73:73) = "D"
    Write(Drecords(1)(74:80), '(i7)')  this%sequence_number
    Write(Drecords(2)(1:8),   '(i8)')  this%entity_type
    Write(Drecords(2)(9:16),  '(i8)')  this%line_weight_number
    Write(Drecords(2)(17:24), '(i8)')  this%color_number
    Write(Drecords(2)(25:32), '(i8)')  this%param_line_count
    Write(Drecords(2)(33:40), '(i8)')  this%form_number
    Write(Drecords(2)(57:64), '(A8)')  this%entity_label
    If (this%entity_sub_number /= 0) Then
      Write(Drecords(2)(65:72), '(i8)')  this%entity_sub_number
    Else
      Write(Drecords(2)(65:72),'(A)') "        "
    End If
    Drecords(2)(73:73) = "D"
    Write(Drecords(2)(74:80), '(i7)')  (this%sequence_number+1)

  End Subroutine makeDErecs

  Subroutine outputDE(this, ounit)
  !! Outputs the components of this DE to a user define output unit

    Class(Dentry_t),  Intent(IN) :: this
    !! This DE passed argument
    Integer,          Intent(IN) :: ounit
    !! User defined output unit

    Character(8) :: status

    Write(ounit,*) ' '
    Write(ounit,'(" Entity Type Number      : ", I8)') this%entity_type
    Write(ounit,'(" Parameter Start Record  : ", I8)') this%param_data
    Write(ounit,'(" Structure               : ", I8)') this%structure
    Write(ounit,'(" Line Font Pattern       : ", I8)') this%line_font_pattern
    Write(ounit,'(" Level                   : ", I8)') this%level
    Write(ounit,'(" View                    : ", I8)') this%view
    Write(ounit,'(" Trans. Matrix Index     : ", I8)') this%transformation_matrix
    Write(ounit,'(" Trans. Matrix Seq. No.  : ", I8)')  &
        DE2PTR(this%transformation_matrix)
    Write(ounit,'(" Label Display Assoc.    : ", I8)') this%label_display
    status = statusString(this%status_numbers)
    Write(ounit,'(" Status Number           : ", A)') status
    Write(ounit,'(" Sequence Number         : ", I8)') this%sequence_number
    Write(ounit,'(" Line Weight Number      : ", I8)') this%line_weight_number
    Write(ounit,'(" Color Number            : ", I8)') this%color_number
    Write(ounit,'(" Parameter Line Count    : ", I8)') this%param_line_count
    Write(ounit,'(" Form Number             : ", I8)') this%form_number
    Write(ounit,'(" Entity Label            : ",  A)') this%entity_label
    Write(ounit,'(" Entity Subscript Number : ", I8)') this%entity_sub_number
    Write(ounit, *) ' '

  End Subroutine outputDE

  Subroutine deallocDsection(this)
  !! Dellocates a Dsection_t Dsection array

    Class(Dsection_t), Intent(INOUT) :: this
    !! This Dsection_t passes argument

    Integer :: i

    Do i=1, this%nentries
      Call this%DE(i)%dealloc()
    End Do
    If (ALLOCATED(this%DE)) DEALLOCATE(this%DE)

  End Subroutine deallocDsection
   
  Subroutine getDsection(this, Drecords)
  !! Builds a D section array from an array of Drecords

    Class(Dsection_t), Intent(INOUT) :: this
    !! This Dsection_t passed argument
    Character(80),     Intent(IN)    :: Drecords(:)
    !! Array of 80 character records used to build the Dsection DE array entries

    Integer :: i, j, nd, numDrecs
   
    Integer     :: tmat, lfp, vw, lda, cn, lev, struct
 
    Type(string_t), Allocatable :: dtemp(:)

    numDrecs = SIZE(Drecords)
    nd       = numDrecs/2
    ALLOCATE(this%DE(nd))
    ALLOCATE(dtemp(nd))
    this%nentries = nd
   
! Loop through the number of D records concatinating ever other records
! into one 160 character string
  
    j = 0
    Do i=1,numDrecs-1,2
      j = j+1
      dtemp(j)%str = Drecords(i) // Drecords(i+1)
    End Do

! Now loop through dtemp array and use internal file writes to assign
! data to each Dentry_t component

    Do i=1,nd
      Read(dtemp(i)%str(1:8),     *)    this%DE(i)%entity_type
      Read(dtemp(i)%str(9:16),    *)    this%DE(i)%param_data
      If (dtemp(i)%str(17:24) /= "        ") Then 
        Read(dtemp(i)%str(17:24),   *)   struct
        If (struct < 0) Then
          this%DE(i)%structure = -PTR2DE(ABS(struct))
        Else 
          this%DE(i)%structure = struct
        End If
      End If
      If (dtemp(i)%str(25:32) /= "        ")  Then 
        Read(dtemp(i)%str(25:32),   *)  lfp 
        If (lfp < 0) Then
          this%DE(i)%line_font_pattern = -PTR2DE(ABS(lfp))
        Else 
          this%DE(i)%line_font_pattern = lfp 
        End If
      End If
      If (dtemp(i)%str(33:40) /= "        ")  Then 
        Read(dtemp(i)%str(33:40),   *)   lev 
        If (lev < 0) Then
          this%DE(i)%level = -PTR2DE(ABS(lev))
        Else 
          this%DE(i)%level = lev 
        End If
      End If
      If (dtemp(i)%str(41:48) /= "        ") Then 
        Read(dtemp(i)%str(41:48),   *)   vw 
        If (vw < 0) Then
          this%DE(i)%view = -PTR2DE(ABS(vw))
        Else 
          this%DE(i)%view = 0 
        End If
      End If
      tmat = 0
      If (dtemp(i)%str(49:56) /= "        ")                                  & 
        Read(dtemp(i)%str(49:56),   *)    tmat 
      If (tmat /= 0) Then
        this%DE(i)%transformation_matrix = (tmat+1)/2
      Else 
        this%DE(i)%transformation_matrix = 0
      End If 

      If (dtemp(i)%str(57:64) /= "        ") Then 
        Read(dtemp(i)%str(57:64),   *)  lda
        If (lda < 0) Then
          this%DE(i)%label_display = -PTR2DE(ABS(lda))
        Else 
          this%DE(i)%label_display= 0 
        End If
      End If
      If (dtemp(i)%str(65:72) /= "        ") Then 
        Read(dtemp(i)%str(65:72),'(4I2)')   this%DE(i)%status_numbers
      Else
        this%DE(i)%status_numbers = 0
      End If 
      Read(dtemp(i)%str(74:80),   *)    this%DE(i)%sequence_number
      If (dtemp(i)%str(89:96) /= "        ")                                  & 
        Read(dtemp(i)%str(89:96),   *)    this%DE(i)%line_weight_number
      If (dtemp(i)%str(97:104) /= "        ") Then 
        Read(dtemp(i)%str(97:104),  *) cn 
        If (vw < 0) Then
          this%DE(i)%color_number = -PTR2DE(ABS(cn))
        Else 
          this%DE(i)%color_number= cn 
        End If
      End If
      Read(dtemp(i)%str(105:112), *)    this%DE(i)%param_line_count
      If (dtemp(i)%str(113:120) /= "        ")                               & 
        Read(dtemp(i)%str(113:120), *)    this%DE(i)%form_number
      Read(dtemp(i)%str(137:144),'(A)') this%DE(i)%entity_label
      If (dtemp(i)%str(145:152) == "        ") CYCLE
      Read(dtemp(i)%str(145:152), *)    this%DE(i)%entity_sub_number
    End Do

  End Subroutine getDsection

  Subroutine outputDsection(this, ounit)
  !! Outputs an entire D section array to a user defined output unit
 
    Class(Dsection_t), Intent(IN) :: this
    !! This Dsection_t passed argument
    Integer,           Intent(IN) :: ounit
    !! User define output unit

    Integer :: i

    Write(ounit,*) ' '
    Write(ounit,*) ' ***** D I R E C T O R Y   D A T A **** '
    Write(ounit,*) ' '

    Do i=1,this%nentries
      Write(ounit,*) ' '
      Write(ounit,'(" ***   IGES Model Directory Entry : ", I0," ***")') i
      Write(ounit,*) ' '
      Call this%DE(i)%output(ounit)
    End Do

  End Subroutine outputDsection

End Module IGES_Dsection
